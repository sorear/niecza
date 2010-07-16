use strict;
use warnings;
use 5.010;

use CgOp;

{
    package Decl;
    use Moose;

    sub preinit_code { CgOp::noop }
    sub enter_code   { CgOp::noop }
    sub write        {}

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::PreInit;
    use Moose;
    extends 'Decl';

    has var    => (isa => 'Str', is => 'ro', predicate => 'has_var');
    has code   => (isa => 'Body', is => 'ro', required => 1);
    has shared => (isa => 'Bool', is => 'ro', default => 0);

    sub preinit_code {
        my ($self, $body) = @_;
        $self->code->outer($body);
        my $c = CgOp::subcall(CgOp::protosub($self->code));
        $self->has_var ? CgOp::proto_var($self->var, $c) : CgOp::sink($c);
    }

    sub enter_code {
        my ($self, $body) = @_;
        !$self->has_var ? CgOp::noop :
            $self->shared ? CgOp::share_lex($self->var) :
            CgOp::copy_lex($self->var);
    }

    sub write {
        my ($self, $body) = @_;
        $self->code->outer($body);
        $self->code->write;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::Sub;
    use Moose;
    extends 'Decl';

    has var    => (isa => 'Str', is => 'ro', required => 1);
    has code   => (isa => 'Body', is => 'ro', required => 1);

    sub preinit_code {
        my ($self, $body) = @_;
        $self->code->outer($body);

        CgOp::proto_var($self->var, CgOp::newscalar(
                CgOp::protosub($self->code)));
    }

    sub enter_code {
        my ($self, $cg, $body) = @_;
        CgOp::clone_lex($self->var);
    }

    sub write {
        my ($self, $body) = @_;
        $self->code->outer($body);
        $self->code->write;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::SimpleVar;
    use Moose;
    extends 'Decl';

    has slot => (isa => 'Str', is => 'ro', required => 1);

    sub preinit_code {
        my ($self, $body) = @_;

        CgOp::proto_var($self->slot,
            CgOp::newrwscalar(CgOp::fetch(CgOp::scopedlex('Any'))));
    }

    sub enter_code {
        my ($self, $body) = @_;

        CgOp::copy_lex($self->slot);
    }

    sub write {
        my ($self, $body) = @_;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::RunMainline;
    use Moose;
    extends 'Decl';

    sub preinit_code {
        my ($self, $body) = @_;

        # XXX ought not to have side effects here.
        $::SETTING_RESUME = $body;

        CgOp::proto_var('!mainline',
            CgOp::subcall(
                CgOp::rawscall('Kernel.MakeSub',
                    CgOp::rawsget('Kernel.MainlineContinuation'),
                    CgOp::null('Frame'), CgOp::null('Frame')),
                CgOp::newscalar(CgOp::aux('protopad'))));
    }

    sub enter_code {
        my ($self, $cg, $body) = @_;
        CgOp::clone_lex('!mainline');
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::Class;
    use Moose;

    has name => (is => 'ro', isa => 'Str', predicate => 'has_name');
    has var  => (is => 'ro', isa => 'Str', required => 1);
    has stub => (is => 'ro', isa => 'Bool', default => 0);
    has parents => (is => 'ro', isa => 'ArrayRef', default => sub { [] });

    # the body is a very sublike thing; it has a preinit existance, and a
    # lexical scope.  but instead of just a Sub, it constructs a ClassHOW at
    # preinit
    has body => (is => 'ro', isa => 'Body::Class');

    sub preinit_code {
        my ($self, $body) = @_;

        if ($self->stub) {
            return CgOp::prog(
                CgOp::proto_var($self->var . '!HOW', CgOp::null('Variable')),
                CgOp::proto_var($self->var, CgOp::null('Variable')));
        }

        $self->body->outer($body);
        $self->body->var($self->var);

        CgOp::with_aux("how",
            CgOp::methodcall(CgOp::scopedlex("ClassHOW"), "new",
                CgOp::wrap(CgOp::clr_string($self->name // 'ANON'))),

            CgOp::proto_var($self->var . '!HOW', CgOp::aux("how")),

            # TODO: Initialize the protoobject to a failure here so an awesome
            # error is produced if someone tries to use an incomplete class in
            # a BEGIN.
            CgOp::proto_var($self->var, CgOp::null('Variable')),

            CgOp::proto_var($self->var . '!BODY',
                CgOp::newscalar(
                    CgOp::protosub($self->body,
                        CgOp::proto_var('!scopenum',
                            CgOp::methodcall(CgOp::aux('how'),
                                "push-scope",
                                CgOp::wrap(CgOp::callframe)))))));
    }

    sub enter_code {
        my ($self, $body) = @_;
        CgOp::prog(
            CgOp::share_lex($self->var . '!HOW'),
            ($self->stub ?
                CgOp::share_lex($self->var) :
                CgOp::clone_lex($self->var . '!BODY')));
    }

    sub write   {
        my ($self, $body) = @_;
        return unless $self->body;
        $self->body->var($self->var);
        $self->body->outer($body);
        $self->body->write;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::HasMethod;
    use Moose;
    extends 'Decl';

    has name => (is => 'ro', isa => 'Str', required => 1);
    has var  => (is => 'ro', isa => 'Str', required => 1);

    sub preinit_code {
        my ($self, $body) = @_;
        if (!$body->isa('Body::Class')) {
            #TODO: Make this a sorry.
            die "Tried to set a method outside a class!";
        }
        CgOp::sink(
            CgOp::methodcall(CgOp::aux("how"), "add-scoped-method",
                CgOp::wrap(CgOp::clr_string($self->name)),
                CgOp::scopedlex('!scopenum'),
                CgOp::scopedlex($self->var)));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::Super;
    use Moose;
    extends 'Decl';

    has name => (is => 'ro', isa => 'Str', required => 1);

    sub preinit_code {
        my ($self, $body) = @_;
        if (!$body->isa('Body::Class')) {
            #TODO: Make this a sorry.
            die "Tried to set a superclass outside a class!";
        }
        if ($body->augmenting) {
            die "Cannot add superclasses in an augment";
        }
        push @{ $body->super }, $self->name;

        CgOp::sink(
            CgOp::methodcall(CgOp::aux('how'), "add-super",
                CgOp::scopedlex($self->name . "!HOW")));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
