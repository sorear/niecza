use strict;
use warnings;
use 5.010;

use CgOp;

{
    package Decl;
    use Moose;

    sub used_slots   { }
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

    sub used_slots {
        my ($self) = @_;
        return $self->has_var ? ($self->var) : ();
    }

    sub preinit_code {
        my ($self, $body) = @_;
        $self->code->outer($body);
        my $c = CgOp::subcall(CgOp::protosub($self->code));
        $self->has_var ? CgOp::proto_var($self->var, $c) : CgOp::sink($c);
    }

    sub enter_code {
        my ($self, $body) = @_;
        !$self->has_var ? CgOp::noop :
            ($self->shared || $body->mainline) ? CgOp::share_lex($self->var) :
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

    sub used_slots {
        return $_[0]->var;
    }

    sub preinit_code {
        my ($self, $body) = @_;
        $self->code->outer($body);

        CgOp::proto_var($self->var, CgOp::newscalar(
                CgOp::protosub($self->code)));
    }

    sub enter_code {
        my ($self, $body) = @_;
        $body->mainline ?
            CgOp::share_lex($self->var) :
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
    has list => (isa => 'Bool', is => 'ro', default => 0);

    sub used_slots {
        return $_[0]->slot;
    }

    sub preinit_code {
        my ($self, $body) = @_;

        if ($self->list) {
            CgOp::proto_var($self->slot,
                CgOp::newrwlistvar(CgOp::fetch(CgOp::scopedlex('Any'))));
        } else {
            CgOp::proto_var($self->slot,
                CgOp::newrwscalar(CgOp::fetch(CgOp::scopedlex('Any'))));
        }
    }

    sub enter_code {
        my ($self, $body) = @_;

        $body->mainline ?
            CgOp::share_lex($self->slot) :
            CgOp::copy_lex($self->slot);
    }

    sub write {
        my ($self, $body) = @_;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::StateVar;
    use Moose;
    extends 'Decl';

    has slot    => (isa => 'Str', is => 'ro', required => 1);
    has backing => (isa => 'Str', is => 'ro', required => 1);

    sub used_slots {
        return $_[0]->slot;
    }

    sub preinit_code {
        my ($self, $body) = @_;
        CgOp::proto_var($self->slot, CgOp::scopedlex($self->backing));
    }

    sub enter_code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->slot, CgOp::scopedlex($self->backing));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::RunMainline;
    use Moose;
    extends 'Decl';

    sub used_slots { '!mainline' }

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
        my ($self, $body) = @_;
        $body->mainline ?
            CgOp::share_lex('!mainline') :
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
    has bodyvar => (is => 'ro', isa => 'Str');
    has stub => (is => 'ro', isa => 'Bool', default => 0);
    has parents => (is => 'ro', isa => 'ArrayRef', default => sub { [] });
    has body => (is => 'ro', isa => 'Body');

    sub used_slots {
        my ($self) = @_;
        if ($self->stub) {
            ($self->var, $self->var . '!HOW');
        } else {
            ($self->var, $self->var . '!HOW', $self->bodyvar);
        }
    }

    sub preinit_code {
        my ($self, $body) = @_;

        if ($self->stub) {
            return CgOp::prog(
                CgOp::proto_var($self->var . '!HOW', CgOp::null('Variable')),
                CgOp::proto_var($self->var, CgOp::null('Variable')));
        }

        $self->body->outer($body);

        CgOp::with_aux("how",
            CgOp::methodcall(CgOp::scopedlex("ClassHOW"), "new",
                CgOp::wrap(CgOp::clr_string($self->name // 'ANON'))),

            CgOp::proto_var($self->var . '!HOW', CgOp::aux("how")),

            # TODO: Initialize the protoobject to a failure here so an awesome
            # error is produced if someone tries to use an incomplete class in
            # a BEGIN.
            CgOp::proto_var($self->var, CgOp::null('Variable')),

            CgOp::proto_var($self->bodyvar,
                CgOp::newscalar(
                    CgOp::protosub($self->body))),
            CgOp::scopedlex($self->var,
                CgOp::methodcall(CgOp::aux("how"), "create-protoobject")));
    }

    sub enter_code {
        my ($self, $body) = @_;
        CgOp::prog(
            CgOp::share_lex($self->var . '!HOW'),
            CgOp::share_lex($self->var),
            ($self->stub ? () :
                ($body->mainline ?
                    CgOp::share_lex($self->bodyvar) :
                    CgOp::clone_lex($self->bodyvar))));
    }

    sub write   {
        my ($self, $body) = @_;
        return unless $self->body;
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
        if ($body->type ne 'class') {
            #TODO: Make this a sorry.
            die "Tried to set a method outside a class!";
        }
        CgOp::sink(
            CgOp::methodcall(CgOp::aux("how"), "add-method",
                CgOp::wrap(CgOp::clr_string($self->name)),
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
        if ($body->type ne 'class') {
            #TODO: Make this a sorry.
            die "Tried to set a superclass outside an initial class!";
        }

        CgOp::sink(
            CgOp::methodcall(CgOp::aux('how'), "add-super",
                CgOp::scopedlex($self->name . "!HOW")));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
