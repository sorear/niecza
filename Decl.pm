use strict;
use warnings;
use 5.010;

{
    package Decl;
    use Moose;

    sub do_preinit {}
    sub do_enter   {}
    sub write     {}

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

    sub do_preinit {
        my ($self, $cg, $body) = @_;
        $self->code->outer($body);
        $cg->open_protopad($self->code);
        $self->code->do_preinit($cg);
        $cg->close_sub($self->code->code);
        $cg->call_sub($self->has_var, 0);
        $cg->proto_var($self->var) if $self->has_var;
    }

    sub do_enter {
        my ($self, $cg, $body) = @_;
        return unless $self->has_var;
        if ($self->shared) {
            $cg->share_lex($self->var);
        } else {
            $cg->copy_lex($self->var);
        }
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

    sub do_preinit {
        my ($self, $cg, $body) = @_;
        $self->code->outer($body);
        $cg->open_protopad($self->code);
        $self->code->do_preinit($cg);
        $cg->close_sub($self->code->code);
        $cg->clr_call_direct('Kernel.NewROVar', 1);
        $cg->proto_var($self->var);
    }

    sub do_enter {
        my ($self, $cg, $body) = @_;
        $cg->clone_lex($self->var);
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
    package Decl::RunMainline;
    use Moose;
    extends 'Decl';

    sub do_preinit {
        my ($self, $cg, $body) = @_;
        $cg->clr_sfield_get('Kernel.MainlineContinuation');
        $cg->push_null('Frame');
        $cg->push_null('Frame');
        $cg->clr_call_direct('Kernel.MakeSub', 3);

        $cg->peek_aux('protopad');
        $cg->clr_call_direct('Kernel.NewROVar', 1);
        $cg->call_sub(1, 1);
        $cg->proto_var('!mainline');

        $::SETTING_RESUME = $body;
    }

    sub do_enter {
        my ($self, $cg, $body) = @_;
        $cg->clone_lex('!mainline');
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

    sub do_preinit {
        my ($self, $cg, $body) = @_;
        if ($self->stub) {
            $cg->push_null('Variable');
            $cg->proto_var($self->var);
            $cg->push_null('Variable');
            $cg->proto_var($self->var . '!HOW');
            return;
        }
        $cg->scopelexget("ClassHOW", $body);
        $cg->dup_fetch;
        $cg->string_var($self->name // 'ANON');
        $cg->call_method(1, "new", 1);
        $cg->push_aux('how');
        $cg->peek_aux('how');
        $cg->proto_var($self->var . '!HOW');

        # TODO: Initialize the protoobject to a failure here so an awesome error
        # is produced if someone tries to use an incomplete class in a BEGIN.
        $cg->push_null('Variable');
        $cg->proto_var($self->var);

        $self->body->outer($body);
        $self->body->var($self->var);

        $cg->open_protopad($self->body);

        $cg->peek_aux('how');
        $cg->dup_fetch;
        $cg->callframe;
        $cg->clr_wrap;
        $cg->call_method(0, "push-scope", 1);

        $self->body->do_preinit($cg);
        $cg->close_sub($self->body->code);
        $cg->clr_call_direct('Kernel.NewROVar', 1);
        $cg->proto_var($self->var . '!BODY');
    }

    sub do_enter   {
        my ($self, $cg, $body) = @_;
        $cg->share_lex($self->var . '!HOW');
        if ($self->stub) {
            $cg->share_lex($self->var);
        } else {
            $cg->clone_lex($self->var . '!BODY');
        }
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
    package Decl::Super;
    use Moose;
    extends 'Decl';

    has name => (is => 'ro', isa => 'Str', required => 1);

    sub do_preinit {
        my ($self, $cg, $body) = @_;
        if (!$body->isa('Body::Class')) {
            #TODO: Make this a sorry.
            die "Tried to set a superclass outside a class!";
        }
        push @{ $body->super }, $self->name;

        $cg->peek_aux('how');
        $cg->dup_fetch;
        $cg->scopelexget($self->name . "!HOW", $body);
        $cg->call_method(0, "add-super", 1);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
