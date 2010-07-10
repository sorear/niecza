use strict;
use warnings;
use 5.010;

{
    package Decl;
    use Moose;

    sub preinit {}
    sub enter   {}
    sub write   {}

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

    sub preinit {
        my ($self, $cg, $body) = @_;
        $self->code->outer($body);
        $cg->open_protopad;
        $self->code->preinit($cg);
        $cg->close_sub($self->code->code);
        $cg->call_sub($self->has_var, 0);
        $cg->proto_var($self->var) if $self->has_var;
    }

    sub enter {
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

    sub preinit {
        my ($self, $cg, $body) = @_;
        $self->code->outer($body);
        $cg->open_protopad;
        $self->code->preinit($cg);
        $cg->close_sub($self->code->code);
        $cg->clr_call_direct('Kernel.NewROVar', 1);
        $cg->proto_var($self->var);
    }

    sub enter {
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
    package Decl::Class;
    use Moose;

    has name => (is => 'ro', isa => 'Str', predicate => 'has_name');
    has var  => (is => 'ro', isa => 'Str', required => 1);
    has stub => (is => 'ro', isa => 'Bool', default => 1);
    has parents => (is => 'ro', isa => 'ArrayRef', default => sub { [] });

    # the body is a very sublike thing; it has a preinit existance, and a
    # lexical scope.  but instead of just a Sub, it constructs a ClassHOW at
    # preinit
    has body => (is => 'ro', isa => 'Body::Class', required => 1);

    sub preinit {
        my ($self, $cg, $body) = @_;
        $cg->scopelexget("ClassHOW", $body);
        $cg->dup_fetch;
        $cg->string_var($self->name // 'ANON');
        $cg->call_method(1, "new", 1);
        $cg->dup;
        $cg->push_aux('how');
        $cg->proto_var($self->var . '!HOW');

        # TODO: Initialize the protoobject to a failure here so an awesome error
        # is produced if someone tries to use an incomplete class in a BEGIN.
        $cg->push_null('Variable');
        $cg->proto_var($self->var);

        $self->body->outer($body);
        $self->body->var($self->var);

        $cg->open_protopad;

        $cg->peek_aux('how');
        $cg->dup_fetch;
        $cg->peek_aux('protopad');
        $cg->clr_call_direct('Kernel.NewROVar', 1);
        $cg->call_method(0, "push-scope", 1);

        $self->body->preinit($cg);
        $cg->close_sub($self->body->code);
        $cg->proto_var($self->var . '!BODY');
    }

    sub enter   {
        my ($self, $cg, $body) = @_;
        $cg->share_lex($self->var . '!HOW');
        $cg->clone_lex($self->var . '!BODY');
        $cg->lexget($self->var . '!BODY');
        $cg->fetch;
        $cg->call_sub(0, 0);
        # the body will set $self->var at ENTER time
    }
    sub write   {
        my ($self, $body) = @_;
        $self->body->var($self->var);
        $self->body->outer($body);
        $self->body->write;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
