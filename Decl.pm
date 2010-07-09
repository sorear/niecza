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

1;
