use strict;
use warnings;
use 5.010;
use CodeGen ();

{
    package Body;
    use Moose;

    has name    => (isa => 'Str', is => 'rw', default => "anon");
    has do      => (isa => 'Op', is => 'rw');
    has enter   => (isa => 'ArrayRef[Op]', is => 'ro',
        default => sub { [] });
    has lexical => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has outer   => (isa => 'Body', is => 'rw', init_arg => undef);
    has decls   => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has codegen => (isa => 'CodeGen', is => 'rw');

    sub code {
        my ($self) = @_;
        if ($self->codegen) { return $self->codegen }
        $self->codegen(CodeGen->new(name => $self->name));
        my $cg = $self->codegen;
        $_->enter($cg, $self) for @{ $self->decls };
        $_->void_cg($cg, $self) for @{ $self->enter };
        $self->do->item_cg($cg, $self);
        # TODO: Bind a return value here to catch non-ro sub use
        $cg->return(1) unless $cg->unreach;
        return $cg;
    }

    sub write {
        my ($self) = @_;
        $self->code->write;
        $_->write($self) for (@{ $self->decls });
    }

    sub preinit {
        my ($self, $cg) = @_;
        $_->preinit($cg, $self) for @{ $self->decls };
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
