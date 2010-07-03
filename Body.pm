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
    # various things which need PRE-INIT time initialization -
    # phasers (Expr), subblocks [str, Body], variables [str, Expr]
    has protos  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has codegen => (isa => 'CodeGen', is => 'rw');

    sub code {
        my ($self) = @_;
        if ($self->codegen) { return $self->codegen }
        $self->codegen(CodeGen->new(name => $self->name));
        my $cg = $self->codegen;
        $_->void_cg($cg, $self) for @{ $self->enter };
        $self->do->item_cg($cg, $self);
        # TODO: Bind a return value here to catch non-ro sub use
        $cg->return(1) unless $cg->unreach;
        return $cg;
    }

    sub write {
        my ($self) = @_;
        $self->code->write;
        for my $pi (@{ $self->protos }) {
            $pi->[2]->outer($self);
            $pi->[2]->write;
        }
    }

    sub preinit {
        my ($self, $cg) = @_;
        for my $pi (@{ $self->protos }) {
            my ($k,$a,$b) = @$pi;
            $b->outer($self);
            $cg->open_protopad;
            $b->preinit($cg);
            $cg->close_sub($b->code);
            if ($k) {
                $cg->call_sub(($a ? 1 : 0), 0);
            } else {
                $cg->clr_call_direct('Kernel.NewROVar', 1);
            }
            $cg->proto_var($a) if $a;
        }
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
