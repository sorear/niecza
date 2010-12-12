use strict;
use warnings;
use utf8;
use 5.010;

{
    package Sig::Parameter;
    use Moose;

    has slot => (is => 'ro', isa => 'Maybe[Str]', required => 1);
    has slurpy => (is => 'ro', isa => 'Bool', default => 0);
    has slurpycap => (is => 'ro', isa => 'Bool', default => 0);
    # rw binding to Mu that does not viv
    has rwtrans => (is => 'ro', isa => 'Bool', default => 0);
    has full_parcel => (is => 'ro', isa => 'Bool', default => 0);
    has optional => (is => 'ro', isa => 'Bool', default => 0);
    has default => (is => 'ro', isa => 'Maybe[Body]', default => undef);
    has mdefault => (is => 'rw', isa => 'Maybe[ArrayRef]');
    has positional => (is => 'ro', isa => 'Bool', default => 1);
    has readonly => (is => 'ro', isa => 'Bool', default => 0);
    has names => (is => 'ro', isa => 'ArrayRef[Str]', default => sub { [] });
    has name => (is => 'ro', isa => 'Str', required => 1);
    has list => (is => 'ro', isa => 'Bool', default => 0);
    has hash => (is => 'ro', isa => 'Bool', default => 0);
    has type => (is => 'ro', isa => 'Str', default => 'Any');
    has tclass => (is => 'rw', isa => 'ArrayRef');

    sub _default_get {
        my ($self, $body) = @_;

        if (defined $self->mdefault) {
            return CgOp::call_uncloned_sub(@{ $self->mdefault });
        } elsif ($self->optional) {
            return CgOp::scopedlex($self->type);
        } else {
            return CgOp::die(
                "No value in " . $body->name . " available for parameter " .
                $self->name);
        }
    }

    sub single_get_inline {
        my ($self, $body, $posr) = @_;

        if ($self->positional && @$posr) {
            return shift @$posr;
        } else {
            return $self->_default_get($body);
        }
    }

    sub bind_inline {
        my ($self, $body, $posr) = @_;

        my $get = $self->full_parcel ? $self->parcel_get_inline($posr) :
            $self->slurpycap ? $self->slurpycap_get_inline($posr) :
            $self->slurpy ? $self->slurpy_get_inline($posr) :
            $self->single_get_inline($body, $posr);

        if (defined $self->slot) {
            return CgOp::scopedlex($self->slot, $self->rwtrans ? $get :
                CgOp::newboundvar($self->readonly, $self->list, $get));
        } else {
            return CgOp::sink($get);
        }
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Sig;
    use Moose;

    has params => (isa => 'ArrayRef[Sig::Parameter]', is => 'ro', required => 1);
    has explicit_inv => (isa => 'Bool', is => 'ro', default => 0);

    sub for_method {
        my $self = shift;
        return $self if $self->explicit_inv;
        my $sp = Sig::Parameter->new(slot => 'self', name => 'self',
            readonly => 1);
        Sig->new(params => [ $sp, @{ $self->params } ]);
    }

    sub simple {
        my ($class, @names) = @_;
        Sig->new(params => [map { Sig::Parameter->new(slot => $_, name => $_,
                readonly => 1)
            } @names]);
    }

    sub bind_inline {
        my ($self, $body, @pos) = @_;

        my @p;
        for (@{ $self->params }) {
            push @p, $_->bind_inline($body, \@pos);
        }

        CgOp::prog(@p);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
