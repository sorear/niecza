use strict;
use warnings;
use 5.010;

{
    package Sig::Target;
    use Moose;

    has slot => (is => 'ro', isa => 'Maybe[Str]', required => 1);

    sub used_slots {
        my $self = shift;
        if ($self->slot) { ($self->slot) } else { () }
    }

    sub binder {
        my ($self, $get) = @_;
        if ($self->slot) {
            # TODO: implement ro, etc
            CgOp::bind(0, CgOp::scopedlex($self->slot), $get);
        } else {
            CgOp::noop;
        }
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Sig::Parameter;
    use Moose;

    has target => (is => 'ro', isa => 'Sig::Target', required => 1,
        handles => [ 'used_slots' ]);

    sub binder {
        my ($self, $ixp) = @_;

        $self->target->binder(CgOp::pos($$ixp++));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Sig;
    use Moose;

    has params => (isa => 'ArrayRef[Sig::Parameter]', is => 'ro', required => 1);

    sub for_method {
        my $self = shift;
        my $sp = Sig::Parameter->new(target =>
            Sig::Target->new(slot => 'self'));
        Sig->new(params => [ $sp, @{ $self->params } ]);
    }

    sub used_slots {
        my $self = shift;
        map { $_->used_slots } @{ $self->params };
    }

    sub binder {
        my ($self) = @_;

        # TODO: Error checking.
        my $ix = 0;
        my @p;
        for (@{ $self->params }) {
            push @p, $_->binder(\$ix);
        }
        CgOp::prog(@p);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
