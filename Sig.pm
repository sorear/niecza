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

    sub gen_binder {
        my ($self, $cg, $get) = @_;
        if ($self->slot) {
            # TODO: implement ro, etc
            $cg->scopelexget($self->slot);
            $get->();
            $cg->bind(0,0);
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

    sub gen_binder {
        my ($self, $cg, $ixp) = @_;

        $self->target->gen_binder($cg, sub { $cg->pos($$ixp) });
        $$ixp++;
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

    sub gen_binder {
        my ($self, $cg) = @_;

        # TODO: Error checking.
        my $ix = 0;
        for (@{ $self->params }) {
            $_->gen_binder($cg, \$ix);
        }
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
