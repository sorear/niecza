package Optimizer::RxSimple;
use 5.010;
use utf8;
use strict;
use warnings;

sub run {
    $_[0]->rxsimp(0);
}

# XXX should use a multi sub.
sub RxOp::rxsimp { my ($self, $cut) = @_;
    my $selfp = bless { %$self }, ref($self);
    $selfp->{zyg} = [ map { $_->rxsimp(0) } @{ $self->zyg } ];
    $selfp;
}

sub RxOp::mayback { 1 }

sub RxOp::Sequence::rxsimp { my ($self, $cut) = @_;
    my @kids;
    for my $i (0 .. $#{ $self->zyg }) {
        my $k = $self->zyg->[$i];
        $k = $k->rxsimp($cut && $i == $#{ $self->zyg });
        if ($k->isa('RxOp::Sequence')) {
            push @kids, @{ $k->zyg };
        } else {
            push @kids, $k;
        }
    }
    (@kids == 1) ? $kids[0] : RxOp::Sequence->new(zyg => \@kids);
}

sub RxOp::Sequence::mayback { my ($self) = @_;
    for (@{ $self->zyg }) {
        return 1 if $_->mayback;
    }
    return 0;
}

sub RxOp::Cut::rxsimp { my ($self, $cut) = @_;
    return $self->zyg->[0]->rxsimp(0) if !$self->zyg->[0]->mayback;

    return RxOp::Cut->new(zyg => [$self->zyg->[0]->rxsimp(1)]);
}

1;
