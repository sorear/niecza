use strict;
use warnings;
use 5.010;
use utf8;

use CgOp;

{
    package RxOp;
    use Moose;

    has zyg => (isa => 'ArrayRef[RxOp]', is => 'ro', default => sub { [] });

    my $i = 0;
    sub _closurize {
        my ($self, $op) = @_;
        Op::SubDef->new(var => 'rx!' . ($i++), class => 'Regex', body =>
            Body->new(
                type        => 'regex',
                signature   => Sig->simple('$¢'),
# XXX transparent bodies with signatures are not yet handled well
#                transparent => 1,
                do          => $op));
    }

    sub closure {
        my ($self) = @_;
        $self->_closurize($self->op);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::String;
    use Moose;
    extends 'RxOp';

    has text => (isa => 'Str', is => 'ro', required => 1);

    sub op {
        my ($self) = @_;
        Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxstr'),
            positionals => [
                Op::Lexical->new(name => '$¢'),
                Op::StringLiteral->new(text => $self->text)]);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Export;
    use Moose;
    extends 'RxOp';

    # zyg * 1

    sub op {
        my ($self) = @_;
        Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxexport'),
            positionals => [$self->zyg->[0]->op]);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Quantifier;
    use Moose;
    extends 'RxOp';

    has type => (isa => 'Str', is => 'ro', required => 1);
    # ? + * only
    # zyg * 1

    my %qf = ( '+', 'plus', '*', 'star', '?', 'opt' );
    sub op {
        my ($self) = @_;
        Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rx' . $qf{$self->type}),
            positionals => [
                Op::Lexical->new(name => '$¢'),
                $self->zyg->[0]->closure]);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Sequence;
    use Moose;
    extends 'RxOp';

    # zyg * N

    sub op {
        my ($self) = @_;
        my @zyg = map { $_->op } @{ $self->zyg };

        while (@zyg >= 2) {
            my $r = pop @zyg;
            my $l = pop @zyg;
            push @zyg, Op::CallSub->new(
                invocant    => Op::Lexical->new(name => '&_rxlazymap'),
                positionals => [ $l, $self->_closurize($r) ]);
        }

        $zyg[0] || Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxone'),
            positionals => [ Op::Lexical->new(name => '$¢') ]);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
