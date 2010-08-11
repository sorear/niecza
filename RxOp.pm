use strict;
use warnings;
use 5.010;
use utf8;

use CgOp;

{
    package RxOp;
    use Moose;

    has zyg => (isa => 'ArrayRef[RxOp]', is => 'ro', default => sub { [] });

    # op(cn, cont): provides cn in environment, calls cont per result, then
    # returns; -> (cn, cont)
    # closure: like op but just returns a function, takes cn/cont though

    sub _close {
        my ($self, $type, $parms, $op) = @_;
        Op::SubDef->new(var => Niecza::Actions->gensym, class => ucfirst($type),
            body => Body->new(
                type        => $type,
                signature   => Sig->simple(@$parms),
                do          => $op));
    }

    sub _close_k {
        my ($self, $cn, $cont) = @_;
        $self->_close('sub', [$cn], $cont);
    }

    sub _close_op {
        my ($self, $op) = @_;
        my $icn   = Niecza::Actions->gensym;
        my $icv   = Niecza::Actions->gensym;
        my $icont = Op::CallSub->new(
            invocant => Op::Lexical->new(name => $icv),
            positionals => [ Op::Lexical->new(name => $icn) ]);
        my ($cn, $cont) = $op->op($icn, $icont);
        $self->_close('sub', [$cn, $icv], $cont);
    }

    sub term_rx {
        my ($self) = @_;
        my $icn   = Niecza::Actions->gensym;
        my $icont = Op::Take->new(value => Op::Lexical->new(name => $icn));
        my ($cn, $cont) = $self->op($icn, $icont);
        $cn, Op::Gather->new(
            var => Niecza::Actions->gensym,
            body => Body->new(type => 'gather', do => $cont));
    }

    sub close_rx {
        my ($self) = @_;
        my ($cn, $op) = $self->term_rx;
        $self->_close('regex', [$cn], $op);
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
        my ($self, $cn, $cont) = @_;
        my $icn = Niecza::Actions->gensym;
        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxstr'),
            positionals => [
                Op::Lexical->new(name => $icn),
                Op::StringLiteral->new(text => $self->text),
                $self->_close_k($cn, $cont)
            ]);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Quantifier;
    use Moose;
    extends 'RxOp';

    has type => (isa => 'Str', is => 'ro', required => 1);
    has minimal => (isa => 'Bool', is => 'ro', required => 1);
    # ? + * only
    # zyg * 1

    my %qf = ( '+', 'plus', '*', 'star', '?', 'opt' );
    sub op {
        my ($self, $cn, $cont) = @_;
        my $icn = Niecza::Actions->gensym;
        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rx' . $qf{$self->type} .
                ($self->minimal ? 'g' : '')),
            positionals => [
                Op::Lexical->new(name => $icn),
                $self->_close_op($self->zyg->[0]),
                $self->_close_k($cn, $cont)]);
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
        my ($self, $cn, $cont) = @_;

        for (reverse @{ $self->zyg }) {
            ($cn, $cont) = $_->op($cn, $cont);
        }

        $cn, $cont;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Cut;
    use Moose;
    extends 'RxOp';

    # zyg * N

    sub op {
        my ($self, $cn, $cont) = @_;

        my $icn = Niecza::Actions->gensym;
        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxcut'),
            positionals => [
                Op::Lexical->new(name => $icn),
                $self->_close_op($self->zyg->[0]),
                $self->_close_k($cn, $cont)]);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Capture;
    use Moose;
    extends 'RxOp';

    has names => (isa => 'ArrayRef[Maybe[Str]]', is => 'ro', required => 1);

    sub op {
        my ($self, $cn, $cont) = @_;
        my $icn = Niecza::Actions->gensym;
        my @n = @{ $self->names };
        for (@n) {
            $::parennum = $_ if defined($_) && $_ =~ /^[0-9]+$/;
            $_ = $::parennum++ if !defined($_);
        }
        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxbind'),
            positionals => [
                Op::Lexical->new(name => $icn),
                Op::CallSub->new(
                    invocant => Op::Lexical->new(name => '&infix:<,>'),
                    positionals => [
                        map { Op::StringLiteral->new(text => $_) }
                            @{ $self->names }
                    ]),
                $self->_close_op($self->zyg->[0]),
                $self->_close_k($cn, $cont)]);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::CallMethod;
    use Moose;
    extends 'RxOp';

    has name => (isa => 'Str', is => 'ro', required => 1);

    sub op {
        my ($self, $cn, $cont) = @_;
        my $icn = Niecza::Actions->gensym;
        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxcall'),
            positionals => [
                Op::CallMethod->new(name => $self->name,
                    receiver => Op::Lexical->new(name => $icn)),
                $self->_close_k($cn, $cont)]);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Sigspace;
    use Moose;
    extends 'RxOp';

    sub op {
        my ($self, $cn, $cont) = @_;
        my $icn = Niecza::Actions->gensym;
        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxcall'),
            positionals => [
                Op::CallMethod->new(name => 'ws',
                    receiver => Op::Lexical->new(name => $icn)),
                $self->_close_k($cn, $cont)]);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
