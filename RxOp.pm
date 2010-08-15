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

    sub lad {
        my ($self) = @_;
        CgOp::rawnew('LADStr', CgOp::clr_string($self->text));
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
    sub op { goto &{ $_[0]->can('op_' . $qf{$_[0]->type}) }; }

    sub op_opt {
        my ($self, $cn, $cont) = @_;
        my $kcl = $self->_close_k($cn, $cont);
        my $zzcn = Niecza::Actions->gensym;
        my ($zcn, $zcont) = $self->zyg->[0]->op($zzcn,
            Op::CallSub->new(invocant => Op::Lexical->new(name => $kcl->var),
                positionals => [Op::Lexical->new(name => $zzcn)]));
        $zcn, Op::StatementList->new(children => [
                $kcl, $zcont, Op::CallSub->new(
                    invocant => Op::Lexical->new(name => $kcl->var),
                    positionals => [Op::Lexical->new(name => $zcn)])]);
    }

    # (sub loop($C) { zyg($C, &loop); cont($C) })($C)
    sub op_star {
        my ($self, $cn, $cont) = @_;
        my $lpn =  Niecza::Actions->gensym;
        my $zzcn = Niecza::Actions->gensym;
        my ($zcn, $zcont) = $self->zyg->[0]->op($zzcn, Op::CallSub->new(
                invocant => Op::Lexical->new(name => $lpn),
                positionals => [Op::Lexical->new(name => $zzcn)]));
        $cn, Op::CallSub->new(
            invocant => Op::SubDef->new(var => $lpn, class => 'Sub', body =>
                Body->new(type => 'sub', signature => Sig->simple($zcn), do =>
                    Op::StatementList->new(children => [ $zcont,
                            Op::CallSub->new(
                                invocant => $self->_close_k($cn, $cont),
                                positionals => [Op::Lexical->new(name => $zcn)])]))),
            positionals => [Op::Lexical->new(name => $cn)]);
    }

    # (sub loop($C) { zyg($C, -> $nC { loop($nC); cont($nC) }) })($C)
    sub op_plus {
        my ($self, $cn, $cont) = @_;
        my $lpn =  Niecza::Actions->gensym;
        my ($zcn, $zcont) = $self->zyg->[0]->op($cn, Op::StatementList->new(
                children => [
                    Op::CallSub->new(
                        invocant => Op::Lexical->new(name => $lpn),
                        positionals => [Op::Lexical->new(name => $cn)]),
                    $cont
                ]));
        $cn, Op::CallSub->new(
            invocant => Op::SubDef->new(var => $lpn, class => 'Sub', body =>
                Body->new(type => 'sub', signature => Sig->simple($zcn), do =>
                    $zcont)),
            positionals => [Op::Lexical->new(name => $cn)]);
    }

    sub lad {
        my ($self) = @_;
        $self->minimal ? CgOp::rawnew('LADImp') :
            CgOp::rawnew('LAD' . ucfirst($qf{$self->type}),
                    $self->zyg->[0]->lad);
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

    sub lad {
        my ($self) = @_;
        my @z = map { $_->lad } @{ $self->zyg };
        while (@z >= 2) {
            my $x = pop @z;
            $z[-1] = CgOp::rawnew('LADSequence', $z[-1], $x);
        }
        $z[0] // CgOp::rawnew('LADNull');
    }


    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::ConfineLang;
    use Moose;
    extends 'RxOp';

    # TODO once :lang is implemented, this will be a bit more complicated
    sub op {
        my ($self, $cn, $cont) = @_;
        $self->zyg->[0]->op($cn, $cont);
    }

    sub lad {
        my ($self) = @_;
        $self->zyg->[0]->lad;
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

    sub lad {
        my ($self) = @_;
        $self->zyg->[0]->lad;
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

    sub lad {
        my ($self) = @_;
        $self->zyg->[0]->lad;
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
        if ($self->name eq 'sym') {
            # Should probably be a separate pass (before rx optimizer)
            return $icn, Op::CallSub->new(
                invocant => Op::Lexical->new(name => '&_rxstr'),
                positionals => [
                    Op::Lexical->new(name => $icn),
                    Op::StringLiteral->new(text => $::symtext),
                    $self->_close_k($cn, $cont)
                ]);
        }

        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxcall'),
            positionals => [
                Op::CallMethod->new(name => $self->name,
                    receiver => Op::Lexical->new(name => $icn)),
                $self->_close_k($cn, $cont)]);
    }

    sub lad {
        my ($self) = @_;
        if ($self->name eq 'sym') {
            return CgOp::rawnew('LADStr', CgOp::clr_string($::symtext));
        }
        CgOp::rawnew('LADMethod', CgOp::clr_string($self->name));
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

    sub lad {
        my ($self) = @_;
        CgOp::rawnew('LADImp');
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Alt;
    use Moose;
    extends 'RxOp';

    sub op {
        my ($self, $cn, $cont) = @_;
        my $icn = Niecza::Actions->gensym;
        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxalt'),
            positionals => [
                Op::Lexical->new(name => $icn),
                Op::CgOp->new(op => CgOp::wrap(CgOp::rawnewarr('LAD',
                            map { $_->lad } @{ $self->zyg }))), #XXX
                $self->_close_k($cn, $cont),
                map { $self->_close_op($_) } @{ $self->zyg }
            ]);
    }

    sub lad {
        my ($self) = @_;
        CgOp::rawnew('LADAny', CgOp::rawnewarr('LAD',
                map { $_->lad } @{ $self->zyg }));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::VoidBlock;
    use Moose;
    extends 'RxOp';

    has block => (isa => 'Op', is => 'ro', required => 1);

    sub op {
        my ($self, $cn, $cont) = @_;
        $cn, Op::StatementList->new(children => [$self->block, $cont]);
    }

    sub lad {
        my ($self) = @_;
        CgOp::rawnew('LADImp');
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::ProtoRedis;
    use Moose;
    extends 'RxOp';

    has name    => (isa => 'Str', is => 'ro', required => 1);
    has cutltm  => (isa => 'Bool', is => 'ro', default => 0);

    sub op {
        my ($self, $cn, $cont) = @_;
        my $icn = Niecza::Actions->gensym;
        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxproto'),
            positionals => [
                Op::Lexical->new(name => $icn),
                $self->_close_k($cn, $cont),
                Op::StringLiteral->new(text => $self->name)
            ]);
    }

    sub lad {
        my ($self) = @_;
        $self->cutltm ? CgOp::rawnew('LADImp') :
            CgOp::rawnew('LADProtoRegex', CgOp::clr_string($self->name));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
