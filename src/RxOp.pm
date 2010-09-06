use strict;
use warnings;
use 5.010;
use utf8;

use CgOp;

{
    package RxOp;
    use Moose;

    has zyg => (isa => 'ArrayRef[RxOp]', is => 'ro', default => sub { [] });

    sub opzyg { map { $_->opzyg } @{ $_[0]->zyg } }

    my $nlabel = 0;
    sub label { "b" . ($nlabel++) }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::String;
    use Moose;
    extends 'RxOp';

    has text => (isa => 'Str', is => 'ro', required => 1);

    sub code {
        my ($self, $body) = @_;
        my $t = $self->text;
        if (length($t) == 1) {
            CgOp::rxbprim('ExactOne', CgOp::char($t));
        } else {
            CgOp::rxbprim('Exact', CgOp::clr_string($t));
        }
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

    has minimal => (isa => 'Bool', is => 'ro', required => 1);
    has min => (isa => 'Int', is => 'ro', required => 1);
    has max => (isa => 'Maybe[Int]', is => 'ro', default => undef);

    sub code {
        my ($self, $body) = @_;
        my @code;

        my $exit = $self->label;
        my $repeat = $self->label;

        push @code, CgOp::rawcall(CgOp::rxframe, 'OpenQuant');
        push @code, CgOp::label($repeat);
        push @code, CgOp::ternary(CgOp::compare('>=',
                CgOp::rawcall(CgOp::rxframe, 'GetQuant'),
                CgOp::int($self->min)),
            CgOp::rxpushb('QUANT', $exit), CgOp::prog());
        if (defined $self->max) {
            push @code, CgOp::ternary(CgOp::compare('>=',
                    CgOp::rawcall(CgOp::rxframe, 'GetQuant'),
                    CgOp::int($self->max)),
                CgOp::rawccall(CgOp::rxframe, 'Backtrack'), CgOp::prog());
        }
        push @code, $self->zyg->[0]->code($body);
        push @code, CgOp::rawcall(CgOp::rxframe, 'IncQuant');
        push @code, CgOp::goto($repeat);
        push @code, CgOp::label($exit);
        push @code, CgOp::rawcall(CgOp::rxframe, 'CloseQuant');

        @code;
    }

    sub lad {
        my ($self) = @_;
        if ($self->minimal) { return CgOp::rawnew('LADImp'); }
        my ($mi,$ma) = ($self->min, $self->max // -1);
        my $str;
        if ($mi == 0 && $ma == -1) { $str = 'Star' }
        if ($mi == 1 && $ma == -1) { $str = 'Plus' }
        if ($mi == 0 && $ma == 1) { $str = 'Opt' }

        if ($str) {
            CgOp::rawnew("LAD$str", $self->zyg->[0]->lad);
        } else {
            CgOp::rawnew("LADImp");
        }
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Sequence;
    use Moose;
    extends 'RxOp';

    # zyg * N

    sub code {
        my ($self, $body) = @_;

        CgOp::prog(map { $_->code($body) } @{ $self->zyg });
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
    package RxOp::SeqAlt;
    use Moose;
    extends 'RxOp';

    # zyg * N

    sub code {
        my ($self, $body) = @_;

        my @ends = map { $self->label } @{ $self->zyg };
        my @code;
        my $n = @{ $self->zyg };

        for (my $i = 0; $i < $n; $i++) {
            push @code, CgOp::rxpushb("SEQALT",
                ($i == $n - 1) ? undef : $ends[$i]);
            push @code, $self->zyg->[$i]->code($body);
            push @code, CgOp::goto($ends[$n-1]) unless $i == $n-1;
            push @code, CgOp::label($ends[$i]);
        }

        push @code, CgOp::rxpushb("ENDSEQALT");
        @code;
    }

    sub lad { $_[0]->zyg->[0]->lad }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::ConfineLang;
    use Moose;
    extends 'RxOp';

    # TODO once :lang is implemented, this will be a bit more complicated
    sub code {
        my ($self, $body) = @_;
        my @code;
        push @code, CgOp::rxpushb("BRACK");
        push @code, $self->zyg->[0]->code($body);
        push @code, CgOp::rxpushb("ENDBRACK");
        @code;
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

    sub code {
        my ($self, $body) = @_;

        my @code;
        push @code, CgOp::rxpushb("CUTGRP");
        push @code, $self->zyg->[0]->code($body);
        push @code, CgOp::rawcall(CgOp::rxframe, 'CommitGroup',
            CgOp::clr_string("CUTGRP"), CgOp::clr_string("ENDCUTGRP"));
        push @code, CgOp::rxpushb("ENDCUTGRP");

        @code;
    }

    sub lad {
        my ($self) = @_;
        $self->zyg->[0]->lad;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Before;
    use Moose;
    extends 'RxOp';

    sub op {
        my ($self, $cn, $cont) = @_;

        my $icn = Niecza::Actions->gensym;
        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxbefore'),
            positionals => [
                Op::Lexical->new(name => $icn),
                $self->_close_op($self->zyg->[0]),
                $self->_close_k($cn, $cont)]);
    }

    sub lad {
        my ($self) = @_;
        CgOp::rawnew('LADSequence', $self->zyg->[0]->lad,
            CgOp::rawnew('LADImp'));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::NotBefore;
    use Moose;
    extends 'RxOp';

    sub op {
        my ($self, $cn, $cont) = @_;

        my $icn = Niecza::Actions->gensym;
        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxnotbefore'),
            positionals => [
                Op::Lexical->new(name => $icn),
                $self->_close_op($self->zyg->[0]),
                $self->_close_k($cn, $cont)]);
    }

    sub lad {
        my ($self) = @_;
        CgOp::rawnew('LADNull');
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
        $self->block->invocant->body->cname($cn); #XXX
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

{
    package RxOp::Any;
    use Moose;
    extends 'RxOp';

    sub op {
        my ($self, $cn, $cont) = @_;
        my $icn = Niecza::Actions->gensym;
        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxdot'),
            positionals => [
                Op::Lexical->new(name => $icn),
                $self->_close_k($cn, $cont)
            ]);
    }

    sub lad {
        my ($self) = @_;
        CgOp::rawnew('LADDot');
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::CClassElem;
    use Moose;
    extends 'RxOp';

    has cc => (isa => 'CClass', is => 'ro', required => 1);

    # TODO: some kind of constant table
    sub ccop {
        my ($self) = @_;
        my @ints = @{ $self->cc };
        CgOp::rawnew('CC', CgOp::rawnewarr('int',
                map { CgOp::int($_) } @ints));
    }

    sub op {
        my ($self, $cn, $cont) = @_;
        my $icn = Niecza::Actions->gensym;
        $icn, Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&_rxcc'),
            positionals => [
                Op::Lexical->new(name => $icn),
                Op::CgOp->new(op => CgOp::wrap($self->ccop)),
                $self->_close_k($cn, $cont)
            ]);
    }

    sub lad {
        my ($self) = @_;
        CgOp::rawnew('LADCC', $self->ccop);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::None;
    use Moose;
    extends 'RxOp';

    sub op {
        my ($self, $cn, $cont) = @_;
        my $icn = Niecza::Actions->gensym;
        $icn, Op::StatementList->new(children => []);
    }

    sub lad {
        my ($self) = @_;
        CgOp::rawnew('LADNone');
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
