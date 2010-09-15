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

    # all that matters is 0-1-infty; $*in_quant valid here
    sub used_caps {
        my %r;
        for my $k (@{ $_[0]->zyg }) {
            my $re = $k->used_caps;
            for my $cn (keys %$re) {
                $r{$cn} += $re->{$cn};
            }
        }
        \%r;
    }

    my $nlabel = 0;
    sub label { "b" . ($nlabel++) }

    sub lad2cgop {
        my ($l) = @_;
        my $r = ref $l;
        if (!$r) {
            return CgOp::clr_string($l);
        } elsif ($r eq 'ARRAY') {
            if (!@$l || ref ($l->[0])) {
                return CgOp::rawnewarr('LAD', map { lad2cgop($_) } @$l);
            } else {
                my ($h,@r) = @$l;
                return CgOp::rawnew("LAD$h", map { lad2cgop($_) } @r);
            }
        } else {
            return $l;
        }
    }

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
        [ 'Str', $self->text ];
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

    sub used_caps { local $::in_quant = 1; $_[0]->zyg->[0]->used_caps }

    sub code {
        my ($self, $body) = @_;
        my @code;

        my $exit = $self->label;
        my $repeat = $self->label;
        my $middle = $self->label;

        my $min = $self->min;
        my $max = $self->max;

        # get the degenerate cases out the way
        if (defined($max)) {
            return CgOp::goto('backtrack') if $max < $min;
            return CgOp::prog() if $max == 0;
            return $self->zyg->[0]->code($body) if $max == 1 && $min == 1;
        }

        my $usequant = (defined($max) && $max != 1) || ($min > 1);
        my $userep   = !(defined($max) && $max == 1);

        push @code, CgOp::rxcall('OpenQuant') if $usequant;
        push @code, CgOp::goto($middle) if $min;
        push @code, CgOp::label($repeat) if $userep;
        # min == 0 or quant >= 1
        if ($min > 1) {
            # only allow exiting if min met
            push @code, CgOp::ternary(CgOp::compare('>=',
                    CgOp::rawcall(CgOp::rxframe, 'GetQuant'),
                    CgOp::int($min)),
                CgOp::rxpushb('QUANT', $exit), CgOp::prog());
        } else {
            # min automatically met
            push @code, CgOp::rxpushb('QUANT', $exit);
        }

        # if userep false, quant == 0
        if (defined($max) && $userep) {
            push @code, CgOp::cgoto('backtrack', CgOp::compare('>=',
                    CgOp::rawcall(CgOp::rxframe, 'GetQuant'),
                    CgOp::int($max)));
        }

        push @code, CgOp::label($middle) if $min;
        push @code, $self->zyg->[0]->code($body);
        push @code, CgOp::rxcall('IncQuant') if $usequant;
        if ($userep) {
            push @code, CgOp::goto($repeat);
        } else {
            # quant == 1
            # userep implies max == 1, min == 0; fall through
        }
        push @code, CgOp::label($exit);
        push @code, CgOp::sink(CgOp::rxcall('CloseQuant')) if $usequant;

        @code;
    }

    sub lad {
        my ($self) = @_;
        if ($self->minimal) { return [ 'Imp' ]; }
        my ($mi,$ma) = ($self->min, $self->max // -1);
        my $str;
        if ($mi == 0 && $ma == -1) { $str = 'Star' }
        if ($mi == 1 && $ma == -1) { $str = 'Plus' }
        if ($mi == 0 && $ma == 1) { $str = 'Opt' }

        if ($str) {
            [ $str, $self->zyg->[0]->lad ];
        } else {
            [ 'Imp' ];
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
        [ 'Sequence', \@z ];
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
            push @code, CgOp::rxpushb("SEQALT", $ends[$i]) unless $i == $n - 1;
            push @code, $self->zyg->[$i]->code($body);
            push @code, CgOp::goto($ends[$n-1]) unless $i == $n-1;
            push @code, CgOp::label($ends[$i]);
        }

        @code;
    }

    sub lad { [ 'Imp' ] }

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
        push @code, CgOp::pushcut("BRACK");
        push @code, $self->zyg->[0]->code($body);
        push @code, CgOp::popcut();
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
        push @code, CgOp::pushcut("CUTGRP");
        push @code, $self->zyg->[0]->code($body);
        push @code, CgOp::rawcall(CgOp::rxframe, 'CommitGroup',
            CgOp::clr_string("CUTGRP"));
        push @code, CgOp::popcut();

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
    package RxOp::BeforeString;
    use Moose;
    extends 'RxOp';

    has str => (is => 'ro', isa => 'Str', required => 1);

    sub code {
        my ($self, $body) = @_;
        CgOp::rxbprim('BeforeStr', CgOp::bool(0),
            CgOp::clr_string($self->str));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::NotBeforeString;
    use Moose;
    extends 'RxOp';

    has str => (is => 'ro', isa => 'Str', required => 1);

    sub code {
        my ($self, $body) = @_;
        CgOp::rxbprim('BeforeStr', CgOp::bool(1),
            CgOp::clr_string($self->str));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::ZeroWidth;
    use Moose;
    extends 'RxOp';

    has type => (isa => 'Str', is => 'ro', required => 1);

    my %map = ('<<' => 0, '>>' => 1, '^' => 2, '$' => 3, '^^' => 4, '$$' => 5);
    sub code {
        my ($self, $body) = @_;
        CgOp::rxbprim('ZeroWidth', CgOp::int($map{$self->type}));
    }

    sub lad { [ 'Null' ] }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Before;
    use Moose;
    extends 'RxOp';

    sub code {
        my ($self, $body) = @_;

        RxOp::NotBefore->new(zyg => [RxOp::NotBefore->new(zyg => $self->zyg)])
            ->code($body);
    }

    sub lad {
        my ($self) = @_;
        [ 'Sequence', [ $self->zyg->[0]->lad, [ 'Imp' ] ] ];
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::NotBefore;
    use Moose;
    extends 'RxOp';

    sub code {
        my ($self, $body) = @_;

        my $pass = $self->label;
        my @code;
        push @code, CgOp::pushcut("CUTGRP");
        push @code, CgOp::rxpushb("CUTGRP", $pass);
        push @code, $self->zyg->[0]->code($body);
        push @code, CgOp::rxcall('CommitGroup', CgOp::clr_string("CUTGRP"));
        push @code, CgOp::goto('backtrack');
        push @code, CgOp::label($pass);
        push @code, CgOp::popcut;

        @code;
    }

    sub lad {
        my ($self) = @_;
        [ 'Null' ];
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Subrule;
    use Moose;
    extends 'RxOp';

    has name     => (isa => 'Str', is => 'ro', required => 1);
    has captures => (isa => 'ArrayRef[Maybe[Str]]', is => 'ro', default => sub { [] });
    has arglist  => (isa => 'Maybe[ArrayRef[Op]]', is => 'ro');
    has selfcut  => (isa => 'Bool', is => 'ro', default => 0);

    sub used_caps {
        my ($self) = @_;
        +{ map { $_ => $::in_quant ? 2 : 1 } @{ $self->captures } };
    }

    sub true {
        my ($self) = @_;
        # all not quite right in the capturey case
        if ($self->name eq 'sym') {
            return RxOp::String->new(text => $::symtext);
        }
        if ($self->name eq 'before') {
            return RxOp::Before->new(zyg => $self->zyg);
        }
        if ($self->name eq 'after') {
            return RxOp::After->new(zyg => $self->zyg);
        }
    }

    sub code {
        my ($self, $body) = @_;
        my $bt = $self->label;
        my $sk = $self->label;

        if (my $true = $self->true) {
            return $true->code($body);
        }

        my $namesf = CgOp::const(CgOp::rawnewarr('String',
                map { CgOp::clr_string($_) } @{ $self->captures }));
        my $callf = CgOp::methodcall(CgOp::newscalar(
                CgOp::rxcall("MakeCursor")), $self->name);
        my @pushcapf = (@{ $self->captures } == 0) ? () : (
            CgOp::rxcall("PushCapture", $namesf,
                CgOp::cast('Cursor', CgOp::letvar("k"))));
        my $updatef = CgOp::prog(
            CgOp::ncgoto('backtrack', CgOp::rawcall(CgOp::letvar("k"),
                    'IsDefined')),
            @pushcapf,
            CgOp::rxcall("SetPos", CgOp::getfield("pos", CgOp::cast("Cursor",
                        CgOp::letvar("k")))));

        my @code;

        if ($self->selfcut) {
            push @code, CgOp::letn(
                "k", CgOp::fetch(CgOp::rawsccall('Kernel.GetFirst:c,Variable',
                    CgOp::fetch($callf))),
                $updatef);
        } else {
            push @code, CgOp::rxcall("SetCursorList", $callf);
            push @code, CgOp::goto($sk);
            push @code, CgOp::label($bt);
            push @code, CgOp::sink(CgOp::methodcall(CgOp::rxcall(
                        "GetCursorList"), "shift"));
            push @code, CgOp::label($sk);
            push @code, CgOp::letn(
                "k", CgOp::fetch(CgOp::rawsccall('Kernel.GetFirst:c,Variable',
                    CgOp::fetch(CgOp::rxcall("GetCursorList")))),
                $updatef);
            push @code, CgOp::rxpushb("SUBRULE", $bt);
            push @code, CgOp::rxcall("SetCursorList", CgOp::null("Variable"));
        }

        @code;
    }

    sub lad {
        my ($self) = @_;
        if (my $true = $self->true) {
            return $true->lad;
        }
        [ 'Method', $self->name ];
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Sigspace;
    use Moose;
    extends 'RxOp';
    has selfcut => (isa => 'Bool', is => 'ro', default => 0);

    sub code {
        my ($self, $body) = @_;
        RxOp::Subrule->new(name => 'ws',
            selfcut => $self->selfcut)->code($body);
    }

    sub lad {
        my ($self) = @_;
        [ 'Imp' ];
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::CutLTM;
    use Moose;
    extends 'RxOp';

    sub code {
        my ($self, $body) = @_;
        CgOp::rxcall('CommitGroup', CgOp::clr_string("LTM"))
    }

    sub lad {
        my ($self) = @_;
        [ 'Imp' ]; #special case
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::CutRule;
    use Moose;
    extends 'RxOp';

    sub code {
        my ($self, $body) = @_;
        CgOp::rxcall('CommitRule');
    }

    sub lad {
        my ($self) = @_;
        [ 'Null' ];
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Alt;
    use Moose;
    extends 'RxOp';

    sub used_caps {
        my %used;
        for my $x (@{ $_[0]->zyg }) {
            my $used_br = $x->used_caps;
            for my $y (keys %$used_br) {
                $used{$y} = $used_br->{$y} if $used_br->{$y} > ($used{$y} // 0);
            }
        }
        \%used;
    }

    has lads => (is => 'ro', isa => 'ArrayRef', lazy => 1,
        default => sub { [ map { $_->lad } @{ $_[0]->zyg } ] });

    sub code {
        my ($self, $body) = @_;
        my @ls = map { $self->label } @{ $self->zyg };
        my $end = $self->label;

        my @code;
        push @code, CgOp::rxcall("LTMPushAlts",
            CgOp::rawscall('Lexer.GetLexer',
                CgOp::rxcall('GetClass'),
                CgOp::const(RxOp::lad2cgop($self->lads)),
                CgOp::clr_string('')),
            CgOp::const(CgOp::rawnewarr('Int32', map { CgOp::labelid($_) } @ls)));
        push @code, CgOp::goto('backtrack');
        for (my $i = 0; $i < @ls; $i++) {
            push @code, CgOp::label($ls[$i]);
            push @code, $self->zyg->[$i]->code($body);
            push @code, CgOp::goto($end) unless $i == @ls - 1;
        }
        push @code, CgOp::label($end);
        push @code, CgOp::popcut;
        @code;
    }

    sub lad {
        my ($self) = @_;
        [ 'Any', [ map { $_->lad } @{ $self->zyg } ] ];
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::VoidBlock;
    use Moose;
    extends 'RxOp';

    has block => (isa => 'Op', is => 'ro', required => 1);
    sub opzyg { $_->block }

    sub code {
        my ($self, $body) = @_;
        CgOp::subcall(CgOp::fetch($self->block->cgop($body)),
            CgOp::newscalar(CgOp::rxcall("MakeCursor")));
    }

    sub lad {
        my ($self) = @_;
        [ 'Imp' ];
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

    sub code {
        my ($self, $body) = @_;
        # will probably break with complicated harnesses
        CgOp::letn(
          "fns", CgOp::rawscall('Lexer.RunProtoregex',
            CgOp::fetch(CgOp::scopedlex('$¢')),
            CgOp::clr_string($self->name)),
          "i",   CgOp::int(0),
          "ks",  CgOp::null('Variable'),
          "k",   CgOp::null('IP6'),
          CgOp::pushcut('LTM'),
          CgOp::label('nextfn'),
          CgOp::cgoto('backtrack',
            CgOp::compare('>=', CgOp::letvar("i"),
              CgOp::getfield("Length", CgOp::letvar("fns")))),
          CgOp::rxpushb('LTM', 'nextfn'),
          CgOp::letvar("ks", CgOp::subcall(CgOp::getindex(CgOp::letvar("i"),
                CgOp::letvar("fns")), CgOp::newscalar(CgOp::rxcall(
                  'MakeCursor')))),
          CgOp::letvar("i", CgOp::arith('+', CgOp::letvar("i"), CgOp::int(1))),
          CgOp::letvar("k", CgOp::fetch(CgOp::rawsccall('Kernel.GetFirst:c,Variable',
                CgOp::fetch(CgOp::letvar("ks"))))),
          CgOp::ncgoto('backtrack',
            CgOp::rawcall(CgOp::letvar("k"), 'IsDefined')),
          CgOp::rawccall(CgOp::rxframe, 'End', CgOp::cast('Cursor',
              CgOp::letvar("k"))),
          CgOp::letvar('ks', CgOp::methodcall(CgOp::methodcall(
                CgOp::letvar('ks'), "list"), "clone")),
          CgOp::sink(CgOp::methodcall(CgOp::letvar('ks'), 'shift')),
          CgOp::label('nextcsr'),
          CgOp::ncgoto('backtrack', CgOp::unbox('Boolean', CgOp::fetch(
                CgOp::methodcall(CgOp::letvar('ks'), 'Bool')))),
          CgOp::rxpushb('SUBRULE', 'nextcsr'),
          CgOp::rawccall(CgOp::rxframe, 'End', CgOp::cast('Cursor',
              CgOp::fetch(CgOp::methodcall(CgOp::letvar('ks'), 'shift')))),
          CgOp::goto('backtrack'));
    }

    sub lad {
        my ($self) = @_;
        $self->cutltm ? [ 'Imp' ] :
            [ 'ProtoRegex', $self->name ];
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Any;
    use Moose;
    extends 'RxOp';

    sub code {
        my ($self, $body) = @_;
        CgOp::rxbprim("AnyChar");
    }

    sub lad {
        my ($self) = @_;
        [ 'Dot' ];
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

# generated by optimizer so needs no lad; always greedy
{
    package RxOp::QuantCClass;
    use Moose;
    extends 'RxOp';

    has cc => (isa => 'CClass', is => 'ro', required => 1);
    has min => (isa => 'Int', is => 'ro', required => 1);
    has max => (isa => 'Maybe[Int]', is => 'ro', default => undef);

    sub code {
        my ($self, $body) = @_;
        CgOp::rxbprim("ScanCClass", CgOp::int($self->min),
            CgOp::int($self->max // (2**31-1)),
            CgOp::const(RxOp::CClassElem::ccop($self)));
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

    sub code {
        my ($self, $body) = @_;
        CgOp::rxbprim("CClass", CgOp::const($self->ccop));
    }

    sub lad {
        my ($self) = @_;
        [ 'CC', $self->ccop ];
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::None;
    use Moose;
    extends 'RxOp';

    sub code {
        my ($self, $body) = @_;
        CgOp::goto('backtrack');
    }

    sub lad {
        my ($self) = @_;
        [ 'None' ];
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
