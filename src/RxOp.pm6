class RxOp;

use CgOp;
use CClass;

has $.zyg = []; # Array of RxOp

method opzyg()  { map *.opzyg, @$!zyg }
method oplift() { map *.oplift, @$!zyg }
method uncut()  { self }

method check()  { for @$!zyg { $_.check } }
method tocclist() { CClass }

# all that matters is 0-1-infty; $*in_quant valid here
method used_caps() {
    # XXX Hash.push
    my %r;
    for @$!zyg -> $k {
        for $k.used_caps.pairs -> $p {
            %r{$p.key} = (%r{$p.key} // 0) + $p.value;
        }
    }
    %r;
}

my $nlabel = 0;
method label() { "b" ~ ($nlabel++) }

class Capturing is RxOp {
    has $.captures = []; # Array of Str

    method check() {
        for @$.captures -> $c is rw {
            if !defined($c) {
                $c = $*paren++;
            } elsif $c ~~ /^<[ 0..9 ]>+$/ {
                $*paren = $c + 1;
            }
        }
        for @$.zyg { $_.check }
    }

    method used_caps() {
        my %h = map { ($_ => $*in_quant ?? 2 !! 1) }, @$.captures;
        %h
    }
}

class Sym is Capturing {
    has $.text; # Str, is rw
    has $.endsym; # Str, is rw
    has $.igcase; # Bool
    has $.igmark; # Bool

    method clone(:$captures) {
        self.WHAT.new(text => $.text, igcase => $.igcase, igmark => $.igmark,
            :$captures);
    }

    method check() { $.text = $*symtext; $.endsym = $*endsym; nextsame }

    method code($body) { #OK not used
        my $t = $.text;
        # We aren't going to make a real Match unless somebody comes up with
        # a good reason.
        my $p = CgOp.rxpushcapture(CgOp.string_var($t), @$.captures);
        my $ic = $.igcase ?? "NoCase" !! "";
        my @e = !defined($.endsym) ?? () !!
            ::RxOp::Subrule.new(method => $.endsym, :selfcut).code($body);
        if chars($t) == 1 {
            $p, CgOp.rxbprim("ExactOne$ic", CgOp.char($t)), @e;
        } else {
            $p, CgOp.rxbprim("Exact$ic", CgOp.str($t)), @e;
        }
    }

    method tocclist() { $!text.comb.map({ CClass.enum($_) }) }

    method lad() {
        my $m = [ ($!igcase ?? 'StrNoCase' !! 'Str'), $!text ];
        defined($!endsym) ?? [ 'Sequence', [$m, [ 'Method', $!endsym ]] ] !! $m;
    }
}

class String is RxOp {
    has $.text = die "RxOp::String.text required"; # Str
    has $.igcase; # Bool

    method code($body) { #OK not used
        my $t = $!text;
        my $ic = $!igcase ?? "NoCase" !! "";
        if chars($t) == 1 {
            CgOp.rxbprim("ExactOne$ic", CgOp.char($t));
        } else {
            CgOp.rxbprim("Exact$ic", CgOp.str($t));
        }
    }

    method tocclist() { $!text.comb.map({ CClass.enum($_) }) }

    method lad() {
        [ ($!igcase ?? 'StrNoCase' !! 'Str'), $!text ];
    }
}

class VarString is RxOp {
    has $.param; # Str
    has $.ops = die "RxOp::VarString.ops required"; # Op
    method opzyg() { $!ops }

    method code($body) {
        CgOp.rxbprim('Exact', CgOp.obj_getstr($!ops.cgop($body)));
    }

    method lad() { defined($!param) ?? ['Param', $!param] !! ['Imp'] }
}

class Quantifier is RxOp {
    has $.minimal = die "RxOp::Quantifier.minimal required"; # Bool
    has $.min = die "RxOp::Quantifier.min required"; # Int
    has $.max; # Int
    has $.nonlisty;

    method used_caps() {
        temp $*in_quant;
        $*in_quant = True unless $.nonlisty;
        # XXX callsame()
        my %r;
        for @$.zyg -> $k {
            for $k.used_caps -> $p {
                %r{$p.key} = (%r{$p.key} // 0) + $p.value;
            }
        }
        %r;
    }

    method mincode($body) {
        my @code;

        my $exit = self.label;
        my $add  = self.label;

        push @code, CgOp.rxopenquant;
        push @code, CgOp.goto($exit);
        push @code, CgOp.label($add);
        push @code, CgOp.cgoto('backtrack', CgOp.compare('>=',
                CgOp.rxgetquant, CgOp.int($!max))) if defined ($!max);
        if $.zyg[1] {
            push @code, $.zyg[1].code($body);
            push @code, CgOp.label($exit);
            push @code, $.zyg[0].code($body);
            push @code, CgOp.rxincquant;
        } else {
            push @code, $.zyg[0].code($body);
            push @code, CgOp.rxincquant;
            push @code, CgOp.label($exit);
        }
        push @code, CgOp.rxpushb('QUANT', $add);
        push @code, CgOp.cgoto('backtrack', CgOp.compare('<',
                CgOp.rxgetquant, CgOp.int($!min))) if $!min > 0;
        push @code, CgOp.sink(CgOp.rxclosequant);

        @code;
    }

    method code($body) {
        my @code;

        return self.mincode($body) if $!minimal;

        my $exit   = self.label;
        my $repeat = self.label;
        my $middle = self.label;

        my $min = $!min;
        my $max = $!max;

        # get the degenerate cases out the way
        if defined $max {
            return CgOp.goto('backtrack') if $max < $min;
            return CgOp.prog() if $max == 0;
            return $.zyg[0].code($body) if $max == 1 && $min == 1;
        }

        my $usequant = (defined($max) && $max != 1) || ($min > 1);
        my $userep   = !(defined($max) && $max == 1);

        push @code, CgOp.rxopenquant if $usequant;
        push @code, CgOp.goto($middle) if $min;
        push @code, CgOp.label($repeat) if $userep;
        # min == 0 or quant >= 1
        if $min > 1 {
            # only allow exiting if min met
            push @code, CgOp.ternary(CgOp.compare('>=',
                    CgOp.rxgetquant,
                    CgOp.int($min)),
                CgOp.rxpushb('QUANT', $exit), CgOp.prog());
        } else {
            # min automatically met
            push @code, CgOp.rxpushb('QUANT', $exit);
        }

        # if userep false, quant == 0
        if defined($max) && $userep {
            push @code, CgOp.cgoto('backtrack', CgOp.compare('>=',
                    CgOp.rxgetquant, CgOp.int($max)));
        }

        push @code, $.zyg[1].code($body) if $.zyg[1];
        push @code, CgOp.label($middle) if $min;
        push @code, $.zyg[0].code($body);
        push @code, CgOp.rxincquant if $usequant;
        if $userep {
            push @code, CgOp.goto($repeat);
        } else {
            # quant == 1
            # userep implies max == 1, min == 0; fall through
        }
        push @code, CgOp.label($exit);
        push @code, CgOp.sink(CgOp.rxclosequant) if $usequant;

        @code;
    }

    method lad() {
        return [ 'Imp' ] if $!minimal;
        my $mi = $!min;
        my $ma = $!max // -1;
        my $str;
        if $mi == 0 && $ma == -1 { $str = 'Star' }
        if $mi == 1 && $ma == -1 { $str = 'Plus' }
        if $mi == 0 && $ma ==  1 { $str = 'Opt' }

        if $str {
            [ $str, $.zyg[0].lad ];
        } else {
            [ 'Imp' ];
        }
    }
}

class Sequence is RxOp {
    # zyg * N

    method code($body) { CgOp.prog(map { $_.code($body) }, @$.zyg); }

    method lad() { [ 'Sequence', [ map { $_.lad }, @$.zyg ] ] }

    method tocclist() { map { $_.tocclist }, @$.zyg }
}

class AltBase is RxOp {
    has $.dba; # Str

    method check() {
        my $maxparen = $*paren;
        $!dba //= ($*dba // die "wtf no dba");

        for @$.zyg {
            temp $*paren;
            $_.check;
            if ($*paren > $maxparen) { $maxparen = $*paren }
        }

        $*paren = $maxparen;
    }

    method used_caps() {
        my %used;
        for @$.zyg -> $x {
            my $used_br = $x.used_caps;
            for keys $used_br -> $y {
                %used{$y} = $used_br{$y} if $used_br{$y} > (%used{$y} // 0);
            }
        }
        %used;
    }
}

class SeqAlt is AltBase {
    has $.dba; # is rw, Str
    # zyg * N

    method code($body) {
        my @ends = map { self.label }, @$.zyg;
        my @code;
        my $n = @$.zyg;

        my $i = 0;
        while $i < $n {
            push @code, CgOp.rxpushb("SEQALT", @ends[$i]) unless $i == $n - 1;
            push @code, $.zyg[$i].code($body);
            push @code, CgOp.goto(@ends[$n-1]) unless $i == $n-1;
            push @code, CgOp.label(@ends[$i]);
            $i++;
        }

        @code;
    }

    method lad() { $.zyg[0] ?? $.zyg[0].lad !! [ 'Imp' ] }
}

class ConfineLang is RxOp {
    # Note that BRACK automatically confines the language change
    method code($body) {
        my @code;
        push @code, CgOp.pushcut("BRACK");
        push @code, $.zyg[0].code($body);
        push @code, CgOp.popcut;
        @code;
    }

    method lad() { $.zyg[0].lad }
}

class Cut is RxOp {
    method uncut() { $.zyg[0] }
    method tocclist() { $.uncut.tocclist }
    method lad() { $.uncut.lad }

    method code($body) {
        my @code;
        push @code, CgOp.pushcut("CUTGRP");
        push @code, $.zyg[0].code($body);
        push @code, CgOp.rxcommitgroup(CgOp.str("CUTGRP"));
        push @code, CgOp.popcut;

        @code;
    }
}

class BeforeString is RxOp {
    has $.str = die "RxOp::BeforeString.str required"; # Str

    method code($) {
        CgOp.rxbprim('BeforeStr', CgOp.bool(0), CgOp.str($.str));
    }
}

class ZeroWidthCCs is RxOp {
    has $.ccs   = die "ZeroWidthCCs.ccs required"; # Array of CClass
    has $.after = die "ZeroWidthCCs.after required"; # Bool
    has $.neg   = die "ZeroWidthCCs.neg required"; # Bool

    method lad() { [ 'Null' ] }

    method code($) {
        CgOp.rxbprim(($!after ?? 'AfterCCs' !! 'BeforeCCs'),
            CgOp.bool(+$!neg), CgOp.const(CgOp.fcclist_new(
                    map { CgOp.cc_expr($_) }, @$!ccs)));
    }
}

class NotBeforeString is RxOp {
    has $.str = die "NotBeforeString.str required"; # Str

    method code($body) { #OK not used
        CgOp.rxbprim('BeforeStr', CgOp.bool(1), CgOp.str($!str));
    }
}

class ZeroWidth is RxOp {
    has $.type = die "ZeroWidth.type required"; # Str

    my %map = '<<' => 0, '>>' => 1, '^' => 2, '$' => 3, '^^' => 4, '$$' => 5;
    method code($) { CgOp.rxbprim('ZeroWidth', CgOp.int(%map{$!type})); }
    method lad() { [ 'Null' ] }
}

class NotBefore is RxOp {
    method code($body) {
        my $pass = self.label;
        my @code;
        push @code, CgOp.pushcut("NOTBEFORE");
        push @code, CgOp.rxpushb("NOTBEFORE", $pass);
        push @code, $.zyg[0].code($body);
        push @code, CgOp.rxcall('CommitGroup', CgOp.str("NOTBEFORE"));
        push @code, CgOp.goto('backtrack');
        push @code, CgOp.label($pass);
        push @code, CgOp.popcut;

        @code;
    }

    method lad() { ['Null'] }
}

class Before is RxOp {
    method code($body) {
        RxOp::NotBefore.new(zyg =>
            [RxOp::NotBefore.new(zyg => $.zyg)]).code($body);
    }

    method lad() { [ 'Sequence', [ $.zyg[0].lad, [ 'Imp' ] ] ]; }
}

class Tilde is RxOp {
    has $.closer = die "Tilde.closer required"; # Str
    has $.dba;

    method check() {
        $!dba //= $*dba;
        nextsame;
    }

    method code($body) {
        my @code;
        my $fail = self.label;
        my $pass = self.label;

        push @code, CgOp.pushcut("TILDE $!closer");
        push @code, CgOp.rxsetquant(CgOp.rxgetpos);
        push @code, $.zyg[0].code($body);
        push @code, CgOp.rxpushb("TILDE", $fail);
        push @code, CgOp.rxbprim('Exact', CgOp.str($!closer));
        push @code, CgOp.goto($pass);
        push @code, CgOp.label($fail);
        push @code, CgOp.sink(CgOp.methodcall(CgOp.newscalar(
                CgOp.rxcall("MakeCursor")), 'FAILGOAL',
            CgOp.string_var($!closer), CgOp.string_var($!dba),
            CgOp.box('Num', CgOp.cast('num', CgOp.rxgetquant))));
        push @code, CgOp.label($pass);
        push @code, CgOp.popcut;

        @code;
    }

    method lad() { ['Imp'] }
}

class Subrule is Capturing {
    has $.method; # Str
    has $.regex; # Op
    has $.passcap = False; # Bool
    has $._passcapzyg; # RxOp, is rw
    has $._passcapltm; # is rw
    has $.selfcut = False; # Bool
    has $.zerowidth; # Bool
    has $.negative; # Bool

    method clone(*%_) {
        self.WHAT.new(method => $!method, regex => $!regex,
            passcap => $!passcap, _passcapltm => $!_passcapltm,
            _passcapzyg => $!_passcapzyg, selfcut => $!selfcut,
            zerowidth => $!zerowidth, negative => $!negative,
            captures => $.captures, |%_);
    }

    method opzyg() { $!regex // Nil }

    method used_caps() {
        my %h = map { ($_ => $*in_quant ?? 2 !! 1) }, @$.captures;
        if $!passcap {
            my $h2 = $!_passcapzyg.used_caps;
            for keys $h2 { %h{$_} = (%h{$_} // 0) + $h2{$_} }
        }
        %h
    }

    method check() {
        if $!_passcapzyg {
            if $!passcap {
                $!_passcapzyg.check;
            } else {
                my $*paren = 0;
                $!_passcapzyg.check;
            }
        }
        nextsame;
    }

    method code($body) {
        my $bt = self.label;

        my $callf = $!regex ?? $!regex.cgop($body) !!
            CgOp.methodcall(CgOp.newscalar(CgOp.rxcall("MakeCursor")),
                $!method);
        my @newcapf = (!$.captures) ?? () !!
            CgOp.rxpushcapture(
                ($!passcap ??
                    CgOp.newscalar(CgOp.rxstripcaps(
                            CgOp.cast("cursor", CgOp.letvar("k")))) !!
                    CgOp.letvar("kv")), @$.captures);
        my @pushcapf = ($!passcap ??
            (CgOp.rxsetcapsfrom(CgOp.cast("cursor", CgOp.letvar("k")))) !! ()),
                @newcapf;
        my @bfargs = 'backtrack', CgOp.obj_is_defined(CgOp.letvar("k"));
        my $backf = $!negative ?? CgOp.cgoto(@bfargs) !! CgOp.ncgoto(@bfargs);
        my $updatef = CgOp.prog(@pushcapf,
            CgOp.rxsetpos(CgOp.cursor_pos(CgOp.cast("cursor",
                        CgOp.letvar("k")))));
        $updatef = CgOp.prog() if $!zerowidth;

        my @code;

        if $!selfcut {
            push @code, CgOp.letn(
                "kv", CgOp.get_first($callf),
                "k", CgOp.fetch(CgOp.letvar("kv")),
                $backf,
                $updatef);
        } else {
            push @code, CgOp.rxcall("SetCursorList", CgOp.vvarlist_new_singleton($callf));
            push @code, CgOp.label($bt);
            push @code, CgOp.ncgoto("backtrack", CgOp.iter_hasflat(
                    CgOp.rxcall("GetCursorIter")));
            push @code, CgOp.letn(
                "kv", CgOp.vvarlist_shift(CgOp.rxcall("GetCursorIter")),
                "k", CgOp.fetch(CgOp.letvar("kv")),
                CgOp.rxpushb("SUBRULE", $bt),
                $updatef);
            push @code, CgOp.rxcall("SetCursorList", CgOp.null("var"));
        }

        @code;
    }

    method lad() {
        defined($!method) ?? [ 'Method', $!method ] !!
            $!_passcapzyg ?? ($!_passcapltm // die "passcapltm missing") !!
            [ 'Imp' ];
    }
}

class Sigspace is RxOp {
    has $.selfcut = False; # Bool

    method code($body) {
        RxOp::Subrule.new(method => 'ws', selfcut => $.selfcut).code($body);
    }

    method lad() { ['Imp'] }
}

class CutLTM is RxOp {
    method code($) { CgOp.rxcall('CommitGroup', CgOp.str("LTM")) }
    method lad() { [ 'Imp' ]; } #special case
}

class CutRule is RxOp {
    method code($) { CgOp.rxcall('CommitRule') }
    method lad() { [ 'Null' ]; }
}

class SetLang is RxOp {
    has $.expr = die "SetLang.expr required"; #Op
    method opzyg() { $!expr }

    method code($body) {
        CgOp.rxsetclass(CgOp.obj_llhow(CgOp.fetch($!expr.cgop($body))));
    }

    method lad() { ['Imp'] }
}

class Alt is AltBase {
    has $.optimized_lads;
    method code($body) {
        my @ls = map { self.label }, @$.zyg;
        my @lads = @( $.optimized_lads // (map { $_.lad }, @$.zyg) );
        my $end = self.label;

        die "check screwed up" unless defined $.dba;

        my @code;
        push @code, CgOp.rxcall("LTMPushAlts",
            CgOp.get_lexer(
                CgOp.callframe,
                CgOp.rxcall('GetClass'),
                CgOp.const(CgOp.construct_lad(@lads)),
                CgOp.str($.dba)),
            CgOp.const(CgOp.label_table(@ls)));
        push @code, CgOp.goto('backtrack');
        my $i = 0;
        while $i < @ls {
            push @code, CgOp.label(@ls[$i]);
            push @code, $.zyg[$i].code($body);
            push @code, CgOp.goto($end) unless $i == @ls - 1;
            $i++;
        }
        push @code, CgOp.label($end);
        push @code, CgOp.popcut;
        @code;
    }

    method lad() { [ 'Any', [ map { $_.lad }, @$.zyg ] ]; }
}

class CheckBlock is RxOp {
    has $.block = die "CheckBlock.block required"; # Op
    method opzyg() { $!block }

    method code($body) {
        CgOp.ncgoto('backtrack', CgOp.obj_getbool($!block.cgop($body)));
    }

    method lad() { ['Imp'] }
}

class SaveValue is RxOp {
    has $.capid = die "SaveValue.capid required"; # Str
    has $.block = die "SaveValue.block required"; # Op
    method opzyg() { $!block }

    method used_caps() {
        { $.capid => ($*in_quant ?? 2 !! 1) }
    }

    method code($body) { CgOp.rxpushcapture($!block.cgop($body), $!capid); }
    method lad() { ['Imp'] }
}

class VoidBlock is RxOp {
    has $.block = die "VoidBlock.block required"; # Op
    method opzyg() { $!block }

    method code($body) { CgOp.sink($!block.cgop($body)); }
    method lad() { ['Imp'] }
}

class Statement is RxOp {
    has $.stmt = die "Statement.stmt required"; # Op
    method oplift() { $!stmt }

    method code($) { CgOp.prog() }
    method lad() { ['Null'] }
}

class ProtoRedis is RxOp {
    has $.name = die "ProtoRedis.name required"; # Str
    has $.cutltm = False; # Bool

    method code($) {
        CgOp.letn(
          "fns", CgOp.run_dispatch(CgOp.callframe,
            CgOp.fetch(CgOp.scopedlex('self'))),
          "i",   CgOp.int(0),
          "ks",  CgOp.null('vvarlist'),
          CgOp.pushcut('LTM'),
          CgOp.label('nextfn'),
          CgOp.cgoto('backtrack',
            CgOp.compare('>=', CgOp.letvar("i"),
              CgOp.mrl_count(CgOp.letvar("fns")))),
          CgOp.rxpushb('LTM', 'nextfn'),
          CgOp.letvar("ks", CgOp.vvarlist_new_singleton(
            CgOp.subcall(CgOp.mrl_index(CgOp.letvar("i"),
                CgOp.letvar("fns")), CgOp.newscalar(CgOp.rxcall(
                    'MakeCursor'))))),
          CgOp.letvar("i", CgOp.arith('+', CgOp.letvar("i"), CgOp.int(1))),
          CgOp.label('nextcsr'),
          CgOp.ncgoto('backtrack', CgOp.iter_hasflat(CgOp.letvar('ks'))),
          CgOp.rxpushb('SUBRULE', 'nextcsr'),
          CgOp.rxcall('EndWith', CgOp.cast('cursor',
              CgOp.fetch(CgOp.vvarlist_shift(CgOp.letvar('ks'))))),
          CgOp.goto('backtrack'));
    }

    method lad() {
        $.cutltm ?? [ 'Imp' ] !! [ 'Dispatcher' ];
    }
}

class Any is RxOp {
    method code($) { CgOp.rxbprim("AnyChar") }
    method lad() { ['Dot'] }
}

# generated by optimizer so needs no lad; always greedy
class QuantCClass is RxOp {
    has $.cc = die "QuantCClass.cc required"; # CClass
    has $.min = die "QuantCClass.min required"; # Int
    has $.max;

    method code($) {
        CgOp.rxbprim("ScanCClass", CgOp.int($.min),
            CgOp.int($.max // 0x7FFF_FFFF),
            CgOp.const(CgOp.cc_expr($.cc)));
    }
}

class CClassElem is RxOp {
    has $.cc = die "CClassElem.cc required"; # CClass

    method code($) { CgOp.rxbprim("CClass", CgOp.const(CgOp.cc_expr($.cc))); }
    method tocclist { $.cc }
    method lad() { [ 'CC', @( $.cc.terms ) ] }
}

class None is RxOp {
    method code($) { CgOp.goto('backtrack') }
    method lad() { ['None'] }
}
