class RxOp;

use CgOp;
use CClass;

has $.zyg = []; # Array of RxOp

method opzyg()    { map *.opzyg, @$!zyg }
method ctxopzyg() { map *.ctxopzyg, @$!zyg }
method oplift()   { map *.oplift, @$!zyg }
method uncut()    { self }

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
        self.WHAT.new(text => $!text, igcase => $!igcase, igmark => $!igmark,
            endsym => $!endsym, :$captures);
    }

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
    has $.ops = die "RxOp::VarString.ops required"; # Op
    method ctxopzyg() { $!ops, 1 }
    method opzyg() { $!ops }

    method code($body) {
        CgOp.rxbprim('Exact', CgOp.obj_getstr($!ops.cgop($body)));
    }

    method lad() { ['Imp'] }
}

class Quantifier is RxOp {
    has $.minimal = die "RxOp::Quantifier.minimal required"; # Bool
    has $.min = die "RxOp::Quantifier.min required"; # Int
    has $.max; # Int
    has $.closure;
    has $.nonlisty;
    has $.opsep;

    method opzyg() { $!closure // () }

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

    method code($body) {
        my $rmin = $!closure ?? CgOp.letvar('!min') !! CgOp.int($!min);
        my $rmax = $!closure ?? CgOp.letvar('!max') !! CgOp.int($!max//2**31-1);

        my $exit   = self.label;
        my $repeat = self.label;
        my $sep    = self.label;

        my @code;

        push @code, CgOp.cgoto('backtrack',
            CgOp.compare('>', CgOp.int(0), $rmax)) if $!closure || $!max < 0;
        push @code, CgOp.rxopenquant;

        sub exit($label, $cond) {
            if $!minimal {
                push @code, CgOp.ternary($cond,
                    CgOp.prog(CgOp.rxpushb('QUANT', $label),
                        CgOp.goto($exit)), CgOp.prog());
            } else {
                push @code, CgOp.ternary($cond,
                    CgOp.rxpushb('QUANT', $exit), CgOp.prog());
            }
        }

        # Allow 0-time exit matching null string
        exit($repeat, CgOp.compare('<', $rmin, CgOp.int(1)));

        # We have to match something now
        push @code, CgOp.label($repeat);
        push @code, CgOp.cgoto('backtrack',
            CgOp.compare('>=', CgOp.rxgetquant, $rmax));
        push @code, $.zyg[0].code($body);
        push @code, CgOp.rxincquant;

        if $.zyg[1] {
            exit($sep, CgOp.compare('>=', CgOp.rxgetquant, $rmin));

            push @code, CgOp.label($sep);
            push @code, $.zyg[1].code($body);

            # Allow exiting here if a trailing separator is allowed
            if $!opsep {
                exit($repeat, CgOp.compare('>=', CgOp.rxgetquant, $rmin));
            }
        } else {
            exit($repeat, CgOp.compare('>=', CgOp.rxgetquant, $rmin));
        }

        push @code, CgOp.goto($repeat);

        push @code, CgOp.label($exit);
        push @code, CgOp.sink(CgOp.rxclosequant);

        return @code unless $!closure;

        return CgOp.letn(
            '!range', $!closure.code($body),
            '!min', CgOp.cast('int', CgOp.obj_getnum(CgOp.methodcall(
                        CgOp.letvar('!range'), 'niecza_quantifier_min'))),
            '!max', CgOp.cast('int', CgOp.obj_getnum(CgOp.methodcall(
                        CgOp.letvar('!range'), 'niecza_quantifier_max'))),
            @code);
    }

    method lad() {
        return [ 'Imp' ] if $!minimal || $!closure;

        my $mode = 0;
        $mode += 1 if $!min <= 0;
        $mode += 2 if ($!max // Inf) > 1;
        $mode += 4 if $!opsep;

        [ 'Quant', $mode, map *.lad, @.zyg ];
    }
}

class Sequence is RxOp {
    # zyg * N

    method code($body) { CgOp.prog(map { $_.code($body) }, @$.zyg); }

    method lad() { [ 'Sequence', [ map { $_.lad }, @$.zyg ] ] }

    method tocclist() { map { $_.tocclist }, @$.zyg }
}

class Conj is RxOp {
    # zyg * N

    method code($body) {
        my @z = @$.zyg;
        my @code;
        return () unless @z;

        push @code, CgOp.rxcall('PushConjStart');
        push @code, shift(@z).code($body);
        push @code, CgOp.rxcall('PushConjEnd');

        for @z -> $subseq {
            push @code, CgOp.rxcall('GotoConjStart');
            push @code, $subseq.code($body);
            push @code, CgOp.rxbprim('CheckConjEnd');
        }

        push @code, CgOp.rxcall('EndConj');


        CgOp.prog(map { $_.code($body) }, @$.zyg);
    }

    method lad() { [ 'Imp' ] }
}

class AltBase is RxOp {
    has $.dba; # Str

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

    method code($body) {
        my @code;
        my $fail = self.label;
        my $pass = self.label;

        $body.add_my_name('$*GOAL') unless $body.has_lexical('$*GOAL');

        push @code, CgOp.rxcall("PushGoal", CgOp.callframe, CgOp.str($!closer));
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
        push @code, CgOp.rxcall("PopGoal", CgOp.callframe);

        @code;
    }

    method lad() { ['Imp'] }
}

class Subrule is Capturing {
    has $.method; # Str
    has $.regex; # Op
    has $.ltm;
    has $.selfcut = False; # Bool
    has $.zerowidth; # Bool
    has $.negative; # Bool

    method clone(*%_) {
        self.WHAT.new(method => $!method, regex => $!regex, ltm => $!ltm,
            selfcut => $!selfcut, zerowidth => $!zerowidth,
            negative => $!negative, captures => $.captures, |%_);
    }

    method ctxopzyg() { defined($!regex) ?? ($!regex, 1) !! () }
    method opzyg() { $!regex // Nil }

    method used_caps() {
        my %h = map { ($_ => $*in_quant ?? 2 !! 1) }, @$.captures;
        %h
    }

    method code($body) {
        my $callf = $!regex ?? $!regex.cgop($body) !!
            CgOp.methodcall(CgOp.rxcall("MakeCursorV"),
                $!method);

        my @code;

        if $!selfcut {
            push @code, CgOp.rxincorpcut($.captures, +?$!zerowidth,
                +?$!negative, $callf);
        } else {
            my $bt = self.label;

            push @code, CgOp.rxcall("InitCursorList", $callf);
            push @code, CgOp.label($bt);
            push @code, CgOp.rxincorpshift($.captures, $bt);
        }

        @code;
    }

    method lad() {
        $!ltm // (defined($!method) ?? [ 'Method', $!method ] !! [ 'Imp' ]);
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

class CutBrack is RxOp {
    method code($) { CgOp.rxcall('CommitGroup', CgOp.str("BRACK")) }
    method lad() { [ 'Null' ]; }
}

class SetLang is RxOp {
    has $.expr = die "SetLang.expr required"; #Op
    method ctxopzyg() { $!expr, 1 }
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

        my @code;
        push @code, CgOp.ltm_push_alts([@lads], $.dba, [@ls]);
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
    has Bool $.negate;
    method ctxopzyg() { $!block, 1 }
    method opzyg() { $!block }

    method code($body) {
        my $m = $!negate ?? "cgoto" !! "ncgoto";
        CgOp."$m"('backtrack', CgOp.obj_getbool($!block.cgop($body)));
    }

    method lad() { ['Null'] }
}

class SaveValue is RxOp {
    has $.capid = die "SaveValue.capid required"; # Str
    has $.block = die "SaveValue.block required"; # Op
    method ctxopzyg() { $!block, 1 }
    method opzyg() { $!block }

    method used_caps() {
        { $.capid => ($*in_quant ?? 2 !! 1) }
    }

    method code($body) { CgOp.rxpushcapture($!block.cgop($body), $!capid); }
    method lad() { ['Imp'] }
}

class VoidBlock is RxOp {
    has $.block = die "VoidBlock.block required"; # Op
    method ctxopzyg() { $!block, 0 }
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
    method code($) {
        my $bt = self.label;

        my @code;
        push @code, CgOp.rxcall("InitCursorList",
            CgOp.rxlprim('proto_dispatch', CgOp.scopedlex('Any')));
        push @code, CgOp.label($bt);
        push @code, CgOp.rxincorpshift(['dispatch'], $bt);
        @code;
    }

    method lad() { [ 'Dispatcher' ] }
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

class RxOp::Newline is RxOp {
    method code($) { CgOp.rxbprim('Newline') }
    method lad() {
        ['Any', [ ['Str', "\x0D\x0A"],
                  [ 'CC', @( $CClass::VSpace.terms ) ] ] ]
    }
}

class RxOp::StringCap is RxOp::Capturing {
    # zyg * 1

    method clone(:$captures) {
        self.WHAT.new(zyg => $.zyg, :$captures);
    }

    method code($body) { #OK not used
        my @code;

        push @code, CgOp.pushcut("CAP");
        push @code, CgOp.rxsetquant(CgOp.rxgetpos);
        push @code, $.zyg[0].code($body);
        push @code, CgOp.rxpushcapture(CgOp.rxcall("StringCapture"), @$.captures);
        push @code, CgOp.popcut;

        @code;
    }

    method lad() { $.zyg[0].lad }
}

class RxOp::ListPrim is RxOp {
    has Str $.name; # used for LTM cheatery
    has Str $.type;
    has $.ops = die "RxOp::Variable.ops required"; # Op

    method ctxopzyg() { $!ops, 1 }
    method opzyg() { $!ops }

    method code($body) {
        my $bt = self.label;

        my @code;
        push @code, CgOp.rxcall("InitCursorList",
            CgOp.rxlprim($!type, $!ops.cgop($body)));
        push @code, CgOp.label($bt);
        push @code, CgOp.rxincorpshift([], $bt);
        @code;
    }

    method lad() { $!type eq 'scalar_var' ?? ['Param', $!name] !! ['Imp'] }
}

class RxOp::Endpoint is RxOp {
    has Str $.type = die "Endpoint.type required";

    method code($) { CgOp.rxcall('SetEndpoint', CgOp.str($!type)); }
    method lad() { [ 'Null' ] }
}
