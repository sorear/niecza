class Op;

use CgOp;

# XXX Use raw-er StrPos stuff, and track more details
has $.line; # Int

method zyg() { }
# This should be a conservative approximation of nonvoid context for
# the optimizer; semantic contexts are very downplayed in Perl 6
# and we can live without them for a while.
method ctxzyg($) { map { $_, 1 }, self.zyg }

method cgop($body) {
    if (defined $.line) {
        CgOp.ann("", $.line, self.code($body));
    } else {
        self.code($body);
    }
}

# ick
method cgop_labelled($body, $label) {
    if (defined $.line) {
        CgOp.ann("", $.line, self.code_labelled($body, $label));
    } else {
        self.code_labelled($body, $label);
    }
}

method code_labelled($body, $label) { self.code($body) } #OK not used

# A few words on the nature of bvalues
# A bvalue cannot escape a sub; the return would always extract the
# Variable.  Most ops don't return bvalues, nor expect them.  To avoid
# the overhead of every Op::Lexical returning a bvalue, we do a very
# primitive escape analysis here; only generate bvalues on the LHS of
# binds.  Further, we don't need to generate the bvalues explicitly, since
# we know they'll just be bound.
method code_bvalue($body, $ro, $rhscg) { #OK not used
    die "Illegal use of {self.WHAT.perl} in bvalue context";
}

method statement_level() { self }

{ class CgOp is Op {
    has $.op;
    has $.optree;

    method zyg() {
        return Nil unless $.optree;
        sub rec($node is rw) {
            ($node ~~ Op) ?? $node !!
                ($node ~~ List) ?? (map &rec, @$node) !! ();
        }
        rec($.optree);
    }

    method code($body) {
        return $.op if $.op;
        sub rec($node) {
            return $node.cgop($body) if $node ~~ Op;
            return $node if $node !~~ List;
            my ($cmd, @vals) = @$node;
            ::GLOBAL::CgOp."$cmd"(|(map &rec, @vals));
        }
        rec($.optree);
    }
}; }

class StatementList is Op {
    has $.children = []; # Array of Op
    method new(:$children = [], *%_) {
        nextwith(self, children => [ @$children ], |%_);
    }
    method zyg() { @$.children }
    method ctxzyg($f) {
        my $i = 1 - $.children;
        map { $_, (($i++) ?? 0 !! $f) }, @$.children;
    }

    method code($body) {
        my @ch = map { $_.cgop($body) }, @$.children;
        my $end = @ch ?? pop(@ch) !! CgOp.corelex('Nil');

        CgOp.prog((map { CgOp.sink($_) }, @ch), $end);
    }
}

class CallLike is Op {
    has $.positionals = [];
    has $.args;
    method zyg() { @( $.args // $.positionals ) }

    method new(:$positionals = [], :$args, *%_) {
        nextwith(self,
            positionals => ($positionals andthen [@$positionals]),
            args => ($args andthen [@$args]), |%_);
    }

    method getargs() {
        $.args ?? @$.args !! (map { ::Op::Paren.new(inside => $_) },
            @$.positionals );
    }

    sub parsearglist($body, @args) {
        my @out;
        for @args -> $a {
            if $a.^isa(::Op::SimplePair) {
                push @out, ":" ~ $a.key, $a.value.cgop($body);
            } elsif $a.^isa(::Op::CallSub) && $a.invocant.^isa(::Op::Lexical)
                    && $a.invocant.name eq '&prefix:<|>' {
                push @out, 'flatcap', CgOp.fetch(CgOp.methodcall(
                    $a.positionals[0].cgop($body), 'Capture'));
            } else {
                push @out, $a.cgop($body);
            }
        }
        @out;
    }

    method argblock($body) {
        if !$.args {
            return map { $_.cgop($body) }, @$.positionals;
        }
        parsearglist($body, $.args);
    }
}

class CallSub is CallLike {
    has $.invocant = die "CallSub.invocant required"; # Op
    method zyg() { $.invocant, @( $.args // $.positionals ) } # XXX callsame

    method adverb($adv) {
        Op::CallSub.new(invocant => $.invocant, args => [ self.getargs, $adv ])
    }

    method code($body) {
        CgOp.subcall(CgOp.fetch($.invocant.cgop($body)), self.argblock($body));
    }
}

class CallMethod is CallLike {
    has $.receiver = die "CallMethod.receiver required"; # Op
    has $.name = die "CallMethod.name required"; # Op | Str
    has $.private = False; # Bool
    has $.ppath; # Array of Str
    has $.pclass; # Xref, is rw
    has $.ismeta = ''; # Str

    method adverb($adv) {
        Op::CallMethod.new(receiver => $.receiver, name => $.name,
            private => $.private, ppath => $.ppath, pclass => $.pclass,
            ismeta => $.ismeta, args => [ self.getargs, $adv ])
    }

    method zyg() { $.receiver, (($.name ~~ Op) ?? $.name !! Nil),
        @( $.args // $.positionals ) } # XXX callsame

    method code($body) {
        my $name = ($.name ~~ Op) ?? CgOp.obj_getstr($.name.cgop($body))
            !! CgOp.str($.name);
        if $.private {
            CgOp.subcall(CgOp.stab_privatemethod(
                    CgOp.class_ref('mo', @( $.pclass )), $name),
                $.receiver.cgop($body), self.argblock($body));
        } elsif $.ismeta eq '^' {
            CgOp.let($.receiver.cgop($body), -> $r {
                CgOp.methodcall(CgOp.newscalar(CgOp.how(CgOp.fetch($r))),
                    $name, $r, self.argblock($body))});
        } elsif $.ismeta eq '?' {
            # TODO maybe use a lower-level check
            CgOp.let($.receiver.cgop($body), -> $r { CgOp.let($name, -> $n {
                CgOp.ternary(
                    CgOp.obj_getbool(CgOp.methodcall(CgOp.newscalar(CgOp.how(
                        CgOp.fetch($r))), "can", $r, CgOp.box('Str',$n))),
                    CgOp.methodcall($r, $n, self.argblock($body)),
                    CgOp.scopedlex('Nil'))})});
        } else {
            CgOp.methodcall($.receiver.cgop($body),
                $name, self.argblock($body));
        }
    }
}

class GetSlot is Op {
    has $.object = die "GetSlot.object required"; # Op
    has $.name = die "GetSlot.name required"; # Str
    method zyg() { $.object }

    method code($body) {
        CgOp.varattr($.name, CgOp.fetch($.object.cgop($body)));
    }
}

class Paren is Op {
    has $.inside = die "Paren.inside required"; # Op
    method zyg() { $.inside }
    method ctxzyg($f) { $.inside, $f }

    method code($body) { $.inside.cgop($body) }

    method code_bvalue($body, $ro, $rhscg) {
        $.inside.code_bvalue($body, $ro, $rhscg)
    }
}

class SimplePair is Op {
    has $.key = die "SimplePair.key required"; # Str
    has $.value = die "SimplePair.value required"; # Op

    method zyg() { $.value }

    method code($body) {
        CgOp.bif_pair(CgOp.const(CgOp.string_var($.key)), $.value.cgop($body));
    }
}

class SimpleParcel is Op {
    has $.items = die "SimpleParcel.items required"; #Array of Op
    method zyg() { @$.items }

    method new(:$items, *%_) {
        nextwith(self, items => [@$items], |%_);
    }

    method code($body) {
        CgOp.bif_comma(map { $_.cgop($body) }, @$.items);
    }
}

class Interrogative is Op {
    has $.receiver = die "Interrogative.receiver required"; #Op
    has $.name = die "Interrogative.name required"; #Str
    method zyg() { $.receiver }

    method code($body) {
        if $.name eq "VAR" {
            return CgOp.var_get_var($.receiver.cgop($body));
        }
        my $c = CgOp.fetch($.receiver.cgop($body));
        if $.name eq "HOW" {
            $c = CgOp.how($c);
        }
        elsif $.name eq "WHAT" {
            $c = CgOp.obj_what($c);
        }
        else {
            die "Invalid interrogative $.name";
        }
        CgOp.newscalar($c);
    }
}

class HereStub is Op {
    # this points to a STD writeback node
    has $.node = die "HereStub.node required";

    method zyg() {
        $.node // die "Here document used before body defined";
    }

    method code($body) { self.zyg.cgop($body) }
}

class Yada is Op {
    has $.kind = die "Yada.kind required"; #Str

    method code($ ) { CgOp.die(">>>Stub code executed") }
}

class ShortCircuit is Op {
    has $.kind = die "ShortCircuit.kind required"; # Str
    has $.args = die "ShortCircuit.args required"; # Array of Op
    method zyg() { @$.args }
    method new(:$args, *%_) {
        nextwith(self, args => [@$args], |%_);
    }

    method red2($sym, $o2) {
        if $!kind eq '&&' {
            CgOp.ternary(CgOp.obj_getbool($sym), $o2, $sym);
        }
        elsif $!kind eq '||' {
            CgOp.ternary(CgOp.obj_getbool($sym), $sym, $o2);
        }
        elsif $!kind eq 'andthen' {
            CgOp.ternary(CgOp.obj_getdef($sym), $o2, $sym);
        }
        elsif $!kind eq '//' {
            CgOp.ternary(CgOp.obj_getdef($sym), $sym, $o2);
        }
        else {
            die "That's not a sensible short circuit, now is it?";
        }
    }

    method code($body) {
        my @r = reverse @$.args;
        my $acc = (shift @r).cgop($body);

        for @r {
            $acc = CgOp.let($_.cgop($body), -> $v { self.red2($v, $acc) });
        }

        $acc;
    }
}

class ShortCircuitAssign is Op {
    has $.kind = die "ShortCircuitAssign.kind required"; #Str
    has $.lhs  = die "ShortCircuitAssign.lhs required"; #Op
    has $.rhs  = die "ShortCircuitAssign.rhs required"; #Op
    method zyg() { $.lhs, $.rhs }

    method code($body) {
        my $sym   = ::GLOBAL::NieczaActions.gensym;
        my $assn  = CgOp.assign(CgOp.letvar($sym), $.rhs.cgop($body));
        my $cond  = CgOp.letvar($sym);
        my $cassn;

        if $.kind eq '&&' {
            $cassn = CgOp.ternary(CgOp.obj_getbool($cond), $assn, $cond);
        }
        elsif $.kind eq '||' {
            $cassn = CgOp.ternary(CgOp.obj_getbool($cond), $cond, $assn);
        }
        elsif $.kind eq 'andthen' {
            $cassn = CgOp.ternary(CgOp.obj_getdef($cond), $assn, $cond);
        }
        elsif $.kind eq '//' {
            $cassn = CgOp.ternary(CgOp.obj_getdef($cond), $cond, $assn);
        }

        CgOp.letn($sym, $.lhs.cgop($body), $cassn);
    }
}

class StringLiteral is Op {
    has $.text = die "StringLiteral.text required"; # Str

    method code($) { CgOp.const(CgOp.string_var($.text)); }
}

class Conditional is Op {
    has $.check = die "Conditional.check required"; # Op
    has $.true;
    has $.false;

    method zyg() { grep *.&defined, $.check, $.true, $.false }
    method ctxzyg($f) {
        $.check, 1,
        map { defined($_) ?? ($_, $f) !! () }, $.true, $.false;
    }

    method code($body) {
        CgOp.ternary(
            CgOp.obj_getbool($.check.cgop($body)),
            ($.true ?? $.true.cgop($body) !! CgOp.corelex('Nil')),
            ($.false ?? $.false.cgop($body) !! CgOp.corelex('Nil')));
    }
}

class WhileLoop is Op {
    has Op $.check = die "WhileLoop.check required";
    has Op $.body = die "WhileLoop.body required";
    has Bool $.once = die "WhileLoop.once required";
    has Bool $.until = die "WhileLoop.until required";
    has Bool $.need_cond;
    method zyg() { $.check, $.body }
    method ctxzyg($) { $.check, 1, $.body, 0 }

    method code($body) { self.code_labelled($body,'') }
    method code_labelled($body, $l) {
        my $id = ::GLOBAL::NieczaActions.genid;

        my $cond = $!need_cond ?? 
            CgOp.prog(CgOp.letvar('!cond', $.check.cgop($body)),
                CgOp.obj_getbool(CgOp.letvar('!cond'))) !!
            CgOp.obj_getbool($.check.cgop($body));
        my @loop =
            CgOp.whileloop(+$.until, +$.once, $cond,
                CgOp.sink(CgOp.xspan("redo$id", "next$id", 0, $.body.cgop($body),
                    1, $l, "next$id", 2, $l, "last$id", 3, $l, "redo$id"))),
            CgOp.label("last$id"),
            CgOp.corelex('Nil');

        $!need_cond ?? CgOp.letn('!cond', CgOp.scopedlex('Any'), @loop) !!
            CgOp.prog(@loop)
    }
}

class GeneralLoop is Op {
    has $.init; # Op
    has $.cond; # Op
    has $.step; # Op
    has $.body; # Op

    method zyg() { grep &defined, $.init, $.cond, $.step, $.body }
    method ctxzyg($) {
        ($.init ?? ($.init, 0) !! ()),
        ($.cond ?? ($.cond, 1) !! ()),
        ($.step ?? ($.step, 0) !! ()),
        $.body, 0
    }

    method code($body) { self.code_labelled($body,'') }
    method code_labelled($body, $l) {
        my $id = ::GLOBAL::NieczaActions.genid;

        CgOp.prog(
            ($.init ?? CgOp.sink($.init.cgop($body)) !! ()),
            CgOp.whileloop(0, 0,
                ($.cond ?? CgOp.obj_getbool($.cond.cgop($body)) !!
                    CgOp.bool(1)),
                CgOp.prog(
                    CgOp.sink(CgOp.xspan("redo$id", "next$id", 0,
                            $.body.cgop($body), 1, $l, "next$id",
                            2, $l, "last$id", 3, $l, "redo$id")),
                    ($.step ?? CgOp.sink($.step.cgop($body)) !! ()))),
            CgOp.label("last$id"),
            CgOp.corelex('Nil'));
    }
}

class ForLoop is Op {
    has $.source = die "ForLoop.source required"; # Op
    has $.sink = die "ForLoop.sink required"; # Op
    method zyg() { $.source, $.sink }

    method code($body) {
        CgOp.methodcall(
            CgOp.subcall(CgOp.fetch(CgOp.corelex('&flat')),
                $.source.cgop($body)), 'map', $.sink.cgop($body));
    }

    method statement_level() {
        my $var = [ map { ::GLOBAL::NieczaActions.gensym },
            0 ..^ +$.sink.body.signature.params ];
        $.sink.once = True;
        ::Op::ImmedForLoop.new(source => $.source, var => $var,
            sink => ::Op::CallSub.new(invocant => $.sink,
                positionals => [ map { ::Op::LetVar.new(name => $_) }, @$var]));
    }
}

# A for-loop which must be run immediately because it is at statement
# level, as contrasted with a ForLoop, which turns into a map call.
# ForLoops turn into this at statement level; if a ForLoop is in any
# other context, it is regarded as a lazy comprehension.
class ImmedForLoop is Op {
    has $.var = die "ImmedForLoop.source required"; # Str
    has $.source = die "ImmedForLoop.source required"; # Op
    has $.sink = die "ImmedForLoop.sink required"; # Op

    method zyg() { $.source, $.sink }
    method ctxzyg($) { $.source, 1, $.sink, 0 }

    method code($body) { self.code_labelled($body, '') }
    method code_labelled($body, $l) {
        my $id = ::GLOBAL::NieczaActions.genid;

        CgOp.rnull(CgOp.letn(
            "!iter$id", CgOp.start_iter($.source.cgop($body)),
            (map { $_, CgOp.null('var') }, @$.var),
            CgOp.whileloop(0, 0,
                CgOp.iter_hasflat(CgOp.letvar("!iter$id")),
                CgOp.prog(
                    (map { CgOp.letvar($_,
                        CgOp.vvarlist_shift(CgOp.letvar("!iter$id")))},@$.var),
                    CgOp.sink(CgOp.xspan("redo$id", "next$id", 0,
                        $.sink.cgop($body),
                        1, $l, "next$id",
                        2, $l, "last$id",
                        3, $l, "redo$id")))),
            CgOp.label("last$id")));
    }
}

class Labelled is Op {
    has $.stmt;
    has $.name;
    method zyg() { $.stmt }

    method code($body) {
        CgOp.prog(CgOp.label("goto_$.name"),$.stmt.cgop_labelled($body,$.name));
    }

    method statement_level() {
        self.new(name => $.name, stmt => $.stmt.statement_level);
    }
}

class When is Op {
    has $.match;
    has $.body;
    method zyg() { $.match, $.body }

    method code($body) {
        my $id = ::GLOBAL::NieczaActions.genid;

        CgOp.ternary(CgOp.obj_getbool(CgOp.methodcall(
                $.match.cgop($body), 'ACCEPTS', CgOp.scopedlex('$_'))),
            CgOp.xspan("start$id", "end$id", 0, CgOp.prog(
                    CgOp.sink($.body.cgop($body)),
                    CgOp.control(6, CgOp.null('frame'), CgOp.int(-1),
                        CgOp.null('str'), CgOp.corelex('Nil'))),
                7, '', "end$id"),
            CgOp.corelex('Nil'));
    }
}

# only for state $x will start and START{} in void context, yet
class Start is Op {
    # possibly should use a raw boolean somehow
    has $.condvar = die "Start.condvar required"; # Str
    has $.body = die "Start.body required"; # Op
    method zyg() { $.body }
    method ctxzyg($f) { $.body, $f }

    method code($body) {
        CgOp.ternary(
            CgOp.obj_getbool(CgOp.scopedlex($.condvar)),
            CgOp.corelex('Nil'),
            CgOp.prog(
                CgOp.sink(CgOp.assign(CgOp.scopedlex($.condvar),
                    CgOp.box('Bool', CgOp.bool(1)))),
                $.body.cgop($body)));
    }
}

class VoidPhaser is Op {
    has $.body = die "VoidPhaser.body required"; # Body

    method code($ ) { CgOp.corelex('Nil') }
}

class Try is Op {
    has $.body = die "Try.body required"; # Op
    method zyg() { $.body }

    method code($body) {
        my $id = ::GLOBAL::NieczaActions.genid;

        CgOp.xspan("start$id", "end$id", 1, $.body.cgop($body),
            5, '', "end$id");
    }
}

class Control is Op {
    has $.payload = die "Control.payload required"; # Op
    has $.name = "";
    has $.number = die "Control.number required"; # Num

    method zyg() { $.payload }

    method code($body) {
        CgOp.control($.number, CgOp.null('frame'), CgOp.int(-1),
            ($.name eq '' ?? CgOp.null('str') !! CgOp.str($.name)),
            $.payload.cgop($body));
    }
}

{ class Num is Op {
    has $.value = die "Num.value required"; # Numeric

    method code($) {
        if $.value ~~ Array {
            CgOp.const(CgOp.exactnum(|$.value))
        } else {
            CgOp.const(CgOp.box('Num', CgOp.double($.value)))
        }
    }
}; }

class Bind is Op {
    has $.readonly = die "Bind.readonly required"; #Bool
    has $.lhs      = die "Bind.lhs required"; #Op
    has $.rhs      = die "Bind.rhs required"; #Op
    method zyg() { $.lhs, $.rhs }

    method code($body) {
        $.lhs.code_bvalue($body, $.readonly, $.rhs.cgop($body))
    }
}

class Augment is Op {
    has $.name; # Str
    has $.bodyvar; # Str
    has $.body; # Body
    has $.pkg; # Array of Str

    method code($ ) {
        CgOp.subcall(CgOp.fetch(CgOp.scopedlex($.bodyvar)));
    }
}

class PackageDef is Op {
    has $.name; # Str
    has $.var = die "PackageDef.var required"; # Str
    has $.bodyvar; # Str
    has $.stub = False; # Bool
    has $.body; # Body
    has $.exports = []; # Array of Str
    has $.ourpkg; # Array of Str
    has $.ourvar; # Str

    method code($ ) {
        if $.stub {
            CgOp.scopedlex($.var);
        } else {
            CgOp.prog(
                CgOp.sink(CgOp.subcall(CgOp.fetch(
                            CgOp.scopedlex($.bodyvar)))),
                CgOp.scopedlex($.var));
        }
    }
}

class ModuleDef is PackageDef { }
class RoleDef is ModuleDef {
    has $.signature; # Sig

    method code($ ) { $.signature ?? CgOp.scopedlex($.var) !! nextsame }
}
class ClassDef is ModuleDef { }
class GrammarDef is ClassDef { }

class Super is Op {
    has $.name; # Str
    has $.path; # Array of Str

    method code($) { CgOp.corelex('Nil') }
}

class Attribute is Op {
    has $.name; # Str
    has $.sigil;
    has $.accessor; # Bool
    has $.initializer; # Body, is rw
    has $.typeconstraint; # Array of Str
    has $.rw;

    method code($) { CgOp.corelex('Nil') }
}

class Whatever is Op {
    has $.slot = die "Whatever.slot required"; # Str

    method code($) { CgOp.methodcall(CgOp.corelex('Whatever'), 'new') }
}

class WhateverCode is Op {
    has $.ops = die "WhateverCode.ops required"; # Op
    has $.vars = die "WhateverCode.vars required"; # Array of Str
    has $.slot = die "WhateverCode.slot required"; # Str

    method code($) { CgOp.scopedlex($.slot) }
}

class BareBlock is Op {
    has $.var = die "BareBlock.var required"; # Str
    has $.body = die "BareBlock.body required"; # Body

    method code($) { CgOp.scopedlex($.var) }

    method statement_level() {
        $.body.type = 'voidbare';
        ::Op::CallSub.new(invocant => ::Op::SubDef.new(body => $.body, :once));
    }
}

class SubDef is Op {
    has $.body = die "SubDef.body required"; # Body

    # often a gensym; will be set to the "correct" symbol if it is being
    # used as a lexical; set by begin
    has $.symbol; # Str, is rw

    has $.multiness; # proto, only, multi, Any=null
    has $.bindlex; # Bool
    has $.bindpackages; # Array of Array of Str to install in
    # used for 'our' and 'is export'
    has $.bindmethod; # named array blocky thing

    # Is candidate for beta-optimization.  Not compatible with method_too,
    # exports, ltm
    has $.once = False; # is rw, Bool

    method zyg() { ($.bindmethod && ($.bindmethod[1] ~~ Op)) ?? $.bindmethod[1] !! () }

    method code($) { CgOp.scopedlex($.symbol // 'Any') }
}

class Lexical is Op {
    has $.name = die "Lexical.name required"; # Str
    has $.state_decl = False; # Bool

    has $.declaring; # Bool
    has $.list; # Bool
    has $.hash; # Bool
    has $.typeconstraint; # Array of Str

    has $.state_backing; # Str

    method code($) { CgOp.scopedlex($.name) }

    method code_bvalue($ , $ro, $rhscg) {
        CgOp.prog(
            CgOp.scopedlex($.name, defined($ro) ?? CgOp.newboundvar(+?$ro,
                +(?($.list || $.hash)), $rhscg) !! $rhscg),
            CgOp.scopedlex($.name));
    }
}

class ConstantDecl is Op {
    has $.name = die "ConstantDecl.name required"; # Str
    has $.init; # Op, is rw
    has $.path; # Array

    method code($ ) { CgOp.scopedlex($.name) }
}

class ContextVar is Op {
    has $.name = die "ContextVar.name required"; # Str
    has $.uplevel = 0; # Int

    method code($ ) {
        my @a = (CgOp.str($.name), CgOp.int($.uplevel));
        ($.name eq '$*/' || $.name eq '$*!') ??
            CgOp.status_get(|@a) !! CgOp.context_get(|@a);
    }
}

class PackageVar is Op {
    has $.name = die "PackageVar.name required"; # Str
    has $.slot = die "PackageVar.slot required"; # Str
    has $.path = die "PackageVar.path required"; # Array of Str
    has $.list = False; # Bool
    has $.hash = False; # Bool

    # TODO: Design and reimplement dynamic-ish $CALLER::, $MY::

    method code($ ) { CgOp.scopedlex($.slot) }

    method code_bvalue($ , $ro, $rhscg) {
        CgOp.prog(
            CgOp.scopedlex($.slot,
                defined($ro) ?? CgOp.newboundvar(+?$ro,
                    +(?($.list || $.hash)), $rhscg) !! $rhscg),
            CgOp.scopedlex($.slot));
    }
}

class Use is Op {
    has $.unit = die "Use.unit required"; # Str

    method code($ ) { CgOp.corelex('Nil') }
}

class Require is Op {
    has $.unit = die "Require.unit required"; # Str

    method code($ ) { CgOp.rnull(CgOp.do_require($.unit)) }
}

class Take is Op {
    has $.value = die "Take.value required"; # Op
    method zyg() { $.value }

    method code($body) { CgOp.take($.value.cgop($body)) }
}

class Gather is Op {
    has $.body = die "Gather.body required"; # Body
    has $.var  = die "Gather.var required"; # Str

    method code($ ) {
        # construct a frame for our sub ip=0
        # construct a GatherIterator with said frame
        # construct a List from the iterator

        CgOp.subcall(CgOp.fetch(CgOp.corelex('&_gather')),
            CgOp.newscalar(CgOp.startgather(
                    CgOp.fetch(CgOp.scopedlex($.var)))));
    }
}

class MakeCursor is Op {
    method code($ ) {
        CgOp.prog(
            CgOp.scopedlex('$*/', CgOp.newscalar(CgOp.rxcall('MakeCursor'))),
            CgOp.scopedlex('$*/'));
    }
}

# Provides access to a variable with a scope smaller than the sub.  Used
# internally in a few places; should not be exposed to the user, because
# these can't be closed over.
# the existance of these complicates cross-sub inlining a bit
class LetVar is Op {
    has $.name = die "LetVar.name required"; # Str

    method code($ ) { CgOp.letvar($.name) }
}

class RegexBody is Op {
    has $.rxop = die "RegexBody.rxop required"; # RxOp
    has $.name = '';
    has $.passcap = False;
    has $.passcut = False;
    has $.pre = []; # Array of Op
    has $.canback = True;

    method ctxzyg($ ) { (map { $_, 0 }, @$.pre), $.rxop.ctxopzyg }
    method zyg() { @$.pre, $.rxop.opzyg }

    method code($body) {
        my @mcaps;
        my $*in_quant = False;
        if !$.passcap {
            my $u = $.rxop.used_caps;
            for keys $u {
                push @mcaps, $_ if $u{$_} >= 2;
            }
        }
        my @pre = map { CgOp.sink($_.cgop($body)) }, @$.pre;

        CgOp.prog(
            @pre,
            CgOp.rxinit(CgOp.str($.name),
                    CgOp.cast('cursor', CgOp.fetch(CgOp.scopedlex('self'))),
                    +$.passcap, +$.passcut),
            ($.passcap ?? () !!
                CgOp.rxpushcapture(CgOp.null('var'), @mcaps)),
            $.rxop.code($body),
            ($.canback ?? CgOp.rxend !! CgOp.rxfinalend),
            CgOp.label('backtrack'),
            CgOp.rxbacktrack,
            CgOp.null('var'));
    }
}

class YouAreHere is Op {
    has $.unitname; # Str

    method code($ ) {
        # this should be a little fancier so closure can work
        CgOp.subcall(CgOp.fetch(CgOp.context_get(CgOp.str(
                        '*resume_' ~ $.unitname), CgOp.int(0))));
    }
}



### BEGIN DESUGARING OPS
# These don't appear in source code, but are used by other ops to preserve
# useful structure.

# used after β-reductions
class SigBind is Op {
    has $.signature = die "SigBind.signature required"; # Sig
    # positionals *really* should be a bunch of gensym Lexical's, or else
    # you risk shadowing hell.  this needs to be handled at a different level
    has $.positionals = die "SigBind.positionals required"; # Array of Op

    method zyg() { @$.positionals }
    method new(:$positionals, *%_) {
        nextwith(self, positionals => [@$positionals], |%_);
    }

    method code($body) {
        CgOp.prog(
            $.signature.bind_inline($body,
                map { $_.cgop($body) }, @$.positionals),
            CgOp.null('var'));
    }
}

class Assign is Op {
    has $.lhs = die "Assign.lhs required"; # Op
    has $.rhs = die "Assign.rhs required"; # Op
    method zyg() { $.lhs, $.rhs }

    method code($body) {
        CgOp.assign($.lhs.cgop($body), $.rhs.cgop($body));
    }
}

class Builtin is Op {
    has $.args = die "Builtin.args required"; # Array of Op
    has $.name = die "Builtin.name required"; # Str
    method zyg() { @$.args }
    method new(:$args, *%_) {
        nextwith(self, args => [@$args], |%_);
    }

    method code($body) {
        my @a = (map { $_.cgop($body) }, @$.args);
        CgOp."bif_$.name"(|@a);
    }
}

class Let is Op {
    has $.var = die "Let.var required"; # Str
    has $.type; # Str
    has $.to; # Op
    has $.in = die "Let.in required"; # Op

    method zyg() { ($.to // Nil), $.in }

    method code($body) {
        CgOp.letn($.var, ($.to ?? $.to.cgop($body) !! CgOp.null($.type)),
            $.in.cgop($body));
    }
}

class LetScope is Op {
    has $.transparent;
    has $.names;
    has $.inner;

    method zyg() { $.inner }

    method code($body) {
        CgOp.letscope(+$.transparent, @($.names), $.inner.cgop($body));
    }
}

# These two are created to codegen wrappers in NAMOutput... bad factor
class TopicalHook is Op {
    has $.inner;
    method zyg() { $.inner }

    method code($body) {
        my $id = ::GLOBAL::NieczaActions.genid;

        CgOp.xspan("start$id", "end$id", 0, $.inner.cgop($body),
            6, '', "end$id");
    }
}

class LabelHook is Op {
    has $.inner;
    has $.labels;
    method zyg() { $.inner }

    method code($body) {
        my $id = ::GLOBAL::NieczaActions.genid;

        CgOp.xspan("start$id", "end$id", 0, $.inner.cgop($body),
            map({ 8, $_, "goto_$_" }, @$.labels));
    }
}
