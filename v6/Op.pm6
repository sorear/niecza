# b508f750646c0869a9efa961965e17e6e40c8d83

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

# A few words on the nature of bvalues
# A bvalue cannot escape a sub; the return would always extract the
# Variable.  Most ops don't return bvalues, nor expect them.  To avoid
# the overhead of every Op::Lexical returning a bvalue, we do a very
# primitive escape analysis here; only generate bvalues on the LHS of
# binds.  Further, we don't need to generate the bvalues explicitly, since
# we know they'll just be bound.
method code_bvalue($body, $ro, $rhscg) { #OK not used
    die "Illegal use of $.typename in bvalue context"; # XXX niecza
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

class YouAreHere is Op {
    has $.unitname; # Str

    method code($ ) {
        # this should be a little fancier so closure can work
        CgOp.subcall(CgOp.fetch(CgOp.context_get(CgOp.str(
                        '*resume_' ~ $.unitname), CgOp.int(0))));
    }
}

class CallMethod is CallLike {
    has $.receiver = die "CallMethod.receiver required"; # Op
    has $.name = die "CallMethod.name required"; # Op | Str
    has $.private = False; # Bool
    has $.ppath; # Array of Str
    has $.pclass; # Xref, is rw
    has $.ismeta = False; # Bool

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
        } elsif $.ismeta {
            CgOp.let($.receiver.cgop($body), -> $r {
                CgOp.methodcall(CgOp.newscalar(CgOp.how(CgOp.fetch($r))),
                    $name, $r, self.argblock($body))});
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
        CgOp.subcall(CgOp.fetch(CgOp.corelex('&infix:<=>>')),
            CgOp.string_var($.key), $.value.cgop($body));
    }
}

class SimpleParcel is Op {
    has $.items = die "SimpleParcel.items required"; #Array of Op
    method zyg() { @$.items }

    method new(:$items, *%_) {
        nextwith(self, items => [@$items], |%_);
    }

    method code($body) {
        CgOp.subcall(CgOp.fetch(CgOp.corelex('&infix:<,>')),
            map { $_.cgop($body) }, @$.items);
    }
}

class Interrogative is Op {
    has $.receiver = die "Interrogative.receiver required"; #Op
    has $.name = die "Interrogative.name required"; #Str
    method zyg() { $.receiver }

    method code($body) {
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
        if defined($.node.[0]) && $.node.[0] ~~ Match {
            $.node.[0] = $.node.[0]<nibbler>.ast
        }
        $.node.[0] // die "Here document used before body defined";
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
            $cassn = CgOp.ternary(CgOp.obj_getbool($cond), $assn, CgOp.noop);
        }
        elsif $.kind eq '||' {
            $cassn = CgOp.ternary(CgOp.obj_getbool($cond), CgOp.noop, $assn);
        }
        elsif $.kind eq 'andthen' {
            $cassn = CgOp.ternary(CgOp.obj_getdef($cond), $assn, CgOp.noop);
        }
        elsif $.kind eq '//' {
            $cassn = CgOp.ternary(CgOp.obj_getdef($cond), CgOp.noop, $assn);
        }

        CgOp.letn($sym, $.lhs.cgop($body), $cassn, $cond);
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
    has $.check = die "WhileLoop.check required"; # Op
    has $.body = die "WhileLoop.body required"; # Op
    has $.once = die "WhileLoop.once required"; # Bool
    has $.until = die "WhileLoop.until required"; # Bool
    method zyg() { $.check, $.body }
    method ctxzyg($) { $.check, 1, $.body, 0 }

    method code($body) {
        my $id = ::GLOBAL::NieczaActions.genid;

        CgOp.prog(
            CgOp.whileloop(+$.until, +$.once,
                CgOp.obj_getbool($.check.cgop($body)),
                CgOp.prog(
                    CgOp.label("redo$id"),
                    CgOp.sink($.body.cgop($body)),
                    CgOp.label("next$id"),
                    CgOp.ehspan(1, '', 0, "redo$id", "next$id", "next$id"),
                    CgOp.ehspan(2, '', 0, "redo$id", "next$id", "last$id"),
                    CgOp.ehspan(3, '', 0, "redo$id", "next$id", "redo$id"))),
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
        my $var = ::GLOBAL::NieczaActions.gensym;
        $.sink.once = True;
        ::Op::ImmedForLoop.new(source => $.source, var => $var,
            sink => ::Op::CallSub.new(invocant => $.sink,
                positionals => [ ::Op::LetVar.new(name => $var) ]));
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

    method code($body) {
        my $id = ::GLOBAL::NieczaActions.genid;

        CgOp.rnull(CgOp.letn(
            "!iter$id", CgOp.vvarlist_new_empty,
            $.var, CgOp.null('var'),
            CgOp.vvarlist_push(CgOp.letvar("!iter$id"),
                $.source.cgop($body)),
            CgOp.whileloop(0, 0,
                CgOp.iter_hasflat(CgOp.letvar("!iter$id")),
                CgOp.prog(
                    CgOp.letvar($.var,
                        CgOp.vvarlist_shift(CgOp.letvar("!iter$id"))),
                    CgOp.label("redo$id"),
                    CgOp.sink($.sink.cgop($body)),
                    CgOp.label("next$id"),
                    CgOp.ehspan(1, '', 0, "redo$id", "next$id", "next$id"),
                    CgOp.ehspan(2, '', 0, "redo$id", "next$id", "last$id"),
                    CgOp.ehspan(3, '', 0, "redo$id", "next$id", "redo$id"))),
            CgOp.label("last$id")));
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
                CgOp.assign(CgOp.scopedlex($.condvar),
                    CgOp.box('Bool', CgOp.bool(1))),
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

        CgOp.prog(
            CgOp.ehspan(5, '', 0, "start$id", "end$id", "end$id"),
            CgOp.span("start$id", "end$id", 1, $.body.cgop($body)));
    }
}

{ class Num is Op {
    has $.value = die "Num.value required"; # Numeric

    method code($) { CgOp.const(CgOp.box('Num', CgOp.double($.value))) }
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
    has $.accessor; # Bool
    has $.initializer; # Body, is rw

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
        ::Op::CallSub.new(invocant => ::Op::SubDef.new(var => $.var,
                body => $.body, once => True));
    }
}

class SubDef is Op {
    has $.var = die "SubDef.var required"; # Str
    has $.body = die "SubDef.body required"; # Body
    has $.method_too; # Array
    has $.exports = []; # Array of Str
    # Is candidate for beta-optimization.  Not compatible with method_too,
    # exports, ltm
    has $.once = False; # is rw, Bool

    method zyg() { ($.method_too && ($.method_too[1] ~~ Op)) ?? $.method_too[1] !! () }

    method code($) { CgOp.scopedlex($.var) }
}

class Lexical is Op {
    has $.name = die "Lexical.name required"; # Str
    has $.state_decl = False; # Bool

    has $.declaring; # Bool
    has $.list; # Bool
    has $.hash; # Bool

    has $.state_backing; # Str

    method code($) { CgOp.scopedlex($.name) }

    method code_bvalue($ , $ro, $rhscg) {
        CgOp.prog(
            CgOp.scopedlex($.name, CgOp.newboundvar(+$ro, +($.list || $.hash), $rhscg)),
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
                CgOp.newboundvar(+$ro, +($.list || $.hash), $rhscg)),
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
        CgOp.rnull(CgOp.assign($.lhs.cgop($body), $.rhs.cgop($body)));
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
