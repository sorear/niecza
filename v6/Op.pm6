# b508f750646c0869a9efa961965e17e6e40c8d83

class Op;

use CgOp;

# XXX Use raw-er StrPos stuff, and track more details
has $.file; # Str
has $.line; # Int

method zyg() { }
# This should be a conservative approximation of nonvoid context for
# the optimizer; semantic contexts are very downplayed in Perl 6
# and we can live without them for a while.
method ctxzyg($) { map { $_, 1 }, self.zyg }

method cgop($body) {
    if (defined $.file) {
        CgOp.ann($.file, $.line, self.code($body));
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
        sub rec($node) {
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
            CgOp."$cmd"(|(map &rec, @vals));
        }
        rec($.optree);
    }
}; }

class StatementList is Op {
    has $.children = []; # Array of Op
    method zyg() { @$.children }
    method ctxzyg($f) {
        my @r = map { $_, 0 }, @$.children;
        @r[*-1] = $f if @r;
        @r;
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
    sub zyg { $.invocant, @( $.args // $.positionals ) } # XXX callsame

    method adverb($adv) {
        Op::CallSub.new(invocant => $.invocant, args => [ self.getargs, $adv ])
    }

    method code($body) {
        CgOp.subcall(CgOp.fetch($.invocant.cgop($body)), self.argblock($body));
    }
}

class YouAreHere is Op {
    has $.unitname; # Str

    method code($body) {
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
        $.node.[0] // die "Here document used before body defined";
    }

    method code($body) { self.zyg.cgop($body) }
}

class Yada is Op {
    has $.kind = die "Yada.kind required"; #Str

    method code($body) { CgOp.die(">>>Stub code executed") }
}

class ShortCircuit is Op {
    has $.kind = die "ShortCircuit.kind required"; # Str
    has $.args = die "ShortCircuit.args required"; # Array of Op
    method zyg() { @$.args }

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
        my $sym   = ::NieczaActions.gensym;
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
        my $id = ::NieczaActions.genid;

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
        my $var = ::NieczaActions.gensym;
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
        my $id = ::NieczaActions.genid;

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

    method code($body) { CgOp.corelex('Nil') }
}

class Try is Op {
    has $.body = die "Try.body required"; # Op
    method zyg() { $.body }

    method code($body) {
        my $id = ::NieczaActions.genid;

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

    method code($body) {
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

    method code($body) {
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

    method code($body) { $.signature ?? CgOp.scopedlex($.var) !! nextsame }
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

{
    package Op::Whatever;
    use Moose;
    extends 'Op';

    has slot => (isa => 'Str', is => 'ro', required => 1);

    sub code {
        my ($self, $body) = @_;
        CgOp::methodcall(CgOp::corelex('Whatever'), "new");
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::WhateverCode;
    use Moose;
    extends 'Op';

    has ops  => (isa => 'Op', is => 'ro', required => 1);
    has vars => (isa => 'ArrayRef[Str]', is => 'ro', required => 1);
    has slot => (isa => 'Str', is => 'ro', required => 1);

    sub code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->slot);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::BareBlock;
    use Moose;
    extends 'Op';

    has var    => (isa => 'Str', is => 'ro', required => 1);
    has body   => (isa => 'Body', is => 'ro', required => 1);

    sub code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->var);
    }

    sub statement_level {
        $_[0]->body->type('voidbare');
        Op::CallSub->new(invocant => Op::SubDef->new(var => $_[0]->var,
                body => $_[0]->body, once => 1));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::SubDef;
    use Moose;
    extends 'Op';

    has var    => (isa => 'Str', is => 'ro', required => 1);
    has body   => (isa => 'Body', is => 'ro', required => 1);
    has method_too => (isa => 'Maybe[ArrayRef]', is => 'ro');
    has exports => (isa => 'ArrayRef[Str]', is => 'ro', default => sub { [] });
    # Is candidate for beta-optimization.  Not compatible with method_too,
    # exports, ltm
    has once   => (isa => 'Bool', is => 'rw', default => 0);

    sub zyg { ($_[0]->method_too && blessed $_[0]->method_too->[1]) ?
        $_[0]->method_too->[1] : () }

    sub code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->var);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Lexical;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro', required => 1);
    has state_decl => (isa => 'Bool', is => 'ro', default => 0);

    has declaring => (isa => 'Bool', is => 'ro');
    has list => (isa => 'Bool', is => 'ro');
    has hash => (isa => 'Bool', is => 'ro');

    has state_backing => (isa => 'Str', is => 'ro');

    sub code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->name);
    }

    sub code_bvalue {
        my ($self, $body, $ro, $rhscg) = @_;
        CgOp::prog(
            CgOp::scopedlex($self->name, CgOp::newboundvar($ro, (($self->list || $self->hash) ? 1 : 0), $rhscg)),
            CgOp::scopedlex($self->name));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::ConstantDecl;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro', required => 1);
    has init => (isa => 'Op', is => 'rw');
    has path => (isa => 'Maybe[ArrayRef]', is => 'ro');

    sub code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->name);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::ContextVar;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro', required => 1);
    has uplevel => (isa => 'Int', is => 'ro', default => 0);

    sub code {
        my ($self, $body) = @_;
        my @a = (CgOp::str($self->name), CgOp::int($self->uplevel));
        ($self->name eq '$*/' || $self->name eq '$*!') ?
            CgOp::status_get(@a) : CgOp::context_get(@a);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::PackageVar;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro', required => 1);
    has slot => (isa => 'Str', is => 'ro', required => 1);
    has path => (isa => 'ArrayRef[Str]', is => 'ro', required => 1);
    has list => (isa => 'Bool', is => 'ro', default => 0);
    has hash => (isa => 'Bool', is => 'ro', default => 0);

    sub looks_static {
        my ($self) = @_;
        my $v = $self->path->[0];
        if (!defined($v) || $v eq 'MY' || $v eq 'CALLER' || $v eq 'OUTER'
                || $v eq 'DYNAMIC') {
            return 0;
        } else {
            return 1;
        }
    }

    sub code {
        my ($self, $body) = @_;
        $self->looks_static ? CgOp::scopedlex($self->slot) :
            CgOp::bget(($body->lookup_var($self->name, @{ $self->path }))[1]);
    }

    sub code_bvalue {
        my ($self, $body, $ro, $rhscg) = @_;
        $self->looks_static ?
            CgOp::prog(
                CgOp::scopedlex($self->slot,
                    CgOp::newboundvar($ro, $self->list || $self->hash, $rhscg)),
                CgOp::scopedlex($self->slot)) :
            CgOp::letn('!bv', ($body->lookup_var($self->name,
                        @{ $self->path }))[1],
                CgOp::bset(CgOp::letvar('!bv'), CgOp::newboundvar($ro,
                        $self->list || $self->hash, $rhscg)),
                CgOp::bget(CgOp::letvar('!bv')));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Use;
    use Moose;
    extends 'Op';

    has unit => (isa => 'Str', is => 'ro', required => 1);

    sub code { CgOp::corelex('Nil') }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Require;
    use Moose;
    extends 'Op';

    has unit => (isa => 'Str', is => 'ro', required => 1);

    sub code { CgOp::rnull(CgOp::do_require($_[0]->unit)) }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Take;
    use Moose;
    extends 'Op';

    has value => (isa => 'Op', is => 'ro', required => 1);
    sub zyg { $_[0]->value }

    sub code {
        my ($self, $body) = @_;
        CgOp::take($self->value->cgop($body));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Gather;
    use Moose;
    extends 'Op';

    has body => (isa => 'Body', is => 'ro', required => 1);
    has var  => (isa => 'Str',  is => 'ro', required => 1);

    sub code {
        my ($self, $body) = @_;

        # construct a frame for our sub ip=0
        # construct a GatherIterator with said frame
        # construct a List from the iterator

        CgOp::subcall(CgOp::fetch(CgOp::corelex('&_gather')),
            CgOp::newscalar(CgOp::startgather(
                    CgOp::fetch(CgOp::scopedlex($self->var)))));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

# the existance of these complicates cross-sub inlining a bit
{
    package Op::MakeCursor;
    use Moose;
    extends 'Op';

    sub code {
        my ($self, $body) = @_;

        CgOp::prog(
            CgOp::scopedlex('$*/', CgOp::newscalar(CgOp::rxcall('MakeCursor'))),
            CgOp::scopedlex('$*/'));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

# Provides access to a variable with a scope smaller than the sub.  Used
# internally in a few places; should not be exposed to the user, because
# these can't be closed over.
{
    package Op::LetVar;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro', required => 1);

    sub code { CgOp::letvar($_[0]->name); }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::RegexBody;
    use Moose;
    extends 'Op';

    has rxop => (isa => 'RxOp', is => 'ro', required => 1);
    has name => (isa => 'Str', is => 'ro', default => '');
    has passcap => (isa => 'Bool', is => 'ro', default => 0);
    has passcut => (isa => 'Bool', is => 'ro', default => 0);
    has pre => (isa => 'ArrayRef[Op]', is => 'ro', default => sub { [] });
    has canback => (isa => 'Bool', is => 'ro', default => 1);

    sub zyg { @{ $_[0]->pre }, $_[0]->rxop->opzyg }

    sub code {
        my ($self, $body) = @_;

        my @mcaps;
        local $::in_quant = 0;
        if (!$self->passcap) {
            my $u = $self->rxop->used_caps;
            for (keys %$u) {
                push @mcaps, $_ if $u->{$_} >= 2;
            }
        }
        my @pre = map { CgOp::sink($_->code($body)) } @{ $self->pre };

        CgOp::prog(
            @pre,
            CgOp::rxinit(CgOp::str($self->name),
                    CgOp::cast('cursor', CgOp::fetch(CgOp::scopedlex('self'))),
                    ($self->passcap?1:0), ($self->passcut?1:0)),
            ($self->passcap ? () :
                CgOp::rxpushcapture(CgOp::null('var'), @mcaps)),
            $self->rxop->code($body),
            ($self->canback ? CgOp::rxend() : CgOp::rxfinalend()),
            CgOp::label('backtrack'),
            CgOp::rxbacktrack(),
            CgOp::null('var'));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

### BEGIN DESUGARING OPS
# These don't appear in source code, but are used by other ops to preserve
# useful structure.

# used after β-reductions
{
    package Op::SigBind;
    use Moose;
    extends 'Op';

    has signature   => (isa => 'Sig', is => 'ro', required => 1);
    # positionals *really* should be a bunch of gensym Lexical's, or else
    # you risk shadowing hell.  this needs to be handled at a different level
    has positionals => (isa => 'ArrayRef[Op]', is => 'ro', required => 1);

    sub zyg { @{ $_[0]->positionals } }

    sub code {
        my ($self, $body) = @_;

        CgOp::prog(
            $self->signature->bind_inline($body,
                map { $_->cgop($body) } @{ $self->positionals }),
            CgOp::null('var'));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Assign;
    use Moose;
    extends 'Op';

    has lhs => (isa => 'Op', is => 'ro', required => 1);
    has rhs => (isa => 'Op', is => 'ro', required => 1);
    sub zyg { $_[0]->lhs, $_[0]->rhs }

    sub code {
        my ($self, $body) = @_;
        CgOp::rnull(CgOp::assign($self->lhs->cgop($body), $self->rhs->cgop($body)));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Builtin;
    use Moose;
    extends 'Op';

    has args => (isa => 'ArrayRef[Op]', is => 'ro', required => 1);
    has name => (isa => 'Str', is => 'ro', required => 1);
    sub zyg { @{ $_[0]->args } }

    sub code {
        my ($self, $body) = @_;
        no strict 'refs';
        my $name = $self->name;
        my @a = (map { $_->cgop($body) } @{ $self->args });
        &{ "CgOp::bif_$name" }(@a);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Let;
    use Moose;
    extends 'Op';

    has var  => (isa => 'Str', is => 'ro', required => 1);
    has type => (isa => 'Str', is => 'ro');
    has to   => (isa => 'Op',  is => 'ro');
    has in   => (isa => 'Op',  is => 'ro', required => 1);

    sub zyg { ($_[0]->to ? ($_[0]->to) : ()), $_[0]->in }

    sub code {
        my ($self, $body) = @_;

        CgOp::letn($self->var,
            ($self->to ? $self->to->cgop($body) : CgOp::null($self->type)),
            $self->in->cgop($body));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
