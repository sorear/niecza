# first half of the file - begin augments are in Begin.pm6

class Metamodel;

method locstr($fo, $lo, $fn, $ln) {
    $fo := $fo // '???';
    $lo := $lo // '???';
    $fn := $fn // '???';
    $ln := $ln // '???';

    $fn eq $fo ?? " (see line $lo)" !! " (see $fo line $lo)";
}

### NIECZA COMPILER METAMODEL
# The metamodel exists to create a timeline inside the compiler.  Previously,
# the compiler operated as a pure tree transformer with no conception of how
# PRE-INIT code would play out, thus precluding a lot of important
# optimizations (based on precomputing immutable objects and optimizing
# references to them, mostly).
#
# The metamodel has two main life stages.  First, it is built; an incremental
# process logically called BEGIN.  Then, it is processed to perform closed-
# world optimizations and generate code; this is (UNIT)CHECK.
#
# Kinds of objects which exist in the metamodel
# - Static subs
# - Packages (incl. classes, modules, grammars)
# - Stashes (Foo::)
#
# This graph is a lot more random than the old trees were...

# While manipulating metamodel bits during BEGIN, these contextuals are needed:
# $*unit: current unit for new objects to attach to
# %*units: maps unit names to unit objects
# $*CURSUB<!sub>: the top non-transparent sub

# Almost all longname and most identifier uses in Perl6 can be divided into
# two groups.
#
# DECLARATIVE references, like class Foo::Bar::Baz {}, have an ending token,
# and the remainder identifies a stash.  Leading :: is ignored; if 0 tokens,
# anon is forced, if 1, scope-sensitive special behavior, if 2+, our required.
# Evaluating a declarative reference returns a (stash,name) pair.
#
# REFERENTIAL names, like $Foo::Bar::baz, are interpreted as referring to a
# single variable; in many cases this is used to look for a type object.
# Referential names default to MY:: if 1 token and 0 leading colon.
# Evaluating a referential name returns or binds a variable.
#
# The one exception seems to be method calls, which take a referential name
# plus an extra identifier to name the method.
#
# Trailing :: is forbidden when declaring and means .WHO when referencing.
#
# Functions for handling names in actions:
#
#   package_var: Basic function for handling referential names, produces Op.
#
#   immed_ref: Like package_var in a BEGIN context.
#
#   decl_expr:
#
#   immed_decl:

# A stash is an object like Foo::.  Stashes are named to allow them to be
# sensibly named across merges.
#
# 'my' stashes are really 'our' stashes with gensym mergable names.  Because
# stashes have no identity beyond their contents and set of names, they don't
# mind being copied around a lot.
#
# Stashes are not referencable objects in precompilation mode.  You need to
# keep the paths around, instead.
#
# This object holds the stash universe for a unit.
# XXX forward decls are a little broken
my $Package;
class Namespace {
    # all maps stash names to stashes.  Stashes are represented as simple
    # hashes here; the values are always arrays like [$xref, $file, $line].
    # $xref may be undefined to indicate a stash entry with no compile-time
    # value (our $x, my $x is export).
    #
    # Stash names are keyed like "GLOBAL::Foo::Bar" or "MAIN:15".  Stashes
    # outside GLOBAL or PROCESS are anonymous packages, for my aliasing.
    has %.all;

    # Records *local* operations, so they may be stored and used to
    # set up the runtime stashes.  Read-only log access is part of the
    # public API.
    #
    # Each entry is an arrayref of the form [$who, $name, $xref, $file, $line].
    has @.log;

    # This is set up post-creation by NieczaGrammar.  It points to a package
    # with a who of ''.
    has $.root is rw;

    method _merge_item($i1, $i2, $who, $name) {
        # supress absent entries
        return $i2 unless defined $i1;
        return $i1 unless defined $i2;

        # suppress simple COMMONs if no absent
        return $i2 unless defined $i1[0];
        return $i1 unless defined $i2[0];

        # ooh, we now know we have no COMMONs
        my $item1 = $*unit.deref($i1[0]);
        my $item2 = $*unit.deref($i2[0]);

        return $i1 if $item1 === $item2;

        if $item1.^isa($Package) && $item2.^isa($Package) &&
                $item1.who eq $item2.who &&
                ($item1.WHAT === $Package || $item2.WHAT === $Package) {
            return $i1;
        }

        die "Two definitions found for symbol {$who}::$name\n\n" ~
                "  first at $i1[1] line $i1[2]\n" ~
                "  second at $i2[1] line $i2[2]";
    }

    method exists($who, $item) {
        return ?(%!all{$who}{$item});
    }

    method get($who, $item) {
        return %!all{$who}{$item}[0]
    }

    method bind($who, $name, $item, :$file, :$line, :$pos) { #OK not used
        my $slot := %!all{$who}{$name};
        $slot = self._merge_item($slot, [ $item,
                $file // '???', $line // '???' ], $who, $name);
        push @!log, [ $who, $name, $item, $file, $line ];
    }

    method get_pkg($from is copy, *@names, :$auto) {
        for @names {
            my $sl = self.get($from.who, $_);
            my $pkg;
            if $sl && $sl[0] && ($pkg = $*unit.deref($sl)).^isa($Package) {
            } elsif !$auto {
                die "Name component $_ not found in $from.who()";
            } else {
                $pkg = $Package.new(name => $_, who => $from.who ~ '::' ~ $_);
                self.bind($from.who, $_, $pkg.xref);
            }
            $from = $pkg;
        }
        $from;
    }

    # Add a new unit set to the from-set and checks mergability
    method add_from($from) {
        for %*units{$from}.ns.log -> $logent {
            # not using bind since we don't want this in the log
            my $slot := %!all{$logent[0]}{$logent[1]};
            $slot = self._merge_item($slot, [ $logent[2], $logent[3],
                    $logent[4] ], $logent[0], $logent[1]);
        }
    }

    # List objects in a stash for use by the importer; returns pairs
    # of [name, xref]
    method list_stash($who) {
        my $h = %!all{$who};
        map -> $a { $a => $h{$a}[0] }, sort keys $h;
    }
}

class RefTarget {
    has $.xref;
    has $.name = 'ANON';

    # TODO BUILD
    method new(:$no_xref, *%_) {
        my $n = callwith(self, |%_);
        return $n if $no_xref;
        $n.xref = [ $*unit.name, +$*unit.xref, $n.name ];
        push $*unit.xref, $n;
        $n
    }

    method set_name($name) {
        $!xref[2] = $!name = $name;
    }
}

class Package is RefTarget {
    has $.closed;
    has $.who;

    method close() { $!closed = True; }
}
$Package = Package;

class Module is Package {
}

class Method {
    # normally a Str, but may be Op for param roles
    has $.name = die "Method.name is required";
    # normal, private, meta, sub
    has $.kind = die "Method.kind is required"; # Str
    has $.multi = die "Method.multi is required"; # Str
    has $.var; # Str
    has $.body; # Xref
    has $.file;
    has $.line;
}

class Attribute {
    has $.name; # Str, required
    has $.sigil; # Str, required
    has $.public; # Bool
    has $.ivar; # Str
    has $.ibody; # Xref
    has $.typeconstraint; # Xref
    has $.file;
    has $.line;
}

class Class is Module {
    has $.attributes = [];
    has $.methods = [];
    has $.superclasses = [];
    has $.linearized_mro; # is rw
    has $!closing;

    method add_attribute($name, $sigil, $public, $ivar, $ibody,
            $typeconstraint, :$file, :$line, :$pos) { #OK not used
        if grep $name eq *.name, @($!attributes) -> $O {
            die "Two definitions of attribute $name" ~ Metamodel.locstr($O[0].file, $O[0].line, $file, $line);
        }
        push $.attributes, Metamodel::Attribute.new(:$name, :$sigil,
            :$public, :$ivar, :$ibody, :$typeconstraint, :$file, :$line);
        $.attributes.[*-1];
    }

    method add_method($multi, $kind, $name, $var, $body, :$file, :$line, :$pos) { #OK not used
        if $name ~~ Str && $multi eq 'only' &&
                grep { $name eq .name && $kind eq .kind }, @($!methods) -> $O {
            die "Two definitions of method $name" ~ Metamodel.locstr($O[0].file, $O[0].line, $file, $line);
        }
        push $.methods, Metamodel::Method.new(:$name, :$body, :$kind, :$multi,
            :$file, :$line);
    }

    method add_super($targ) {
        die "bad attempt to add null super" unless $targ;
        push $.superclasses, $targ;
    }

    sub c3clear($item, @lists) {
        for @lists -> $l {
            my $i = 1;
            while $i < $l {
                return False if $*unit.deref($l[$i]) === $*unit.deref($item);
                $i++;
            }
        }

        for @lists -> $l {
            $l.shift if $l && $*unit.deref($l[0]) === $*unit.deref($item);
        }

        True;
    }

    sub c3merge(@onto, @lists) {
        my $ix = 0;
        while $ix < @lists {
            #say "C3 MRO status ($ix):";
            #say "Onto: ", @onto.map({ $*unit.deref($_).name }).join(" <- ");
            #say $_.map({ $*unit.deref($_).name }).join(" <- ") for @lists;
            #say "---";
            my $l = @lists[$ix];
            if !$l || !c3clear((my $item = $l[0]), @lists) {
                $ix++;
                next;
            }
            push @onto, $item;
            $ix = 0;
        }

        my $bad = False;
        for @lists -> $l { $bad ||= $l }
        if $bad {
            my @hrl = @lists.grep(*.Bool).map(
                { $^l.map({ $*unit.deref($^i).name }).join(" <- ") });
            die "C3-MRO wedged! @hrl.join(" | ")";
        }
    }

    method close() {
        return if $.closed;
        if ($!closing) {
            die "Class hierarchy circularty detected at $.name\n";
        }
        $!closing = True;

        if (($.name ne 'Mu' || !$*unit.is_true_setting)
                && !$.superclasses) {
            self.add_super($*CURLEX<!sub>.compile_get_pkg(self._defsuper).xref);
        }

        my @merge;
        push @merge, [ $.xref ];
        for @$.superclasses -> $x {
            my $d = $*unit.deref($x);
            $d.close unless $d.linearized_mro;
            push @merge, [ @( $d.linearized_mro ) ];
        }
        push @merge, [ @( $.superclasses ) ];
        my @mro;
        c3merge(@mro, @merge);
        $.linearized_mro = @mro;
        nextsame;
    }

    method _defsuper() { 'CORE', 'Any' }
}

# roles come in two types; Role objects are used for simple roles, while roles
# with parameters get ParametricRole.  Instantiations of parametric roles
# would get ConcreteRole, but that won't be implemented in Niecza A since it
# requires evaluating role parameters, unless we restrict it to typenames or
# something.
class Role is Module {
    has $.attributes = [];
    has $.methods = [];
    has $.superclasses = [];

    method add_attribute($name, $sigil, $public, $ivar, $ibody, $typeconstraint, :$file, :$line, :$pos) { #OK not used
        if grep $name eq *.name, @($!attributes) -> $O {
            die "Two definitions of attribute $name" ~ Metamodel.locstr($O[0].file, $O[0].line, $file, $line);
        }
        push $.attributes, Metamodel::Attribute.new(:$name, :$sigil,
            :$public, :$ivar, :$ibody, :$typeconstraint, :$file, :$line);
        $.attributes.[*-1];
    }

    method add_method($multi, $kind, $name, $var, $body, :$file, :$line, :$pos) { #OK not used
        if $name ~~ Str && $multi eq 'only' &&
                grep { $name eq .name && $kind eq .kind }, @($!methods) -> $O {
            die "Two definitions of method $name" ~ Metamodel.locstr($O[0].file, $O[0].line, $file, $line);
        }
        if $name !~~ Str {
            die "Computed names are legal only in parametric roles";
        }
        push $.methods, Metamodel::Method.new(:$name, :$body, :$kind,
            :$multi, :$file, :$line);
    }

    method add_super($targ) {
        die "bad attempt to add null super" unless $targ;
        push $.superclasses, $targ;
    }
}

class ParametricRole is Module {
    has $.attributes = [];
    has $.methods = [];
    has $.superclasses = [];

    method add_attribute($name, $sigil, $public, $ivar, $ibody, $typeconstraint, :$file, :$line, :$pos) { #OK not used
        if grep $name eq *.name, @($!attributes) -> $O {
            die "Two definitions of attribute $name" ~ Metamodel.locstr($O[0].file, $O[0].line, $file, $line);
        }
        push $.attributes, Metamodel::Attribute.new(:$name, :$sigil,
            :$public, :$ivar, :$ibody, :$typeconstraint, :$file, :$line);
        $.attributes.[*-1];
    }

    method add_method($multi, $kind, $name, $var, $body, :$file, :$line, :$pos) { #OK not used
        if $name ~~ Str && $multi eq 'only' &&
                grep { $name eq .name && $kind eq .kind }, @($!methods) -> $O {
            die "Two definitions of method $name" ~ Metamodel.locstr($O[0].file, $O[0].line, $file, $line);
        }
        push $.methods, ::Metamodel::Method.new(:$name, :$body, :$var, :$kind, :$multi, :$file, :$line);
    }

    method add_super($targ) {
        die "bad attempt to add null super" unless $targ;
        push $.superclasses, $targ;
    }
}

class Grammar is Class {
    method _defsuper() { 'CORE', 'Grammar' }
}

# subsets are a bit simpler than roles/grammars/classes, as they have
# no body and so attributes &c cannot be added to them directly.
class Subset is Module {
    # subset <longname>? <trait>* [where <EXPR>]?
    has $.basetype is rw;
    # Xref to a sub which will be called once the first time the subset
    # is used.
    has $.where is rw;
}

#####

# This is a static lexical; they exist in finite number per unit.  They may
# occupy specific slots in pads, or globals, or something else entirely.
class Lexical {
    has $.file;
    has $.line;
    has $.pos;

    # my $foo, @foo, %foo, &foo
    class Simple is Lexical {
        has Bool $.list   = False;
        has Bool $.hash   = False;
        has Bool $.noinit = False;
        has Bool $.defouter = False;
        has Bool $.roinit = False;
        has $.typeconstraint; # Xref
    }

    # These are used for $?foo et al, and should be inaccessible until assigned,
    # although the current code won't enforce that well.
    class Hint is Lexical {
    }

    # These store destinations for lexotic control transfers, and clone like
    # subs to handle recursion properly.
    class Label is Lexical {
    }

    class Dispatch is Lexical {
    }

    # our...
    class Common is Lexical {
        has $.pkg  = die "M:L:Common.path required"; # Xref to Package
        has $.name = die "M:L:Common.name required"; # Str
    }

    # mostly for state
    class Alias is Lexical {
        has $.to = die "M:L:Alias.to required"; # Str
    }

    # sub foo { ... }
    class SubDef is Lexical {
        has $.body; # Metamodel::StaticSub
    }

    # my class Foo { } or our class Foo { }; either case, the true
    # stash lives in stashland.  Actually this points at a package now.
    class Stash is Lexical {
        has $.pkg; # Xref
    }
}

# The life cycle of a static sub has three phases.
# 1. Open - the end of the sub hasn't been seen, so the full code is absent.
# 2. Closing - all attributes are available but no references exist.  The
#    perfect time for most optimizations, especially ones that look like
#    escape analyses.
# 3. Closed - references exist, possibly even from BEGIN-run code.  The sub
#    must be treated as semantically immutable.  The code can probably still
#    be changed to reflect new information, though.

# figure out how post-declared lexicals should interact with codegen
# std accepts:  sub foo() { bar }; BEGIN { foo }; sub bar() { }
# DONE: TimToady says bar can be compiled to a runtime search
class StaticSub is RefTarget {
    has $.unit; # Metamodel::Unit
    has $.outerx; # Xref
    # points directly to the outer so that explain_mystery doesn't need
    # to worry about inlining...
    has $.outer_direct is rw;
    has Bool $.run_once is rw = False;
    has Bool $.spad_exists is rw = False;
    has Bool $.transparent = False; # ignored by OUTER::
    has %.lexicals;
    has $.code is rw; # Op
    has $.signature is rw; # Sig
    has $.zyg = []; # Array of Metamodel::StaticSub

    # inject a take EMPTY
    has Bool $.gather_hack is rw = False;
    # inject a role constructor (Xref)
    has $.parametric_role_hack is rw;
    # some tuples for method definitions; munged into a phaser
    has $.augment_hack is rw;
    # emit code to assign to a hint; [ $subref, $name ]
    has $.hint_hack is rw;

    has $.is_phaser is rw; # Int
    has Bool $.strong_used is rw = False; # prevents elision
    has $.body_of  is rw; # Xref of Package
    has $.in_class is rw; # Xref of Package
    has $.cur_pkg  is rw; # Xref of Package
    has Bool $.returnable is rw = False; # catches &return
    has Bool $.augmenting is rw = False; # traps add_attribute
    has Bool $.unsafe is rw = False; # disallowed in safe mode
    has Str $.class is rw = 'Sub';
    has $.ltm is rw;
    # a place to hang off extra stuff that's not used for most subs
    # currently: "prec" for operators, "builtin" for primitives
    has $.extend is rw;

    # used during parse only
    has Str $.outervar is rw; # Xref, used during parse
    has $.methodof is rw; # Xref, used during parse
    has %.lexicals-used;
    # not just local lexicals, but all in parse; from current or any inside
    # block

    method outer() {
        $!outer_direct //= ($!outerx ?? $*unit.deref($!outerx) !! StaticSub)
    }

    method true_setting() {
        my $cursor = self;
        while $cursor && !$cursor.unit.is_true_setting {
            $cursor = $cursor.outer;
        }
        $cursor || self;
    }

    method to_unit() {
        my $cursor = self;
        my $unit = self.unit;
        my $outer;
        while ($outer = $cursor.outer) && $outer.unit === $unit {
            $cursor = $outer
        }
        $cursor;
    }

    method is_routine() {
        state %routine = (:Routine, :Sub, :Submethod, :Method, :Regex);
        %routine{$!class}
    }

    method add_child($z) { push $.zyg, $z }
    method children() { @$.zyg }

    method clear_optree() {
        $.code = Any;
        $.ltm = Any;
    }

    method create_static_pad() {
        return Nil if $.spad_exists;
        $.spad_exists = True;
        $.outer.create_static_pad if $.outer;
    }

    method noninlinable() {
        loop (my $c = self; $c && $c.unit === $*unit; $c = $c.outer) {
            $c.strong_used = True;
        }
    }

    method topicalizer() {
        $.signature && ?( grep { .slot && .slot eq '$_' }, @( $.signature.params ) )
    }

    # helper for compile_get_pkg; handles stuff like SETTING::OUTER::Foo,
    # recursively.
    method _lexy_ref(*@names, :$auto) {
        @names || die "Cannot use a lexical psuedopackage as a compile time package reference";
        self // die "Passed top of lexical tree";
        given shift @names {
            when 'OUTER'   { return self.outer._lexy_ref(@names, :$auto) }
            when 'SETTING' { return self.to_unit.outer._lexy_ref(@names, :$auto) }
            when 'UNIT'    { return self.to_unit._lexy_ref(@names, :$auto) }
            when 'CALLER'  { die "Cannot use CALLER in a compile time name" }
            default {
                my $lex = self.find_lex($_);
                $lex // die "No lexical found for $_";
                $lex.^isa(Metamodel::Lexical::Stash) || die "Lexical $_ is not a package";
                return $*unit.get_pkg($*unit.deref($lex.pkg), @names, :$auto);
            }
        }
    }

    # returns direct reference to package, or dies
    method compile_get_pkg(*@names, :$auto) {
        @names || die "Cannot make a compile time reference to the semantic root package";
        my $n0 = shift(@names);
        if $n0 eq 'OUR' {
            return $*unit.get_pkg($*unit.deref($!cur_pkg), @names, :$auto);
        } elsif $n0 eq 'PROCESS' or $n0 eq 'GLOBAL' {
            return $*unit.abs_pkg($n0, @names, :$auto);
        } elsif $n0 eq 'COMPILING' or $n0 eq 'DYNAMIC' or $n0 eq 'CALLER' {
            # Yes, COMPILING is right here.  Because COMPILING is only valid
            # when recursively running code within the compiler, but this
            # function is only called directly from the compiler.  The closest
            # it comes to making sense is if you use eval in a macro.  Don't
            # do that, okay?
            die "Pseudo package $n0 may not be used in compile time reference";
        } elsif $n0 eq 'MY' {
            return self._lexy_ref(@names, :$auto);
        } elsif $n0 eq 'CORE' {
            return self.true_setting._lexy_ref(@names, :$auto);
        } elsif $n0 eq 'OUTER' or $n0 eq 'SETTING' or $n0 eq 'UNIT' {
            return self._lexy_ref($n0, @names, :$auto);
        } elsif $n0 ne 'PARENT' && self.find_lex($n0) {
            return self._lexy_ref($n0, @names, :$auto);
        } elsif $n0 ~~ /^\W/ {
            return $*unit.get_pkg($*unit.deref($!cur_pkg), $n0, @names, :$auto);
        } else {
            return $*unit.abs_pkg('GLOBAL', $n0, @names, :$auto);
        }
    }

    method bind_our_name($path, $name, $item, *%_) {
        my $pkg = self.compile_get_pkg($path ?? @$path !! 'OUR', :auto);
        $*unit.bind($pkg, $name, $item, |%_);
    }

    method find_lex($name) {
        my $l = %!lexicals{$name};
        if $l {
            return $l.^isa(Metamodel::Lexical::Alias) ??
                self.find_lex($l.to) !! $l;
        }
        return ($.outer ?? $.outer.find_lex($name) !! Metamodel::Lexical);
    }

    method delete_lex($name) {
        my $l = %!lexicals{$name};
        if $l {
            if $l.^isa(Metamodel::Lexical::Alias) { self.delete_lex($l.to) }
            else { %!lexicals{$name}:delete }
        } else {
            $.outer && $.outer.unit === $.unit && $.outer.delete_lex($name);
        }
    }

    method add_lex($slot, $item) {
        if %!lexicals{$slot} -> $o {
            my $l = Metamodel.locstr($o.file, $o.line, $item.file, $item.line);
            if $slot ~~ /^\w/ {
                die "Illegal redeclaration of symbol '$slot'$l";
            } elsif $slot ~~ /^\&/ {
                die "Illegal redeclaration of routine '$slot.substr(1)'$l";
            } else {
                $*worry.("Useless redeclaration of variable $slot$l");
                return;
            }
        }
        # We don't know in advance if $_ exists.  This is OK.
        # TODO: The semantics are off here.  $_ should be in every block.
        elsif $slot ne '$_' && %!lexicals-used{$slot} -> $p {
            my $truename = $slot;
            my $c = self;
            while $c && !$c.lexicals{$slot} {
                $truename ~~ s/<?before \w>/OUTER::/;
                $c = $c.outer;
            }
            die "Lexical tracking inconsistency" unless $c;
            my $o = $c.lexicals{$slot};
            die "Lexical symbol '$slot' is already bound to an outer symbol{Metamodel.locstr($o.file, $o.line, $item.file, $item.line)};\n  the implicit outer binding at line $p.value() must be rewritten as $truename\n  before you can unambiguously declare a new '$slot' in this scope";
        }
        %!lexicals{$slot} = $item;
        if substr($slot,0,1) eq '&' && (%*MYSTERY{substr($slot,1)}:exists) {
            %!lexicals-used{$slot} = True;
        }
    }

    method add_my_name($slot, *%param) {
        self.add_lex($slot, Metamodel::Lexical::Simple.new(|%param));
    }

    method add_hint($slot, *%params) {
        self.add_lex($slot, Metamodel::Lexical::Hint.new(|%params));
    }

    method add_label($slot, *%params) {
        self.add_lex($slot, Metamodel::Lexical::Label.new(|%params));
    }

    method add_dispatcher($slot, *%params) {
        self.add_lex($slot, Metamodel::Lexical::Dispatch.new(|%params));
    }

    method add_common_name($slot, $pkg, $name, :$file, :$line, :$pos) {
        $*unit.bind($*unit.deref($pkg), $name, Any, :$file, :$line)
            unless $*unit.ns.exists($*unit.deref($pkg).who, $name);
        self.add_lex($slot, Metamodel::Lexical::Common.new(:$pkg, :$name,
            :$file, :$line, :$pos));
    }

    method add_state_name($slot, $back, *%param) {
        # outermost sub isn't cloned so a fallback to my is safe
        my $up = (self === self.to_unit) ?? self !! self.outer;
        $up.lexicals{$back} = Metamodel::Lexical::Simple.new(|%param);
        if defined($slot) {
            self.add_lex($slot, Metamodel::Lexical::Alias.new(to => $back,
                |%param));
        }
    }

    method add_my_stash($slot, $pkg, *%params) {
        self.add_lex($slot, Metamodel::Lexical::Stash.new(:$pkg, |%params));
    }

    method add_my_sub($slot, $body, *%params) {
        self.add_lex($slot, Metamodel::Lexical::SubDef.new(:$body, |%params));
    }

    method add_exports($name, $xref, $tags) {
        for @$tags -> $tag {
            $*unit.bind($*unit.get_pkg($*unit.deref($!cur_pkg), 'EXPORT',
                $tag, :auto), $name, $xref);
        }
        +$tags;
    }

    method close() { }
}

class Unit {
    has Metamodel::StaticSub $.mainline is rw;
    has Str $.name;
    has Metamodel::Namespace $.ns;
    has $.setting_ref is rw;
    has $.bottom_ref is rw;
    has $.xref = [];
    has $.tdeps = {};
    has Str $.filename is rw;
    has $.modtime is rw; # Numeric
    has Int $.next_anon_stash is rw = 0; # is rw, Int
    has @.stubbed_stashes; # Pair[Stash,Cursor]

    method bind($pkg,$name,$item,*%_)   { $!ns.bind($pkg.who,$name,$item,|%_) }
    method list_stash($pkg)             { $!ns.list_stash($pkg.who) }
    method get($pkg,$name)              { $!ns.get($pkg.who,$name) }
    method get_pkg($pkg,*@names,:$auto) { $!ns.get_pkg($pkg,@names,:$auto) }
    method abs_pkg(*@names, :$auto) {
        $!ns.get_pkg($*unit.deref($!ns.root),@names,:$auto)
    }

    method is_true_setting() { $!name eq 'CORE' }

    method get_unit($name) { %*units{$name} }

    method anon_stash() { "{$.name}:{$.next_anon_stash++}" }

    method deref($thing) {
        die "trying to dereference null" unless $thing;
        self.get_unit($thing[0]).xref[$thing[1]] // die "invalid ref @$thing";
    }

    method visit_units_preorder($cb) {
        my %seen;
        sub rec {
            return Nil if %seen{$_};
            %seen{$_} = True;
            for sort keys self.get_unit($_).tdeps { rec($_) }
            $cb(self.get_unit($_));
        }
        rec($.name);
    }

    method visit_local_packages($cb) {
        for @$.xref -> $x {
            $cb($x) if defined($x) && $x.^isa(Metamodel::Package);
        }
    }

    method clear_optrees() {
        self.visit_local_subs_postorder({ $_.clear_optree })
    }

    method visit_local_subs_postorder($cb) {
        sub rec {
            for $_.children { rec($_) }
            $cb($_);
        }
        rec($.mainline);
    }

    method visit_local_subs_preorder($cb) {
        sub rec {
            $cb($_);
            for $_.children { rec($_) }
        }
        rec($.mainline);
    }

    method need_unit($u2name) {
        return %*units{$u2name} if $.tdeps{$u2name};
        my $u2 = %*units{$u2name} //= $*module_loader.($u2name);
        $.tdeps{$u2name} = [ $u2.filename, $u2.modtime ];
        my @new = $u2name;
        for keys $u2.tdeps -> $k {
            next if $.tdeps{$k};
            push @new, $k;
            %*units{$k} //= $*module_loader.($k);
            $.tdeps{$k} = $u2.tdeps{$k};
        }
        $!ns.add_from($_) for @new;
        $u2;
    }
}
