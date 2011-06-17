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

# While manipulating metamodel bits, these contextuals are needed:
# @*opensubs: stack of non-transparent subs, new lexicals go in [*-1]
# $*unit: current unit for new objects to attach to
# %*units: maps unit names to unit objects

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

# A stash is an object like Foo::.  Foo and Foo:: are closely related, but
# generally must be accessed separately due to constants (which have Foo but
# not Foo::) and stub packages (vice versa).
#
# 'my' stashes are really 'our' stashes with gensym mergable names.  Because
# stashes have no identity beyond their contents and set of names, they don't
# mind being copied around a lot.
#
# Stashes are not referencable objects in precompilation mode.  You need to
# keep the paths around, instead.
#
# This object holds the stash universe for a unit.
class Namespace {
    # root points to a graph of hashes each representing one package.
    # Each such hash has keys for each member; the values are arrayrefs:
    # ["graft", [@path]]: A graft
    # ["var", $meta, $sub]: A common variable and/or subpackage; either
    # field may be undef.
    #
    # Paths do not start from GLOBAL; they start from an unnamed package
    # which contains GLOBAL, and also lexical namespaces (MAIN 15 etc).
    has $.root = {}; # is rw

    # Records *local* operations, so they may be stored and used to
    # set up the runtime stashes.  Read-only log access is part of the
    # public API.
    has $.log = [];

    method !lookup_common($used, @path_) {
        my $cursor = $.root;
        my @path = @path_;
        while @path > 1 {
            my $k = shift @path;
            if ($cursor{$k} && $cursor{$k}[0] eq 'graft') {
                ($cursor, $used) = self!lookup_common([], [ @($cursor{$k}[1]), '' ]);
                next;
            }

            $cursor{$k} //= ['var',Any,Any];
            if !defined $cursor{$k}[2] {
                $.log.push(['pkg',[@$used, $k]]);
                $cursor{$k}[2] = {};
            }
            $cursor = $cursor{$k}[2];
            push @$used, $k;
        }
        @($cursor, $used, @path);
    }

    method stash_cname(@path) {
        self!lookup_common([], @path)[1,2];
    }

    method stash_canon(@path) {
        my ($npath, $nhead) = self.stash_cname(@path);
        @$npath, $nhead;
    }

    method visit_stashes($cb) {
        sub visitor($node, @path) {
            $cb([@path]);
            for sort keys $node -> $k {
                if $node{$k}[0] eq 'var' && defined $node{$k}[2] {
                    visitor($node{$k}[2], [ @path, $k ]);
                }
            }
        }
        visitor($.root, []);
    }

    # Add a new unit set to the from-set and checks mergability
    method add_from($from) {
        $!root = _merge($!root, %*units{$from}.ns.root, []);
    }

    sub _dclone($tree) {
        return $tree unless defined $tree;
        my $rinto = { };
        for keys $tree -> $k {
            my $i = $tree{$k};
            if $i[0] eq 'var' {
                $i = ['var', $i[1], _dclone($i[2])];
            }
            $rinto{$k} = $i;
        }
        $rinto;
    }

    sub _merge_item($i1, $i2, *@path) {
        my $nn1 = $i1[0] && $i1[0][0];
        my $nn2 = $i2[0] && $i2[0][0];
        if $nn1 && $nn2 && ($i1[0][0] ne $i2[0][0] || $i1[0][1] != $i2[0][1]) {
            die "Two definitions found for package symbol [{@path}]\n\n" ~
                "  first at $i1[1] line $i1[2]\n" ~
                "  second at $i2[1] line $i2[2]";
        }

        ($nn1 ?? $i1 !! $nn2 ?? $i2 !! ($i1 // $i2))
    }

    sub _merge($rinto_, $rfrom, @path) {
        my $rinto = _hash_constructor( %$rinto_ );
        for sort keys $rfrom -> $k {
            if !$rinto{$k} {
                $rinto{$k} = $rfrom{$k};
                if $rinto{$k}[0] eq 'var' {
                    $rinto{$k} = ['var', $rinto{$k}[1], _dclone($rinto{$k}[2]) ];
                }
                next;
            }
            my $i1 = $rinto{$k};
            my $i2 = $rfrom{$k};
            if $i1[0] ne $i2[0] {
                die "Merge type conflict " ~ join(" ", $i1[0], $i2[0], @path, $k);
            }
            if $i1[0] eq 'graft' {
                die "Grafts cannot be merged " ~ join(" ", @path, $k)
                    unless join("\0", @($i1[1])) eq join("\0", @($i2[1]));
            }
            if $i1[0] eq 'var' {
                $rinto{$k} = ['var',
                    _merge_item($i1[1], $i2[1], @path, $k),
                    ((defined($i1[2]) && defined($i2[2])) ??
                        _merge($i1[2], $i2[2], [@path, $k]) !!
                    _dclone($i1[2] // $i2[2]))];
            }
        }
        return $rinto;
    }

    # Create or reuse a (stub) package for a given path
    method create_stash(@path) {
        self!lookup_common([], [@path, '']);
    }

    # Create or reuse a variable for a given path
    method create_var(@path) {
        my ($c,$u,$n) = self!lookup_common([], @path);
        my $i = $c{$n} //= ['var',Any,Any];
        if $i[0] ne 'var' {
            die "Collision with non-variable on @path";
        }
        if !$i[1] {
            $.log.push([ 'var', [ @$u,$n ] ]);
            $i[1] = [['',0],'',0];
        }
    }

    # Lookup by name; returns undef if not found
    method get_item(@path) {
        my ($c,$u,$n) = self!lookup_common([], @path); #OK not used
        my $i = $c{$n} or return Any;
        if $i[0] eq 'graft' {
            self.get_item($i[1]);
        } elsif $i[0] eq 'var' {
            $i[1][0];
        }
    }

    # Bind an unmergable thing (non-stub package) into a stash.
    method bind_item($path, $item, :$file = '???', :$line = '???', :$pos) {
        my ($c,$u,$n) = self!lookup_common([], $path); #OK not used
        my $i = $c{$n} //= ['var',Any,Any];
        if $i[0] ne 'var' {
            die "Installing item at $path, collide with graft";
        }
        $i[1] = _merge_item($i[1], [$item,$file,$line], @$path);
    }

    # Bind a graft into a stash
    method bind_graft($path1, $path2) {
        my ($c,$u,$n) = self!lookup_common([], $path1);
        if $c{$n} {
            die "Collision installing graft $path1 -> $path2";
        }
        push $.log, [ 'graft', [ @$u, $n ], $path2 ];
        $c{$n} = ['graft', $path2];
    }

    # List objects in a stash for use by the importer; returns tuples
    # of [name, var] etc
    method list_stash(@path) {
        my $c = self!lookup_common([], [@path, ''])[0];
        map { [ $_, @( $c{$_} ) ] }, sort keys $c;
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
    has $.exports; # is rw
    has $.closed;

    method add_attribute($name, $sigil, $public, $ivar, $ibody, $tc) { #OK not used
        die "attribute $name defined in a lowly package";
    }

    method add_method($multi, $kind, $name, $var, $body) { #OK not used
        die "method $name defined in a lowly package";
    }

    method add_super($super) {
        die "superclass $*unit.deref($super).name() defined in a lowly package";
    }

    method close() { $!closed = True; }
}

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
}

class Attribute {
    has $.name; # Str, required
    has $.sigil; # Str, required
    has $.public; # Bool
    has $.ivar; # Str
    has $.ibody; # Xref
    has $.typeconstraint; # Xref
}

class Class is Module {
    has $.attributes = [];
    has $.methods = [];
    has $.superclasses = [];
    has $.linearized_mro; # is rw
    has $!closing;

    method add_attribute($name, $sigil, $public, $ivar, $ibody, $typeconstraint) {
        push $.attributes, Metamodel::Attribute.new(:$name, :$sigil,
            :$public, :$ivar, :$ibody, :$typeconstraint);
        $.attributes.[*-1];
    }

    method add_method($multi, $kind, $name, $var, $body) { #OK not used
        push $.methods, Metamodel::Method.new(:$name, :$body, :$kind, :$multi);
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
            self.add_super($*unit.get_item(
                $*CURLEX<!sub>.true_setting.find_pkg(self._defsuper)));
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

    method _defsuper() { 'Any' }
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

    method add_attribute($name, $sigil, $public, $ivar, $ibody, $typeconstraint) {
        push $.attributes, Metamodel::Attribute.new(:$name, :$sigil,
            :$public, :$ivar, :$ibody, :$typeconstraint);
        $.attributes.[*-1];
    }

    method add_method($multi, $kind, $name, $var, $body) { #OK not used
        if $name !~~ Str {
            die "Computed names are legal only in parametric roles";
        }
        push $.methods, Metamodel::Method.new(:$name, :$body, :$kind,
            :$multi);
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

    method add_attribute($name, $sigil, $public, $ivar, $ibody, $typeconstraint) {
        push $.attributes, Metamodel::Attribute.new(:$name, :$sigil,
            :$public, :$ivar, :$ibody, :$typeconstraint);
        $.attributes.[*-1];
    }

    method add_method($multi, $kind, $name, $var, $body) { #OK not used
        push $.methods, ::Metamodel::Method.new(:$name, :$body, :$var, :$kind, :$multi);
    }

    method add_super($targ) {
        die "bad attempt to add null super" unless $targ;
        push $.superclasses, $targ;
    }
}

class Grammar is Class {
    method _defsuper() { 'Grammar' }
}

# subsets are a bit simpler than roles/grammars/classes, as they have
# no body and so attributes &c cannot be added to them directly.
class Subset is Module {
    # subset <longname>? <trait>* [where <EXPR>]?
    has $.basetype;
    # Xref to a sub which will be called once the first time the subset
    # is used.
    has $.where;
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
        has $.list   = False; # Bool
        has $.hash   = False; # Bool
        has $.noinit = False; # Bool
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
        has $.path = die "M:L:Common.path required"; # Array of Str
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

    # my class Foo { } or our class Foo { }; either case, the true stash lives in
    # stashland
    class Stash is Lexical {
        has $.path; # Array of Str
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
    has Bool $.strong_used is rw = False; # Bool, is rw; prevents elision
    has $.body_of is rw; # Xref of Package
    has $.in_class is rw; # Xref of Package
    has $.cur_pkg is rw; # Array of Str
    has Bool $.returnable is rw = False; # catches &return
    has Bool $.augmenting is rw = False; # traps add_attribute
    has Bool $.unsafe is rw = False; # disallowed in safe mode
    has Str $.class is rw = 'Sub';
    has $.ltm is rw;
    has $.exports is rw;
    has $.prec_info is rw;

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
        $cursor;
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

    method find_lex_pkg($name) {
        my $toplex = self.find_lex($name) // return Array;
        if !$toplex.^isa(Metamodel::Lexical::Stash) {
            die "$name is declared as a non-package";
        }
        $toplex.path;
    }

    method find_pkg($names) {
        my @names = $names ~~ Str ?? ('MY', $names) !! @$names;
        for @names { $_ = substr($_, 0, chars($_)-2) if chars($_) >= 2 && substr($_, chars($_)-2, 2) eq '::' } # XXX
        my @tp;
        if @names[0] eq 'OUR' {
            @tp = @$.cur_pkg;
            shift @names;
        } elsif @names[0] eq 'PROCESS' or @names[0] eq 'GLOBAL' {
            @tp = shift @names;
        } elsif @names[0] eq 'MY' {
            @tp = @( self.find_lex_pkg(@names[1]) // die "{@names} doesn't seem to exist" );
            shift @names;
            shift @names;
        } elsif my $p = self.find_lex_pkg(@names[0]) {
            @tp = @$p;
            shift @names;
        } else {
            @tp = 'GLOBAL';
        }

        [ @tp, @names ];
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
            $.outer && $.outer.delete_lex($name);
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

    method add_common_name($slot, $path, $name, :$file, :$line, :$pos) {
        $*unit.create_stash($path);
        $*unit.create_var([ @$path, $name ]);
        self.add_lex($slot, Metamodel::Lexical::Common.new(:$path, :$name,
            :$file, :$line, :$pos));
    }

    method add_state_name($slot, $back, *%param) {
        # outermost sub isn't cloned so a fallback to my is safe
        my $up = $.outer // self;
        $up.lexicals{$back} = Metamodel::Lexical::Simple.new(|%param);
        if defined($slot) {
            self.add_lex($slot, Metamodel::Lexical::Alias.new(to => $back,
                |%param));
    }
    }

    method add_my_stash($slot, $path, *%params) {
        self.add_lex($slot, Metamodel::Lexical::Stash.new(:$path, |%params));
    }

    method add_my_sub($slot, $body, *%params) {
        self.add_lex($slot, Metamodel::Lexical::SubDef.new(:$body, |%params));
    }

    method add_pkg_exports($unit, $name, $path2, $tags) {
        for @$tags -> $tag {
            $unit.bind_graft([@$.cur_pkg, 'EXPORT', $tag, $name], $path2);
        }
        +$tags;
    }

    # NOTE: This only marks the variables as used.  The code generator
    # still has to spit out assignments for these!
    method add_exports($unit, $name, $tags) {
        for @$tags -> $tag {
            $unit.create_var([ @$.cur_pkg, 'EXPORT', $tag, $name ]);
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

    method bind_item($path,$item,*%_) { $!ns.bind_item($path,$item,|%_) }
    method bind_graft($path1,$path2)  { $!ns.bind_graft($path1,$path2) }
    method create_stash(@path)        { $!ns.create_stash(@path) }
    method create_var(@path)          { $!ns.create_var(@path) }
    method list_stash(@path)          { $!ns.list_stash(@path) }
    method get_item(@path)            { $!ns.get_item(@path) }

    method is_true_setting() { $.name eq 'CORE' }

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
        my $u2 = %*units{$u2name} //= $*module_loader.($u2name);
        $.tdeps{$u2name} = [ $u2.filename, $u2.modtime ];
        for keys $u2.tdeps -> $k {
            %*units{$k} //= $*module_loader.($k);
            $.tdeps{$k} //= $u2.tdeps{$k};
        }
        $.ns.add_from($u2name);
        $u2;
    }
}
