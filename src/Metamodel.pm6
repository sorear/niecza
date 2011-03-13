# first half of the file - begin augments are in Begin.pm6

module Metamodel;

use NAME;
use Stash;

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
                my $nn1 = $i1[1] && $i1[1][0];
                my $nn2 = $i2[1] && $i2[1][0];
                die "Non-stub packages cannot be merged " ~ join(" ", @path, $k)
                    if $nn1 && $nn2 && ($i1[1][0] ne $i2[1][0] ||
                        $i1[1][1] != $i2[1][1]);
                $rinto{$k} = ['var',
                    ($nn1 ?? $i1[1] !! $nn2 ?? $i2[1] !! ($i1[1] // $i2[1])),
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
            $i[1] = ['',0];
        }
    }

    # Lookup by name; returns undef if not found
    method get_item(@path) {
        my ($c,$u,$n) = self!lookup_common([], @path); #OK not used
        my $i = $c{$n} or return Any;
        if $i[0] eq 'graft' {
            self.get_item($i[1]);
        } elsif $i[0] eq 'var' {
            $i[1];
        }
    }

    # Bind an unmergable thing (non-stub package) into a stash.
    method bind_item($path, $item) {
        my ($c,$u,$n) = self!lookup_common([], $path); #OK not used
        my $i = $c{$n} //= ['var',Any,Any];
        if $i[0] ne 'var' || $i[1] && $i[1][0] {
            die "Collision installing pkg $path";
        }
        $i[1] = $item;
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
    has $.xref; # is rw
    has $.name = 'ANON';

    # TODO BUILD
    method new(*%_) {
        my $n = self.CREATE(|%_);
        $n.xref = [ $*unit.name, +$*unit.xref, $n.name ];
        push $*unit.xref, $n;
        $n
    }
}

class Package is RefTarget {
    has $.exports; # is rw

    method add_attribute($name, $public, $ivar, $ibody, $tc) { #OK not used
        die "attribute $name defined in a lowly package";
    }

    method add_method($kind, $name, $var, $body) { #OK not used
        die "method $name defined in a lowly package";
    }

    method add_super($super) {
        die "superclass $*unit.deref($super).name() defined in a lowly package";
    }

    method close() { }
}

class Module is Package {
}

class Method {
    # normally a Str, but may be Op for param roles
    has $.name = die "Method.name is required";
    # normal, private, meta, sub
    has $.kind = die "Method.kind is required"; # Str
    has $.var; # Str
    has $.body; # Xref
}

class Attribute {
    has $.name; # Str, required
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

    method add_attribute($name, $public, $ivar, $ibody, $typeconstraint) {
        push $.attributes, Metamodel::Attribute.new(:$name,
            :$public, :$ivar, :$ibody, :$typeconstraint);
    }

    method add_method($kind, $name, $var, $body) { #OK not used
        push $.methods, Metamodel::Method.new(:$name, :$body, :$kind);
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
        return Nil if $.linearized_mro;
        if ($!closing) {
            die "Class hierarchy circularty detected at $.name\n";
        }
        $!closing = True;

        if (($.name ne 'Mu' || !$*unit.is_true_setting)
                && !$.superclasses) {
            self.add_super($*unit.get_item(
                    @*opensubs[*-1].true_setting.find_pkg(self._defsuper)));
        }

        my @merge;
        push @merge, [ $.xref, @( $.superclasses ) ];
        for @$.superclasses -> $x {
            my $d = $*unit.deref($x);
            $d.close unless $d.linearized_mro;
            push @merge, [ @( $d.linearized_mro ) ];
        }
        my @mro;
        c3merge(@mro, @merge);
        $.linearized_mro = @mro;
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

    method add_attribute($name, $public, $ivar, $ibody, $typeconstraint) {
        push $.attributes, Metamodel::Attribute.new(:$name,
            :$public, :$ivar, :$ibody, :$typeconstraint);
    }

    method add_method($kind, $name, $var, $body) { #OK not used
        if $name !~~ Str {
            die "Computed names are legal only in parametric roles";
        }
        push $.methods, Metamodel::Method.new(:$name, :$body, :$kind);
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

    method add_attribute($name, $public, $ivar, $ibody, $typeconstraint) {
        push $.attributes, Metamodel::Attribute.new(:$name,
            :$public, :$ivar, :$ibody, :$typeconstraint);
    }

    method add_method($kind, $name, $var, $body) { #OK not used
        push $.methods, ::Metamodel::Method.new(:$name, :$body, :$var, :$kind);
    }

    method add_super($targ) {
        die "bad attempt to add null super" unless $targ;
        push $.superclasses, $targ;
    }
}

class Grammar is Class {
    method _defsuper() { 'Grammar' }
}

#####

# This is a static lexical; they exist in finite number per unit.  They may
# occupy specific slots in pads, or globals, or something else entirely.
class Lexical {
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

    # our...
    class Common is Lexical {
        has $.path = die "M:L:Common.path required"; # Array of Str
        has $.name = die "M:L:Common.name required"; # Str
    }

    # mostly for state
    class Alias is Lexical {
        has $.to = die "M:L:Alias.to required"; # Str

        method new($to) { self.CREATE(:$to) }
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
    has $.run_once = False; # Bool
    has $.spad_exists = False; # Bool
    has $.transparent = False; # Bool; ignored by OUTER::
    has $.lexicals = {};
    has $.code; # Op, is rw
    has $.signature; # Sig, is rw
    has $.zyg = []; # Array of Metamodel::StaticSub

    # inject a take EMPTY
    has $.gather_hack = False; # Bool
    # inject a role constructor
    has $.parametric_role_hack; # Xref, is rw
    # some tuples for method definitions; munged into a phaser
    has $.augment_hack; # Array, is rw
    # emit code to assign to a hint; [ $subref, $name ]
    has $.hint_hack; # Array, is rw

    has $.is_phaser; # Int, is rw
    has $.strong_used = False; # Bool, is rw; prevents elision
    has $.body_of; # Xref of Package
    has $.in_class; # Xref of Package
    has $.cur_pkg; # Array of Str
    has $.returnable = False; # Bool; catches &return
    has $.augmenting = False; # Bool; traps add_attribute
    has $.unsafe = False; # Bool; disallowed in safe mode
    has $.class = 'Sub'; # Str
    has $.ltm; # is rw
    has $.exports; # is rw

    method outer() { $!outerx ?? $*unit.deref($!outerx) !! StaticSub }

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
        my $l = $.lexicals{$name};
        if $l {
            return $l.^isa(Metamodel::Lexical::Alias) ??
                self.find_lex($l.to) !! $l;
        }
        return ($.outer ?? $.outer.find_lex($name) !! Metamodel::Lexical);
    }

    method delete_lex($name) {
        my $l = $.lexicals{$name};
        if $l {
            if $l.^isa(Metamodel::Lexical::Alias) { self.delete_lex($l.to) }
            else { $.lexicals{$name}:delete }
        } else {
            $.outer && $.outer.delete_lex($name);
        }
    }

    method add_my_name($slot, *%param) {
        $.lexicals{$slot} = Metamodel::Lexical::Simple.new(|%param);
    }

    method add_hint($slot) {
        $.lexicals{$slot} = Metamodel::Lexical::Hint.new;
    }

    method add_label($slot) {
        $.lexicals{$slot} = Metamodel::Lexical::Label.new;
    }

    method add_common_name($slot, $path, $name) {
        $*unit.create_stash($path);
        $*unit.create_var([ @$path, $name ]);
        $.lexicals{$slot} = Metamodel::Lexical::Common.new(:$path, :$name);
    }

    method add_state_name($slot, $back, *%param) {
        # outermost sub isn't cloned so a fallback to my is safe
        my $up = $.outer // self;
        $up.lexicals{$back} = Metamodel::Lexical::Simple.new(|%param);
        $.lexicals{$slot} = Metamodel::Lexical::Alias.new($back)
            if defined($slot);
    }

    method add_my_stash($slot, $path) {
        $.lexicals{$slot} = Metamodel::Lexical::Stash.new(:$path);
    }

    method add_my_sub($slot, $body) {
        self.add_child($body);
        $.lexicals{$slot} = Metamodel::Lexical::SubDef.new(:$body);
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
    has $.mainline; # Metamodel::StaticSub, is rw
    has $.name; # Str
    has $.ns; # Metamodel::Namespace
    has $.setting; # Str
    has $.bottom_ref; # is rw
    has $.xref = [];
    has $.tdeps = {};
    has $.filename; # is rw, Str
    has $.modtime; # is rw, Numeric
    has $.next_anon_stash = 0; # is rw, Int

    method bind_item($path,$item)    { $!ns.bind_item($path,$item) }
    method bind_graft($path1,$path2) { $!ns.bind_graft($path1,$path2) }
    method create_stash(@path)       { $!ns.create_stash(@path) }
    method create_var(@path)         { $!ns.create_var(@path) }
    method list_stash(@path)         { $!ns.list_stash(@path) }
    method get_item(@path)           { $!ns.get_item(@path) }

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

    # STD-based frontend wants a slightly different representation.
    sub _syml_myname($xr) { "MY:unit<$xr[0]>:xid<$xr[1]>" }

    method create_syml() {
        my $all = {};
        my $*unit = self;

        if (self.get_unit($.name) !=== self) {
            die "Local unit cache inconsistant";
        }

        $.ns.visit_stashes(sub (@path) {
            return Nil unless @path;
            my $tag = join("", map { $_ ~ "::" }, @path);
            my $st = $all{$tag} = Stash.new('!id' => [$tag]);
            my @ppath = @path;
            my $name = pop @ppath;
            my $ptag = join("", map { $_ ~ "::" }, @ppath);
            if $ptag ne '' {
                $st<PARENT::> = [ $ptag ];
                $all{$ptag}{$name ~ '::'} = $st;
            }
            for self.list_stash(@path) -> $tok {
                if $tok[1] eq 'var' {
                    my $name = $tok[0];
                    $st{$name} = NAME.new( name => $name );
                    $st{'&' ~ $name} = $st{$name} if $name !~~ /^<[\$\@\%\&]>/;
                }
            }
        });

        my $top = self.deref($.bottom_ref // $.mainline.xref);
        #say STDERR "Top = $top";
        my $cursor = $top;
        while $cursor {
            my $id = _syml_myname($cursor.xref);
            #say STDERR "Creating $cursor [$id]";
            $all{$id} = Stash.new( '!id' => [ $id ] );
            $cursor = $cursor.outer;
        }

        $cursor = $top;
        while $cursor {
            my $st = $all{_syml_myname($cursor.xref)};
            #say STDERR "Populating $cursor";
            for sort keys $cursor.lexicals -> $name {
                my $lx = $cursor.lexicals{$name};
                $st{$name} = NAME.new( name => $name );
                $st{'&' ~ $name} = $st{$name} if $name !~~ /^<[\$\@\%\&]>/;

                if $lx.^isa(Metamodel::Lexical::Stash) {
                    my @cpath = $.ns.stash_canon($lx.path);
                    $st{$name ~ '::'} = $all{join "", map { $_ ~ "::" },
                        $.ns.stash_canon(@cpath)};
                }
            }
            $st<OUTER::> = $cursor.outer ?? $all{_syml_myname($cursor.outerx)}<!id> !! [];
            if ($cursor.unit.bottom_ref && $cursor.unit.name eq 'CORE') {
                $all<CORE> //= $st;
            }
            if ($cursor.unit.bottom_ref) {
                $all<SETTING> //= $st;
            }
            $cursor = $cursor.outer;
        }
        #say STDERR "UNIT ", $self->mainline;
        #$all->{'UNIT'} = $subt{$self->mainline};
        {
            my @nbits = @( $.mainline.find_pkg([$.name.split('::')]) );
            @nbits = $.ns.stash_canon(@nbits);
            # XXX wrong, but makes STD importing work
            # say STDERR (YAML::XS::Dump @nbits);
            $all<UNIT> = $all{join "", map { $_ ~ '::' }, @nbits};
        }
        # say STDERR (YAML::XS::Dump("Regenerated syml for " . $self->name, $all));
        $all;
    }
}
