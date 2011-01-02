# 7ab22da574d860f10011a6dc4c99d2b4de3f0809
# first half of the file - begin augments are in Begin.pm6

module Metamodel;

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

    method !lookup_common($used, @path) {
        my $cursor = $.root;
        while @path > 1 {
            my $k = shift @path;
            if ($cursor{$k} && $cursor{$k}[0] eq 'graft') {
                ($cursor, $used) = self!lookup_common([], [ @($cursor{$k}[1]), '' ]);
                next;
            }

            $cursor{$k} //= ['var',Any,Any];
            if !$cursor{$k}[2] {
                $.log.push(['pkg',[@$used, $k]]);
                $cursor{$k}[2] = {};
            }
            $cursor = $cursor{$k}[2];
            push @$used, $k;
        }
        ($cursor, $used, @path);
    }

    method stash_cname(@path) {
        self!lookup_common([], @path))[1,2];
    }

    method stash_canon(@path) {
        my ($npath, $nhead) = self.stash_cname(@path);
        @$npath, $nhead;
    }

    method visit_stashes($cb) {
        sub visitor($node, @path) {
            $cb(@path);
            for sort keys $node -> $k {
                if $node{$k}[0] eq 'var' && $node{$k}[2] {
                    visitor($node{$k}[2], [ @path, $k ]);
                }
            }
        }
        _visitor($.root, []);
    }

    # Add a new unit set to the from-set and checks mergability
    method add_from($from) {
        my ($self, $from) = @_;
        $!root = _merge($!root, %*units{$from}.ns.root, []);
    }

    sub _merge($rinto, $rfrom, @path) {
        $rinto = { %$rinto };
        for sort keys $rfrom -> $k {
            if !$rinto{$k} {
                $rinto{$k} = $rfrom{$k};
                next;
            }
            my $i1 = $rinto{$k};
            my $i2 = $rfrom{$k};
            if $i1[0] ne $i2[0] {
                die "Merge type conflict " ~ join(" ", $i1[0], $i2[0], @path, $k);
            }
            if $i1[0] eq 'graft' {
                die "Grafts cannot be merged " ~ join(" ", @path, $k)
                    unless join("\0", @($i1[1])) eq join("\0", @($i2->[1]));
            }
            if $i1[0] eq 'var' {
                die "Non-stub packages cannot be merged " ~ join(" ", @path, $k)
                    if $i1[1] && $i2[1] && $i1[1][0] ne $i2[1][0] &&
                        $i1[1][1] != $i2[1][1];
                $rinto{$k} = ['var', $i1[1] // $i2[1],
                    ($i1[2] && $i2[2]) ?? _merge($i1[2], $i2[2], [@path, $k]) !!
                    ($i1[2] // $i2[2])];
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
        my ($c,$u,$n) = self!lookup_common([], @path);
        my $i = $c{$n} or return Any;
        if $i[0] eq 'graft' {
            self.get_item($i[1]);
        } elsif $i[0] eq 'var' {
            $i[1];
        }
    }

    # Bind an unmergable thing (non-stub package) into a stash.
    method bind_item($path, $item) {
        my ($c,$u,$n) = self!lookup_common([], $path);
        my $i = $c{$n} //= ['var',Any,Any];
        if $i[0] ne 'var' || $i[1] {
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
        push $self.log, [ 'graft', [ @$u, $n ], $path2 ];
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

    method add_attribute($name, $public, $ivar, $ibody) {
        die "attribute $name defined in a lowly package";
    }

    method add_method($kind, $name, $var, $body) {
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
}

class Class is Module {
    has $.attributes = [];
    has $.methods = [];
    has $.superclasses = [];
    has $.linearized_mro; # is rw
    has $!closing;

    method add_attribute($name, $public, $ivar, $ibody) {
        push $.attributes, Metamodel::Attribute.new(:$name,
            :$public, :$ivar, :$ibody);
    }

    method add_method($kind, $name, $var, $body) {
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
            my @hrl;
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
        c3merge(@mro, @mergs);
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

    method add_attribute($name, $public, $ivar, $ibody) {
        push $.attributes, Metamodel::Attribute.new(:$name,
            :$public, :$ivar, :$ibody);
    }

    method add_method($kind, $name, $var, $body) {
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

    method add_attribute($name, $public, $ivar, $ibody) {
        push $.attributes, Metamodel::Attribute.new(:$name,
            :$public, :$ivar, :$ibody);
    }

    method add_method($kind, $name, $var, $body) {
        push $.methods, Metamodel::Method.new(:$name, :$body, :$kind);
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
    }

    # These are used for $?foo et al, and should be inaccessible until assigned,
    # although the current code won't enforce that well.
    class Hint is Lexical {
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
{
    package Metamodel::StaticSub;
    use Moose;
    extends 'Metamodel::RefTarget';

    has unit => (isa => 'Metamodel::Unit', is => 'ro', weak_ref => 1);
    has outer => (is => 'bare');
    has run_once => (isa => 'Bool', is => 'ro', default => 0);
    has spad_exists => (isa => 'Bool', is => 'rw', default => 0);

    has lexicals => (isa => 'HashRef[Metamodel::Lexical]', is => 'ro',
        default => sub { +{} });
    has code     => (isa => 'Op', is => 'rw');
    has signature=> (isa => 'Maybe[Sig]', is => 'rw');
    has zyg      => (isa => 'ArrayRef[Metamodel::StaticSub]', is => 'ro',
        default => sub { [] });

    # inject a take EMPTY
    has gather_hack => (isa => 'Bool', is => 'ro', default => 0);
    # inject a role constructor
    has parametric_role_hack => (isa => 'Maybe[ArrayRef]', is => 'rw');
    # some tuples for method definitions; munged into a phaser
    has augment_hack => (isa => 'Maybe[ArrayRef]', is => 'rw');
    # emit code to assign to a hint; [ $subref, $name ]
    has hint_hack => (isa => 'Maybe[ArrayRef]', is => 'rw');
    has is_phaser => (isa => 'Maybe[Int]', is => 'rw', default => undef);
    has strong_used => (isa => 'Bool', is => 'rw', default => 0);
    has body_of  => (isa => 'Maybe[ArrayRef]', is => 'ro');
    has in_class => (isa => 'Maybe[ArrayRef]', is => 'ro');
    has cur_pkg  => (isa => 'Maybe[ArrayRef[Str]]', is => 'ro');
    has returnable => (isa => 'Bool', is => 'ro', default => 0);
    has augmenting => (isa => 'Bool', is => 'ro', default => 0);
    has class    => (isa => 'Str', is => 'ro', default => 'Sub');
    has ltm      => (is => 'rw');
    has exports  => (is => 'rw');

    sub outer {
        my $v = $_[0]{outer};
        return $v if !$v or blessed($v);
        $_[0]{unit}->deref($v);
    }

    sub true_setting {
        my $cursor = $_[0];
        while ($cursor && !$cursor->unit->is_true_setting) {
            $cursor = $cursor->outer;
        }
        $cursor;
    }

    sub add_child { push @{ $_[0]->zyg }, $_[1] }
    sub children { @{ $_[0]->zyg } }

    sub create_static_pad {
        my ($self) = @_;

        return if $self->spad_exists;
        $self->spad_exists(1);
        $self->outer->create_static_pad if $self->outer;
    }

    sub find_lex_pkg { my ($self, $name) = @_;
        my $toplex = $self->find_lex($name) // return undef;
        if (!$toplex->isa('Metamodel::Lexical::Stash')) {
            die "$name is declared as a non-package";
        }
        $toplex->path;
    }

    sub find_pkg { my ($self, $names) = @_;
        my @names = ref($names) ? @$names : ('MY', $names);
        $_ =~ s/::$// for (@names); #XXX
        my @tp;
        if ($names[0] eq 'OUR') {
            @tp = @{ $self->cur_pkg };
            shift @names;
        } elsif ($names[0] eq 'PROCESS' or $names[0] eq 'GLOBAL') {
            @tp = shift(@names);
        } elsif ($names[0] eq 'MY') {
            @tp = @{ $self->find_lex_pkg($names[1]) };
            splice @names, 0, 2;
        } elsif (my $p = $self->find_lex_pkg($names->[0])) {
            @tp = @$p;
            shift @names;
        } else {
            @tp = 'GLOBAL';
        }

        [ @tp, @names ];
    }

    sub find_lex { my ($self, $name) = @_;
        my $l = $self->lexicals->{$name};
        if ($l) {
            return $l->isa('Metamodel::Lexical::Alias') ?
                $self->find_lex($l->to) : $l;
        }
        return ($self->outer ? $self->outer->find_lex($name) : undef);
    }

    sub delete_lex { my ($self, $name) = @_;
        my $l = $self->lexicals->{$name};
        if ($l) {
            if ($l->isa('Metamodel::Lexical::Alias')) { $self->delete_lex($l->to) }
            else { delete $self->lexicals->{$name} }
        } else {
            $self->outer && $self->outer->delete_lex($name);
        }
    }

    sub add_my_name { my ($self, $slot, @ops) = @_;
        $self->lexicals->{$slot} = Metamodel::Lexical::Simple->new(@ops);
    }

    sub add_hint { my ($self, $slot) = @_;
        $self->lexicals->{$slot} = Metamodel::Lexical::Hint->new;
    }

    sub add_common_name { my ($self, $slot, $path, $name) = @_;
        $unit->create_stash(@$path);
        $unit->create_var(@$path, $name);
        $self->lexicals->{$slot} = Metamodel::Lexical::Common->new(
            path => $path, name => $name);
    }

    sub add_state_name { my ($self, $slot, $back, @ops) = @_;
        # outermost sub isn't cloned so a fallback to my is safe
        my $up = $self->outer // $self;
        $up->lexicals->{$back} = Metamodel::Lexical::Simple->new(@ops);
        $self->lexicals->{$slot} = Metamodel::Lexical::Alias->new($back)
            if defined($slot);
    }

    sub add_my_stash { my ($self, $slot, $path) = @_;
        $self->lexicals->{$slot} = Metamodel::Lexical::Stash->new(
            path => $path);
    }

    sub add_my_sub { my ($self, $slot, $body) = @_;
        $self->add_child($body);
        $self->lexicals->{$slot} = Metamodel::Lexical::SubDef->new(
            body => $body);
    }

    sub add_pkg_exports { my ($self, $unit, $name, $path2, $tags) = @_;
        for my $tag (@$tags) {
            $unit->bind_graft([@{ $self->cur_pkg }, 'EXPORT', $tag, $name],
                $path2);
        }
        scalar @$tags;
    }

    # NOTE: This only marks the variables as used.  The code generator
    # still has to spit out assignments for these!
    sub add_exports { my ($self, $unit, $name, $tags) = @_;
        for my $tag (@$tags) {
            $unit->create_var(@{ $self->cur_pkg }, 'EXPORT', $tag, $name);
        }
        scalar @$tags;
    }

    sub close { }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::Unit;
    use Moose;

    has mainline => (isa => 'Metamodel::StaticSub', is => 'rw');
    has name     => (isa => 'Str', is => 'ro');
    has ns       => (isa => 'Metamodel::Namespace', is => 'ro',
        handles => [qw/ bind_item bind_graft create_stash create_var
            list_stash get_item /]);

    has setting  => (isa => 'Maybe[Str]', is => 'ro');
    # ref to parent of Op::YouAreHere
    has bottom_ref => (is => 'rw');

    has xref     => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has tdeps    => (isa => 'HashRef[ArrayRef]', is => 'ro',
        default => sub { +{} });

    has filename => (isa => 'Maybe[Str]', is => 'rw');
    has modtime  => (isa => 'Maybe[Num]', is => 'rw');

    has next_anon_stash => (isa => 'Int', is => 'rw', default => 0);

    sub is_true_setting { $_[0]->name eq 'SAFE' || $_[0]->name eq 'CORE' }

    sub get_unit {
        my ($self, $name) = @_;
        $units{$name};
    }

    sub anon_stash {
        my $i = $_[0]->next_anon_stash;
        $_[0]->next_anon_stash($i+1);
        $_[0]->name . ":" . $i;
    }

    sub deref {
        my ($self, $thing) = @_;
        Carp::confess "trying to dereference null" unless $thing;
        return $self->get_unit($thing->[0])->xref->[$thing->[1]] //
            Carp::confess "invalid ref @$thing";
    }

    sub visit_units_preorder {
        my ($self, $cb) = @_;
        my %seen;
        our $rec; local $rec = sub {
            return if $seen{$_};
            $seen{$_} = 1;
            for (sort keys %{ $self->get_unit($_)->tdeps }) {
                $rec->();
            }
            $cb->($_) for ($self->get_unit($_));
        };
        $rec->() for ($self->name);
    }

    sub visit_local_packages {
        my ($self, $cb) = @_;
        for (@{ $self->xref }) {
            $cb->($_) if defined($_) && $_->isa('Metamodel::Package');
        }
    }

    sub visit_local_subs_postorder {
        my ($self, $cb) = @_;
        our $rec; local $rec = sub {
            for ($_->children) { $rec->(); }
            $cb->($_);
        };
        for ($self->mainline) { $rec->(); }
    }

    sub visit_local_subs_preorder {
        my ($self, $cb) = @_;
        our $rec; local $rec = sub {
            $cb->($_);
            for ($_->children) { $rec->(); }
        };
        for ($self->mainline) { $rec->(); }
    }

    sub need_unit {
        my ($self, $u2name) = @_;
        my $u2 = $units{$u2name} //= CompilerDriver::metadata_for($u2name);
        $self->tdeps->{$u2name} = [ $u2->filename, $u2->modtime ];
        for (keys %{ $u2->tdeps }) {
            $units{$_} //= CompilerDriver::metadata_for($_);
            $self->tdeps->{$_} //= $u2->tdeps->{$_};
        }
        $self->ns->add_from($u2name);
        $u2;
    }

    # STD-based frontend wants a slightly different representation.
    sub _syml_myname {
        my ($xr) = @_;
        "MY:unit<" . $xr->[0] . ">:xid<" . $xr->[1] . ">";
    }

    sub create_syml {
        my ($self) = @_;
        my $all = {};
        my %subt;

        if ($self->get_unit($self->name) != $self) {
            Carp::confess "Local unit cache inconsistant";
        }

        $self->ns->visit_stashes(sub {
            my @path = @{ $_[0] };
            return unless @path;
            my $tag = join("", map { $_ . "::" } @path);
            my $st = $all->{$tag} = bless { '!id' => $tag }, 'Stash';
            my @ppath = @path;
            my $name = pop @ppath;
            my $ptag = join("", map { $_ . "::" } @ppath);
            if ($ptag ne '') {
                $st->{'PARENT::'} = [ $ptag ];
                $all->{$ptag}{$name . '::'} = $st;
            }
            for my $tok ($self->ns->list_stash(@path)) {
                if ($tok->[1] eq 'var') {
                    my $name = $tok->[0];
                    $st->{$name} = bless { name => $name }, 'NAME';
                    $st->{'&' . $name} = $st->{$name} if $name !~ /^[\$\@\%\&]/;
                }
            }
        });

        my $top = $self->deref($self->bottom_ref // $self->mainline->xref);
        #say STDERR "Top = $top";
        for (my $cursor = $top; $cursor; $cursor = $cursor->outer) {
            my $id = _syml_myname($cursor->xref);
            #say STDERR "Creating $cursor [$id]";
            $subt{$cursor} = $all->{$id} = bless { '!id' => [ $id ] }, 'Stash';
        }

        for (my $cursor = $top; $cursor; $cursor = $cursor->outer) {
            my $st = $subt{$cursor};
            #say STDERR "Populating $cursor";
            for my $name (sort keys %{ $cursor->lexicals }) {
                my $lx = $cursor->lexicals->{$name};
                $st->{$name} = bless { name => $name }, 'NAME';
                $st->{'&' . $name} = $st->{$name} if $name !~ /^[\$\@\%\&]/;

                if ($lx->isa('Metamodel::Lexical::Stash')) {
                    my @cpath = $self->ns->stash_canon(@{ $lx->path });
                    $st->{$name . '::'} = $all->{join "", map { $_ . "::" }
                        $self->ns->stash_canon(@cpath)};
                }
            }
            $st->{'OUTER::'} = $cursor->outer ? $subt{$cursor->outer}{'!id'} :
                [];
            if ($cursor->unit->bottom_ref && $cursor->unit->name eq 'CORE') {
                $all->{'CORE'} //= $st;
            }
            if ($cursor->unit->bottom_ref) {
                $all->{'SETTING'} //= $st;
            }
        }
        #say STDERR "UNIT ", $self->mainline;
        #$all->{'UNIT'} = $subt{$self->mainline};
        {
            my @nbits = @{ $self->mainline->find_pkg(['MY', split '::', $self->name]) };
            @nbits = $self->ns->stash_canon(@nbits);
            # XXX wrong, but makes STD importing work
            # say STDERR (YAML::XS::Dump @nbits);
            $all->{'UNIT'} = $all->{join "", map { $_ . '::' } @nbits};
        }
        # say STDERR (YAML::XS::Dump("Regenerated syml for " . $self->name, $all));
        $all;
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}
