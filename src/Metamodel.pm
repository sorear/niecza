use 5.010;
use strict;
use warnings;
use utf8;

package Metamodel;
use Unit;
use Body;
use Op;
use RxOp;
use Sig;
use YAML::XS;
use Scalar::Util 'blessed';

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

# these should only be used during the Op walk
our @opensubs;
our $unit;
our %units;

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
{
    package Metamodel::Namespace;
    use Moose;

    # root points to a graph of hashes each representing one package.
    # Each such hash has keys for each member; the values are arrayrefs:
    # ["graft", [@path]]: A graft
    # ["var", $meta, $sub]: A common variable and/or subpackage; either
    # field may be undef.
    #
    # Paths do not start from GLOBAL; they start from an unnamed package
    # which contains GLOBAL, and also lexical namespaces (MAIN 15 etc).
    has root => (is => 'rw', default => sub { +{} });

    # Records *local* operations, so they may be stored and used to
    # set up the runtime stashes.  Read-only log access is part of the
    # public API.
    has log => (is => 'ro', default => sub { [] });

    # These are transitional things
    sub stash_cname {
        my ($self, @path) = @_;
        (_lookup_common($self, [], @path))[1,2];
    }

    sub visit_stashes {
        my ($self, $cb) = @_;
        _visitor($self->root, $cb);
    }

    sub _visitor {
        my ($node, $cb, @path) = @_;
        $cb->(\@path);
        for my $k (sort keys %$node) {
            if ($node->{$k}[0] eq 'var' && $node->{$k}[2]) {
                _visitor($node->{$k}[2], $cb, @path, $k);
            }
        }
    }

    # Add a new unit set to the from-set and checks mergability
    sub add_from {
        my ($self, $from) = @_;
        $self->root(_merge($self->root, $units{$from}->ns->root));
    }

    sub _merge {
        my ($rinto, $rfrom, @path) = @_;
        $rinto = { %$rinto };
        for my $k (sort keys %$rfrom) {
            if (!$rinto->{$k}) {
                $rinto->{$k} = $rfrom->{$k};
                next;
            }
            my $i1 = $rinto->{$k};
            my $i2 = $rfrom->{$k};
            if ($i1->[0] ne $i2->[0]) {
                die "Merge type conflict " . join(" ", $i1->[0], $i2->[0], @path, $k);
            }
            if ($i1->[0] eq 'graft') {
                die "Grafts cannot be merged " . join(" ", @path, $k)
                    unless join("\0", @{ $i1->[1] }) eq join("\0", @{ $i2->[1] });
            }
            if ($i1->[0] eq 'var') {
                die "Non-stub packages cannot be merged " . join(" ", @path, $k)
                    if $i1->[1] && $i2->[1] && $i1->[1][0] ne $i2->[1][0] &&
                        $i1->[1][1] != $i2->[1][1];
                $rinto->{$k} = ['var', $i1->[1] // $i2->[1],
                    ($i1->[2] && $i2->[2]) ? _merge($i1->[2], $i2->[2], @path, $k) :
                    ($i1->[2] // $i2->[2])];
            }
        }
        return $rinto;
    }

    sub _lookup_common {
        my ($self, $used, @path) = @_;
        my $cursor = $self->root;
        while (@path > 1) {
            my $k = shift @path;
            if ($cursor->{$k} && $cursor->{$k}[0] eq 'graft') {
                ($cursor, $used) = _lookup_common($self, [], @{$cursor->{$k}[1]}, '');
                next;
            }

            $cursor->{$k} //= ['var',undef,undef];
            if (!$cursor->{$k}[2]) {
                push @{ $self->log }, ['pkg',[@$used, $k]];
                $cursor->{$k}[2] = {};
            }
            $cursor = $cursor->{$k}[2];
            push @$used, $k;
        }
        ($cursor, $used, @path);
    }

    # Create or reuse a (stub) package for a given path
    sub create_stash {
        my ($self, @path) = @_;
        _lookup_common($self, [], @path, '');
    }

    # Create or reuse a variable for a given path
    sub create_var {
        my ($self, @path) = @_;
        my ($c,$u,$n) = _lookup_common($self, [], @path);
        my $i = $c->{$n} //= ['var',undef,undef];
        if ($i->[0] ne 'var') {
            die "Collision with non-variable on @path";
        }
        if (!$i->[1]) {
            push @{ $self->log }, [ 'var', [ @$u,$n ] ];
            $i->[1] = ['',0];
        } elsif ($i->[1][0] ne '' || $i->[1][1] != 0) {
            die "collision with meta-object on @path";
        }
    }

    # Lookup by name; returns undef if not found
    sub get_item {
        my ($self, @path) = @_;
        my ($c,$u,$n) = _lookup_common($self, [], @path);
        my $i = $c->{$n} or return undef;
        if ($i->[0] eq 'graft') {
            return $self->get_item(@{ $i->[1] });
        } elsif ($i->[0] eq 'var') {
            return $i->[1];
        }
    }

    # Bind an unmergable thing (non-stub package) into a stash.
    sub bind_item {
        my ($self, $path, $item) = @_;
        my ($c,$u,$n) = _lookup_common($self, [], @$path);
        my $i = $c->{$n} //= ['var',undef,undef];
        if ($i->[0] ne 'var' || $i->[1]) {
            die "Collision installing pkg @$path";
        }
        $i->[1] = $item;
    }

    # Bind a graft into a stash
    sub bind_graft {
        my ($self, $path1, $path2) = @_;
        my ($c,$u,$n) = _lookup_common($self, [], @$path1);
        if ($c->{$n}) {
            die "Collision installing graft @$path1 -> @$path2";
        }
        push @{ $self->log }, [ 'graft', [ @$u, $n ], $path2 ];
        $c->{$n} = ['graft', $path2];
    }

    # List objects in a stash for use by the importer; returns tuples
    # of [name, var] etc
    sub list_stash {
        my ($self, @path) = @_;
        my ($c) = _lookup_common($self, [], @path, '');
        my @out;
        map { [ $_, @{ $c->{$_} } ] } sort keys %$c;
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::RefTarget;
    use Moose;

    has xref => (isa => 'ArrayRef', is => 'rw');

    sub BUILD {
        $_[0]->xref([ $unit->name, scalar(@{ $unit->xref }) ]);
        push @{ $unit->xref }, $_[0];
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::Package;
    use Moose;
    extends 'Metamodel::RefTarget';

    # an intrinsic name, even if anonymous
    has name => (isa => 'Str', is => 'ro', default => 'ANON');
    has exports => (is => 'rw', isa => 'ArrayRef[ArrayRef[Str]]');

    sub BUILD { push @{ $unit->packages }, $_[0] }

    sub add_attribute {
        my ($self, $name) = @_;
        die "attribute $name defined in a lowly package";
    }

    sub add_method {
        my ($self, $type, $name, $var, $body) = @_;
        die "method $name defined in a lowly package";
    }

    sub add_super {
        my ($self, $super) = @_;
        die "superclass " . $unit->deref($super)->name .
            " defined in a lowly package";
    }

    sub close { }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::Module;
    use Moose;
    extends 'Metamodel::Package';

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::Class;
    use Moose;
    extends 'Metamodel::Module';

    has attributes => (isa => 'ArrayRef[Str]', is => 'ro',
        default => sub { [] });
    has methods => (isa => 'ArrayRef[Metamodel::Method]', is => 'ro',
        default => sub { [] });
    has superclasses => (isa => 'ArrayRef', is => 'ro',
        default => sub { [] });
    has linearized_mro => (isa => 'ArrayRef[ArrayRef]', is => 'rw');

    sub add_attribute {
        my ($self, $name) = @_;
        push @{ $self->attributes }, $name;
    }

    sub add_method {
        my ($self, $type, $name, $var, $body) = @_;
        push @{ $self->methods }, Metamodel::Method->new(name => $name,
            body => $body, private => ($type eq '!'));
    }

    sub add_super {
        my ($self, $targ) = @_;
        Carp::confess "bad attempt to add null super" unless $targ;
        push @{ $self->superclasses }, $targ;
    }

    sub close {
        my ($self) = @_;
        if (($self->name ne 'Mu' || !$unit->is_true_setting)
                && !@{ $self->superclasses }) {
            $self->add_super($unit->get_item(
                    @{ $opensubs[-1]->true_setting->find_pkg($self->_defsuper) }));
        }

        my @merge;
        push @merge, [ $self->xref, @{ $self->superclasses } ];
        for (@{ $self->superclasses }) {
            my $d = $unit->deref($_);
            $d->close unless $d->linearized_mro;
            push @merge, [ @{ $d->linearized_mro } ];
        }
        my @mro;
        my %used;

        MRO: for(;;) {
            CANDIDATE: for (my $i = 0; $i < @merge; $i++) {
                my $item = $merge[$i][0];
                for (my $j = 0; $j < @merge; $j++) {
                    for (my $k = 1; $k < @{ $merge[$j] }; $k++) {
                        next CANDIDATE if $unit->deref($merge[$j][$k])
                            == $unit->deref($item);
                    }
                }
                shift @{ $merge[$i] };
                splice @merge, $i, 1 if !@{ $merge[$i] };
                push @mro, $item unless $used{$unit->deref($item)}++;
                next MRO;
            }
            last MRO if (!@merge);
            die "C3-MRO wedged! " . join(" | ", map { (join " <- ",
                    map { $unit->deref($_)->name } @$_) } @merge);
        }

        $self->linearized_mro(\@mro);
    }

    sub _defsuper { 'Any' }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

# roles come in two types; Role objects are used for simple roles, while roles
# with parameters get ParametricRole.  Instantiations of parametric roles
# would get ConcreteRole, but that won't be implemented in Niecza A since it
# requires evaluating role parameters, unless we restrict it to typenames or
# something.
{
    package Metamodel::Role;
    use Moose;
    extends 'Metamodel::Module';

    has attributes => (isa => 'ArrayRef[Str]', is => 'ro',
        default => sub { [] });
    has methods => (isa => 'ArrayRef[Metamodel::Method]', is => 'ro',
        default => sub { [] });
    has superclasses => (isa => 'ArrayRef', is => 'ro',
        default => sub { [] });

    sub add_attribute {
        my ($self, $name) = @_;
        push @{ $self->attributes }, $name;
    }

    sub add_method {
        my ($self, $type, $name, $var, $body) = @_;
        if (blessed $name) {
            die "Computed names are legal only in parametric roles";
        }
        push @{ $self->methods }, Metamodel::Method->new(name => $name,
            body => $body, private => ($type eq '!'));
    }

    sub add_super {
        my ($self, $targ) = @_;
        Carp::confess "bad attempt to add null super" unless $targ;
        push @{ $self->superclasses }, $targ;
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::ParametricRole;
    use Moose;
    extends 'Metamodel::Module';

    has builder => (isa => 'ArrayRef', is => 'rw');
    has attributes => (isa => 'ArrayRef[Str]', is => 'ro',
        default => sub { [] });
    has methods => (isa => 'ArrayRef', is => 'ro',
        default => sub { [] });
    has superclasses => (isa => 'ArrayRef', is => 'ro',
        default => sub { [] });

    sub add_attribute {
        my ($self, $name) = @_;
        push @{ $self->attributes }, $name;
    }

    sub add_method {
        my ($self, $type, $name, $var, $body) = @_;
        push @{ $self->methods }, [ $name, $var, ($type eq '!') ];
    }

    sub add_super {
        my ($self, $targ) = @_;
        Carp::confess "bad attempt to add null super" unless $targ;
        push @{ $self->superclasses }, $targ;
    }


    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::Grammar;
    use Moose;
    extends 'Metamodel::Class';

    sub _defsuper { 'Grammar' }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::Method;
    use Moose;

    has name => (isa => 'Str', is => 'ro', required => 1);
    has private => (isa => 'Bool', is => 'ro', required => 1);
    has body => (is => 'ro', required => 1);

    no Moose;
    __PACKAGE__->meta->make_immutable;
}


# This is a static lexical; they exist in finite number per unit.  They may
# occupy specific slots in pads, or globals, or something else entirely.
{
    package Metamodel::Lexical;
    use Moose;

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

# my $foo, @foo, %foo, &foo
{
    package Metamodel::Lexical::Simple;
    use Moose;
    extends 'Metamodel::Lexical';

    has list   => (isa => 'Bool', is => 'ro', default => 0);
    has hash   => (isa => 'Bool', is => 'ro', default => 0);
    has noinit => (isa => 'Bool', is => 'ro', default => 0);

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

# our...
{
    package Metamodel::Lexical::Common;
    use Moose;
    extends 'Metamodel::Lexical';

    has path => (isa => 'ArrayRef[Str]', is => 'ro', required => 1);
    has name => (isa => 'Str', is => 'ro', required => 1);

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

# mostly for state
{
    package Metamodel::Lexical::Alias;
    use Moose;
    extends 'Metamodel::Lexical';

    has to => (isa => 'Str', is => 'ro', required => 1);
    sub BUILDARGS { +{ to => $_[1] } }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

# sub foo { ... }
{
    package Metamodel::Lexical::SubDef;
    use Moose;
    extends 'Metamodel::Lexical';

    has body => (isa => 'Metamodel::StaticSub', is => 'ro');

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

# my class Foo { } or our class Foo { }; either case, the true stash lives in
# stashland
{
    package Metamodel::Lexical::Stash;
    use Moose;
    extends 'Metamodel::Lexical';

    has path => (isa => 'ArrayRef[Str]', is => 'ro');

    no Moose;
    __PACKAGE__->meta->make_immutable;
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
    has is_phaser => (isa => 'Maybe[Int]', is => 'rw', default => undef);
    has strong_used => (isa => 'Bool', is => 'rw', default => 0);
    has body_of  => (isa => 'Maybe[ArrayRef]', is => 'ro');
    has in_class => (isa => 'Maybe[ArrayRef]', is => 'ro');
    has cur_pkg  => (isa => 'Maybe[ArrayRef[Str]]', is => 'ro');
    has name     => (isa => 'Str', is => 'ro', default => 'ANON');
    has returnable => (isa => 'Bool', is => 'ro', default => 0);
    has augmenting => (isa => 'Bool', is => 'ro', default => 1);
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

    sub add_common_name { my ($self, $slot, $path, $name) = @_;
        $unit->create_stash(@$path);
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

    has setting  => (isa => 'Str', is => 'ro');
    # ref to parent of Op::YouAreHere
    has bottom_ref => (is => 'rw');

    has xref     => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has tdeps    => (isa => 'HashRef[Metamodel::Unit]', is => 'ro',
        default => sub { +{} });

    has filename => (isa => 'Str', is => 'rw');
    has modtime  => (isa => 'Num', is => 'rw');
    has syml     => (is => 'rw');

    # we like to delete staticsubs in the optimizer, so visiting them is
    # a tad harder
    has packages => (isa => 'ArrayRef[Metamodel::Package]', is => 'ro',
        default => sub { [] });
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
        $cb->($_) for @{ $self->packages };
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
            $u2->tdeps->{$_} = $self->tdeps->{$_}; # save a bit of memory
        }
        $self->ns->add_from($u2name);
        $u2;
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}


### Code goes here to build up the metamodel from an Op tree
# We should eventually wire this to the parser, so that metamodel stuff can
# exist during the parse itself; will be needed for macros

sub Unit::begin {
    my $self = shift;
    local $unit = Metamodel::Unit->new(name => $self->name,
        ns => Metamodel::Namespace->new,
        $::SETTING_UNIT ? (setting => $::SETTING_UNIT) : ());
    $units{$self->name} = $unit;

    $unit->need_unit($::SETTING_UNIT) if $::SETTING_UNIT;

    $unit->create_stash('GLOBAL');
    $unit->create_stash('PROCESS');

    local @opensubs;
    $unit->mainline($self->mainline->begin(once => 1,
            top => ($::SETTING_UNIT ? $unit->get_unit($::SETTING_UNIT)->bottom_ref : undef)));

    $unit;
}

sub Body::begin {
    my $self = shift;
    my %args = @_;

    my $top = @opensubs ? $opensubs[-1] : $args{top};
    my $rtop = !$top ? $top : Scalar::Util::blessed($top) ? $top :
        $unit->deref($top);

    my $type = $self->type // '';
    my $metabody = Metamodel::StaticSub->new(
        unit       => $unit,
        outer      => $top,
        body_of    => $args{body_of},
        in_class   => $args{body_of} // (@opensubs ? $opensubs[-1]->in_class :
            undef),
        cur_pkg    => $args{cur_pkg} // (@opensubs ? $opensubs[-1]->cur_pkg :
            [ 'GLOBAL' ]), # cur_pkg does NOT propagate down from settings
        augmenting => $args{augmenting},
        name       => ($args{prefix} // '') . $self->name,
        returnable => $self->returnable,
        gather_hack=> $args{gather_hack},
        is_phaser  => ($type eq 'init' ? 0 : $type eq 'end' ? 1 : undef),
        class      => $self->class,
        ltm        => $self->ltm,
        run_once   => $args{once} && (!@opensubs || $rtop->run_once));

    $unit->create_stash(@{ $metabody->cur_pkg });

    push @opensubs, $metabody; # always visible in the signature XXX

    if ($self->signature) {
        $self->signature->begin;
        $metabody->signature($self->signature);
    }

    if ($type && $type eq 'regex') {
        $metabody->add_my_name('$*/');
    }

    pop @opensubs if $self->transparent;

    my $do = $self->do;

    $do->begin;
    $metabody->code($do);

    $metabody->close;
    pop @opensubs unless $self->transparent;

    $metabody;
}

sub Sig::begin {
    my $self = shift;

    $_->begin for @{ $self->params };
}

sub Sig::Parameter::begin {
    my $self = shift;

    $opensubs[-1]->add_my_name($self->slot, list => $self->list,
        hash => $self->hash, noinit => 1) if defined $self->slot;
    if (defined ($self->default)) {
        $self->mdefault($self->default->begin);
        $opensubs[-1]->add_child($self->mdefault);
        delete $self->{default};
    }
}

sub Op::begin {
    my $self = shift;

    $_->begin for $self->zyg;
}

sub Op::YouAreHere::begin {
    my $self = shift;
    $unit->bottom_ref($opensubs[-1]->xref);
    $opensubs[-1]->strong_used(1);
    $opensubs[-1]->create_static_pad;
}

sub Op::Use::begin {
    my $self = shift;
    my $name = $self->unit;
    my $u2 = $unit->need_unit($self->unit);

    my @can = @{ $u2->mainline->find_pkg(['MY', split /::/, $name]) };
    my @exp = (@can, 'EXPORT', 'DEFAULT');
    my @zyg = $unit->list_stash(@exp);

    # XXX I am not sure how need binding should work in the :: case
    if ($name !~ /::/) {
        $opensubs[-1]->lexicals->{$name} =
            Metamodel::Lexical::Stash->new(path => [@can]);
    }

    for my $tup (@zyg) {
        my $uname = $tup->[0];
        my $lex;
        if ($tup->[1] eq 'var') {
            if ($tup->[2] && !$tup->[2][0]) {
                $lex = Metamodel::Lexical::Common->new(path => [@exp], name => $uname);
            } elsif ($tup->[2]) {
                $lex = Metamodel::Lexical::Stash->new(path => [@exp, $uname]);
            }
        } elsif ($tup->[1] eq 'graft') {
            $lex = Metamodel::Lexical::Stash->new(path => $tup->[2]);
        } else {
            die "weird return";
        }

        $opensubs[-1]->lexicals->{$uname} = $lex;
    }
}

sub Op::Lexical::begin {
    my $self = shift;

    if ($self->state_backing) {
        $opensubs[-1]->add_state_name($self->name, $self->state_backing,
            list => $self->list, hash => $self->hash);
    } elsif ($self->declaring) {
        $opensubs[-1]->add_my_name($self->name, list => $self->list,
            hash =>$self->hash);
    }
}

sub Op::CallMethod::begin {
    my $self = shift;

    $self->Op::begin;
    if ($self->private) {
        if ($self->ppath) {
            $self->pclass($unit->get_item(@{ $opensubs[-1]->find_pkg($self->ppath) }));
        } elsif ($opensubs[-1]->in_class) {
            $self->pclass($opensubs[-1]->in_class);
        } else {
            die "unable to resolve class of reference for method";
        }
    }
}

sub Op::ConstantDecl::begin {
    my $self = shift;

    if ($self->path) {
        $opensubs[-1]->add_common_name($self->name,
            $opensubs[-1]->find_pkg($self->path), $self->name);
    } else {
        $opensubs[-1]->add_my_name($self->name);
    }
}

sub Op::PackageVar::begin {
    my $self = shift;

    if ($self->looks_static) {
        # cache the lookup here
        $opensubs[-1]->add_common_name($self->slot,
            $opensubs[-1]->find_pkg($self->path), $self->name);
    }
}

sub Op::Attribute::begin {
    my $self = shift;
    my $ns   = $opensubs[-1]->body_of // die ("attribute " . $self->name .
        " declared outside of any class");
    die "attribute $self->name declared in an augment"
        if $opensubs[-1]->augmenting;
    $ns = $unit->deref($ns);
    $ns->add_attribute($self->name);
    my $nb = Metamodel::StaticSub->new(
        unit       => $unit,
        outer      => $opensubs[-1],
        name       => $self->name,
        cur_pkg    => $opensubs[-1]->cur_pkg,
        returnable => 0,
        class      => 'Sub',
        run_once   => 0,
        code       => Op::GetSlot->new(name => $self->name,
            object => Op::CgOp->new(optree => [ pos => 0 ])));
    $opensubs[-1]->create_static_pad; # for protosub instance
    $nb->strong_used(1);
    $opensubs[-1]->add_my_sub($self->name . '!a', $nb);
    $ns->add_method('!', $self->name, $self->name . '!a', $nb->xref);
    if ($self->accessor) {
        $ns->add_method('', $self->name, $self->name . '!a', $nb->xref);
    }
}

sub Op::Super::begin {
    my $self = shift;
    my $ns   = $opensubs[-1]->body_of // die ("superclass " . $self->name .
        " declared outside of any class");
    $ns = $unit->deref($ns);
    die "superclass $self->name declared in an augment"
        if $opensubs[-1]->augmenting;
    $ns->add_super($unit->get_item(@{ $opensubs[-1]->find_pkg([ @{ $self->path // ['MY'] }, $self->name ]) }));
}

sub Op::SubDef::begin {
    my $self = shift;
    my $prefix;
    if (defined $self->method_too) {
        $prefix = $unit->deref($opensubs[-1]->body_of)->name . ".";
    }
    my $body = $self->body->begin(prefix => $prefix,
        once => (($self->body->type // '') eq 'voidbare'));
    $opensubs[-1]->add_my_sub($self->var, $body);
    my $r = $body->xref;
    if (@{ $self->exports } || defined($self->method_too)) {
        $body->strong_used(1);
    }
    $opensubs[-1]->create_static_pad if $body->strong_used;

    if (defined($self->method_too)) {
        if ($opensubs[-1]->augment_hack) {
            if (blessed $self->method_too->[1]) {
                die "Computed names are legal only in parametric roles";
            }
            push @{ $opensubs[-1]->augment_hack },
                @{ $self->method_too }, $self->var, $r;
        } else {
            $unit->deref($opensubs[-1]->body_of)
                ->add_method(@{ $self->method_too }, $self->var, $r);
        }
    }

    $opensubs[-1]->add_exports($unit, $self->var, $self->exports);
    $body->exports([ map { [ @{ $body->cur_pkg }, 'EXPORT', $_, $self->var ] }
            @{ $self->exports } ]);

    delete $self->{$_} for (qw( body method_too exports ));
}

sub Op::VoidPhaser::begin {
    my $self = shift;
    my $body = $self->body->begin;
    $opensubs[-1]->create_static_pad;
    $opensubs[-1]->add_child($body);
    delete $self->{body};
}

sub Op::BareBlock::begin {
    my $self = shift;
    my $body = $self->body->begin;
    $opensubs[-1]->add_my_sub($self->var, $body);
    delete $self->{$_} for (qw( body ));
}

sub Op::Gather::begin {
    my $self = shift;
    my $body = $self->body->begin(gather_hack => 1);
    $opensubs[-1]->add_my_sub($self->var, $body);
    delete $self->{$_} for (qw( body ));
}

sub Op::WhateverCode::begin {
    my $self = shift;
    my $body = Body->new(name => 'ANON', transparent => 1, do => $self->ops,
        signature => Sig->simple(@{ $self->vars }));
    delete $self->{$_} for (qw( vars ops ));
    $opensubs[-1]->add_my_sub($self->slot, $body->begin);
}

sub Op::Start::begin {
    my $self = shift;
    $opensubs[-1]->add_state_name(undef, $self->condvar);
    $self->Op::begin;
}

sub Op::PackageDef::begin {
    my $self   = shift;
    my $pclass = ref($self);
    $pclass =~ s/Op::(.*)Def/Metamodel::$1/;

    if ($pclass eq 'Metamodel::Role' && $self->signature) {
        $pclass = 'Metamodel::ParametricRole';
        $self->body->signature($self->signature);
    }

    my @ns = $self->ourpkg ?
        (@{ $opensubs[-1]->find_pkg($self->ourpkg) }, $self->var) :
        ($unit->anon_stash);
    my $n = pop(@ns);

    $unit->create_stash(@ns, $n);
    $opensubs[-1]->add_my_stash($self->var, [ @ns, $n ]);
    $opensubs[-1]->add_pkg_exports($unit, $self->var, [ @ns, $n ], $self->exports);
    if (!$self->stub) {
        my $obj  = $pclass->new(name => $self->name)->xref;
        $unit->bind_item([ @ns, $n ], $obj);
        my $body = $self->body->begin(body_of => $obj, cur_pkg => [ @ns, $n ],
            once => ($pclass ne 'Metamodel::ParametricRole'));
        $unit->deref($obj)->close;
        $unit->deref($obj)->exports([
                [ @ns, $n ],
                map { [ @{ $opensubs[-1]->cur_pkg }, 'EXPORT', $_, $self->var ]}
                    @{ $self->exports } ]);

        if ($pclass eq 'Metamodel::ParametricRole') {
            $unit->deref($obj)->builder($body->xref);
            $body->parametric_role_hack($obj);
            $body->add_my_name('*params', noinit => 1);
            $body->create_static_pad;
        }
        $opensubs[-1]->add_my_sub($self->bodyvar, $body);
    }

    delete $self->{$_} for (qw(name body exports ourpkg));
}

sub Op::Augment::begin {
    my $self = shift;

    # XXX shouldn't we distinguish augment class Foo { } from ::Foo ?
    my $pkg = $opensubs[-1]->find_pkg([ @{ $self->pkg }, $self->name ]);
    my $so = $unit->get_item(@$pkg);
    my $dso = $unit->deref($so);
    if ($dso->isa('Metamodel::Role')) {
        die "illegal augment of a role";
    }
    my @ah = $so;
    my $body = $self->body->begin(augment_hack => \@ah,
        body_of => $so, augmenting => 1, once => 1, cur_pkg => $pkg);
    delete $body->{augment_hack};
    $opensubs[-1]->add_my_sub($self->bodyvar, $body);

    my $ph = Metamodel::StaticSub->new(
        unit       => $unit,
        outer      => $body,
        cur_pkg    => [ 'GLOBAL' ],
        name       => 'ANON',
        is_phaser  => 0,
        augment_hack => \@ah,
        class      => 'Sub',
        code       => Op::StatementList->new(children => []),
        run_once   => $body->run_once);
    $body->create_static_pad;
    $body->add_child($ph);

    delete $self->{$_} for (qw(name body pkg));
}

1;
