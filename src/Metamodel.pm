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

# a stash is an object like Foo::.  Foo is a member of Foo::, as witnessed by
# the fact that Foo:: can exist without Foo but not vice versa; Foo is reached
# by ->obj.
#
# a stash is associated with a single unit.  each unit has a graph of stashes
# rooted from the unit's global.  'my' stashes are really 'our' stashes with
# gensym mergable names.  because stashes have no identity beyond their contents
# and set of names, they don't mind being copied around a lot.
#
# for the same reasons you generally shouldn't keep references to stashes.  ask
# for the path and look it up when needed.
{
    package Metamodel::Stash;
    use Moose;

    # zyg entries can point to:
    #  - other Stashes (but only in the same unit; also, tree structure)
    #  - Metamodel::Stash::Graft for class import/export
    #  - StaticSub (via reference)
    has zyg => (isa => 'HashRef', is => 'ro',
        default => sub { +{} });
    # not canonical, but at least usable in importers
    # 1st element is always GLOBAL or $unitname $id
    has path => (isa => 'ArrayRef[Str]', is => 'ro', required => 1);
    # undef here -> stub like my class Foo { ... }
    has obj => (isa => 'Maybe[ArrayRef]', is => 'rw');
    has parent => (isa => 'Maybe[Metamodel::Stash]', is => 'ro');

    sub bind_name {
        my ($self, $name, $sub) = @_;
        $self->zyg->{$name} = $sub;
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::Stash::Graft;
    use Moose;

    has to => (isa => 'ArrayRef[Str]', is => 'ro', required => 1);

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::Package;
    use Moose;

    has xid => (isa => 'Int', is => 'rw');
    # an intrinsic name, even if anonymous
    has name => (isa => 'Str', is => 'ro', default => 'ANON');
    has unit_closed => (isa => 'Bool', is => 'rw');

    sub BUILD { push @{ $unit->packages }, $_[0] }

    sub add_attribute {
        my ($self, $name) = @_;
        die "attribute $name defined in a lowly package";
    }

    sub add_method {
        my ($self, $name, $body) = @_;
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
    has multi_regex_lists => (isa => 'HashRef[ArrayRef]',
        is => 'ro', lazy => 1, default => sub { +{} });

    sub add_attribute {
        my ($self, $name) = @_;
        push @{ $self->attributes }, $name;
    }

    sub add_method {
        my ($self, $type, $name, $body) = @_;
        push @{ $self->methods }, Metamodel::Method->new(name => $name,
            body => $body, private => ($type eq '!'));
    }

    sub push_multi_regex {
        my ($self, $name, $body) = @_;
        push @{ $self->multi_regex_lists->{$name} //= [] }, $body;
    }

    sub add_super {
        my ($self, $targ) = @_;
        Carp::confess "bad attempt to add null super" unless $targ;
        push @{ $self->superclasses }, $targ;
    }

    sub close {
        my ($self, $targ) = @_;
        if ($self->name ne 'Mu' && $unit->is_true_setting
                && !@{ $self->superclasses }) {
            $self->add_super(
                $unit->get_stash(@{ $opensubs[-1]->find_pkg($self->_defsuper) })
                ->obj);
        }
    }

    sub _defsuper { 'Any' } #XXX CORE::Any

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

{
    package Metamodel::Lexical::SubImport;
    use Moose;
    extends 'Metamodel::Lexical';

    has ref => (isa => 'ArrayRef', is => 'ro');

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

# TODO: figure out how post-declared lexicals should interact with codegen
# std accepts:  sub foo() { bar }; BEGIN { foo }; sub bar() { }
{
    package Metamodel::StaticSub;
    use Moose;

    has unit => (isa => 'Metamodel::Unit', is => 'ro', weak_ref => 1);
    has xid => (isa => 'Int', is => 'rw');
    has outer => (is => 'bare');
    has run_once => (isa => 'Bool', is => 'ro', default => 0);
    has spad_exists => (isa => 'Bool', is => 'rw', default => 0);

    has lexicals => (isa => 'HashRef[Metamodel::Lexical]', is => 'ro',
        default => sub { +{} });
    has code     => (isa => 'Op', is => 'rw');
    has signature=> (isa => 'Maybe[Sig]', is => 'rw');

    # inject a take EMPTY
    has gather_hack => (isa => 'Bool', is => 'ro', default => 0);
    has strong_used => (isa => 'Bool', is => 'rw', default => 0);
    has body_of  => (isa => 'Maybe[ArrayRef]', is => 'ro');
    has in_class => (isa => 'Maybe[ArrayRef]', is => 'ro');
    has cur_pkg  => (isa => 'Maybe[ArrayRef[Str]]', is => 'ro');
    has name     => (isa => 'Str', is => 'ro', default => 'ANON');
    has returnable => (isa => 'Bool', is => 'ro', default => 0);
    has augmenting => (isa => 'Bool', is => 'ro', default => 1);
    has class    => (isa => 'Str', is => 'ro', default => 'Sub');
    has ltm      => (is => 'rw');

    has unit_closed => (isa => 'Bool', is => 'rw');

    sub outer {
        my $v = $_[0]{outer};
        return $v if !$v or blessed($v);
        $_[0]{unit}->deref($v);
    }

    sub children {
        map { $_->body } grep { $_->isa('Metamodel::Lexical::SubDef') }
            values %{ $_[0]->lexicals };
    }

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

    sub add_my_name { my ($self, $slot, @ops) = @_;
        $self->lexicals->{$slot} = Metamodel::Lexical::Simple->new(@ops);
    }

    sub add_common_name { my ($self, $slot, $path, $name) = @_;
        $unit->get_stash(@$path);
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
        $self->lexicals->{$slot} = Metamodel::Lexical::SubDef->new(
            body => $body);
    }

    sub add_pkg_exports { my ($self, $unit, $name, $path2, $tags) = @_;
        my $thing = Metamodel::Stash::Graft->new(to => $path2);
        for my $tag (@$tags) {
            my $repo = $unit->get_stash(@{ $self->cur_pkg }, 'EXPORT', $tag);
            $repo->bind_name($name, $thing);
        }
        scalar @$tags;
    }

    sub add_exports { my ($self, $unit, $name, $thing, $tags) = @_;
        for my $tag (@$tags) {
            my $repo = $unit->get_stash(@{ $self->cur_pkg }, 'EXPORT', $tag);
            $repo->bind_name($name, $thing);
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
    has sroot    => (isa => 'Metamodel::Stash', is => 'ro', default =>
        sub { Metamodel::Stash->new(path => []) });

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

    # XXX should be fed in perhaps from name, but this is good for testing
    sub is_true_setting { 1 }

    sub get_unit {
        my ($self, $name) = @_;
        if ($name eq $self->name) { return $self }
        $self->tdeps->{$name};
    }

    sub anon_stash {
        my $i = $_[0]->next_anon_stash;
        $_[0]->next_anon_stash($i+1);
        $_[0]->name . ":" . $i;
    }

    sub get_stash {
        my ($self, @path) = @_;
        my $ptr = $self->sroot;
        for (@path) {
            my $name = $_;
            $name =~ s/:://; #XXX frontend broken
            if ($name eq 'PARENT') {
                $ptr = $ptr->parent // die "stash has no parent";
            } elsif ($name eq 'CALLER' || $name eq 'OUTER' ||
                    $name eq 'SETTING' || $name eq 'UNIT') {
                die "$name cannot be used to descend from a package";
            } else {
                $ptr = $ptr->zyg->{$name} //=
                    Metamodel::Stash->new(parent => $ptr,
                        path => [ @{ $ptr->path }, $name ]);
                $ptr = $self->deref($ptr) if !blessed($ptr);
                if ($ptr->isa('Metamodel::Stash::Graft')) {
                    $ptr = $self->get_stash(@{ $ptr->path });
                } elsif ($ptr->isa('Metamodel::Stash')) {
                } else {
                    die "$name is a non-subpackage";
                }
            }
        }
        $ptr;
    }

    sub make_ref {
        my ($self, $thing) = @_;
        my $xid;
        if (!defined ($xid = $thing->xid)) {
            $thing->xid($xid = scalar @{ $self->xref });
            push @{ $self->xref }, $thing;
        }
        return [ $self->name, $xid ];
    }

    sub deref {
        my ($self, $thing) = @_;
        Carp::confess "trying to dereference null" unless $thing;
        return $self->get_unit($thing->[0])->xref->[$thing->[1]];
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

    sub visit_local_stashes {
        my ($self, $cb) = @_;
        our $rec; local $rec = sub {
            $cb->($_);
            for (values %{ $_->zyg }) {
                next unless blessed($_) && $_->isa('Metamodel::Stash');
                $rec->();
            }
        };
        $rec->() for $self->sroot;
    }

    sub visit_local_subs_postorder {
        my ($self, $cb) = @_;
        our $rec; local $rec = sub {
            return if $_->unit_closed;
            for ($_->children) { $rec->(); }
            $cb->($_);
        };
        for ($self->mainline) { $rec->(); }
    }

    sub visit_local_subs_preorder {
        my ($self, $cb) = @_;
        our $rec; local $rec = sub {
            return if $_->unit_closed;
            $cb->($_);
            for ($_->children) { $rec->(); }
        };
        for ($self->mainline) { $rec->(); }
    }

    # must be LAST call before Storable dump - breaks visitors!
    sub close_unit {
        my ($self) = @_;
        $self->visit_local_subs_postorder(sub { $_->unit_closed(1) });
        $self->visit_local_packages(sub { $_->unit_closed(1) });
        @{ $self->packages } = ();
    }

    sub need_unit {
        my ($self, $u2) = @_;
        $self->tdeps->{$u2->name} = $u2;
        for (keys %{ $u2->tdeps }) {
            $self->tdeps->{$_} //= $u2->tdeps->{$_};
        }
        our $rec; local $rec = sub {
            my (@path) = @_;
            my $sl = $self->get_stash(@path);
            my $sf = $u2->get_stash(@path);

            if ($sl->obj && $sf->obj && ($sl->obj->[0] ne $sf->obj->[0] ||
                    $sl->obj->[1] != $sf->obj->[1])) {
                die "unification error: clashing main objects on " .
                    join ("::", @path);
            }
            $sl->obj($sl->obj // $sf->obj);

            for my $fk (sort keys %{ $sf->zyg }) {
                my $fo = $sf->zyg->{$fk};
                my $lo = $sl->zyg->{$fk};
                if (blessed($fo) && $fo->isa('Metamodel::Stash')) {
                    if ($lo && (!blessed($lo) || !$lo->isa('Metamodel::Stash'))) {
                        die "unification error: non-stash local, stash foreign; " . join("::", @path, $fk);
                    }
                    $rec->(@path, $fk);
                } else {
                    if ($lo && blessed($lo) && $lo->isa('Metamodel::Stash')) {
                        die "unification error: stash local, non=stash foreign; " . join("::", @path, $fk);
                    }

                    $sl->bind_name($fk, $fo);
                }
            }
        };
        $rec->();
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
        $::SETTING_UNIT ? (setting => $::SETTING_UNIT->name) : ());

    $unit->need_unit($::SETTING_UNIT) if $::SETTING_UNIT;

    $unit->get_stash('GLOBAL');
    $unit->get_stash('PROCESS');

    local @opensubs;
    $unit->mainline($self->mainline->begin(once => 1,
            top => ($::SETTING_UNIT ? $::SETTING_UNIT->bottom_ref : undef)));

    $unit;
}

sub Body::begin {
    my $self = shift;
    my %args = @_;

    my $top = @opensubs ? $opensubs[-1] : $args{top};
    my $rtop = !$top ? $top : Scalar::Util::blessed($top) ? $top :
        $unit->deref($top);

    my $metabody = Metamodel::StaticSub->new(
        unit       => $unit,
        outer      => $top,
        body_of    => $args{body_of},
        in_class   => $args{body_of} // (@opensubs ? $opensubs[-1]->in_class :
            undef),
        cur_pkg    => $args{cur_pkg} // (@opensubs ? $opensubs[-1]->cur_pkg :
            [ 'GLOBAL' ]), # cur_pkg does NOT propagate down from settings
        augmenting => $args{augmenting},
        name       => $self->name,
        returnable => $self->returnable,
        gather_hack=> $args{gather_hack},
        class      => $self->class,
        ltm        => $self->ltm,
        run_once   => $args{once} && (!@opensubs || $rtop->run_once));

    if ($self->signature && @{ $self->signature->params } >= 1 &&
            $self->signature->params->[0]->slot eq '$¢') {
        $metabody->lexicals->{'$/'} = Metamodel::Lexical::Alias->new('$¢');
    }

    $unit->get_stash(@{ $metabody->cur_pkg });

    push @opensubs, $metabody; # always visible in the signature XXX

    if ($self->signature) {
        $self->signature->begin;
        $metabody->signature($self->signature);
    }

    pop @opensubs if $self->transparent;

    $self->do->begin;
    $metabody->code($self->do);

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
    $self->default->begin if defined($self->default);
}

sub Op::begin {
    my $self = shift;

    $_->begin for $self->zyg;
}

sub Op::YouAreHere::begin {
    my $self = shift;
    $unit->bottom_ref($unit->make_ref($opensubs[-1]));
    $opensubs[-1]->strong_used(1);
    $opensubs[-1]->create_static_pad;
}

sub Op::Use::begin {
    my $self = shift;
    my $name = $self->unit;
    my $u2 = CompilerDriver::metadata_for($self->unit);
    $unit->need_unit($u2);

    my @can = @{ $u2->mainline->find_pkg(split /::/, $name) };
    my $exp = $unit->get_stash(@can, 'EXPORT', 'DEFAULT');

    # XXX I am not sure how need binding should work

    for my $en (sort keys %{ $exp->zyg }) {
        my $ref = $exp->zyg->{$en};
        my $ex = blessed($ref) ? $ref : $unit->deref($ref);
        my $lex;
        if ($ex->isa('Metamodel::Stash')) {
            $lex = Metamodel::Lexical::Stash->new(path => $ex->path);
        } elsif ($ex->isa('Metamodel::Stash::Graft')) {
            $lex = Metamodel::Lexical::Stash->new(path => $ex->path);
        } elsif ($ex->isa('Metamodel::StaticSub')) {
            $lex = Metamodel::Lexical::SubImport->new(ref => $ref);
        } elsif ($ex->isa('Metamodel::ExportedVar')) {
            $lex = Metamodel::Lexical::VarImport->new(ref => $ref);
        } else {
            die "unhandled export type " . ref($ex);
        }

        $opensubs[-1]->lexicals->{$en} = $lex;
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
            $self->pclass($unit->get_stash(@{ $opensubs[-1]->find_pkg($self->ppath) })->obj);
        } elsif ($opensubs[-1]->in_class) {
            $self->pclass($opensubs[-1]->in_class);
        } else {
            die "unable to resolve class of reference for method";
        }
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
    my $r = $unit->make_ref($nb);
    $ns->add_method('!', $self->name, $r);
    if ($self->accessor) {
        $ns->add_method('', $self->name, $unit->make_ref($nb));
    }
}

sub Op::Super::begin {
    my $self = shift;
    my $ns   = $opensubs[-1]->body_of // die ("superclass " . $self->name .
        " declared outside of any class");
    $ns = $unit->deref($ns);
    die "superclass $self->name declared in an augment"
        if $opensubs[-1]->augmenting;
    $ns->add_super($unit->get_stash(@{ $opensubs[-1]->find_pkg($self->name) })->obj);
}

sub Op::SubDef::begin {
    my $self = shift;
    my $body = $self->body->begin(once => $self->once);
    $opensubs[-1]->add_my_sub($self->var, $body);
    my $r;
    if (@{ $self->exports } || defined($self->method_too) ||
            defined ($self->proto_too)) {
        $r = $unit->make_ref($body);
        $body->strong_used(1);
    }
    $opensubs[-1]->create_static_pad if $body->strong_used;

    if (defined($self->method_too)) {
        $unit->deref($opensubs[-1]->body_of)
            ->add_method(@{ $self->method_too }, $r);
    }

    if (defined($self->proto_too)) {
        $unit->deref($opensubs[-1]->body_of)
            ->push_multi_regex($self->proto_too, $r);
    }

    $opensubs[-1]->add_exports($unit, $self->var, $r, $self->exports);

    delete $self->{$_} for (qw( body method_too proto_too exports ));
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

    my $ns = $self->ourpkg ?
        [ @{ $opensubs[-1]->find_pkg($self->ourpkg) }, $self->var ] :
        [ $unit->anon_stash ];

    $opensubs[-1]->add_my_stash($self->var, $ns);
    $opensubs[-1]->add_pkg_exports($unit, $self->var, $ns, $self->exports);

    if (!$self->stub) {
        my $obj  = $unit->make_ref($pclass->new(name => $self->name));
        my $body = $self->body->begin(body_of => $obj, cur_pkg => $ns,
            once => 1);
        $unit->deref($obj)->close;
        $unit->get_stash(@$ns)->obj($obj);
        $opensubs[-1]->add_my_sub($self->bodyvar, $body);
    }

    delete $self->{$_} for (qw(name body exports ourpkg));
}

sub Op::Augment::begin {
    my $self = shift;

    # XXX shouldn't we distinguish augment class Foo { } from ::Foo ?
    my $pkg = $opensubs[-1]->find_pkg([ @{ $self->pkg }, $self->name ]);
    my $body = $self->body->begin(body_of => $unit->get_stash(@$pkg)->obj,
        augmenting => 1, once => 1, cur_pkg => $pkg);
    $opensubs[-1]->add_my_sub($self->bodyvar, $body);

    delete $self->{$_} for (qw(name body pkg));
}

1;
