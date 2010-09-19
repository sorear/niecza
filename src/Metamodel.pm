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
# - Classes
# - Packages
# - Scopes (the border between two frames, invariant under frame merging)
#
# This graph is a lot more random than the old trees were...

our @opensubs;
our $global;

# package, class, etc.  Things with stashes, protoobjects, etc.
# We don't handle normal variables here, those exist only in the runtime
# package tree.
{
    package Metamodel::Stash;
    use Moose;

    # zyg entries can point to other stashes, to Lexical::Simple, to StaticSub
    # important: if a stash points to a lexical, it's binding to the static
    # version of the lexical
    has zyg => (isa => 'HashRef', is => 'ro',
        default => sub { +{} });
    # undef here -> stub like my class Foo { ... }
    has obj => (isa => 'Maybe[Metamodel::Package]', is => 'rw');
    has parent => (isa => 'Maybe[Metamodel::Stash]', is => 'ro');

    sub bind_name {
        my ($self, $name, $sub) = @_;
        $self->zyg->{$name} = $sub;
    }

    sub subpkg {
        my ($self, $name) = @_;
        $name =~ s/::$//; #XXX frontend brokenness
        if ($name eq 'PARENT') {
            return $self->parent // die "stash has no parent";
        }
        if ($name eq 'CALLER' || $name eq 'OUTER' || $name eq 'SETTING' ||
                $name eq 'UNIT') {
            die "$name cannot be used to descend from a package";
        }
        my $r = $self->zyg->{$name} //= Metamodel::Stash->new(parent => $self);
        if (!$r->isa('Metamodel::Stash')) {
            die "$name is a non-subpackage";
        }
        $r;
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::Package;
    use Moose;

    # an intrinsic name, even if anonymous
    has name => (isa => 'Str', is => 'ro', default => 'ANON');

    sub add_attribute {
        my ($self, $name, $accessor) = @_;
        die "attribute $name defined in a lowly package";
    }

    sub add_method {
        my ($self, $name, $body) = @_;
        die "method $name defined in a lowly package";
    }

    sub add_super {
        my ($self, $super) = @_;
        die "superclass $super->name defined in a lowly package";
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
    has superclasses => (isa => 'ArrayRef[Metamodel::Class]', is => 'ro',
        default => sub { [] });

    sub add_attribute {
        my ($self, $name, $accessor) = @_;
        push @{ $self->attributes }, $name;
        # TODO $accessor
    }

    sub add_method {
        my ($self, $name, $body) = @_;
        push @{ $self->methods }, Metamodel::Method->new(name => $name, body => $body);
        # TODO $accessor
    }

    sub add_super {
        my ($self, $targ) = @_;
        push @{ $self->superclasses }, $targ;
    }

    sub close {
        my ($self, $targ) = @_;
        # XXX should probably check that these are CORE::Mu and CORE::Any
        if ($self->name ne 'Mu' && !@{ $self->superclasses }) {
            $self->add_super($opensubs[-1]->find_lex($self->_defsuper)
                ->referent->obj);
        }
    }

    sub _defsuper { 'Any' }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::Method;
    use Moose;

    has name => (isa => 'Str', is => 'ro');
    has body => (isa => 'Metamodel::StaticSub', is => 'ro');

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

# my class Foo { } or our class Foo { }; the difference is whether some
# package also holds a ref
{
    package Metamodel::Lexical::Stash;
    use Moose;
    extends 'Metamodel::Lexical';

    has referent => (isa => 'Metamodel::Stash', is => 'ro');

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

    has outer => (isa => 'Maybe[Metamodel::StaticSub]', is => 'ro');
    has run_once => (isa => 'Bool', is => 'ro', default => 0);

    has lexicals => (isa => 'HashRef[Metamodel::Lexical]', is => 'ro',
        default => sub { +{} });
    has code     => (isa => 'Op', is => 'rw');
    has signature=> (isa => 'Maybe[Sig]', is => 'rw');
    has initq    => (isa => 'ArrayRef[Metamodel::StaticSub]', is => 'ro',
        default => sub { [] });

    has strong_used => (isa => 'Bool', is => 'rw', default => 0);
    has body_of  => (isa => 'Maybe[Metamodel::Package]', is => 'ro');
    has cur_pkg  => (isa => 'Metamodel::Stash', is => 'ro');
    has name     => (isa => 'Str', is => 'ro', default => 'ANON');
    has returnable => (isa => 'Bool', is => 'ro', default => 0);
    has augmenting => (isa => 'Bool', is => 'ro', default => 1);
    has class    => (isa => 'Str', is => 'ro', default => 'Sub');

    sub find_lex_pkg { my ($self, $name) = @_;
        my $toplex = $self->find_lex($name) // return undef;
        if (!$toplex->isa('Metamodel::Lexical::Stash')) {
            die "$name is declared as a non-package";
        }
        $toplex->referent;
    }

    sub find_pkg { my ($self, $names) = @_;
        my @names = ref($names) ? @$names : ('MY', $names);
        $_ =~ s/::$// for (@names); #XXX
        my $ptr;
        if ($names[0] eq 'OUR') {
            $ptr = $self->cur_pkg;
            shift @names;
        } elsif ($names[0] eq 'MY') {
            $ptr = $self->find_lex_pkg($names[1]);
            splice @names, 0, 2;
        } elsif ($ptr = $self->find_lex_pkg($names->[0])) {
            shift @names;
        } else {
            $ptr = $global;
        }

        for my $n (@names) {
            $ptr = $ptr->subpkg($n);
        }

        $ptr;
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

    sub add_state_name { my ($self, $slot, $back, @ops) = @_;
        # outermost sub isn't cloned so a fallback to my is safe
        my $up = $self->outer // $self;
        $up->lexicals->{$back} = Metamodel::Lexical::Simple->new(@ops);
        $self->lexicals->{$slot} = Metamodel::Lexical::Alias->new($back)
            if defined($slot);
    }

    sub add_my_stash { my ($self, $slot, $stash) = @_;
        $self->lexicals->{$slot} = Metamodel::Lexical::Stash->new(
            referent => $stash);
    }

    sub add_my_sub { my ($self, $slot, $body) = @_;
        $self->lexicals->{$slot} = Metamodel::Lexical::SubDef->new(
            body => $body);
    }

    sub add_exports { my ($self, $name, $thing, $tags) = @_;
        for my $tag (@$tags) {
            my $repo = $self->cur_pkg->subpkg('EXPORT')->subpkg($tag);
            $repo->bind_name($name, $thing);
        }
    }

    sub close { }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package Metamodel::Unit;
    use Moose;

    has mainline => (isa => 'Metamodel::StaticSub', is => 'rw');
    has global   => (isa => 'Metamodel::Stash', is => 'ro');
    has name     => (isa => 'Str', is => 'ro');

    no Moose;
    __PACKAGE__->meta->make_immutable;
}


### Code goes here to build up the metamodel from an Op tree
# We should eventually wire this to the parser, so that metamodel stuff can
# exist during the parse itself; will be needed for macros

sub Unit::begin {
    my $self = shift;
    local $global = Metamodel::Stash->new;
    my $munit = Metamodel::Unit->new(global => $global, name => $self->name);

    local @opensubs;
    $munit->mainline($self->mainline->begin(once => 1));

    $munit;
}

sub Body::begin {
    my $self = shift;
    my %args = @_;

    my $top = @opensubs ? $opensubs[-1] : undef;

    my $metabody = Metamodel::StaticSub->new(
        outer      => $top,
        body_of    => $args{body_of},
        cur_pkg    => $args{cur_pkg} // ($top ? $top->cur_pkg : $global),
        augmenting => $args{augmenting},
        name       => $self->name,
        returnable => $self->returnable,
        class      => $self->class,
        run_once   => $args{once} && (!defined($top) || $top->run_once));

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

sub Op::Attribute::begin {
    my $self = shift;
    my $ns   = $opensubs[-1]->body_of // die ("attribute " . $self->name .
        " declared outside of any class");
    die "attribute $self->name declared in an augment"
        if $opensubs[-1]->augmenting;
    $ns->add_attribute($self->name, $self->accessor);
}

sub Op::Super::begin {
    my $self = shift;
    my $ns   = $opensubs[-1]->body_of // die ("superclass " . $self->name .
        " declared outside of any class");
    die "superclass $self->name declared in an augment"
        if $opensubs[-1]->augmenting;
    $ns->add_super($opensubs[-1]->find_pkg($self->name));
}

sub Op::SubDef::begin {
    my $self = shift;
    my $body = $self->body->begin;
    $opensubs[-1]->add_my_sub($self->var, $body);
    if (defined($self->method_too)) {
        $body->strong_used(1);
        $opensubs[-1]->body_of->add_method($self->method_too, $body);
    }
    delete $self->{$_} for (qw( body method_too proto_too exports once ));
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

    my $ns = Metamodel::Stash->new;
    # XXX handle exports, ourpkg

    $opensubs[-1]->add_my_stash($self->var, $ns);

    if ($self->ourpkg) {
        my $pkg = $opensubs[-1]->find_pkg($self->ourpkg);
        $pkg->bind_name($self->var, $ns);
    }

    $opensubs[-1]->add_exports($self->var, $ns, $self->exports);

    if (!$self->stub) {
        my $obj  = $pclass->new(name => $self->name);
        my $body = $self->body->begin(body_of => $obj, cur_pkg => $ns,
            once => 1);
        $obj->close;
        $ns->obj($obj);
        $opensubs[-1]->add_my_sub($self->bodyvar, $body);
    }

    delete $self->{$_} for (qw(name body exports ourpkg));
}

sub Op::Augment::begin {
    my $self = shift;

    # XXX shouldn't we distinguish augment class Foo { } from ::Foo ?
    my $pkg = $opensubs[-1]->find_pkg([ @{ $self->pkg }, $self->name ]);
    my $body = $self->body->begin(body_of => $pkg->obj, augmenting => 1,
        once => 1, cur_pkg => $pkg);
    $opensubs[-1]->add_my_sub($self->bodyvar, $body);

    delete $self->{$_} for (qw(name body pkg));
}

1;
