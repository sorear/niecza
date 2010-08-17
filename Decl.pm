use strict;
use warnings;
use 5.010;

use CgOp;

{
    package Decl;
    use Moose;

    has zyg => (is => 'ro', isa => 'ArrayRef', default => sub { [] });

    sub used_slots   { () }
    sub preinit_code { CgOp::noop }
    sub enter_code   { CgOp::noop }

    sub outer_decls  {}
    sub bodies       {}

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::PreInit;
    use Moose;
    extends 'Decl';

    has var    => (isa => 'Str', is => 'ro', predicate => 'has_var');
    has code   => (isa => 'Body', is => 'ro', required => 1);

    sub bodies { $_[0]->code }

    sub used_slots {
        my ($self) = @_;
        $self->has_var ? [$self->var, 'Variable', 1] : ();
    }

    sub preinit_code {
        my ($self, $body) = @_;
        my $c = CgOp::subcall(CgOp::protosub($self->code));
        $self->has_var ? CgOp::proto_var($self->var, $c) : CgOp::sink($c);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::Sub;
    use Moose;
    extends 'Decl';

    has var    => (isa => 'Str', is => 'ro', required => 1);
    has code   => (isa => 'Body', is => 'ro', required => 1);

    sub bodies { $_[0]->code }

    sub used_slots {
        [$_[0]->var, 'Variable', $_[1]];
    }

    sub preinit_code {
        my ($self, $body) = @_;

        CgOp::proto_var($self->var, CgOp::newscalar(
                CgOp::protosub($self->code)));
    }

    sub enter_code {
        my ($self, $body) = @_;
        $body->mainline ? CgOp::noop : CgOp::clone_lex($self->var);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::SimpleVar;
    use Moose;
    extends 'Decl';

    has slot     => (isa => 'Str', is => 'ro', required => 1);
    has list     => (isa => 'Bool', is => 'ro', default => 0);
    has hash     => (isa => 'Bool', is => 'ro', default => 0);

    sub dynamic {
        $_[0]->slot =~ /^.?[?*]/;
    }

    sub used_slots {
        [$_[0]->slot, 'Variable', $_[1] && !$_[0]->dynamic];
    }

    sub preinit_code {
        my ($self, $body) = @_;

        if ($self->list) {
            CgOp::proto_var($self->slot, CgOp::newblanklist);
        } elsif ($self->hash) {
            CgOp::proto_var($self->slot, CgOp::newblankhash);
        } else {
            CgOp::proto_var($self->slot, CgOp::newblankrwscalar);
        }
    }

    sub enter_code {
        my ($self, $body) = @_;

        ($body->mainline && !$self->dynamic) ? CgOp::noop :
            CgOp::scopedlex($self->slot, $self->list ? CgOp::newblanklist :
                $self->hash ? CgOp::newblankhash : CgOp::newblankrwscalar);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

# only use this for classes &c which have no meaningful commoning behavior
{
    package Decl::PackageAlias;
    use Moose;
    extends 'Decl';

    has slot   => (isa => 'Str', is => 'ro', required => 1);
    has path   => (isa => 'ArrayRef[Str]', is => 'ro',
        default => sub { ['OUR'] });
    has name   => (isa => 'Str', is => 'ro', required => 1);

    sub used_slots { }

    sub preinit_code {
        my ($self, $body) = @_;

        CgOp::bind(1, ($body->lookup_var($self->name, @{ $self->path }))[1],
            CgOp::scopedlex($self->slot));
    }

    sub enter_code { }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::OurAlias;
    use Moose;
    extends 'Decl';

    has slot   => (isa => 'Str', is => 'ro', required => 1);
    has path   => (isa => 'ArrayRef[Str]', is => 'ro',
        default => sub { ['OUR'] });
    has name   => (isa => 'Str', is => 'ro', required => 1);

    sub used_slots { [ $_[0]->slot, 'Variable', 1 ] }

    sub preinit_code {
        my ($self, $body) = @_;
        my ($st, $cg) = $body->lookup_var($self->name, @{ $self->path });
        Carp::confess("bad use of OurAlias") if $st;
        CgOp::proto_var($self->slot, $cg);
    }

    sub enter_code {
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::StateVar;
    use Moose;
    extends 'Decl';

    has slot    => (isa => 'Str', is => 'ro', required => 0);
    has backing => (isa => 'Str', is => 'ro', required => 1);
    has list    => (isa => 'Bool', is => 'ro', default => 0);

    sub used_slots {
        $_[0]->slot ? [$_[0]->slot, 'Variable', 0] : ();
    }

    sub outer_decls {
        my $self = shift;
        Decl::SimpleVar->new(slot => $self->backing, list => $self->list);
    }

    sub preinit_code {
        my ($self, $body) = @_;
        $self->slot ?
            CgOp::proto_var($self->slot, CgOp::scopedlex($self->backing)) :
            CgOp::noop;
    }

    sub enter_code {
        my ($self, $body) = @_;
        $self->slot ?
            CgOp::scopedlex($self->slot, CgOp::scopedlex($self->backing)) :
            CgOp::noop;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::SaveEnv;
    use Moose;
    extends 'Decl';

    has unitname => (isa => 'Str', is => 'ro', required => 1);

    sub preinit_code {
        my ($self, $body) = @_;

        $::SETTING_RESUME = $body->scopetree;
        my $n = $self->unitname;
        $n =~ s/::/./g;

        CgOp::rawsset($n . '.Environment', CgOp::letvar('protopad'));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::Package;
    use Moose;
    extends 'Decl';

    has var     => (is => 'ro', isa => 'Str', required => 1);
    has body    => (is => 'ro', isa => 'Body');
    has bodyvar => (is => 'ro', isa => 'Str');
    has stub    => (is => 'ro', isa => 'Bool', default => 0);
    has name    => (is => 'ro', isa => 'Str', predicate => 'has_name');
    # my packages always have a unique stash, our ones just alias part of GLOBAL
    has ourpkg   => (is => 'ro', isa => 'Maybe[ArrayRef[Str]]');

    sub bodies { $_[0]->body ? $_[0]->body : () }
    sub stashvar { $_[0]->var . '::' }

    sub stash {
        my ($self, $body, $suf) = @_;
        ($body->lookup_pkg(@{ $self->ourpkg }, $self->name . $suf))[1];
    }

    sub used_slots {
        my ($self) = @_;
        [$self->var, 'Variable', 1], [$self->stashvar, 'Variable', 1],
            (!$self->stub ? [$self->bodyvar, 'Variable', $_[1]] : ());
    }

    sub make_how { CgOp::newscalar(CgOp::null('IP6')); }
    sub finish_obj { CgOp::noop; }

    sub preinit_code {
        my ($self, $body) = @_;

        if ($self->stub) {
            return CgOp::prog(
                CgOp::proto_var($self->var, CgOp::newscalar(CgOp::null('IP6'))),
                CgOp::proto_var($self->stashvar,
                    ($self->ourpkg ? $self->stash($body, '::') :
                    CgOp::wrap(CgOp::rawnew('Dictionary<string,Variable>')))));
        }

        CgOp::letn("pkg",
            ($self->ourpkg ? $self->stash($body, '::') :
                CgOp::wrap(CgOp::rawnew('Dictionary<string,Variable>'))),
            CgOp::letn("how", $self->make_how,
                # catch usages before the closing brace
                CgOp::proto_var($self->var, CgOp::newscalar(CgOp::null('IP6'))),
                CgOp::proto_var($self->var . "::", CgOp::letvar("pkg")),

                CgOp::proto_var($self->bodyvar,
                    CgOp::newscalar(
                        CgOp::protosub($self->body))),
                $self->finish_obj($body)));
    }

    sub enter_code {
        my ($self, $body) = @_;
        ($self->stub || $body->mainline) ? CgOp::noop :
            CgOp::clone_lex($self->bodyvar);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::Module;
    use Moose;
    extends 'Decl::Package';

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::Class;
    use Moose;
    extends 'Decl::Module';

    sub make_how {
        my ($self) = @_;
        CgOp::methodcall(CgOp::scopedlex("ClassHOW"), "new",
            CgOp::string_var($self->name // 'ANON'));
    }

    sub defsuper { 'Any' }

    sub finish_obj {
        my ($self, $body) = @_;
        my @r;
        if (!grep { $_->isa('Decl::Super') } @{ $self->body->decls }) {
            push @r, CgOp::sink(CgOp::methodcall(CgOp::letvar("how"),
                    "add-super", CgOp::scopedlex($self->defsuper)));
        }
        push @r, CgOp::scopedlex($self->var,
                CgOp::methodcall(CgOp::letvar("how"), "create-protoobject"));
        push @r, CgOp::bind(1, $self->stash($body, ''),
                CgOp::scopedlex($self->var)) if $self->ourpkg;
        @r;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::Grammar;
    use Moose;
    extends 'Decl::Class';

    sub defsuper { 'Grammar' }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::HasMethod;
    use Moose;
    extends 'Decl';

    has name => (is => 'ro', isa => 'Str', required => 1);
    has var  => (is => 'ro', isa => 'Str', required => 1);

    sub preinit_code {
        my ($self, $body) = @_;
        if ($body->type !~ /class|grammar|role/) {
            #TODO: Make this a sorry.
            die "Tried to set a method outside a class!";
        }
        CgOp::sink(
            CgOp::methodcall(CgOp::letvar("how"), "add-method",
                CgOp::string_var($self->name), CgOp::scopedlex($self->var)));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::HasMultiRx;
    use Moose;
    extends 'Decl';

    has name => (is => 'ro', isa => 'Str', required => 1);
    has var  => (is => 'ro', isa => 'Str', required => 1);

    sub preinit_code {
        my ($self, $body) = @_;
        CgOp::sink(
            CgOp::methodcall(CgOp::letvar("how"), "add-multiregex",
                CgOp::string_var($self->name), CgOp::scopedlex($self->var)));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::Super;
    use Moose;
    extends 'Decl';

    has name => (is => 'ro', isa => 'Str', required => 1);

    sub preinit_code {
        my ($self, $body) = @_;
        if ($body->type ne 'class' && $body->type ne 'grammar' &&
                $body->type ne 'role') {
            #TODO: Make this a sorry.
            die "Tried to set a superclass outside an initial class!";
        }

        CgOp::sink(
            CgOp::methodcall(CgOp::letvar('how'), "add-super",
                CgOp::scopedlex($self->name)));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::Attribute;
    use Moose;
    extends 'Decl';

    has name => (is => 'ro', isa => 'Str', required => 1);

    sub preinit_code {
        my ($self, $body) = @_;
        if ($body->type ne 'class' && $body->type ne 'grammar' &&
                $body->type ne 'role') {
            #TODO: Make this a sorry.
            die "Tried to set an attribute outside a class!";
        }

        CgOp::sink(
            CgOp::methodcall(CgOp::letvar('how'), "add-attribute",
                CgOp::string_var($self->name)));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::Hint;
    use Moose;
    extends 'Decl';

    has name  => (is => 'ro', isa => 'Str', required => 1);
    has value => (is => 'ro', isa => 'CgOp', required => 1);

    sub used_slots { [ $_[0]->name, 'Variable', 0 ] } #XXX
    sub preinit_code {
        my ($self, $body) = @_;
        CgOp::proto_var($self->name, $self->value);
    }

    sub enter_code {
        my ($self, $body) = @_;
        CgOp::share_lex($self->name);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::Use;
    use Moose;
    extends 'Decl';

    has unit => (is => 'ro', isa => 'Str', required => 1);
    has symbols => (isa => 'HashRef[ArrayRef[Str]]', is => 'ro', required => 1);

    sub used_slots {
        my ($self) = @_;
        map { [ $_, 'Variable', 1 ] } sort keys %{ $self->symbols };
    }

    sub preinit_code {
        my ($self, $body) = @_;
        my $scope = Storable::retrieve($self->unit . "_ast.store");

        CodeGen->know_module($self->unit);
        CgOp::prog(
            CgOp::rawscall($self->unit . '.Initialize'),
            map {
                my ($head, @path) = @{ $self->symbols->{$_} };
                CodeGen->know_sfield($scope->{$head}[1], $scope->{$head}[0]);
                my $first = CgOp::rawsget($scope->{$head}[1]);
                for (@path) {
                    $first = CgOp::rawscall('Kernel.PackageLookup',
                        CgOp::fetch($first), CgOp::clr_string($_));
                }

                CgOp::prog(
                    CgOp::proto_var($_, CgOp::newrwscalar(CgOp::fetch(
                        CgOp::scopedlex('Any')))),
                    CgOp::bind(0, CgOp::scopedlex($_), $first));
            } sort keys %{ $self->symbols });
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
