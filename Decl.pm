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
    sub write        {}

    sub extra_decls  {}
    sub outer_decls  {}

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::PreInit;
    use Moose;
    extends 'Decl';

    has var    => (isa => 'Str', is => 'ro', predicate => 'has_var');
    has code   => (isa => 'Body', is => 'ro', required => 1);
    has shared => (isa => 'Bool', is => 'ro', default => 0);

    sub extra_decls { $_[0]->code->floated_decls }

    sub used_slots {
        my ($self) = @_;
        $self->has_var ? ($self->var, 'Variable') : ();
    }

    sub preinit_code {
        my ($self, $body) = @_;
        $self->code->outer($body);
        my $c = CgOp::subcall(CgOp::protosub($self->code));
        $self->has_var ? CgOp::proto_var($self->var, $c) : CgOp::sink($c);
    }

    sub enter_code {
        my ($self, $body) = @_;
        !$self->has_var ? CgOp::noop :
            ($self->shared || $body->mainline) ? CgOp::share_lex($self->var) :
            CgOp::copy_lex($self->var);
    }

    sub write {
        my ($self, $body) = @_;
        $self->code->outer($body);
        $self->code->write;
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

    sub extra_decls { $_[0]->code->floated_decls }

    sub used_slots {
        $_[0]->var, 'Variable';
    }

    sub preinit_code {
        my ($self, $body) = @_;
        $self->code->outer($body);

        CgOp::proto_var($self->var, CgOp::newscalar(
                CgOp::protosub($self->code)));
    }

    sub enter_code {
        my ($self, $body) = @_;
        $body->mainline ?
            CgOp::share_lex($self->var) :
            CgOp::clone_lex($self->var);
    }

    sub write {
        my ($self, $body) = @_;
        $self->code->outer($body);
        $self->code->write;
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
    has shared   => (isa => 'Bool', is => 'ro', default => 0);
    has zeroinit => (isa => 'Bool', is => 'ro', default => 0);

    sub used_slots {
        $_[0]->slot, 'Variable';
    }

    sub preinit_code {
        my ($self, $body) = @_;

        if ($self->zeroinit) {
            CgOp::proto_var($self->slot, CgOp::newrwscalar(CgOp::null('IP6')));
        } elsif ($self->list) {
            CgOp::proto_var($self->slot,
                CgOp::newrwlistvar(CgOp::fetch(CgOp::scopedlex('Any'))));
        } else {
            CgOp::proto_var($self->slot,
                CgOp::newrwscalar(CgOp::fetch(CgOp::scopedlex('Any'))));
        }
    }

    sub enter_code {
        my ($self, $body) = @_;

        ($body->mainline || $self->shared) ?
            CgOp::share_lex($self->slot) :
            CgOp::copy_lex($self->slot, $self->list);
    }

    sub write {
        my ($self, $body) = @_;
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

        CgOp::bind(1, $body->lookup_var($self->name, @{ $self->path }),
            CgOp::scopedlex($self->slot));
    }

    sub enter_code { }

    sub write {
        my ($self, $body) = @_;
    }

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

    sub used_slots { $_[0]->slot, 'Variable' }

    sub preinit_code {
        my ($self, $body) = @_;

        CgOp::proto_var($self->slot,
            $body->lookup_var($self->name, @{ $self->path }));
    }

    sub enter_code {
        my ($self, $body) = @_;

        CgOp::share_lex($self->slot);
    }

    sub write {
        my ($self, $body) = @_;
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
        $_[0]->slot ? ($_[0]->slot, 'Variable') : ();
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

        # XXX ought not to have side effects here.
        $::SETTING_RESUME = $body->gen_setting;
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

    sub extra_decls { $_[0]->body ? ($_[0]->body->floated_decls) : () }
    sub stashvar { $_[0]->var . '::' }

    sub used_slots {
        my ($self) = @_;
        $self->var, 'Variable', $self->stashvar,
            'Variable', (!$self->stub ? ($self->bodyvar, 'Variable') : ());
    }

    sub make_how { CgOp::newscalar(CgOp::null('IP6')); }
    sub finish_obj { CgOp::noop; }

    sub preinit_code {
        my ($self, $body) = @_;

        if ($self->stub) {
            return CgOp::prog(
                CgOp::proto_var($self->var, CgOp::newscalar(CgOp::null('IP6'))),
                CgOp::proto_var($self->stashvar,
                    CgOp::wrap(CgOp::rawnew('Dictionary<string,Variable>'))));
        }

        $self->body->outer($body);

        CgOp::letn("pkg",
            CgOp::wrap(CgOp::rawnew('Dictionary<string,Variable>')),
            CgOp::letn("how", $self->make_how,
                # catch usages before the closing brace
                CgOp::proto_var($self->var, CgOp::newscalar(CgOp::null('IP6'))),
                CgOp::proto_var($self->var . "::", CgOp::letvar("pkg")),

                CgOp::proto_var($self->bodyvar,
                    CgOp::newscalar(
                        CgOp::protosub($self->body))),
                $self->finish_obj));
    }

    sub enter_code {
        my ($self, $body) = @_;
        CgOp::prog(
            CgOp::share_lex($self->var),
            CgOp::share_lex($self->var . "::"),
            ($self->stub ? () :
                ($body->mainline ?
                    CgOp::share_lex($self->bodyvar) :
                    CgOp::clone_lex($self->bodyvar))));
    }

    sub write   {
        my ($self, $body) = @_;
        return unless $self->body;
        $self->body->outer($body);
        $self->body->write;
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
        my ($self) = @_;
        my @r;
        if (!grep { $_->isa('Decl::Super') } $self->body->do->local_decls) {
            push @r, CgOp::sink(CgOp::methodcall(CgOp::letvar("how"),
                    "add-super", CgOp::scopedlex($self->defsuper)));
        }
        @r, CgOp::scopedlex($self->var,
                CgOp::methodcall(CgOp::letvar("how"), "create-protoobject"));
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
        if ($body->type ne 'class') {
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

# XXX this is yucky
{
    package Decl::PackageLink;
    use Moose;
    extends 'Decl';

    has name => (is => 'ro', isa => 'Str', required => 1);

    sub used_slots { $_[0]->name, 'Variable' }
    sub preinit_code {
        my ($self, $body) = @_;
        CgOp::proto_var($self->name, CgOp::letvar('pkg'));
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
        map { $_, 'Variable' } sort keys %{ $self->symbols };
    }

    sub preinit_code {
        my ($self, $body) = @_;
        CodeGen->know_module($self->unit);
        CgOp::let(CgOp::prog(
                CgOp::rawscall($self->unit . '.Initialize'),
                CgOp::getfield('lex',
                    CgOp::rawsget($self->unit . '.Environment'))),
            sub {
                my $lex = shift;
                CgOp::prog(map {
                    my @path = @{ $self->symbols->{$_} };
                    my $first = CgOp::cast('Variable',
                        CgOp::getindex(shift(@path), $lex));
                    for (@path) {
                        $first = CgOp::rawscall('Kernel.PackageLookup',
                            CgOp::fetch($first), CgOp::clr_string($_));
                    }

                    CgOp::prog(
                        CgOp::proto_var($_, CgOp::newrwscalar(CgOp::fetch(
                            CgOp::scopedlex('Any')))),
                        CgOp::bind(0, CgOp::scopedlex($_), $first));
                } sort keys %{ $self->symbols });
            });
    }

    sub enter_code {
        my ($self) = @_;
        CgOp::prog(map { CgOp::share_lex($_) } sort keys %{ $self->symbols });
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
