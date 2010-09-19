use strict;
use warnings;
use 5.010;

use CgOp;

{
    package Decl;
    use Moose;

    has zyg => (is => 'ro', isa => 'ArrayRef', default => sub { [] });

    sub dyn_name { $_[1] =~ /^.?[?*]/ }

    sub used_slots   { () }
    sub preinit_code { CgOp::noop }
    sub enter_code   { CgOp::noop }
    sub needs_protopad {0}

    sub outer_decls  {}
    sub bodies       {}

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
        [$_[0]->var, 'Variable', $_[1] ? 3 :
            ($_[0]->dyn_name($_[0]->var) ? 0 : 4)];
    }

    sub preinit_code {
        my ($self, $body) = @_;

        $body->needs_protovars ?
            CgOp::prog(
                CgOp::protosub($self->code),
                CgOp::proto_var($self->var,
                    $body->mainline ? CgOp::sub_obj($self->code)
                                    : CgOp::sub_var($self->code))) :
            CgOp::protosub($self->code);
    }

    sub enter_code {
        my ($self, $body) = @_;
        $body->mainline ? CgOp::noop :
            CgOp::scopedlex($self->var, CgOp::sub_var($self->code));
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
    has noinit   => (isa => 'Bool', is => 'ro', default => 0);

    sub dynamic {
        $_[0]->slot =~ /^.?[?*]/;
    }

    sub used_slots {
        [$_[0]->slot, 'Variable', $_[0]->dynamic ? 0 : $_[1] ? 1 : 4];
    }

    sub preinit_code {
        my ($self, $body) = @_;

        return CgOp::noop if !$body->needs_protovars;

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

        # conveniently, uncloned blocks cannot have interesting signatures
        # XXX std bug
        ($body->mainline && !$self->dynamic || $self->noinit) ? CgOp::noop :
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

        CgOp::bset(($body->lookup_var($self->name, @{ $self->path }))[1],
            CgOp::newboundvar(1, 0, CgOp::scopedlex($self->slot)))
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

    sub used_slots {
        [ $_[0]->slot . '!b', 'BValue', 1 ],
        [ $_[0]->slot, 'Variable', 5 ]
    }

    sub preinit_code {
        my ($self, $body) = @_;
        my ($st, $cg) = $body->lookup_var($self->name, @{ $self->path });
        Carp::confess("bad use of OurAlias") if $st;
        CgOp::proto_var($self->slot . '!b', $cg);
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
        $_[0]->slot ? [$_[0]->slot, 'Variable', 4] : ();
    }

    sub outer_decls {
        my $self = shift;
        Decl::SimpleVar->new(slot => $self->backing, list => $self->list);
    }

    sub preinit_code {
        my ($self, $body) = @_;
        ($self->slot && $body->needs_protovars) ?
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

    sub needs_protopad { 1 }
    sub preinit_code {
        my ($self, $body) = @_;

        $::SETTING_RESUME = $body->scopetree;
        my $n = $self->unitname;

        CgOp::rawsset($n . '.Environment', CgOp::letvar('protopad'));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Decl::HasMethod;
    use Moose;
    extends 'Decl';

    has name => (is => 'ro', isa => 'Str', required => 1);
    has var  => (is => 'ro', isa => 'Str', required => 1);

    sub needs_protopad { 1 }
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

    sub needs_protopad { 1 }
    sub preinit_code {
        my ($self, $body) = @_;
        CgOp::sink(
            CgOp::methodcall(CgOp::letvar("how"), "add-multiregex",
                CgOp::string_var($self->name), CgOp::scopedlex($self->var)));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

# XXX CHEAP HACK ALERT
{
    package Decl::VarAlias;
    use Moose;
    extends 'Decl';

    has oname => (is => 'ro', isa => 'Str', required => 1);
    has nname => (is => 'ro', isa => 'Str', required => 1);

    sub used_slots { [ $_[0]->nname, 'Variable',
            $_[0]->dyn_name($_[0]->nname) ? 0 : 4 ] }

    sub preinit_code {
        my ($self, $body) = @_;
        return CgOp::noop unless $body->needs_protovars;
        CgOp::proto_var($self->nname, CgOp::scopedlex($self->oname));
    }

    sub enter_code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->nname, CgOp::scopedlex($self->oname));
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

    # 2?  yeah we're going into magic number land.  that means use a hint.
    sub used_slots { [ $_[0]->name, 'Variable', 2 ] }
    sub preinit_code {
        my ($self, $body) = @_;
        CgOp::proto_var($self->name, $self->value);
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
        my $scope = CompilerDriver::metadata_for($self->unit)->{setting};

        CodeGen->know_module($self->unit);
        CgOp::prog(
            CgOp::rawscall($self->unit . '.Initialize'),
            map {
                my ($head, @path) = @{ $self->symbols->{$_} };
                CodeGen->know_sfield($scope->{$head}[2], $scope->{$head}[0]);
                my $first = CgOp::newscalar(CgOp::rawsget($scope->{$head}[2]));
                for (@path) {
                    $first = CgOp::bget(CgOp::rawscall('Kernel.PackageLookup',
                        CgOp::fetch($first), CgOp::clr_string($_)));
                }

                CgOp::proto_var($_, CgOp::newboundvar(0, 0, $first));
            } sort keys %{ $self->symbols });
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
