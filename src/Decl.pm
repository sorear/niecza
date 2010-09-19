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
