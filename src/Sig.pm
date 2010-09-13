use strict;
use warnings;
use utf8;
use 5.010;

{
    package Sig::Parameter;
    use Moose;

    has slot => (is => 'ro', isa => 'Maybe[Str]', required => 1);
    has slurpy => (is => 'ro', isa => 'Bool', default => 0);
    has optional => (is => 'ro', isa => 'Bool', default => 0);
    has default => (is => 'ro', isa => 'Maybe[Op]', default => undef);
    has positional => (is => 'ro', isa => 'Bool', default => 1);
    has readonly => (is => 'ro', isa => 'Bool', default => 0);
    has names => (is => 'ro', isa => 'ArrayRef[Str]', default => sub { [] });
    has name => (is => 'ro', isa => 'Str', required => 1);
    has list => (is => 'ro', isa => 'Bool', default => 0);
    has hash => (is => 'ro', isa => 'Bool', default => 0);
    has type => (is => 'ro', isa => 'Str', default => 'Any');

    sub local_decls {
        my ($self) = @_;
        my @r;
        push @r, Decl::SimpleVar->new(slot => $self->slot, hash => $self->hash,
                list => $self->list, noinit => 1)
            if defined $self->slot;
        push @r, $self->default->lift_decls if $self->default;
        @r;
    }

    sub slurpy_get {
        my ($self) = @_;
        CgOp::let(CgOp::rawnew('DynObject', CgOp::getfield('klass',
                    CgOp::cast('DynObject', CgOp::fetch(CgOp::scopedlex('List'))))), sub {
            my $do = shift;
            CgOp::prog(
                CgOp::rawcall($do, 'SetSlot', CgOp::clr_string('flat'),
                    CgOp::bool(1)),
                CgOp::rawcall($do, 'SetSlot', CgOp::clr_string('items'),
                    CgOp::rawnew('VarDeque')),
                CgOp::rawcall($do, 'SetSlot', CgOp::clr_string('rest'),
                    CgOp::rawscall('Kernel.SlurpyHelper',
                        CgOp::callframe, CgOp::letvar('!ix'))),
                CgOp::newscalar($do))});
    }

    sub _default_get {
        my ($self, $body) = @_;

        if (defined $self->default) {
            # the default code itself was generated in decls
            return $self->default->code($body);
        } elsif ($self->optional) {
            if ($self->type eq 'Any') {
                return CgOp::newscalar(CgOp::rawsget('Kernel.AnyP'));
            } else {
                return CgOp::scopedlex($self->type);
            }
        } else {
            return CgOp::prog(
                CgOp::die("No value in " . $body->name .
                    " available for parameter " . $self->name),
                CgOp::null('Variable'));
        }
    }

    sub _positional_get {
        my ($self, $fb) = @_;

        CgOp::ternary(
            CgOp::compare('>',
                CgOp::getfield('Length', CgOp::getfield('pos',
                        CgOp::callframe)), CgOp::letvar('!ix')),
            CgOp::letn('!ixp', CgOp::letvar('!ix'),
                CgOp::scopedlex('!ix', CgOp::arith('+', CgOp::letvar('!ixp'),
                        CgOp::int(1))),
                CgOp::pos(CgOp::letvar('!ixp'))),
            $fb);
    }

    sub _named_get {
        my ($self, $name, $fb) = @_;

        CgOp::letn('!v', CgOp::rawcall(CgOp::callframe, 'ExtractNamed',
                CgOp::clr_string($name)),
            CgOp::ternary(
                CgOp::compare('!=', CgOp::null('Variable'),
                    CgOp::letvar('!v')),
                CgOp::letvar('!v'),
                $fb));
    }

    sub single_get {
        my ($self, $body) = @_;

        my $cg = $self->_default_get($body);
        $cg = $self->_positional_get($cg) if $self->positional;
        for (reverse @{ $self->names }) {
            $cg = $self->_named_get($_, $cg);
        }
        $cg;
    }

    sub single_get_inline {
        my ($self, $body, $posr) = @_;

        if ($self->positional && @$posr) {
            return shift @$posr;
        } else {
            return $self->_default_get($body);
        }
    }

    sub binder {
        my ($self, $body) = @_;

        my $get = $self->slurpy ? $self->slurpy_get :
            $self->single_get($body);

        if (defined $self->slot) {
            return CgOp::scopedlex($self->slot,
                CgOp::newboundvar($self->readonly, $self->list, $get));
        } else {
            return CgOp::sink($get);
        }
    }

    sub bind_inline {
        my ($self, $body, $posr) = @_;

        my $get = $self->slurpy ? $self->slurpy_get_inline($posr) :
            $self->single_get_inline($body, $posr);

        if (defined $self->slot) {
            return CgOp::scopedlex($self->slot,
                CgOp::newboundvar($self->readonly, $self->list, $get));
        } else {
            return CgOp::sink($get);
        }
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Sig;
    use Moose;

    has params => (isa => 'ArrayRef[Sig::Parameter]', is => 'ro', required => 1);

    sub for_method {
        my $self = shift;
        my $sp = Sig::Parameter->new(slot => 'self', name => 'self');
        Sig->new(params => [ $sp, @{ $self->params } ]);
    }

    sub for_regex {
        my ($self) = @_;
        my $sp = Sig::Parameter->new(slot => '$¢', name => '$¢');
        Sig->new(params => [ $sp, @{ $self->params } ]);
    }

    sub simple {
        my ($class, @names) = @_;
        Sig->new(params => [map { Sig::Parameter->new(slot => $_, name => $_)
            } @names]);
    }

    sub local_decls {
        my $self = shift;
        map { $_->local_decls } @{ $self->params };
    }

    sub binder {
        my ($self, $body) = @_;

        my @p;
        for (@{ $self->params }) {
            push @p, $_->binder($body);
        }
        CgOp::letn('!ix', CgOp::int(0), CgOp::prog(@p));
    }

    sub bind_inline {
        my ($self, $body, @pos) = @_;

        my @p;
        for (@{ $self->params }) {
            push @p, $_->bind_inline($body, \@pos);
        }

        CgOp::prog(@p);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
