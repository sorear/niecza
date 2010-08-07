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
    has default => (is => 'ro', isa => 'Maybe[Body]', default => undef);
    has positional => (is => 'ro', isa => 'Bool', default => 1);
    has readonly => (is => 'ro', isa => 'Bool', default => 0);
    has names => (is => 'ro', isa => 'ArrayRef[Str]', default => sub { [] });
    has name => (is => 'ro', isa => 'Str', required => 1);
    has list => (is => 'ro', isa => 'Bool', default => 0);
    has zeroinit => (is => 'ro', isa => 'Bool', default => 0);
    has type => (is => 'ro', isa => 'Str', default => 'Any');

    sub local_decls {
        my ($self) = @_;
        if (defined $self->slot) {
            return (Decl::SimpleVar->new(slot => $self->slot,
                    list => $self->list, zeroinit => $self->zeroinit));
        } else {
            return ();
        }
    }

    sub slurpy_get {
        my ($self) = @_;
        CgOp::let(CgOp::rawnew('DynObject', CgOp::getfield('klass',
                    CgOp::cast('DynObject', CgOp::fetch(CgOp::scopedlex('List'))))), sub {
            my $do = shift;
            CgOp::prog(
                CgOp::setindex('flat', CgOp::getfield('slots', $do),
                    CgOp::box('Bool', CgOp::bool(1))),
                CgOp::setindex('items', CgOp::getfield('slots', $do),
                    CgOp::box('LLArray', CgOp::rawnew('List<Variable>'))),
                CgOp::setindex('rest', CgOp::getfield('slots', $do),
                    CgOp::box('LLArray',
                        CgOp::rawscall('Kernel.SlurpyHelper',
                            CgOp::callframe, CgOp::letvar('!ix')))),
                CgOp::newscalar($do))});
    }

    sub _default_get {
        my ($self, $subname) = @_;

        if (defined $self->default) {
            # the default code itself was generated in decls
            return CgOp::prog(
                CgOp::clone_lex($self->default_sym),
                CgOp::subcall(CgOp::fetch(
                        CgOp::scopedlex($self->default_sym))));
        } elsif ($self->optional) {
            if ($self->type eq 'Any') {
                return CgOp::newscalar(CgOp::rawsget('Kernel.AnyP'));
            } else {
                return CgOp::scopedlex($self->type);
            }
        } else {
            return CgOp::prog(
                CgOp::die("No value in $subname available for parameter " .
                    $self->name),
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
        # TODO: implement named parameters

        $fb;
    }

    sub single_get {
        my ($self, $subname) = @_;

        my $cg = $self->_default_get($subname);
        $cg = $self->_positional_get($cg) if $self->positional;
        for (reverse @{ $self->names }) {
            $cg = $self->_named_get($_, $cg);
        }
        $cg;
    }

    sub binder {
        my ($self, $subname) = @_;

        my $get = $self->slurpy ? $self->slurpy_get :
            $self->single_get($subname);

        if (defined $self->slot) {
            return CgOp::bind($self->readonly, CgOp::scopedlex($self->slot),
                    $get);
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
        my $self = shift;
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
        my ($self, $subname) = @_;

        my @p;
        for (@{ $self->params }) {
            push @p, $_->binder($subname);
        }
        CgOp::letn('!ix', CgOp::int(0), CgOp::prog(@p));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
