use strict;
use warnings;
use 5.010;

{
    package Sig::Target;
    use Moose;

    has slot => (is => 'ro', isa => 'Maybe[Str]', required => 1);
    has list => (is => 'ro', isa => 'Bool', default => 0);
    has zeroinit => (is => 'ro', isa => 'Bool', default => 0);

    sub local_decls {
        my $self = shift;
        if ($self->slot) {
            Decl::SimpleVar->new(slot => $self->slot, list => $self->list,
                zeroinit => $self->zeroinit)
        } else {
            ()
        }
    }

    sub binder {
        my ($self, $get) = @_;
        if ($self->slot) {
            # TODO: implement ro, etc
            CgOp::bind(0, CgOp::scopedlex($self->slot), $get);
        } else {
            CgOp::noop;
        }
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Sig::Parameter;
    use Moose;

    has target => (is => 'ro', isa => 'Sig::Target', required => 1,
        handles => [ 'local_decls' ]);
    has slurpy => (is => 'ro', isa => 'Bool', default => 0);

    sub binder {
        my ($self, $ixp) = @_;

        if ($self->slurpy) {
            $self->target->binder(
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
                                    CgOp::int($$ixp)))),
                        CgOp::newscalar($do))}));
        } else {
            $self->target->binder(CgOp::pos($$ixp++));
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
        my $sp = Sig::Parameter->new(target =>
            Sig::Target->new(slot => 'self'));
        Sig->new(params => [ $sp, @{ $self->params } ]);
    }

    sub local_decls {
        my $self = shift;
        map { $_->local_decls } @{ $self->params };
    }

    sub binder {
        my ($self) = @_;

        # TODO: Error checking.
        my $ix = 0;
        my @p;
        for (@{ $self->params }) {
            push @p, $_->binder(\$ix);
        }
        CgOp::prog(@p);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
