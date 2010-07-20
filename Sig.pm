use 5.010;
use MooseX::Declare;

class Sig::Target {
    has slot => (is => 'ro', isa => 'Maybe[Str]', required => 1);
    has list => (is => 'ro', isa => 'Bool', default => 0);

    method used_slots () {
        if ($self->slot) { [ $self->slot, $self->list ] } else { () }
    }

    method binder ($get) {
        if ($self->slot) {
            # TODO: implement ro, etc
            CgOp::bind(0, CgOp::scopedlex($self->slot), $get);
        } else {
            CgOp::noop;
        }
    }
}

class Sig::Parameter {
    has target => (is => 'ro', isa => 'Sig::Target', required => 1,
        handles => [ 'used_slots' ]);
    has slurpy => (is => 'ro', isa => 'Bool', default => 0);

    method binder ($ixp) {
        if ($self->slurpy) {
            $self->target->binder(
                CgOp::let(CgOp::rawnew('DynObject', CgOp::getfield('klass',
                            CgOp::cast('DynObject', CgOp::fetch(CgOp::scopedlex('List'))))),
                    'DynObject', sub {
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
}

class Sig {
    has params => (isa => 'ArrayRef[Sig::Parameter]', is => 'ro', required => 1);

    method for_method () {
        my $sp = Sig::Parameter->new(target =>
            Sig::Target->new(slot => 'self'));
        Sig->new(params => [ $sp, @{ $self->params } ]);
    }

    method used_slots () {
        map { $_->used_slots } @{ $self->params };
    }

    method binder () {
        # TODO: Error checking.
        my $ix = 0;
        my @p;
        for (@{ $self->params }) {
            push @p, $_->binder(\$ix);
        }
        CgOp::prog(@p);
    }
}

1;
