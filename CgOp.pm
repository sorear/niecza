use 5.010;
use strict;
use warnings;


# for transition only!
{
    package CgOp::NIL;
    use Moose;

    has ops => (isa => 'ArrayRef', is => 'ro');

    sub var_cg {
        my ($self, $cg) = @_;
        for (@{ $self->ops }) {
            if (blessed $_) {
                $_->var_cg($cg);
            } else {
                my ($c, @o) = @$_;
                $cg->$c(@o);
            }
        }
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::Ternary;
    use Moose;

    has check => (is => 'ro');
    has true  => (is => 'ro');
    has false => (is => 'ro');

    sub var_cg {
        my ($self, $cg) = @_;
        my $l1 = $cg->label;
        my $l2 = $cg->label;

        $self->check->var_cg($cg);
        $cg->ncgoto($l1);
        $self->true->var_cg($cg);
        $cg->goto($l2);
        $cg->labelhere($l1);
        $self->false->var_cg($cg);
        $cg->labelhere($l2);
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::While;
    use Moose;

    has check => (is => 'ro');
    has body  => (is => 'ro');
    has once  => (is => 'ro', isa => 'Bool');
    has until => (is => 'ro', isa => 'Bool');

    sub var_cg {
        my ($self, $cg) = @_;
        my $lagain = $cg->label;
        my $lcheck = $self->once ? 0 : $cg->label;

        $cg->goto($lcheck) unless $self->once;

        $cg->labelhere($lagain);
        $self->body->var_cg($cg);

        $cg->labelhere($lcheck) unless $self->once;
        $self->check->var_cg($cg);
        if ($self->until) {
            $cg->ncgoto($lagain);
        } else {
            $cg->cgoto($lagain);
        }
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

# just a bunch of smart constructors
{
    package CgOp;

    sub nil {
        CgOp::NIL->new(ops => [ @_ ]);
    }

    sub null {
        CgOp::NIL->new(ops => [[ push_null => $_[0] ]]);
    }

    sub prog {
        CgOp::NIL->new(ops => [ @_ ]);
    }

    sub wrap {
        CgOp::NIL->new(ops => [ $_[0], [ 'clr_wrap' ] ]);
    }

    sub sink {
        CgOp::NIL->new(ops => [ $_[0], [ 'drop' ] ]);
    }

    sub fetch {
        CgOp::NIL->new(ops => [ $_[0], [ 'fetch' ] ]);
    }

    sub how {
        CgOp::NIL->new(ops => [ $_[0], [ 'how' ] ]);
    }

    sub getfield {
        CgOp::NIL->new(ops => [ $_[1], [ 'clr_field_get', $_[0] ] ]);
    }

    sub cast {
        CgOp::NIL->new(ops => [ $_[1], [ 'cast', $_[0] ] ]);
    }

    sub newscalar {
        CgOp::NIL->new(ops => [ $_[0], [ 'newscalar' ] ]);
    }

    sub string_var {
        CgOp::NIL->new(ops => [ [ 'string_var', $_[0] ] ]);
    }

    sub double {
        CgOp::NIL->new(ops => [ [ 'clr_double', $_[0] ] ]);
    }

    sub unbox {
        CgOp::NIL->new(ops => [ $_[1], [ 'unbox', $_[0] ] ]);
    }

    sub box {
        CgOp::NIL->new(ops => [ $_[1], [ 'box', $_[0] ] ]);
    }

    sub bind {
        CgOp::NIL->new(ops => [ $_[1], $_[2], [ 'bind', $_[0] ] ]);
    }

    sub scopedlex {
        CgOp::NIL->new(ops => [[ scopelexget => $_[0] ]]);
    }

    sub subcall {
        my ($sub, @args) = @_;
        CgOp::NIL->new(ops => [ $sub, @args, [ 'call_sub', 1, scalar @args ] ]);
    }

    sub methodcall {
        my ($obj, $name, @args) = @_;
        CgOp::NIL->new(ops => [ $obj, [ 'dup_fetch' ], @args,
                [ 'call_method', 1, $name, scalar @args ] ]);
    }

    sub ternary {
        CgOp::Ternary->new(
            check => $_[0],
            true  => $_[1],
            false => $_[2]);
    }

    sub whileloop {
        CgOp::While->new(
            until => $_[0],
            once  => $_[1],
            check => $_[2],
            body  => $_[3]);
    }
}

1;
