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
                if ($cg->unreach && $c ne 'labelhere') {
                    next;
                }
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
    use Scalar::Util 'blessed';

    sub nil {
        CgOp::NIL->new(ops => [ @_ ]);
    }

    sub noop {
        CgOp::NIL->new(ops => []);
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

    sub unwrap {
        cast($_[0], getfield('val', cast('CLRImportObject', $_[1])));
    }

    sub sink {
        CgOp::NIL->new(ops => [ $_[0], [ 'drop' ] ]);
    }

    sub fetch {
        CgOp::NIL->new(ops => [ $_[0], [ 'fetch' ] ]);
    }

    sub how {
        rawcall($_[0], "HOW");
    }

    sub getfield {
        CgOp::NIL->new(ops => [ $_[1], [ 'clr_field_get', $_[0] ] ]);
    }

    sub setfield {
        CgOp::NIL->new(ops => [ $_[1], $_[2], [ 'clr_field_set', $_[0] ] ]);
    }

    sub getindex {
        CgOp::NIL->new(ops => [ $_[1], (blessed($_[0]) ? $_[0] : ()),
                [ 'clr_index_get', (blessed($_[0]) ? () : $_[0])]]);
    }

    sub setindex {
        CgOp::NIL->new(ops => [ $_[1], (blessed($_[0]) ? $_[0] : ()),
                $_[2], [ 'clr_index_set', (blessed($_[0]) ? () : $_[0])]]);
    }

    sub getattr {
        fetch(varattr($_[0], $_[1]));
    }

    sub varattr {
        CgOp::NIL->new(ops => [ $_[1], [ 'attr_var', $_[0] ] ]);
    }

    sub cast {
        CgOp::NIL->new(ops => [ $_[1], [ 'cast', $_[0] ] ]);
    }

    sub newscalar {
        rawscall('Kernel.NewROScalar', $_[0]);
    }

    sub newrwscalar {
        rawscall('Kernel.NewRWScalar', $_[0]);
    }

    sub string_var {
        box('Str', clr_string($_[0]));
    }

    sub double {
        CgOp::NIL->new(ops => [ [ 'clr_double', $_[0] ] ]);
    }

    sub int {
        CgOp::NIL->new(ops => [ [ 'clr_int', $_[0] ] ]);
    }

    sub unbox {
        cast($_[0], rawscall('Kernel.UnboxAny', $_[1]));
    }

    sub box {
        rawscall('Kernel.BoxAny', $_[1], fetch(scopedlex($_[0])));
    }

    sub bind {
        CgOp::NIL->new(ops => [ $_[1], $_[2], [ 'bind', $_[0] ] ]);
    }

    sub assign {
        CgOp::NIL->new(ops => [ $_[0], $_[1], [ 'assign' ] ]);
    }

    sub compare {
        CgOp::NIL->new(ops => [$_[1], $_[2], [ 'clr_compare', $_[0] ]]);
    }

    sub arith {
        CgOp::NIL->new(ops => [$_[1], $_[2], [ 'clr_arith', $_[0] ]]);
    }

    sub scopedlex {
        my $n = shift;
        CgOp::NIL->new(ops => [ @_, [ scopelex => $n, scalar @_ ]]);
    }

    sub lexput {
        CgOp::NIL->new(ops => [ $_[2], [ lexput => $_[0], $_[1] ]]);
    }

    sub lexget {
        CgOp::NIL->new(ops => [[ lexget => $_[0], $_[1] ]]);
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

    sub callframe {
        CgOp::NIL->new(ops => [[ 'callframe' ]]);
    }

    sub aux {
        CgOp::NIL->new(ops => [[ 'peek_aux', $_[0] ]]);
    }

    sub clr_string {
        CgOp::NIL->new(ops => [[ 'clr_string', $_[0] ]]);
    }

    sub lextypes {
        CgOp::NIL->new(ops => [[ 'lextypes', @_ ]]);
    }

    sub share_lex {
        CgOp::NIL->new(ops => [[ 'share_lex', $_[0] ]]);
    }

    sub copy_lex {
        CgOp::NIL->new(ops => [[ 'copy_lex', $_[0] ]]);
    }

    sub clone_lex {
        CgOp::NIL->new(ops => [[ 'clone_lex', $_[0] ]]);
    }

    sub proto_var {
        CgOp::NIL->new(ops => [ $_[1], [ 'proto_var', $_[0] ]]);
    }

    sub return {
        $_[0] ?
            CgOp::NIL->new(ops => [ $_[0], [ 'return', 1 ] ]) :
            CgOp::NIL->new(ops => [[return => 0]]);
    }

    sub rawscall {
        my ($name, @args) = @_;
        CgOp::NIL->new(ops => [ @args, [ 'clr_call_direct', $name, scalar @args ] ]);
    }

    sub rawcall {
        my ($inv, $name, @args) = @_;
        CgOp::NIL->new(ops => [ $inv, @args, [ 'clr_call_virt', $name, scalar @args ] ]);
    }

    sub rawsget {
        CgOp::NIL->new(ops => [[ 'clr_sfield_get', $_[0] ]]);
    }

    sub rawnew {
        my ($name, @args) = @_;
        CgOp::NIL->new(ops => [ @args, [ 'clr_new', $name, scalar @args ] ]);
    }

    sub protosub {
        my ($body, @extra) = @_;
        CgOp::NIL->new(ops => [ [ 'open_protopad', $body ], @extra,
                $body->preinit_code, [ 'close_sub', $body->code ] ]);
    }

    sub new_aux {
        CgOp::NIL->new(ops => [[ 'new_aux', $_[0], $_[1] ]]);
    }

    sub with_aux {
        my ($name, $value, @stuff) = @_;
        CgOp::NIL->new(ops => [ $value, [ 'push_aux', $name ], @stuff,
                [ 'pop_aux', $name ], [ 'drop' ] ]);
    }

    sub pos {
        CgOp::NIL->new(ops => [[ 'pos', $_[0] ]]);
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
