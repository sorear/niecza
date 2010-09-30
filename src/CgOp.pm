use 5.010;
use strict;
use warnings;

# The invariant of cps_convert is that, in the output, a primitive which does
# a CPS call cannot appear inside the argument list of another primitive.

{
    package CgOp;
    use Moose;

    has zyg => (isa => 'ArrayRef[CgOp]', is => 'ro', default => sub { [] });

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::Annotation;
    use Moose;
    extends 'CgOp';

    has file => (isa => 'Str', is => 'ro', required => 1);
    has line => (isa => 'Int', is => 'ro', required => 1);

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::Let;
    use Moose;
    extends 'CgOp';

    has name => (isa => 'Str', is => 'ro');

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::Span;
    use Moose;
    extends 'CgOp';

    has lstart => (isa => 'Str', is => 'ro');
    has lend   => (isa => 'Str', is => 'ro');

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::Seq;
    use Moose;
    extends 'CgOp';

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::Primitive;
    use Moose;
    extends 'CgOp';

    has op  => (isa => 'ArrayRef', is => 'ro', required => 1);
    has is_cps_call => (isa => 'Bool', is => 'ro', default => 0);
    has constant => (isa => 'Bool', is => 'ro', default => 0);

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::Ternary;
    use Moose;
    extends 'CgOp';

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::While;
    use Moose;
    extends 'CgOp';

    has once  => (is => 'ro', isa => 'Bool');
    has until => (is => 'ro', isa => 'Bool');

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

# just a bunch of smart constructors
{
    package CgOp;
    use Scalar::Util 'blessed';

    sub noop {
        CgOp::Seq->new;
    }

    sub null {
        CgOp::Primitive->new(op => [ push_null => $_[0] ], constant => 1);
    }

    sub prog {
        CgOp::Seq->new(zyg => [ @_ ]);
    }

    sub span {
        my ($ls,$le,@r) = @_;
        CgOp::Span->new(lstart => $ls, lend => $le, zyg => [prog(@r)]);
    }

    sub ehspan {
        CgOp::Primitive->new(op => [ ehspan => @_ ]);
    }

    sub sink {
        CgOp::Primitive->new(op => ['drop'], zyg => [ $_[0] ]);
    }

    sub rnull {
        prog($_[0], null('Variable'));
    }

    sub fetch {
        rawcall($_[0], 'Fetch');
    }

    sub how {
        rawccall($_[0], "HOW");
    }

    sub getfield {
        CgOp::Primitive->new(op => [ 'clr_field_get', $_[0] ],
            zyg => [ $_[1] ]);
    }

    sub setfield {
        CgOp::Primitive->new(op => [ 'clr_field_set', $_[0] ],
            zyg => [ $_[1], $_[2] ]);
    }

    sub getindex {
        CgOp::Primitive->new(
            op  => [ 'clr_index_get', blessed($_[0]) ? undef : $_[0] ],
            zyg => [ $_[1], (blessed($_[0]) ? $_[0] : ()) ]);
    }

    sub setindex {
        CgOp::Primitive->new(
            op  => [ 'clr_index_set', blessed($_[0]) ? undef : $_[0] ],
            zyg => [ $_[1], (blessed($_[0]) ? $_[0] : ()), $_[2] ]);
    }

    sub getattr {
        fetch(varattr($_[0], $_[1]));
    }

    sub getslot {
        rawcall(cast('DynObject', $_[1]), 'GetSlot', $_[0]);
    }

    sub setslot {
        rawcall(cast('DynObject', $_[1]), 'SetSlot', $_[0], $_[2]);
    }

    sub varattr {
        CgOp::Primitive->new(op => [ 'attr_var', $_[0] ], zyg => [ $_[1] ],
            is_cps_call => 1);
    }

    sub cast {
        CgOp::Primitive->new(op => [ 'cast', $_[0] ], zyg => [ $_[1] ]);
    }

    sub const {
        CgOp::Primitive->new(op => [ 'const' ], zyg => [ $_[0] ], constant => 1);
    }

    sub newscalar {
        rawscall('Kernel.NewROScalar', $_[0]);
    }

    sub newblankrwscalar {
        rawscall('Kernel.NewRWScalar', rawsget('Kernel.AnyP'));
    }

    sub newrwscalar {
        rawscall('Kernel.NewRWScalar', $_[0]);
    }

    sub newrwlistvar {
        rawscall('Kernel.NewRWListVar', $_[0]);
    }

    sub newblanklist {
        newrwlistvar(ternary(
                compare('==', rawsget('Kernel.ArrayP'), null('IP6')),
                null('IP6'),
                fetch(methodcall(newscalar(rawsget('Kernel.ArrayP')), 'new'))));
    }

    sub newblankhash {
        newrwlistvar(
                fetch(methodcall(newscalar(rawsget('Kernel.HashP')), 'new')));
    }

    sub string_var {
        box('Str', clr_string($_[0]));
    }

    sub double {
        CgOp::Primitive->new(op => [ 'clr_double', $_[0] ], constant => 1);
    }

    sub labelid {
        CgOp::Primitive->new(op => [ 'labelid', $_[0] ], zyg => [ ],
            constant => 1);
    }

    sub int {
        CgOp::Primitive->new(op => [ 'clr_int', $_[0] ], constant => 1);
    }

    sub bool {
        CgOp::Primitive->new(op => [ 'clr_bool', $_[0] ], constant => 1);
    }

    sub unbox {
        cast($_[0], rawscall('Kernel.UnboxAny', $_[1]));
    }

    sub box {
        rawscall('Kernel.BoxAny', $_[1],
            blessed($_[0]) ? $_[0] :
            ($_[0] eq 'Str') ? rawsget('Kernel.StrP') :
                fetch(scopedlex($_[0])));
    }

    sub bget { getfield('v', $_[0]) }
    sub bset { setfield('v', $_[0], $_[1]) }

    sub newboundvar {
        rawsccall('Kernel.NewBoundVar', bool($_[0] || $_[1]), bool($_[1]),
            $_[2]);
    }

    sub assign {
        rawsccall('Kernel.Assign', $_[0], $_[1]);
    }

    sub compare {
        CgOp::Primitive->new(op => [ 'clr_compare', $_[0] ],
            zyg => [ $_[1], $_[2] ]);
    }

    sub arith {
        CgOp::Primitive->new(op => [ 'clr_arith', $_[0] ],
            zyg => [ $_[1], $_[2] ]);
    }

    # Not a CgOp function, rewritten by the resolve_lex pass
    sub scopedlex {
        my $n = shift;
        CgOp::Primitive->new(op => [ scopelex => $n, scalar @_ ],
            zyg => [ @_ ]);
    }

    sub _process_arglist {
        my $ar = shift;
        my @sig;
        my $j = 0;
        for (my $i = 0; $i < @$ar; ) {
            if (blessed($ar->[$i])) {
                push @sig, '';
            } else {
                push @sig, $ar->[$i++];
            }
            $ar->[$j++] = $ar->[$i++];
        }
        $#$ar = $j - 1;
        @sig;
    }

    sub subcall {
        my ($sub, @args) = @_;
        my @sig = _process_arglist(\@args);
        CgOp::Primitive->new(op => [ 'call_sub', \@sig ],
            zyg => [ $sub, @args ], is_cps_call => 1);
    }

    sub methodcall {
        my ($obj, $name, @args) = @_;
        my @sig = _process_arglist(\@args);
        let($obj, sub {
            CgOp::Primitive->new(op => [ 'call_method', $name, ['', @sig] ],
                zyg => [ fetch($_[0]), $_[0], @args ], is_cps_call => 1)});
    }

    sub callframe {
        # for the life of the function, constant
        CgOp::Primitive->new(op => [ 'callframe' ], constant => 1);
    }

    sub rxframe { getfield('rx', callframe) }
    sub rxcall { rawcall(rxframe, @_) }
    sub pushcut { rxcall('PushCutGroup', clr_string($_[0])) }
    sub popcut { rxcall('PopCutGroup') }

    sub letvar {
        $_[1] ?
            CgOp::Primitive->new(op => [ 'poke_let', $_[0] ], zyg => [ $_[1] ]):
            CgOp::Primitive->new(op => [ 'peek_let', $_[0] ]);
    }

    sub clr_string {
        CgOp::Primitive->new(op => [ 'clr_string', $_[0] ], constant => 1);
    }

    sub char {
        CgOp::Primitive->new(op => [ 'clr_char', $_[0] ], constant => 1);
    }

    sub withtypes {
        if (blessed($_[0])) {
            prog(@_);
        } else {
            my $n = shift;
            my $t = shift;
            letn($n, null($t), withtypes(@_));
        }
    }

    sub return {
        $_[0] ?
            CgOp::Primitive->new(op => [ 'return', 1 ], zyg => [ $_[0] ]) :
            CgOp::Primitive->new(op => [ return => 0]);
    }

    sub rawscall {
        my ($name, @args) = @_;
        CgOp::Primitive->new(op => [ 'clr_call_direct', $name, scalar @args ],
            zyg => [ @args ]);
    }

    sub rawcall {
        my ($inv, $name, @args) = @_;
        CgOp::Primitive->new(op => [ 'clr_call_virt', $name, scalar @args ],
            zyg => [ $inv, @args ]);
    }

    sub rawsccall {
        my ($name, @args) = @_;
        CgOp::Primitive->new(op => [ 'clr_call_direct', $name, scalar @args ],
            zyg => [ @args ], is_cps_call => 1);
    }

    sub rawccall {
        my ($inv, $name, @args) = @_;
        CgOp::Primitive->new(op => [ 'clr_call_virt', $name, scalar @args ],
            zyg => [ $inv, @args ], is_cps_call => 1);
    }

    sub label {
        my ($name) = @_;
        CgOp::Primitive->new(op => [ 'labelhere', $name ],
            zyg => [ ], is_cps_call => 1);
    }

    sub goto {
        my ($name) = @_;
        CgOp::Primitive->new(op => [ 'goto', $name ],
            zyg => [ ], is_cps_call => 1);
    }

    sub cgoto {
        my ($name) = @_;
        CgOp::Primitive->new(op => [ 'cgoto', $name ],
            zyg => [ $_[1] ], is_cps_call => 1);
    }

    sub ncgoto {
        my ($name) = @_;
        CgOp::Primitive->new(op => [ 'ncgoto', $name ],
            zyg => [ $_[1] ], is_cps_call => 1);
    }

    sub rxpushb {
        my ($tag, $lbl) = @_;
        CgOp::Primitive->new(op => [ 'rxpushb', $tag, $lbl ],
            zyg => [ ], is_cps_call => 1);
    }

    sub rxbprim {
        my ($name, @args) = @_;
        CgOp::Primitive->new(op => [ 'rxbprim', $name, scalar @args ],
            zyg => [ @args ], is_cps_call => 1);
    }

    sub rawsget {
        Carp::confess "Undefined name in rawsget" unless defined $_[0];
        CgOp::Primitive->new(op => [ 'clr_sfield_get', $_[0] ]);
    }

    sub rawsset {
        Carp::confess "Undefined name in rawsset" unless defined $_[0];
        CgOp::Primitive->new(op => [ 'clr_sfield_set', $_[0] ],
            zyg => [ $_[1] ]);
    }

    sub rawnew {
        my ($name, @args) = @_;
        CgOp::Primitive->new(op => [ 'clr_new', $name, scalar @args ],
            zyg => \@args);
    }

    sub rawnewarr {
        my ($name, @args) = @_;
        CgOp::Primitive->new(op => [ 'clr_new_arr', $name, scalar @args ],
            zyg => \@args);
    }

    sub rawnewzarr {
        my ($name, $ni) = @_;
        CgOp::Primitive->new(op => [ 'clr_new_zarr', $name ], zyg => [ $ni ]);
    }

    sub ann {
        my ($file, $line, $stuff) = @_;
        CgOp::Annotation->new(file => $file, line => $line,
            zyg => [$stuff]);
    }

    sub die {
        my ($msg) = @_;
        if (blessed($msg)) {
            rawsccall('Kernel.SearchForHandler', &int(5), null('Frame'),
                &int(-1), null('String'), newscalar($msg));
        } else {
            rawsccall('Kernel.Die', clr_string($msg));
        }
    }

    sub letn {
        my (@stuff) = @_;
        if (blessed($stuff[0])) {
            @stuff;
        } else {
            if (!@stuff) {
                Carp::confess "Invalid letn protocol";
            }
            my ($name, $value) = splice @stuff, 0, 2;
            CgOp::Let->new(name => $name, zyg => [ $value, letn(@stuff) ]);
        }
    }

    sub pos {
        CgOp::Primitive->new(op => [ 'pos', blessed($_[0]) ? undef : $_[0] ],
            zyg => [blessed($_[0]) ? ($_[0]) : ()], constant => 1);
    }

    sub ternary {
        CgOp::Ternary->new(zyg => [ $_[0], $_[1], $_[2] ]);
    }

    sub whileloop {
        CgOp::While->new(
            until => $_[0],
            once  => $_[1],
            zyg => [ $_[2], $_[3] ]);
    }

    my $nextlet = 0;
    sub let {
        my ($head, $bodyf) = @_;
        my $v = ($nextlet++);
        letn($v, $head, $bodyf->(letvar($v)));
    }
}

1;
