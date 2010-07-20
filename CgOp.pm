use 5.010;
use strict;
use warnings;


{
    package CgOp;
    use Moose;

    has zyg => (isa => 'ArrayRef', is => 'ro', default => sub { [] });

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::Seq;
    use Moose;
    extends 'CgOp';

    sub var_cg {
        my ($self, $cg) = @_;
        for (@{ $self->zyg }) {
            $_->var_cg($cg);
        }
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::Primitive;
    use Moose;
    extends 'CgOp';

    has op  => (isa => 'ArrayRef', is => 'ro', required => 1);

    sub var_cg {
        my ($self, $cg) = @_;
        for (@{ $self->zyg }) {
            $_->var_cg($cg);
        }
        my ($c, @o) = @{ $self->op };
        if ($cg->unreach && $c ne 'labelhere') {
            return;
        }
        $cg->$c(@o);
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::Ternary;
    use Moose;
    extends 'CgOp';

    sub var_cg {
        my ($self, $cg) = @_;
        my ($check, $true, $false) = @{ $self->zyg };
        my $l1 = $cg->label;
        my $l2 = $cg->label;

        $check->var_cg($cg);
        $cg->ncgoto($l1);
        $true->var_cg($cg);
        $cg->goto($l2);
        $cg->labelhere($l1);
        $false->var_cg($cg);
        $cg->labelhere($l2);
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::While;
    use Moose;
    extends 'CgOp';

    has once  => (is => 'ro', isa => 'Bool');
    has until => (is => 'ro', isa => 'Bool');

    sub var_cg {
        my ($self, $cg) = @_;
        my ($check, $body) = @{ $self->zyg };
        my $lagain = $cg->label;
        my $lcheck = $self->once ? 0 : $cg->label;

        $cg->goto($lcheck) unless $self->once;

        $cg->labelhere($lagain);
        $body->var_cg($cg);

        $cg->labelhere($lcheck) unless $self->once;
        $check->var_cg($cg);
        if ($self->until) {
            $cg->ncgoto($lagain);
        } else {
            $cg->cgoto($lagain);
        }
    }

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp::Let;
    use Moose;
    extends 'CgOp';

    has var  => (is => 'ro', isa => 'Str', required => 1);
    has type => (is => 'ro', isa => 'Str', required => 1);

    sub var_cg {
        my ($self, $cg) = @_;

        $cg->lextypes($self->var, $self->type);
        $self->zyg->[0]->var_cg($cg);
        $cg->rawlexput($self->var, 0);
        $self->zyg->[1]->var_cg($cg);
        $cg->push_null($self->type);
        $cg->rawlexput($self->var, 0);
    }

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
        CgOp::Primitive->new(op => [ push_null => $_[0] ]);
    }

    sub prog {
        CgOp::Seq->new(zyg => [ @_ ]);
    }

    sub wrap {
        newscalar(rawnew('CLRImportObject', $_[0]));
    }

    sub unwrap {
        cast($_[0], getfield('val', cast('CLRImportObject', $_[1])));
    }

    sub sink {
        CgOp::Primitive->new(op => ['drop'], zyg => [ $_[0] ]);
    }

    sub fetch {
        rawscall("Kernel.Fetch", $_[0]);
    }

    sub how {
        rawcall($_[0], "HOW");
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
            op  => [ 'clr_index_get', (blessed($_[0])) ? () : $_[0] ],
            zyg => [ $_[1], (blessed($_[0]) ? $_[0] : ()) ]);
    }

    sub setindex {
        CgOp::Primitive->new(
            op  => [ 'clr_index_set', (blessed($_[0])) ? () : $_[0] ],
            zyg => [ $_[1], (blessed($_[0]) ? $_[0] : ()), $_[2] ]);
    }

    sub getattr {
        fetch(varattr($_[0], $_[1]));
    }

    sub varattr {
        CgOp::Primitive->new(op => [ 'attr_var', $_[0] ], zyg => [ $_[1] ]);
    }

    sub cast {
        CgOp::Primitive->new(op => [ 'cast', $_[0] ], zyg => [ $_[1] ]);
    }

    sub newscalar {
        rawscall('Kernel.NewROScalar', $_[0]);
    }

    sub newrwscalar {
        rawscall('Kernel.NewRWScalar', $_[0]);
    }

    sub newrwlistvar {
        rawscall('Kernel.NewRWListVar', $_[0]);
    }

    sub string_var {
        box('Str', clr_string($_[0]));
    }

    sub double {
        CgOp::Primitive->new(op => [ 'clr_double', $_[0] ]);
    }

    sub int {
        CgOp::Primitive->new(op => [ 'clr_int', $_[0] ]);
    }

    sub bool {
        CgOp::Primitive->new(op => [ 'clr_bool', $_[0] ]);
    }

    sub unbox {
        cast($_[0], rawscall('Kernel.UnboxAny', $_[1]));
    }

    sub box {
        rawscall('Kernel.BoxAny', $_[1], fetch(scopedlex($_[0])));
    }

    sub bind {
        rawscall('Kernel.Bind', $_[1], getfield('lv', $_[2]),
            bool($_[0]), bool(0));
    }

    sub assign {
        rawscall('Kernel.Assign', getfield('lv', $_[0]),
            getfield('lv', $_[1]));
    }

    sub compare {
        CgOp::Primitive->new(op => [ 'clr_compare', $_[0] ],
            zyg => [ $_[1], $_[2] ]);
    }

    sub arith {
        CgOp::Primitive->new(op => [ 'clr_arith', $_[0] ],
            zyg => [ $_[1], $_[2] ]);
    }

    sub scopedlex {
        my $n = shift;
        CgOp::Primitive->new(op => [ scopelex => $n, scalar @_ ],
            zyg => [ @_ ]);
    }

    sub lexput {
        CgOp::Primitive->new(op => [ lexput => $_[0], $_[1] ],
            zyg => [ $_[2] ]);
    }

    sub lexget {
        CgOp::Primitive->new(op => [ lexget => $_[0], $_[1] ]);
    }

    sub subcall {
        my ($sub, @args) = @_;
        CgOp::Primitive->new(op => [ 'call_sub', 1, scalar @args ],
            zyg => [ $sub, @args ]);
    }

    sub methodcall {
        my ($obj, $name, @args) = @_;
        let($obj, 'Variable', sub {
            CgOp::Primitive->new(op => [ 'call_method', 1, $name, scalar @args ],
                zyg => [ fetch($_[0]), $_[0], @args ])});
    }

    sub callframe {
        CgOp::Primitive->new(op => [ 'callframe' ]);
    }

    sub aux {
        CgOp::Primitive->new(op => [ 'peek_aux', $_[0] ]);
    }

    sub clr_string {
        CgOp::Primitive->new(op => [ 'clr_string', $_[0] ]);
    }

    # XXX This being treated as a function is completely wrong.
    sub lextypes {
        CgOp::Primitive->new(op => [ 'lextypes', @_ ]);
    }

    sub new_aux {
        CgOp::Primitive->new(op => [ 'new_aux', $_[0], $_[1] ]);
    }

    sub share_lex {
        prog(
          lextypes($_[0], 'Variable'),
          lexput(0, $_[0], protolget($_[0])));
    }

    # this will need changing once @vars are implemented... or maybe something
    # entirely different, I think cloning at all may be wrong
    sub copy_lex {
        prog(
          lextypes($_[0], 'Variable'),
          lexput(0, $_[0], newrwscalar(fetch(protolget($_[0])))));
    }

    sub clone_lex {
        prog(
          lextypes($_[0], 'Variable'),
          lexput(0, $_[0], methodcall(protolget($_[0]), "clone",
            newscalar(callframe))));
    }

    sub proto_var {
        CgOp::Primitive->new(op => [ 'proto_var', $_[0] ], zyg => [ $_[1] ]);
    }

    sub protolget {
        CgOp::Primitive->new(op => [ 'protolget', $_[0] ]);
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

    sub rawsget {
        CgOp::Primitive->new(op => [ 'clr_sfield_get', $_[0] ]);
    }

    sub rawnew {
        my ($name, @args) = @_;
        CgOp::Primitive->new(op => [ 'clr_new', $name, scalar @args ],
            zyg => \@args);
    }

    # the aux stacks probably ought to die.
    sub protosub {
        my ($body, @extra) = @_;
        prog(
            CgOp::Primitive->new(op => [ 'open_protopad', $body ]),
            $body->preinit_code,
            CgOp::Primitive->new(op => [ 'close_sub', $body->code ]));
    }

    sub with_aux {
        my ($name, $value, @stuff) = @_;
        prog(
            CgOp::Primitive->new(op => [ 'push_aux', $name ],
                zyg => [ $value ]),
            @stuff,
            sink(CgOp::Primitive->new(op => [ 'pop_aux', $name ])));
    }

    sub pos {
        CgOp::Primitive->new(op => [ 'pos', $_[0] ]);
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
        my ($head, $type, $bodyf) = @_;
        my $v = 'let!' . ($nextlet++);
        my $body = $bodyf->(CgOp::Primitive->new(
                op => [ rawlexget => $v, 0 ]));

        CgOp::Let->new(var => $v, type => $type, zyg => [ $head, $body ]);
    }
}

1;
