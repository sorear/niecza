use 5.010;
use strict;
use warnings;
use utf8;

package CgOpToCLROp;

no warnings 'recursion';

use CgOp;

{
    package CLROp::Term;
    use Moose;

    has op       => (isa => 'ArrayRef', is => 'ro', required => 1);
    has type     => (isa => 'Str', is => 'ro', default => 'Void');
    has constant => (isa => 'Bool', is => 'ro', default => 0);
    has zyg      => (isa => 'ArrayRef[CLROp::Term]', is => 'ro', default => sub { [] });

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package CLROp::Value;
    use Moose;

    # all void
    has stmts => (isa => 'ArrayRef[CLROp::Term]', is => 'ro',
        default => sub { [] });
    # NOT void; if undef, use resultSlot
    has head => (isa => 'Maybe[CLROp::Term]', is => 'ro');
    has type => (isa => 'Str', is => 'ro', required => 1);

    sub anyhead {
        $_[0]->head // $_[0]->get_result;
    }

    sub get_result {
        CLROp::Term->new(op => ['result'], type => $_[0]->type);
    }

    sub stmts_result {
        my @v = @{ $_[0]->stmts };
        if ($_[0]->head) {
            push @v, CLROp::Term->new(op => ['set_result'], zyg => [$_[0]->head]);
        }
        @v;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

our %lettypes;
our $spill;

my %fixtype = ( callframe => 'Frame',
    clr_bool => 'System.Boolean', clr_string => 'System.String',
    clr_char => 'Char', clr_int => 'Int32', clr_double => 'Double',
    labelid => 'Int32', clr_compare => 'Boolean' );

$fixtype{$_} = 'Void' for (qw/ poke_let labelhere goto cgoto ncgoto ehspan
    rtpadput rtpadputi drop tail_call_sub clr_field_set clr_sfield_set
    clr_index_set rxbprim rxpushb return cpssync /);

$fixtype{$_} = 'Variable' for (qw/ pos call_sub call_method /);

my %_cps = map { $_ => 1 } qw/ call_method call_sub cgoto goto labelhere
    ncgoto return rxbprim cpssync /;
my %_const = map { $_ => 1 } qw/ callframe clr_bool clr_char clr_double clr_int
    clr_string const labelid pos push_null /;

sub type_infer {
    my ($cpsr, $op, @argtypes) = @_;
    my $head = $op->[0];

    if ($fixtype{$head}) { return $fixtype{$head} }

    given ($head) {
        when ("clr_call_direct") {
            my ($n, $cps, $type) = CLRTypes->info("cm", $op->[1]);
            $$cpsr = ($cps eq 'c');
            return $type;
        }
        when ("clr_call_virt") {
            my ($n, $cps, $type) = CLRTypes->info("cm", $argtypes[0], $op->[1]);
            $$cpsr = ($cps eq 'c');
            return $type;
        }
        when ("clr_field_get") {
            return (CLRTypes->info("f", $argtypes[0], $op->[1]))[2];
        }
        when ("clr_index_get") {
            return (CLRTypes->info("i", $argtypes[1], 'Item'))[2];
        }
        when ("clr_sfield_get") {
            return (CLRTypes->info("f", $op->[1]))[2];
        }
        when ("cast")           { return $op->[1] }
        when ("clr_arith")      { return $argtypes[0] }
        when ("clr_new")        { return $op->[1] }
        when ("clr_new_arr")    { return $op->[1] . "[]" }
        when ("clr_new_zarr")   { return $op->[1] . "[]" }
        when ("const")          { return $argtypes[0] }
        when ("hintget")        { return $op->[1] }
        when ("peek_let")       { return $lettypes{$op->[1]} }
        when ("push_null")      { return $op->[1] }
        when ("rtpadget")       { return $op->[1] }
        when ("rtpadgeti")      { return $op->[1] }
        default { die "No type inference for $head" }
    }
}

# C# has a lot of special cases for this.
sub _drop { CgOp->new(op => ['drop'], zyg => [$_[0]]) }
my %_dropnow = map {; $_ => 1 } qw/ push_null rtpadget rtpadgeti hint_get
    clr_sfield_get result pos peek_let /;
sub do_drop {
    my ($tgt) = @_;

    my @zyg = @{ $tgt->zyg };
    my $op = $tgt->op;

    given ($op->[0]) {
        when ('let') {
            $zyg[-1] = _drop($zyg[-1]);
            return cvt(CgOp->new(op => $op, zyg => [ @zyg ]));
        }
        when ('seq') {
            $zyg[-1] = _drop($zyg[-1]);
            return cvt(CgOp->new(op => $op, zyg => [ @zyg ]));
        }
        when ('ternary') {
            @zyg[1,2] = map { _drop($_) } @zyg[1,2];
            return cvt(CgOp->new(op => $op, zyg => [ @zyg ]));
        }
        when ('ann') {
            return cvt(CgOp->new(op => $op, zyg => [ _drop($zyg[0]) ]));
        }
        when ('span') {
            return cvt(CgOp->new(op => $op, zyg => [ _drop($zyg[0]) ]));
        }
        when ("while") { die "implausible use of drop on while" }
        default {
            my $v = cvt($tgt);
            my @zyg = @{ $v->stmts };
            my $h = $v->head;
            if ($h && $h->op->[0] eq 'clr_field_get') { $h = $h->zyg->[0] }
            if ($h && !$_dropnow{$h->op->[0]}) {
                push @zyg, CLROp::Term->new(op => ['drop'], zyg => [$h]);
            }
            return CLROp::Value->new(type => 'Void', stmts => \@zyg);
        }
    }
}

sub do_primitive {
    my ($ops, @zyg) = @_;
    my @args;
    my @prep;
    my @pop;

    while (@zyg) {
        my $z = shift @zyg;
        push @prep, @{ $z->stmts };

        my $nospill = 1;
        for (@zyg) {
            $nospill = 0 if @{ $_->stmts };
        }

        my $head = $z->anyhead;

        # we *might* be able to use the head as is, but not if that would
        # switch it with any effects
        if ($nospill || $head->constant) {
            push @args, $head;
        # otherwise, it has to be saved.  TODO try parking it in resultSlot
        } else {
            my $ln = 'spill' . ($spill++);
            push @prep, CLROp::Term->new(op => ['push_let', $ln],
                zyg => [ $head ]);
            push @args, CLROp::Term->new(op => ['peek_let', $ln],
                type => $z->type);
            unshift @pop, CLROp::Term->new(op => ['drop_let', $ln]);
        }
    }

    my $cps = $_cps{$ops->[0]};
    my $type = type_infer(\$cps, $ops, map { $_->type } @args);
    my $const = $_const{$ops->[0]};

    if (!defined $type) {
        say(YAML::XS::Dump($ops, map { $_->type } @args));
        die "Type inference returned undef";
    }

    if ($type eq 'Void') {
        my $nhead = CLROp::Term->new(op => $ops, type => $type,
            zyg => [ @args ]);

        return CLROp::Value->new(stmts => [ @prep, $nhead, @pop ],
            type => 'Void');
    } elsif ($cps) {
        my $nhead = CLROp::Term->new(op => $ops, type => 'Void',
            zyg => [ @args ]);

        return CLROp::Value->new(stmts => [ @prep, $nhead, @pop ],
            type => $type);
    } elsif (@pop) {
        my $nhead = CLROp::Term->new(op => ['set_result'], type => 'Void',
            zyg => [ CLROp::Term->new(op => $ops, type => $type,
                    zyg => [ @args ]) ]);

        return CLROp::Value->new(stmts => [ @prep, $nhead, @pop ],
            type => $type);
    } else {
        my $nhead = CLROp::Term->new(op => $ops, type => $type,
            zyg => [ @args ], constant => $const);

        return CLROp::Value->new(stmts => [ @prep ],
            type => $type, head => $nhead);
    }
}

sub do_ternary {
    my ($op, $check, $true, $false) = @_;

    my $lf = 'false' . ($spill++);
    my $le = 'end' . ($spill++);

    CLROp::Value->new(
        type => $true->type,
        stmts => [
            @{ $check->stmts },
            CLROp::Term->new(op => ['ncgoto', $lf], zyg => [ $check->anyhead ]),
            $true->stmts_result,
            CLROp::Term->new(op => ['goto', $le]),
            CLROp::Term->new(op => ['labelhere', $lf]),
            $false->stmts_result,
            CLROp::Term->new(op => ['labelhere', $le])
        ]);
}

sub do_while {
    my ($op, $check, $body) = @_;
    my $until = $op->[1];
    my $once = $op->[2];
    die "type error" unless $body->type eq 'Void';

    my $lagain = 'again' . ($spill++);
    my $lcheck = 'check' . ($spill++);

    my @bits;
    push @bits, CLROp::Term->new(op => ['goto', $lcheck]) unless $once;
    push @bits, CLROp::Term->new(op => ['labelhere', $lagain]);
    push @bits, @{ $body->stmts };
    push @bits, CLROp::Term->new(op => ['labelhere', $lcheck]) unless $once;
    push @bits, @{ $check->stmts };
    push @bits, CLROp::Term->new(op => [($until ? 'ncgoto' : 'cgoto'), $lagain],
        zyg => [ $check->anyhead ]);
    CLROp::Value->new(stmts => \@bits, type => 'Void');
}

sub do_seq {
    my ($op, @zyg) = @_;
    return CLROp::Value->new(stmts => [ ], type => 'Void') unless @zyg;
    my $fin = pop @zyg;
    for (@zyg) { next if $_->type eq 'Void'; say(YAML::XS::Dump($_)); die "type error"; }
    CLROp::Value->new(stmts => [ map { @{ $_->stmts } } @zyg, $fin ],
        head => $fin->head, type => $fin->type);
}

sub do_span {
    my ($op, $zyg) = @_;
    CLROp::Value->new(type => $zyg->type, stmts => [
            CLROp::Term->new(op => ['labelhere', $op->[1]]),
            ($op->[3] ? (CLROp::Term->new(op => ['cpssync'])) : ()),
            $zyg->stmts_result,
            ($op->[3] ? (CLROp::Term->new(op => ['cpssync'])) : ()),
            CLROp::Term->new(op => ['labelhere', $op->[2]]) ]);
}

sub do_annotation {
    my ($op, $zyg) = @_;
    return $zyg unless @{ $zyg->stmts };
    CLROp::Value->new(type => $zyg->type, head => $zyg->head, stmts => [
            CLROp::Term->new(op => ['push_line', $op->[1]]),
            @{ $zyg->stmts },
            CLROp::Term->new(op => ['pop_line']) ]);
}

sub do_let {
    my ($op, $head, @zyg) = @_;
    CLROp::Value->new(type => (@zyg ? $zyg[-1]->type : 'Void'), stmts => [
            @{ $head->stmts },
            CLROp::Term->new(op => ['push_let', $op->[1]],
                zyg => [ $head->anyhead ]),
            (map { $_->stmts_result } @zyg),
            CLROp::Term->new(op => ['drop_let', $op->[1]])]);
}

my @_all = qw/ callframe call_method call_sub cast cgoto clr_arith clr_bool
    clr_call_direct clr_call_virt clr_char clr_compare clr_double clr_field_get
    clr_field_set clr_index_get clr_index_set clr_int clr_new clr_new_arr
    clr_new_zarr clr_sfield_get clr_sfield_set clr_string const drop drop_let
    ehspan goto hintget labelhere labelid ncgoto peek_let poke_let pop_line
    pos push_let push_line push_null return rtpadget rtpadgeti rtpadput
    rtpadputi rxbprim rxpushb cpssync /;

my %_md = (
    'ann' => \&do_annotation,
    'while' => \&do_while,
    'ternary' => \&do_ternary,
    'span' => \&do_span,
    'seq' => \&do_seq,
    (map { $_ => \&do_primitive } @_all),
);

sub cvt {
    my $zyg = $_[0]->zyg;
    my $op  = $_[0]->op;

    if ($op->[0] eq 'let') {
        my ($z, @r) = @$zyg;
        $z = cvt($z);
        local $lettypes{$op->[1]} = $z->type;
        do_let($op, $z, map { cvt($_) } @r);
    } elsif ($op->[0] eq 'drop') {
        do_drop($zyg->[0]);
    } else {
        $_md{$op->[0]}->($op, map { cvt($_) } @$zyg);
    }
}

sub codegen {
    my ($cg, $root) = @_;
    local %lettypes;
    local $spill = 0;

    #say(YAML::XS::Dump($root)) if $cg->csname eq 'G256ACCEPTSC';
    my @stmts = cvt($root)->stmts_result;
    #say(YAML::XS::Dump(@stmts)) if $cg->csname eq 'G256ACCEPTSC';

    for (@stmts) {
        codegen_term($cg, $_)
            if !$cg->unreach || $_->op->[0] eq 'ehspan' ||
                $_->op->[0] eq 'labelhere' || $_->op->[0] eq 'drop_let';
    }
}

sub codegen_term {
    my ($cg, $term) = @_;
    my ($op, @args) = @{ $term->op };
    $cg->$op(@args, map { codegen_term($cg, $_) } @{ $term->zyg });
}
