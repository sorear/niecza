use 5.010;
use strict;
use warnings;
use utf8;

package CgOpToCLROp;

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
    clr_index_set rxbprim rxpushb return/);

$fixtype{$_} = 'Variable' for (qw/ pos call_sub call_method attr_var /);

sub type_infer {
    my ($op, @argtypes) = @_;
    my $head = $op->[0];

    if ($fixtype{$head}) { return $fixtype{$head} }

    given ($head) {
        when ("clr_call_direct") {
            return (CLRTypes->info("cm", $op->[1]))[2];
        }
        when ("clr_call_virt") {
            return (CLRTypes->info("cm", $argtypes[0], $op->[1]))[2];
        }
        when ("clr_field_get") {
            return (CLRTypes->info("f", $argtypes[0], $op->[1]))[2];
        }
        when ("clr_index_get") {
            return (CLRTypes->info("i", $argtypes[0], 'Item'))[2];
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
sub _drop { CgOp::Primitive->new(op => ['drop'], zyg => [$_[0]]) }
my %_dropnow = map {; $_ => 1 } qw/ push_null rtpadget rtpadgeti hint_get
    clr_sfield_get result pos peek_let /;
sub cvt_drop {
    my $self = shift;

    my $tgt = $self->zyg->[0];
    my @zyg = @{ $tgt->zyg };

    given (ref $tgt) {
        when ('CgOp::Let') {
            $zyg[-1] = _drop($zyg[-1]);
            return cvt(CgOp::Let->new(name => $tgt->name, zyg => [ @zyg ]));
        }
        when ('CgOp::Seq') {
            $zyg[-1] = _drop($zyg[-1]);
            return cvt(CgOp::Seq->new(zyg => [ @zyg ]));
        }
        when ('CgOp::Ternary') {
            @zyg[1,2] = map { _drop($_) } @zyg[1,2];
            return cvt(CgOp::Ternary->new(zyg => [ @zyg ]));
        }
        when ('CgOp::Annotation') {
            return cvt(CgOp::Annotation->new(line => $tgt->line,
                    file => $tgt->file, zyg => [ _drop($zyg[0]) ]));
        }
        when ('CgOp::Span') {
            return cvt(CgOp::Span->new(lstart => $tgt->lstart,
                    lend => $tgt->lend, zyg => [ _drop($zyg[0]) ]));
        }
        when ('CgOp::Primitive') {
            my $v = cvt($tgt);
            my @zyg = @{ $v->stmts };
            my $h = $v->head;
            if ($h && $h->op->[0] eq 'clr_field_get') { $h = $h->zyg->[0] }
            if ($h && !$_dropnow{$h->op->[0]}) {
                push @zyg, CLROp::Term->new(op => ['drop'], zyg => [$h]);
            }
            return CLROp::Value->new(type => 'Void', stmts => \@zyg);
        }
        when ("CgOp::While") { die "implausible use of drop on while" }
    }
}

sub cvt_primitive {
    my $self = $_[0];
    if ($self->op->[0] eq 'drop') { goto &cvt_drop; }
    my @zyg = map { cvt($_) } @{ $self->zyg };
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

    my $type = type_infer($self->op, map { $_->type } @args);

    if ($type eq 'Void') {
        my $nhead = CLROp::Term->new(op => $self->op, type => $type,
            zyg => [ @args ]);

        return CLROp::Value->new(stmts => [ @prep, $nhead, @pop ],
            type => 'Void');
    } elsif ($self->is_cps_call) {
        my $nhead = CLROp::Term->new(op => $self->op, type => 'Void',
            zyg => [ @args ]);

        return CLROp::Value->new(stmts => [ @prep, $nhead, @pop ],
            type => $type);
    } elsif (@pop) {
        my $nhead = CLROp::Term->new(op => ['set_result'], type => 'Void',
            zyg => [ CLROp::Term->new(op => $self->op, type => $type,
                    zyg => [ @args ]) ]);

        return CLROp::Value->new(stmts => [ @prep, $nhead, @pop ],
            type => $type);
    } else {
        my $nhead = CLROp::Term->new(op => $self->op, type => $type,
            zyg => [ @args ], constant => $self->constant);

        return CLROp::Value->new(stmts => [ @prep ],
            type => $type, head => $nhead);
    }
}

sub cvt_ternary {
    my $self = shift;
    my ($check, $true, $false) = map { cvt($_) } @{ $self->zyg };

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

sub cvt_while {
    my $self = shift;
    my ($check, $body) = map { cvt($_) } @{ $self->zyg };
    my $once = $self->once;
    my $until = $self->until;
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

sub cvt_seq {
    my $self = shift;
    my @zyg = map { cvt($_) } @{ $self->zyg };
    return CLROp::Value->new(stmts => [ ], type => 'Void') unless @zyg;
    my $fin = pop @zyg;
    for (@zyg) { next if $_->type eq 'Void'; say(YAML::XS::Dump($_)); die "type error"; }
    CLROp::Value->new(stmts => [ map { @{ $_->stmts } } @zyg, $fin ],
        head => $fin->head, type => $fin->type);
}

sub cvt_span {
    my $self = shift;
    my $zyg = cvt($self->zyg->[0]);
    CLROp::Value->new(type => $zyg->type, stmts => [
            CLROp::Term->new(op => ['labelhere', $self->lstart]),
            $zyg->stmts_result,
            CLROp::Term->new(op => ['labelhere', $self->lend]) ]);
}

sub cvt_annotation {
    my $self = shift;
    my $zyg = cvt($self->zyg->[0]);
    return $zyg unless @{ $zyg->stmts };
    CLROp::Value->new(type => $zyg->type, head => $zyg->head, stmts => [
            CLROp::Term->new(op => ['push_line', $self->line]),
            @{ $zyg->stmts },
            CLROp::Term->new(op => ['pop_line']) ]);
}

sub cvt_let {
    my $self = shift;
    my ($head, @zyg) = @{ $self->zyg };
    $head = cvt($head);
    {
        local $lettypes{$self->name} = $head->type;
        @zyg = map { cvt($_) } @zyg;
    }
    CLROp::Value->new(type => (@zyg ? $zyg[-1]->type : 'Void'), stmts => [
            @{ $head->stmts },
            CLROp::Term->new(op => ['push_let', $self->name],
                zyg => [ $head->anyhead ]),
            (map { $_->stmts_result } @zyg),
            CLROp::Term->new(op => ['drop_let', $self->name])]);
}

my %_md = (
    'CgOp::Annotation' => \&cvt_annotation,
    'CgOp::Let' => \&cvt_let,
    'CgOp::Primitive' => \&cvt_primitive,
    'CgOp::While' => \&cvt_while,
    'CgOp::Ternary' => \&cvt_ternary,
    'CgOp::Span' => \&cvt_span,
    'CgOp::Seq' => \&cvt_seq,
);
sub cvt { goto &{ $_md{ref($_[0])} } }

sub codegen {
    my ($cg, $root) = @_;
    local %lettypes;
    local $spill = 0;

    #say(YAML::XS::Dump($root));
    my @stmts = cvt($root)->stmts_result;
    #say(YAML::XS::Dump(@stmts));

    for (@stmts) {
        codegen_term($cg, $_)
            if !$cg->unreach || $_->op->[0] eq 'ehspan' ||
                $_->op->[0] eq 'labelhere';
    }
}

sub codegen_term {
    my ($cg, $term) = @_;
    my ($op, @args) = @{ $term->op };
    $cg->$op(@args, map { codegen_term($cg, $_) } @{ $term->zyg });
}
