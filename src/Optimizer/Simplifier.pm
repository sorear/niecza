use 5.010;
use strict;
use warnings;
use utf8;

package Optimizer::Simplifier;

# This optimization pass handles lowering calls to well-known functions

our $unit;

sub run {
    (local $unit) = @_;

    # XXX enter and sigs need love
    $unit->visit_local_subs_postorder(sub { run_optree($_, $_->code, 1) });
}

sub no_named_params {
    my $op = shift;

    if ($op->args) {
        for (@{ $op->args }) {
            if ($_->isa('Op::SimplePair') || $_->isa('Op::Flatten')) {
                return 0;
            }
        }
    }
    return ($op->args // $op->positionals);
}

sub is_simple_var {
    my $op = shift;
    $op = $op->inner while $op->isa('Op::Paren');
    return unless $op->isa('Op::Lexical');
    return if $op->declaring || $op->state_backing;
    return $op->name;
}

our %funcs = (
    '&infix:<=>' => \&do_assign,
);

sub do_assign {
    my ($body, $nv, $invname, $args) = @_;
    return unless @$args == 2;

    if (!$nv) {
        return Op::Assign->new(lhs => $args->[0], rhs => $args->[1]);
    } elsif (defined(my $name = is_simple_var($args->[0]))) {
        return Op::StatementList->new(children => [
                Op::Assign->new(lhs => $args->[0], rhs => $args->[1]),
                Op::Lexical->new(name => $name)]);
    } else {
        my $id = Niecza::Actions->gensym;
        return Op::Let->new(var => $id, to => $args->[0], in =>
            Op::StatementList->new(children => [
                    Op::Assign->new(lhs => Op::LetVar->new(name => $id),
                        rhs => $args->[1]),
                    Op::LetVar->new(name => $id)]));
    }
}

sub run_optree {
    my ($body, $op, $nv) = @_;
    my @kids = $op->ctxzyg($nv);
    while (my ($kc, $nvc) = splice @kids, 0, 2) {
        run_optree($body, $kc, $nvc);
    }

    return unless $op->isa('Op::CallSub');
    my $inv = $op->invocant;
    return unless $inv->isa('Op::Lexical');
    my $invname = $inv->name;
    my $inv_lex = $body->find_lex($invname);
    return unless $inv_lex && $inv_lex->isa('Metamodel::Lexical::SubDef')
        && $inv_lex->body->unit->is_true_setting;
    return unless my $func = $funcs{$invname};
    return unless my $pos = no_named_params($op);

    my $r = $func->($body, $nv, $invname, $pos);
    if ($r) {
        %$op = %$r;
        bless $op, ref($r);
    }
}

1;
