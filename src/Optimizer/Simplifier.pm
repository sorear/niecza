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

sub capture_params {
    my $op = shift;

    if (!$op->args) {
        return ($op->positionals);
    }

    my @named;
    my @pos;

    for (@{ $op->args }) {
        return () if ($_->isa('Op::Flatten'));
        if ($_->isa('Op::SimplePair')) {
            push @named, $_->key, $_->value;
        } else {
            push @pos, $_;
        }
    }

    (\@pos, @named);
}

sub is_simple_var {
    my $op = shift;
    $op = $op->inner while $op->isa('Op::Paren');
    return unless $op->isa('Op::Lexical');
    return if $op->declaring || $op->state_backing;
    return $op->name;
}

our %funcs = (
    '&chars'               => sub { do_builtin(chars => 1, @_) },
    '&defined'             => sub { do_builtin(defined => 1, @_) },
    '&infix:<=>'           => \&do_assign,
    '&infix:<eq>'          => sub { do_builtin(streq => 2,   @_) },
    '&infix:<ge>'          => sub { do_builtin(strge => 2,   @_) },
    '&infix:<gt>'          => sub { do_builtin(strgt => 2,   @_) },
    '&infix:<le>'          => sub { do_builtin(strle => 2,   @_) },
    '&infix:<lt>'          => sub { do_builtin(strlt => 2,   @_) },
    '&infix:<ne>'          => sub { do_builtin(strne => 2,   @_) },
    '&infix:</>'           => sub { do_builtin(divide => 2,  @_) },
    '&infix:<->'           => sub { do_builtin(minus => 2,   @_) },
    '&infix:<*>'           => sub { do_builtin(mul => 2,     @_) },
    '&infix:<==>'          => sub { do_builtin(numeq => 2,   @_) },
    '&infix:<>=>'          => sub { do_builtin(numge => 2,   @_) },
    '&infix:<>>'           => sub { do_builtin(numgt => 2,   @_) },
    '&infix:<<=>'          => sub { do_builtin(numle => 2,   @_) },
    '&infix:<<>'           => sub { do_builtin(numlt => 2,   @_) },
    '&infix:<!=>'          => sub { do_builtin(numne => 2,   @_) },
    '&infix:<+>'           => sub { do_builtin(plus => 2,    @_) },
    '&make'                => sub { do_builtin(make => 1, @_) },
    '&not'                 => sub { do_builtin(not => 1, @_) },
    '&postcircumfix:<{ }>' => \&do_atkey,
    '&postcircumfix:<[ ]>' => \&do_atpos,
    '&postfix:<++>'        => sub { do_builtin(postinc => 1, @_) },
    '&prefix:<?>'          => sub { do_builtin(bool => 1, @_) },
    '&prefix:<->'          => sub { do_builtin(negate => 1, @_) },
    '&prefix:<!>'          => sub { do_builtin(not => 1, @_) },
    '&prefix:<+>'          => sub { do_builtin(num => 1, @_) },
    '&prefix:<~>'          => sub { do_builtin(str => 1, @_) },
    '&so'                  => sub { do_builtin(bool => 1, @_) },
    '&substr'              => sub { do_builtin(substr3 => 3, @_) },
);

sub do_assign {
    my ($body, $nv, $invname, $op) = @_;
    return unless my $args = no_named_params($op);
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

sub do_builtin {
    my ($name, $expect, $body, $nv, $invname, $op) = @_;
    return unless my $args = no_named_params($op);
    return unless @$args == $expect;
    return Op::Builtin->new(name => $name, args => $args);
}

sub do_atkey {
    my ($body, $nv, $invname, $op) = @_;
    return unless my ($args, %named) = capture_params($op);
    return unless @$args == 2;
    my $delete = delete $named{delete};
    my $exists = delete $named{exists};
    return if %named;
    return if $delete && (!$delete->isa('Op::Lexical') || $delete->name ne 'True');
    return if $exists && (!$exists->isa('Op::Lexical') || $exists->name ne 'True');
    return if $delete && $exists;
    return Op::Builtin->new(name => ($delete ? 'delete_key' :
            $exists ? 'exists_key' : 'at_key'), args => $args);
}

sub do_atpos {
    my ($body, $nv, $invname, $op) = @_;
    return unless my $args = no_named_params($op);
    return unless @$args == 2;
    return Op::Builtin->new(name => 'at_pos', args => $args);
}

sub run_optree {
    my ($body, $op, $nv) = @_;
    Carp::confess "WTF" if !defined $nv;
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

    my $r = $func->($body, $nv, $invname, $op);
    if ($r) {
        %$op = %$r;
        bless $op, ref($r);
    }
}

1;
