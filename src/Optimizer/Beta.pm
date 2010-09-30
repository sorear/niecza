use 5.010;
use strict;
use warnings;
use utf8;

package Optimizer::Beta;

# A simple Perl6 compiler generates a lot of expressions of the form
# (-> $x { block })($y), due to control structures and regexes.  Try to clean
# that up here.

sub run {
    my ($unit) = @_;

    # XXX enter and sigs need love
    $unit->visit_local_subs_postorder(sub { run_optree($_, $_->code) });
}

sub run_optree {
    my ($body, $op) = @_;

    for ($op->zyg) {
        run_optree($body, $_);
    }

    return unless $op->isa('Op::CallSub') && no_named_params($op);
    my $inv = $op->invocant;
    return unless $inv->isa('Op::SubDef') && $inv->once;
    my $cbody = $body->lexicals->{$inv->var} or return;
    $cbody = $cbody->body;
    return unless is_removable_body($cbody);

    beta_optimize($body, $op, $inv, $cbody);
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
    return 1;
}

sub deb {
    #say @_;
}

sub is_removable_body {
    my ($body) = @_;

    deb $body->name, " is a candidate for beta-removal";

    if (!$body->signature) {
        deb "... unsuitable because it's a raw call";
        return 0;
    }

    if ($body->strong_used) {
        return 0;
    }

    # We can't currently handle the possibility of outer references to the
    # frame we're mangling
    for my $lname (keys %{ $body->lexicals }) {
        my $lex = $body->lexicals->{$lname};

        if (!$lex->isa('Metamodel::Lexical::Simple')) {
            deb "... unsuitable because it has an unhandled decl $_";
            return 0;
        }

        if ($lname =~ /^.?[?*]/) {
            deb "... unsuitable because it has a context variable ($lname)";
            return 0;
        }
    }

    return 1;
}

# Applicability already checked
sub beta_optimize {
    my ($body, $op, $inv, $cbody) = @_;

    # Bind the arguments to gensyms so they won't be shadowed by anything in
    # the function
    my @args = map { [ $_, Niecza::Actions->gensym ] } @{ $op->positionals };

    delete $body->lexicals->{$inv->var};

    my @pos = (map { Op::Lexical->new(name => $_->[1]) } @args);

    my $nop = Op::StatementList->new(children => [
        Op::SigBind->new(signature => $cbody->signature,
            positionals => \@pos),
        $cbody->code]);

    for my $dn (sort keys %{ $cbody->lexicals }) {
        my $d = $cbody->lexicals->{$dn};
        my $to = $d->noinit ? CgOp::null('var') :
                 $d->hash   ? CgOp::newblankhash :
                 $d->list   ? CgOp::newblanklist :
                              CgOp::newblankrwscalar;
        $nop = Op::Let->new(var => $dn,
            to => Op::CgOp->new(op => $to), in => $nop);
    }

    for my $a (reverse @args) {
        $nop = Op::Let->new(var => $a->[1], to => $a->[0], in => $nop);
    }

    # XXX
    %$op = %$nop;
    bless $op, ref($nop);
}

1;
