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

    run_body($unit->mainline);
}

sub run_body {
    my ($body) = @_;
    run_body($_) for map { $_->bodies } @{ $body->decls };

    # XXX enter and sigs need love
    run_optree($body, $body->do);
}

sub run_optree {
    my ($body, $op) = @_;

    if ($op->isa('Op::CallSub') && $op->invocant->isa('Op::SubDef')
            && $op->invocant->once && is_removable_body($op->invocant->body)) {
        beta_optimize($body, $op);
    } else {
        for ($op->zyg) {
            run_optree($body, $_);
        }
    }
}

sub deb {
    #say @_;
}

sub is_removable_body {
    my ($body) = @_;

    deb $body->csname, " is a candidate for beta-removal";

    if (!$body->signature) {
        deb "... unsuitable because it's a raw call";
        return 0;
    }

    # We can't currently handle the possibility of outer references to the
    # frame we're mangling
    for (@{ $body->decls }) {
        for ($_->bodies) {
            deb "... unsuitable because it has a child: ", $_->csname;
            return 0;
        }

        if (!$_->isa('Decl::SimpleVar')) {
            deb "... unsuitable because it has an unhandled decl $_";
            return 0;
        }

        for my $ke ($_->used_slots(0)) {
            my $k = $ke->[0];
            if ($k =~ /^.?[?*]/) {
                deb "... unsuitable because it has a context variable ($k)";
                return 0;
            }
        }
    }

    return 1;
}

# Applicability already checked
sub beta_optimize {
    my ($body, $op) = @_;

    my $ib = $op->invocant->body;
    # Bind the arguments to gensyms so they won't be shadowed by anything in
    # the function
    my @args = map { [ $_, Niecza::Actions->gensym ] } @{ $op->positionals };

    @{ $body->decls } = grep { !$_->isa('Decl::Sub') ||
        $_->code != $ib } @{ $body->decls };

    my @pos = (map { Op::Lexical->new(name => $_->[1]) } @args);

    my $nop = Op::StatementList->new(children => [
        Op::SigBind->new(signature => $ib->signature,
            positionals => \@pos),
        $ib->do]);

    for my $d (reverse @{ $ib->decls }) {
        my $to = $d->hash ? CgOp::newblankhash :
                 $d->list ? CgOp::newblanklist :
                            CgOp::newblankrwscalar;
        $nop = Op::Let->new(var => $d->slot,
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
