use 5.010;
use strict;
use warnings;
use utf8;

package ResolveLex;

sub run {
    my ($unit) = @_;

    run_body($unit->mainline);
    run_cgop($unit->bootcgop, []);
}

sub run_body {
    my ($body) = @_;

    for my $d (@{ $body->decls }) {
        for my $b ($d->bodies) {
            run_body($b);
        }
    }

    local %::haslet;
    run_cgop($body->cgoptree, [ $body ]);
}

sub run_cgop {
    my ($op, $btree) = @_;
    my $lvl = scalar @$btree;

    if ($op->isa('CgOp::Let')) {
        local $::haslet{$op->name} = 1;
        run_cgop($_, $btree) for @{ $op->zyg };
    } else {
        run_cgop($_, $btree) for @{ $op->zyg };
    }

    if ($op->isa('CgOp::Primitive')) {
        my ($opc, $arg, @rest) = @{ $op->op };
        if ($opc eq 'open_protopad') {
            push @$btree, $arg;
        } elsif ($opc eq 'close_sub') {
            pop @$btree;
        } elsif ($opc eq 'clr_call_direct' && $arg eq 'Kernel.Fetch' &&
                $op->zyg->[0]->isa('CgOp::Primitive') &&
                $op->zyg->[0]->op->[0] eq 'clr_sfield_get' &&
                $op->zyg->[0]->op->[1] =~ /(.*)_var:f,Variable/) {
            my $nn = CgOp::rawsget($1 . ":f,IP6");
            %$op = %$nn;
            bless $op, ref($nn);
        } elsif ($opc eq 'scopelex') {
            my $nn = resolve_lex($arg, $btree->[-1], $op->zyg->[0]);
            #XXX
            %$op = %$nn;
            bless $op, ref($nn);
        }
    }
}

sub resolve_lex {
    my ($name, $body, $set_to) = @_;

    if ($::haslet{$name}) {
        return CgOp::letvar($name, $set_to);
    }

    my ($order, $type, $kind, $data) = $body->lex_info($name);
    if ($order < 0) {
        #print STDERR YAML::XS::Dump ($body);
        die "Internal error: failed to resolve lexical $name in " . $body->name;
    }

    if (($kind == 1 || $kind == 2 || $kind == 3) && $data =~ /(.*)\./) {
        $::UNITREFS{$1} = 1;
    }

    if ($kind == 4) {
        if ($set_to) {
            return CgOp::Primitive->new(op => ['rtpadputi', $order, $data],
                zyg => [$set_to]);
        } else {
            return CgOp::Primitive->new(op => ['rtpadgeti', $type, $order, $data]);
        }
    } elsif ($kind == 3) {
        if ($set_to) {
            return CgOp::let($set_to, sub { my $x = $_[0];
                CgOp::prog( CgOp::rawsset($data, CgOp::fetch($x)),
                    CgOp::rawsset($data . "_var", CgOp::newscalar(
                            CgOp::rawsget($data . ":f,IP6")))) });
        } else {
            return CgOp::rawsget($data . "_var:f,Variable");
        }
    } elsif ($kind == 2) {
        if ($set_to) {
            die "panic: Assigning to a hint";
        } else {
            return CgOp::Primitive->new(op => ['hintget', $type, $data, $name]);
        }
    } elsif ($kind == 1) {
        if ($set_to) {
            return CgOp::rawsset($data, $set_to);
        } else {
            return CgOp::rawsget($data . ":f," . $type);
        }
    } elsif ($kind == 0) {
        if ($set_to) {
            return CgOp::Primitive->new(op => ['rtpadput', $order, $name],
                zyg => [$set_to]);
        } else {
            return CgOp::Primitive->new(op => ['rtpadget', $type, $order, $name]);
        }
    } else {
        die "panic: invalid kind $kind";
    }
}

1;
