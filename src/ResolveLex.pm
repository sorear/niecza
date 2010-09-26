use 5.010;
use strict;
use warnings;
use utf8;

package ResolveLex;

our %haslet;
our $body;

sub run {
    my ($ops, $tbody) = @_;

    local %haslet;
    local $body = $tbody;
    run_cgop($ops);
}

sub run_cgop {
    my ($op) = @_;

    if ($op->isa('CgOp::Primitive')) {
        my ($opc, $arg, @rest) = @{ $op->op };
        if ($opc eq 'clr_call_virt' && $arg eq 'Fetch' &&
                $op->zyg->[0]->isa('CgOp::Primitive') &&
                $op->zyg->[0]->op->[0] eq 'scopelex') {
            my $nn = resolve_lex($arg, $op->zyg->[0], undef, 1);
            %$op = %$nn;
            bless $op, ref($nn);
        } elsif ($opc eq 'scopelex') {
            my $nn = resolve_lex($arg, $op->zyg->[0]);
            #XXX
            %$op = %$nn;
            bless $op, ref($nn);
        }
    }

    if ($op->isa('CgOp::Let')) {
        local $::haslet{$op->name} = 1;
        run_cgop($_) for @{ $op->zyg };
    } else {
        run_cgop($_) for @{ $op->zyg };
    }
}

sub _dofetch {
    my ($fetch, $x) = @_;
    $fetch ? CgOp::fetch($x) : $x;
}

sub resolve_lex {
    my ($name, $set_to, $pad, $fetch) = @_;

    if ($haslet{$name}) {
        return _dofetch($fetch, CgOp::letvar($name, $set_to));
    }

    my $lex = $body->find_lex($name);
    return undef;

=for comment

    if (!defined($lex)) {
        #print STDERR YAML::XS::Dump ($body);
        die "Internal error: failed to resolve lexical $name in " . $body->name;
    }

    if ($lex->isa('Metamodel::Lexical::Simple')) {
        x$CSharpBackend::peers{$lex}

    if (($kind =~ /[1235]/) && $data =~ /(.*)\./) {
        $::UNITREFS{$1} = 1;
    }

    if ($kind == 5) {
        if ($set_to) {
            return CgOp::bset(CgOp::rawsget($data . ":f,BValue"), $set_to);
        } else {
            return CgOp::bget(CgOp::rawsget($data . ":f,BValue"));
        }
    } elsif ($kind == 4) {
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

=cut

}

1;
