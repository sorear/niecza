use 5.010;
use strict;
use warnings;
use utf8;

package CSharpBackend;

# Input:  A Metamodel::Unit object
# Output: A CodeGenUnit object

# each StaticSub generates <some code> for the sub's body
# we also generate <some code> to build the runtime MOP (really, a thawer using
# CIL; a thaw using a custom format would be faster)

# StaticSub : SubInfo (handled partially in CodeGen itself)
# StaticSub : (opt) static pad, some static fields
# Stash     : Dictionary<string,BValue>
# Class     : DynMetaObject
# Class     : (opt) HOW

our $unit;
our %peers;
our $nid = 0;

sub gsym {
    my ($type, $desc) = @_;
    $desc =~ s/(\W)/"_" . ord($1)/eg;
    $unit->name . ".G" . ($nid++) . $desc . ':f,' . $type;
}

my $st_ty = 'Dictionary<string,BValue>';
my $cl_ty = 'DynMetaObject';

sub run {
    local $unit = shift;
    local %peers;
    local $nid = 0;
    my @thaw;
    my @decls;

    $unit->visit_local_stashes(sub {
        my $p = $peers{$_} = gsym($st_ty, 'STASH');
        push @decls, $p;
        push @thaw, CgOp::rawsset($p, CgOp::rawnew($st_ty));
    });
    $unit->visit_local_packages(sub {
        return unless $_->isa('Metamodel::Class');
        my $p = $peers{$_} = gsym($cl_ty, $_->name);
        push @decls, $p;
        if ($unit->is_true_setting && ($_->name eq 'Scalar' ||
                $_->name eq 'Sub')) {
            push @thaw, CgOp::rawsset($p,
                CgOp::rawsget("Kernel." . $_->name . "MO:f,$cl_ty"));
        } else {
            push @thaw, CgOp::rawsset($p, CgOp::rawnew($cl_ty,
                    CgOp::clr_string($_->name)));
            for my $a (@{ $_->attributes }) {
                push @thaw, CgOp::rawcall(CgOp::rawsget($p), 'AddAttribute',
                    CgOp::clr_string($a));
            }
        }
        for my $s (@{ $_->superclasses }) {
            push @thaw, CgOp::rawcall(CgOp::rawsget($p), 'AddSuperclass',
                CgOp::rawsget($peers{$s}));
        }
        push @thaw, CgOp::rawcall(CgOp::rawsget($p), 'Complete');
    });

    +{ thaw => \@thaw, decls => \@decls, peers => \%peers };
}
