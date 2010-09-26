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

# deliberately omitted for now: the run_once optimization & lazy static pad
# generation & indexification

our $unit;
our %peers;
our $nid = 0;
our @decls;
our @thaw;
our @cgs;
our %haslet;

sub gsym {
    my ($type, $desc) = @_;
    $desc =~ s/(\W)/"_" . ord($1)/eg;
    my $base = 'G' . ($nid++) . $desc;
    my $full = $unit->name . "." . $base . ':f,' . $type;
    wantarray ? ($full, $base) : $full;
}

my $st_ty = 'Dictionary<string,BValue>';
my $cl_ty = 'DynMetaObject';
my $si_ty = 'SubInfo';

sub run {
    local $unit = shift;
    local %peers;
    local $nid = 0;
    local @thaw;
    local @decls;
    local @cgs;

    # First, set up all the objects
    # Then, fill them out
    # this makes reference loops work.

    $unit->visit_local_stashes(\&head_stash);
    $unit->visit_local_packages(\&head_pkg);
    $unit->visit_local_subs_preorder(\&head_sub);

    $unit->visit_local_subs_preorder(\&fill_sub);

    my $mod = '';
    $mod .= <<EOH ;
using Niecza;
using System;
using System.Collections.Generic;

public class ${\ $unit->name } {
    public static void Main() {
        Kernel.RunLoop(new SubInfo("boot", BOOT));
    }

EOH

    push @thaw, CgOp::subcall(CgOp::rawsget($peers{$unit->mainline}{ps}));
    push @thaw, CgOp::return;

    for (@decls) {
        /(?:.*?\.)?(.*):f,(.*)/;
        $mod .= "    public static $2 $1;\n";
    }

    $mod .= CodeGen->new(csname => 'BOOT',
        ops => CgOp::prog(@thaw)->cps_convert(0))->csharp;
    $mod .= $_ for (@cgs);
    $mod .= "}\n";

    +{ mod => $mod, peers => \%peers };
}

sub head_stash {
    my $p = $peers{$_} = gsym($st_ty, 'STASH');
    push @decls, $p;
    push @thaw, CgOp::rawsset($p, CgOp::rawnew($st_ty));
}

sub head_pkg {
    return unless $_->isa('Metamodel::Class');
    my $p   = $peers{$_}{mo} = gsym($cl_ty, $_->name);
    my $whv = $peers{$_}{what_var} = gsym('Variable', $_->name . '_WHAT');
    my $wh6 = $peers{$_}{what_ip6} = gsym('IP6', $_->name . '_WHAT');
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
            CgOp::rawsget($peers{$s}{mo}));
    }
    push @thaw, CgOp::rawcall(CgOp::rawsget($p), 'Complete');
}

sub enter_code {
    ();
}

sub access_lex {
    my ($body, $name, $set_to) = @_;

    if ($haslet{$name}) {
        return CgOp::letvar($name, $set_to);
    }

    my $bp = $body;
    my $order = 0;
    my $lex;
    while ($bp) {
        $lex = $bp->lexicals->{$name};
        if (!$lex) {
            $bp = $bp->outer;
            $order++;
        } elsif ($lex->isa('Metamodel::Lexical::Alias')) {
            $name = $lex->to;
        } else {
            last;
        }
    }

    if (!defined($lex)) {
        die "Internal error: failed to resolve lexical $name in " . $body->name;
    }

    if ($lex->isa('Metamodel::Lexical::SubDef') ||
            $lex->isa('Metamodel::Lexical::Simple')) {
        return $set_to ?
            CgOp::Primitive->new(op => [ rtpadput => $order, $name ],
                zyg => [ $set_to ]) :
            CgOp::Primitive->new(op => [ rtpadget => 'Variable',$order,$name ]);
    } elsif ($lex->isa('Metamodel::Lexical::Stash')) {
        die "cannot rebind stashes" if $set_to;
        return CgOp::rawsget($peers{$lex->referent}{what_var});
    } elsif ($lex->isa('Metamodel::Lexical::Common')) {
        return $set_to ?
            CgOp::bset(CgOp::rawsget($peers{$lex}), $set_to) :
            CgOp::bget(CgOp::rawsget($peers{$lex}));
    } else {
        die "unhandled $lex";
    }
}

sub resolve_lex {
    my ($body, $op) = @_;

    if ($op->isa('CgOp::Primitive')) {
        my ($opc, $arg, @rest) = @{ $op->op };
        if ($opc eq 'scopelex') {
            my $nn = access_lex($body, $arg, $op->zyg->[0]);
            #XXX
            %$op = %$nn;
            bless $op, ref($nn);
        }
    }

    if ($op->isa('CgOp::Let')) {
        local $haslet{$op->name} = 1;
        resolve_lex($body, $_) for @{ $op->zyg };
    } else {
        resolve_lex($body, $_) for @{ $op->zyg };
    }
}

sub codegen_sub {
    my @enter = enter_code($_);
    my $ops;
    # TODO: Bind a return value here to catch non-ro sub use
    if ($_->gather_hack) {
        $ops = CgOp::prog(@enter, CgOp::sink($_->code->cgop),
            CgOp::rawsccall('Kernel.Take', CgOp::scopedlex('EMPTY')));
    } elsif ($_->returnable && defined($_->signature)) {
        $ops = CgOp::prog(@enter,
            CgOp::return(CgOp::span("rstart", "rend",
                    $_->code->cgop)),
            CgOp::ehspan(4, undef, 0, "rstart", "rend", "rend"));
    } else {
        $ops = CgOp::prog(@enter, CgOp::return($_->code->cgop));
    }

    local %haslet;
    resolve_lex($_, $ops);
    CodeGen->new(csname => $peers{$_}{cbase}, ops => $ops->cps_convert(0));
}

# lumped under a sub are all the static-y lexicals
# protopads and proto-sub-instances need to exist early because methods, in
# particular, bind to them
# note: preorder
sub head_sub {
    my $node = ($peers{$_} = {});
    my $si = $node->{si} = gsym($si_ty, $_->name);
    @$node{'cref','cbase'} = gsym('DynBlockDelegate', $_->name . 'C');
    push @decls, $si;

    my $cg = codegen_sub($_);

    push @thaw, CgOp::rawsset($si, CgOp::rawnew($si_ty,
            $cg->subinfo_ctor_args(
                ($_->outer ? CgOp::rawsget($peers{$_->outer}{si}) :
                    CgOp::null('SubInfo')),
                CgOp::null('LAD'))));

    push @cgs, $cg->csharp;

    my $pp = $node->{pp} = gsym('Frame', $_->name . 'PP');
    push @decls, $pp;
    push @thaw, CgOp::rawsset($pp, CgOp::rawnew('Frame',
            CgOp::null('Frame'), (!$_->outer ? CgOp::null('Frame') :
                CgOp::rawsget($peers{$_->outer}{pp})),
            CgOp::rawsget($si)));
    push @thaw, CgOp::setfield('lex', CgOp::rawsget($pp),
        CgOp::rawnew('Dictionary<string,object>'));

    my $ps = $node->{ps} = gsym('IP6', $_->name . 'PS');
    push @decls, $ps;
    push @thaw, CgOp::rawsset($ps, CgOp::rawscall('Kernel.MakeSub',
            CgOp::rawsget($si), !$_->outer ? CgOp::null('Frame') :
                CgOp::rawsget($peers{$_->outer}{pp})));

    for my $ln (keys %{ $_->lexicals }) {
        my $lx = $_->lexicals->{$ln};

        if ($lx->isa('Metamodel::Lexical::Common')) {
            my $bv = $peers{$lx} = gsym('BValue', $lx->name);
            push @decls, $bv;
        }
    }
}

sub fill_sub {
    for my $ln (keys %{ $_->lexicals }) {
        my $lx = $_->lexicals->{$ln};
        my $frag;

        if ($lx->isa('Metamodel::Lexical::Common')) {
            push @thaw, CgOp::rawsset($peers{$lx},
                CgOp::rawscall('Kernel.PackageLookup',
                    CgOp::rawsget($peers{$lx->stash}),
                    CgOp::clr_string($lx->name)));
        } elsif ($lx->isa('Metamodel::Lexical::SubDef')) {
            push @thaw, CgOp::setindex($ln,
                CgOp::getfield('lex', CgOp::rawsget($peers{$_}{pp})),
                CgOp::newscalar(CgOp::rawsget($peers{$lx->body}{ps})));
        } elsif ($lx->isa('Metamodel::Lexical::Simple')) {
            if ($lx->hash || $lx->list) {
                # XXX should be SAFE::
                my $imp = $_->find_lex($lx->hash ? 'Hash' : 'Array')->referent;
                my $var = $peers{$imp->obj}{what_var};
                $frag = CgOp::methodcall(CgOp::rawsget($var), 'new');
            } else {
                $frag = CgOp::newblankrwscalar;
            }
            push @thaw, CgOp::setindex($ln,
                CgOp::getfield('lex', CgOp::rawsget($peers{$_}{pp})),
                $frag);
        }
    }
}
