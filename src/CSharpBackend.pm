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
our @decls;
our @thaw;
our @cgs;

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

    +{ thaw => \@thaw, decls => \@decls, peers => \%peers };
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

# lumped under a sub are all the static-y lexicals
# protopads and proto-sub-instances need to exist early because methods, in
# particular, bind to them
# note: preorder
sub head_sub {
    my $node = ($peers{$_} = {});
    my $si = $node->{si} = gsym($si_ty, $_->name);
    @$node{'cref','cbase'} = gsym('DynBlockDelegate', $_->name . 'C');
    push @decls, $si;

    #my $cg = $node->{cg} = codegen_sub($_);

    #push @thaw, CgOp::rawsset($si, CgOp::rawnew($si_ty,
    #        $cg->subinfo_ctor_args));

    if ($_->spad_exists) {
        my $pp = $node->{pp} = gsym('Frame', $_->name . 'PP');
        push @decls, $pp;
        push @thaw, CgOp::rawsset($pp, CgOp::rawnew('Frame',
                CgOp::null('Frame'), (!$_->outer ? CgOp::null('Frame') :
                    CgOp::rawsget($peers{$_->outer}{pp})),
                CgOp::null('Frame'), CgOp::rawsget($si)));
    }

    if (!$_->outer || $_->outer->spad_exists) {
        my $ps = $node->{ps} = gsym('IP6', $_->name . 'PS');
        push @decls, $ps;
        push @thaw, CgOp::rawsset($ps, CgOp::rawscall('Kernel.MakeSub',
                CgOp::rawsget($si), !$_->outer ? CgOp::null('Frame') :
                    CgOp::rawsget($peers{$_->outer}{pp})));
    }

    for my $ln (keys %{ $_->lexicals }) {
        my $lx = $_->lexicals->{$ln};

        if ($lx->isa('Metamodel::Lexical::Common')) {
            my $bv = $peers{$lx} = gsym('BValue', $lx->name);
            push @decls, $bv;
        } elsif (($lx->isa('Metamodel::Lexical::Simple') ||
                $lx->isa('Metamodel::Lexical::SubDef')) && $_->run_once) {
            my $sl = $peers{$lx} = gsym('Variable', $ln);
            push @decls, $sl;
        }
    }
}

sub fill_sub {
    return unless $_->spad_exists;
    for my $ln (keys %{ $_->lexicals }) {
        my $lx = $_->lexicals->{$ln};
        my $frag;
        my $forcevar;

        if ($lx->isa('Metamodel::Lexical::Common')) {
            $frag = CgOp::rawscall('Kernel.PackageLookup',
                CgOp::rawsget($peers{$lx->stash}), CgOp::clr_string($lx->name));
            $forcevar = 1;
        } elsif ($lx->isa('Metamodel::Lexical::SubDef')) {
            $frag = CgOp::rawsget($peers{$lx->body}{ps_var});
        } elsif ($lx->isa('Metamodel::Lexical::Simple')) {
            if ($lx->hash || $lx->list) {
                # XXX should be SAFE::
                my $imp = $_->find_lex($lx->hash ? 'Hash' : 'Array')->referent;
                my $var = $peers{$imp->obj}{what_var};
                $frag = CgOp::methodcall(CgOp::rawsget($var), 'new');
            } else {
                $frag = CgOp::newblankrwscalar;
            }
        } else {
            next;
        }
    }
}
