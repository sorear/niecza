use 5.010;
use strict;
use warnings;
use utf8;

package CSharpBackend;

use Scalar::Util 'blessed';

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
our $nid = 0;
our @decls;
our @thaw;
our @cgs;
our %lpeers;
our %haslet;
our $classhow;
our $libmode;

sub gsym {
    my ($type, $desc) = @_;
    $desc =~ s/(\W)/"_" . ord($1)/eg;
    my $base = 'G' . ($nid++) . $desc;
    my $full = $unit->name . "." . $base . ':f,' . $type;
    wantarray ? ($full, $base) : $full;
}

my $cl_ty = 'DynMetaObject';
my $si_ty = 'SubInfo';

sub run {
    local $unit = shift;
    local $libmode = $unit->name ne 'MAIN';
    local %lpeers;
    local $nid = 0;
    local @thaw;
    local @decls;
    local @cgs;
    local $classhow;

    local $Metamodel::unit = $unit;

    # 0s just set up variables
    # 1s set up subs
    # 2s set up objects
    # 3s set up relationships

    $unit->visit_local_stashes(\&stash2) unless $libmode; #XXX weird timing

    $unit->visit_local_packages(\&pkg0);
    $unit->visit_local_subs_preorder(\&sub0);

    $unit->visit_local_subs_preorder(\&sub1);

    my $mod = '';
    $mod .= <<EOH ;
using Niecza;
using System;
using System.Collections.Generic;

public class ${\ $unit->name } {
EOH
    $mod .= <<EOM unless $libmode ;
    public static void Main() {
        Kernel.RunLoop(new SubInfo("boot", BOOT));
    }

EOM

    unless ($libmode) {
        $unit->visit_units_preorder(sub {
            $_->visit_local_packages(\&pkg2);
            $_->visit_local_subs_preorder(\&sub2);

            $unit->visit_local_stashes(\&stash3) if $_ == $unit;
            $_->visit_local_packages(\&pkg3);
            $_->visit_local_subs_preorder(\&sub3);

            return if $_->bottom_ref;

            my $s = $_->setting;
            my $m = $_->mainline;
            while ($s) {
                my $su = $unit->get_unit($s);
                push @thaw, CgOp::setindex("*resume_$s",
                    CgOp::getfield("lex", CgOp::callframe),
                    CgOp::newscalar(CgOp::rawsget($m->{peer}{ps})));
                $s = $su->setting;
                $m = $su->mainline;
            }
            push @thaw, CgOp::sink(CgOp::subcall(CgOp::rawsget($m->{peer}{ps})));
        });
        push @thaw, CgOp::return;

        push @cgs, CodeGen->new(csname => 'BOOT', name => 'BOOT',
            usednamed => 1, ops => CgOp::prog(@thaw))->csharp;
    }

    for (@decls) {
        /(?:.*?\.)?(.*):f,(.*)/;
        $mod .= "    public static $2 $1;\n";
    }

    $mod .= $_ for (@cgs);
    $mod .= "}\n";

    $unit->{mod} = $mod;
    $unit;
}

sub stash2 {
    my $p = $lpeers{$_} = gsym('IP6', join("_", @{ $_->path }));
    push @decls, $p;
    push @thaw, CgOp::rawsset($p, CgOp::fetch(CgOp::box(
            CgOp::rawsget('Kernel.StashP'),
            CgOp::rawnew('clr:Dictionary<string,BValue>'))));
    if (@{ $_->path } == 1 && $_->path->[0] =~ /^(?:GLOBAL|PROCESS)$/) {
        push @thaw, CgOp::rawsset('Kernel.' . ucfirst(lc($_->path->[0])) .
            'O', CgOp::rawsget($p));
    }
}

sub stash3 {
    my $p = $lpeers{$_};
    for my $k (sort keys %{ $_->zyg }) {
        my $ch = $_->zyg->{$k};
        my $bit;

        if (blessed($ch)) {
            $bit = CgOp::newscalar(CgOp::rawsget($lpeers{$ch}));
        } else {
            my $chd = $unit->deref($ch);
            if ($chd->isa('Metamodel::Package') &&
                    defined $chd->{peer}{what_var}) {
                $bit = CgOp::rawsget($chd->{peer}{what_var});
            }
        }

        next unless $bit;
        push @thaw, CgOp::bset(CgOp::rawscall('Kernel.PackageLookup',
                CgOp::rawsget($p), CgOp::clr_string($k)), $bit);
    }
}

# xxx check for SAFE::
my %loopbacks = (
    'MAny', 'Kernel.AnyMO',
    'MCallFrame', 'Kernel.CallFrameMO',
    'MGatherIterator', 'RxFrame.GatherIteratorMO',
    'MList', 'RxFrame.ListMO',
    'MMatch', 'RxFrame.MatchMO',
    'PAny', 'Kernel.AnyP',
    'PArray', 'Kernel.ArrayP',
    'PEMPTY', 'RxFrame.EMPTYP',
    'PHash', 'Kernel.HashP',
    'PStr', 'Kernel.StrP',
);

sub pkg0 {
    return unless $_->isa('Metamodel::Class') || $_->isa('Metamodel::Role')
        || $_->isa('Metamodel::ParametricRole');
    my $p   = $_->{peer}{mo} = gsym($cl_ty, $_->name);
    my $whv = $_->{peer}{what_var} = gsym('Variable', $_->name . '_WHAT');
    my $wh6 = $_->{peer}{what_ip6} = gsym('IP6', $_->name . '_WHAT');
    push @decls, $p, $whv, $wh6;
}

sub pkg2 {
    return unless $_->{peer};
    @_ = ($_->{peer}{mo}, $_->{peer}{what_ip6}, $_->{peer}{what_var});
    &pkg2_class if $_->isa('Metamodel::Class');
    &pkg2_role  if $_->isa('Metamodel::Role');
    &pkg2_prole if $_->isa('Metamodel::ParametricRole');
}

sub create_type_object {
    my $peer = shift;

    push @thaw, CgOp::rawsset($peer->{what_ip6},
        CgOp::rawnew('clr:DynObject', CgOp::rawsget($peer->{mo})));
    push @thaw, CgOp::setfield('slots',
        CgOp::cast('clr:DynObject', CgOp::rawsget($peer->{what_ip6})),
        CgOp::null('clr:object[]'));
    push @thaw, CgOp::setfield('typeObject', CgOp::rawsget($peer->{mo}),
        CgOp::rawsget($peer->{what_ip6}));
    push @thaw, CgOp::rawsset($peer->{what_var},
        CgOp::newscalar(CgOp::rawsget($peer->{what_ip6})));
}

sub pkg2_role {
    my ($p, $wh6, $whv) = @_;
    push @thaw, CgOp::rawsset($p, CgOp::rawnew("clr:$cl_ty",
            CgOp::clr_string($_->name)));
    push @thaw, CgOp::rawcall(CgOp::rawsget($p), 'FillRole',
        CgOp::rawnewarr('str', map { CgOp::clr_string($_) }
            @{ $_->attributes }),
        CgOp::rawnewarr('clr:DynMetaObject',
            map { CgOp::rawsget($unit->deref($_)->{peer}{mo}) }
                @{ $_->superclasses }),
        CgOp::rawnewarr('clr:DynMetaObject'));

    create_type_object($_->{peer});
}

sub pkg2_prole {
    my ($p, $wh6, $whv) = @_;
    push @thaw, CgOp::rawsset($p, CgOp::rawnew("clr:$cl_ty",
            CgOp::clr_string($_->name)));

    create_type_object($_->{peer});
}

sub pkg2_class {
    my ($p, $wh6, $whv) = @_;
    if ($unit->is_true_setting && ($_->name eq 'Scalar' ||
            $_->name eq 'Sub' || $_->name eq 'Stash')) {
        push @thaw, CgOp::rawsset($p,
            CgOp::rawsget("Kernel." . $_->name . "MO:f,$cl_ty"));
    } else {
        push @thaw, CgOp::rawsset($p, CgOp::rawnew("clr:$cl_ty",
                CgOp::clr_string($_->name)));
    }
    my $abase;
    for (@{ $_->linearized_mro }) {
        $abase += scalar @{ $unit->deref($_)->attributes };
    }
    push @thaw, CgOp::rawcall(CgOp::rawsget($p), 'FillClass',
        CgOp::rawnewarr('str', map { CgOp::clr_string($_) }
            @{ $_->attributes }),
        CgOp::rawnewarr('str', map { CgOp::clr_string($_) }
            map { @{ $unit->deref($_)->attributes } } @{ $_->linearized_mro }),
        CgOp::rawnewarr('clr:DynMetaObject',
            map { CgOp::rawsget($unit->deref($_)->{peer}{mo}) }
                @{ $_->superclasses }),
        CgOp::rawnewarr('clr:DynMetaObject',
            map { CgOp::rawsget($unit->deref($_)->{peer}{mo}) }
                @{ $_->linearized_mro }));

    create_type_object($_->{peer});

    push @thaw, CgOp::rawsset($loopbacks{'P' . $_->name}, CgOp::rawsget($wh6))
        if $loopbacks{'P' . $_->name};
    push @thaw, CgOp::rawsset($loopbacks{'M' . $_->name}, CgOp::rawsget($p))
        if $loopbacks{'M' . $_->name};
    $classhow = $wh6 if $_->name eq 'ClassHOW';
}

sub pkg3 {
    return unless $_->isa('Metamodel::Class') || $_->isa('Metamodel::Role');
    my $p   = $_->{peer}{mo};
    for my $m (@{ $_->methods }) {
        push @thaw, CgOp::rawcall(CgOp::rawsget($p),
            ($m->private ? 'AddPrivateMethod' : 'AddMethod'),
            CgOp::clr_string($m->name),
            CgOp::rawsget($unit->deref($m->body)->{peer}{ps}));
    }
    for my $k (sort keys %{ $_->multi_regex_lists }) {
        for my $b (@{ $_->multi_regex_lists->{$k} }) {
            push @thaw, CgOp::rawcall(CgOp::rawsget($p), 'AddMultiRegex',
                CgOp::clr_string($k),
                CgOp::rawsget($unit->deref($b)->{peer}{ps}));
        }
    }
    if ($classhow) {
        push @thaw, CgOp::setfield('how', CgOp::rawsget($p), CgOp::fetch(
                CgOp::box(CgOp::rawsget($classhow), CgOp::rawsget($p))));
    }
}

sub enter_code {
    my ($body) = @_;
    my @code;
    for my $ln (sort keys %{ $body->lexicals }) {
        my $lx = $body->lexicals->{$ln};

        next if $body->run_once && $body->spad_exists && !dynname($ln);

        if ($lx->isa('Metamodel::Lexical::SubDef')) {
            push @code, access_lex($body, $ln,
                CgOp::newscalar(CgOp::rawscall('Kernel.MakeSub',
                        CgOp::rawsget($lx->body->{peer}{si}),
                        CgOp::callframe)));
        } elsif ($lx->isa('Metamodel::Lexical::Simple')) {
            my $frag;
            next if $lx->noinit;
            if ($lx->hash || $lx->list) {
                # XXX should be SAFE::
                my $imp = $_->find_lex($lx->hash ? 'Hash' : 'Array')->path;
                my $var = $unit->deref($unit->get_stash_obj(@$imp))
                    ->{peer}{what_var};
                $frag = CgOp::methodcall(CgOp::rawsget($var), 'new');
            } else {
                $frag = CgOp::newblankrwscalar;
            }
            push @code, access_lex($body, $ln, $frag);
        }
    }
novars:

    if (defined $body->signature) {
        push @code, $body->signature->binder($body);
    }
    @code;
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
        if ($bp->run_once && !dynname($name)) {
            return $set_to ? CgOp::rawsset($lex->{peer}, $set_to) :
                CgOp::rawsget($lex->{peer});
        } elsif ((my $ix = $lex->{peer}) >= 0) {
            return $set_to ?
                CgOp->new(op => [ rtpadputi => $order, $ix ],
                    zyg => [ $set_to ]) :
                CgOp->new(op => [ rtpadgeti => 'Variable', $order, $ix ]);
        } else {
            return $set_to ?
                CgOp->new(op => [ rtpadput => $order, $name ],
                    zyg => [ $set_to ]) :
                CgOp->new(op => [ rtpadget => 'Variable', $order, $name ]);
        }
    } elsif ($lex->isa('Metamodel::Lexical::Stash')) {
        die "cannot rebind stashes" if $set_to;
        my $ref = $unit->get_stash_obj(@{ $lex->path });
        my $obj = $ref && $unit->deref($ref);
        return $obj->{peer} ? CgOp::rawsget($obj->{peer}{what_var}) :
            CgOp::null('var');
    } elsif ($lex->isa('Metamodel::Lexical::Common')) {
        return $set_to ?
            CgOp::bset(CgOp::rawsget($lex->{peer}), $set_to) :
            CgOp::bget(CgOp::rawsget($lex->{peer}));
    } elsif ($lex->isa('Metamodel::Lexical::SubImport')) {
        die "cannot rebind imported subs" if $set_to;
        return CgOp::newscalar(CgOp::rawsget($unit->deref($lex->ref)->{peer}{ps}));
    } else {
        die "unhandled $lex";
    }
}

sub resolve_lex {
    my ($body, $op) = @_;

    my ($opc, $arg, @rest) = @{ $op->op };
    if ($opc eq 'scopelex') {
        my $nn = access_lex($body, $arg, $op->zyg->[0]);
        #XXX
        %$op = %$nn;
        bless $op, ref($nn);

        resolve_lex($body, $_) for @{ $op->zyg };
    } elsif ($opc eq 'class_ref') {
        my $cl = (@rest > 1) ? $unit->deref([ @rest ]) :
            $unit->deref($unit->get_stash_obj(@{$body->find_lex(@rest)->path}));
        my $nn = CgOp::rawsget($cl->{peer}{$arg});
        %$op = %$nn;
        bless $op, ref($nn);
    } elsif ($opc eq 'let') {
        local $haslet{$arg} = 1;

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
        $ops = CgOp::prog(@enter, CgOp::sink($_->code->cgop($_)),
            CgOp::rawscall('Kernel.Take', CgOp::scopedlex('EMPTY')));
    } elsif ($_->parametric_role_hack) {
        my $obj = $unit->deref($_->parametric_role_hack);
        my @build;
        push @build, CgOp::rawcall(CgOp::letvar('!mo'), "FillRole",
            CgOp::rawnewarr('str', map { CgOp::clr_string($_) }
                @{ $obj->attributes }),
            CgOp::rawnewarr('clr:DynMetaObject',
                map { CgOp::rawsget($unit->deref($_)->{peer}{mo}) }
                @{ $obj->superclasses }),
            CgOp::rawnewarr('clr:DynMetaObject'));
        for my $m (@{ $obj->methods }) {
            push @build, CgOp::rawcall(CgOp::letvar('!mo'),
                ($m->[2] ? 'AddPrivateMethod' : 'AddMethod'),
                CgOp::clr_string($m->[0]),
                CgOp::fetch(CgOp::scopedlex($m->[1])));
        }
        for my $k (sort keys %{ $obj->multi_regex_lists }) {
            for my $b (@{ $obj->multi_regex_lists->{$k} }) {
                push @build, CgOp::rawcall(CgOp::letvar('!mo'),
                    'AddMultiRegex', CgOp::clr_string($k),
                    CgOp::fetch(CgOp::scopedlex($b)));
            }
        }
        $ops = CgOp::prog(@enter, CgOp::sink($_->code->cgop($_)),
            CgOp::letn("!mo", CgOp::rawnew('clr:DynMetaObject',
                    CgOp::clr_string($obj->name)),
                "!to", CgOp::rawnew('clr:DynObject', CgOp::letvar('!mo')),
                @build,
                CgOp::setfield('slots', CgOp::letvar('!to'),
                    CgOp::null('clr:object[]')),
                CgOp::setfield('typeObject', CgOp::letvar('!mo'),
                    CgOp::letvar('!to')),
                CgOp::return(CgOp::newscalar(CgOp::letvar('!to')))));
    } elsif ($_->returnable && defined($_->signature)) {
        $ops = CgOp::prog(@enter,
            CgOp::return(CgOp::span("rstart", "rend",
                    $_->code->cgop($_))),
            CgOp::ehspan(4, undef, 0, "rstart", "rend", "rend"));
    } else {
        $ops = CgOp::prog(@enter, CgOp::return($_->code->cgop($_)));
    }

    local %haslet;
    resolve_lex($_, $ops);
    CodeGen->new(csname => $_->{peer}{cbase}, name => ($_->name eq 'ANON' ?
            $_->{peer}{cbase} : $_->name), ops => $ops,
        usednamed => $_->{peer}{uname}, minlets => $_->{peer}{nlexn});
}

sub dynname { $_[0] =~ /^.?[*?]/ }
# lumped under a sub are all the static-y lexicals
# protopads and proto-sub-instances need to exist early because methods, in
# particular, bind to them
# note: preorder
sub sub0 {
    my $node = ($_->{peer} = {});
    push @decls, ($node->{si} = gsym($si_ty, $_->name));
    push @decls, ($node->{ps} = gsym('IP6', $_->name . 'PS'))
        if !$_->outer || $_->outer->spad_exists;
    push @decls, ($node->{pp} = gsym('Frame', $_->name . 'PP'))
        if $_->spad_exists;
    @$node{'cref','cbase'} = gsym('DynBlockDelegate', $_->name . 'C');

    my ($nlexn, $uname) = (0,0);

    for my $ln (sort keys %{ $_->lexicals }) {
        my $lx = $_->lexicals->{$ln};

        if ($lx->isa('Metamodel::Lexical::Common')) {
            my $bv = $lx->{peer} = gsym('BValue', $lx->name);
            push @decls, $bv;
        }

        if ($lx->isa('Metamodel::Lexical::SubDef') ||
                $lx->isa('Metamodel::Lexical::Simple')) {
            if (dynname($ln)) {
                $lx->{peer} = -1;
                $uname = 1;
            } elsif ($_->run_once) {
                push @decls, ($lx->{peer} = gsym('Variable', $ln));
            } else {
                $lx->{peer} = ($nlexn++);
            }
        }
    }

    @$node{'nlexn', 'uname'} = ($nlexn, $uname);
}

sub sub1 {
    my $node = $_->{peer};
    my $si = $node->{si};

    my $cg = codegen_sub($_);
    $node->{sictor} = [ $cg->subinfo_ctor_args(
            ($_->outer ? CgOp::rawsget($_->outer->{peer}{si}) :
                CgOp::null('clr:SubInfo')),
            ($_->ltm ? RxOp::lad2cgop($_->ltm) : CgOp::null('clr:LAD'))) ];

    push @cgs, $cg->csharp;
}

sub sub2 {
    my $node = $_->{peer};
    my $si = $node->{si};

    push @thaw, CgOp::rawsset($si, CgOp::rawnew("clr:$si_ty", @{ $node->{sictor} }));

    if ($_->class ne 'Sub') {
        my $cl = $unit->deref($unit->get_stash_obj(@{ $_->find_lex_pkg($_->class) }));
        push @thaw, CgOp::setfield('mo', CgOp::rawsget($si), CgOp::rawsget($cl->{peer}{mo}));
    }

    my $pp = $node->{pp};
    if ($pp) {
        push @thaw, CgOp::rawsset($pp, CgOp::rawnew('clr:Frame',
                CgOp::null('clr:Frame'), (!$_->outer ? CgOp::null('clr:Frame') :
                    CgOp::rawsget($_->outer->{peer}{pp})),
                CgOp::rawsget($si)));
        if ($node->{uname}) {
            push @thaw, CgOp::setfield('lex', CgOp::rawsget($pp),
                CgOp::rawnew('clr:Dictionary<string,object>'));
        }
        if ($node->{nlexn} > 4) {
            push @thaw, CgOp::setfield('lexn', CgOp::rawsget($pp),
                CgOp::rawnewzarr('clr:object', CgOp::int($node->{nlexn} - 4)));
        }
    }

    my $ps = $node->{ps};
    if ($ps) {
        push @thaw, CgOp::rawsset($ps, CgOp::rawscall('Kernel.MakeSub',
                CgOp::rawsget($si), !$_->outer ? CgOp::null('clr:Frame') :
                    CgOp::rawsget($_->outer->{peer}{pp})));
        if ($_->parametric_role_hack) {
            push @thaw, CgOp::rawcall(
                CgOp::rawsget($unit->deref($_->parametric_role_hack)
                    ->{peer}{mo}), "FillParametricRole", CgOp::rawsget($ps));
        }
    }
}

# use for SubDef / Simple only
sub protolset {
    my ($body, $lname, $lex, $frag) = @_;

    if ($body->run_once && !dynname($lname)) {
        push @thaw, CgOp::rawsset($lex->{peer}, $frag);
    } elsif ((my $ix = $lex->{peer}) >= 4) {
        push @thaw, CgOp::setindex(CgOp::int($ix - 4),
            CgOp::getfield('lexn', CgOp::rawsget($body->{peer}{pp})),
            $frag);
    } elsif ($ix >= 0) {
        push @thaw, CgOp::setfield("lex$ix",
            CgOp::rawsget($body->{peer}{pp}), $frag);
    } else {
        push @thaw, CgOp::setindex($lname,
            CgOp::getfield('lex', CgOp::rawsget($body->{peer}{pp})),
            $frag);
    }
}


sub sub3 {
    for my $ln (sort keys %{ $_->lexicals }) {
        my $lx = $_->lexicals->{$ln};
        my $frag;

        if ($lx->isa('Metamodel::Lexical::Common')) {
            my $stash = $unit->get_stash(@{ $lx->path });
            if (!$lpeers{$stash}) {
                Carp::confess("Peer for " . join("::", @{ $stash->path }) . " has gone missing!");
            }
            push @thaw, CgOp::rawsset($lx->{peer},
                CgOp::rawscall('Kernel.PackageLookup',
                    CgOp::rawsget($lpeers{$stash}),
                    CgOp::clr_string($lx->name)));
        } elsif ($lx->isa('Metamodel::Lexical::SubDef')) {
            next unless $_->spad_exists;
            protolset($_, $ln, $lx,
                CgOp::newscalar(CgOp::rawsget($lx->body->{peer}{ps})));
        } elsif ($lx->isa('Metamodel::Lexical::Simple')) {
            next unless $_->spad_exists;
            if ($lx->hash || $lx->list) {
                # XXX should be SAFE::
                my $imp = $_->find_lex($lx->hash ? 'Hash' : 'Array')->path;
                my $var = $unit->deref($unit->get_stash_obj($$imp))
                    ->{peer}{what_var};
                $frag = CgOp::methodcall(CgOp::rawsget($var), 'new');
            } else {
                $frag = CgOp::newblankrwscalar;
            }
            protolset($_, $ln, $lx, $frag);
        }
    }
}
