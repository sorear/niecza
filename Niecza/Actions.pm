package Niecza::Actions;
use 5.010;
use strict;
use warnings;

use Op;
use RxOp;
use Body;
use Unit;
use Sig;

use Try::Tiny;

our $AUTOLOAD;
my %carped;
sub AUTOLOAD {
    my ($cl, $M) = @_;
    if ($AUTOLOAD =~ /^Niecza::Actions::(.*)__S_\d\d\d(.*)$/) {
        my ($cat, $spec) = ($1, $2);
        my $m = "${cat}__S_$spec";
        my $a = "${cat}__S_ANY";
        if ($cl->can($m) || !$cl->can($a)) {
            return $cl->$m($M);
        } else {
            return $cl->$a($M, $spec);
        }
    }
    $M->sorry("Action method $AUTOLOAD not yet implemented") unless $carped{$AUTOLOAD}++;
}

sub node { my ($M) = @_;
    file => $::FILE->{name}, line => $M->lineof($M->pos)
}

sub ws { }
sub normspace { }
sub vws { }
sub unv { }
sub begid { }
sub comment { }
sub comment__S_Sharp { }
sub spacey { }
sub unspacey { }
sub nofun { }
sub curlycheck { }
sub pod_comment { }
sub infixstopper { }

sub decint { my ($cl, $M) = @_;
    $M->{_ast} = eval $M->Str; # XXX use a real string parser
}

sub integer { my ($cl, $M) = @_;
    $M->{_ast} =
        ($M->{decint} // $M->{octint} // $M->{hexint} // $M->{binint})->{_ast};
}

sub number { my ($cl, $M) = @_;
    my $child = $M->{integer} // $M->{dec_number} // $M->{rad_number};
    $M->{_ast} = $child ? $child->{_ast} :
        ($M->Str eq 'NaN') ? (1e99999/1e99999) : (1e99999);
}

# Value :: Op
sub value { }
sub value__S_number { my ($cl, $M) = @_;
    # TODO: Implement the rest of the numeric hierarchy once MMD exists
    $M->{_ast} = Op::Num->new(node($M), value => $M->{number}{_ast});
}

sub value__S_quote { my ($cl, $M) = @_;
    $M->{_ast} = $M->{quote}{_ast};
}

sub ident { my ($cl, $M) = @_;
    $M->{_ast} = $M->Str;
}

sub identifier { my ($cl, $M) = @_;
    $M->{_ast} = $M->Str;
}

# Either String Op
sub morename { my ($cl, $M) = @_;
    $M->{_ast} = $M->{identifier}[0] ? $M->{identifier}[0]{_ast} :
        $M->{EXPR}[0]{_ast};
}

# { dc: Bool, names: [Either String Op] }
sub name { my ($cl, $M) = @_;
    my @names = map { $_->{_ast} } @{ $M->{morename} };
    unshift @names, $M->{identifier}{_ast} if $M->{identifier};
    $M->{_ast} = { dc => !($M->{identifier}), names => \@names };
}

sub longname {} # look at the children yourself
sub deflongname {}

sub mangle_longname { my ($cl, $M, $single) = @_;
    if ($M->{name}{_ast}{dc}) {
       $M->sorry('Leading double colons not yet supported');
       return "";
    }

    if ($single && @{ $M->{name}{_ast}{names} } > 1) {
        $M->sorry("Multipart names not yet supported for $single");
        return "";
    }

    my @ns = @{ $M->{name}{_ast}{names} };
    my $n = pop @ns;

    for my $cp (@{ $M->{colonpair} }) {
        my $k = $cp->{k};
        if (ref $cp->{v}) {
            $n .= ":" . $cp->{k};
            $n .= $cp->{v}{qpvalue} // do {
                $M->sorry("Invalid colonpair used as name extension");
                "";
            }
        } else {
            # STD seems to think term:name is term:sym<name>.  Needs speccy
            # clarification.
            $M->sorry("Boolean colonpairs as name extensions NYI");
        }
    }

    $single ? $n : ($n, @ns);
}

sub subshortname { my ($cl, $M) = @_;
    if (@{ $M->{colonpair} }) {
        $M->sorry("Colonpair subshortnames NYI");
        return;
    }

    $M->{_ast} = $M->{desigilname}{_ast};
}

sub sublongname { my ($cl, $M) = @_;
    if (@{ $M->{sigterm} }) {
        $M->sorry("Sigterm sublongnames NYI");
        return;
    }

    $M->{_ast} = $M->{subshortname}{_ast};
}

sub desigilname { my ($cl, $M) = @_;
    if ($M->{variable}) {
        $M->sorry("Truncated contextualizer syntax NYI");
        return;
    }

    $M->{_ast} = [ $cl->mangle_longname($M->{longname}) ];
}

sub stopper { }

# quote :: Op
sub quote {}

sub quote__S_Double_Double { my ($cl, $M) = @_;
    $M->{_ast} = $M->{nibble}{_ast};
}

sub quote__S_Single_Single { my ($cl, $M) = @_;
    $M->{_ast} = $M->{nibble}{_ast};
}

sub quote__S_qq { my ($cl, $M) = @_;
    $M->{_ast} = $M->{quibble}{_ast};
}

sub quote__S_q { my ($cl, $M) = @_;
    $M->{_ast} = $M->{quibble}{_ast};
}

sub quote__S_Q { my ($cl, $M) = @_;
    $M->{_ast} = $M->{quibble}{nibble}{_ast};
}

sub quote__S_Slash_Slash { my ($cl, $M) = @_;
    my $slot = $cl->gensym;
    # TODO should be a real pass.
    $M->{_ast} = Op::CallMethod->new(node($M), name => 'bless',
        receiver => Op::Lexical->new(name => 'Regex'),
        positionals => [
            RxOp::Export->new(zyg => [$M->{nibble}{_ast}])->closure ]);
}

# :: RxOp
sub atom { my ($cl, $M) = @_;
    if ($M->{metachar}) {
        $M->{_ast} = $M->{metachar}{_ast};
    } else {
        $M->{_ast} = RxOp::String->new(text => $M->Str,
            igcase => $::RX{i}, igmark => $::RX{a});
    }
}

sub quantified_atom { my ($cl, $M) = @_; # :: RxOp
    my $atom = $M->{atom}{_ast};
    my $ns   = $M->{normspace}[0];
    my $q    = $M->{quantifier}[0] ? $M->{quantifier}[0]{_ast} : undef;

    if (!$q) {
        $M->{_ast} = $atom;
    } elsif ($q->{simple}) {
        $M->{_ast} = RxOp::Quantifier->new(type => $q->{simple},
            zyg => [$atom]);
    } else {
        $M->sorry("Unhandled quantifier " . $M->{quantifier}[0]->Str);
    }
}

# :: Context hash interpreted by quantified_atom
sub quantifier {}
sub quantifier__S_Star { my ($cl, $M) = @_;
    $M->{_ast} = { simple => '*' };
}
sub quantifier__S_Plus { my ($cl, $M) = @_;
    $M->{_ast} = { simple => '+' };
}
sub quantifier__S_Question { my ($cl, $M) = @_;
    $M->{_ast} = { simple => '?' };
}

sub quantmod { my ($cl, $M) = @_;
    if ($M->Str ne '') {
        $M->sorry('Quantmods NYI');
        return;
    }
}

sub quant_atom_list { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::Sequence->new(zyg =>
        [ map { $_->{_ast} } @{ $M->{quantified_atom} } ]);
}

sub metachar {}
sub metachar__S_sigwhite { my ($cl, $M) = @_;
    $M->{_ast} = $::RX{s} ? RxOp::Sigspace->new : RxOp::Sequence->new;
}

sub metachar__S_unsp { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::Sequence->new;
}

sub metachar__S_Cur_Ly { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::VoidBlock->new(block => $M->{embeddedblock}{_ast});
}

sub metachar__S_mod { my ($cl, $M) = @_;
    # most of these have only parse-time effects
    $M->{_ast} = $M->{mod_internal}{_ast} // RxOp::Sequence->new;
}

sub metachar__S_ColonColon { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::CutLTM->new;
}

sub metachar__S_ColonColonColon { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::CutRule->new;
}

sub metachar__S_Bra_Ket { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::ConfineLang->new(zyg => [$M->{nibbler}{_ast}]);
}

sub metachar__S_Paren_Thesis { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::Capture->new(zyg => [
            RxOp::ConfineLang->new(zyg => [$M->{nibbler}{_ast}])]);
}

sub metachar__S_LtParen { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::MarkFrom->new;
}

sub metachar__S_ThesisGt { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::MarkTo->new;
}

sub metachar__S_LtLt { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::LWB->new;
}

sub metachar__S_GtGt { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::RWB->new;
}

sub metachar__S_Fre { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::LWB->new;
}

sub metachar__S_Nch { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::RWB->new;
}

sub metachar__S_qw { my ($cl, $M) = @_;
    $M->sorry("< > splitting NYI");
}

sub metachar__S_Lt_Gt { my ($cl, $M) = @_;
    $M->{_ast} = $M->{assertion}{_ast};
}

sub metachar__S_Back { my ($cl, $M) = @_;
    $M->{_ast} = $M->{backslash}{_ast};
}

sub metachar__S_Dot { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::Any->new;
}

sub metachar__S_Caret { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::StrStart->new;
}

sub metachar__S_CaretCaret { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::LineStart->new;
}

sub metachar__S_Dollar { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::StrEnd->new;
}

sub metachar__S_DollarDollar { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::LineEnd->new;
}

sub metachar__S_Single_Single { my ($cl, $M) = @_;
    if (! $M->{quote}{_ast}->isa('Op::StringLiteral')) {
        $M->sorry("Interpolating strings in regexes NYI");
        return;
    }
    $M->{_ast} = RxOp::String->new(text => $M->{quote}{_ast}->text,
        igcase => $::RX{i}, igmark => $::RX{a});
}

sub metachar__S_Double_Double { my ($cl, $M) = @_;
    if (! $M->{quote}{_ast}->isa('Op::StringLiteral')) {
        $M->sorry("Interpolating strings in regexes NYI");
        return;
    }
    $M->{_ast} = RxOp::String->new(text => $M->{quote}{_ast}->text,
        igcase => $::RX{i}, igmark => $::RX{a});
}

# These have effects only in the parser, so undef ast is correct.
sub mod_value {}
sub mod_internal {}
sub mod_internal__S_Coloni {}
sub mod_internal__S_ColonBangi {}
sub mod_internal__S_ColoniParen_Thesis {}
sub mod_internal__S_Colon0i {}
sub mod_internal__S_Colons {}
sub mod_internal__S_ColonBangs {}
sub mod_internal__S_ColonsParen_Thesis {}
sub mod_internal__S_Colon0s {}
sub mod_internal__S_Colonr {}
sub mod_internal__S_ColonBangr {}
sub mod_internal__S_ColonrParen_Thesis {}
sub mod_internal__S_Colon0r {}
sub mod_internal__S_Colona {}
sub mod_internal__S_ColonBanga {}
sub mod_internal__S_ColonaParen_Thesis {}
sub mod_internal__S_Colon0a {}

sub nibbler { my ($cl, $M) = @_;
    if ($M->isa('STD::Regex')) {
        $M->{_ast} = $M->{EXPR}{_ast};
    } elsif ($M->isa('Niecza::Grammar::CgOp')) {
        # XXX We don't interpret the code, so we can't tell if it's actually
        # using variables, but still, it probably is.
        for my $k (keys %$::CURLEX) {
            $::CURLEX->{$k}{used} = 1 if $k =~ /^[\@\%\&\$]\w/;
        }
        $M->{_ast} = Op::CgOp->new(node($M), op => $M->{cgexp}{_ast});
    } else {
        # garden variety nibbler
        my $str = "";
        for my $n (@{ $M->{nibbles} }) {
            if ($n->isa('Str')) {
                $str .= $n->{TEXT};
            } else {
                $M->sorry("Non-literal contents of strings NYI");
            }
        }
        $M->{_ast} = Op::StringLiteral->new(node($M), text => $str);
    }
}

sub circumfix { }
sub circumfix__S_Lt_Gt { my ($cl, $M) = @_;
    my $sl = $M->{nibble}{_ast};

    if (!$sl->isa('Op::StringLiteral') || ($sl->text =~ /\S\s\S/)) {
        $M->sorry("Word splitting NYI");
        return;
    }

    my ($t) = $sl->text =~ /^\s*(.*?)\s*$/;

    $M->{_ast} = Op::StringLiteral->new(node($M), text => $t);
    $M->{qpvalue} = '<' . $t . '>';
}
sub circumfix__S_LtLt_GtGt { goto &circumfix__S_Lt_Gt }

sub circumfix__S_Paren_Thesis { my ($cl, $M) = @_;
    my @kids = grep { defined } @{ $M->{semilist}{_ast} };
    if (@kids == 1 && $kids[0]->isa('Op::WhateverCode')) {
        # XXX in cases like * > (2 + *), we *don't* want the parens to disable
        # syntactic specialization, since they're required for grouping
        $M->{_ast} = $kids[0];
    } else {
        $M->{_ast} = Op::StatementList->new(node($M), children => 
            [ map { Op::Paren->new(inside => $_) } @kids ]);
    }
}

sub circumfix__S_Cur_Ly { my ($cl, $M) = @_;
    $M->{pblock}{_ast}->type('bare');
    $M->{_ast} = Op::BareBlock->new(node($M), var => $cl->gensym,
        body => $M->{pblock}{_ast});
}

sub infixish { my ($cl, $M) = @_;
    $M->sorry("Metaoperators NYI") if $M->{infix_postfix_meta_operator}[0];
    $M->sorry("Adverbs NYI") if $M->{colonpair};
}
sub INFIX { my ($cl, $M) = @_;
    my $s = '&infix:<' . $M->{infix}{sym} . '>';
    my ($st,$l,$r) = $cl->whatever_precheck($s, $M->{left}{_ast},
        $M->{right}{_ast});

    if ($s eq '&infix:<:=>') { #XXX macro
        $M->{_ast} = Op::Bind->new(node($M), lhs => $l, rhs => $r,
            readonly => 0);
    } elsif ($s eq '&infix:<?? !!>') { # XXX macro
        $M->{_ast} = Op::Conditional->new(node($M), check => $l,
            true => $M->{middle}{_ast}, false => $r);
    } elsif ($s eq '&infix:<,>') {
        #XXX STD bug causes , in setting to be parsed as left assoc
        my @r;
        push @r, $l->splittable_parcel ? @{ $l->positionals } : ($l);
        push @r, $r->splittable_parcel ? @{ $r->positionals } : ($r);
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => Op::Lexical->new(name => '&infix:<,>'),
            positionals => \@r);
    } else {
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => Op::Lexical->new(node($M), name => $s),
            positionals => [ $l, $r ]);

        if ($s eq '&infix:<=>' && $l->isa('Op::Lexical') && $l->state_decl) {
            # Assignments (and assign metaops, but we don't do that yet) to has
            # and state declarators are rewritten into an appropriate phaser
            my $cv = $cl->gensym;
            $M->{_ast} = Op::StatementList->new(node($M), children => [
                Op::Start->new(condvar => $cv, body => $M->{_ast}),
                Op::Lexical->new(name => $l->name)]);
        }
    }
    $M->{_ast} = $cl->whatever_postcheck($M, $st, $M->{_ast});
}

sub CHAIN { my ($cl, $M) = @_;
    my $op = '&infix:<' . $M->{chain}[1]{sym} . '>';
    if (@{ $M->{chain} } != 3) {
        $M->sorry('Chaining not yet implemented');
        return;
    }

    my ($st, @args) = $cl->whatever_precheck($op, $M->{chain}[0]{_ast},
        $M->{chain}[2]{_ast});

    $M->{_ast} = $cl->whatever_postcheck($M, $st, Op::CallSub->new(node($M),
        invocant => Op::Lexical->new(name => $op), positionals => [ @args ]));
}

my %loose2tight = (
    '&&' => '&&', '||' => '||', '//' => '//', 'andthen' => 'andthen',
    'orelse' => '//', 'and' => '&&', 'or' => '||',
);
sub LIST { my ($cl, $M) = @_;
    # STD guarantees that all elements of delims have the same sym
    # the last item may have an ast of undef due to nulltermish
    my $op  = $M->{delims}[0]{sym};
    my ($st, @pos) = $cl->whatever_precheck("&infix:<$op>",
        grep { defined } map { $_->{_ast} } @{ $M->{list} });

    if ($loose2tight{$op}) {
        $M->{_ast} = Op::ShortCircuit->new(node($M), kind => $loose2tight{$op},
            args => \@pos);
    } else {
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => Op::Lexical->new(name => "&infix:<$op>"),
            positionals => \@pos);
    }
    $M->{_ast} = $cl->whatever_postcheck($M, $st, $M->{_ast});
}

sub POSTFIX { my ($cl, $M) = @_;
    my $op = $M->{_ast};
    my ($st, $arg) = $cl->whatever_precheck('', $M->{arg}{_ast});
    if ($op->{postfix}) {
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => Op::Lexical->new(name => '&postfix:<' . $op->{postfix} . '>'),
            positionals => [ $arg ]);
    } elsif ($op->{name} && $op->{name} =~ /^(?:HOW|WHAT)$/) {
        if ($op->{args}) {
            $M->sorry("Interrogative operator " . $op->{name} .
                " does not take arguments");
            return;
        }
        $M->{_ast} = Op::Interrogative->new(node($M),
            receiver => $arg,
            name => $op->{name});
    } elsif ($op->{metamethod}) {
        $M->{_ast} = Op::CallMetaMethod->new(node($M),
            receiver => $arg,
            name => $op->{metamethod},
            positionals => $op->{args} // []);
    } elsif ($op->{name}) {
        $M->{_ast} = Op::CallMethod->new(node($M),
            receiver => $arg,
            name => $op->{name},
            positionals => $op->{args} // []);
    } elsif ($op->{postcall}) {
        if (@{ $op->{postcall} } > 1) {
            $M->sorry("Slicels NYI");
            return;
        }
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => $arg,
            positionals => ($op->{postcall}[0] // []));
    } else {
        $M->sorry("Unhandled postop type");
    }
    $M->{_ast} = $cl->whatever_postcheck($M, $st, $M->{_ast});
}

sub PREFIX { my ($cl, $M) = @_;
    my $op = '&prefix:<' . $M->{sym} . '>';
    my ($st, $arg) = $cl->whatever_precheck($op, $M->{arg}{_ast});
    $M->{_ast} = $cl->whatever_postcheck($M, $st, Op::CallSub->new(node($M),
        invocant => Op::Lexical->new(name => $op),
        positionals => [ $M->{arg}{_ast} ]));
}

# infix et al just parse the operator itself
sub infix { }
sub infix__S_ANY { }

sub prefix { }
sub prefix__S_ANY { }

sub postfix { }
sub postfix__S_ANY { }

sub postcircumfix { }
sub postcircumfix__S_Paren_Thesis { my ($cl, $M) = @_;
    $M->{_ast} = { postcall => $M->{semiarglist}{_ast} };
}

sub postop { my ($cl, $M) = @_;
    $M->{_ast} = $M->{postcircumfix} ? $M->{postcircumfix}{_ast} :
        { postfix => $M->{sym} };
}
sub POST { my ($cl, $M) = @_;
    $M->{_ast} = $M->{dotty}{_ast} if $M->{dotty};
    $M->{_ast} = $M->{privop}{_ast} if $M->{privop};
    $M->{_ast} = $M->{postop}{_ast} if $M->{postop};
}

sub PRE { }

sub methodop { my ($cl, $M) = @_;
    my %r;
    $r{name}  = $cl->mangle_longname($M->{longname}, "method call") if $M->{longname};
    $r{quote} = $M->{quote}{_ast} if $M->{quote};
    $r{ref}   = $cl->do_variable_reference($M, $M->{variable}{_ast})
        if $M->{variable};

    $r{args}  = $M->{args}[0]{_ast}[0] if $M->{args}[0];
    $r{args}  = $M->{arglist}[0]{_ast} if $M->{arglist}[0];

    $M->{_ast} = \%r;
}

sub dottyop { my ($cl, $M) = @_;
    if ($M->{colonpair}) {
        $M->sorry("Colonpair dotties NYI");
        return;
    }

    $M->{_ast} = $M->{methodop}{_ast} if $M->{methodop};
    $M->{_ast} = { postfix => $M->{postop}{_ast} } if $M->{postop};
}

sub privop { my ($cl, $M) = @_;
    $M->{_ast} = { %{ $M->{methodop}{_ast} }, private => 1 };
}

sub dotty { }
sub dotty__S_Dot { my ($cl, $M) = @_;
    $M->{_ast} = $M->{dottyop}{_ast};
}

sub dotty__S_DotStar { my ($cl, $M) = @_;
    if ($M->{sym} eq '.^' && $M->{dottyop}{_ast}{name}) {
        $M->{_ast} = { metamethod => $M->{dottyop}{_ast}{name},
                       args => $M->{dottyop}{_ast}{args} };
    } else {
        $M->sorry('NYI dottyop form ' . $M->{sym});
    }
}

sub coloncircumfix { my ($cl, $M) = @_;
    $M->{_ast} = $M->{circumfix}{_ast};
    $M->{qpvalue} = $M->{circumfix}{qpvalue};
}

sub colonpair { }

my %_nowhatever = (map { $_, 1 } ('&infix:<,>', '&infix:<..>', '&infix:<...>',
    '&infix:<=>', '&infix:<xx>'));
sub whatever_precheck { my ($cl, $op, @args) = @_;
    return ([], @args) if $_nowhatever{$op};
    my @vars;
    for (@args) {
        Carp::confess("invalid undef here") if !$_;
        if ($_->isa('Op::Whatever')) {
            push @vars, $_->slot;
            $_ = Op::Lexical->new(name => $_->slot);
        } elsif ($_->isa('Op::WhateverCode')) {
            push @vars, @{ $_->vars };
            $_ = $_->ops;
        }
    }
    (\@vars, @args);
}

sub whatever_postcheck { my ($cl, $M, $st, $term) = @_;
    if (@$st) {
        return Op::WhateverCode->new(ops => $term, vars => $st,
            slot => $cl->gensym, node($M));
    } else {
        return $term;
    }
}

# term :: Op
sub term { }

sub term__S_value { my ($cl, $M) = @_;
    $M->{_ast} = $M->{value}{_ast};
}

sub term__S_name { my ($cl, $M) = @_;
    my ($id, @pkg) = $cl->mangle_longname($M->{longname});

    if ($M->{postcircumfix}[0] || $M->{args}) {
        $M->sorry("Unsupported form of term:name");
        return;
    }

    if (@pkg) {
        $M->{_ast} = Op::PackageVar->new(node($M), name => $id,
            slot => $cl->gensym, path => \@pkg);
    } else {
        $M->{_ast} = Op::Lexical->new(node($M), name => $id);
    }
}

sub term__S_identifier { my ($cl, $M) = @_;
    my $id  = $M->{identifier}{_ast};
    my $sal = $M->{args}{_ast} // [];  # TODO: support zero-D slicels

    if (@$sal > 1) {
        $M->sorry("Slicel lists are NYI");
        return;
    }

    if ($M->is_name($M->{identifier}->Str)) {
        $M->{_ast} = Op::Lexical->new(node($M), name => $id);
        return;
    }

    my $args = $sal->[0] // [];

    $M->{_ast} = Op::CallSub->new(node($M),
        invocant => Op::Lexical->new(name => '&' . $id),
        positionals => $args);
}

sub term__S_self { my ($cl, $M) = @_;
    $M->{_ast} = Op::Lexical->new(node($M), name => 'self');
}

sub term__S_circumfix { my ($cl, $M) = @_;
    $M->{_ast} = $M->{circumfix}{_ast};
}

sub term__S_scope_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{scope_declarator}{_ast};
}

sub term__S_multi_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{multi_declarator}{_ast};
}

sub term__S_package_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_declarator}{_ast};
}

sub term__S_routine_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{routine_declarator}{_ast};
}

sub term__S_regex_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{regex_declarator}{_ast};
}

sub term__S_type_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{type_declarator}{_ast};
}

sub term__S_dotty { my ($cl, $M) = @_;
    $M->{_ast} = $M->{dotty}{_ast};
}

sub term__S_capterm { my ($cl, $M) = @_;
    $M->{_ast} = $M->{capterm}{_ast};
}

sub term__S_sigterm { my ($cl, $M) = @_;
    $M->{_ast} = $M->{sigterm}{_ast};
}

sub term__S_statement_prefix { my ($cl, $M) = @_;
    $M->{_ast} = $M->{statement_prefix}{_ast};
}

sub term__S_variable { my ($cl, $M) = @_;
    $M->{_ast} = $cl->do_variable_reference($M, $M->{variable}{_ast});
}

sub term__S_DotDotDot { my ($cl, $M) = @_;
    $M->{_ast} = Op::Yada->new(node($M), kind => '...');
}

sub term__S_BangBangBang { my ($cl, $M) = @_;
    $M->{_ast} = Op::Yada->new(node($M), kind => '!!!');
}

sub term__S_QuestionQuestionQuestion { my ($cl, $M) = @_;
    $M->{_ast} = Op::Yada->new(node($M), kind => '???');
}

sub term__S_lambda { my ($cl, $M) = @_;
    $M->{pblock}{_ast}->type('pointy');
    $M->{_ast} = $cl->block_to_closure($M, $M->{pblock}{_ast});
}

sub term__S_Star { my ($cl, $M) = @_;
    $M->{_ast} = Op::Whatever->new(node($M), slot => $cl->gensym);
}

sub do_variable_reference { my ($cl, $M, $v) = @_;
    my $sl = $v->{sigil} . $v->{twigil} . $v->{name};

    if (@{ $v->{rest} } && $v->{twigil} =~ /[*=~?^:]/) {
        $M->sorry("Twigil " . $v->{twigil} . " cannot be used with " .
            "qualified names");
        return;
    }

    given ($v->{twigil}) {
        when ('!') {
            if (@{ $v->{rest} }) {
                $M->sorry('$!Foo::bar syntax NYI');
                return;
            }

            return Op::GetSlot->new(node($M), name => $v->{name},
                object => Op::Lexical->new(name => 'self'));
        }
        when ('.') {
            if (@{ $v->{rest} }) {
                $M->sorry('$.Foo::bar syntax NYI');
                return;
            }

            return Op::CallMethod->new(node($M), name => $v->{name},
                receiver => Op::Lexical->new(name => 'self'));
        }
        # These are just ordinary lexicals at run time
        when ({ '^' => 1, ":" => 1, "?" => 1}) {
            return Op::Lexical->new(node($M), name => $sl);
        }
        when ('*') {
            return Op::ContextVar->new(node($M), name => $sl);
        }
        when ('') {
            if (@{ $v->{rest} }) {
                return Op::PackageVar->new(path => $v->{rest}, name => $sl,
                    slot => $cl->gensym, node($M));
            } else {
                return Op::Lexical->new(node($M), name => $sl);
            }
        }
        default {
            $M->sorry("Unhandled reference twigil " . $v->{twigil});
        }
    }
}

sub variable { my ($cl, $M) = @_;
    my $sigil = $M->{sigil} ? $M->{sigil}->Str : substr($M->Str, 0, 1);
    my $twigil = $M->{twigil}[0] ? $M->{twigil}[0]{sym} : '';

    my ($name, @rest);
    if ($M->{desigilname}) {
        ($name, @rest) = @{ $M->{desigilname}{_ast} };
    } elsif ($M->{sublongname}) {
        ($name, @rest) = @{ $M->{sublongname}{_ast} };
    } else {
        $M->sorry("Non-simple variables NYI");
        return;
    }

    $M->{_ast} = {
        sigil => $sigil, twigil => $twigil, name => $name, rest => \@rest
    };
}

sub param_sep {}

# :: Sig::Target
sub param_var { my ($cl, $M) = @_;
    if ($M->{signature}) {
        $M->sorry('Sub-signatures NYI');
        return;
    }
    my $twigil = $M->{twigil}[0] ? $M->{twigil}[0]->Str : '';
    my $sigil = $M->{sigil}->Str;
    if ($twigil || ($sigil ne '$' && $sigil ne '@')) {
        $M->sorry('Non bare scalar targets NYI');
        return;
    }
    $M->{_ast} = Sig::Target->new(list => ($sigil eq '@'), slot =>
        $M->{name}[0] ? ($sigil . $M->{name}[0]->Str) : undef);
}

# :: Sig::Parameter
sub parameter { my ($cl, $M) = @_;
    if (@{ $M->{type_constraint} } > 0) {
        $M->sorry('Parameter type constraints NYI');
        return;
    }

    if (@{ $M->{trait} } > 0) {
        $M->sorry('Parameter traits NYI');
        return;
    }

    if (@{ $M->{post_constraint} } > 0) {
        $M->sorry('Parameter post constraints NYI');
        return;
    }

    if ($M->{default_value}[0]) {
        $M->sorry('Default values NYI');
        return;
    }

    if ($M->{named_param}) {
        $M->sorry('Named parameters NYI');
        return;
    }

    if ($M->{quant} ne '*' && ($M->{quant} ne '' || $M->{kind} ne '!')) {
        $M->sorry('Exotic parameters NYI');
        return;
    }

    $M->{_ast} = Sig::Parameter->new(target => $M->{param_var}{_ast},
        slurpy => ($M->{quant} eq '*'));
}

# signatures exist in several syntactic contexts so just make an object for now
sub signature { my ($cl, $M) = @_;
    if ($M->{type_constraint}[0]) {
        $M->sorry("Return type constraints NYI");
        return;
    }

    if ($M->{param_var}) {
        $M->sorry('\| signatures NYI');
        return;
    }

    for (@{ $M->{param_sep} }) {
        if ($_->Str !~ /,/) {
            $M->sorry('Parameter separator ' . $_->Str . ' NYI');
            return;
        }
    }

    $M->{_ast} = Sig->new(params =>
        [map { $_->{_ast} } @{ $M->{parameter} }]);
}

sub multisig { my ($cl, $M) = @_;
    if (@{ $M->{signature} } != 1) {
        $M->sorry("Multiple signatures NYI");
        return;
    }
    $M->{_ast} = $M->{signature}[0]{_ast};
}

sub cgopname { my ($cl, $M) = @_;
    $M->{_ast} = $M->Str;
}

sub cgexp { }
sub cgexp__S_name { my ($cl, $M) = @_;
    $M->{_ast} = $M->{cgopname}{_ast};
}

sub cgexp__S_decint { my ($cl, $M) = @_;
    $M->{_ast} = $M->{decint}{_ast};
}

sub cgexp__S_quote { my ($cl, $M) = @_;
    if (!$M->{quote}{_ast}->isa('Op::StringLiteral')) {
        $M->sorry("Strings used in CgOp code must be compile time constants");
    }
    $M->{_ast} = $M->{quote}{_ast}->text;
}

my %opshortcut = (
    '@',   [ 'fetch' ],
    'l',   [ 'scopedlex' ],
    'w',   [ 'wrap' ],
    'ns',  [ 'newscalar' ],
    'nsw', [ 'newrwscalar' ],
    '==',  [ 'compare', '==' ], '!=',  [ 'compare', '!=' ],
    '>=',  [ 'compare', '>=' ], '<=',  [ 'compare', '<=' ],
    '<',   [ 'compare', '<' ],  '>',   [ 'compare', '>' ],
    '+',   [ 'arith', '+' ],    '-',   [ 'arith', '-' ],
    '*',   [ 'arith', '*' ],    '/',   [ 'arith', '/' ],
);

sub cgexp__S_op { my ($cl, $M) = @_;
    no strict 'refs';
    my $l = $M->{cgopname}{_ast};
    my ($op, @p) = @{ $opshortcut{$l} // [ $l ] };
    $M->{_ast} = &{"CgOp::$op"}(@p, map { $_->{_ast} } @{ $M->{cgexp} });
}

sub apostrophe {}
sub quibble {}
sub tribble {}
sub babble {}
sub quotepair {}

# We can't do much at blockoid reduce time because the context is unknown.
# Roles and subs need somewhat different code gen
sub blockoid { my ($cl, $M) = @_;
    # XXX horrible cheat, but my data structures aren't up to the task of
    # $::UNIT being a class body &c.
    if ($M->Str eq '{YOU_ARE_HERE}') {
        $M->{_ast} = Op::YouAreHere->new(node($M), unitname => $::UNITNAME);
    } else {
        $M->{_ast} = $M->{statementlist}{_ast};
    }
}
sub lambda {}

sub sigil {}
sub sigil__S_Amp {}
sub sigil__S_Dollar {}
sub sigil__S_At {}
sub sigil__S_Percent {}

sub twigil {}
sub twigil__S_Equal {}
sub twigil__S_Bang {}
sub twigil__S_Dot {}
sub twigil__S_Tilde {}
sub twigil__S_Star {}
sub twigil__S_Question {}
sub twigil__S_Caret {}
sub twigil__S_Colon {}

sub terminator {}
sub terminator__S_Thesis {}
sub terminator__S_Semi {}
sub terminator__S_Ket {}
sub terminator__S_Ly {}
sub terminator__S_then {}
sub terminator__S_again {}
sub terminator__S_repeat {}
sub terminator__S_while {}
sub terminator__S_else {}
sub terminator__S_BangBang {}
sub stdstopper {}
sub unitstopper {}
sub eat_terminator {}

sub scoped { my ($cl, $M) = @_;
    $M->{_ast} = ($M->{declarator} // $M->{regex_declarator} //
        $M->{package_declarator} // $M->{multi_declarator})->{_ast};
}

# :: Op (but adds decls)
sub declarator { my ($cl, $M) = @_;
    if ($M->{signature}) {
        $M->sorry("Signature declarations NYI");
        return;
    }
    $M->{_ast} = $M->{variable_declarator} ? $M->{variable_declarator}{_ast} :
                 $M->{routine_declarator}  ? $M->{routine_declarator}{_ast} :
                 $M->{regex_declarator}    ? $M->{regex_declarator}{_ast} :
                 $M->{type_declarator}{_ast};
}

sub scope_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{scoped}{_ast};
}
sub scope_declarator__S_my {}
sub scope_declarator__S_our {}
sub scope_declarator__S_augment {}
sub scope_declarator__S_supercede {}
sub scope_declarator__S_has {}
sub scope_declarator__S_state {}
sub scope_declarator__S_anon {}

sub variable_declarator { my ($cl, $M) = @_;
    if ($M->{trait}[0] || $M->{post_constraint}[0] || $M->{shape}[0]) {
        $M->sorry("Traits, postconstraints, and shapes on variable declarators NYI");
        return;
    }

    my $scope = $::SCOPE // 'my';

    if ($scope eq 'augment' || $scope eq 'supercede') {
        $M->sorry("Illogical scope $scope for simple variable");
        return;
    }

    my $v = $M->{variable}{_ast};
    my $t = $v->{twigil};
    if ($t =~ /[?=~^:]/) {
        $M->sorry("Variables with the $t twigil cannot be declared using " .
            "$scope; they are created " .
            ($t eq '?' ? "using 'constant'." :
             $t eq '=' ? "by parsing POD blocks." :
             $t eq '~' ? "by 'slang' definitions." :
             "automatically as parameters to the current block."));
        return;
    }

    if ($scope ne 'has' && $t =~ /[.!]/) {
        $M->sorry("Twigil $t is only valid on attribute definitions ('has').");
        return;
    }

    if (@{ $v->{rest} }) {
        $M->sorry(":: syntax is only valid when referencing variables, not when defining them.");
        return;
    }

    my $name = $v->{sigil} . $v->{twigil} . $v->{name};
    # otherwise identical to my
    my $slot = ($scope eq 'anon') ? $cl->gensym : $name;

    if ($scope eq 'has') {
        $M->{_ast} = Op::Attribute->new(node($M), name => $v->{name},
            accessor => ($v->{twigil} eq '.'));
    } elsif ($scope eq 'state') {
        $M->{_ast} = Op::Lexical->new(node($M), name => $slot, state_decl => 1,
            state_backing => $cl->gensym, declaring => 1,
            list => scalar ($M->{variable}->Str =~ /^\@/));
    } elsif ($scope eq 'our') {
        $M->{_ast} = Op::PackageVar->new(node($M), name => $slot, slot => $slot,
            path => [ 'OUR' ]);
    } else {
        $M->{_ast} = Op::Lexical->new(node($M), name => $slot, declaring => 1,
            list => scalar ($M->{variable}->Str =~ /^\@/));
    }
}

sub type_declarator {}
sub type_declarator__S_constant { my ($cl, $M) = @_;
    my $scope = $::SCOPE // 'my';
    if (!$M->{identifier} && !$M->{variable}) {
        $M->sorry("Anonymous constants NYI"); #wtf?
        return;
    }
    my $slot  = ($M->{identifier} // $M->{variable})->Str;

    # This is a cheat.  Constants should be, well, constant, and we should be
    # using the phaser rewrite mechanism to get the initializer here.  XXX
    # terms need to use a context hash.
    $M->{_ast} = Op::Lexical->new(node($M), name => $slot, declaring => 1);
}

sub package_declarator {}
sub package_declarator__S_class { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_grammar { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_package { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_module { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_knowhow { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_role { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_slang { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_also { my ($cl, $M) = @_;
    $M->{_ast} = Op::StatementList->new(node($M), children =>
        $cl->process_package_traits($M, undef, @{ $M->{trait} }));
}

sub process_package_traits { my ($cl, $M, $export, @tr) = @_;
    my @r;

    for (@tr) {
        if (exists $_->{_ast}{name}) {
            push @r, Op::Super->new(node($M), name => $_->{_ast}{name});
        } elsif ($_->{_ast}{export}) {
            if ($export) {
                push @$export, @{ $_->{_ast}{export} };
            } else {
                $M->sorry('Cannot mark a class as exported outside the declarator');
            }
        } else {
            $M->sorry("Non-superclass traits for packageoids NYI");
        }
    }

    @r;
}

sub termish {}
sub nulltermish { my ($cl, $M) = @_; # for 1,2,3,
    # XXX this is insane
    $M->{term}{_ast} = $M->{term}{term}{_ast} if $M->{term};
}
sub EXPR {}

sub arglist { my ($cl, $M) = @_;
    $M->sorry("Invocant handling is NYI") if $::INVOCANT_IS;
    my $x = $M->{EXPR}{_ast};

    if (!defined $x) {
        $M->{_ast} = [];
    } elsif ($x && $x->splittable_parcel) {
        $M->{_ast} = $x->positionals;
    } else {
        $M->{_ast} = [$x];
    }
}

sub semiarglist { my ($cl, $M) = @_;
    $M->{_ast} = [ map { $_->{_ast} } @{ $M->{arglist} } ];
}

sub args { my ($cl, $M) = @_;
    if ($M->{moreargs} || $M->{semiarglist} && $M->{arglist}[0]) {
        $M->sorry("Interaction between semiargs and args is not understood");
        return;
    }

    $M->{_ast} = $M->{semiarglist} ? $M->{semiarglist}{_ast} :
        $M->{arglist}[0] ? [ $M->{arglist}[0]{_ast} ] : undef;
}

sub statement { my ($cl, $M) = @_;
    if ($M->{label} || $M->{statement_mod_cond}[0] || $M->{statement_mod_loop}[0]) {
        $M->sorry("Control is NYI");
        return;
    }

    $M->{_ast} = $M->{statement_control} ? $M->{statement_control}{_ast} :
                 $M->{EXPR} ? $M->{EXPR}{_ast} : undef;
}

sub statementlist { my ($cl, $M) = @_;
    $M->{_ast} = Op::StatementList->new(node($M), children => 
        [ map { $_->statement_level } grep { defined }
            map { $_->{_ast} } @{ $M->{statement} } ]);
}

sub semilist { my ($cl, $M) = @_;
    $M->{_ast} = [  map { $_->{_ast} } @{ $M->{statement} } ];
}

sub module_name { }
sub module_name__S_normal { my ($cl, $M) = @_;
    # name-extension stuff is just ignored on module names for now
    $M->{_ast} = {
        name => $M->{longname}{name}->Str,
        args => $M->{arglist}[0] ? $M->{arglist}[0]{_ast} : undef };
}

sub statement_control { }
sub statement_control__S_if { my ($cl, $M) = @_;
    my $else = $M->{else}[0] ?
        $cl->block_to_immediate($M, 'cond', $M->{else}[0]{_ast}) : undef;
    my @elsif;
    for (reverse @{ $M->{elsif} }) {
        $else = Op::Conditional->new(node($M), check => $_->{_ast}[0],
            true => $cl->block_to_immediate($M, 'cond', $_->{_ast}[1]),
            false => $else);
    }
    $M->{_ast} = Op::Conditional->new(node($M), check => $M->{xblock}{_ast}[0],
        true => $cl->block_to_immediate($M, 'cond', $M->{xblock}{_ast}[1]),
        false => $else);
}

sub statement_control__S_while { my ($cl, $M) = @_;
    $M->{_ast} = Op::WhileLoop->new(node($M), check => $M->{xblock}{_ast}[0],
        body => $cl->block_to_immediate($M, 'loop',$M->{xblock}{_ast}[1]),
        until => 0, once => 0);
}

sub statement_control__S_until { my ($cl, $M) = @_;
    $M->{_ast} = Op::WhileLoop->new(node($M), check => $M->{xblock}{_ast}[0],
        body => $cl->block_to_immediate($M, 'loop', $M->{xblock}{_ast}[1]),
        until => 1, once => 0);
}

sub statement_control__S_for { my ($cl, $M) = @_;
    $M->{xblock}{_ast}[1]->type('loop');
    $M->{_ast} = Op::ForLoop->new(node($M), source => $M->{xblock}{_ast}[0],
        sink => $cl->block_to_closure($M, $M->{xblock}{_ast}[1]));
}

sub statement_control__S_use { my ($cl, $M) = @_;
    my $name = $M->{module_name}{_ast}{name};
    my $args = $M->{arglist} ? $M->{arglist}{_ast} : [];

    if ($M->{module_name}{_ast}{args}) {
        $M->sorry("'use' of an instantiated role not yet understood");
        return;
    }

    if (@$args) {
        $M->sorry("'use' with arguments NYI");
        return;
    }

    push @::UNITDEPS, $name;
    my %symbols;
    $symbols{$name} = [ $name ];
    $symbols{$name . '::'} = [ $name . '::' ];

    my $pkg = $M->find_stash($name);
    if ($pkg->{really}) {
        $pkg = $pkg->{really}->{UNIT};
    }
    else {
        $pkg = $M->find_stash($name . '::');
    }

    # XXX This code is wrong.  It either needs to be more integrated with STD,
    # or less.
    for my $exp (keys %{ $pkg->{'EXPORT::'}->{'DEFAULT::'} }) {
        $symbols{$exp} = [ $name . '::', 'EXPORT::', 'DEFAULT::', $exp ];
    }

    $M->{_ast} = Op::Use->new(node($M), unit => $name, symbols => \%symbols);
}

# All package defs have a couple things in common - a special-ish block,
# with a special decl, and some nice runtimey code
sub package_def { my ($cl, $M) = @_;
    my $scope = $::SCOPE;
    if (!$M->{longname}[0]) {
        $scope = 'anon';
    }
    if ($scope eq 'augment' || $scope eq 'supercede') {
        $M->sorry('Monkey typing is not yet supported');
        return;
    }
    if ($scope eq 'has' || $scope eq 'state') {
        $M->sorry("Illogical scope $scope for package block");
        return;
    }
    # XXX shouldn't fully mangle here, c.f. STD:auth<http://perl.org>
    my $name = $M->{longname}[0] ?
        $cl->mangle_longname($M->{longname}[0], "package definition") : 'ANON';
    my $outervar = $scope ne 'anon' ? $name : $cl->gensym;

    my $optype = 'Op::' . ucfirst($::PKGDECL) . 'Def';
    my $blocktype = $::PKGDECL;
    my $bodyvar = $cl->gensym;
    # We need the OUR because otherwise the name lookup latches on to the
    # nascent lexical alias and crashes.  Possibly a bug.
    my $ourpkg = ($scope eq 'our') ? [ 'OUR::' ] : undef;

    if (!$M->{decl}{stub}) {
        my $stmts = $M->{statementlist} // $M->{blockoid};
        my @export;

        $stmts = Op::StatementList->new(children =>
            [ $cl->process_package_traits($M, \@export, @{ $M->{trait} }),
                $stmts->{_ast} ]);

        my $cbody = $cl->sl_to_block($blocktype, $stmts,
            name => $name);
        $M->{_ast} = $optype->new(
            node($M),
            name    => $name,
            var     => $outervar,
            exports => \@export,
            bodyvar => $bodyvar,
            ourpkg  => $ourpkg,
            body    => $cbody);
    } else {
        $M->{_ast} = $optype->new(
            node($M),
            name    => $name,
            var     => $outervar,
            ourpkg  => $ourpkg,
            stub    => 1);
    }
}

sub trait_mod {}
sub trait_mod__S_is { my ($cl, $M) = @_;
    my $trait = $M->{longname}->Str;
    my $noparm;

    if ($M->is_name($trait)) {
        $M->{_ast} = { name => $trait };
        $noparm = 'Superclasses cannot have parameters';
    } elsif ($trait eq 'export') {
        $M->{_ast} = { export => [ 'DEFAULT', 'ALL' ] };
        $noparm = 'Export tags NYI';
    } else {
        $M->sorry('Unhandled trait ' . $trait);
    }

    if ($noparm && $M->{circumfix}[0]) {
        $M->sorry($noparm);
        return;
    }
}

sub trait { my ($cl, $M) = @_;
    if ($M->{colonpair}) {
        $M->sorry('Colonpair traits NYI');
        return;
    }

    $M->{_ast} = $M->{trait_mod}{_ast};
}

sub routine_declarator {}
sub routine_declarator__S_sub { my ($cl, $M) = @_;
    $M->{_ast} = $M->{routine_def}{_ast};
}
sub routine_declarator__S_method { my ($cl, $M) = @_;
    $M->{_ast} = $M->{method_def}{_ast};
}

my $next_anon_id = 0;
sub gensym { 'anon_' . ($next_anon_id++) }

sub blockcheck { my ($cl) = @_;
}

sub sl_to_block { my ($cl, $type, $ast, %args) = @_;
    my $subname = $args{subname} // 'ANON';
    $cl->blockcheck;
    Body->new(
        name      => $subname,
        type      => $type,
        signature => $args{signature},
        do        => $ast);
}

sub get_outer { my ($cl, $pad) = @_;
    $STD::ALL->{ $pad->{'OUTER::'}[0] };
}

sub block_to_immediate { my ($cl, $M, $type, $blk) = @_;
    $blk->type($type);
    Op::CallSub->new(node($M),
        invocant => $cl->block_to_closure($M, $blk),
        positionals => []);
}

sub block_to_closure { my ($cl, $M, $blk, %args) = @_;
    my $outer_key = $args{outer_key} // $cl->gensym;

    Op::SubDef->new(var => $outer_key, body => $blk, node($M),
        method_too => $args{method_too}, exports => ($args{exports} // []));
}

# always a sub, though sometimes it's an implied sub after multi/proto/only
sub routine_def { my ($cl, $M) = @_;
    if ($M->{sigil}[0] && $M->{sigil}[0]->Str eq '&*') {
        $M->sorry("Contextuals NYI");
        return;
    }
    my $dln = $M->{deflongname}[0];
    if (@{ $M->{multisig} } > 1) {
        $M->sorry("Multiple multisigs (what?) NYI");
        return;
    }
    my @export;
    for my $t (@{ $M->{trait} }) {
        if ($t->{_ast}{export}) {
            push @export, @{ $t->{_ast}{export} };
        } else {
            $M->sorry('Non-export sub traits NYI');
        }
    }
    my $scope = !$dln ? 'anon' : $::SCOPE || 'my';
    if ($scope ne 'my' && $scope ne 'our' && $scope ne 'anon') {
        $M->sorry("Illegal scope $scope for subroutine");
        return;
    }
    if ($scope eq 'our') {
        $M->sorry('Package subs NYI');
        return;
    }

    my $m = $dln ? $cl->mangle_longname($dln, "subroutine definition") : undef;

    $M->{_ast} = $cl->block_to_closure($M,
            $cl->sl_to_block('sub',
                $M->{blockoid}{_ast},
                subname => $m,
                signature => ($M->{multisig}[0] ? $M->{multisig}[0]{_ast} : undef)),
        outer_key => (($scope eq 'my') ? "&$m" : undef),
        exports => \@export);
}

sub method_def { my ($cl, $M) = @_;
    my $scope = $::SCOPE // 'has';
    $scope = 'anon' if !$M->{longname};
    my $name = $M->{longname} ? $cl->mangle_longname($M->{longname}, "method definition") : undef;

    if ($M->{trait}[0] || $M->{sigil}) {
        $M->sorry("Method traits NYI");
        return;
    }
    if (@{ $M->{multisig} } > 1) {
        $M->sorry("Multiple multisigs (what?) NYI");
        return;
    }

    my $sym = ($scope eq 'my') ? ('&' . $name) : $cl->gensym;

    if ($scope eq 'augment' || $scope eq 'supercede' || $scope eq 'state') {
        $M->sorry("Illogical scope $scope for method");
        return;
    }

    if ($scope eq 'our') {
        $M->sorry("Packages NYI");
        return;
    }

    my $bl = $cl->sl_to_block('sub', $M->{blockoid}{_ast},
        subname => $name,
        signature => ($M->{multisig}[0] ?
            $M->{multisig}[0]{_ast}->for_method : undef));

    $M->{_ast} = $cl->block_to_closure($M, $bl, outer_key => $sym,
        method_too => ($scope ne 'anon' ? $name : undef));
}

sub block { my ($cl, $M) = @_;
    $M->{_ast} = $cl->sl_to_block('', $M->{blockoid}{_ast});
}

# :: Body
sub pblock { my ($cl, $M) = @_;
    my $rw = $M->{lambda} && $M->{lambda}->Str eq '<->';
    $M->{_ast} = $cl->sl_to_block('', $M->{blockoid}{_ast},
        signature => ($M->{signature} ? $M->{signature}{_ast} : undef));
}

sub xblock { my ($cl, $M) = @_;
    $M->{_ast} = [ $M->{EXPR}{_ast}, $M->{pblock}{_ast} ];
}

# returns Body of 0 args
sub blast { my ($cl, $M) = @_;
    if ($M->{block}) {
        $M->{_ast} = $M->{block}{_ast};
    } else {
        $M->{_ast} = Body->new(
            transparent => 1,
            name => 'ANON',
            do   => $M->{statement}{_ast});
    }
}

sub statement_prefix {}
sub statement_prefix__S_do { my ($cl, $M) = @_;
    $M->{_ast} = $cl->block_to_immediate($M, 'do', $M->{blast}{_ast});
}
sub statement_prefix__S_PREMinusINIT { my ($cl, $M) = @_;
    my $var = $cl->gensym;

    $M->{blast}{_ast}->type('phaser');

    $M->{_ast} = Op::PreInit->new(var => $var, body => $M->{blast}{_ast},
        shared => 1, node($M));
}

sub statement_prefix__S_START { my ($cl, $M) = @_;
    my $cv = $cl->gensym;
    $M->{_ast} = Op::Start->new(node($M), condvar => $cv, body =>
        $cl->block_to_immediate($M, 'phaser', $M->{blast}{_ast}));
}

sub comp_unit { my ($cl, $M) = @_;
    my $body;
    my $sl = $M->{statementlist}{_ast};

    if (!$::YOU_WERE_HERE && $::UNITNAME) {
        $sl = Op::StatementList->new(node($M), children => [ $sl,
                Op::YouAreHere->new(save_only => 1, unitname => $::UNITNAME)]);
    }

    $body = $cl->sl_to_block('mainline', $sl, subname => 'mainline');
    if ($::YOU_WERE_HERE) {
        $body = Body->new(
            type => 'mainline',
            name => 'install',
            signature => Sig->new(params => [
                    Sig::Parameter->new(target => Sig::Target->new(
                            slot => '!mainline', zeroinit => 1))]),
            do => Op::CallSub->new(node($M),
                invocant => Op::CgOp->new(op => CgOp::newscalar(
                        CgOp::rawsget($::SETTINGNAME . ".Installer"))),
                positionals => [Op::SubDef->new(
                    var => $cl->gensym, body => $body)]));
    }

    $M->{_ast} = Unit->new(mainline => $body, name => $::UNITNAME,
        ($::SETTING_RESUME ? (setting => $::SETTING_RESUME) : ()),
        is_setting => (!!$::YOU_WERE_HERE),
        setting_name => $::SETTINGNAME);
}

1;
