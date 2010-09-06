package Niecza::Actions;
use 5.010;
use strict;
use warnings;
use utf8;
use Scalar::Util 'blessed';

use Op;
use RxOp;
use Body;
use Unit;
use Sig;
use CClass;

use Try::Tiny;

# I actually prefer this to the official AUTOLOAD solution
{
    package CursorBase;

    no warnings 'redefine';
    sub _REDUCE { my $self = shift;
        my $S = shift;
        my $meth = shift;
        my $key = $meth;
        $key .= ' ' . $_[0] if @_;

        $self->{_reduced} = $key;
        $self->{_from} = $S;
        if ($::ACTIONS) {
            $::ACTIONS->REDUCE($meth, $self, @_);
        }
        $self->deb("REDUCE $key from " . $S . " to " . $self->{_pos}) if &CursorBase::DEBUG() & &DEBUG::matchers();
        $self;
    }
}

my %carped;
sub REDUCE {
    my ($cl, $meth, $M) = @_;
    eval {
        my ($snd, $spec);
        if ($meth =~ /^(.*)__S_\d\d\d(.*)$/) {
            $meth = "$1__S_$2";
            $snd  = "$1__S_ANY";
            $spec = $2;
        }
        if ($cl->can($meth)) {
            return $cl->$meth($M);
        } elsif ($snd && $cl->can($snd)) {
            return $cl->$snd($M, $spec);
        } elsif (!( $carped{$meth}++ )) {
            die("Action method $meth not yet implemented");
        }
    };

    if ($@) {
        my $foo = $@;
        $foo =~ s/^(?:[^\n]*\n){5}\K.*//s;
        $M->sorry($foo);
    }
}

sub node { my ($M) = @_;
    file => $::FILE->{name}, line => $M->lineof($M->pos)
}

sub ws { }
sub is_ok { }
sub dumbsmart { }
sub normspace { }
sub vws { }
sub unv { }
sub begid { }
sub comment { }
sub comment__S_Sharp { }
sub comment__S_SharpGraveParenDotDotDotThesis { }
sub opener { }
sub starter { }
sub spacey { }
sub unspacey { }
sub unsp { }
sub nofun { }
sub curlycheck { }
sub pod_comment { }
sub infixstopper { }

sub category { }
sub category__S_category { }
sub category__S_sigil { }
sub category__S_twigil { }
sub category__S_special_variable { }
sub category__S_comment { }
sub category__S_version { }
sub category__S_module_name { }
sub category__S_value { }
sub category__S_term { }
sub category__S_strtonum { }
sub category__S_quote { }
sub category__S_prefix { }
sub category__S_infix { }
sub category__S_postfix { }
sub category__S_dotty { }
sub category__S_circumfix { }
sub category__S_postcircumfix { }
sub category__S_quote_mod { }
sub category__S_trait_mod { }
sub category__S_type_declarator { }
sub category__S_scope_declarator { }
sub category__S_package_declarator { }
sub category__S_multi_declarator { }
sub category__S_routine_declarator { }
sub category__S_regex_declarator { }
sub category__S_statement_prefix { }
sub category__S_statement_control { }
sub category__S_statement_mod_cond { }
sub category__S_statement_mod_loop { }
sub category__S_infix_prefix_meta_operator { }
sub category__S_infix_postfix_meta_operator { }
sub category__S_infix_circumfix_meta_operator { }
sub category__S_postfix_prefix_meta_operator { }
sub category__S_prefix_postfix_meta_operator { }
sub category__S_prefix_circumfix_meta_operator { }
sub category__S_terminator { }
sub category__S_metachar { }
sub category__S_backslash { }
sub category__S_assertion { }
sub category__S_quantifier { }
sub category__S_mod_internal { }

sub sign { }

sub decint { my ($cl, $M) = @_;
    $M->{_ast} = eval $M->Str; # XXX use a real string parser
}

sub hexint { my ($cl, $M) = @_;
    my $s = $M->Str;
    $s =~ s/_//g;
    $M->{_ast} = hex $M->Str;
}

sub octint { my ($cl, $M) = @_;
    my $s = $M->Str;
    $s =~ s/_//g;
    $M->{_ast} = oct $M->Str;
}

sub decints { my ($cl, $M) = @_;
    $M->{_ast} = [ map { $_->{_ast} } @{ $M->{decint} } ];
}

sub hexints { my ($cl, $M) = @_;
    $M->{_ast} = [ map { $_->{_ast} } @{ $M->{hexint} } ];
}

sub octints { my ($cl, $M) = @_;
    $M->{_ast} = [ map { $_->{_ast} } @{ $M->{octint} } ];
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

sub typename { }
sub type_constraint { }

# { dc: Bool, names: [Either String Op] }
sub name { my ($cl, $M) = @_;
    my @names = map { $_->{_ast} } @{ $M->{morename} };
    unshift @names, $M->{identifier}{_ast} if $M->{identifier};
    $M->{_ast} = { dc => !($M->{identifier}), names => \@names };
}

sub longname {} # look at the children yourself
sub deflongname {}

# Turns a name like ::Foo::Bar:sym[ 'x' ] into
# { name => 'Bar:sym<x>', path => [ 'Foo '] }
# path can be undefined for a simple name like $x, which goes straight to pad
# pass $clean if you want to ignore adverbs entirely - currently needed for
# package names
sub unqual_longname { my ($cl, $M, $what, $clean) = @_;
    my $h = $cl->mangle_longname($M, $clean);
    if ($h->{path}) {
        $M->sorry($what);
        return;
    }
    return $h->{name};
}

sub mangle_longname { my ($cl, $M, $clean) = @_;
    my @ns = @{ $M->{name}{_ast}{names} };
    my $n = pop @ns;

    unless ($clean) {
        $n .= $_->{_ast}{ext} // do {
            $M->sorry("Invalid colonpair for name extension");
            "";
        } for @{ $M->{colonpair} };
    }

    my @path = ($M->{name}{_ast}{dc} || @ns) ? (path => \@ns) : ();
    return { name => $n, @path };
}

sub subshortname { my ($cl, $M) = @_;
    if (@{ $M->{colonpair} }) {
        my $n = $M->{category}->Str;
        $n .= $_->{_ast}{ext} // do {
            $M->sorry("Invalid colonpair for name extension");
            "";
        } for @{ $M->{colonpair} };
        $M->{_ast} = { name => $n };
    } else {
        $M->{_ast} = $M->{desigilname}{_ast};
    }
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
        $M->{_ast} = { ind => $cl->do_variable_reference($M,
                $M->{variable}{_ast}) };
    } else {
        $M->{_ast} = $cl->mangle_longname($M->{longname});
    }
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
    $M->{_ast} = $M->{quibble}{_ast};
}

sub quote__S_Slash_Slash { my ($cl, $M) = @_;
    $M->{_ast} = Op::SubDef->new(
        var  => $cl->gensym,
        body => Body->new(
            class => 'Regex',
            type  => 'regex',
            signature => Sig->simple->for_regex,
            do => Op::RegexBody->new(rxop => $M->{nibble}{_ast})));
}

sub regex_block { my ($cl, $M) = @_;
    if (@{ $M->{quotepair} }) {
        $M->sorry('Regex adverbs NYI');
        return;
    }
    $M->{_ast} = $M->{nibble}{_ast};
}

sub regex_def { my ($cl, $M) = @_;
    my ($name, $path) = $M->{deflongname}[0] ?
        @{ $cl->mangle_longname($M->{deflongname}[0]) }{'name', 'path'} : ();
    my $scope = (!defined($name)) ? "anon" : ($::SCOPE || "has");

    if (@{ $M->{signature} } > 1) {
        $M->sorry("Multiple signatures on a regex NYI");
        return;
    }

    my $isproto;
    my $symtext =
        !defined($name) ? undef :
        ($name =~ /:sym<(.*)>/) ? $1 :
        ($name =~ /:(\w+)/) ? $1 :
        undef; #XXX
    my $unsymtext =
        !defined($name) ? undef :
        ($name =~ /(.*):sym<.*>/) ? $1 :
        ($name =~ /(.*):\w+/) ? $1 :
        undef;
    if ($::MULTINESS eq 'proto') {
        if ($M->{signature}[0] || !$M->{regex_block}{onlystar} || $scope ne 'has') {
            $M->sorry("Only simple {*} protoregexes with no parameters are supported");
            return;
        }
        $isproto = 1;
    } else {
        my $m2 = defined($::symtext) ? 'multi' : 'only';
        if ($::MULTINESS && $::MULTINESS ne $m2) {
            $M->sorry("Inferred multiness disagrees with explicit");
            return;
        }
    }

    if ($path && $scope ne 'our') {
        $M->sorry("Putting a regex in a package requires using the our scope.");
        return;
    }

    my $sig = $M->{signature}[0] ? $M->{signature}[0]{_ast}
        : $cl->get_placeholder_sig($M);

    if ($scope =~ /state|augment|supersede/) {
        $M->sorry("Nonsensical scope $scope for regex");
        return;
    }

    if ($scope eq 'our') {
        $M->sorry("our regexes NYI");
        return;
    }

    my $var = ($scope eq 'anon' || $scope eq 'has') ? $cl->gensym
        : '&' . $name;

    my $ast = $M->{regex_block}{_ast};
    if ($isproto) {
        $ast = RxOp::ProtoRedis->new(name => $name);
    }

    local $::symtext = $symtext;
    $M->{_ast} = Op::SubDef->new(
        var  => $var,
        method_too => ($scope eq 'has' ? $name : undef),
        proto_too => ($scope eq 'has' ? $unsymtext : undef),
        body => Body->new(
            ltm   => $ast->lad,
            class => 'Regex',
            type  => 'regex',
            signature => $sig->for_regex,
            do => Op::RegexBody->new(sym => $symtext,
                name => ($name // ''), rxop => $ast)));
}

sub regex_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{regex_def}{_ast};
}
sub regex_declarator__S_regex {}
sub regex_declarator__S_rule {}
sub regex_declarator__S_token {}

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

    if ($::RX{r}) {
        # no quantifier at all?  treat it as :
        $q //= { mod => '' };
        # quantifier without explicit :? / :! gets :
        $q->{mod} //= '';
    }

    if (defined $q->{min}) {
        $atom = RxOp::Quantifier->new(min => $q->{min}, max => $q->{max},
            zyg => [$atom], minimal => ($q->{mod} && $q->{mod} eq '?'));
    }

    if (defined $q->{mod} && $q->{mod} eq '') {
        $atom = RxOp::Cut->new(zyg => [$atom]);
    }

    $M->{_ast} = $atom;
}

# :: Context hash interpreted by quantified_atom
sub quantifier {}
sub quantifier__S_Star { my ($cl, $M) = @_;
    $M->{_ast} = { min => 0, mod => $M->{quantmod}{_ast} };
}
sub quantifier__S_Plus { my ($cl, $M) = @_;
    $M->{_ast} = { min => 1, mod => $M->{quantmod}{_ast} };
}
sub quantifier__S_Question { my ($cl, $M) = @_;
    $M->{_ast} = { min => 0, max => 1, mod => $M->{quantmod}{_ast} };
}
sub quantifier__S_Colon { my ($cl, $M) = @_;
    $M->{_ast} = { mod => '' };
}

sub quantmod { my ($cl, $M) = @_;
    my $t = $M->Str;
    return if ($t eq '');
    $t =~ s/://;
    if ($t eq '+') {
        $M->sorry('STD parses + as a quantmod but there is nothing at all in S05 to explain what it should _do_'); #XXX
        return;
    }
    $M->{_ast} = $t;
}

sub quant_atom_list { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::Sequence->new(zyg =>
        [ map { $_->{_ast} } @{ $M->{quantified_atom} } ]);
}

my %LISTrx_types = (
    '&'  => 'RxOp::Conj',
    '|'  => 'RxOp::Alt',
    '&&' => 'RxOp::SeqConj',
    '||' => 'RxOp::SeqAlt',
);
sub LISTrx { my ($cl, $M) = @_;
    $M->{_ast} = $LISTrx_types{$M->{delims}[0]{sym}}->new(zyg =>
        [ map { $_->{_ast} } @{ $M->{list} } ]);
}

sub regex_infix {}
sub regex_infix__S_Vert {}
sub regex_infix__S_VertVert {}
sub regex_infix__S_Amp {}
sub regex_infix__S_AmpAmp {}

sub metachar {}
sub metachar__S_sigwhite { my ($cl, $M) = @_;
    $M->{_ast} = $::RX{s} ? RxOp::Sigspace->new : RxOp::Sequence->new;
}

sub metachar__S_unsp { my ($cl, $M) = @_;
    $M->{_ast} = RxOp::Sequence->new;
}

sub metachar__S_Cur_Ly { my ($cl, $M) = @_;
    my $inv = $M->{embeddedblock}{_ast}->invocant;
    $inv->body->type('rxembedded');
    $inv->body->signature(Sig->simple('$¢'));
    $M->{_ast} = RxOp::VoidBlock->new(block => $inv);
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
    $M->{_ast} = RxOp::Capture->new(names => [undef], zyg => [
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
    my $cc = $M->{backslash}{_ast};
    $M->{_ast} = ref($cc) ?
        RxOp::CClassElem->new(cc => $cc,
            igcase => $::RX{i}, igmark => $::RX{a}) :
        RxOp::String->new(text => $cc,
            igcase => $::RX{i}, igmark => $::RX{a});
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

sub rxcapturize { my ($cl, $name, $rxop) = @_;
    if (!$rxop->isa('RxOp::Subrule')) {
        # <before>, etc.  Not yet really handled XXX
        return $rxop;
    }

    my @extra = map { $_ => $rxop->$_ } qw/zyg arglist name/;

    # $<foo>=(...)
    if (@{ $rxop->captures } == 1 && !defined($rxop->captures->[0])) {
        return RxOp::Subrule->new(captures => [$name], @extra);
    }

    return RxOp::Subrule->new(captures => [ $name, @{ $rxop->captures } ],
        @extra);
}

sub do_cclass { my ($cl, $M) = @_;
    my @cce = @{ $M->{cclass_elem} };

    my $rxop;
    for (@cce) {
        my $sign = $_->{sign}->Str ne '-';
        my $exp = $_->{quibble} ?
            RxOp::CClassElem->new(cc => $_->{quibble}{_ast}) :
            RxOp::CallMethod->new(name => $_->{name}->Str); # assumes no capture

        if ($sign) {
            $rxop = $rxop ? RxOp::SeqAlt->new(zyg => [ $exp, $rxop ]) : $exp;
        } else {
            $rxop = RxOp::Sequence->new(zyg => [
                RxOp::NotBefore->new(zyg => [ $exp ]),
                $rxop // RxOp::Any->new]);
        }
    }

    $M->{_ast} = $rxop;
}

sub decapturize { my ($cl, $M) = @_;
    if ($M->{assertion}{assertion}[0]) {
        $M->sorry("Binding to a method doesn't work like that");
        return;
    }
    if (!$M->{assertion}{_ast}->isa('RxOp::Subrule')) {
        $M->sorry("Internal error in assertion:method parse");
        return;
    }
    RxOp::Subrule->new(captures => [],
        zyg => $M->{assertion}{_ast}->zyg,
        arglist => $M->{assertion}{_ast}->arglist,
        name => $M->{assertion}{_ast}->name);
}

sub cclass_elem {}

sub assertion {}
# This needs to be deconstructed by :method, so it needs a regular structure
sub assertion__S_name { my ($cl, $M) = @_;
    my $name = $cl->unqual_longname($M->{longname},
        "Qualified method calls NYI");
    if ($M->{assertion}[0]) {
        $M->{_ast} = $M->{assertion}[0]{_ast};
    } else {
        if ($M->{nibbler}[0]) {
            my $args = [$M->{nibbler}[0]{_ast}];
            $M->{_ast} = RxOp::Subrule->new(zyg => $args, name => $name);
        } else {
            my $args = ($M->{arglist}[0] ? $M->{arglist}[0]{_ast} : []);
            $M->{_ast} = RxOp::Subrule->new(arglist => $args, name => $name);
        }
    }
    $M->{_ast} = $cl->rxcapturize($name, $M->{_ast});
}

sub assertion__S_method { my ($cl, $M) = @_;
    if ($M->{dottyop}) {
        $M->sorry("Dottyop assertions NYI");
        return;
    }
    $M->{_ast} = $cl->decapturize($M);
}

sub assertion__S_Question { my ($cl, $M) = @_;
    if ($M->{assertion}) {
        $M->{_ast} = RxOp::Before->new(zyg => [$cl->decapturize($M)]);
    } else {
        $M->{_ast} = RxOp::Sequence->new;
    }
}

sub assertion__S_Bang { my ($cl, $M) = @_;
    if ($M->{assertion}) {
        $M->{_ast} = RxOp::NotBefore->new(zyg => [$cl->decapturize($M)]);
    } else {
        $M->{_ast} = RxOp::None->new;
    }
}

sub assertion__S_Cur_Ly { my ($cl, $M) = @_;
    my $inv = $M->{embeddedblock}{_ast}->invocant;
    $inv->body->type('rxembedded');
    $inv->body->signature(Sig->simple('$¢'));
    $M->{_ast} = RxOp::CheckBlock->new(block => $inv);
}

*assertion__S_Bra   = \&do_cclass;
*assertion__S_Minus = \&do_cclass;
*assertion__S_Plus  = \&do_cclass;

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

sub backslash { my ($cl, $M) = @_;
    if ($M->Str =~ /^[A-Z]$/ && $M->{sym} =~ /^[a-z]$/) {
        if (!ref($M->{_ast}) && length($M->{_ast}) != 1) {
            $M->sorry("Improper attempt to negate a string");
            return;
        }
        $M->{_ast} = CClass->enum($M->{_ast}) unless blessed $M->{_ast};
        $M->{_ast} = $M->{_ast}->negate;
    }
}
sub backslash__S_x { my ($cl, $M) = @_;
    if ($M->{hexint}) {
        $M->{_ast} = chr($M->{hexint}{_ast});
    } else {
        $M->{_ast} = join "", map { chr } @{ $M->{hexints}{_ast} };
    }
}
sub backslash__S_o { my ($cl, $M) = @_;
    if ($M->{octint}) {
        $M->{_ast} = chr($M->{octint}{_ast});
    } else {
        $M->{_ast} = join "", map { chr } @{ $M->{octints}{_ast} };
    }
}
sub backslash__S_Back { my ($cl, $M) = @_;
    $M->{_ast} = $M->{text}->Str;
}
sub backslash__S_stopper { my ($cl, $M) = @_;
    $M->{_ast} = $M->{text}->Str;
}
sub backslash__S_unspace { my ($cl, $M) = @_;
    $M->{_ast} = "";
}
sub backslash__S_misc { my ($cl, $M) = @_;
    $M->{_ast} = $M->{text} // $M->{litchar}->Str;
}
# XXX h, v, s, needs spec clarification
sub backslash__S_0 { my ($cl, $M) = @_; $M->{_ast} = "\0" }
sub backslash__S_a { my ($cl, $M) = @_; $M->{_ast} = "\a" }
sub backslash__S_b { my ($cl, $M) = @_; $M->{_ast} = "\b" }
sub backslash__S_d { my ($cl, $M) = @_; $M->{_ast} = $CClass::Digit }
sub backslash__S_e { my ($cl, $M) = @_; $M->{_ast} = "\e" }
sub backslash__S_f { my ($cl, $M) = @_; $M->{_ast} = "\f" }
sub backslash__S_h { my ($cl, $M) = @_; $M->{_ast} = $CClass::HSpace }
sub backslash__S_n { my ($cl, $M) = @_; $M->{_ast} = "\n" }
sub backslash__S_r { my ($cl, $M) = @_; $M->{_ast} = "\r" }
sub backslash__S_s { my ($cl, $M) = @_; $M->{_ast} = $CClass::Space }
sub backslash__S_t { my ($cl, $M) = @_; $M->{_ast} = "\t" }
sub backslash__S_v { my ($cl, $M) = @_; $M->{_ast} = $CClass::VSpace }
sub backslash__S_w { my ($cl, $M) = @_; $M->{_ast} = $CClass::Word }

sub escape {}
sub escape__S_Back { my ($cl, $M) = @_;
    $M->{_ast} = $M->{item}{_ast};
}
sub escape__S_Cur_Ly { my ($cl, $M) = @_;
    $M->{_ast} = $M->{embeddedblock}{_ast};
}
sub escape__S_Dollar { my ($cl, $M) = @_;
    $M->{_ast} = $M->{EXPR}{_ast};
}
sub escape__S_At { my ($cl, $M) = @_;
    $M->{_ast} = $M->{EXPR}{_ast};
}
sub escape__S_Percent { my ($cl, $M) = @_;
    $M->{_ast} = $M->{EXPR}{_ast};
}
sub escape__S_ch { my ($cl, $M) = @_;
    $M->{_ast} = $M->{ch}->Str;
}
sub escape__S_ws { my ($cl, $M) = @_;
    $M->{_ast} = "";
}
sub escape__S_DotDot { my ($cl, $M) = @_;
    $M->{_ast} = \"DotDot";  #yuck
}

sub nibbler { my ($cl, $M) = @_;
    if ($M->isa('STD::Regex')) {
        $M->{_ast} = $M->{EXPR}{_ast};
    } elsif ($M->isa('Niecza::Grammar::CgOp')) {
        # XXX We don't interpret the code, so we can't tell if it's actually
        # using variables, but still, it probably is.
        if ($::SAFEMODE) {
            $M->sorry('Q:CgOp not allowed in safe mode');
            return;
        }
        for my $k (keys %$::CURLEX) {
            $::CURLEX->{$k}{used} = 1 if $k =~ /^[\@\%\&\$]\w/;
        }
        $M->{_ast} = Op::CgOp->new(node($M), optree => $M->{cgexp}{_ast});
    } elsif ($M->can('ccstate')) { #XXX XXX try to catch cclasses
        my @nib = @{ $M->{nibbles} };
        my @bits = map { $_->{_ast} } @nib;

        for (my $i = 0; $i < @bits; $i++) {
            my $t = ref($bits[$i]);
            if (!$t) {
                if (length($bits[$i]) > 1) {
                    $nib[$i]->sorry("Cannot use a string in a character class");
                    $bits[$i] = "";
                }
            } elsif ($t eq 'SCALAR') {
                # .. hack
            } elsif ($t eq 'CClass') {
                # also ok
            } else {
                $nib[$i]->sorry("Cannot use an interpolation in a character class");
                $bits[$i] = "";
            }

            if ($bits[$i] eq "") {
                splice @bits, $i, 1;
                splice @nib, $i, 1;
                $i--;
            }
        }

        for (my $i = 0; $i < @bits; $i++) {
            next unless ref($bits[$i]) && ref($bits[$i]) eq 'SCALAR';

            for ($i-1, $i + 1) {
                if (ref($bits[$_])) {
                    $nib[$_]->sorry("Bad range endpoint");
                    $bits[$_] = "\0";
                }
            }

            splice @bits, $i-1, 3, CClass->range($bits[$i-1], $bits[$i+1]);
            splice @nib, $i-1, 2;
            $i--;
        }
        $M->{_ast} = $CClass::Empty;
        $M->{_ast} = $M->{_ast}->plus($_) for @bits;
        #say(YAML::XS::Dump($M->{_ast}));
    } else {
        # garden variety nibbler
        my @bits;
        for my $n (@{ $M->{nibbles} }) {
            if (!blessed($n)) {
                say(STDERR YAML::XS::Dump($n));
                next;
            }
            my $bit = $n->isa('Str') ? $n->{TEXT} : $n->{_ast};

            if (ref($bit) && ref($bit) eq 'CClass') {
                $n->sorry("Tried to use a character class in a string");
                $bit = "";
            }

            # this *might* belong in an optimization pass
            if (!blessed($bit) && @bits && !blessed($bits[-1])) {
                $bits[-1] .= $bit;
            } else {
                push @bits, $bit;
            }
        }
        push @bits, '' unless @bits;
        @bits = map { blessed($_) ? $_ : Op::StringLiteral->new(node($M),
                text => $_) } @bits;
        $M->{_ast} = (@bits == 1) ? $bits[0] :
            Op::CallSub->new(node($M),
                invocant => Op::Lexical->new(name => '&infix:<~>'),
                positionals => \@bits);
    }
}

sub circumfix { }
sub circumfix__S_Lt_Gt { my ($cl, $M) = @_;
    my $sl = $M->{nibble}{_ast};

    if (!$sl->isa('Op::StringLiteral')) {
        $M->sorry("Runtime word splitting NYI");
        return;
    }

    my @tok = split ' ', $sl->text;
    @tok = map { Op::StringLiteral->new(node($M), text => $_) } @tok;

    $M->{_ast} = (@tok == 1) ? $tok[0] :
        Op::CallSub->new(node($M),
            invocant => Op::Lexical->new(name => '&infix:<,>'),
            positionals => \@tok);
    $M->{qpvalue} = '<' . join(" ", map { $_->text } @tok) . '>'; # XXX what if there are spaces or >
}
sub circumfix__S_LtLt_GtGt { goto &circumfix__S_Lt_Gt }
sub circumfix__S_Fre_Nch { goto &circumfix__S_Lt_Gt }

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

sub circumfix__S_sigil { my ($cl, $M) = @_;
    circumfix__S_Paren_Thesis($cl, $M); # XXX
    $M->{_ast} = $cl->docontext($M, $M->{sigil}->Str, $M->{_ast});
}

sub infixish { my ($cl, $M) = @_;
    if ($M->{colonpair}) {
        return; # handled in POST
    }

    if ($M->{infix_postfix_meta_operator}[0]) {
        # TODO: there should probably be at least a potential for others
        $M->{infix}{_ast} = Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&assignop'),
            args => [ $M->{infix}{_ast} ]);
    }
}

sub INFIX { my ($cl, $M) = @_;
    my $fn = $M->{infix}{_ast};
    my $s = $fn->isa('Op::Lexical') ? $fn->name : '';
    my ($st,$l,$r) = $cl->whatever_precheck($s, $M->{left}{_ast},
        $M->{right}{_ast});

    if ($s eq '&infix:<?? !!>') { # XXX macro
        $M->{_ast} = Op::Conditional->new(node($M), check => $l,
            true => $M->{middle}{_ast}, false => $r);
    } elsif ($s eq '&infix:<,>') {
        #XXX STD bug causes , in setting to be parsed as left assoc
        my @r;
        push @r, $l->isa('Op::SimpleParcel') ? @{ $l->items } : ($l);
        push @r, $r->isa('Op::SimpleParcel') ? @{ $r->items } : ($r);
        $M->{_ast} = Op::SimpleParcel->new(items => \@r);
    } else {
        $M->{_ast} = Op::CallSub->new(node($M), invocant => $fn,
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
    my @args;
    for my $i (0 .. scalar @{ $M->{chain} }) {
        if (($i % 2) == 0) {
            push @args, $M->{chain}[$i]{_ast};
        }
    }

    my ($st, @vargs) = $cl->whatever_precheck($op, @args);

    my @pairwise;
    while (@vargs >= 2) {
        push @pairwise, Op::CallSub->new(node($M),
                invocant => Op::Lexical->new(name => $op),
                positionals => [ $vargs[0], $vargs[1] ]);
        shift @vargs;
    }

    $M->{_ast} = (@pairwise > 1) ?  Op::ShortCircuit->new(node($M),
        kind => '&&', args => \@pairwise) : $pairwise[0];

    $M->{_ast} = $cl->whatever_postcheck($M, $st, $M->{_ast});
}

my %loose2tight = (
    '&&' => '&&', '||' => '||', '//' => '//', 'andthen' => 'andthen',
    'orelse' => '//', 'and' => '&&', 'or' => '||',
);
sub LIST { my ($cl, $M) = @_;
    if ($M->isa('STD::Regex')) {
        goto &LISTrx;
    }
    # STD guarantees that all elements of delims have the same sym
    # the last item may have an ast of undef due to nulltermish
    my $op  = $M->{delims}[0]{sym};
    my ($st, @pos) = $cl->whatever_precheck("&infix:<$op>",
        grep { defined } map { $_->{_ast} } @{ $M->{list} });

    if ($op eq ',') {
        $M->{_ast} = Op::SimpleParcel->new(node($M), items => \@pos);
    } elsif ($loose2tight{$op}) {
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
    } elsif ($op->{postcircumfix}) {
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => Op::Lexical->new(name => '&postcircumfix:<' .
                $op->{postcircumfix} . '>'),
            positionals => [ $arg, @{ $op->{args} } ]);
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
            args => $op->{args} // []);
    } elsif ($op->{name}) {
        $M->{_ast} = Op::CallMethod->new(node($M),
            receiver => $arg,
            name => ($op->{private} ? '!' . $op->{name} : $op->{name}),
            args => $op->{args} // []);
    } elsif ($op->{postcall}) {
        if (@{ $op->{postcall} } > 1) {
            $M->sorry("Slicels NYI");
            return;
        }
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => $arg,
            args => ($op->{postcall}[0] // []));
    } elsif ($M->{colonpair}) {
        if ($arg->isa('Op::CallLike')) {
            $M->{_ast} = $arg->adverb($M->{colonpair}{_ast}{term});
        } else {
            $M->sorry("You can't adverb that");
            return;
        }
    } else {
        say join(" ", %$M);
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

sub infix { my ($cl, $M) = @_;
    $M->{_ast} = Op::Lexical->new(name => '&infix:<' . $M->{sym} . '>');
}
sub infix__S_ANY { }

# hacked in infixish - = is the only one
sub infix_postfix_meta_operator { }
sub infix_postfix_meta_operator__S_Equal { }

sub prefix { }
sub prefix__S_ANY { }

sub postfix { }
sub postfix__S_ANY { }

sub postcircumfix { }
sub postcircumfix__S_Paren_Thesis { my ($cl, $M) = @_;
    $M->{_ast} = { postcall => $M->{semiarglist}{_ast} };
}

sub semilist_to_args { my ($cl, $M) = @_;
    if (@{ $M->{_ast} } > 1) {
        $M->sorry('Slice lookups NYI');
        return;
    }
    my ($al) = @{ $M->{_ast} };

    if (!defined $al) {
        return [];
    } elsif ($al && $al->isa('Op::SimpleParcel')) {
        return $al->items;
    } else {
        return [$al];
    }
}

sub postcircumfix__S_Bra_Ket { my ($cl, $M) = @_;
    $M->{_ast} = { postcircumfix => '[ ]',
        args => $cl->semilist_to_args($M->{semilist}) };
}
sub postcircumfix__S_Cur_Ly { my ($cl, $M) = @_;
    $M->{_ast} = { postcircumfix => '{ }',
        args => $cl->semilist_to_args($M->{semilist}) };
}
sub postcircumfix__S_Lt_Gt { my ($cl, $M) = @_;
    $cl->circumfix__S_Lt_Gt($M); #XXX
    delete $M->{qpvalue};
    $M->{_ast} = { postcircumfix => '{ }',
        args => [ $M->{_ast} ] };
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
    $r{name}  = $cl->unqual_longname($M->{longname},
        "Qualified method calls NYI") if $M->{longname};
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
    $M->{_ast} = $M->{postop}{_ast} if $M->{postop};
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

sub colonpair { my ($cl, $M) = @_;
    my $k = $M->{k};
    # STD seems to think term:name is term:sym<name>.  Needs speccy
    # clarification.  XXX
    my $n;
    if (!ref $M->{v}) {
        $n = ":" . ($M->{v} ? '' : '!') . $M->{k};
    } elsif (defined $M->{v}{qpvalue}) {
        $n = ":" . $M->{k} . $M->{v}{qpvalue};
    }
    my $tv = ref($M->{v}) ? $M->{v}{_ast} :
        Op::Lexical->new(name => $M->{v} ? 'True' : 'False');
    $M->{_ast} = { ext => $n, term => Op::SimplePair->new(
            key => $M->{k}, value => $tv) };
}

sub fatarrow { my ($cl, $M) = @_;
    $M->{_ast} = Op::SimplePair->new(
        key => $M->{key}->Str,
        value => $M->{val}{_ast});
}

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
    my ($id, $path) = @{ $cl->mangle_longname($M->{longname}) }{'name','path'};

    if ($M->{postcircumfix}[0] || $M->{args}) {
        $M->sorry("Unsupported form of term:name");
        return;
    }

    if ($path) {
        $M->{_ast} = Op::PackageVar->new(node($M), name => $id,
            slot => $cl->gensym, path => $path);
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
        args => $args);
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

sub term__S_colonpair { my ($cl, $M) = @_;
    if (@{ $M->{colonpair} } > 1) {
        $M->sorry("Multi colonpair syntax not yet understood"); #XXX
        return;
    }
    $M->{_ast} = $M->{colonpair}[0]{_ast}{term};
}

sub term__S_fatarrow { my ($cl, $M) = @_;
    $M->{_ast} = $M->{fatarrow}{_ast};
}

sub do_variable_reference { my ($cl, $M, $v) = @_;
    if ($v->{term}) {
        return $v->{term};
    }

    my $sl = $v->{sigil} . $v->{twigil} . $v->{name};

    if ($v->{rest} && $v->{twigil} =~ /[*=~?^:]/) {
        $M->sorry("Twigil " . $v->{twigil} . " cannot be used with " .
            "qualified names");
        return;
    }

    given ($v->{twigil}) {
        when ('!') {
            if ($v->{rest}) {
                $M->sorry('$!Foo::bar syntax NYI');
                return;
            }

            return Op::GetSlot->new(node($M), name => $v->{name},
                object => Op::Lexical->new(name => 'self'));
        }
        when ('.') {
            if ($v->{rest}) {
                $M->sorry('$.Foo::bar syntax NYI');
                return;
            }

            return Op::CallMethod->new(node($M), name => $v->{name},
                receiver => Op::Lexical->new(name => 'self'));
        }
        # no twigil in lex name for these
        when ({ '^' => 1, ":" => 1}) {
            return Op::Lexical->new(node($M), name => $v->{sigil} . $v->{name});
        }
        when ('?') {
            return Op::Lexical->new(node($M), name => $sl);
        }
        when ('*') {
            return Op::ContextVar->new(node($M), name => $sl);
        }
        when ('') {
            if ($v->{rest}) {
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

sub docontext { my ($cl, $M, $sigil, $term) = @_;
    if ($sigil !~ /[\$\@\%]/) {
        $M->sorry("Unhandled conext character $sigil");
    }
    my $method = ($sigil eq '$') ? 'item' :
                 ($sigil eq '@') ? 'list' :
                                   'hash';

    Op::CallMethod->new(node($M), name => $method, receiver => $term);
}

sub variable { my ($cl, $M) = @_;
    my $sigil = $M->{sigil} ? $M->{sigil}->Str : substr($M->Str, 0, 1);
    my $twigil = $M->{twigil}[0] ? $M->{twigil}[0]{sym} : '';

    my ($name, $rest);
    my $dsosl = ($M->{desigilname} || $M->{sublongname} || {})->{_ast};
    if ($dsosl && $dsosl->{ind}) {
        $M->{_ast} = { term => $cl->docontext($M, $sigil, $dsosl->{ind}) };
        return;
    } elsif ($dsosl) {
        ($name, $rest) = @$dsosl{'name', 'path'};
    } elsif ($M->{name}[0]) {
        # Both these cases are marked XXX in STD.  I agree.  What are they for?
        if ($M->{name}[0]{dc}) {
            $M->sorry("*ONE* pair of leading colons SHALL BE ENOUGH");
            return;
        }
        if ($M->Str =~ /^\$::/) {
            $rest = $M->{name}[0]{_ast}{names};
            $name = pop @$rest;
        } else {
            if (@{ $M->{name}[0]{_ast}{names} } > 1) {
                $M->sorry("Nonsensical attempt to qualify a self-declared named parameter detected");
                return;
            }
            $name = $M->{name}[0]{_ast}{names}[0];
            $twigil = ':';
        }
    } elsif ($M->{special_variable}) {
        $name = substr($M->{special_variable}->Str, 1);
    } elsif ($M->{index}) {
        $M->{_ast} = { term =>
            # maybe a little of a cheat
            $M->{_ast} = Op::CallMethod->new(node($M), name => 'at-pos',
                receiver => Op::Lexical->new(name => '$/'),
                positionals => [ Op::Num->new(value => $M->{index}{_ast}) ])
        };
        return;
    } elsif ($M->{postcircumfix}[0]) {
        if ($M->{postcircumfix}[0]{sym} eq '< >') {
            $M->{_ast} = { term =>
                # maybe a little of a cheat
                $M->{_ast} = Op::CallMethod->new(node($M), name => 'at-key',
                    receiver => Op::Lexical->new(name => '$/'),
                    positionals => $M->{postcircumfix}[0]{_ast}{args})
            };
            return;
        } else {
            $M->sorry("Contextualizer variables NYI");
            return;
        }
    } else {
        say join " ", %$M;
        $M->sorry("Non-simple variables NYI");
        return;
    }

    $M->{_ast} = {
        sigil => $sigil, twigil => $twigil, name => $name, rest => $rest
    };
}

sub special_variable {}
sub special_variable__S_DollarSlash {}
sub special_variable__S_Dollar_a2_ {}

sub param_sep {}

# :: { list : Bool, hash : Bool  slot : Maybe[Str], names : [Str] }
sub named_param { my ($cl, $M) = @_;
    my %rt;
    if ($M->{name}) {
        if ($M->{named_param}) {
            %rt = %{ $M->{named_param}{_ast} };
        } else {
            %rt = %{ $M->{param_var}{_ast} };
        }
        $rt{names} = [ @{ $rt{names} // [] }, $M->{name}->Str ];
    } else {
        %rt = %{ $M->{param_var}{_ast} };
        if ($rt{slot} && $rt{slot} =~ /^[\@\$\%][.*!]?(.*)/) {
            $rt{names} = [ $1 ];
        } else {
            $M->sorry("Abbreviated named parameter must have a name");
        }
    }
    $rt{positional} = 0;
    $M->{_ast} = \%rt;
}

# :: { list : Bool, hash : Bool, slot : Maybe[Str] }
sub param_var { my ($cl, $M) = @_;
    if ($M->{signature}) {
        $M->sorry('Sub-signatures NYI');
        return;
    }
    my $twigil = $M->{twigil}[0] ? $M->{twigil}[0]->Str : '';
    my $sigil = $M->{sigil}->Str;
    if ($twigil || ($sigil ne '$' && $sigil ne '@' && $sigil ne '%')) {
        $M->sorry('Non bare scalar targets NYI');
        return;
    }
    $M->{_ast} = { list => ($sigil eq '@'), hash => ($sigil eq '%'), slot =>
        $M->{name}[0] ? ($sigil . $M->{name}[0]->Str) : undef };
}

# :: Sig::Parameter
sub parameter { my ($cl, $M) = @_;
    if (@{ $M->{trait} } > 0) {
        $M->sorry('Parameter traits NYI');
        return;
    }

    if (@{ $M->{post_constraint} } > 0) {
        $M->sorry('Parameter post constraints NYI');
        return;
    }

    my $default = $M->{default_value}[0] ? $M->{default_value}[0]{_ast} : undef;

    my $sorry;
    my $slurpy;
    my $optional;
    given ($M->{quant} . ':' . $M->{kind}) {
        when ('**:*') { $sorry = "Slice parameters NYI" }
        when ('*:*')  { $slurpy = 1 }
        when ('|:*')  { $sorry = "Captures NYI" }
        when ('\\:!') { $sorry = "Simple parcel parameters NYI" }
        when ('\\:?') { $sorry = "Simple parcel parameters NYI" }
        when (':!')   { }
        when (':*')   { $optional = 1 }
        when (':?')   { $optional = 1 }
        when ('?:?')  { $optional = 1 }
        when ('!:!')  { }
        when ('!:?')  { $optional = 1 }
        when ('!:*')  { }
        default       { $sorry = "Confusing parameters ($_)" }
    }
    if ($sorry) { $M->sorry($sorry); return }
    my $p = $M->{param_var} // $M->{named_param};

    $M->{_ast} = Sig::Parameter->new(name => $M->Str, default => $default,
        optional => $optional, slurpy => $slurpy, %{ $p->{_ast} });
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

sub cgexp__S_p6exp { my ($cl, $M) = @_;
    $M->{_ast} = $M->{statementlist}{_ast};
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
    my @p = @{ $opshortcut{$l} // [ $l ] };
    $M->{_ast} = [@p, map { $_->{_ast} } @{ $M->{cgexp} }];
}

sub apostrophe {}
sub quibble { my ($cl, $M) = @_;
    if ($M->{babble}{B}[0]{_herelang}) { #XXX
        $M->{_ast} = Op::HereStub->new(node => $M);
    } else {
        $M->{_ast} = $M->{nibble}{_ast};
    }
}
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
sub embeddedblock { my ($cl, $M) = @_;
    $M->{_ast} = $cl->block_to_immediate($M, 'bare',
        $cl->sl_to_block('bare', $M->{statementlist}{_ast},
            signature => Sig->simple()));
}

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
sub terminator__S_if {}
sub terminator__S_unless {}
sub terminator__S_for {}
sub terminator__S_until {}
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
sub scope_declarator__S_supersede {}
sub scope_declarator__S_has {}
sub scope_declarator__S_state {}
sub scope_declarator__S_anon {}

sub multi_declarator { my ($cl, $M) = @_;
    $M->{_ast} = ($M->{declarator} // $M->{routine_def})->{_ast};
}
sub multi_declarator__S_multi {}
sub multi_declarator__S_proto {}
sub multi_declarator__S_only  {}

sub variable_declarator { my ($cl, $M) = @_;
    if ($::MULTINESS) {
        $M->sorry("Multi variables NYI");
    }
    if ($M->{trait}[0] || $M->{post_constraint}[0] || $M->{shape}[0]) {
        $M->sorry("Traits, postconstraints, and shapes on variable declarators NYI");
        return;
    }

    my $scope = $::SCOPE // 'my';

    if ($scope eq 'augment' || $scope eq 'supersede') {
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

    if ($v->{rest}) {
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
            list => ($v->{sigil} eq '@'), hash => ($v->{sigil} eq '%'));
    } elsif ($scope eq 'our') {
        $M->{_ast} = Op::PackageVar->new(node($M), name => $slot, slot => $slot,
            path => [ 'OUR' ]);
    } else {
        $M->{_ast} = Op::Lexical->new(node($M), name => $slot, declaring => 1,
            list => ($v->{sigil} eq '@'), hash => ($v->{sigil} eq '%'));
    }
}

sub type_declarator {}
sub type_declarator__S_constant { my ($cl, $M) = @_;
    if ($::MULTINESS) {
        $M->sorry("Multi variables NYI");
    }
    my $scope = $::SCOPE // 'my';
    if (!$M->{identifier} && !$M->{variable}) {
        $M->sorry("Anonymous constants NYI"); #wtf?
        return;
    }
    my $slot  = ($M->{identifier} // $M->{variable})->Str;

    # This is a cheat.  Constants should be, well, constant, and we should be
    # using the phaser rewrite mechanism to get the initializer here.  XXX
    # terms need to use a context hash.
    $M->{_ast} = ($::SCOPE eq 'our') ?
        Op::PackageVar->new(node($M), name => $slot, slot => $slot,
            path => [ 'OUR' ]) :
        Op::Lexical->new(node($M), name => $slot, declaring => 1);
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
sub modifier_expr { my ($cl, $M) = @_;
    $M->{_ast} = $M->{EXPR}{_ast};
}
sub default_value { my ($cl, $M) = @_;
    $M->{_ast} = $M->{EXPR}{_ast};
}

sub arglist { my ($cl, $M) = @_;
    $M->sorry("Invocant handling is NYI") if $::INVOCANT_IS;
    my $x = $M->{EXPR}{_ast};

    if (!defined $x) {
        $M->{_ast} = [];
    } elsif ($x && $x->isa('Op::SimpleParcel')) {
        $M->{_ast} = $x->items;
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
    if ($M->{label}) {
        $M->sorry("Labels are NYI");
        return;
    }

    $M->{_ast} = $M->{statement_control} ? $M->{statement_control}{_ast} :
                 $M->{EXPR} ? $M->{EXPR}{_ast} : undef;

    if ($M->{statement_mod_cond}[0]) {
        my ($sym, $exp) = @{ $M->{statement_mod_cond}[0]{_ast} };

        if ($sym eq 'if') {
            $M->{_ast} = Op::Conditional->new(node($M), check => $exp,
                true => $M->{_ast}, false => undef);
        } elsif ($sym eq 'unless') {
            $M->{_ast} = Op::Conditional->new(node($M), check => $exp,
                false => $M->{_ast}, true => undef);
        } else {
            $M->sorry("Unhandled statement modifier $sym");
            return;
        }
    }

    if ($M->{statement_mod_loop}[0]) {
        my ($sym, $exp) = @{ $M->{statement_mod_loop}[0]{_ast} };

        if ($sym eq 'while') {
            $M->{_ast} = Op::WhileLoop->new(node($M), check => $exp,
                body => $M->{_ast}, until => 0, once => 0);
        } elsif ($sym eq 'until') {
            $M->{_ast} = Op::WhileLoop->new(node($M), check => $exp,
                body => $M->{_ast}, until => 1, once => 0);
        } else {
            $M->sorry("Unhandled statement modifier $sym");
            return;
        }
    }
}

sub statement_mod_cond { my ($cl, $M) = @_;
    $M->{_ast} = [ $M->{sym}, $M->{modifier_expr}{_ast} ];
}
sub statement_mod_loop { my ($cl, $M) = @_;
    $M->{_ast} = [ $M->{sym}, $M->{modifier_expr}{_ast} ];
}

sub statement_mod_cond__S_if {}
sub statement_mod_cond__S_unless {}
sub statement_mod_cond__S_when {}
sub statement_mod_loop__S_while {}
sub statement_mod_loop__S_until {}
sub statement_mod_loop__S_for {}
sub statement_mod_loop__S_given {}

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
    # TODO: should use 'once'
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

    my $meta = CompilerDriver::metadata_for($name);
    $::UNITREFS{$name} = 1;
    $::UNITREFSTRANS{$name} = 1;
    %::UNITDEPSTRANS = (%::UNITDEPSTRANS, %{ $meta->{deps} });
    %::UNITREFSTRANS = (%::UNITREFSTRANS, %{ $meta->{trefs} });
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
    if ($::MULTINESS) {
        $M->sorry("Multi variables NYI");
    }
    my $scope = $::SCOPE;
    if (!$M->{longname}[0]) {
        $scope = 'anon';
    }
    if ($scope eq 'augment' || $scope eq 'supersede') {
        $M->sorry('Monkey typing is not yet supported');
        return;
    }
    if ($scope eq 'has' || $scope eq 'state') {
        $M->sorry("Illogical scope $scope for package block");
        return;
    }
    my $name = $M->{longname}[0] ?
        $cl->unqual_longname($M->{longname}[0],
            "Qualified package definitions NYI", 1) : 'ANON';
    my $outervar = $scope ne 'anon' ? $name : $cl->gensym;

    my $optype = 'Op::' . ucfirst($::PKGDECL) . 'Def';
    my $blocktype = $::PKGDECL;
    my $bodyvar = $cl->gensym;
    # currently always install into the local stash
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
    } elsif ($trait eq 'rawcall') {
        $M->{_ast} = { nobinder => 1 };
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
        ($type eq 'mainline' ? (
                file => $::FILE->{name},
                text => $::ORIG) : ()),
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
        invocant => $cl->block_to_closure($M, $blk, once => 1),
        positionals => []);
}

sub block_to_closure { my ($cl, $M, $blk, %args) = @_;
    my $outer_key = $args{outer_key} // $cl->gensym;

    Op::SubDef->new(var => $outer_key, body => $blk, node($M),
        once => $args{once}, method_too => $args{method_too},
        exports => ($args{exports} // []));
}

sub get_placeholder_sig { my ($cl, $M) = @_;
    # for some reason, STD wants to deparse this
    my @things = split ", ", $::CURLEX->{'$?SIGNATURE'};
    shift @things if $things[0] eq '';
    my @parms;
    for (@things) {
        if ($_ =~ /^\$_ is ref/) {
            push @parms, Sig::Parameter->new(optional => 1,
                slot => '$_', name => '$_');
        } elsif ($_ eq '*@_') {
            push @parms, Sig::Parameter->new(slurpy => 1, slot => '@_',
                list => 1, name => '*@_');
        } elsif ($_ =~ /^([@\$])/) {
            push @parms, Sig::Parameter->new(slot => $_, name => $_,
                list => ($1 eq '@'));
        } else {
            $M->sorry('Named placeholder parameters NYI');
            return;
        }
    }
    return Sig->new(params => \@parms);
}

# always a sub, though sometimes it's an implied sub after multi/proto/only
sub routine_def { my ($cl, $M) = @_;
    if ($::MULTINESS) {
        $M->sorry("Multi routines NYI");
    }
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
    my $signature = $M->{multisig}[0] ? $M->{multisig}[0]{_ast} :
        $cl->get_placeholder_sig($M);
    for my $t (@{ $M->{trait} }) {
        if ($t->{_ast}{export}) {
            push @export, @{ $t->{_ast}{export} };
        } elsif ($t->{_ast}{nobinder}) {
            $signature = undef;
        } else {
            $M->sorry('Non-export sub traits NYI');
        }
    }
    my $scope = !$dln ? 'anon' : $::SCOPE || 'my';
    my ($m,$p) = $dln ? @{$cl->mangle_longname($dln)}{'name','path' } : ();

    if ($scope ne 'my' && $scope ne 'our' && $scope ne 'anon') {
        $M->sorry("Illegal scope $scope for subroutine");
        return;
    }
    if ($scope eq 'our') {
        $M->sorry('Package subs NYI');
        return;
    } elsif ($p) {
        $M->sorry('Defining a non-our sub with a package-qualified name makes no sense');
        return;
    }

    $M->{_ast} = $cl->block_to_closure($M,
            $cl->sl_to_block('sub',
                $M->{blockoid}{_ast},
                subname => $m,
                signature => $signature),
        outer_key => (($scope eq 'my') ? "&$m" : undef),
        exports => \@export);
}

sub method_def { my ($cl, $M) = @_;
    my $scope = $::SCOPE // 'has';
    my $type = $M->{type} ? $M->{type}->Str : '';
    $scope = 'anon' if !$M->{longname};
    my $name = $M->{longname} ? $cl->unqual_longname($M->{longname},
        "Qualified method definitions not understood") : undef; #XXX

    if ($M->{sigil}) {
        $M->sorry("Method sgils NYI");
        return;
    }
    if ($type eq '^') {
        $M->sorry("Metamethod mixins NYI");
        return;
    }
    if (@{ $M->{multisig} } > 1) {
        $M->sorry("Multiple multisigs (what?) NYI");
        return;
    }

    my $sym = ($scope eq 'my') ? ('&' . $name) : $cl->gensym;

    if ($scope eq 'augment' || $scope eq 'supersede' || $scope eq 'state') {
        $M->sorry("Illogical scope $scope for method");
        return;
    }

    if ($scope eq 'our') {
        $M->sorry("Packages NYI");
        return;
    }
    my $sig = $M->{multisig}[0] ? $M->{multisig}[0]{_ast} :
        $cl->get_placeholder_sig($M);

    for my $t (@{ $M->{trait} }) {
        if ($t->{_ast}{nobinder}) {
            $sig = undef;
        } else {
            $M->sorry("NYI method trait " . $M->Str);
        }
    }

    my $bl = $cl->sl_to_block('sub', $M->{blockoid}{_ast},
        subname => $name,
        signature => $sig ? $sig->for_method : undef);

    $M->{_ast} = $cl->block_to_closure($M, $bl, outer_key => $sym,
        method_too => ($scope ne 'anon' ? "$type$name" : undef));
}

sub block { my ($cl, $M) = @_;
    $M->{_ast} = $cl->sl_to_block('', $M->{blockoid}{_ast});
}

# :: Body
sub pblock { my ($cl, $M) = @_;
    my $rw = $M->{lambda} && $M->{lambda}->Str eq '<->';
    $M->{_ast} = $cl->sl_to_block('', $M->{blockoid}{_ast},
        signature => ($M->{signature} ? $M->{signature}{_ast} :
            $cl->get_placeholder_sig($M)));
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
sub statement_prefix__S_gather { my ($cl, $M) = @_;
    $M->{blast}{_ast}->type('gather');
    $M->{_ast} = Op::Gather->new(node($M), var => $cl->gensym,
        body => $M->{blast}{_ast});
}
sub statement_prefix__S_PREMinusINIT { my ($cl, $M) = @_;
    my $var = $cl->gensym;

    $M->{blast}{_ast}->type('phaser');

    $M->{_ast} = Op::PreInit->new(var => $var, body => $M->{blast}{_ast},
        node($M));
}

sub statement_prefix__S_START { my ($cl, $M) = @_;
    my $cv = $cl->gensym;
    $M->{_ast} = Op::Start->new(node($M), condvar => $cv, body =>
        $cl->block_to_immediate($M, 'phaser', $M->{blast}{_ast}));
}

sub comp_unit { my ($cl, $M) = @_;
    my $body;
    my $sl = $M->{statementlist}{_ast};

    if (!$::YOU_WERE_HERE && $::UNITNAME ne 'MAIN') {
        $sl = Op::StatementList->new(node($M), children => [ $sl,
                Op::YouAreHere->new(save_only => 1, unitname => $::UNITNAME)]);
    }

    $body = $cl->sl_to_block('mainline', $sl, subname => 'mainline');
    if ($::YOU_WERE_HERE) {
        $body = Body->new(
            type => 'mainline', file => '(generated)', text => '',
            name => 'install',
            signature => Sig->new(params => [
                    Sig::Parameter->new(name => '!mainline',
                        slot => '!mainline')]),
            do => Op::CallSub->new(node($M),
                invocant => Op::CgOp->new(optree => [ 'newscalar',
                        [ 'rawsget', $::SETTINGNAME . ".Installer" ] ]),
                positionals => [Op::SubDef->new(
                    var => $cl->gensym, body => $body)]));
    }

    my $sn = $::SETTINGNAME; $sn =~ s/::/./g;
    $M->{_ast} = Unit->new(mainline => $body, name => $::UNITNAME,
        ($::SETTING_RESUME ? (setting => $::SETTING_RESUME) : ()),
        is_setting => (!!$::YOU_WERE_HERE),
        setting_name => $sn);
}

1;
