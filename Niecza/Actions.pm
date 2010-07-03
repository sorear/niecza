package Niecza::Actions;
use 5.010;
use strict;
use warnings;

use Op;
use Body;
use Unit;

our $AUTOLOAD;
my %carped;
sub AUTOLOAD {
    my ($cl, $M) = @_;
    if ($AUTOLOAD =~ /^Niecza::Actions::(.*)__S_\d\d\d(.*)$/) {
        # TODO: Change CursorBase so this doesn't happen.
        my $m = "$1__S_$2";
        return $cl->$m($M);
    }
    $M->sorry("Action method $AUTOLOAD not yet implemented") unless $carped{$AUTOLOAD}++;
}

sub ws { }
sub vws { }
sub unv { }
sub comment { }
sub comment__S_Sharp { }
sub spacey { }
sub nofun { }
sub curlycheck { }

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
    $M->sorry("Num is NYI");
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
    $M->{_ast} = $M->{identifier} ? $M->{identifier}{_ast} : $M->{EXPR}{_ast};
}

# { dc: Bool, names: [Either String Op] }
sub name { my ($cl, $M) = @_;
    my @names = map { $_->{_ast} } @{ $M->{morename} };
    unshift @names, $M->{identifier}{_ast} if $M->{identifier};
    $M->{_ast} = { dc => !($M->{identifier}), names => \@names };
}

sub longname {} # look at the children yourself
sub deflongname {}

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

sub nibbler { my ($cl, $M) = @_;
    if ($M->isa('STD::Regex')) {
        $M->{_ast} = $M->{EXPR}{_ast};
    } elsif ($M->isa('Niecza::Grammar::NIL')) {
        $M->{_ast} = Op::NIL->new(code => [map { @{$_->{_ast}} } @{$M->{insn}}]);
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
        $M->{_ast} = Op::StringLiteral->new(text => $str);
    }
}

# term :: Op
sub term { }

sub term__S_value { my ($cl, $M) = @_;
    $M->{_ast} = $M->{value}{_ast};
}

sub term__S_identifier { my ($cl, $M) = @_;
    my $id  = $M->{identifier}{_ast};
    my $sal = $M->{args}{_ast};

    if (@$sal > 1) {
        $M->sorry("Slicel lists are NYI");
        return;
    }

    my $args = $sal->[0] // [];

    $M->{_ast} = Op::CallSub->new(
        invocant => Op::Lexical->new(name => '&' . $id),
        positionals => $args);
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
    $M->{_ast} = $M->{variable}{_ast};
}

sub term__S_DotDotDot { my ($cl, $M) = @_;
    $M->{_ast} = Op::Yada->new(kind => '...');
}

sub term__S_BangBangBang { my ($cl, $M) = @_;
    $M->{_ast} = Op::Yada->new(kind => '!!!');
}

sub term__S_QuestionQuestionQuestion { my ($cl, $M) = @_;
    $M->{_ast} = Op::Yada->new(kind => '???');
}

sub voidmark { my ($cl, $M) = @_;
    $M->{_ast} = 1;
}

sub up { my ($cl, $M) = @_;
    $M->{_ast} = length ($M->Str);
}

sub lexdecl { my ($cl, $M) = @_;
    $M->{_ast} = [ map { $_, $M->{clrid}->Str } @{ $M->{varid} } ];
}

# :: [row of NIL op]
sub insn {}
sub insn__S_lextypes { my ($cl, $M) = @_;
    $M->{_ast} = [[ lextypes => [ map { @{ $_->{_ast} } } @{ $M->{lexdecl} } ] ]];
}

sub insn__S_clone_lex { my ($cl, $M) = @_;
    $M->{_ast} = [ map { [ clone_lex => $_->Str ] } @{ $M->{varid} } ];
}

sub insn__S_copy_lex { my ($cl, $M) = @_;
    $M->{_ast} = [ map { [ copy_lex => $_->Str ] } @{ $M->{varid} } ];
}

sub insn__S_string_lv { my ($cl, $M) = @_;
    if (!$M->{quote}{_ast}->isa('Op::StringLiteral')) {
        $M->sorry("Strings used in NIL code must be compile time constants");
    }
    $M->{_ast} = [[ string_lv => $M->{quote}{_ast}->text ]];
}

sub insn__S_clr_string { my ($cl, $M) = @_;
    if (!$M->{quote}{_ast}->isa('Op::StringLiteral')) {
        $M->sorry("Strings used in NIL code must be compile time constants");
    }
    $M->{_ast} = [[ clr_string => $M->{quote}{_ast}->text ]];
}

# the negatives here are somewhat of a cheat.
sub insn__S_label { my ($cl, $M) = @_;
    $M->{_ast} = [[ labelhere => -$M->{decint}{_ast} ]];
}

sub insn__S_goto { my ($cl, $M) = @_;
    $M->{_ast} = [[ goto => -$M->{decint}{_ast} ]];
}

sub insn__S_lex_lv { my ($cl, $M) = @_;
    $M->{_ast} = [[ lex_lv => $M->{up}{_ast}, $M->{varid}->Str ]];
}

sub insn__S_rawlexget { my ($cl, $M) = @_;
    $M->{_ast} = [[ rawlexget => $M->{up}{_ast}, $M->{varid}->Str ]];
}

sub insn__S_rawlexput { my ($cl, $M) = @_;
    $M->{_ast} = [[ rawlexput => $M->{up}{_ast}, $M->{varid}->Str ]];
}

sub insn__S_how { my ($cl, $M) = @_;
    $M->{_ast} = [[ 'how' ]];
}

sub insn__S_fetchlv { my ($cl, $M) = @_;
    $M->{_ast} = [[ 'fetchlv' ]];
}

sub insn__S_dup_fetchlv { my ($cl, $M) = @_;
    $M->{_ast} = [[ 'dup_fetchlv' ]];
}

sub insn__S_pos { my ($cl, $M) = @_;
    $M->{_ast} = [[ pos => $M->{decint}{_ast} ]];
}

sub insn__S_call_method { my ($cl, $M) = @_;
    $M->{_ast} = [[ call_method => !$M->{voidmark}, $M->{identifier}->Str,
            $M->{decint}{_ast} ]];
}

sub insn__S_call_sub { my ($cl, $M) = @_;
    $M->{_ast} = [[ call_sub => !$M->{voidmark}, $M->{decint}{_ast} ]];
}

sub insn__S_tail_call_sub { my ($cl, $M) = @_;
    $M->{_ast} = [[ tail_call_sub => $M->{decint}{_ast} ]];
}

sub insn__S_clr_call_direct { my ($cl, $M) = @_;
    $M->{_ast} = [[ clr_call_direct => $M->{clrid}->Str, $M->{decint}{_ast} ]];
}

sub insn__S_unwrap { my ($cl, $M) = @_;
    $M->{_ast} = [[ clr_unwrap => $M->{clrid}->Str ]];
}

sub insn__S_new { my ($cl, $M) = @_;
    $M->{_ast} = [[ clr_new => $M->{clrid}->Str, $M->{decint}{_ast} ]];
}

sub insn__S_clr_field_get { my ($cl, $M) = @_;
    $M->{_ast} = [[ clr_field_get => $M->{varid}->Str ]];
}

sub insn__S_clr_field_set { my ($cl, $M) = @_;
    $M->{_ast} = [[ clr_field_set => $M->{varid}->Str ]];
}

sub insn__S_clr_index_get { my ($cl, $M) = @_;
    $M->{_ast} = [[ clr_index_get => ($M->{varid}[0] ? ($M->{varid}[0]->Str) : ()) ]];
}

sub insn__S_clr_index_set { my ($cl, $M) = @_;
    $M->{_ast} = [[ clr_index_set => ($M->{varid}[0] ? ($M->{varid}[0]->Str) : ()) ]];
}

sub insn__S_cast { my ($cl, $M) = @_;
    $M->{_ast} = [[ cast => $M->{clrid}->Str ]];
}

sub insn__S_return { my ($cl, $M) = @_;
    $M->{_ast} = [[ return => $M->{0} ]];
}

sub insn__S_push_null { my ($cl, $M) = @_;
    $M->{_ast} = [[ push_null => $M->{clrid}->Str ]];
}

sub clrid {}
sub varid {}
sub apostrophe {}
sub quibble {}
sub tribble {}
sub babble {}
sub quotepair {}

# We can't do much at blockoid reduce time because the context is unknown.
# Roles and subs need somewhat different code gen
sub blockoid { my ($cl, $M) = @_;
    $M->{_ast} = $M->{statementlist}{_ast};
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
sub stdstopper {}
sub unitstopper {}
sub eat_terminator {}

sub scoped { my ($cl, $M) = @_;
    $M->{_ast} = ($M->{declarator} // $M->{regex_declarator} //
        $M->{package_declarator} // $M->{multi_declarator})->{_ast};
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

sub termish {}
sub EXPR {}

sub arglist { my ($cl, $M) = @_;
    $M->sorry("Invocant handling is NYI") if $::INVOCANT_IS;
    my $x = $M->{EXPR}{_ast};

    if ($x && $x->isa('Op::SubCall') && $x->splittable_parcel) {
        $M->{_ast} = $x->positionals;
    } else {
        $M->{_ast} = [$x];
    }
}

sub semiarglist { my ($cl, $M) = @_;
    $M->{_ast} = [ map { $_->{_ast} } @{ $M->{arglist} } ];
}

sub args { my ($cl, $M) = @_;
    if ($M->{semiarglist} && $M->{arglist}[0]) {
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

    $M->{_ast} = $M->{EXPR} ? $M->{EXPR}{_ast} : undef;
}

sub statementlist { my ($cl, $M) = @_;
    $M->{_ast} = Op::StatementList->new(children => 
        [ grep { defined $_ } map { $_->{_ast} } @{ $M->{statement} } ]);
}

sub package_def { my ($cl, $M) = @_;
    if ($M->{longname}[0] && $::SCOPE ne 'my') {
        $M->sorry('Non-lexical class definitions are not yet supported');
        return;
    }
    if (!$M->{decl}{stub}) {
        $M->sorry('Non-stub class definitions are not yet supported');
        return;
    }
    # allocate a slot
    $::CURLEX->{'!slots'}{$M->{decl}{name}} = 1;
}

sub routine_declarator {}
sub routine_declarator__S_sub { my ($cl, $M) = @_;
    $M->{_ast} = $M->{routine_def}{_ast};
}

my $next_anon_id = 0;
sub gensym { 'anon_' . ($next_anon_id++) }

sub sl_to_block { my ($cl, $ast, %args) = @_;
    my $subname = $args{subname} // 'ANON';
    Body->new(
        name    => $subname,
        $args{bare} ? () : (
            protos  => ($::CURLEX->{'!preinit'} // []),
            enter   => ($::CURLEX->{'!enter'} // []),
            lexical => ($::CURLEX->{'!slots'} // {})),
        do      => $ast);
}

sub block_to_closure { my ($cl, $blk, %args) = @_;
    my $outer = $STD::ALL->{ $::CURLEX->{'OUTER::'}[0] };
    my $outer_key = $args{outer_key} // $cl->gensym;

    $outer->{'!slots'}{$outer_key} = 1 if $outer;

    unless ($args{stub}) {
        push @{ $outer->{'!preinit'} //= [] },
            [ 0, $outer_key, $blk ] if $outer;
        push @{ $outer->{'!enter'} //= [] },
            Op::CloneSub->new(name => $outer_key) if $outer;
    }

    Op::Lexical->new(name => $outer_key);
}

# always a sub, though sometimes it's an implied sub after multi/proto/only
sub routine_def { my ($cl, $M) = @_;
    if ($M->{sigil}[0] && $M->{sigil}[0]->Str eq '&*') {
        $M->sorry("Contextuals NYI");
        return;
    }
    my $dln = $M->{deflongname}[0];
    if ($dln && ($dln->{colonpair}[0] || $dln->{name}{_ast}{dc} ||
            @{$dln->{name}{_ast}{names}} > 1)) {
        $M->sorry("Fancy names NYI");
        return;
    }
    if ($M->{multisig}[0]) {
        $M->sorry("Signatures NYI");
        return;
    }
    if ($M->{trait}[0]) {
        $M->sorry("Sub traits NYI");
        return;
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

    $M->{_ast} = $cl->block_to_closure(
            $cl->sl_to_block($M->{blockoid}{_ast},
                subname => ($dln ? $dln->Str : undef)),
        stub => $dln && $M->{decl}{stub},
        outer_key => (($scope eq 'my') ? ('&' . $dln->Str) : undef));
}

sub block { my ($cl, $M) = @_;
    $M->{_ast} = $cl->sl_to_block($M->{blockoid}{_ast});
}

# returns Body of 0 args
sub blast { my ($cl, $M) = @_;
    if ($M->{block}) {
        $M->{_ast} = $M->{block}{_ast};
    } else {
        $M->{_ast} = Body->new(
            name => 'ANON',
            do   => $M->{statement}{_ast});
    }
}

sub statement_prefix {}
sub statement_prefix__S_PREMinusINIT { my ($cl, $M) = @_;
    my $var = $cl->gensym;

    push @{ $::CURLEX->{'!preinit'} //= [] },
        [ 1, $var, $M->{blast}{_ast} ];
    push @{ $::CURLEX->{'!enter'} //= [] },
        Op::ShareLex->new(name => $var);

    $M->{_ast} = Op::Lexical->new(name => $var);
}

sub comp_unit { my ($cl, $M) = @_;
    my $body = $cl->sl_to_block($M->{statementlist}{_ast},
        subname => 'mainline');

    $M->{_ast} = Unit->new(mainline => $body);
}

1;
