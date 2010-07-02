package Niecza::Actions;
use 5.010;
use strict;
use warnings;

use Op;

our $AUTOLOAD;
sub AUTOLOAD {
    my ($cl, $M) = @_;
    if ($AUTOLOAD =~ /^Niecza::Actions::(.*)__S_\d\d\d(.*)$/) {
        # TODO: Change CursorBase so this doesn't happen.
        my $m = "$1__S_$2";
        return $cl->$m($M);
    }
    say "reduce $AUTOLOAD";
}

sub ws { }
sub vws { }

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
    $M->{_ast} = Op::StringLiteral->new(text => $M->{quote}{_ast});
}

sub ident { my ($cl, $M) = @_;
    $M->{_ast} = $M->Str;
}

sub identifier { my ($cl, $M) = @_;
    $M->{_ast} = $M->Str;
}

sub stopper { }

# quote :: Op
sub quote {}

sub quote__S_Double_Double { my ($cl, $M) = @_;
    my $str = "";
    for my $n (@{ $M->{nibble}{nibbles} }) {
        if ($n->isa('Str')) {
            $str .= $n->{TEXT};
        } else {
            $M->sorry("Non-literal contents of strings NYI");
        }
    }
    $M->{_ast} = $str;
}

sub nibbler { }

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

sub terminator {}
sub terminator__S_Thesis {}
sub stdstopper {}
sub unitstopper {}
sub eat_terminator {}

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

1;
