package Optimizer::RxSimple;
use 5.010;
use utf8;
use strict;
use warnings;

sub run {
    my $r = $_[0]->rxsimp(0);
    $r, $r->mayback;
}

sub run_lad {
    my $lad = shift;
    my ($op, @zyg) = @$lad;
    if ($op eq 'Sequence') {
        my @ozyg;
        for my $z (@{ $zyg[0] }) {
            my $oz = run_lad($z);
            if ($oz->[0] eq 'Sequence') {
                push @ozyg, @{ $oz->[1] };
            } elsif ($oz->[0] eq 'Null') {
            } else {
                push @ozyg, $oz;
            }
        }
        for (my $ix = 0; $ix < @ozyg; ) {
            return ['None'] if ($ozyg[$ix][0] eq 'None');
            if ($ozyg[$ix][0] eq 'Imp') {
                $#ozyg = $ix;
                last;
            } elsif ($ix >= 1 && $ozyg[$ix][0] eq 'Str' &&
                    $ozyg[$ix-1][0] eq 'Str') {
                $ozyg[$ix-1][1] .= $ozyg[$ix][1];
                splice @ozyg, $ix, 1;
            } else {
                $ix++;
            }
        }
        if (@ozyg == 0) {
            return [ 'Null' ];
        } elsif (@ozyg == 1) {
            return $ozyg[0];
        } else {
            return [ 'Sequence', [ @ozyg ] ];
        }
    } else {
        return $lad;
    }
}

# XXX should use a multi sub.
sub RxOp::rxsimp { my ($self, $cut) = @_;
    my $selfp = bless { %$self }, ref($self);
    $selfp->{zyg} = [ map { $_->rxsimp(0) } @{ $self->zyg } ];
    $selfp;
}

sub RxOp::mayback { 1 }

sub RxOp::Sequence::rxsimp { my ($self, $cut) = @_;
    my @kids;
    for my $i (0 .. $#{ $self->zyg }) {
        my $k = $self->zyg->[$i];
        $k = $k->rxsimp($cut && $i == $#{ $self->zyg });
        if ($k->isa('RxOp::Sequence')) {
            push @kids, @{ $k->zyg };
        } else {
            push @kids, $k;
        }
    }
    NEGCC: {
        last NEGCC unless @kids;
        my @cond = @kids;
        my $fin = pop @cond;
        for (@cond) {
            last NEGCC unless $_->isa('RxOp::NotBefore') &&
                $_->zyg->[0]->isa('RxOp::CClassElem');
            $_ = $_->zyg->[0]->cc;
        }
        if ($fin->isa('RxOp::CClassElem')) {
            $fin = $fin->cc;
        } elsif ($fin->isa('RxOp::Any')) {
            $fin = $CClass::Full;
        } else {
            last NEGCC;
        }
        for (@cond) {
            $fin = $fin->minus($_);
        }
        return RxOp::CClassElem->new(cc => $fin);
    }
    (@kids == 1) ? $kids[0] : RxOp::Sequence->new(zyg => \@kids);
}

sub RxOp::Sequence::mayback { my ($self) = @_;
    for (@{ $self->zyg }) {
        return 1 if $_->mayback;
    }
    return 0;
}

sub RxOp::Alt::rxsimp { my ($self, $cut) = @_;
    my @kids = map { $_->rxsimp($cut) } @{ $self->zyg };
    return RxOp::Alt->new(
        lads => [ map { Optimizer::RxSimple::run_lad($_) } @{ $self->lads } ],
        dba => $self->dba,
        zyg => \@kids);
}

sub RxOp::Cut::rxsimp { my ($self, $cut) = @_;
    my $kid = $self->zyg->[0]->rxsimp(1);
    return $kid unless $kid->mayback;

    return RxOp::Cut->new(zyg => [$kid]);
}

sub RxOp::Subrule::rxsimp { my ($self, $cut) = @_;
    if ($cut) {
        return RxOp::Subrule->new(%$self, selfcut => 1);
    }
    return $self;
}

sub RxOp::Subrule::mayback { my ($self) = @_;
    return !$self->selfcut;
}

sub RxOp::Sigspace::rxsimp { my ($self, $cut) = @_;
    if ($cut) {
        return RxOp::Sigspace->new(selfcut => 1);
    }
    return $self;
}

sub RxOp::Sigspace::mayback { my ($self) = @_;
    return !$self->selfcut;
}

sub RxOp::Cut::mayback { 0 }
sub RxOp::VoidBlock::mayback { 0 }
sub RxOp::CheckBlock::mayback { 0 }
sub RxOp::Sym::mayback { 0 }
sub RxOp::VarString::mayback { 0 }
sub RxOp::ConfineLang::mayback { $_[0]->zyg->[0]->mayback }
sub RxOp::AfterCCs::mayback { 0 }
sub RxOp::SaveValue::mayback { 0 }
sub RxOp::Statement::mayback { 0 }
sub RxOp::String::mayback { 0 }
sub RxOp::Any::mayback { 0 }
sub RxOp::None::mayback { 0 }
sub RxOp::CClassElem::mayback { 0 }
sub RxOp::CutLTM::mayback { 0 }
sub RxOp::CutRule::mayback { 0 }
sub RxOp::SetLang::mayback { 0 }
sub RxOp::ZeroWidth::mayback { 0 }
sub RxOp::Statement::rxsimp { RxOp::Sequence->new(zyg => []) }
sub RxOp::BeforeString::mayback { 0 }
sub RxOp::NotBeforeString::mayback { 0 }
sub RxOp::Before::mayback { 0 }
# it's not uncommon to write <!before> and <?before>
sub RxOp::Before::rxsimp { my ($self, $cut) = @_;
    my $z = $self->zyg->[0]->rxsimp(1);
    if ($z->isa('RxOp::BeforeString')) {
        return $z;
    }
    if ($z->isa('RxOp::Before')) {
        return RxOp::Before->new(zyg => $z->zyg);
    }
    if ($z->isa('RxOp::String')) {
        return RxOp::BeforeString->new(str => $z->text);
    }
    return RxOp::Before->new(zyg => [ $z ]);
}
sub RxOp::NotBefore::mayback { 0 }
sub RxOp::NotBefore::rxsimp { my ($self, $cut) = @_;
    my $z = $self->zyg->[0]->rxsimp(1);
    if ($z->isa('RxOp::BeforeString')) {
        return RxOp::NotBeforeString->new(str => $z->str);
    }
    if ($z->isa('RxOp::Before')) {
        return RxOp::NotBefore->new(zyg => $z->zyg);
    }
    if ($z->isa('RxOp::String')) {
        return RxOp::NotBeforeString->new(str => $z->text);
    }
    return RxOp::NotBefore->new(zyg => [ $z ]);
}

sub RxOp::Quantifier::rxsimp { my ($self, $cut) = @_;
    my @z = map { $_->rxsimp(0) } @{ $self->zyg };
    if ($cut && @z == 1 && $z[0]->isa('RxOp::CClassElem')) {
        return RxOp::QuantCClass->new(cc => $z[0]->cc, min => $self->min,
            max => $self->max);
    }
    return RxOp::Quantifier->new(%$self, zyg => [@z]);
}

sub RxOp::QuantCClass::mayback { 0 }

1;
