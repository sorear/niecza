our ($OptRxSimple);
class OptRxSimple;

use MONKEY_TYPING;
use RxOp;

method run($tree) {
    my $r = $tree.rxsimp(False);
    $r, $r.mayback;
}

method run_lad($lad) {
    my ($op, @zyg) = @$lad;
    if $op eq 'Sequence' {
        my @fzyg;
        for @( @zyg[0] ) -> $z {
            my $oz = self.run_lad($z);
            if $oz[0] eq 'Sequence' {
                push @fzyg, @( $oz[1] );
            } elsif $oz[0] eq 'Null' {
            } else {
                push @fzyg, $oz;
            }
        }
        my @ozyg;
        for @fzyg -> $z {
            return ['None'] if $z[0] eq 'None';
            if $z[0] eq 'Imp' {
                push @ozyg, $z;
                last;
            } elsif @ozyg && @ozyg[*-1][0] eq 'Str' && $z[0] eq 'Str' {
                @ozyg[*-1] = ['Str', @ozyg[*-1][1] ~ $z[1]];
            } else {
                push @ozyg, $z;
            }
        }
        if @ozyg == 0 {
            return [ 'Null' ];
        } elsif @ozyg == 1 {
            return @ozyg[0];
        } else {
            return [ 'Sequence', [ @ozyg ] ];
        }
    } else {
        return $lad;
    }
}

# XXX should use a multi sub.
augment class RxOp {
    method rxsimp($ ) {
        $!zyg = [ map *.rxsimp(False), @$.zyg ];
        self;
    }
    method mayback() { True }
}

augment class RxOp::Sequence { #OK exist
    method rxsimp($cut) {
        my @kids;
        my $cx = 1-@kids;
        for @$.zyg -> $k_ {
            my $k = $k_.rxsimp($cut && !($cx++));
            if $k.^isa(::RxOp::Sequence) {
                push @kids, @( $k.zyg );
            } else {
                push @kids, $k;
            }
        }
        (@kids == 1) ?? @kids[0] !! ::RxOp::Sequence.new(zyg => @kids);
    }

    method mayback() {
        for @$.zyg { return True if $_.mayback }
        return False;
    }
}

augment class RxOp::Alt { #OK exist
    method rxsimp($cut) {
        my @lads = map { OptRxSimple.run_lad($_.lad) }, @$.zyg;
        my @kids = map *.rxsimp($cut), @$.zyg;
        ::RxOp::Alt.new(
            optimized_lads => @lads,
            dba  => $.dba,
            zyg  => @kids);
    }
}

augment class RxOp::Cut { #OK exist
    method mayback() { False }
    method rxsimp($cut) { #OK not used
        my $kid = $.zyg[0].rxsimp(True);
        $kid.mayback ?? ::RxOp::Cut.new(zyg => [$kid]) !! $kid;
    }
}

augment class RxOp::Subrule { #OK exist
    method rxsimp($cut) {
        !$cut ?? self !! self.clone(selfcut => True);
    }
    method mayback() { !$.selfcut }
}

augment class RxOp::Sigspace { #OK exist
    method rxsimp($cut) {
        $cut ?? ::RxOp::Sigspace.new(selfcut => True) !! self
    }
    method mayback() { !$.selfcut }
}

augment class RxOp::Statement { #OK exist
    method mayback()    { False }
    method rxsimp($ ) { ::RxOp::Sequence.new(zyg => []) }
}

augment class RxOp::ConfineLang { #OK exist
    method mayback() { $.zyg[0].mayback }
}

augment class RxOp::Before { #OK exist
    method mayback() { False }
    # it's not uncommon to write <!before> and <?before>
    method rxsimp($ ) {
        my $z = $.zyg[0].rxsimp(True);
        return $z if $z.^isa(::RxOp::BeforeString);
        return ::RxOp::Before.new(zyg => $z.zyg)
            if $z.^isa(::RxOp::Before);
        return ::RxOp::BeforeString.new(str => $z.text)
            if $z.^isa(::RxOp::String);
        return $z.clone(zerowidth => True, selfcut => True)
            if $z.^isa(::RxOp::Subrule);
        return $z if $z.^isa(::RxOp::ZeroWidthCCs);
        return ::RxOp::ZeroWidthCCs.new(ccs => [$z.cc], :!after, :!neg)
            if $z.^isa(::RxOp::CClassElem);
        return ::RxOp::Before.new(zyg => [ $z ]);
    }
}

augment class RxOp::NotBefore { #OK exist
    method mayback() { False }
    method rxsimp($ ) {
        my $z = $.zyg[0].rxsimp(True);
        if $z.^isa(::RxOp::BeforeString) {
            return ::RxOp::NotBeforeString.new(str => $z.str);
        }
        if $z.^isa(::RxOp::Before) {
            return ::RxOp::NotBefore.new(zyg => $z.zyg);
        }
        if $z.^isa(::RxOp::String) {
            return ::RxOp::NotBeforeString.new(str => $z.text);
        }
        if $z.^isa(::RxOp::Subrule) {
            return $z.clone(:zerowidth, negative => !$z.negative, :selfcut);
        }
        if $z.^isa(::RxOp::ZeroWidthCCs) {
            return ::RxOp::ZeroWidthCCs.new(ccs => $z.ccs, after => $z.after,
                neg => !$z.neg);
        }
        if $z.^isa(::RxOp::CClassElem) {
            return ::RxOp::ZeroWidthCCs.new(ccs => [$z.cc], :!after, :neg);
        }
        return ::RxOp::NotBefore.new(zyg => [ $z ]);
    }
}

augment class RxOp::Quantifier { #OK exist
    method rxsimp($cut) {
        my @z = map *.rxsimp(False), @$.zyg;
        if $cut && @z == 1 && @z[0].^isa(::RxOp::CClassElem) {
            return ::RxOp::QuantCClass.new(cc => @z[0].cc, min => $.min,
                max => $.max);
        }
        $.zyg = @z; self;
    }
}

augment class RxOp::QuantCClass { #OK exist
    method mayback() { False }
}
augment class RxOp::VoidBlock { #OK exist
    method mayback() { False }
}
augment class RxOp::CheckBlock { #OK exist
    method mayback() { False }
}
augment class RxOp::Sym { #OK exist
    method mayback() { False }
}
augment class RxOp::VarString { #OK exist
    method mayback() { False }
}
augment class RxOp::ZeroWidthCCs { #OK exist
    method mayback() { False }
}
augment class RxOp::SaveValue { #OK exist
    method mayback() { False }
}
augment class RxOp::String { #OK exist
    method mayback() { False }
}
augment class RxOp::Any { #OK exist
    method mayback() { False }
}
augment class RxOp::None { #OK exist
    method mayback() { False }
}
augment class RxOp::CClassElem { #OK exist
    method mayback() { False }
}
augment class RxOp::CutLTM { #OK exist
    method mayback() { False }
}
augment class RxOp::CutRule { #OK exist
    method mayback() { False }
}
augment class RxOp::SetLang { #OK exist
    method mayback() { False }
}
augment class RxOp::ZeroWidth { #OK exist
    method mayback() { False }
}
augment class RxOp::BeforeString { #OK exist
    method mayback() { False }
}
augment class RxOp::NotBeforeString { #OK exist
    method mayback() { False }
}
INIT { $OptRxSimple = OptRxSimple }
