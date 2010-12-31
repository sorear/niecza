# 28f112a757ef2d6f553d144dd8f8b9a1de17c71b
class CClass;

sub ord($x) { Q:CgOp { (rawscall Builtins,Kernel.Ord {$x}) } }
sub chr($x) { Q:CgOp { (rawscall Builtins,Kernel.Chr {$x}) } }
sub infix:<+&>($x, $y) { Q:CgOp { (rawscall Builtins,Kernel.NumAnd {$x} {$y}) } }
sub infix:<+|>($x, $y) { Q:CgOp { (rawscall Builtins,Kernel.NumOr {$x} {$y}) } }
sub infix:<+^>($x, $y) { Q:CgOp { (rawscall Builtins,Kernel.NumXor {$x} {$y}) } }
sub infix:<< +< >>($x, $y) { Q:CgOp { (rawscall Builtins,Kernel.NumLShift {$x} {$y}) } }
sub infix:<< +> >>($x, $y) { Q:CgOp { (rawscall Builtins,Kernel.NumRShift {$x} {$y}) } }
sub prefix:<< +^ >>($x) { Q:CgOp { (rawscall Builtins,Kernel.NumCompl {$x}) } }

has $.terms;

our %Gc = < Lu Ll Lt Lm Lo Mn Ms Me Nd Nl No Zs Zl Zp Cc Cf Cs Co Pc
    Pd Ps Pc Pi Pf Po Sm Sc Sk So Cn >.map({ $_ => ((state $i)++) });

our $Empty = CClass.new(terms => [ ]);
our $Full  = CClass.new(terms => [ 0, 0x3FFF_FFFF ]);

method range($c1, $c2) {
    ($c1 gt $c2) ?? $Empty !!
        self.new(terms => [ ord($c1), 0x3FFF_FFFF, ord($c2) + 1, 0 ]);
}

method enum(*@cs) {
    my $ch = $Empty;
    for @cs { $ch = $ch.plus($_) }
    $ch;
}

method catm(*@bits) {
    my $m = 0;
    for @bits { $m = $m +| (1 +< %Gc{$_}) }
    $m ?? self.new(terms => [ 0, $m ]) !! $Empty;
}

sub _binop($func, $alr, $blr) {
    my $bl = ($blr ~~ CClass) ?? $blr.terms !! CClass.range($bl, $bl).terms;
    my $al = $alr.terms;
    my ($alix, $alcur) = (0, 0);
    my ($blix, $blcur) = (0, 0);
    my @o;
    my $pos = 0;
    my $ocur = $func(0, 0);
    if $ocur != 0 {
        push @o, 0, $ocur;
    }

    while $pos != 10_000_000 {
        my $ata = $alix < @$al && $al[$alix] == $pos;
        my $atb = $blix < @$bl && $bl[$blix] == $pos;

        if $ata {
            $alcur = $al[$alix+1];
            $alix = $alix + 2;
        }

        if $atb {
            $blcur = $bl[$blix+1];
            $blix = $blix + 2;
        }

        my $onew = $func($alcur, $blcur);
        if $onew != $ocur {
            push @o, $pos, $onew;
            $ocur = $onew;
        }

        my $toa = $alix < @$al ?? $al[$alix] !! 10_000_000;
        my $tob = $blix < @$bl ?? $bl[$blix] !! 10_000_000;

        $pos = $toa < $tob ?? $toa !! $tob;
    }

    CClass.new(terms => @o);
}

method plus($other) { _binop(* +| *, self, $other); }
method minus($other) { _binop(* +& +^*, self, $other); }
method negate() { _binop( 0x3FFF_FFFF +& +^*, self, CClass.new(terms => [])) }

our $Word   = CClass.catm(< Lu Lt Ll Lm Lo Nd Nl No >).plus('_');
our $Digit  = CClass.catm(< Nd Nl No >);
our $Space  = CClass.enum(' ', "\t", "\r", "\x0B", "\n", "\x3000"); # TODO
our $HSpace = CClass.enum("\t", " ", "\x3000");
our $VSpace = CClass.enum("\r", "\x0B", "\n");

our &internal = sub ($name) {
    ($name eq 'alpha') && return CClass.catm(< Lu Lt Ll Lm Lo >).plus('_');
    die "unknown internal cclass $name";
}
