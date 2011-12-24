class CClass;

has $.terms;

my $nclass = 0;
our %Gc = < Lu Ll Lt Lm Lo Mn Ms Me Nd Nl No Zs Zl Zp Cc Cf Cs Co Pc
    Pd Ps Pe Pi Pf Po Sm Sc Sk So Cn >.map(-> $n { $n => ($nclass++) });

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
    my $bl = ($blr ~~ CClass) ?? $blr.terms !! CClass.range($blr, $blr).terms;
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
method minus($other) { _binop({ $^a +& +^$^b }, self, $other); }
method negate() { _binop(-> $a, $ { 0x3FFF_FFFF +& +^$a }, self, $Empty) }

# the range here is the only part of Other_Alphabetic which is not already
# contained in M* in Unicode 6.0.0
our $Word   = CClass.catm(< Me Mn Ms Pc Nd Ll Lt Lu Lm Lo Nl >)\
    .plus(CClass.range(chr(9398), chr(9450)));
our $Digit  = CClass.catm(< Nd >);
our $Space  = CClass.enum( # Unicode :Whitespace property - TODO use db
    "\x0009", "\x000A", "\x000B", "\x000C", "\x000D", "\x0020", "\x0085",
    "\x00A0", "\x1680", "\x180E", "\x2000", "\x2001", "\x2002", "\x2003",
    "\x2004", "\x2005", "\x2006", "\x2007", "\x2008", "\x2009", "\x200A",
    "\x2028", "\x2029", "\x202F", "\x205F", "\x3000");
our $VSpace = CClass.enum("\x000A", "\x000B", "\x000C", "\x000D", "\x0085",
    "\x2028", "\x2029");
our $HSpace = $Space.minus($VSpace);

our &internal = sub ($name) {
    ($name eq 'alpha') && return CClass.catm(< Lu Lt Ll Lm Lo >).plus('_');
    die "unknown internal cclass $name";
}
method internal($name) { &internal($name) }
