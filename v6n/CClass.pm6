# 28f112a757ef2d6f553d144dd8f8b9a1de17c71b
class CClass;

has $.terms;

our %Gc = < Lu Ll Lt Lm Lo Mn Ms Me Nd Nl No Zs Zl Zp Cc Cf Cs Co Pc
    Pd Ps Pc Pi Pf Po Sm Sc Sk So Cn >.map({ $_ => ((state $i)++) });

our $Empty = CClass.new(terms => [ ]);
our $Full  = CClass.new(terms => [ 0, 0x3FFF_FFFF ]);

method range($c1, $c2) {
    ($c1 gt $c2) ?? $Empty !!
        self.new(terms => [ $*Cheats.ord($c1), 0x3FFF_FFFF, $*Cheats.ord($c2) + 1, 0 ]);
}

method enum(*@cs) {
    my $ch = $Empty;
    for @cs { $ch = $ch.plus($_) }
    $ch;
}

method catm(*@bits) {
    my $m = 0;
    for @bits { $m = $m +| (1 +< $Gc{$_}) }
    $m ?? self.new(terms => [ 0, $m ]) !! $Empty;
}

sub _binop($func, $al, $blr) {
    my $bl = ($blr ~~ CClass) ?? $blr !! CClass.range($bl, $bl);
    my ($alix, $alcur) = (0, 0);
    my ($blix, $blcur) = (0, 0);
    my @o;
    my $pos = 0;
    my $ocur = $func->(0, 0);
    if ($ocur != 0) {
        push @o, 0, $ocur;
    }

    while ($pos != 1e7) {
        my $ata = $alix < @$al && $al->[$alix] == $pos;
        my $atb = $blix < @$bl && $bl->[$blix] == $pos;

        if ($ata) {
            $alcur = $al->[$alix+1];
            $alix += 2;
        }

        if ($atb) {
            $blcur = $bl->[$blix+1];
            $blix += 2;
        }

        my $onew = $func->($alcur, $blcur);
        if ($onew != $ocur) {
            push @o, $pos, $onew;
            $ocur = $onew;
        }

        my $toa = $alix < @$al ? $al->[$alix] : 1e7;
        my $tob = $blix < @$bl ? $bl->[$blix] : 1e7;

        $pos = ($toa < $tob) ? $toa : $tob;
    }

    bless \@o, 'CClass';
}

sub plus {
    my ($self, $other) = @_;
    _binop(sub { $_[0] | $_[1] }, $self, $other);
}

sub minus {
    my ($self, $other) = @_;
    _binop(sub { $_[0] & ~$_[1] }, $self, $other);
}

sub negate {
    my ($self) = @_;
    _binop(sub { 0x3FFF_FFFF & ~$_[0] }, $self, []);
}

our $Word   = CClass->catm(qw< Lu Lt Ll Lm Lo Nd Nl No >)->plus('_');
our $Digit  = CClass->catm(qw< Nd Nl No >);
our $Space  = CClass->enum(' ', "\t", "\r", "\cK", "\n", "\x{3000}"); # TODO
our $HSpace = CClass->enum("\t", " ", "\x{3000}");
our $VSpace = CClass->enum("\r", "\cK", "\n");

sub internal {
    my ($name) = @_;
    ($name eq 'alpha') && return CClass->catm(qw< Lu Lt Ll Lm Lo >)->plus('_');
    die "unknown internal cclass $name";
}

1;

