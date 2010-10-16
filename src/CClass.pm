use 5.010;
use strict;
use warnings;
use utf8;

package CClass;

our %Gc = (map { $_ => ((state $i)++) } qw/ Lu Ll Lt Lm Lo
    Mn Ms Me Nd Nl No Zs Zl Zp Cc Cf Cs Co Pc Pd Ps Pc Pi Pf Po Sm
    Sc Sk So Cn /);

our $Empty = bless [ ], 'CClass';
our $Full  = bless [ 0, 0x1FFF_FFFF ], 'CClass';

sub range {
    my ($cl, $c1, $c2) = @_;

    return $Empty if $c1 gt $c2;

    bless [ ord($c1), 0x1FFF_FFFF, ord($c2) + 1, 0 ], $cl;
}

sub enum {
    my ($cl, @cs) = @_;
    my $ch = $Empty;

    for (@cs) {
        $ch = $ch->plus($_);
    }

    $ch;
}

sub catm {
    my ($cl, @bits) = @_;
    my $m = 0;
    for (@bits) {
        $m |= 1 << $Gc{$_};
    }
    $m ? (bless [ 0, $m ], $cl) : $Empty;
}

sub _binop {
    my ($func, $al, $bl) = @_;
    $bl = CClass->range($bl, $bl) if !ref($bl);
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
    _binop(sub { 0x1FFF_FFFF & ~$_[0] }, $self, []);
}

our $Word   = CClass->catm(qw< Lu Lt Ll Lm Lo Nd Nl No >)->plus('_');
our $Digit  = CClass->catm(qw< Nd Nl No >);
our $Space  = CClass->enum(' ', "\t", "\r", "\cK", "\n", "\x{3000}"); # TODO
our $HSpace = CClass->enum("\t", " ", "\x{3000}");
our $VSpace = CClass->enum("\r", "\cK", "\n");

sub internal {
    my ($name) = @_;
    ($name eq 'alpha') && return CClass->catm(qw< Lu Lt Ll Lm Lo >);
    die "unknown internal cclass $name";
}

1;
