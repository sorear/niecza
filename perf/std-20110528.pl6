use JSYNC;

sub timethis($nr, $fun) {
    my $i = -$nr;
    my $start = times[0];
    $fun() while $i++;
    my $end = times[0];
    ($end - $start) / $nr;
}

my $base1 = timethis(1000000, sub () {});
my $base2 = timethis(1000000, sub () {});
my $avg = ($base1 + $base2) / 2;
say "null check: rd = {abs ($base1 - $base2) / $avg}  ($base1 $base2)";

sub bench($name, $nr, $f) {
    my $time = timethis($nr, $f);
    say "$name = {($time - $avg)*1e6}µs [{$time*$nr}s / $nr]";
}

my @arr;

bench "or", 10000000, sub () { 1 +| 2 };
bench "xor", 10000000, sub () { 1 +^ 2 };
bench "and", 10000000, sub () { 1 +& 2 };
bench "lshift", 10000000, sub () { 1 +> 2 };
bench "rshift", 10000000, sub () { 1 +< 2 };
bench "compl", 10000000, sub () { +^ 1 };
