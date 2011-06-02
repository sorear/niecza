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

bench 'modified', 1000000, sub () { 1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1 };
bench 'same', 1000000, sub () { 1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1 };
