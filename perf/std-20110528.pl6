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

my Mu $j = any(1..5);

sub onlysub($x) { $x }

bench '|||', 100000, sub () { 1|2|3|4|5 };
bench 'any(,,)', 10000, sub () { any(1,2,3,4,5) };
bench 'any(..)', 100000, sub () { any(1..5) };
bench '3 == any(,,)', 100000, sub () { ( 3 == 1|2|3|4|5 ) ?? True !! False };
bench 'grep * == 3 equiv', 100000, sub () { ( grep * == 3, 1,2,3,4,5 ) ?? True !! False };
bench 'grep 3 equiv', 100000, sub () { ( grep 3, 1,2,3,4,5 ) ?? True !! False };
bench 'onlysub($j)', 100000, sub () { onlysub($j) };
bench '$j.abs', 100000, sub () { $j.abs };
