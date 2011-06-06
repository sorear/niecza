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
my %h = (1 => True, 2 => True, 3 => True, 4 => True, 5 => True);
my %h2 = (:a, :b, :c, :d, :e);

bench '|||', 1000000, sub () { 1|2|3|4|5 };
#bench 'any(..)', 10000, sub () { any(1..5) };
bench 'onlysub($j)', 100000, sub () { onlysub($j) };
bench '$j.abs', 100000, sub () { $j.abs };
