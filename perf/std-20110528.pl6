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

bench "(zeroing array)", 100000, sub () { @arr = () };
bench "(tenning array)", 100000, sub () { @arr = 0,1,2,3,4,5,6,7,8,9 };
bench "push", 100000, sub () { @arr = (); push @arr, 1; push @arr, 2; push @arr, 3; push @arr, 4; push @arr, 5; push @arr, 6; push @arr, 7; push @arr, 8; push @arr, 9; push @arr, 10; };
bench "unshift", 100000, sub () { @arr = (); unshift @arr, 1; unshift @arr, 2; unshift @arr, 3; unshift @arr, 4; unshift @arr, 5; unshift @arr, 6; unshift @arr, 7; unshift @arr, 8; unshift @arr, 9; unshift @arr, 10; };
bench "pop", 100000, sub () { @arr = 0,1,2,3,4,5,6,7,8,9; pop @arr; pop @arr; pop @arr; pop @arr; pop @arr; pop @arr; pop @arr; pop @arr; pop @arr; pop @arr };
bench "shift", 100000, sub () { @arr = 0,1,2,3,4,5,6,7,8,9; shift @arr; shift @arr; shift @arr; shift @arr; shift @arr; shift @arr; shift @arr; shift @arr; shift @arr; shift @arr };
