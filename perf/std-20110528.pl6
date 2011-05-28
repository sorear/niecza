use JSYNC;

sub calibrate() {
    my $start = now.to-posix.[0];
    my $i = -1000000;
    my $f = sub () {};
    $f() while $i++;
    my $end = now.to-posix.[0];
    ($end - $start) / 1000000;
}

my $base = calibrate();

sub bench($name, $nr, $f) {
    my $start = now.to-posix.[0];
    my $i = -$nr;
    $f() while $i++;
    my $end   = now.to-posix.[0];
    say "$name = {($end - $start) / $nr - $base} [raw {$end-$start} for $nr]";
}

bench "nulling test", 1000000, sub () {};
{
    my @l;
    bench "iterate empty list", 1000000, sub () { for @l { } };
}

my $str = "0";
$str ~= $str for ^18;
say $str.chars;
# $str = substr($str,0,1000000);

my grammar GTest {
    token TOP { <.bit>* }
    token bit { . }
}

bench "grammar", 1, sub () { GTest.parse($str) };
{
    my class GAct0 {
    }
    bench "grammar (no action)", 1, sub () { GTest.parse($str, :actions(GAct0)) };
}

{
    my class GAct1 {
        method bit($ ) { }
    }
    bench "grammar (empty action)", 1, sub () { GTest.parse($str, :actions(GAct1)) };
}

{
    my class GAct2 {
        method FALLBACK($ , $ ) { }
    }
    bench "grammar (fallback action)", 1, sub () { GTest.parse($str, :actions(GAct2)) };
}

bench "Any.exists-key", 1000000, sub () { Any<foo>:exists };

my $arr = [1];
bench "JSON array iteration", 1000000, sub () { to-json($arr) };
