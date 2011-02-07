say "1..4";
my $foo;
my $bar;
$bar := $foo;
$foo = "ok 1";
say $bar;

$foo := "ok 2";
$bar := $foo;
say $bar;

my $c = "ok 3 # binding a variable to itself";
$c := $c;
say $c;

my $a;
my $b;
$a = "ok 4";
$b := $a;
$a := $b;
say $a;



