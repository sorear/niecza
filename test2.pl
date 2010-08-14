# vim: ft=perl6
use Test;

my $x = '';
ok !("a" ~~ / a { $x = 1; } b /), '{} does not terminate regex';
is $x, 1, '{} is run even if regex fails';
$x = '';
ok !("" ~~ / a { $x = 1; } b /), '{} does not affect regex that ends before it';
is $x, '', '{} is only run if reached';
$x = 0;
ok ("aab" ~~ / a* { $x++ } ab /), '{} does not block backtracking';
is $x, 2, '{} is run multiple times when backtracking';

$x = '';
ok ("foo" ~~ / foo { $x = $x ~ 1 } | foo { $x = $x ~ 2 } /),
    "foo ~~ foo|foo";
is $x, 1, "with no other constraints, first item is used";
$x = '';
ok ("foo" ~~ / fo* { $x = $x ~ 1 } | foo { $x = $x ~ 2 } /),
    "foo ~~ fo*|foo";
is $x, 2, "longer literal prefix wins over seniority";
$x = '';
ok ("fooo" ~~ / fo* { $x = $x ~ 1 } | foo { $x = $x ~ 2 } /),
    "foo ~~ fo*|foo";
is $x, 1, "longer length wins over prefix";
$x = '';
ok !("fooo" ~~ / [ fo*: { $x = $x ~ 1 } | foo { $x = $x ~ 2 } ] x /),
    "foo !~~ [fo*:|foo]x";
is $x, '12', "will backtrack into shorter token";

my grammar G5 {
    token a { foo }
    token b { foobar }
    token c { <a> | <b> }
    token d { <c> x }

    token e { x <e> x | y }

    token TOP { A <d> | E <e> }
}

ok G5.parse('Afoobarx'), 'LTM works even through subrules';
ok G5.parse('Exxyxx'), 'recursivity does not crash LTM';

my grammar G6 {
    token a   { fo* { $x = 1 } }
    token b   { foo { $x = 2 } }
    token TOP { <a> | <b> }
}
G6.parse("foo");
is $x, 2, "prefix length testing works in subrules";

done-testing;
