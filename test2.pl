# vim: ft=perl6
use Test;

my grammar G7 {
    token tok:sym<+> { <sym> }
    token tok:foo    { <sym> }
    token tok { <tok:sym<+>> | <tok:foo> }

    rule TOP { <tok> }
}

ok G7.parse('+'), "can parse :sym<> symbols";
ok G7.parse('foo'), "can parse : symbols";

done-testing;
