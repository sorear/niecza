# vim: ft=perl6
use Test;

#ok '{}' ~~ / \{ <.ws> \} /, 'ws matches between \W';

{
    ok ("a" ~~ /a/), "letter matches itself";
    ok !("a" ~~ /b/), "letter does not match other";
    ok ("xxa" ~~ /a/), "leading garbage ignored";
    ok ("axx" ~~ /a/), "trailing garbage ignored";
    ok ("ab" ~~ /ab/), "sequence matches sequence";
    ok !("ab" ~~ /ba/), "sequence requires order";
    ok ("abc" ~~ /ab?c/), "conditional can match";
    ok ("ac" ~~ /ab?c/), "conditional can match nothing";
    ok !("adc" ~~ /ab?c/), "conditional cannot match something else";
    ok ("ac" ~~ /ab*c/), "kleene closure can match none";
    ok ("abbc" ~~ /ab*c/), "kleene closure can match many";
    ok !("adc" ~~ /ab*c/), "kleene closure cannot match other";
    ok ("abc" ~~ /ab+c/), "plus can match one";
    ok ("abbbc" ~~ /ab+c/), "plus can match many";
    ok !("adc" ~~ /ab+c/), "plus cannot match other";
    ok !("ac" ~~ /ab+c/), "plus cannot match none";

    grammar Bob {
        regex TOP {ab*c}
    }

    ok Bob.parse("abbc"), "grammars work (1)";
    ok !Bob.parse("adc"), "grammars work (2)";
    ok !Bob.parse("xac"), "grammars anchor (1)";
    ok !Bob.parse("acx"), "grammars anchor (2)";
}

{
    my grammar G1 {
        regex TOP { <.foo> }
        regex foo { x }
    }

    ok G1.parse("x"), "subrules work (positive)";
    ok !G1.parse("y"), "subrules work (negative)";

    my grammar G2 {
        regex TOP { y <.foo> <.foo> y }
        regex foo { x }
    }

    ok G2.parse("yxxy"), "subrule position tracking works";
    ok !G2.parse("yxy"), "subrule position tracking works (2)";

#     my grammar G3 {
#         regex TOP { <moo> }
#         regex moo { x }
#     }
# 
#     ok G3.parse("x"), "capturing subrules work (positive)";
#     ok !G3.parse("y"), "capturing subrules work (negative)";
}

{
    ok ("aab" ~~ /a* ab/), "a*ab backtracks";
    ok !("aab" ~~ /a*: ab/), "a*: ab doesn't";
    ok ("aab" ~~ /a*! ab/), "a*! ab backtracks";
    ok !("aab" ~~ /:r a* ab/), "ratcheting a* ab does not";
    ok !("aab" ~~ /:r a*: ab/), "ratcheting a*: ab does not";
    ok ("aab" ~~ /:r a*! ab/), "ratcheting a*! ab does";
    ok !("aab" ~~ token { a* ab }), "a* ab in a token does not";
# 
#     ok ("ab ab" ~~ / ab <.ws> ab /), "ws matches a space";
#     ok (q:to/end/ ~~ / ab <.ws> ab /), "ws matches a newline";
# ab
# ab
# end
#     ok ("ab   ab" ~~ / ab <.ws> ab /), "ws matches several spaces";
#     ok !("abab" ~~ / ab <.ws> ab /), "ws does not match nothing";
#     ok ("ab   ab" ~~ rule { ab ab }), "rule gives space";
}
# 
# {
#     # doing a more reasonable test will probably require embedded blocks
#     ok "foobarx" ~~ / [ foo | foobar ]: x /, "LTM picks longest even if second";
#     ok "foobarx" ~~ / [ foobar | foo ]: x /, "LTM picks longest even if first";
# }
# 
# {
#     my $x = '';
#     ok !("a" ~~ / a { $x = 1; } b /), '{} does not terminate regex';
#     is $x, 1, '{} is run even if regex fails';
#     $x = '';
#     ok !("" ~~ / a { $x = 1; } b /), '{} does not affect regex that ends before it';
#     is $x, '', '{} is only run if reached';
#     $x = 0;
#     ok ("aab" ~~ / a* { $x++ } ab /), '{} does not block backtracking';
#     is $x, 2, '{} is run multiple times when backtracking';
# 
#     $x = '';
#     ok ("foo" ~~ / foo { $x = $x ~ 1 } | foo { $x = $x ~ 2 } /),
#         "foo ~~ foo|foo";
#     is $x, 1, "with no other constraints, first item is used";
#     $x = '';
#     ok ("foo" ~~ / fo* { $x = $x ~ 1 } | foo { $x = $x ~ 2 } /),
#         "foo ~~ fo*|foo";
#     is $x, 2, "longer literal prefix wins over seniority";
#     $x = '';
#     ok ("fooo" ~~ / fo* { $x = $x ~ 1 } | foo { $x = $x ~ 2 } /),
#         "foo ~~ fo*|foo";
#     is $x, 1, "longer length wins over prefix";
#     $x = '';
#     ok !("fooo" ~~ / [ fo*: { $x = $x ~ 1 } | foo { $x = $x ~ 2 } ] x /),
#         "foo !~~ [fo*:|foo]x";
#     is $x, '12', "will backtrack into shorter token";
# 
#     my grammar G5 {
#         token a { foo }
#         token b { foobar }
#         token c { <a> | <b> }
#         token d { <c> x }
# 
#         token e { x <e> x | y }
# 
#         token TOP { A <d> | E <e> }
#     }
# 
#     ok G5.parse('Afoobarx'), 'LTM works even through subrules';
#     ok G5.parse('Exxyxx'), 'recursivity does not crash LTM';
# 
#     my grammar G6 {
#         token a   { fo* { $x = 1 } }
#         token b   { foo { $x = 2 } }
#         token TOP { <a> | <b> }
#     }
#     G6.parse("foo");
#     is $x, 2, "prefix length testing works in subrules";
# }
# 
# {
#     my grammar G7 {
#         proto token tok  {*}
#         token tok:sym<+> { <sym> }
#         token tok:foo    { <sym> }
# 
#         rule TOP { <tok> }
#     }
# 
#     ok G7.parse('+'), "can parse :sym<> symbols";
#     ok G7.parse('foo'), "can parse : symbols";
# }
# 
# {
#     ok 'xxy' ~~ /x { $a = $/.pos } /, "can match with \$/ stuff";
#     is $a, 1, '$/.pos is the right sort of thing';
#     'xxy' ~~ /x { $a = ($¢ ~~ Cursor) }/;
#     is $a, True, '$¢ isa Cursor';
# }

#rxtest /x.y/, "x.y", ("xay", "x y"), ("xy", "xaay");
#rxtest /<!>/, '<!>', Nil, ("", "x");
#rxtest /\s/, '\s', (" ", ("\n" => '\n'), ("\r" => '\r'), "\x3000"),
#    ("x", "1", "+");
#rxtest /\S/, '\S', ("x", "1", "+"),
#    (" ", ("\n" => '\n'), ("\r" => '\r'), ("\x3000" => 'id space'));
#rxtest /\w/, '\w', ("x", "1", "_", "\x4E00"), ("+", " ");
#rxtest /<[ y ]>/, '<[ y ]>', ("y"), (" ", "x", "z");
#rxtest /<[ i .. k ]>/, '<[ i .. k ]>', ("i", "j", "k"), ("h", "l");
#rxtest /<[ \W a..z ]>/, '<[\W a..z]>', ("a", "z", "+"), ("\x4E00");

rxtest /a || b/, 'a || b', ("a", "b"), ("c", "");
rxtest /x [a || aa]: c/, 'x[a||aa]:c', ("xac",), ("xaac",);

#ok "axy" ~~ / a <before x> \w y / , "before is zero-width";
#ok "axy" ~~ / a <?before x> \w y / , "?before is zero-width";
#ok "azy" ~~ / a <!before x> \w y / , "!before is zero-width";
#ok !("azy" ~~ / a <?before x> \w y /) , "?before x needs x";
#ok !("axy" ~~ / a <!before x> \w y /) , "!before x needs !x";
done-testing;
