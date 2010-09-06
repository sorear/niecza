# vim: ft=perl6
use Test;

ok '{}' ~~ / \{ <.ws> \} /, 'ws matches between \W';

{
    ok ("aab" ~~ /a* ab/), "a*ab backtracks";
    ok !("aab" ~~ /a*: ab/), "a*: ab doesn't";
    ok ("aab" ~~ /a*! ab/), "a*! ab backtracks";
    ok !("aab" ~~ /:r a* ab/), "ratcheting a* ab does not";
    ok !("aab" ~~ /:r a*: ab/), "ratcheting a*: ab does not";
    ok ("aab" ~~ /:r a*! ab/), "ratcheting a*! ab does";
    ok !("aab" ~~ token { a* ab }), "a* ab in a token does not";

    ok ("ab ab" ~~ / ab <.ws> ab /), "ws matches a space";
    ok (q:to/end/ ~~ / ab <.ws> ab /), "ws matches a newline";
ab
ab
end
    ok ("ab   ab" ~~ / ab <.ws> ab /), "ws matches several spaces";
    ok !("abab" ~~ / ab <.ws> ab /), "ws does not match nothing";
    ok ("ab   ab" ~~ rule { ab ab }), "rule gives space";
}

{
    # doing a more reasonable test will probably require embedded blocks
    ok "foobarx" ~~ / [ foo | foobar ]: x /, "LTM picks longest even if second";
    ok "foobarx" ~~ / [ foobar | foo ]: x /, "LTM picks longest even if first";
}

{
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
}

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

{
    my $a;
    ok 'xxy' ~~ /x { $a = $/.pos } /, "can match with \$/ stuff";
    is $a, 1, '$/.pos is the right sort of thing';
    'xxy' ~~ /x { $a = ($¢ ~~ Cursor) }/;
    is $a, True, '$¢ isa Cursor';
}

#ok "axy" ~~ / a <before x> \w y / , "before is zero-width";
#ok "axy" ~~ / a <?before x> \w y / , "?before is zero-width";
#ok "azy" ~~ / a <!before x> \w y / , "!before is zero-width";
#ok !("azy" ~~ / a <?before x> \w y /) , "?before x needs x";
#ok !("axy" ~~ / a <!before x> \w y /) , "!before x needs !x";
done-testing;
