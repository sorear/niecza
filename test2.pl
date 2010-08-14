# vim: ft=perl6
use Test;

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

{
    is "\x63", "c", '\x works';
    is "Y\x63", "Yc", 'can put stuff before escapes';
    is "\x63Y", "cY", 'can put stuff after escapes';
    is "Y\x63Y", "YcY", 'can put stuff before and after escapes';
    is "\x[63,69]", "ci", 'bracketed \x works';
    is "\x4E03", "七", '\x with >2 characters works';
    is "七".chars, 1, "nana is one kanji";
    is "\\", "\x5C", 'can backslash backslashes';
    is "\"", "\x22", 'can backslash quotes';
    is '\'', "\x27", 'can backslash single quotes';
    is "\b", "\x08", '\b works';
    is "\a", "\x07", '\a works';
    # punt named forms for now
    is "\e", "\x1B", '\e works';
    is "\f", "\x0C", '\f works';
    is "\n", "\x0A", '\n works';
    is "\r", "\x0D", '\r works';
    is "\t", "\x09", '\t works';
    is "\o[61,63,65]", '135', '\o works (bracketed)';
    is "\o67", '7', '\o works (bare)';
    is "\0", "\x00", '\0 works';

    is "foo { 2 + 2 } bar", "foo 4 bar", "code interpolation works";
    my $cow = 'hi';
    is "foo $cow bar", "foo hi bar", '$-interpolation works';
    is "foo $cow.substr(0,1) bar", "foo h bar", 'methodcall interpolation works';
}

done-testing;
