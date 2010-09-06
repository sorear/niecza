# vim: ft=perl6
use Test;

ok '{}' ~~ / \{ <.ws> \} /, 'ws matches between \W';

{
    my grammar G7 {
        proto token tok  {*}
        token tok:sym<+> { <sym> }
        token tok:foo    { <sym> }

        rule TOP { <tok> }
    }

    ok G7.parse('+'), "can parse :sym<> symbols";
    ok G7.parse('foo'), "can parse : symbols";
}

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
