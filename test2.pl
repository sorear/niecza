# vim: ft=perl6
use Test;

ok '{}' ~~ / \{ <.ws> \} /, 'ws matches between \W';

{
    rxtest /z .* y [ a :: x | . ]/, "z.*y[a::x|.]",
        ("zyax", "zyb", "zyaxya"), ("zya",);
    # no ::> until STD gets here...
    rxtest /z .* y [ a ::: x || . ]/, "z.*y[a:::x||.]",
        ("zyax", "zyb"), ("zya", "zyaxya");

    my grammar G7 {
        proto regex TOP {*}
        regex TOP:foo { a :: x }
        regex TOP:bar { . }
    }

    ok G7.parse("ax"), ":: does not block forward";
    ok G7.parse("b"), ":: does not affect other paths";
    ok !G7.parse("a"), "cannot backtrack past :: in proto ltm";
}

done-testing;
