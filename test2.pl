# vim: ft=perl6
use Test;

sub _rxcut($C, $f, $k) {
    my @l := gather $f($C, &take);
    @l && $k(@l.shift);
}

PRE-INIT {
    Cursor.HOW.add-method("orig", anon method orig { Q:CgOp {
        (box Str (getfield backing (unbox Cursor (@ (l self))))) } });
    Cursor.HOW.add-method("ws", anon method ws() {
        gather
            Q:CgOp {
                (letn rt (rawcall (unbox Cursor (@ (l self))) SimpleWS)
                  [ternary
                    (!= (l rt) (null Cursor))
                    (subcall (@ (l &take)) (box (@ (l self)) (l rt)))
                    (null Variable)])
            };
    });
}

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

done-testing;
