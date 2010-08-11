# vim: ft=perl6
use Test;

sub _rxcut($C, $f, $k) {
    my @l := gather $f($C, &take);
    @l && $k(@l.shift);
}

ok ("aab" ~~ /a* ab/), "a*ab backtracks";
ok !("aab" ~~ /a*: ab/), "a*: ab doesn't";
ok ("aab" ~~ /a*! ab/), "a*! ab backtracks";
ok !("aab" ~~ /:r a* ab/), "ratcheting a* ab does not";
ok !("aab" ~~ /:r a*: ab/), "ratcheting a*: ab does not";
ok ("aab" ~~ /:r a*! ab/), "ratcheting a*! ab does";
ok !("aab" ~~ token { a* ab }), "a* ab in a token does not";

done-testing;
