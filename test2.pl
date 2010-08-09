# vim: ft=perl6
use Test;

# maybe should take a method name?
sub _rxcall($C, $fun) {
    my @list := $fun($C);
    sub () { @list ?? @list.shift !! Any; }
}

# A call to a subrule could return a cursor of a different type, or with
# unwanted subcaptures that need to be cleared for <.foo>
sub _rxunbind($C, $fun) {
    my $it = $fun($C);
    sub {
        if my $v = $it() { #OK
            Q:CgOp {
                (box (@ (l $C)) (rawcall (unbox Cursor (@ (l $v)))
                    SetCaps (getfield captures (unbox Cursor (@ (l $C))))))
            }
        } else {
            Any
        }
    }
}

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

done-testing;
