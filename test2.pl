# vim: ft=perl6
use Test;

sub rxtest($rgx, $rgxname, @y, @n) {
    for @y {
        ok $_ ~~ $rgx, "$rgxname ~~ $_";
    }
    for @n {
        ok !($_ ~~ $rgx), "$rgxname !~~ $_";
    }
}

sub _rxdot($C, $k) {
    Q:CgOp {
        (letn rt (rawcall (unbox Cursor (@ (l $C))) AnyChar)
          [ternary
            (!= (l rt) (null Cursor))
            (subcall (@ (l $k)) (box (@ (l $C)) (l rt)))
            (null Variable)])
    };
}

sub _rxcc($C, $cc, $k) {
    Q:CgOp {
        (letn rt (rawcall (unbox Cursor (@ {$C})) CClass
                   (unwrap CC (@ {$cc})))
          [ternary
            (!= (l rt) (null Cursor))
            (subcall (@ {$k}) (box (@ {$C}) (l rt)))
            (null Variable)])
    };
}

rxtest /x.y/, "x.y", ("xay", "x y"), ("xy", "xaay");
rxtest /<!>/, '<!>', Nil, ("", "x");

done-testing;
