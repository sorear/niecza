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

done-testing;
