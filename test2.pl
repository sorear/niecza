# vim: ft=perl6
use Test;

# one CgOp bug manifested as a failure to compile this
ok (/ <?before x>: <ws>: /).defined, "unnamed regression";

done-testing;
