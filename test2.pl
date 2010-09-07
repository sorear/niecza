# vim: ft=perl6
use Test;

say Q:CgOp { (rawsccall Kernel.GetFirst:c,Variable (@ { flat(1,2,3) })) };

done-testing;
