# vim: ft=perl6

use Test;

plan 17;

is 10.abs, 10, "10.abs == 10";
is (-10).abs, 10, "(-10).abs == 10";
is (-10).abs.WHAT, 10.WHAT, "(-10).abs and 10 have same WHAT";

{
    my $big = 2 ** 200 - 42;
    is $big.abs, $big, '$big.abs == $big';
    is (-$big).abs, $big, '(-$big).abs == $big';
    is (-$big).abs.WHAT, $big.WHAT, '(-$bi).abs and $big have same WHAT';
}

ok (10/2).abs == 10/2, "(10/2).abs == 10/2";
ok (-10/2).abs == 10/2, "(-10/2).abs == 10/2";
is (-10/2).abs.WHAT, (10/2).WHAT, "(-10/2).abs and 10/2 have same WHAT";

ok (10.Num).abs == 10.Num, "(10.Num).abs == 10.Num";
ok (-10.Num).abs == 10.Num, "(-10.Num).abs == 10.Num";
is (-10.Num).abs.WHAT, (10.Num).WHAT, "(-10.Num).abs and 10.Num have same WHAT";

ok (10 + 5i).abs == 125.sqrt, "(10 + 5i).abs == 125.sqrt";
ok (-10 - 5i).abs == 125.sqrt, "(-10 - 5i).abs == 125.sqrt";
is (-10 - 5i).abs.WHAT, "Num()", "(-10 - 5i).abs is a Num";

ok (10 + 5i).re == 10, "(10 + 5i).re == 10";
ok (10 + 5i).im == 5, "(10 + 5i).im == 5";
