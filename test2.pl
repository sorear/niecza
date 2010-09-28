# vim: ft=perl6
use Test;

{
    my $x;
    INIT {
        $x = 1;
    }
    ok $x, "changes made in the protolexpad are visible at runtime";
}

ok INIT { 1 }, "init blocks can return values";

{
    my $x;
    my $unclonable-sub = INIT { sub () { $x } };
    $x = 42;
    ok $unclonable-sub() == 42, "mainlines are not cloned";
}

is $?FILE, 'test.pl', '$?FILE works';
is $?ORIG.substr(0,5), '# vim', '$?ORIG works';


done-testing;
