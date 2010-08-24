# vim: ft=perl6
use Test;

sub testx:sym«foo bar»() { 42 }
is &testx:sym<<foo bar>>(), 42, "can use french quotes in declarations";

done-testing;
