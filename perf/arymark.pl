# vim: ft=perl6
use MONKEY_TYPING;

my $i = 0;
my @arr;
@arr[$i] = $i until ($i++) == 1000000;
