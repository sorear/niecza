# vim: ft=perl6
use Test;

ok "\n" ~~ /\n/, '\n in regex matches literal NL';
ok !('\n' ~~ /\n/), '\n in regex does not match literal \n';
ok '+' ~~ /\+/, '\+ in regex matches literal +';
ok '\\' ~~ /\\/, '\\\\ in regex matches literal \\';
ok 'a' ~~ /\x61/, '\x61 in regex matches literal a';

ok 'xy' ~~ /x <?> y/, '<?> matches null string';

my $a = 2;
my $b ::= $a;
$a = 5;
is $b, 2, "ro binding loses original container";

done-testing;
