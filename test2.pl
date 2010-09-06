# vim: ft=perl6
use Test;

ok '{}' ~~ / \{ <.ws> \} /, 'ws matches between \W';

done-testing;
