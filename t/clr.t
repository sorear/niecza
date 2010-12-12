# vim: ft=perl6
use Test;

sub fi($a,$b) { Q:CgOp { (foreign_class (obj_getstr {$a}) (obj_getstr {$b})) } }

my $Console = fi('dotnet', 'System.Console');

$Console.WriteLine('Hello from CLR');
