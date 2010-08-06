# vim: ft=perl6
use Test;

my class Array is List {
    method new() {
        Array.RAWCREATE("flat", 1, "items", LLArray.new, "rest", LLArray.new);
    }

    method push(*@x) {
        $!rest.push(@x);
    }

    method Numeric { self.elems }
}
PRE-INIT { Q:CgOp { (prog (rawsset Kernel.ArrayP (@ (l Array))) (null Variable)) } }

sub prefix:<+>($x) { $x.Numeric }

my @x;

ok @x ~~ Array, '@x isa Array';
ok @x.elems == 0, 'no elements';
ok +@x == 0, 'no elements (+)';

@x.push(5);
ok @x.elems == 1, 'one element now';
ok @x.shift == 5, 'element removed is 5';

done-testing;
