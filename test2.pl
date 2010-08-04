# vim: ft=perl6
use Test;

my class Array is List {
    method new() {
        Array.RAWCREATE("flat", 1, "items", LLArray.new, "rest", LLArray.new);
    }
}
PRE-INIT { Q:CgOp { (prog (rawsset Kernel.ArrayP (@ (l Array))) (null Variable)) } }

my @x;

ok @x ~~ Array, '@x isa Array';

done-testing;
