# vim: ft=perl6
use Test;

PRE-INIT { Q:CgOp { (prog (rawsset Kernel.HashP (@ (l Hash))) (null Variable)) } }

my class Enum is Cool {
    has $.key;
    has $.value;

    method kv() {
        ($.key, $.value);
    }

    method pairs() {
        self.flat;
    }
}

my class Pair is Enum {
}

sub infix:<< => >>($k, $v) { Pair.RAWCREATE("key", $k, "value", $v) }

is :foo.value, 'Bool::True', ":foo is true";
is :!foo.value, 'Bool::False', ":!foo is false";
is :foo<12>.value, '12', ":foo<12> is 12";
is :foo.key, 'foo', ":foo is foo";

is (foo => 1).key, 'foo', "foo => 1 keeps key";
is (foo => 1).value, '1', "foo => 1 keeps value";
is ("foo" => 1).key, 'foo', '"foo" => 1 keeps key';
is ("foo" => 1).value, '1', '"foo" => 1 keeps value';

my %hash;
ok %hash ~~ Hash, '%-vars are Hash';

done-testing;
