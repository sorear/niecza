# vim: ft=perl6
use Test;

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

sub _pair($k, $v) { Pair.RAWCREATE("key", $k, "value", $v) }

is :foo.value, 'Bool::True', ":foo is true";
is :!foo.value, 'Bool::False', ":!foo is false";
is :foo<12>.value, '12', ":foo<12> is 12";
is :foo.key, 'foo', ":foo is foo";

done-testing;
