# vim: ft=perl6
use Test;

{
    use MONKEY_TYPING;
    my class Foo {
        method foo { 1 }
    }
    is Foo.foo, 2, "augments run early";
    is Any.g4077, 4077, "can augment core classes";
    is Cool.g4077, 4077, "augments visible in subclasses";

    augment class Foo {
        method foo { 2 }
    }
    augment class Any {
        method g4077 { 4077 }
    }
}

use MONKEY_TYPING;
augment class Match {
    method at-key($k) { Q:CgOp {
        (rawcall (cast Cursor (@ {self})) GetKey (unbox String (@ {$k.Str})))
    } }
    method at-pos($i) { self.at-key($i) }
}
augment class Cursor {
    method at-key($k) { Q:CgOp {
        (rawcall (cast Cursor (@ {self})) GetKey (unbox String (@ {$k.Str})))
    } }
    method at-pos($i) { self.at-key($i) }
}

{
    my $ma = (grammar {
        regex TOP { <foo> <bar> }
        regex foo { \d+ }
        regex bar { \w+ }
    }).parse("123abc");
    ok $ma<foo> ~~ Match, "<foo> sub is a Match";
    is $ma<foo>, "123", "<foo> sub got 123";
    ok $ma<bar> ~~ Match, "<bar> sub is a Match";
    is $ma<bar>, "abc", "<bar> sub got 123";
    ok !$ma<quux>, "no <quux> sub";
}

done-testing;
