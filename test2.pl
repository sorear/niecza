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

done-testing;
