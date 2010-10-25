# vim: ft=perl6
use Test;
class Foo {
}

sub t($a,$b) { is Foo.canonicalize_name($a).join('|'), $b, "$a -> $b" }

t '$:foo', '$foo';
t '$^foo', '$foo';
t 'Foo:D', 'Foo';
t 'Foo::Bar', 'Foo::|Bar';
t '$Foo::Bar', 'Foo::|$Bar';
