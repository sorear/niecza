say "1..2";
class Foo {
    method foo {
        say "ok 1 ";
    }
}
class Bar {
    method bar {
        say "ok 2";
    }
}
my $foo = Foo.new();
my $bar = Bar.new();
$foo.foo();
$bar.bar();
