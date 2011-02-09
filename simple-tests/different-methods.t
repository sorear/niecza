say "1..2";
class Foo {
    method ok {
        say "ok 1";
    }
}
class Bar {
    method ok {
        say "ok 2";
    }
}
my $foo = Foo.new();
my $bar = Bar.new();
$foo.ok();
$bar.ok();
