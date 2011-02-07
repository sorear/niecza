class Foo {
    method method_with_arg($arg) {
        say $arg;
    }
}
say "1..1";
my $foo = Foo.new();
$foo.method_with_arg("ok 1");
