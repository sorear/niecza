class Foo {
    method method_with_arg($arg) {
        say $arg;
    }
}
say "1..1";
my $foo = Foo.new();
my $method = "method_with_arg";
$foo."$method"("ok 1");
