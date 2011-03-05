say "1..1";
sub foo {
    say "ok 1";
}
my $bar = &foo;
$bar.();
