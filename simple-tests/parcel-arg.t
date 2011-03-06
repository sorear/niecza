say "1..2";
sub foo(\$arg) {
    say $arg;
    $arg = "ok 2";
}
my $ok = "ok 1";
foo($ok);
say $ok;
