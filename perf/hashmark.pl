# vim: ft=perl6
sub _exists_key(\$container, $key) {
    $container.defined ?? $container.exists-key($key) !! False
}
sub _delete_key(\$container, $key) {
    $container.defined ?? $container.delete-key($key) !! Any
}
sub _at_key(\$container, $key) {
    $container.defined
        ?? $container.at-key($key)
        !! Any!Any::butWHENCE(sub (\$var) {
            $container.defined && die("Autovivification collision");
            $container = Hash.new;
            $container!Hash::extend($key, $var);
        });
}

my $i = 0;
my %hash;
%hash{$i} = $i until ($i++) == 100000;
