# vim: ft=perl6
sub _exists_key(\$container, $key) {
    defined($container) ?? $container.exists-key($key) !! False
}
sub _delete_key(\$container, $key) {
    defined($container) ?? $container.delete-key($key) !! Any
}
sub _at_key(\$container, $key) {
    defined($container)
        ?? $container.at-key($key)
        !! Any!Any::butWHENCE(sub (\$var) {
            defined($container) && die("Autovivification collision");
            $container = Hash.new;
            $container!Hash::extend($key, $var);
        });
}

my $i = 0;
my %hash;
%hash{$i} = $i until ($i++) == 1000000;
