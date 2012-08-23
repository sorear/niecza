class NieczaPathSearch;

has @.path;

# Given Foo::Bar, find ($?FILE, $?ORIG)
method load_module($name) {
    my $sub = "".IO.combine($name.split('::'));

    my @path = ($*unit ?? $*unit.get_incs !! ()), @!path;

    for @path -> $pe {
        for <pm6 pm setting> -> $ext {
            my $fn = $pe.IO.append($sub).but-extension($ext);
            if $fn.f {
                my $text = $fn.slurp;
                # check borrowed from STD to weed out Perl 5 heuristically
                next if $ext eq 'pm' && $text ~~ /^^\h*package\h+\w+\s*\;/;
                return ~$fn.realpath, $text;
            }
        }
    }

    die "Unable to locate module $name in @path[]";
}

method load_file($name) {
    my $p = $name.IO;
    (~$p.realpath, $p.slurp);
}
