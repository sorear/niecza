class NieczaCompiler;

use JSYNC;

has $.module_finder;
has $.backend;
has $.stages;
has $.frontend;
has $.verbose;

has $.unitcache = {};

sub gettimeofday() { Q:CgOp { (rawscall Builtins,Kernel.GetNow) } }

method !compile($unitname, $filename, $modtime, $source, $main, $run, $end) {
    my %*units;

    my $*module_loader = sub ($m) { self!load_dependent($m) };
    my $*verbose = $.verbose;

    my $ast;
    my @steps = (
        $.frontend.typename => { $ast = $.frontend.parse(:$unitname,
            :$filename, :$modtime, :$source); },
        (map -> $st { $st.typename => { $ast = $st.invoke($ast) } }, @$.stages),
        ($.backend.typename ~ "-save") =>
            { $.backend.save_unit($unitname, $ast); $ast = Any },
        ($.backend.typename ~ "-post") =>
            { $.backend.post_save($unitname, :$main); },
        ($.backend.typename ~ "-run") =>
            { $run && $.backend.run($unitname); },
    );

    for @steps -> $step {
        my $start = gettimeofday;
        $step.value.();
        my $time = gettimeofday() - $start;

        if $.verbose {
            say "$unitname: $step.key() took $time";
        }

        if $end eq $step.key {
            say to-jsync($ast);
            last;
        }
    }
}

method compile_module($module, $stop = "") {
    my ($filename, $modtime, $source) = $.module_finder.load_module($module);
    self!compile($module, $filename, $modtime, $source, False, False, $stop);
}

method compile_file($file, $run, $stop = "") {
    my ($filename, $modtime, $source) = $.module_finder.load_file($file);
    self!compile("MAIN", $filename, $modtime, $source, True, $run, $stop);
}

method compile_string($source, $run, $stop = "") {
    self!compile("MAIN", "(eval)", 0, $source, True, $run, $stop);
}

method !up_to_date($mod) {
    say "Checking datedness of $mod.name()" if $.verbose;
    for $mod.tdeps.pairs -> $p {
        my ($filename, $modtime, $source) = $.module_finder.load_module($p.key);
        if $filename ne $p.value.[0] {
            say "$p.key() resolves to $filename now, was $p.value.[0]" if $.verbose;
            return False;
        }
        if $modtime > $p.value.[1] {
            say "$p.key() mod-time increased from $p.value.[1] to $modtime" if $.verbose;
            return False;
        }
    }
    return True;
}

method !load_dependent($module) {
    say "Trying to load depended module $module" if $.verbose;
    my $newmod = $.unitcache{$module} //= $.backend.get_unit($module);

    if !defined($newmod) || !self!up_to_date($newmod) {
        $.unitcache{$module}:delete;
        say "(Re)compilation needed" if $.verbose;
        self.compile_module($module);
        $newmod = $.unitcache{$module} = $.backend.get_unit($module);
    }

    %*units{$module} = $newmod;
    for keys $newmod.tdeps -> $mn {
        %*units{$mn} //= ($.unitcache{$mn} //= $.backend.get_unit($mn));
    }
    say "Loaded $module" if $.verbose;
    $newmod;
}
