class NieczaCompiler;

has $.module_finder;
has $.backend;
has $.stages;
has $.frontend;
has $.verbose;
has $!main-sn = 0;
has $!discount-time = 0;

has %.units;

method !compile(:$unitname, :$filename, :$source, :$main, :$run, :$evalmode, :$outer, :$outer_frame, :$repl) {
    # FIXME this is a bit of a fudge
    $unitname := 'CORE' if $!frontend.lang eq 'NULL';

    # send information to NieczaGrammar for creating the root sub
    my $*niecza_outer_ref = $outer;
    my $*niecza_outer_frame = $outer_frame;

    # useful references
    my $*compiler = self;
    my $*verbose = $.verbose;
    my $*backend = $.backend;
    my $*run_mode = $run;

    # tells parser not to exit when parse fails
    my $*in_repl = $repl;
    my @*INC;

    my $start = times[0] - $!discount-time;

    my $ast = $!frontend.parse(:$unitname, :$filename,
        :$source, :$outer, :$main, :$run, :$evalmode, :$repl);

    my $time = times[0] - $!discount-time - $start;

    if $.verbose {
        say "$unitname: took $time";
    }

    # don't count this time towards any other timing in progress
    $!discount-time += $time;

    $ast;
}

method compile_module($unitname) {
    my ($filename, $source) = $.module_finder.load_module($unitname);
    self!compile(:$unitname, :$filename, :$source);
}

method !main_name() {
    my $i = $!main-sn++;
    $i ?? "MAIN_$i" !! "MAIN";
}

method compile_file($file, $run) {
    my ($filename, $source) = $.module_finder.load_file($file);
    self!compile(unitname => self!main_name, :$filename, :$source, :main, :$run);
}

method compile_string($source, $run, :$evalmode = False, :$outer, :$repl, :$outer_frame) {
    self!compile(unitname => self!main_name, filename => "(eval)", :$source,
        :main, :$run, :$evalmode, :$outer, :$outer_frame, :$repl);
}
