class NieczaCompiler;

use JSYNC;

has $.module_finder;
has $.backend;
has $.stages;
has $.frontend;
has $.verbose;
has $!main-sn = 0;
has $!discount-time = 0;

has %.units;

method !compile($unitname, $filename, $modtime, $source, $main, $run, $end, $evalmode, $outer, $outer_frame, $repl) {
    # FIXME this is a bit of a fudge
    $unitname := 'CORE' if $!frontend.lang eq 'NULL';

    my $*niecza_outer_ref = $outer;
    my $*niecza_outer_frame = $outer_frame;
    my $*compiler = self;
    my $*verbose = $.verbose;
    my $*backend = $.backend;
    my $*in_repl = $repl;
    my @*INC;

    my $start = times[0] - $!discount-time;

    my $ast = $!frontend.parse(:$unitname, :$filename, :$modtime,
        :$source, :$outer, :$main, :$run, :$evalmode, :$repl);

    my $time = times[0] - $!discount-time - $start;

    if $.verbose {
        say "$unitname: took $time";
    }

    # don't count this time towards any other timing in progress
    $!discount-time += $time;

    $ast;
}

method compile_module($module, $stop = "") {
    my ($filename, $modtime, $source) = $.module_finder.load_module($module);
    self!compile($module, $filename, $modtime, $source, False, False, $stop, False, Any, Any, False);
}

method !main_name() {
    my $i = $!main-sn++;
    $i ?? "MAIN_$i" !! "MAIN";
}

method compile_file($file, $run, $stop = "") {
    my $*orig_file = $file; # XXX
    my ($filename, $modtime, $source) = $.module_finder.load_file($file);
    self!compile(self!main_name, $filename, $modtime, $source, True, $run, $stop, False, Any, Any, False);
}

method compile_string($source, $run, $stop = "", :$evalmode = False, :$outer, :$repl, :$outer_frame) {
    self!compile(self!main_name, "(eval)", 0, $source, True, $run, $stop, $evalmode, $outer, $outer_frame, $repl);
}
