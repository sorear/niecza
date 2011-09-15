class NieczaBackendDotnet;

use NAMOutput;

has $.safemode = False;
has $.obj_dir;
has $.run_args = [];

# The purpose of the backend is twofold.  It must be able to accept
# and process units; and it must be able to retrieve processed units
# at a later time.

# Return Metamodel::Unit, undefined if unit not available.  The caller
# will check tdeps, load them if needed, and maybe even discard the
# returned unit.
method get_unit($name) {
    my $file = ($name.split('::').join('.') ~ ".nam").IO\
        .relative($.obj_dir);
    $file.e ?? NAMOutput.load($file.slurp) !! ::Metamodel::Unit;
}

# Save a unit.  If $main is true, it is being considered as a main
# module; if $run, it should be auto-run.  Main modules do not need
# to be retrievable.
method save_unit($name, $unit) {
    my $file = ($name.split('::').join('.') ~ ".nam").IO\
        .relative($.obj_dir);
    $file.spew(NAMOutput.run($unit));
}

sub upcalled(@strings) {
    given @strings[0] {
        when "eval" {
            my $*IN_EVAL = True;
            # XXX NieczaException is eaten by boundary
            try {
                $*compiler.compile_string(@strings[1], True, :evalmode,
                    :outer([@strings[2], +@strings[3]]));
                return "";
            }
            return $!;
        }
        say "upcall: @strings.join('|')";
        "ERROR";
    }
}

sub downcall(*@args) {
    Q:CgOp { (rnull (rawscall Niecza.Downcaller,CompilerBlob.InitSlave {&upcalled})) };
    Q:CgOp { (rawscall Niecza.Downcaller,CompilerBlob.DownCall {@args}) }
}

method accept($unitname, $ast is rw, :$main, :$run, :$evalmode, :$repl) {
    downcall("safemode") if $.safemode;
    if $run {
        downcall("setnames", $*PROGRAM_NAME // '???',
            $*orig_file // '(eval)') unless $repl;
        my $nam = NAMOutput.run($ast);
        $ast.clear_optrees;
        my ($exn) = downcall(($evalmode ?? "evalnam" !! "runnam"), $.obj_dir, $nam, @$.run_args);
        die $exn if $exn;
        if $repl {
            my ($exn) = downcall("replrun");
            die $exn if $exn;
        }
        $*repl_outer = $ast.mainline.xref if $repl;
        $ast = Any;
        return;
    }
    self.save_unit($unitname, $ast);
    $ast.clear_optrees;
    self.post_save($unitname, :$main);
    $*repl_outer = $ast.mainline.xref if $repl;
    $ast = Any;
}

method post_save($name, :$main) {
    my $fname = $name.split('::').join('.');
    downcall("post_save",
        $.obj_dir, $fname ~ ".nam", $fname ~ ($main ?? ".exe" !! ".dll"),
        $main ?? "1" !! "0");
}
