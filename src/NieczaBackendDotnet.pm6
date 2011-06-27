use NieczaBackendNAM;
class NieczaBackendDotnet is NieczaBackendNAM;

use NAMOutput;

has $.safemode = False;

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
    Q:CgOp { (rawscall Builtins,Kernel.DownCall {&upcalled} {@args}) }
}

method accept($unitname, $ast is rw, :$main, :$run, :$evalmode, :$repl) {
    downcall("safemode") if $.safemode;
    $*repl_outer = $ast.mainline.xref if $repl;
    if $run {
        downcall("setnames", $*PROGRAM_NAME // '???',
            $*orig_file // '(eval)') unless $repl;
        my $nam = NAMOutput.run($ast);
        $ast.clear_optrees;
        $ast = Any;
        downcall(($evalmode ?? "evalnam" !! "runnam"), $.obj_dir, $nam, @$.run_args);
        if $repl {
            my ($exn) = downcall("replrun");
            die $exn if $exn;
        }
        return;
    }
    self.save_unit($unitname, $ast);
    $ast.clear_optrees;
    $ast = Any;
    self.post_save($unitname, :$main);
}

method post_save($name, :$main) {
    my $fname = $name.split('::').join('.');
    downcall("post_save",
        $.obj_dir, $fname ~ ".nam", $fname ~ ($main ?? ".exe" !! ".dll"),
        $main ?? "1" !! "0");
}
