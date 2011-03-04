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
                $*compiler.compile_string(@strings[1], True, :evalmode);
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

sub run_subtask($file, *@args) {
    Q:CgOp { (rawscall Builtins,Kernel.RunCLRSubtask {$file} {@args}) }
}

method accept($unitname, $ast is rw, :$main, :$run, :$evalmode) {
    downcall("safemode") if $.safemode;
    if $run {
        my $nam = NAMOutput.run($ast);
        $ast.clear_optrees;
        $ast = Any;
        downcall(($evalmode ?? "evalnam" !! "runnam"), $.obj_dir, $nam);
        return;
    }
    self.save_unit($unitname, $ast);
    $ast.clear_optrees;
    $ast = Any;
    self.post_save($unitname, :$main);
    $run && self.run($unitname);
}

method post_save($name, :$main) {
    my $fname = $name.split('::').join('.');
    downcall("post_save",
        $.obj_dir, $fname ~ ".nam", $fname ~ ($main ?? ".exe" !! ".dll"),
        $main ?? "1" !! "0");
}

method run($name) {
    my $fname = $name.split('::').join('.');
    run_subtask($.obj_dir.IO.append($fname ~ ".exe"), @$.run_args);
}
