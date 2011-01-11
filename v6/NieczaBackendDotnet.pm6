use NieczaBackendNAM;
class NieczaBackendDotnet is NieczaBackendNAM;

sub run_subtask($file, *@args) {
    Q:CgOp { (rawscall Builtins,Kernel.RunCLRSubtask {$file} {@args}) }
}

method _post_save($name, :$main) {
    my $fname = $name.split('::').join('.');
    run_subtask($.obj_dir.IO.append("CLRBackend.exe"),
        $.obj_dir, $fname ~ ".nam", $fname ~ ($main ?? ".exe" !! ".dll"),
        $main ?? "1" !! "0");
}

method _run($name) {
    my $fname = $name.split('::').join('.');
    run_subtask($.obj_dir.IO.append($fname ~ ".exe"), @$.run_args);
}
