use NieczaBackendNAM;
class NieczaBackendClisp is NieczaBackendNAM;

# XXX XXX .NET doesn't seem to have any real spawnl functionality,
# only system
sub run_command($cmd, $args) {
    # This doesn't seem to compile
    # Q:CgOp { (rnull (rawcall WaitForExit (rawscall System.Diagnostics.Process.Start (obj_getstr {$cmd}) (obj_getstr {$args})))) };
}

method _post_save($name, :$main) {
    # None needed; run does all the work
}

method _run($name) {
    my $fname = $name.split('::').join('.');
    run_command("clisp", "cl-backend/backend.lisp " ~ $fname ~ ".nam");
}
