class NieczaBackendNAM;

use NAMOutput;

has $.obj_dir;
has $.run_args = [];

# The purpose of the backend is twofold.  It must be able to accept
# and process units; and it must be able to retrieve processed units
# at a later time.

# Return Metamodel::Unit, undefined if unit not available.  The caller
# will check tdeps, load them if needed, and maybe even discard the
# returned unit.
method get_unit($name) {
    my $file = $name.split('::').join('.').IO.but-extension('nam')\
        .relative($.obj_dir);
    $file.e ?? NAMOutput.load($file.slurp) !! ::Metamodel::Unit;
}

# Save a unit.  If $main is true, it is being considered as a main
# module; if $run, it should be auto-run.  Main modules do not need
# to be retrievable.
method save_unit($name, $unit) {
    my $file = $name.split('::').join('.').IO.but-extension('nam')\
        .relative($.obj_dir);
    $file.spew(NAMOutput.run($unit));
}

method post_save($name, :$main) { #OK not used
}

method run($name) { #OK not used
    die "nam backend does not support running code";
}
