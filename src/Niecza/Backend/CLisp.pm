package Niecza::Backend::CLisp;
use Moose;

sub compile {
    my ($self,$nam_file,$outfile) = @_;
    die "Compiling using the CLisp backend is Not Yet Implemented\n";
}
sub run {
    my ($self,$nam_file) = @_;
    system "clisp","cl-backend/backend.lisp",$nam_file;
}
1;
