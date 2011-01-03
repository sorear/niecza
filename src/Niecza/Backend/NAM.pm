package Niecza::Backend::NAM;
use NAMBackend;
use Moose;


sub compile {
    my ($self,$ast,$output) = @_;
    my $nam = NAMBackend::run($ast);
    open my $fh, ">", $output;
    print $fh $nam;
    close $fh;
}
1;
