package Niecza::Backend::NAM;
use NAMBackend;
use Moose;

#has optimizer=>(is=>'rw');
#   $ast = $self->optimizer->run($ast);

sub compile {
    my ($self,$ast,$output) = @_;
    my $nam = NAMBackend::run($ast);
    open my $fh, ">", $output;
    print $fh $nam;
    close $fh;
}
1;
