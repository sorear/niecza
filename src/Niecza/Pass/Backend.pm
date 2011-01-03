package Niecza::Pass::Backend;
use Moose;
use File::Temp;
has backend=>(is=>'ro');
has tmp_file=>(is=>'ro');
sub invoke {
    my ($self,$ast) = @_;
    my $file = $self->tmp_file->();
    $self->backend->compile($ast,$file);
    $file;
}
1;
