package Niecza::Compiler;
use Moose;
has frontend => (is=>'ro');
has passes  => (is=>'ro');
has backend => (is=>'ro');
sub compile {
    my ($self,$input,$filename,$output) = @_;
    my $ast = $self->frontend->parse($input,$filename);
    for my $pass (@{$self->passes}) {
        $ast = $pass->invoke($ast);
    }
    $self->backend->compile($ast,$output);
}
1;
