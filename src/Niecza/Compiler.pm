package Niecza::Compiler;
use Moose;
has frontend => (is=>'ro');
has passes  => (is=>'ro');
has backend => (is=>'ro');
sub compile {
    my ($self,%args) = @_;
    my $ast = $self->frontend->parse($args{source},$args{filename});
    for my $pass (@{$self->passes}) {
        $ast = $pass->invoke($ast);
    }
    $self->backend->compile($ast,$args{output});
}
1;
