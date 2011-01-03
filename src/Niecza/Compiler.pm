package Niecza::Compiler;
use Moose;
has frontend => (is=>'ro');
has passes  => (is=>'ro');
has backend => (is=>'ro');
sub ast {
    my ($self,%args) = @_;
    my $ast = $self->frontend->parse($args{source},$args{filename});
    for my $pass (@{$self->passes}) {
        $ast = $pass->invoke($ast);
    }
    $ast;
}
sub run {
    my ($self,%args) = @_;
    $self->backend->run($self->ast(%args));
}
sub compile {
    my ($self,%args) = @_;
    $self->backend->compile($self->ast(%args),$args{output});
}
1;
