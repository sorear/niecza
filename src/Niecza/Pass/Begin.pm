package Niecza::Pass::Begin;
use Moose;
sub run {
    my ($self,$ast) = @_;
    $ast->begin;
}
1;
