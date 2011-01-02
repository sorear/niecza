package Niecza::Pass::Begin;
use Moose;
sub invoke {
    my ($self,$ast) = @_;
    $ast->begin;
}
1;
