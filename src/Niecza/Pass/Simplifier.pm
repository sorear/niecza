package Niecza::Pass::Simplifier;
use Optimizer::Simplifier;
use Moose;
sub run {
    my ($self,$ast) = @_;
    Optimizer::Simplifier::run($ast);
    $ast;
}
1;
