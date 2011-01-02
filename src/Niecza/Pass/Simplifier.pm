package Niecza::Pass::Simplifier;
use Optimizer::Simplifier;
use Moose;
sub invoke {
    my ($self,$ast) = @_;
    Optimizer::Simplifier::run($ast);
    $ast;
}
1;
