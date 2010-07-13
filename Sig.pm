use strict;
use warnings;
use 5.010;

{
    package Sig::Target;
    use Moose;

    has slot => (is => 'ro', isa => 'Maybe[Str]', required => 1);

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Sig::Parameter;
    use Moose;

    has target => (is => 'ro', isa => 'Sig::Target', required => 1);

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Sig;
    use Moose;

    has params => (isa => 'ArrayRef[Sig::Parameter]', is => 'ro', required => 1);

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
