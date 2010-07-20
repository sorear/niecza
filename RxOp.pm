use strict;
use warnings;
use 5.010;

use CgOp;

{
    package RxOp;
    use Moose;

    has zyg => (isa => 'ArrayRef[RxOp]', is => 'ro');

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::String;
    use Moose;
    extends 'RxOp';

    has text => (isa => 'Str', is => 'ro', required => 1);

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Quantifier;
    use Moose;
    extends 'RxOp';

    has type => (isa => 'Str', is => 'ro', required => 1);
    # ? + * only
    # zyg * 1

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package RxOp::Sequence;
    use Moose;
    extends 'RxOp';

    # zyg * N

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
