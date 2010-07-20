use 5.010;
use MooseX::Declare;

use CgOp;

class RxOp {
    has zyg => (isa => 'ArrayRef[RxOp]', is => 'ro');
}

class RxOp::String extends RxOp {
    has text => (isa => 'Str', is => 'ro', required => 1);
}

class RxOp::Quantifier extends RxOp {
    has type => (isa => 'Str', is => 'ro', required => 1);
    # ? + * only
    # zyg * 1
}

class RxOp::Sequence extends RxOp {
    # zyg * N
}

1;
