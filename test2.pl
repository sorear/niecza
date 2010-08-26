# vim: ft=perl6
use Test;

{
    my $obj ::= (class {
        method item() { "item" }
        method list() { "list" }
        method hash() { "hash" }
    }).new;

    is $($obj), "item", '$() calls item';
    is @($obj), "list", '@() calls list';
    is %($obj), "hash", '%() calls hash';

    is $$obj, "item", '$$ truncated context';
    is @$obj, "list", '@$ truncated context';
    is %$obj, "hash", '%$ truncated context';

    is "x$$obj", "xitem", '$$ interpolation';
    is "x@$obj", "xlist", '@$ interpolation';
    is "x%$obj", "xhash", '%$ interpolation';
}

done-testing;
