# vim: ft=perl6
use Test;

# XXX multi dispatch
sub next {
    Q:CgOp { (rawsccall Kernel.SearchForHandler (int 1) (null Frame) (int -1) (null String) (null Variable)) }
}
sub last {
    Q:CgOp { (rawsccall Kernel.SearchForHandler (int 2) (null Frame) (int -1) (null String) (null Variable)) }
}
sub redo {
    Q:CgOp { (rawsccall Kernel.SearchForHandler (int 3) (null Frame) (int -1) (null String) (null Variable)) }
}
sub return is rawcall {
    Q:CgOp { (rawsccall Kernel.SearchForHandler (int 4) (null Frame) (int -1) (null String) (pos 0)) }
}

{
    sub flow-ok($fn, $flw, $msg) {
        my $log = '';
        $fn(-> $i { $log ~= $i });
        is $log, $flw, $msg;
    }

    flow-ok -> &l { my $i = 0; while $i < 2 { $i++; l(1); next; l(2) } }, '11',
        "next skips second half of while loop";
    flow-ok -> &l { my $i = 0; while $i < 2 { $i++; l(1); last; l(2) } }, '1',
        "last skips everything";
    flow-ok -> &l { my $i = 0; while True { l($i++); last if $i == 3 } }, '012',
        "last can leave inf loop";
    flow-ok -> &l { my $i = 3; while $i == 3 { l($i--); redo if $i } }, '321',
        "redo reenters loops";
    sub foo { return 2; }
    is foo(), 2, "return values work";
    my $cont = False;
    sub foo3 { return 2; $cont = True; }
    foo3;
    ok !$cont, "return exits function";
}

{
    rxtest /^ x**2..4 $/, 'x**2..4', ('xx','xxx','xxxx'), ('x','xxxxx');
    rxtest /^ x**2..* $/, 'x**2..*', ('xx','xxx','xxxx'), ('x',);
    rxtest /^ [x**2] $/, 'x**2', ('xx',), ('x','xxx');
    rxtest /^ [x**y] $/, 'x**y', ('x','xyx','xyxyx'), ('','xy','yx');
}

done-testing;
