use CgOp;
use Test;
use JSYNC;

sub cgopt($term, $name, $want) {
    is to-json($term), $want, $name;
}

cgopt CgOp._cgop("foo",1,2), '_cgop("foo",1,2)', '["foo",1,2]';
cgopt CgOp.letvar("bar"), 'letvar("bar")', '["letvar","bar"]';
cgopt CgOp.string_var("bar"), 'string_var',
    '["box","Str",["str","bar"]]';
cgopt CgOp.let(CgOp.letvar('x'), -> $k { $k }), 'let',
    '["letn","!L0",["letvar","x"],["letvar","!L0"]]';
cgopt CgOp.subcall(CgOp.letvar('x'), CgOp.letvar('y')), 'sub1',
    '["subcall","\\u0000",["letvar","x"],["letvar","y"]]';
cgopt CgOp.subcall(CgOp.letvar('x'), ':foo', CgOp.letvar('y')), 'sub1',
    '["subcall","\\u0004:foo",["letvar","x"],["letvar","y"]]';
done;
