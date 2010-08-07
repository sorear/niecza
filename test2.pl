# vim: ft=perl6
use Test;

my class Hash {
    method new() { Q:CgOp { (box Hash (rawnew Dictionary<string,Variable>)) } }
    method !extend is rawcall {
        Q:CgOp {
            (letn d [unbox Dictionary<string,Variable> (@ (pos 0))]
                  k [unbox String (@ (methodcall (pos 1) Str))]
              [ternary (rawcall (l d) ContainsKey (l k))
                (die "Autovivification collision")
                (prog)]
              [setindex (l k) (l d) (pos 2)]
              [null Variable])
        };
    }

    # TODO: We need something like pir:: notation for this to not suck
    method at-key($key) {
        Q:CgOp {
            (box Bool (rawcall [unbox Dictionary<string,Variable> (@ (l self))]
                ContainsKey [unbox String (@ (methodcall (l $key) Str))]))
        }
            ?? Q:CgOp {
                (getindex [unbox String (@ (methodcall (l $key) Str))]
                  [unbox Dictionary<string,Variable> (@ (l self))])
            } !! Any!butWHENCE({ self!extend($key, Q:CgOp { (pos 0) }) });
    }
}

sub postcircumfix:<{ }> is rawcall {
    my $key ::= Q:CgOp { (pos 1) };

    (Q:CgOp { (pos 0) }).defined
        ?? (Q:CgOp { (pos 0) }).at-key($key)
        !! Any!butWHENCE(sub () is rawcall {
            my $ar := Q:CgOp { (getindex (int 0) (getfield pos
                                 (getfield outer (callframe)))) };
            $ar.defined && die("Autovivification collision");
            $ar = Hash.new;
            $ar!extend($key, Q:CgOp { (pos 0) });
        });
}

{
    sub postcircumfix:<{ }> { @_.join("|") }
    is 1<2 3>, '1|2|3', "angle bracket postcircumfix works";
}

my $foo;
ok !($foo<x>.defined), "fetch from hash, no value";
ok !($foo.defined), "no autoviv for rvalue";
$foo<x> = 'foo';
is $foo<x>, 'foo', "values are retained";
ok !($foo<y>.defined), "no cross-slot leakage";
ok $foo ~~ Hash, "foo isa hash now";
$foo<z><a> = 'pie';
is $foo<z><a>, 'pie', "can autoviv deeply";
$foo<y>[2] = 'zed';
is $foo<y>[2], 'zed', "can mix array and hash viv";
$foo<12> = 'fry';
is $foo{12}, 'fry', "keys are strings";

done-testing;
