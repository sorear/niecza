# vim: ft=perl6
use Test;
use MONKEY_TYPING;

my class Capture {
    has $!positionals;
    has $!named;

    method Capture () { self }
}

augment class List {
    method Capture () {
        Q:CgOp {
            (letn n (obj_newblank (obj_llhow (@ {Capture})))
              (setslot positionals (l n) (vvarlist_to_fvarlist
                  (getslot items vvarlist (@ {self.eager}))))
              (ns (l n)))
        }
    }
}

augment class Parcel {
    method Capture () {
        Q:CgOp {
            (letn n (obj_newblank (obj_llhow (@ {Capture})))
              (setslot positionals (l n) (unbox fvarlist (@ {self})))
              (ns (l n)))
        }
    }
}

augment class Enum {
    method Capture () {
        Q:CgOp {
            (letn n (obj_newblank (obj_llhow (@ {Capture})))
                  d (varhash_new)
              (setslot positionals (l n) (fvarlist_new))
              (setslot named (l n) (l d))
              (varhash_setindex (unbox str (@ {$!key.Str}))
                (l d) {$!value})
              (ns (l n)))
        }
    }
}

augment class Hash {
    method Capture () {
        Q:CgOp {
            (letn n (obj_newblank (obj_llhow (@ {Capture})))
              (setslot positionals (l n) (fvarlist_new))
              (setslot named (l n) (varhash_dup
                  (unbox varhash (@ {self}))))
              (ns (l n)))
        }
    }
}

ok !('xy' ~~ /x <{ False }> y/), '<{False}> blocks a match';
ok 'xy' ~~ /x <{ True }> y/, '<{True}> does not affect it';

{
    my $b = "oo";
    is ("foox" ~~ /f$b/), "foo", '$x matches contents in a regex';
}

{
    sub f1(:$x) { $x }
    is f1(|("x" => 2)), 2, "can flatten pairs";
    is f1(|{"x" => 2}), 2, "can flatten hashes";
    sub f2($x,$) { $x }
    is f2(|[1,2]), 1, "can flatten lists";
    is f2(|(1,2)), 1, "can flatten parcels";
}

# {
#     our role Stop4717[$a] {
#         token foo { $a }
#     }
#
#     grammar X {
#         token TOP { [ <foo> | foo ]: x }
#     }
#
#     ok (X but OUR::Stop4717["foobar"]).parse("foobarx"),
#         "LTM works through parameterized role variables";
# }

#is $?FILE, 'test.pl', '$?FILE works';
#is $?ORIG.substr(0,5), '# vim', '$?ORIG works';

# {
#     {
#         our $x = 5; #OK
#     }
#     ok $::x == 5, '$::x finds our variable';
# 
#     package Fao { our $y = 6; } #OK
#     ok $::Fao::y == 6, '$::Fao::y works as $Fao::y';
# 
#     { class Mao { } }
#     ok ::Mao.new.defined, 'can use classes via ::Mao';
# }
# 
# {
#     my $x = 7; #OK
#     ok $::x == 7, '$::x can find lexicals';
#     class A3 {
#         method moo { 42 }
#         class B4 {
#             ok ::A3.moo, '::A3 can find outer classes';
#         }
#     }
# }

done-testing;
