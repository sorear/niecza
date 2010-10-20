# vim: ft=perl6
use Test;
use MONKEY_TYPING;

augment class Regex {
    method ACCEPTS($str) {
        my $i = 0;
        my $mat;
        my $C = Cursor.new($str);
        while !$mat && ($i <= $str.chars) {
            $mat = (self)($C.cursor($i++));
        }
        unitem($mat.head);
    }
}

augment class Cursor {
    method O (*%hash) {
        Q:CgOp { (newscalar (cursor_O (cast cursor (@ {self}))
                                      (unbox varhash (@ {%hash})))) }
    }
    method list () { @( self.Capture ) }
    method flat () { @( self.Capture ) }
    method iterator () { self.flat.iterator }
    method hash () { %( self.Capture ) }
    method Capture () { Q:CgOp {
        (letn cap (obj_newblank (obj_llhow (@ {Capture})))
          (cursor_unpackcaps (cast cursor (@ {self})) (l cap))
          (newscalar (l cap)))
    } }
}

augment class Capture {
    method list () { @( Q:CgOp { (box Parcel (getslot positionals fvarlist
        (@ {self}))) } ) }
    method hash () { unitem( Q:CgOp { (box Hash (getslot named varhash
        (@ {self}))) } // {} ) }
}

augment class Match {
    method list () { @( self.Capture ) }
    method hash () { %( self.Capture ) }
    method flat () { @( self.Capture ) }
    method iterator () { self.flat.iterator }
    method Capture () { Q:CgOp {
        (letn cap (obj_newblank (obj_llhow (@ {Capture})))
          (cursor_unpackcaps (cast cursor (@ {self})) (l cap))
          (newscalar (l cap)))
    } }
}

{
    my $m = "ab" ~~ / (.) <alpha> /;
    is (@$m)[0], "a", "Match.list returns positional captures";
    is (%$m)<alpha>, "b", "Match.hash returns named";
    is ((-> $p, :$alpha { $p, $alpha })(|$m)).join("|"), "a|b", "Match.Capture returns both";
    my @arr = "abc" ~~ / (.) (.) (.) /;
    is @arr.join("|"), "a|b|c", "Regex.ACCEPTS in list context returns captures";
    $m = "" ~~ / <O( foo => 2 )> /;
    is $m<O><foo>, 2, "<O> is functional";
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
