# vim: ft=perl6
use Test;
use MONKEY_TYPING;

{
    sub infix:<=>($x, $y) { $x ~ "|" ~ $y }
    is (1 = 2), '1|2', 'can override infix:<=> in lexical scope';
}

{
    my class Regex { }
    ok "x" ~~ /x/, "Regex shadowing doesn't cause problems";
}

ok "\x2FFF" ~~ /<-[ x ]>/, "Negated char classes match unassigned characters";
ok "x:" ~~ /. >> ./, "Punctuation ends words";

{
    my class A { method foo(:$x) { $x * 2 } }
    my class B is A { method foo() { nextwith( self, x => 5 ) } }
    is B.foo, 10, "nextwith works";
}

{
    our role R6025[$x] {
        method foo() { $x }
    }

    ok ((Any but OUR::R6025[True]).foo.^isa(Bool)),
        "parameterized roles can store non-strings";
}

{
    our role Stop4717[$a] {
        token foo { $a }
    }

    grammar X {
        token TOP { [ <foo> | foo ]: x }
    }

    ok (X but OUR::Stop4717["foobar"]).parse("foobarx"),
        "LTM works through parameterized role variables";
}

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
