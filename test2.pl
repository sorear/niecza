# vim: ft=perl6
use Test;
use MONKEY_TYPING;

{
    my $str;
    INIT $str = '';
    $str ~= 1;
    INIT $str ~= 2;
    $str ~= 3;
    INIT $str ~= 4;
    is $str, '2413', 'INIT blocks run in correct order';
}

{
    my class X3 {
        has $.a;
    }
    my $x = X3.new(a => 5);
    is $x.a, 5, 'Attribute values can be passed in constructors';

    sub foo($/) { $<a> }
    is foo({ a => 5 }), 5, 'Can bind $/ in signature';

    my class X4 {
        method foo { 13 }
        method !bar { 17 }
        method sam($x) { self!"$x"() }
    }

    is X4."foo"(), 13, 'indirect method calls work'; #OK
    is X4."{ "fo" ~ "o" }"(), 13, 'indirect calls work with interpolation';
    is X4.sam("bar"), 17, "indirect private method calls work";
}

{
    "foo" ~~ /oo/;
    is $/.ast, 'oo', '.ast defaults to .Str';
    "foo" ~~ /oo { make 15 }/;
    is $/.ast, 15, 'make can change .ast';
    "foo" ~~ /{make 30} oo/;
    is $/.ast, 30, 'make works in the middle';

    my grammar X5 {
        proto token TOP {*}
        token TOP:x { foo { make 45 } }
    }
    is X5.parse("foo").ast, 45, 'make works in multiregexes';
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
