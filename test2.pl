# vim: ft=perl6
use Test;
use MONKEY_TYPING;

{
    my class Foo {
        has @.bar;
        has %.baz;
        has @.quux = 1,2,3;
    }
    isa_ok Foo.new.bar, Array, '@.bar initializes as an array';
    isa_ok Foo.new.baz, Hash, '%.baz initializes as a hash';
    is +[ Foo.new(bar => (1,2,4)).bar ], 3, '@.bar initializes with list context';
    is +[ Foo.new(bar => 5).bar ], 1, '@.bar can initialize from a single item';
    is +[ Foo.new.quux ], 3, '@.quux with init list works';

    my $str = '';
    for 1,2,3,4 -> $x, $y { $str ~= "$x|$y," }
    is $str, "1|2,3|4,", 'multivariable for works';
}

{
    class X7140::X1122 { }
    my X7140::X1122 $obj .= new;
    isa_ok $obj, X7140::X1122, 'Type constraints via :: work';
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

done;
