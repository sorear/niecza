# vim: ft=perl6
use Test;
use MONKEY_TYPING;

{
    my class Bt {
        has $!pie;
        method get_pie() { $!pie }
        submethod BUILD(:$x) { $!pie = $x }
    }
    is Bt.new(x => 5).get_pie, 5, "BUILD basically works";
    my class SubBT is Bt {
        has $!pie2;
        method get_pie2() { $!pie2 }
        submethod BUILD(:$y) { $!pie2 = $y }
    }
    is SubBT.new(x => 5, y => 2).get_pie, 5, "superclass' BUILD in subclass";
    is SubBT.new(x => 5, y => 2).get_pie2, 2, "subclass' BUILD in subclass";

    my @l;
    @l := [1,2,3];
    is +[@l], 3, 'binding to existing list vars works';
}

is [ 1,2,3 ... 10 ], [1..10];
is [ 1,2,4 ... 256 ], [map 2 ** *, 0..8];
is [ 1,1,*+* ...^ *>100 ], [1,1,2,3,5,8,13,21,34,55,89];

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
