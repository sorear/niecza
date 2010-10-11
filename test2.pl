# vim: ft=perl6
use Test;

sub nextsame() {
    Q:CgOp { (control 9 (null frame) (int -1) (null str) (null obj)) }
}

{
    my class A {
        method tom() { 12 }
        method foo($x) { $x * $x }
        method bar(:$x) { $x + $x }
    }
    my class B is A {
        method tom() { nextsame; }
        method foo($x) { nextsame; } #OK
        method bar(:$x) { nextsame; } #OK
    }
    is B.tom(), 12, "nextsame functional";
    is B.foo(5), 25, "nextsame functional w/ argument";
    # TODO
    # is B.bar(:x(7)), 14, "nextsame functional w/ named arg";

    sub foo(*%x) { %x }
    is foo(:z(2))<z>, 2, "slurpy hashes work";
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
