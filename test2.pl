# vim: ft=perl6
use Test;
use MONKEY_TYPING;

{
    my $a = 3; $a &&= 4; is $a, 4, '&&= works (T)';
    my $b = 0; $b &&= 4; is $b, 0, '&&= works (F)';
    my $c = 3; $c ||= 4; is $c, 3, '||= works (T)';
    my $d = 0; $d ||= 4; is $d, 4, '||= works (F)';
    my $e = 0; $e andthen= 4; is $e, 4, 'andthen= works (D)';
    my $f = Any; $f andthen= 4; is $f, Any, 'andthen= works (U)';
    my $g = 0; $g //= 4; is $g, 0, '//= works (D)';
    my $h = Any; $h //= 4; is $h, 4, '//= works (U)';
}

{
    my $str = '';
    $str ~= 1;
    INIT $str ~= 2;
    $str ~= 3;
    INIT $str ~= 4;
    is $str, '2413', 'INIT blocks run in correct order';
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
