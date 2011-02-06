# vim: ft=perl6
use Test;
use MONKEY_TYPING;

{
    is [ 1..5 ], "1 2 3 4 5", "Ranges work in list context";
    is [ 1 ..^ 5 ], "1 2 3 4", "Tail exclusion works";
    is [ 1 ^.. 5 ], "2 3 4 5", "Head exclusion works";
    is [ 1 ^..^ 5 ], "2 3 4", "Dual exclusion works";
    is [ ^5 ], "0 1 2 3 4", "Shorthand form works";
    is ((5 .. *)[3]), 8, "Infinite ranges can be iterated";
    ok 3 ~~ 1..4, "Range checking works (+)";
    ok 5 !~~ 1..4, "Range checking works (-)";
}

{
    my $i = 0;
    1 < ($i++; 2) < 3;
    is $i, 1, "Chained comparisons only evaluate terms once";

    my $foo = [5];
    for $foo { .shift }
    is +$foo, 0, ".method works";

    my $x = 5;
    $x ~~ .++;
    is $x, 6, "~~ topicalization works";

    my $y;
    given 12 { $y = $_ }
    is $y, 12, "prefix given works";

    $y = $_ given 24;
    is $y, 24, "postfix given works";

    my $z = '';
    $z ~= $_ for 1, 2, 3;
    is $z, '123', "postfix for works";

    my $k = '';
    given 12 {
        $k ~= 1 when 12;
        $k ~= 2 when * > 5;
        $k ~= 3 when * <= 5;
    }
    is $k, '12', "postfix when works";

    $k = '';
    given 12 {
        when 9 { $k ~= 1 }
        when * > 6 { $k ~= 2 }
        when * > 3 { $k ~= 3 }
    }
    is $k, '2', "normal when works";
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
