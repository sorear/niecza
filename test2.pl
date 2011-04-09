# vim: ft=perl6
use Test;
use MONKEY_TYPING;

{
    grammar G4675 {
        token TOP { abc }
    }
    G4675.parse("abc");
    ok $/, '.parse sets $/ (1)';
    G4675.parse("def");
    nok $/, '.parse sets $/ (2)';
}

{
    is "a1b2c".subst(/\d/, 'd'), 'adb2c', '.subst works';
    is "a1b2c".subst(:global, /\d/, 'd'), 'adbdc', '.subst works with :g';
    is "a1b2c".subst(/\d/, {$/+1}, :g), 'a2b3c', '.subst works with $/';

    ok Str.^can("subst"), "Str can subst";
    ok Str.^can("defined"), "Str can defined";
    nok Str.^can("quux"), "Str cannot quux";

    rxtest /z .* y [ a ::> x || . ]/, "z.*y[a::>x||.]",
        ("zyax", "zyb", "zyaxya"), ("zya",);
    rxtest /z [ [ a ::> x || . ] | . y ]/, "z[[a::>x||.]|.y]", ("zay",), Nil;
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
