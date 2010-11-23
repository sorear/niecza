# vim: ft=perl6
use Test;
use MONKEY_TYPING;

{
    our role R5634[$x] {
        regex ::($x) { foo }
    }

    ok (Grammar but OUR::R5634["TOP"]).parse("foo"), "roles with dynamic regex names work";

    my $M;
    my $t;
    $M = ("a()" ~~ / <alpha> '(' ~ ')' { $t = $<alpha>.Str } /);
    is $t, "a", "Inside of ~ can see captures";
    $M = ("(a)" ~~ / '(' ~ ')' <alpha> /);
    is $M<alpha>, "a", "Captures can escape from ~";

    my $died = 1;
    try { $*NONEX = 1; $died = 0; }
    ok $died, "Assignment to non-existing dynvar fails";

    is "foo".substr(5,2), "", "substr starting off end works";
    is "foo".substr(1,10), "oo", "substr ending off end works";

    rxtest /:i "abc"/, ':i "abc"',
        ("abc","aBc","ABC"),("cba","ab");

    my grammar G {
        proto token TOP {*}
        token TOP:foo { :i <sym> }
    }

    ok G.parse("fOo"), ":i <sym> works";

    my %h; %h<used>++;
    is %h<used>, 1, "autoviv works with ++";
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
