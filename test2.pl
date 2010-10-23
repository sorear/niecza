# vim: ft=perl6
use Test;
use MONKEY_TYPING;

{
    my $m = "ab" ~~ / (.) <alpha> /;
    is (@$m)[0], "a", "Match.list returns positional captures";
    is (%$m)<alpha>, "b", "Match.hash returns named";
    is ((-> $p, :$alpha { $p, $alpha })(|$m)).join("|"), "a|b", "Match.Capture returns both";
    my @arr = "abc" ~~ / (.) (.) (.) /;
    is @arr.join("|"), "a|b|c", "Regex.ACCEPTS in list context returns captures";
    $m = "" ~~ / <O( foo => 2 )> /;
    is $m<O><foo>, 2, "<O> is functional";

    $m = (grammar {
        proto token TOP {*}
        token TOP:foo { <sym> }
    }).parse("foo");
    is $m<sym>, "foo", '$<sym> is functional';

    ok !(try die "foo").defined, "try of an error is undef";
    is $!, "foo", 'the error goes into $!';

    {
        my @*foo = 1, 2, 3;
        {
            temp @*foo;
            push @*foo, 4;
            is +@*foo, 4, '@*foo has 4 elements in temp scope';
        }
        is +@*foo, 3, '@*foo has 3 elements again after temp';
    }

    my @ar = [1, 2, 3];
    is +@ar, 1, "array constructors are singular";
    my $i = 0;
    $i++ until $i == 10;
    is $i, 10, "until loops functional";
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
