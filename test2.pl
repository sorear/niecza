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

{
    my $i = 0;
    L1: while $i < 10 { L1.last if $i == 5; $i++ }
    is $i, 5, "method .last works";

    L2: for 2,3,4 { $i = $_; last L2 if $i == 3; }
    is $i, 3, "last in for works";
}

{
    my ($str, $v1, $v2);
    $str = "abcdef";
    substr($str,1,2) = "xy";
    is $str, "axydef", "lvalue substr is functional";
    $str = "abcdef";
    substr($str,1,2) = "jklmno";
    is $str, "ajklmnodef", "lvalue substr can change string length";
    $str = "abcdef";
    $v1 := substr($str,1,2);
    $str = "xyzw";
    is $v1, "yz", "substr return values read lazily";
    $str = "abcdef";
    $v1 := substr($str,0,1);
    $v2 := substr($str,3,1);
    $v1 = "xx";
    $v2 = "yy";
    is $str, "xxbyydef", "substr thunks track by index";
}

{
    my class A {
        multi method foo(Str $, Any $) { "A" }
    }
    my class B is A {
        multi method foo(Any $, Str $) { "B" }
    }
    is B.foo("x","y"), "B", "MRO used as tiebreaker";
    multi bar(Str $, Any $) { "X" } #OK
    {
        multi bar(Any $, Str $) { "Y" }
        is bar("a","b"), "Y", "depth used as tiebreaker";
    }

    my $ok;
    given 1 {
        when 2 { }
        default { $ok = True }
    }
    ok $ok, "default works";
}

{
    my @q;
    sub capture() { push @q, caller.hints('$_') }

    $_ := 5;
    capture;
    for 6 { capture }
    capture given 7;
    capture;

    is @q[0], 5, 'can capture $CALLER::_ from run-once block';
    is @q[1], 6, 'can capture $_ from a run-many block';
    is @q[2], 7, 'can capture temporary $_ from postfix given';
    is @q[3], 5, '$_ not disturbed by given';
}

{
    $_ := "baar";
    ok (/a+/ ?? True !! False), "Regex.Bool works";
    is $/.chars, 2, 'Regex.Bool sets $/ properly';

    is 'ab-c'.split(/<.ws>/).join('|'), '|ab|-|c|',
        'zero-width split works correctly';
}

{
    ok ![1,2,3].flattens, "[1,2,3] non-flatteny";
    ok [1,2,3].list.flattens, "[1,2,3].list flatteny";

    is Array.perl, "Array", ".perl: Array";
    is [].perl, "[]", ".perl: []";
    is [1].perl, "[1]", ".perl: [1]";
    is [1,2,3].perl, "[1, 2, 3]", ".perl: [1,2,3]";
    is @([1,2,3]).perl, "[1, 2, 3].list", '.perl: @([1,2,3])';

    is Hash.perl, "Hash", ".perl: Hash";
    is {a => 1}.perl, '{"a" => 1}', '.perl: {a => 1}';
    is %({a => 1}).perl, '{"a" => 1}.hash', '.perl: %({a => 1})';

    is Num.perl, "Num", ".perl: Num";
    is 5.perl, "5", ".perl: 5";

    is Str.perl, "Str", ".perl: Str";
    is "foo".perl, '"foo"', '.perl: "foo"';

    is Capture.perl, "Capture", '.perl: Capture';
    is (\1).perl, '\(1)', '.perl: \1';
    is (\(1, :x)).perl, '\(1, |{"x" => Bool::True})', '.perl: \(1, :x)';
    is (\(:x)).perl, '\(|{"x" => Bool::True})', '.perl: \(:x)';

    is Parcel.perl, "Parcel", '.perl: Parcel';
    is ().perl, '()', '.perl: ()';
    is (1,).perl, '(1, )', '.perl: (1,)';
    is (1,2,3).perl, '(1, 2, 3)', '.perl: (1,2,3)';
    is $(1,2,3).perl, '$(1, 2, 3)', '.perl: $(1,2,3)';
}

{
    $_ := 5;
    if True {
        is $_, 5, '$_ passes into ifs';
    }

    if False {
    } else {
        is $_, 5, '$_ passes into elses';
    }

    my $i = 1;
    while $i-- {
        is $_, 5, '$_ passes into whiles';
    }
    given 8 {
        default {
            is $_, 8, '$_ passes into whens/defaults';
        }
    }
    {
        is $_, 5, '$_ passes into bare blocks';
    }

    sub foo($x is rw) { $x }
    dies_ok { foo 5 }, "cannot rw-bind constant";
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
