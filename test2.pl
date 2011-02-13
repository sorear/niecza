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

    given my $g { #OK
        $_ = 'abc';
        s/b/d/;
        is $_, 'adc', 'simple s/// works';
        is $/, 'b', 's/// sets $/';
        $k = 'bac';
        $k ~~ s/c/g/;
        is $k, 'bag', '~~ s/// works';
        $_ = 'abc';
        s/(\w)/$0$0/;
        is $_, 'aabc', 's/// can refer to $/';
        $_ = 'abc';
        ok ?(s/b/x/), 's/// is true if replacing';
        $_ = 'abc';
        ok !(s/d/x/), 's/// is false if not replacing';
        is $_, 'abc', '... and target unchanged';
        $_ = 'abc';
        s!a!xx!;
        is $_, 'xxbc', 's/// with alternate delims works';
        $_ = 'abc123';
        s{b} = 'g' ~ 'k';
        is $_, 'agkc123', 's{} = works';
        $_ = 'abc123';
        s{\D+} = $/ ~ $/;
        is $_, 'abcabc123', 's{} = can refer to $/';
        $_ = 'abc123';
        s{\d+} *= 2;
        is $_, 'abc246', 'metaoperator s{} works';
    }
}

{
    my $a = 0;
    {
        A: while True {
            $a++; while True { last A }; $a++;
            last;
        }
    }
    is $a, 1, "last with label works";
    sub funlp($fn) {
        A: while True {
            $fn(A)
        }
    }
    my $b = 0;
    funlp(-> $o {
        $b++; funlp(-> $i { last $o }); $b++; #OK
        last;
    });
    is $b, 1, "last with label object is not fooled by names";
    my $c = 0;
    funlp(-> $o { #OK
        $c++; funlp(-> $i { last "A" }); $c++; #OK
        last;
    });
    is $c, 2, "last with name picks innermost";

    sub loopy () { True }
    ok loopy, "can call functions starting with 'loop'";

    my @a = "abc" ~~ /abc/;
    is +@a, 1, "capture-less matches return 1 item";
    @a = "abc" ~~ /(a)(b)(c)/;
    is +@a, 3, "capturing matches return catures in list context";
}

{
    is ~[4,0 Z+ 2,0 Z+ 1,0], "7 0", "Z+ works";
    is ~[1,2 X+ 3,4 X+ 5,6], "9 10 10 11 10 11 11 12", "X+ works";
    is ~[4,0 Z  2,0 Z  1,0], "4 2 1 0 0 0", "Z works";
    is ~[1,2 X  3,4 X  5,6], "1 3 5 1 3 6 1 4 5 1 4 6 2 3 5 2 3 6 2 4 5 2 4 6", "X works";

    ok "{1}" ~~ Str, "string interpolation stringifies";
    is q:to[A] , " x\n", "q:to strips equal whitespace";
     x
    A
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
