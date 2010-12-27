# vim: ft=perl6
use Test;
use MONKEY_TYPING;

augment class Cool {
}

{
    my class A { method Numeric { 42 } }
    is A.new + 23, 65, '+ calls user-written .Numeric';
}

{
    "abc" ~~ /. (.) ./;
    is $0, 'b', 'Regex matches set $/';
}

{
    "abc" ~~ /de(.)/;
    ok !defined($/), 'Failed regex matches clear $/';
}

{
    my @foo = 1, 2, 3, 4, 5;

    is @foo[2,3].join('|'), '3|4', 'slicing works';
    is @foo[[1,4]].join('|'), 3, 'items do not slice';
    ok !defined(@foo[4,5,6][1]), 'slices off end work';
    @foo[1,2,3] = 5,6,7;
    is @foo.join('|'), '1|5|6|7|5', 'can assign to slices';
    @foo[4,5,6] = 1,2,3;
    is @foo.join('|'), '1|5|6|7|1|2|3', 'can assign to slices off end';

    my %quux;
    %quux<a b c> = (1, 2, 3);
    is %quux<b>, 2, 'can assign to hash slices';
    is %quux<c b>.join('|'), '3|2', 'can read from hash slices';

    my @bar = 1, 2, 3, 4, 5;
    is @bar[{ $^x / 2 }], 3, 'code indexes work';
    is @bar[*-1], 5, 'WhateverCode indexes work';
}

{
    my @arr;
    my $ix = -1;
    ok !(defined @arr[$ix]), "can index before arrays to get undef";
}

{
    is ("foo bar baz".split(/\s/).join('|')), 'foo|bar|baz', 'basic split';
    is ("foo bar baz".split(' ').join('|')), 'foo|bar|baz', 'split with string';
    is ("foo bar baz".split(' ', 2).join('|')), 'foo|bar baz',
        'split with a limit';
    is ("  foo bar".split(' ').join('|')), '||foo|bar',
        'split with leading empty fields';
    is ("foo bar  ".split(' ').join('|')), 'foo|bar||',
        'split with trailing empty fields';
    try { "foo bar".split };
    ok $!, 'split requires an argument';
    is ("ax+by*cz".split(/\W/, :all).join('|')), 'ax|+|by|*|cz',
        'split :all';

    is "hello world".index('l'), 2, ".index";
    is "hello world".index('l',5), 9, ".index with restart point";
    ok (!defined("hello world".index('x'))), ".index off end";
    is "hello world".rindex('l'), 9, ".rindex";
    is "hello world".rindex('l', 6), 3, ".rindex with restart point";
    ok (!defined("hello world".rindex('x'))), ".rindex off end";

    is ("abc".comb.join('|')), 'a|b|c', 'comb with default matcher';
    is ("abc".comb(/./, 2).join('|')), 'a|b', 'comb with limit';
    is ("A1 B2 C3".comb(/(\w)(\d)/, :match).[2].[1]), 3, 'comb :match';
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
