# vim: ft=perl6
use Test;
use MONKEY_TYPING;

{
    my class Foo {
        has @.bar;
        has %.baz;
        has @.quux = 1,2,3;
    }
    isa_ok Foo.new.bar, Array, '@.bar initializes as an array';
    isa_ok Foo.new.baz, Hash, '%.baz initializes as a hash';
    is +[ Foo.new(bar => (1,2,4)).bar ], 3, '@.bar initializes with list context';
    is +[ Foo.new(bar => 5).bar ], 1, '@.bar can initialize from a single item';
    is +[ Foo.new.quux ], 3, '@.quux with init list works';

    my $str = '';
    for 1,2,3,4 -> $x, $y { $str ~= "$x|$y," }
    is $str, "1|2,3|4,", 'multivariable for works';

    is "moo".subst('o','a',:g), "maa", '.subst can take Str';
    is 'Hello'.substr(1), 'ello', '.substr can take 1 arguaent';
    is hash((a => 1)).perl, '{"a" => 1}.hash', '&hash works (1)';
    is hash((a => 1, b => 2)).<b>, 2, '&hash works (2)';
    is hash({a => 1}).perl, '{"a" => 1}.hash', '&hash works (3)';

    my %hash = "foo", 5;
    is %hash<foo>, 5, "Hash.LISTSTORE can take keys and values separately";
    dies_ok { %hash = "pie" }, "keys must be matched";
}

{
    class X7140::X1122 { }
    my X7140::X1122 $obj .= new;
    isa_ok $obj, X7140::X1122, 'Type constraints via :: work';

    sub foo(X7140::X1122 $) { }
    lives_ok { foo($obj) }, 'Parameter :: constraints work (1)';
    dies_ok { foo(5) }, 'Parameter :: constraints work (1)';

    sub bar(@x) {} #OK
    lives_ok { bar <a b c> }, '<> splitting counts as one argument';

    my class Foo { method foo() { 12 } }
    is Foo.?foo, 12, '.? works (successful)';
    is +[Foo.?bar], 0, '.? works (unsuccessful, list)';

    my $k = 2;
    my $st = '';
    while $k -> $z { $st ~= $z; $k = False }
    is $st, '2', 'while loops can take ->';

    $st = ''; $k = False;
    until $k -> $z { $st ~= $z; $k = True }
    is $st, 'Bool::False', 'until loops can take ->';

    $st = '';
    unless False -> $z { $st ~= $z }
    is $st, 'Bool::False', 'unless can take ->';

    $st = ''; $k = True;
    repeat until $k -> $z { $st ~= ($z // 5); $k = !$k; }
    is $st, '5Bool::False', 'repeat until (prefix) can take ->';

    $st = ''; $k = True;
    repeat -> $z { $st ~= ($z // 5); $k = !$k; } until $k;
    is $st, '5Bool::False', 'repeat until (postfix) can take ->';
}

{
    sub foo($x is copy) { $x++; $x }
    is foo(5), 6, "is copy works (non inline)";
    my $y;
    for 5 -> $k is copy { $k++; $y = $k }
    is $y, 6, "is copy works (inline)";
}

{
    is "filename0000.jpg".succ, "filename0001.jpg", "filename0000.jpg test";
    is "000".succ, "001", "basic succ";
    is "009".succ, "010", ".succ with carry";
    is "099".succ, "100", ".succ with cascading carry";
    is "a99".succ, "b00", ".succ with carry across types";
    is "z99".succ, "aa00", ".succ with extending (non-digit)";
    is "99".succ, "100", ".succ with extending (digit)";

    is "001".pred, "000", "basic .pred";
    is "010".pred, "009", ".pred with borrow";
    is "100".pred, "099", "cascading borrow";
    is "b00".pred, "a99", "borrow across types";

    is "--99--".succ, "--100--", "lengthening shifts";
    is "--00--".succ, "--01--", "not lengthening, no shift";
    is "00.00".succ, "01.00", "dot sets endpoint";

    is +["a" .. "z"], 26, "char ranges work";

    is_deeply [[<za zz az aa>].sort(*.flip)], [<aa za az zz>],
        'Automatic Schwartzian transform works';
}

is 10.abs, 10, "10.abs == 10";
is (-10).abs, 10, "(-10).abs == 10";
is (-10).abs.WHAT, 10.WHAT, "(-10).abs and 10 have same WHAT";

{
    my $big = 2 ** 200 - 42;
    is $big.abs, $big, '$big.abs == $big';
    is (-$big).abs, $big, '(-$big).abs == $big';
    is (-$big).abs.WHAT, $big.WHAT, '(-$bi).abs and $big have same WHAT';
}

ok (10/2).abs == 10/2, "(10/2).abs == 10/2";
ok (-10/2).abs == 10/2, "(-10/2).abs == 10/2";
is (-10/2).abs.WHAT, (10/2).WHAT, "(-10/2).abs and 10/2 have same WHAT";

ok (10.Num).abs == 10.Num, "(10.Num).abs == 10.Num";
ok (-10.Num).abs == 10.Num, "(-10.Num).abs == 10.Num";
is (-10.Num).abs.WHAT, (10.Num).WHAT, "(-10.Num).abs and 10.Num have same WHAT";

ok (10 + 5i).abs == 125.sqrt, "(10 + 5i).abs == 125.sqrt";
ok (-10 - 5i).abs == 125.sqrt, "(-10 - 5i).abs == 125.sqrt";
is (-10 - 5i).abs.WHAT, "Num()", "(-10 - 5i).abs is a Num";

ok (10 + 5i).re == 10, "(10 + 5i).re == 10";
ok (10 + 5i).im == 5, "(10 + 5i).im == 5";

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
