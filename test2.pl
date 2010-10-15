# vim: ft=perl6
use Test;
use MONKEY_TYPING;

sub grep($filter, *@items) { @items.grep($filter) }
sub map($callback, *@items) { @items.map($callback) }

augment class Any {
}

sub sort(*@bits) { @bits.sort }

sub _array_constructor(\$parcel) { anon @new = $parcel }
sub _hash_constructor(\$parcel) { anon %hash = $parcel }

ok 'cow' le 'sow', 'cow le sow';
ok !('sow' le 'cow'), 'sow !le cow';
ok 'row' lt 'tow', 'row lt tow';
ok 'how' gt 'bow', 'how gt bow';
ok 'yow' ge 'yow', 'yow ge yow';
is join("|", sort <c f d z a>), 'a|c|d|f|z', '&sort works';
is join("|", <a3 b2 c1 d0>.sort({ substr($^a,1) leg substr($^b,1) })),
    'd0|c1|b2|a3', '.sort with callback works';

is ("yayay" ~~ /y\w*?y/), "yay", "minimal matching works";
is ("yayay" ~~ /y**?a/), "y", "minimal matching works with **";

is +[ 2 ], 1, "array construction w/ one argument";
is +[ ], 0, "array construction w/ no arguments";
is +[ 3, 4 ], 2, "array construction w/ two";
is +[ $( 3, 4 ) ], 1, "array construction w/ scalar argument";

{
    sub bar { $*x + $*x }
    sub foo($*x) { bar }
    is foo(12), 24, "*-twigilled arguments work";
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
