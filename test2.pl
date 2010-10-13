# vim: ft=perl6
use Test;

sub infix:<x>($str, $ct) {
    my $i = +$ct;
    my $j = ''; # XXX use strbuf
    while $i >= 1 {
        $i--;
        $j ~= $str;
    }
    $j;
}

sub grep($filter, *@items) { @items.grep($filter) }
sub map($callback, *@items) { @items.map($callback) }

sub infix:<leg>($s1, $s2) {
    Q:CgOp { (box Num (cast num (strcmp (unbox str (@ {$s1.Str})) (unbox str (@ {$s2.Str}))))) }
}

sub infix:<ge>($s1, $s2) { ($s1 leg $s2) >= 0 }
sub infix:<gt>($s1, $s2) { ($s1 leg $s2) > 0  }
sub infix:<le>($s1, $s2) { ($s1 leg $s2) <= 0 }
sub infix:<lt>($s1, $s2) { ($s1 leg $s2) < 0  }

ok 'cow' le 'sow', 'cow le sow';
ok !('sow' le 'cow'), 'sow !le cow';
ok 'row' lt 'tow', 'row lt tow';
ok 'how' gt 'bow', 'how gt bow';
ok 'yow' ge 'yow', 'yow ge yow';

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
