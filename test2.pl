# vim: ft=perl6
use Test;
use MONKEY_TYPING;

augment class Cursor {
    method suppose($rx) {
        my $*IN_SUPPOSE = True;
        my $*FATALS = 0;
        my @*WORRIES;
        my %*WORRIES;
        my $*HIGHWATER = -1;
        my $*HIGHEXPECT = {};
        try {
            my $ret = first($rx(self));
            if ($ret) { return self }
        };
        return ();
    }
}

    ok "foo" !~~ / f <.suppose { die }> /, ".suppose works (F)";
    ok "foo" ~~ / f <.suppose o> oo /, ".suppose works (T)";

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
