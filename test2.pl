# vim: ft=perl6
use Test;

{
    is chars("foo"), 3, '&chars works';
    is substr("Hello",1,3), 'ell', '&substr works';
    is substr("Hello",2), "llo", '&substr works (2 args)';
    is reverse(1,2,3).join("|"), '3|2|1', '&reverse works';
    is join("|",1,2,3), '1|2|3', '&join works';
    my @foo = 4,5,6;
    is join("|",item @foo), '4 5 6', '&item works';
    is join("|",@foo.item), '4 5 6', 'Mu.item works';
    is (not False), 'Bool::True', '&not works';
    is (defined 5), 'Bool::True', '&defined works';
    push @foo, 7, 8;
    is join("|",@foo), '4|5|6|7|8', '&push works';
    unshift @foo, 2, 3;
    is join("|",@foo), '2|3|4|5|6|7|8', '&unshift works';
    is pop(@foo), '8', '&pop works';
    is shift(@foo), '2', '&shift works';
    is join("|",@foo), '3|4|5|6|7', '... with side effects';
    is +True, '1', "Bool.Numeric works";
    my %bar = :a<9>;
    is %bar<a>, '9', "Hash.LISTSTORE works";
    %bar = :c<9>;
    ok (!defined %bar<a>), "Hash.LISTSTORE clears existing";
    is keys(%bar), "c", "Hash.keys works";
    is values(%bar), "9", "Hash.values works";
    is (join "|", %bar.kv), "c|9", "Hash.kv works";
    is (%bar.invert.<9>), "c", "Hash.invert works";
    ok %bar<c> :exists, ":exists works";
    is (%bar<c> :delete), "9", ":delete returns old";
    ok !(%bar<c> :exists), ":delete removes value";
}

{
    my class A {
        method tom() { 12 }
        method foo($x) { $x * $x }
        method bar(:$x) { $x + $x }
    }
    my class B is A {
        method tom() { nextsame; }
        method foo($x) { nextsame; } #OK
        method bar(:$x) { nextsame; } #OK
    }
    is B.tom(), 12, "nextsame functional";
    is B.foo(5), 25, "nextsame functional w/ argument";
    # TODO
    # is B.bar(:x(7)), 14, "nextsame functional w/ named arg";

    sub foo(*%x) { %x }
    is foo(:z(2))<z>, 2, "slurpy hashes work";
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
