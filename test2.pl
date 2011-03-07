# vim: ft=perl6
use Test;
use MONKEY_TYPING;

{
    "foo" ~~ /\w+/;
    is $0, "foo", 'Match[0] returns whole match if no parens';

    constant %bar = (:a, :b);
    is +[ %bar ], 2, "constant hashes flatten";
}

{
    package Foo7426 { ... }
    class Foo7426::Inner {
        method pie() { 32 }
    }

    is Foo7426::Inner.pie, 32, ":: class names work";

    class Foo9215::Inner {
        method pie() { 64 }
    }

    is Foo9215::Inner.pie, 64, ":: class names work, without predecl";

    eval_dies_ok 'my class A::B { }', 'cannot use :: with my';
    eval_dies_ok 'my class A::B { }; B', 'A::B does not install B alias';
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
