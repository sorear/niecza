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

{
    my $x = 5;
    $x.=pred;
    is $x, 4, ".=foo works (dottyop form)";

    $x = 4; $x .= pred;
    is $x, 3, ".= foo works (infix form)";
}

{
    my class K { }
    my class L { has Str $.y }

    my Str $x;
    ok $x === Str, "typed variable initializes to type object";
    lives_ok { $x = "pie" }, "can assign correct type";
    dies_ok { $x = True }, "cannot assign wrong type";

    my K $z .= new;
    ok ($z.defined && $z ~~ K), 'my K $z .= new syntax works';

    my $l = L.new;
    ok $l.y === Str, "typed attribute initializes to type object";
    lives_ok { $l.y = "pie" }, "can assign correct type";
    dies_ok { $l.y = True }, "cannot assign wrong type";
}

{
    sub bar(Str $) {}
    lives_ok { bar "foo" }, "can pass correct type";
    dies_ok { bar True }, "cannot pass wrong type";

    is "foo".$({ uc $_ }), "FOO", "contextualizer variables";

    my grammar G19 {
        multi token foo:a { x }
        multi token foo:b { y }
    }
    ok "y" ~~ / :lang(G19) <foo> /, "can use multi regex without proto";

    my class C20 {
        multi method bar(Bool $) { "bool" }
        multi method bar(Str $) { "str" }
    }

    is C20.bar(True), "bool", "multimethods work (1)";
    is C20.bar("foo"), "str", "multimethods work (2)";
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
