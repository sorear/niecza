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
    my $log;
    my $ret;
    my $var := _newtiedscalar(Any, Any, { $log ~= "F"; $ret }, { $log ~= $_ });

    $ret = 5; $log = "";
    my $a = $var;
    is $a, 5, "fetches work";
    is $log, "F", "fetch called once only";

    $ret = 3; $log = "";
    $var = 9;
    is $log, "9", "stores work";

    $log = "";
    $a = _newtiedscalar(Any, { $log ~= "B" }, { Any }, { Any });
    is $log, "", "bind not called spuriously (1)";

    $log = "";
    $a ::= _newtiedscalar(Any, { $log ~= "B" }, { Any }, { Any });
    is $log, "", "bind not called spuriously (2)";

    $log = "";
    my $b := _newtiedscalar(Any, { $log ~= "B" }, { Any }, { Any }); #OK
    is $log, "B", "bind called when needed (bind)";

    $log = "";
    _newtiedscalar(Any, { $log ~= "B" }, { Any }, { Any }) = 5;
    is $log, "B", "bind called when needed (write)";
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
        multi method b1(Bool $) { "bool" }
        multi method b1(Str $) { "str" }

        multi method b2(Bool $) { "bool" }
        multi method b2(Any $) { "any" }

        multi method b3(Any $) { "any" }
        multi method b3(Bool $) { "bool" }

        multi method b4(Bool $ , Any $) { "doom" }
        multi method b4(Any $ , Bool $) { "doom" }
    }

    is C20.b1(True), "bool", "multimethods work (1)";
    is C20.b1("foo"), "str", "multimethods work (2)";

    is C20.b2(True), "bool", "multimethod sorting works (1)";
    is C20.b2("foo"), "any", "multimethod sorting works (2)";
    is C20.b3(True), "bool", "multimethod sorting works (3)";
    is C20.b3("foo"), "any", "multimethod sorting works (4)";

    dies_ok { C20.b4("foo", "bar") }, "multimethod fail checking works";
    dies_ok { C20.b4(True, True) }, "multimethod tie checking works";
}

{
    multi foo(Str $) { "str" }
    multi foo(Bool $) { "bool" }

    is foo(True), "bool", "multisubs work (1)";
    is foo("abc"), "str", "multisubs work (2)";

    {
        multi foo(Any $) { "any" }
        is foo(True), "bool", "augmenting multisubs works (1)";
        is foo(5), "any", "augmenting multisubs works (2)";

        # {
        #     proto foo($) {*}
        #     multi foo(Any $) { "any2" }
        #     is foo(True), "any2", "proto-shadowing works";
        # }

        {
            sub foo(Any $) { "any3" }
            is foo(True), "any3", "only-shadowing works";
        }
    }
}

{
    sub circumfix:<《 》>($a) { "|{$a}|" }
    sub postcircumfix:<「 」>($a,$b) { "|{$a}|{$b}|" }

    is 《 2 》, "|2|", "user circumfix works";
    is 3「4」, "|3|4|", "user postcircumfix works";
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
