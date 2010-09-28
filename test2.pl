# vim: ft=perl6
use Test;

{
    my $x;
    INIT {
        $x = 1;
    }
    ok $x, "changes made in the protolexpad are visible at runtime";
}

ok INIT { 1 }, "init blocks can return values";

{
    my $x;
    my $unclonable-sub = INIT { sub () { $x } };
    $x = 42;
    ok $unclonable-sub() == 42, "mainlines are not cloned";
}

is $?FILE, 'test.pl', '$?FILE works';
is $?ORIG.substr(0,5), '# vim', '$?ORIG works';

{
    {
        our $x = 5; #OK
    }
    ok $::x == 5, '$::x finds our variable';

    package Fao { our $y = 6; } #OK
    ok $::Fao::y == 6, '$::Fao::y works as $Fao::y';

    { class Mao { } }
    ok ::Mao.new.defined, 'can use classes via ::Mao';
}

{
    my $x = 7; #OK
    ok $::x == 7, '$::x can find lexicals';
    class A3 {
        method moo { 42 }
        class B4 {
            ok ::A3.moo, '::A3 can find outer classes';
        }
    }
}

{
    class Foo {
        method foo() { 42 }
        class Bar {
            method bar() { 51 }
        }
        ok Bar.bar == 51, "within Foo, Bar is directly accessible";
        ok OUR::Bar.bar == 51, "within Foo, Bar is package accessible";
        ok Foo::Bar.bar == 51, "within Foo, Bar is longname accessible";
        ok GLOBAL::Foo::Bar.bar == 51, "within Foo, Bar is GLOBAL accessible";
    }
    ok Foo eq 'Foo()', "lexical lookup of our-class works";
    ok OUR::Foo eq 'Foo()', "also visible in ourpad";
    ok GLOBAL::Foo eq 'Foo()', "also visible globally";
    ok Foo::Bar.bar == 51, "can call through nested methods";
    ok GLOBAL::Foo::Bar.bar == 51, "can call through GLOBAL nested";
}


done-testing;
