# vim: ft=perl6

my $testnum = 1;
sub ok($bool, $tag) {
    my $not = (if $bool { "" } else { "not " });
    say ($not ~ ("ok " ~ ($testnum++ ~ (" - " ~ $tag))));
}

sub plan($num) {
    say ("1.." ~ $num);
}

plan 53;

ok 1, "one is true";
ok 2, "two is also true";
ok !(0), "zero is false";
ok 2 + 2 == 4, "two and two makes four";
{
    my $x = 2 + 2;
    ok $x == 4, "two and two can be stored in a variable";
}

ok 2 < 3, "two is less than three";
ok !(3 < 1), "three is not less than one";

ok 42 / 3 == 14, "division comes out in the right order";

{
    my $x = 2;
    ok (--$x) == 1, "predecrement returns new value";
    ok $x == 1, "value clearly decremented";

    ok ($x++) == 1, "postincrement returns old";
    ok $x == 2, "but value increased";
}

{
    my $x = 2;
    my $y := $x;

    ok $y == 2, "binding shares old value";
    $x = 5;
    ok $y == 5, "changing old changes new";
    $y = 4;
    ok $y == 4, "changing new changes old";
}

{
    sub fib($n) {
        if $n <= 2 {
            1
        } else {
            fib($n - 1) + fib($n - 2);
        }
    }

    ok fib(10) == 55, "recursion works";
}

{
    my $n = 9;
    my $a = 1;
    my $b = 1;
    while --$n >= 0 {
        my $c = $a + $b;
        $a = $b;
        $b = $c;
    }

    ok $a == 55, "looping works";
}

{
    my $x;
    PRE-INIT {
        $x = 1;
    }
    ok $x, "changes made in the protolexpad are visible at runtime";
}

ok PRE-INIT { 1 }, "preinit blocks can return values";

{
    sub foo() { 42 }
    ok (foo) == 42, "can call argless function without parens";
}

ok !Mu, "undefined type objects are false";
ok !Mu.defined, "type objects are undefined";
ok "Foo".defined, "strings are defined";
ok !Str.defined, "derived type objects are still undefined";

ok "foo" eq "foo", "equal strings are equal";
ok !("foo" ne "foo"), "equal strings are not not equal";
ok "foo" ne "bar", "unequal strings are unequal";

ok Mu === Mu, "identical objects are identical";
ok !(Mu === Any), "unidentical objects are unidentical";

ok 12 eq "12", "eq stringifies";
ok ("a" ~ "b") eq "ab", "a + b = ab";
ok (?1) eq "Bool::True", "True strings to Bool::True";
ok (?0) eq "Bool::False", "False strings to Bool::False";

ok ~Any eq "Any()", "~Any is Any()";

ok (2.WHAT === 3.WHAT), "different objects get the same WHAT";
ok !(2.WHAT.defined), "WHATs are undefined type objects";
ok (sub foo() {}).WHAT eq 'Sub()', 'WHAT of a Sub is Sub()';
ok "Foo".WHAT === Str, 'WHAT of a Str *is* Str';

ok "Foo".HOW.WHAT eq 'ClassHOW()', 'anything.HOW is a ClassHOW';
ok "Foo".HOW === "Cow".HOW, 'objects of the same class have the same HOW';
ok !("Foo".HOW === Any.HOW), 'objects of different classes have different HOWs';

{
    my class Foo {
        method zow() {
            "A";
        }

        method zilch() {
            "B";
        }

        method crow($x) {
            $x * $x;
        }

        method pie() {
            self.zow
        }
    }

    my class Bar is Foo {
        method zow() {
            "C";
        }
    }

    ok !Foo.defined, "class type objects are undefined";
    ok !Bar.defined, "even derived ones";
    ok Foo.zow eq 'A', "can call defined methods";
    ok Bar.zow eq 'C', "subclasses can override methods";
    ok Bar.zilch eq 'B', "not overriden methods are inherited";
    ok Foo.crow(13) == 169, "can call methods with arguments";
    ok Foo.pie eq 'A', "can call methods through self";
    ok Bar.pie eq 'C', "calls through self are virtual";
}

{
    my $x = 0;
    {
        START { $x = 1 };
        ok $x, "START blocks are run";
    }
}

{
    my $x = '';
    {
        $x = $x ~ '1';
        START { $x = $x ~ '2'; }
        $x = $x ~ '3';
    }
    ok $x eq '123', "START blocks are run in order with other code";
}

{
    my $x = '';
    my $y = 0;
    while $y < 3 {
        $x = $x ~ '1';
        START { $x = $x ~ '2'; }
        $x = $x ~ '3';
        $y++;
    }
    ok $x eq '1231313', "START blocks are only run once";
}

{
    my $x = '';
    my $z = 0;
    while $z < 2 {
        my $y = 0;
        while $y < 3 {
            $x = $x ~ '1';
            START { $x = $x ~ '2'; }
            $x = $x ~ '3';
            $y++;
        }
        $z++;
    }
    ok $x eq '12313131231313', "START blocks reset on clone";
}

