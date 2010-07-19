# vim: ft=perl6

my $testnum = 1;
sub ok($bool, $tag) {
    my $not = (if $bool { "" } else { "not " });
    say ($not ~ ("ok " ~ ($testnum++ ~ (" - " ~ $tag))));
}

sub plan($num) {
    say ("1.." ~ $num);
}

plan 82;

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

{
    my $x = sub () { 5 };
    ok $x() == 5, 'Anonymous functions can be called';

    sub const($y) { sub { $y } }

    my $z = const(42);
    ok $z, "subs are true";
    ok $z() == 42, "subs close over lexicals";
    my $w = const(81);
    ok !($w === $z), "sub returns different values in different clonings";
    ok $w() == 81, "new sub captures new values";
    ok $z() == 42, "old sub keeps old value";
}

{
    sub accum() {
        anon sub go() {
            state $x = 0;
            $x++;
        }
    }

    my $f = accum;
    my $g = accum;

    ok $f() == 0, "state variables can be initialized";
    ok $f() == 1, "state variables preserve values";
    ok $g() == 0, "different clones have different state vars";
}

{
    my $x;
    my $unclonable-sub = PRE-INIT { sub () { $x } };
    $x = 42;
    ok $unclonable-sub() == 42, "mainlines are not cloned";
}

{
    my class A { }
    my class B is A { }
    my class C is B { }

    ok A.^isa(Any), "a new class is Any";
    ok B.^isa(A), "a subclass is the superclass";
    ok C.^isa(A), "isa is transitive";
    ok !(A.^isa(B)), "a superclass is not the subclass";

    ok B.^does(A), "a subclass does the superclass";
    ok !(A.^does(B)), "a superclass not-does the subclass";
}

ok "Foo".^isa(Str), "strings are Str";
ok (?1).^isa(Bool), "booleans are Bool";
ok (1.HOW).^isa(ClassHOW), "class objects are ClassHOW";

{
    my $canary = 1;

    ok 1 || ($canary = 0), "1 || ? returns true";
    ok $canary, "without touching the rhs";
    ok !(0 && ($canary = 0)), "0 && ? returns false";
    ok $canary, "without touching the rhs";
    ok (0 // ($canary = 0)) eq '0', "0 // ? returns 0";
    ok $canary, "without touching the rhs";
    ok (12 && 34) == 34, "12 && 34 -> 34";
    ok (2 andthen "three") eq "three", '2 andthen three -> three';
    ok (12 || 34) == 12, '12 || 34 -> 34';
    ok (0 || 34) == 34, '0 || 34 -> 34';
}
