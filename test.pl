# vim: ft=perl6

use Test;

plan 205;

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
ok ("a" ~ "b" ~ "c") eq "abc", "a + b + c = abc";
ok (?1) eq "Bool::True", "True strings to Bool::True";
ok (?0) eq "Bool::False", "False strings to Bool::False";

ok False ~~ Bool && !False, "False is the right constant";
ok True ~~ Bool && True, "True is the right constant";
ok Bool::False ~~ Bool && !Bool::False, "Bool::False is the right constant";
ok Bool::True ~~ Bool && Bool::True, "Bool::True is the right constant";

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

        method !moo() { "Z" }
        method moop() { self!moo }

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
    ok Foo.moop eq 'Z', "can call 'private' methods";
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

{
    my $x = 42;
    ok (do $x) == 42, "do <statement> sees a lexical";
    ok (do { $x }) == 42, "do <block> sees a lexical";
    ok (do { my $x = 42; $x }) == 42, "do <block> sees a new lexical";
}

{
    ok 42.ACCEPTS(42), "accepts can match stuff";
    ok 12 ~~ "12", "strings match string value";
    ok "Hi" ~~ Str, "types match identity";
    ok (?0) ~~ (?1), "bools are a constant";
}

{
    constant foo = 42;
    constant $bar = 51;
    ok foo == 42, "constants without sigils work";
    ok $bar == 51, "constants with sigils work";
}

{
    ok "Hello".substr(1,3) eq "ell", "substr works";
    ok "Hello".chars == 5, ".chars works";
}

{
    my $buf = '';
    sub cat(*@x) {
        while @x {
            $buf = $buf ~ @x.shift;
        }
        $buf;
    }
    ok cat(1, (2, 3), ((4, 5), (Nil, 7))) eq "123457", "parcels flatten";
}

{
    my package Foo { our $x = 42; }
    ok $Foo::x == 42, "can access our vars";
    my module Bar { our $x = 42; }
    ok $Bar::x == 42, "module accepted too";
    my class Baz { our $x = 42; }
    ok $Baz::x == 42, "and class";

    ok !$Cow::x.defined, "package variables autoviv to undef";
    $Cow::x = 51;
    ok $Cow::x == 51, "but can still hold values";
}

{
    our $kluw = 99;
    ok $GLOBAL::kluw == 99, "GLOBAL:: works";
    ok $OUR::kluw == 99, "OUR:: works";
}

{
    my class Foo {
        has $.a;
        has $!b;

        method test() {
            ok self.a == 33, "values accessible as self.a";
            ok $.a == 33, 'values accessible as $.a';
            $!b = 44;
            ok $!b == 44, 'private values accessible as $!b';
            ok $!a == 33, 'public values so accessible too';
        }
    }

    my class Bar is Foo {
        has $.c;
    }

    my $x = Foo.new;
    my $y = Bar.new;

    $x.a = 33;
    $x.test;
    $y.a = 33;
    $y.test;
    $y.c = 55;
    ok $y.c == 55, "subclass attributes work";
}

{
    sub foo($v) {
        ok $*FOO == $v, "contextuals accessible";
    }

    my $*FOO = 99;
    foo(99);
    ok !$*BAR.defined, "contextuals undefined are undef";

    $PROCESS::qaax = 555;
    ok $*qaax == 555, "contextuals default to PROCESS";
    $GLOBAL::qeex = 5;
    $PROCESS::qeex = 3;
    ok $*qeex == 5, "GLOBAL takes precedence";
    $GLOBAL::quux = 111;
    ok $*quux == 111, "contextuals default to GLOBAL too";
    {
        my $*quux = 222;
        ok $*quux == 222, "but can be overriden";
    }
    ok $*quux == 111, "but only in the one scope";
}

{
    my $whatever = *;
    ok $whatever.WHAT eq 'Whatever()', "can call methods on a specific Whatever";
    my $wwhat = *.WHAT;
    ok !($wwhat.^isa(Whatever)), "method calls against * curry, though";

    ok (* + 5)(2) == 7, "can curry simple binops";
    ok ((*) eq "foo") eq "Bool::False", "parens defeat Whatever directly";
    ok (1 + 2 * *)(5) == 11, "nested Whatever works";
    ok (2 * (1 + *))(5) == 12, "parens for grouping do not kill WhateverCode";
    ok (* + *)(5,12) == 19, "multiple *, multiple args";
    ok ((2 * *) + (3 * *))(13,19) == 83, "even from groups";
    ok (1,*).^isa(Parcel), "infix:<,> doesn't curry";
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

{
    my $x1; my $x2; my $x3; my $x4;
    $x1 = 1 if 0;
    $x2 = 1 if 1;
    $x3 = 1 unless 0;
    $x4 = 1 unless 1;
    ok !$x1, "if 0 doesn't execute";
    ok $x2, "if 1 does execute";
    ok $x3, "unless 0 does execute";
    ok !$x4, "unless 1 doesn't execute";
}

{
    ok ({ $_ * $_ })(20) == 400, '$_ treated as placeholder';
    ok ({ $^a - $^b })(5,3) == 2, '$^x treated as such';
    ok ({ $^b - $^a })(3,5) == 2, '... in the right order';
}

{
    my $canary = 1;
    ok !(1 > 2 > ($canary = 0)), "1 > 2 > ... is false";
    ok $canary, "short circuitally";
    ok !(1 < 3 < 2), "1 < 3 < 2 is false";
    ok (1 < 2 < 3), "1 < 2 < 3 is true";
}

ok (-42) + 42 == 0, "unary minus works";

{
    ok Q:to/EOA/.substr(0,5) eq 'Hello', "heredocs work";
Hello
EOA

    ok Q:to/EOB/ eq Q:to/EOC/, "multiple heredocs in a line work";
Foo
EOB
Foo
EOC
}

is $?FILE, 'test.pl', '$?FILE works';
is $?ORIG.substr(0,5), '# vim', '$?ORIG works';

{
    my @x;

    ok @x ~~ Array, '@x isa Array';
    ok @x.elems == 0, 'no elements';
    ok +@x == 0, 'no elements (+)';

    @x.push(5);
    ok @x.elems == 1, 'one element now';
    ok @x.shift == 5, 'element removed is 5';
    @x.push(7,8);
    ok @x.elems == 2, "added two elements";
    ok @x.shift == 7, "removed first correctly";
    ok @x.shift == 8, "removed second correctly";
    ok @x.elems == 0, "no elements again";

    my $k = 2;
    @x.push($k);
    $k = 3;
    ok @x.shift == 2, "push copies";

    @x.push(11,12);
    ok @x.pop == 12, "pop is LIFO (1)";
    ok @x.pop == 11, "pop is LIFO (2)";
    ok +@x == 0, "pop removed all elements";

    my @y = 15;
    ok @y.elems == 1, "assigning a single value makes a single-item list";
    ok @y.shift == 15, "and the value came right!";

    @y = 1, 2, 3;
    ok @y.elems == 3, "list assignment gets correct item count";
    is @y.join("|"), '1|2|3', "assignment of multiple values works properly";

    @y = 1, (2, 3), 4;
    ok @y.elems == 4, "list assignment flattens (1)";
    is @y.join("|"), '1|2|3|4', "list assignment flattens (2)";

    @y = 5, @y;
    ok @y.elems == 5, "self assignment works (1)";
    is @y.join("|"), '5|1|2|3|4', "self assignment works (2)";
}

{
    my $a = 0;
    my $b = 0;
    my $c = 0;
    my $d = 0;
    my $e = 0;
    my $f = 0;

    (Any!butWHENCE({ $a = 1 }));
    my $x := (Any!butWHENCE({ $b = 1 }));  #OK not used
    my $y ::= (Any!butWHENCE({ $c = 1 })); #OK not used
    my $z = (Any!butWHENCE({ $d = 1 }));   #OK not used
    (Any!butWHENCE({ $e = 1 })) = 2;
    (Any!butWHENCE({ $f = 1 })) := 3;

    ok !$a, "no autovivification in void context";
    ok $b, "autovivification after rw bind";
    ok !$c, "no autovivification after ro bind";
    ok !$d, "no autovivification after rvalue context";
    ok $e, "autovivification after lvalue context";
    ok $f, "autovivification after bvalue context";
}

{
    sub postcircumfix:<[ ]>($a, $b, $c) { $a ~ "|" ~ $b ~ "|" ~ $c }
    is 1[2,3], "1|2|3", "can call postcircumfix [ ]";
}

{
    sub postcircumfix:<{ }>($a, $b, $c) { $a ~ "|" ~ $b ~ "|" ~ $c }
    is 1{2,3}, "1|2|3", 'can call postcircumfix { }';
}

{
    my @arr = <a b c>;
    is @arr.join("|"), 'a|b|c', "word splitter works";

    my @narr;
    @narr[0];
    ok +@narr == 0, "rvalue reference to out of range value does not add";
    @narr[2] = 5;
    ok +@narr == 3, "assigning to element 2 makes length 3";
    ok !(@narr[0].defined), "first element undefined";
    ok !(@narr[1].defined), "second element undefined";
    ok @narr[2] == 5, "third element properly assigned";

    my @darr;
    @darr[1][1];
    ok +@darr == 0, "rvalue nested reference, no effect";
    @darr[2][2] = 'pie';
    ok +@darr == 3, "outer level vivifies elements";
    ok @darr[2] ~~ Array, "inner Array created";
    is @darr[2][2], 'pie', "inner value retained";
}

{
    sub postcircumfix:<{ }> { @_.join("|") }
    is 1<2 3>, '1|2|3', "angle bracket postcircumfix works";
}

{
    my $foo;
    ok !($foo<x>.defined), "fetch from hash, no value";
    ok !($foo.defined), "no autoviv for rvalue";
    $foo<x> = 'foo';
    is $foo<x>, 'foo', "values are retained";
    ok !($foo<y>.defined), "no cross-slot leakage";
    ok $foo ~~ Hash, "foo isa hash now";
    $foo<z><a> = 'pie';
    is $foo<z><a>, 'pie', "can autoviv deeply";
    $foo<y>[2] = 'zed';
    is $foo<y>[2], 'zed', "can mix array and hash viv";
    $foo<12> = 'fry';
    is $foo{12}, 'fry', "keys are strings";
}
