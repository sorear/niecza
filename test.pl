# vim: ft=perl6

use Test;

plan 562;

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
    sub foo() { 42 }
    ok (foo) == 42, "can call argless function without parens";
}

ok !Cool, "undefined type objects are false";
ok !Cool.defined, "type objects are undefined";
ok "Foo".defined, "strings are defined";
ok !Str.defined, "derived type objects are still undefined";

ok "foo" eq "foo", "equal strings are equal";
ok !("foo" ne "foo"), "equal strings are not not equal";
ok "foo" ne "bar", "unequal strings are unequal";

ok Cool === Cool, "identical objects are identical";
ok !(Cool === Any), "unidentical objects are unidentical";

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

    (Any!Any::butWHENCE({ $a = 1 }));
    my $x := (Any!Any::butWHENCE({ $b = 1 }));  #OK not used
    my $y ::= (Any!Any::butWHENCE({ $c = 1 })); #OK not used
    my $z = (Any!Any::butWHENCE({ $d = 1 }));   #OK not used
    (Any!Any::butWHENCE({ $e = 1 })) = 2;

    ok !$a, "no autovivification in void context";
    ok $b, "autovivification after rw bind";
    ok !$c, "no autovivification after ro bind";
    ok !$d, "no autovivification after rvalue context";
    ok $e, "autovivification after lvalue context";
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

{
    ok ("a" ~~ /a/), "letter matches itself";
    ok !("a" ~~ /b/), "letter does not match other";
    ok ("xxa" ~~ /a/), "leading garbage ignored";
    ok ("axx" ~~ /a/), "trailing garbage ignored";
    ok ("ab" ~~ /ab/), "sequence matches sequence";
    ok !("ab" ~~ /ba/), "sequence requires order";
    ok ("abc" ~~ /ab?c/), "conditional can match";
    ok ("ac" ~~ /ab?c/), "conditional can match nothing";
    ok !("adc" ~~ /ab?c/), "conditional cannot match something else";
    ok ("ac" ~~ /ab*c/), "kleene closure can match none";
    ok ("abbc" ~~ /ab*c/), "kleene closure can match many";
    ok !("adc" ~~ /ab*c/), "kleene closure cannot match other";
    ok ("abc" ~~ /ab+c/), "plus can match one";
    ok ("abbbc" ~~ /ab+c/), "plus can match many";
    ok !("adc" ~~ /ab+c/), "plus cannot match other";
    ok !("ac" ~~ /ab+c/), "plus cannot match none";

    grammar Bob {
        rule TOP {ab*c}
    }

    ok Bob.parse("abbc"), "grammars work (1)";
    ok !Bob.parse("adc"), "grammars work (2)";
    ok !Bob.parse("xac"), "grammars anchor (1)";
    ok !Bob.parse("acx"), "grammars anchor (2)";
}

{
    my $x = False;
    my $y = False;
    my @l1 := gather do { $x = True; take 1; $y = True };
    ok !$x, "gather does not run block immediately";
    ok @l1.shift == 1, "first value pulled";
    ok $x, "pull started block";
    ok !$y, "but did not finish it";
    ok !@l1, "no more values";
    ok $y, "querying that fact finished the block";
}

{
    my grammar G1 {
        regex TOP { <.foo> }
        regex foo { x }
    }

    ok G1.parse("x"), "subrules work (positive)";
    ok !G1.parse("y"), "subrules work (negative)";

    my grammar G2 {
        regex TOP { y <.foo> <.foo> y }
        regex foo { x }
    }

    ok G2.parse("yxxy"), "subrule position tracking works";
    ok !G2.parse("yxy"), "subrule position tracking works (2)";

    my grammar G3 {
        regex TOP { <moo> }
        regex moo { x }
    }

    ok G3.parse("x"), "capturing subrules work (positive)";
    ok !G3.parse("y"), "capturing subrules work (negative)";
}

{
    ok (&infix:<+>)(2,2) == 4, '&infix:<+> syntax works';
}

{
    sub foo($x = 5) { $x }
    ok foo() == 5, "defaults operate";
    ok foo(19) == 19, "can override defaults";
    ok !foo(Any).defined, "defaults do not misfire";
    my $y = 0;
    sub bar($x = $y++) { $x }
    ok bar() == 0, "expressional defaults operate";
    ok bar() == 1, "expressional defaults are called each time";
    ok bar(3) == 3, "expressional defaults can be overridden";
    ok bar() == 2, "when not used, expressional defaults are not called";
}

{
    sub foo($x?) { $x }
    ok !foo().defined, "defaults to undef";
    ok foo(19) == 19, "can override optionals";
    sub bar($x?, $y?) { ($x // 5), ($y // 9) }
    is bar().join("|"), "5|9", "2x defaulting works";
    is bar(10,20).join("|"), "10|20", "2x overriding works";
    is bar(10).join("|"), "10|9", "one value hits the right parameter";
}

{
    is :foo.value, 'Bool::True', ":foo is true";
    is :!foo.value, 'Bool::False', ":!foo is false";
    is :foo<12>.value, '12', ":foo<12> is 12";
    is :foo.key, 'foo', ":foo is foo";

    is (foo => 1).key, 'foo', "foo => 1 keeps key";
    is (foo => 1).value, '1', "foo => 1 keeps value";
    is ("foo" => 1).key, 'foo', '"foo" => 1 keeps key';
    is ("foo" => 1).value, '1', '"foo" => 1 keeps value';

    my %hash;
    ok %hash ~~ Hash, '%-vars are Hash';
}

{
    ok ("aab" ~~ /a* ab/), "a*ab backtracks";
    ok !("aab" ~~ /a*: ab/), "a*: ab doesn't";
    ok ("aab" ~~ /a*! ab/), "a*! ab backtracks";
    ok !("aab" ~~ /:r a* ab/), "ratcheting a* ab does not";
    ok !("aab" ~~ /:r a*: ab/), "ratcheting a*: ab does not";
    ok ("aab" ~~ /:r a*! ab/), "ratcheting a*! ab does";
    ok !("aab" ~~ token { a* ab }), "a* ab in a token does not";

    ok ("ab ab" ~~ / ab <.ws> ab /), "ws matches a space";
    ok (q:to/end/ ~~ / ab <.ws> ab /), "ws matches a newline";
ab
ab
end
    ok ("ab   ab" ~~ / ab <.ws> ab /), "ws matches several spaces";
    ok !("abab" ~~ / ab <.ws> ab /), "ws does not match nothing";
    ok ("ab   ab" ~~ rule { ab ab }), "rule gives space";
}

{
    sub meow(*@x) {
        is @x[0], 'a', "can index [0] slurpies";
        is @x[1], 'b', "can index [1] slurpies";
    }

    meow('a', 'b');

    # doing a more reasonable test will probably require embedded blocks
    ok "foobarx" ~~ / [ foo | foobar ]: x /, "LTM picks longest even if second";
    ok "foobarx" ~~ / [ foobar | foo ]: x /, "LTM picks longest even if first";
}

{
    my $x = '';
    ok !("a" ~~ / a { $x = 1; } b /), '{} does not terminate regex';
    is $x, 1, '{} is run even if regex fails';
    $x = '';
    ok !("" ~~ / a { $x = 1; } b /), '{} does not affect regex that ends before it';
    is $x, '', '{} is only run if reached';
    $x = 0;
    ok ("aab" ~~ / a* { $x++ } ab /), '{} does not block backtracking';
    is $x, 2, '{} is run multiple times when backtracking';

    $x = '';
    ok ("foo" ~~ / foo { $x = $x ~ 1 } | foo { $x = $x ~ 2 } /),
        "foo ~~ foo|foo";
    is $x, 1, "with no other constraints, first item is used";
    $x = '';
    ok ("foo" ~~ / fo* { $x = $x ~ 1 } | foo { $x = $x ~ 2 } /),
        "foo ~~ fo*|foo";
    is $x, 2, "longer literal prefix wins over seniority";
    $x = '';
    ok ("fooo" ~~ / fo* { $x = $x ~ 1 } | foo { $x = $x ~ 2 } /),
        "foo ~~ fo*|foo";
    is $x, 1, "longer length wins over prefix";
    $x = '';
    ok !("fooo" ~~ / [ fo*: { $x = $x ~ 1 } | foo { $x = $x ~ 2 } ] x /),
        "foo !~~ [fo*:|foo]x";
    is $x, '12', "will backtrack into shorter token";

    my grammar G5 {
        token a { foo }
        token b { foobar }
        token c { <a> | <b> }
        token d { <c> x }

        token e { x <e> x | y }

        token TOP { A <d> | E <e> }
    }

    ok G5.parse('Afoobarx'), 'LTM works even through subrules';
    ok G5.parse('Exxyxx'), 'recursivity does not crash LTM';

    my grammar G6 {
        token a   { fo* { $x = 1 } }
        token b   { foo { $x = 2 } }
        token TOP { <a> | <b> }
    }
    G6.parse("foo");
    is $x, 2, "prefix length testing works in subrules";
}

{
    is "\x63", "c", '\x works';
    is "Y\x63", "Yc", 'can put stuff before escapes';
    is "\x63Y", "cY", 'can put stuff after escapes';
    is "Y\x63Y", "YcY", 'can put stuff before and after escapes';
    is "\x[63,69]", "ci", 'bracketed \x works';
    is "\x4E03", "七", '\x with >2 characters works';
    is "七".chars, 1, "nana is one kanji";
    is "\\", "\x5C", 'can backslash backslashes';
    is "\"", "\x22", 'can backslash quotes';
    is '\'', "\x27", 'can backslash single quotes';
    is "\b", "\x08", '\b works';
    is "\a", "\x07", '\a works';
    # punt named forms for now
    is "\e", "\x1B", '\e works';
    is "\f", "\x0C", '\f works';
    is "\n", "\x0A", '\n works';
    is "\r", "\x0D", '\r works';
    is "\t", "\x09", '\t works';
    is "\o[61,63,65]", '135', '\o works (bracketed)';
    is "\o67", '7', '\o works (bare)';
    is "\0", "\x00", '\0 works';

    is "foo { 2 + 2 } bar", "foo 4 bar", "code interpolation works";
    my $cow = 'hi';
    is "foo $cow bar", "foo hi bar", '$-interpolation works';
    is "foo $cow.substr(0,1) bar", "foo h bar", 'methodcall interpolation works';
}

{
    my grammar G7 {
        proto token tok  {*}
        token tok:sym<+> { <sym> }
        token tok:foo    { <sym> }

        rule TOP { <tok> }
    }

    ok G7.parse('+'), "can parse :sym<> symbols";
    ok G7.parse('foo'), "can parse : symbols";
}

{
    sub t1(*@k, :$x, :y($)) { $x } #OK
    sub t2(*@k, :y($x), :x($)) { $x } #OK
    sub t3(*@k, :y(:$x)) { $x } #OK
    ok !t1.defined, "no arg, no value";
    ok !t1(12).defined, "positional is not enough";
    ok t1(x => 5) == 5, "can pass argument (fatarrow)";
    ok !t1("x" => 5), "quoted fatarrow doesn't work";
    ok !t1((x => 5)), "parenned fatarrow doesn't work";
    ok t1(:x(6)) == 6, "colonpair works";
    ok !t1(:y(7)).defined, "wrong name, no cigar";
    ok t2(y => 9) == 9, ":y(\$x) syntax picks out y as name";
    ok !t2(x => 10).defined, "x is NOT a usable name";
    ok t3(:x(11)) == 11, ":y(:\$x) works for both (1)";
    ok t3(:y(11)) == 11, ":y(:\$x) works for both (2)";
}

{
    ok "\n" ~~ /\n/, '\n in regex matches literal NL';
    ok !('\n' ~~ /\n/), '\n in regex does not match literal \n';
    ok '+' ~~ /\+/, '\+ in regex matches literal +';
    ok '\\' ~~ /\\/, '\\\\ in regex matches literal \\';
    ok 'a' ~~ /\x61/, '\x61 in regex matches literal a';

    ok 'xy' ~~ /x <?> y/, '<?> matches null string';

    my $a = 2;
    my $b ::= $a;
    $a = 5;
    is $b, 2, "ro binding loses original container";

    ok 'xxy' ~~ /x { $a = $/.pos } /, "can match with \$/ stuff";
    is $a, 1, '$/.pos is the right sort of thing';
    'xxy' ~~ /x { $a = ($¢ ~~ Cursor) }/;
    is $a, True, '$¢ isa Cursor';
}

{
    sub infix:<@>($x, $y, :$z) { $x, $y, $z }
    is (1 @ 2 :z(3)).join("|"), "1|2|3", "adverbs on infix ops work";
}

{
    my $x = 4;
    $x += 3;
    is $x, 7, "metaop += works";

    sub testx:sym«foo bar»() { 42 }
    is &testx:sym<<foo bar>>(), 42, "can use french quotes in declarations";

    sub foo(Str $x) { $x ~ $x }
    is foo("bar"), "barbar", "can parse type constraints";
}

rxtest /x.y/, "x.y", ("xay", "x y"), ("xy", "xaay");
rxtest /<!>/, '<!>', Nil, ("", "x");
rxtest /\s/, '\s', (" ", ("\n" => '\n'), ("\r" => '\r'), "\x3000"),
    ("x", "1", "+");
rxtest /\S/, '\S', ("x", "1", "+"),
    (" ", ("\n" => '\n'), ("\r" => '\r'), ("\x3000" => 'id space'));
rxtest /\w/, '\w', ("x", "1", "_", "\x4E00"), ("+", " ");
rxtest /<[ y ]>/, '<[ y ]>', ("y"), (" ", "x", "z");
rxtest /<[ i .. k ]>/, '<[ i .. k ]>', ("i", "j", "k"), ("h", "l");
rxtest /<[ \W a..z ]>/, '<[\W a..z]>', ("a", "z", "+"), ("\x4E00");

rxtest /a || b/, 'a || b', ("a", "b"), ("c", "");
rxtest /x [a || aa]: c/, 'x[a||b]:c', ("xac",), ("xaac",);

{
    my $obj ::= (class {
        method item() { "item" }
        method list() { "list" }
        method hash() { "hash" }
    }).new;

    is $($obj), "item", '$() calls item';
    is @($obj), "list", '@() calls list';
    is %($obj), "hash", '%() calls hash';

    is $$obj, "item", '$$ truncated context';
    is @$obj, "list", '@$ truncated context';
    is %$obj, "hash", '%$ truncated context';

    is "x$$obj", "xitem", '$$ interpolation';
    is "x@$obj", "xlist", '@$ interpolation';
    is "x%$obj", "xhash", '%$ interpolation';
}

ok "axy" ~~ / a <before x> \w y / , "before is zero-width";
ok "axy" ~~ / a <?before x> \w y / , "?before is zero-width";
ok "azy" ~~ / a <!before x> \w y / , "!before is zero-width";
ok !("azy" ~~ / a <?before x> \w y /) , "?before x needs x";
ok !("axy" ~~ / a <!before x> \w y /) , "!before x needs !x";

ok '{}' ~~ / \{ <.ws> \} /, 'ws matches between \W';

{
    rxtest /z .* y [ a :: x | . ]/, "z.*y[a::x|.]",
        ("zyax", "zyb", "zyaxya"), ("zya",);
    # no ::> until STD gets here...
    rxtest /z .* y [ a ::: x || . ]/, "z.*y[a:::x||.]",
        ("zyax", "zyb"), ("zya", "zyaxya");

    my grammar G7 {
        proto regex TOP {*}
        regex TOP:foo { a :: x }
        regex TOP:bar { . }
    }

    ok G7.parse("ax"), ":: does not block forward";
    ok G7.parse("b"), ":: does not affect other paths";
    ok !G7.parse("a"), "cannot backtrack past :: in proto ltm";
}

rxtest /y [ [ foo || bar ] | . ]: y/, "|| hides both sides from LTM",
    ("yky",), ("yfooy", "ybary");
rxtest /y [ [a||b] | c ]: y/, "|| exposes a declarative prefix",
    ("yay","yby","ycy"), Nil;

{
    # one CgOp bug manifested as a failure to compile this
    ok (/ <?before x>: <ws>: /).defined, "unnamed regression";
    my $ma = ("ab29x" ~~ /\d+/);
    ok $ma.defined, "match is defined";
    ok $ma.WHAT === Match, "matches are Match";
    is $ma.from, 2, '$ma.from works';
    is $ma.to, 4, '$ma.to works';
    is $ma.Str, '29', '$ma.Str works';
}

{
    use MONKEY_TYPING;
    my class Foo {
        method foo { 1 }
    }
    is Foo.foo, 2, "augments run early";

    augment class Foo {
        method foo { 2 }
    }
}

{
    my $ma = (grammar {
        token TOP { <foo> <bar> }
        token foo { \d+ }
        token bar { \D+ }
    }).parse("123abc");
    ok $ma<foo> ~~ Match, "<foo> sub is a Match";
    is $ma<foo>, "123", "<foo> sub got 123";
    ok $ma<bar> ~~ Match, "<bar> sub is a Match";
    is $ma<bar>, "abc", "<bar> sub got 123";
    ok !$ma<quux>, "no <quux> sub";
    my $mb = (grammar {
        token TOP { <foo> <foo> }
        token foo { \D+ | \d+ }
    }).parse("def456");
    ok $mb.defined, "grammar b matched";
    ok $mb<foo> ~~ List, "<foo> sub isa List";
    is +$mb<foo>, 2, "2 matches";
    ok $mb<foo>[0] ~~ Match, "<foo>[0] sub isa Match";
    is $mb<foo>[0], "def", "<foo>[0] got def";
    is $mb<foo>[1], "456", "<foo>[1] got 456";
    my $mc = (grammar {
        token TOP { <foo>+ }
        token foo { \D+ | \d+ }
    }).parse("def");
    ok $mc<foo> ~~ List, "<foo>+ makes a list";
    is +$mc<foo>, 1, "despite only one element";
    my $md = (grammar {
        token TOP { \D+ <foo> \D+ }
        token foo { \d+ <bar> \d+ }
        token bar { \D+ }
    }).parse("a1b2c");
    is $md<foo><bar>, "b", "<foo><bar> works";
    my $mf = (grammar {
        proto token TOP {*}
        token TOP:x { <foo> }
        token foo { \w+ }
    }).parse("abc");
    is $mf<foo>, "abc", "protoregex captures work";
    my $me = (grammar {
        token TOP { <tok>+ }
        proto token tok {*}
        token tok:num { <sign>? <digit>+ }
        token tok:ws { \s+ }
        token sign { \+ | \- }
        token digit { \d | _ }
    }).parse("2 -34 5");
    ok $me<tok> ~~ List, "a list of tokens";
    is +$me<tok>, 5, "5 of them";
    is +$me<tok>[0]<sign>, 0, "first no sign";
    is +$me<tok>[2]<sign>, 1, "third sign";
    is +$me<tok>[4]<sign>, 0, "fifth no sign";
    is $me<tok>[2], '-34', "3rd token '-34'";
}

{
    rxtest / . << . /, ".<<.", (" x",), ("x ","  ","xx");
    rxtest / . << /, ".<<", Nil, ("x", " ");
    rxtest / << . /, "<<.", ("x",), (" ",);
    rxtest / << /, "<<", Nil, ("",);

    rxtest / . >> . /, ".>>.", ("x ",), (" x","  ","xx");
    rxtest / . >> /, ".>>", ("x",), (" ",);
    rxtest / >> . /, ">>.", Nil, ("x"," ");
    rxtest / >> /, ">>", Nil, ("",);

    rxtest / . « . /, ".«.", (" x",), ("x ","  ","xx");
    rxtest / . « /, ".«", Nil, ("x", " ");
    rxtest / « . /, "«.", ("x",), (" ",);
    rxtest / « /, "«", Nil, ("",);

    rxtest / . » . /, ".».", ("x ",), (" x","  ","xx");
    rxtest / . » /, ".»", ("x",), (" ",);
    rxtest / » . /, "».", Nil, ("x"," ");
    rxtest / » /, "»", Nil, ("",);

    rxtest / . ^ . /, ".^.", Nil, ("x",);
    rxtest / . ^ /, ".^", Nil, ("x",);
    rxtest / ^ . /, "^.", ("x",), Nil;
    rxtest / ^ /, "^", ("",), Nil;

    rxtest / . $ . /, '.$.', Nil, ("x",);
    rxtest / . $ /, '.$', ("x",), Nil;
    rxtest / $ . /, '$.', Nil, ("x",);
    rxtest / $ /, '$', ("",), Nil;

    rxtest / . ^^ . /, '.^^.', ("\nx","\n\n"), ("x\n","xx");
    rxtest / . ^^ /, '.^^', Nil, ("x","\n");
    rxtest / ^^ . /, '^^.', ("x","\n"), Nil;
    rxtest / ^^ /, '^^', ("",), Nil;

    rxtest / . $$ . /, '.$$.', ("x\n", "\n\n"), ("\nx","xx");
    rxtest / . $$ /, '.$$', ("x",), ("\n",);
    rxtest / $$ . /, '$$.', ("\n",), ("x",);
    rxtest / $$ /, '$$', ("",), Nil;
}

{
    ok "foo" ~~ / :my $gothere = 1; foo /, "can embed :my in regexes";
    ok $gothere, ":my code is run";
}

{
    sub flow-ok($fn, $flw, $msg) {
        my $log = '';
        $fn(-> $i { $log ~= $i });
        is $log, $flw, $msg;
    }

    flow-ok -> &l { my $i = 0; while $i < 2 { $i++; l(1); next; l(2) } }, '11',
        "next skips second half of while loop";
    flow-ok -> &l { my $i = 0; while $i < 2 { $i++; l(1); last; l(2) } }, '1',
        "last skips everything";
    flow-ok -> &l { my $i = 0; while True { l($i++); last if $i == 3 } }, '012',
        "last can leave inf loop";
    flow-ok -> &l { my $i = 3; while $i == 3 { l($i--); redo if $i } }, '321',
        "redo reenters loops";
    sub foo { return 2; }
    is foo(), 2, "return values work";
    my $cont = False;
    sub foo3 { return 2; $cont = True; }
    foo3;
    ok !$cont, "return exits function";
}

{
    rxtest /^ x**2..4 $/, 'x**2..4', ('xx','xxx','xxxx'), ('x','xxxxx');
    rxtest /^ x**2..* $/, 'x**2..*', ('xx','xxx','xxxx'), ('x',);
    rxtest /^ [x**2] $/, 'x**2', ('xx',), ('x','xxx');
    rxtest /^ [x**y] $/, 'x**y', ('x','xyx','xyxyx'), ('','xy','yx');
}

{
    my $x;
    (class { method foo() {
        $x = 1;
    } }).foo;
    ok $x, "changes made in the protolexpad are visible at runtime";
}

{
    my $x;
    my $unclonable-sub = (class { method foo() { sub () { $x } } }).foo;
    $x = 42;
    ok $unclonable-sub() == 42, "mainlines are not cloned";
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
    role Foo6596[$x] { # XXX std bug, need to use OUR::Foo[$x]
        method rfoo { $x }
    }

    my role Quux {
        method rquux { "quux" }
    }

    my class Bar { }

    is (Bar but Quux).rquux, "quux", "can bind roles to classes";
    is (Bar but OUR::Foo6596["hi"]).rfoo, "hi", "can bind parametric roles to classes";
}

ok "abc" ~~ / :dba("foo") abc /, ":dba doesn't affect parsing";

{
    my $log = '';
    my grammar B {
        token a { { $log ~= 'A' } }
        token b { { $log ~= 'B' } }
        token c { { $log ~= 'C' } }
    }
    my grammar A {
        token a { { $log ~= 'a' } }
        token b { { $log ~= 'b' } }
        token c { { $log ~= 'c' } }
        token TOP { x <.a> [ :lang(B) <.b> ] <.c> x }
    }
    A.parse("xx");
    is $log, 'aBc', ':lang has the expected effect';
}

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
