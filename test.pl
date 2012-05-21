# vim: ft=perl6

use Test;

plan 1006;

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
ok (?1) eq "True", "True strings to True";
ok (?0) eq "False", "False strings to False";

ok False ~~ Bool && !False, "False is the right constant";
ok True ~~ Bool && True, "True is the right constant";
ok Bool::False ~~ Bool && !Bool::False, "Bool::False is the right constant";
ok Bool::True ~~ Bool && Bool::True, "Bool::True is the right constant";

ok Any.gist eq "Any()", "Any.gist is Any()";

ok (2.WHAT === 3.WHAT), "different objects get the same WHAT";
ok !(2.WHAT.defined), "WHATs are undefined type objects";
ok (sub foo() {}).WHAT.gist eq 'Sub()', 'WHAT of a Sub is Sub()';
ok "Foo".WHAT === Str, 'WHAT of a Str *is* Str';

ok "Foo".HOW.WHAT.gist eq 'ClassHOW()', 'anything.HOW is a ClassHOW';
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
    ok (?0) ~~ (?1), "Bool.ACCEPTS ignores its argument";
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
    ok cat(1, (2, 3), ((4, 5), ((), 7))) eq "123457", "parcels flatten";
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
    our $kluw = 99; #OK
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
    ok $whatever.WHAT.gist eq 'Whatever()', "can call methods on a specific Whatever";
    my $wwhat = *.WHAT;
    ok !($wwhat.^isa(Whatever)), "method calls against * curry, though";

    ok (* + 5)(2) == 7, "can curry simple binops";
    is ((*) eq "foo"), Bool::False, "parens defeat Whatever directly";
    ok (1 + 2 * *)(5) == 11, "nested Whatever works";
    ok (2 * (1 + *))(5) == 12, "parens for grouping do not kill WhateverCode";
    ok (* + *)(5,12) == 17, "multiple *, multiple args";
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

    augment class Any { trusts GLOBAL }
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
    sub postcircumfix:<[ ]>($a, $b) { $a ~ "|" ~ $b }
    is 1[2], "1|2", "can call postcircumfix [ ]";
}

{
    sub postcircumfix:<{ }>($a, $b) { $a ~ "|" ~ $b }
    is 1{2}, "1|2", 'can call postcircumfix { }';
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
    is :foo.value, Bool::True, ":foo is true";
    is :!foo.value, Bool::False, ":!foo is false";
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

    our $x6474;
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
        token a   { fo* { $x6474 = 1 } }
        token b   { foo { $x6474 = 2 } }
        token TOP { <a> | <b> }
    }
    G6.parse("foo");
    is $x6474, 2, "prefix length testing works in subrules";
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
    is &testx:sym<foo bar>(), 42, "can use french quotes in declarations";

    sub foo(Str $x) { $x ~ $x }
    is foo("bar"), "barbar", "can parse type constraints";
}

rxtest /x.y/, "x.y", ("xay", "x y"), ("xy", "xaay");
rxtest /<!>/, '<!>', (), ("", "x");
rxtest /\s/, '\s', (" ", ("\n" => '\n'), ("\r" => '\r'), "\x3000"),
    ("x", "1", "+");
rxtest /\S/, '\S', ("x", "1", "+"),
    (" ", ("\n" => '\n'), ("\r" => '\r'), ("\x3000" => 'id space'));
rxtest /\w/, '\w', ("x", "1", "_", "\x4E00"), ("+", " ");
rxtest /<[ y ]>/, '<[ y ]>', ("y",), (" ", "x", "z");
rxtest /<[ i .. k ]>/, '<[ i .. k ]>', ("i", "j", "k"), ("h", "l");
rxtest /<[ \W a..z ]>/, '<[\W a..z]>', ("a", "z", "+"), ("\x4E00",);

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
    is "x@$obj.Str()", "xlist", '@$ interpolation';
    is "x%$obj.Str()", "xhash", '%$ interpolation';
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

rxtest /y [ [ foo || bar ] | . ]: y/, "|| hides right side from LTM",
    ("yky","yfooy"), ("ybary",);
rxtest /y [ [a||b] | c ]: y/, "|| exposes a declarative prefix for left only",
    ("yay","ycy"), ("yby",);

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
    is ?($me<tok>[0]<sign>), False, "first no sign";
    is ?($me<tok>[2]<sign>), True, "third sign";
    is ?($me<tok>[4]<sign>), False, "fifth no sign";
    is $me<tok>[2], '-34', "3rd token '-34'";
}

{
    rxtest / . << . /, ".<<.", (" x",), ("x ","  ","xx");
    rxtest / . << /, ".<<", (), ("x", " ");
    rxtest / << . /, "<<.", ("x",), (" ",);
    rxtest / << /, "<<", (), ("",);

    rxtest / . >> . /, ".>>.", ("x ",), (" x","  ","xx");
    rxtest / . >> /, ".>>", ("x",), (" ",);
    rxtest / >> . /, ">>.", (), ("x"," ");
    rxtest / >> /, ">>", (), ("",);

    rxtest / . « . /, ".«.", (" x",), ("x ","  ","xx");
    rxtest / . « /, ".«", (), ("x", " ");
    rxtest / « . /, "«.", ("x",), (" ",);
    rxtest / « /, "«", (), ("",);

    rxtest / . » . /, ".».", ("x ",), (" x","  ","xx");
    rxtest / . » /, ".»", ("x",), (" ",);
    rxtest / » . /, "».", (), ("x"," ");
    rxtest / » /, "»", (), ("",);

    rxtest / . ^ . /, ".^.", (), ("x",);
    rxtest / . ^ /, ".^", (), ("x",);
    rxtest / ^ . /, "^.", ("x",), ();
    rxtest / ^ /, "^", ("",), ();

    rxtest / . $ . /, '.$.', (), ("x",);
    rxtest / . $ /, '.$', ("x",), ();
    rxtest / $ . /, '$.', (), ("x",);
    rxtest / $ /, '$', ("",), ();

    rxtest / . ^^ . /, '.^^.', ("\nx","\n\n"), ("x\n","xx");
    rxtest / . ^^ /, '.^^', (), ("x","\n");
    rxtest / ^^ . /, '^^.', ("x","\n"), ();
    rxtest / ^^ /, '^^', ("",), ();

    rxtest / . $$ . /, '.$$.', ("x\n", "\n\n"), ("\nx","xx");
    rxtest / . $$ /, '.$$', ("x",), ("\n",);
    rxtest / $$ . /, '$$.', ("\n",), ("x",);
    rxtest / $$ /, '$$', ("",), ();
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
    rxtest /^ [x+%y] $/, 'x**y', ('x','xyx','xyxyx'), ('','xy','yx');
}

{
    our $x8484;
    (class { method foo() {
        $x8484 = 1;
    } }).foo;
    ok $x8484, "changes made in the protolexpad are visible at runtime";
}

{
    our $x1903;
    my $unclonable-sub = (class { method foo() { sub () { $x1903 } } }).foo;
    $x1903 = 42;
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
    ok Foo.gist eq 'Foo()', "lexical lookup of our-class works";
    ok OUR::Foo.gist eq 'Foo()', "also visible in ourpad";
    ok GLOBAL::Foo.gist eq 'Foo()', "also visible globally";
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
    our $log9434 = '';
    my grammar B {
        token a { { $log9434 ~= 'A' } }
        token b { { $log9434 ~= 'B' } }
        token c { { $log9434 ~= 'C' } }
    }
    my grammar A {
        token a { { $log9434 ~= 'a' } }
        token b { { $log9434 ~= 'b' } }
        token c { { $log9434 ~= 'c' } }
        token TOP { x <.a> [ :lang(B) <.b> ] <.c> x }
    }
    A.parse("xx");
    is $log9434, 'aBc', ':lang has the expected effect';
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
    is (not False), True, '&not works';
    is (defined 5), True, '&defined works';
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
    is (%(%bar.invert)<9>), "c", "Hash.invert works";
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

ok 'cow' le 'sow', 'cow le sow';
ok !('sow' le 'cow'), 'sow !le cow';
ok 'row' lt 'tow', 'row lt tow';
ok 'how' gt 'bow', 'how gt bow';
ok 'yow' ge 'yow', 'yow ge yow';
is join("|", sort <c f d z a>), 'a|c|d|f|z', '&sort works';
is join("|", <a3 b2 c1 d0>.sort({ substr($^a,1) leg substr($^b,1) })),
    'd0|c1|b2|a3', '.sort with callback works';

is ("yayay" ~~ /y\w*?y/), "yay", "minimal matching works";
is ("yayay" ~~ /y+?%a/), "y", "minimal matching works with **";

is +[ 2 ], 1, "array construction w/ one argument";
is +[ ], 0, "array construction w/ no arguments";
is +[ 3, 4 ], 2, "array construction w/ two";
is +[ $( 3, 4 ) ], 1, "array construction w/ scalar argument";

{
    sub bar { $*x + $*x }
    sub foo($*x) { bar }
    is foo(12), 24, "*-twigilled arguments work";
}

is { a => 1 }.<a>, 1, "hash constructors work (1)";
is { "a" => 1 }.<a>, 1, "hash constructors work w/ quotes";
is { :a(1) }.<a>, 1, "hash constructors work w/ colons";
is { a => 1, b => 2 }.<b>, 2, "hash constructors work w/ lists";
ok { } ~~ Hash, "hash constructors work w/ nothing";

ok !('xy' ~~ /x <?{ False }> y/), '<{False}> blocks a match';
ok 'xy' ~~ /x <?{ True }> y/, '<{True}> does not affect it';
ok (1 < 3 > 2), "CHAIN works with dissimilar ops";

{
    my $b = "oo";
    is ("foox" ~~ /f$b/), "foo", '$x matches contents in a regex';
}

{
    sub f1(:$x) { $x }
    is f1(|("x" => 2)), 2, "can flatten pairs";
    is f1(|{"x" => 2}), 2, "can flatten hashes";
    sub f2($x,$) { $x }
    is f2(|[1,2]), 1, "can flatten lists";
    is f2(|(1,2)), 1, "can flatten parcels";
}

rxtest / <alpha> / , '<alpha>', ('a', 'A', "\x4E00"), ("+", "1", " ");

{
    my $m = "" ~~ / $<foo> = { 2 + 2 } $<bar> = {"x"} $<bar> = {"y"} /;
    is $m<foo>, 4, "value aliasing works (sing)";
    is $m<bar>, "x y", "value aliasing works (plur)";

    $m = "fo" ~~ / (.) (.) /;
    is $m[0], "f", "numbered captures work";
    is $m[1], "o", "capture auto-numbering works";

    $m = "foo" ~~ / (.) ( (.) (.) ) /;
    is $m[1], "oo", "outer capture sees inner";
    is $m[1][1], "o", "nested numeric captures work";

    $m = "def" ~~ /<a=.alpha> $<moo> = [ <b=.alpha> <c=.alpha> ]/;
    is $m<a>, "d", "aliasing works";
    is $m<c>, "f", "aliased [] transparent to captures";
    is $m<moo>, "ef", "aliased [] captures string";
    ok !$m<moo><b>, "no spurious nested captures";

    my $save;
    "()" ~~ / '(' ~ ')' { $save = $*GOAL } /;
    is $save, ')', 'Setting $*GOAL works';
}

ok 1 !== 2, "infix_prefix_meta_operator:<!> works (T)";
ok !(1 !== 1), "infix_prefix_meta_operator:<!> works (F)";

{
    my $m = "ab" ~~ / (.) <alpha> /;
    is (@$m)[0], "a", "Match.list returns positional captures";
    is (%$m)<alpha>, "b", "Match.hash returns named";
    is ((-> $p, :$alpha { $p, $alpha })(|$m)).join("|"), "a|b", "Match.Capture returns both";
    my @arr = "abc" ~~ / (.) (.) (.) /;
    is @arr.join("|"), "a|b|c", "Regex.ACCEPTS in list context returns captures";
    $m = "" ~~ / <O( foo => 2 )> /;
    is $m<O><foo>, 2, "<O> is functional";

    $m = (grammar {
        proto token TOP {*}
        token TOP:foo { <sym> }
    }).parse("foo");
    is $m<sym>, "foo", '$<sym> is functional';

    ok !(try die "foo").defined, "try of an error is undef";
    is $!, "foo", 'the error goes into $!';

    {
        my @*foo = 1, 2, 3;
        {
            temp @*foo;
            push @*foo, 4;
            is +@*foo, 4, '@*foo has 4 elements in temp scope';
        }
        is +@*foo, 3, '@*foo has 3 elements again after temp';
    }

    my @ar = [1, 2, 3];
    is +@ar, 1, "array constructors are singular";
    my $i = 0;
    $i++ until $i == 10;
    is $i, 10, "until loops functional";

    ok "foo" !~~ / f <.suppose { die }> /, ".suppose works (F)";
    ok "fox" ~~ / f <.suppose o> x /, ".suppose works (T)";

    ok "abcabc" ~~ /^ (\w+) $0 $/, '$/ in variable refs functional';
    ok "abcabc" ~~ /^ (\w+) "$0" $/, '$/ in substrings functional';

    (grammar {
        method moo($cap) { is $cap<cap>, "hi", '$/ from subrule args works'; @( self, ) }
        token TOP { $<cap>={"hi"} <moo($/)> }
    }).parse("");

    ok (grammar {
        method moo() { self }
        regex TOP { <.moo> }
    }).parse(""), "simply returning self from a regex works";

    rxtest / <after ':+'> X / , '<after ":+">X', (':+X', 'a:+X'), (':X','+:X');
    rxtest / <after \s> X / , '<after \s>X', (' X',), ('xX','X');
    rxtest / <after ab> X / , '<after ab>X', ('abX',), ('baX',);
}

{
    my ($x) = (1, 2);
    is $x, 1, "list assign takes first";
    ($x, my $y) = (1, 2);
    is "$x|$y", "1|2", "list assign takes both";
    ($x, $y) = 1;
    is $x, 1, "list assign takes what it can";
    ok !$y.defined, "rest of list gets undef";
    ($x, my @z, my @w) = (1, 2, 3);
    is $x, 1, "scalar gets first";
    is @z.join("|"), "2|3", "list gets rest";
    is +@w, 0, "second list gets none";
}

{
    sub infix:<=>($x, $y) { $x ~ "|" ~ $y }
    is (1 = 2), '1|2', 'can override infix:<=> in lexical scope';
}

{
    my class Regex { }
    ok "x" ~~ /x/, "Regex shadowing doesn't cause problems";
}

ok "\x2FFF" ~~ /<-[ x ]>/, "Negated char classes match unassigned characters";
ok "x:" ~~ /. >> ./, "Punctuation ends words";

{
    my class A { method foo(:$x) { $x * 2 } }
    my class B is A { method foo() { nextwith( x => 5 ) } }
    is B.foo, 10, "nextwith works";
}

{
    our role R6025[$x] {
        method foo() { $x }
    }

    ok ((Any but OUR::R6025[True]).foo.^isa(Bool)),
        "parameterized roles can store non-strings";
}

{
    our role Stop4717[$a] {
        token foo { $a }
    }

    grammar X {
        token TOP { [ <foo> | foo ]: x }
    }

    ok (X but OUR::Stop4717["foobar"]).parse("foobarx"),
        "LTM works through parameterized role variables";
}

{
    my $M;
    my $t;
    $M = ("a()" ~~ / <alpha> '(' ~ ')' { $t = $<alpha>.Str } /);
    is $t, "a", "Inside of ~ can see captures";
    $M = ("(a)" ~~ / '(' ~ ')' <alpha> /);
    is $M<alpha>, "a", "Captures can escape from ~";

    my $died = 1;
    try { $*NONEX = 1; $died = 0; }
    ok $died, "Assignment to non-existing dynvar fails";

    is "foo".substr(5,2), "", "substr starting off end works";
    is "foo".substr(1,10), "oo", "substr ending off end works";

    rxtest /:i "abc"/, ':i "abc"',
        ("abc","aBc","ABC"),("cba","ab");

    my grammar G {
        proto token TOP {*}
        token TOP:foo { :i <sym> }
    }

    ok G.parse("fOo"), ":i <sym> works";

    my %h; %h<used>++;
    is %h<used>, 1, "autoviv works with ++";

    is ((anon method retme ($x:) { $x })(53)), 53, "method definitions with explicit invocants work";
}

{
    my class A { method Numeric { 42 } }
    is A.new + 23, 65, '+ calls user-written .Numeric';
}

{
    "abc" ~~ /. (.) ./;
    is $0, 'b', 'Regex matches set $/';
}

{
    "abc" ~~ /de(.)/;
    ok !defined($/), 'Failed regex matches clear $/';
}

{
    my @foo = 1, 2, 3, 4, 5;

    is @foo[2,3].join('|'), '3|4', 'slicing works';
    is @foo[[1,4]].join('|'), 3, 'items do not slice';
    ok !defined(@foo[4,5,6][1]), 'slices off end work';
    @foo[1,2,3] = 5,6,7;
    is @foo.join('|'), '1|5|6|7|5', 'can assign to slices';
    @foo[4,5,6] = 1,2,3;
    is @foo.join('|'), '1|5|6|7|1|2|3', 'can assign to slices off end';

    my %quux;
    %quux<a b c> = (1, 2, 3);
    is %quux<b>, 2, 'can assign to hash slices';
    is %quux<c b>.join('|'), '3|2', 'can read from hash slices';

    my @bar = 1, 2, 3, 4, 5;
    is @bar[{ $^x / 2 }], 3, 'code indexes work';
    is @bar[*-1], 5, 'WhateverCode indexes work';
}

{
    my @arr;
    my $ix = -1;
    ok !(defined @arr[$ix]), "can index before arrays to get undef";
}

{
    is ("foo bar baz".split(/\s/).join('|')), 'foo|bar|baz', 'basic split';
    is ("foo bar baz".split(' ').join('|')), 'foo|bar|baz', 'split with string';
    is ("foo bar baz".split(' ', 2).join('|')), 'foo|bar baz',
        'split with a limit';
    is ("  foo bar".split(' ').join('|')), '||foo|bar',
        'split with leading empty fields';
    is ("foo bar  ".split(' ').join('|')), 'foo|bar||',
        'split with trailing empty fields';
    try { "foo bar".split };
    ok $!, 'split requires an argument';
    is ("ax+by*cz".split(/\W/, :all).join('|')), 'ax|+|by|*|cz',
        'split :all';

    is "hello world".index('l'), 2, ".index";
    is "hello world".index('l',5), 9, ".index with restart point";
    ok (!defined("hello world".index('x'))), ".index off end";
    is "hello world".rindex('l'), 9, ".rindex";
    is "hello world".rindex('l', 6), 3, ".rindex with restart point";
    ok (!defined("hello world".rindex('x'))), ".rindex off end";

    is ("abc".comb.join('|')), 'a|b|c', 'comb with default matcher';
    is ("abc".comb(/./, 2).join('|')), 'a|b', 'comb with limit';
    is ("A1 B2 C3".comb(/(\w)(\d)/, :match).[2].[1]), 3, 'comb :match';
}

{
    my $a = 3; $a &&= 4; is $a, 4, '&&= works (T)';
    my $b = 0; $b &&= 4; is $b, 0, '&&= works (F)';
    my $c = 3; $c ||= 4; is $c, 3, '||= works (T)';
    my $d = 0; $d ||= 4; is $d, 4, '||= works (F)';
    my $e = 0; $e andthen= 4; is $e, 4, 'andthen= works (D)';
    my $f = Any; $f andthen= 4; is $f.gist, Any.gist, 'andthen= works (U)';
    my $g = 0; $g //= 4; is $g, 0, '//= works (D)';
    my $h = Any; $h //= 4; is $h, 4, '//= works (U)';

    is 2.&not, False, '.& notation works';

    my class X1 {
        submethod foo { 1 }
    }

    my class X2 is X1 {
    }

    is X1.foo, 1, "can call submethods";
    my $i; try { X2.foo; $i = True }
    ok !$i, "submethods are not inherited";
}

{
    my class X1 {
        has $.a = 123;
        has $.b;
    }

    my $x = X1.new;
    ok !defined($x.b), "without initializer, attr is undefined";
    is $x.a, 123, "initializers work";
    $x.a = 456;
    is $x.a, 456, "initialized attrs can still be reset";
}

{
    my $called = 1;
    sub called {
        $called++;
        "foo";
    }
    if called() -> $a {
        is $a,"foo","the condition in an if is passed to the block as an argument";
    }
    is $called,2,"the if condition only gets called once";

    my $false_branch = 0;
    if 0 -> $a {
       1+$a;
       $false_branch = 1;
    }
    is $false_branch,0,"a branch of an if with a false condition doesn't get called";
}

{
    our $str8646;
    INIT $str8646 = '';
    $str8646 ~= 1;
    INIT $str8646 ~= 2;
    $str8646 ~= 3;
    INIT $str8646 ~= 4;
    is $str8646, '2413', 'INIT blocks run in correct order';
}

{
    my class X3 {
        has $.a;
    }
    my $x = X3.new(a => 5);
    is $x.a, 5, 'Attribute values can be passed in constructors';

    sub foo($/) { $<a> }
    is foo({ a => 5 }), 5, 'Can bind $/ in signature';

    my class X4 {
        method foo { 13 }
        method !bar { 17 }
        method sam($x) { self!"$x"() }
    }

    is X4."foo"(), 13, 'indirect method calls work'; #OK
    is X4."{ "fo" ~ "o" }"(), 13, 'indirect calls work with interpolation';
    is X4.sam("bar"), 17, "indirect private method calls work";
}

{
    "foo" ~~ /oo/;
    ok !$/.ast.defined, '.ast defaults to undefined';
    "foo" ~~ /oo { make 15 }/;
    is $/.ast, 15, 'make can change .ast';
    "foo" ~~ /{make 30} oo/;
    is $/.ast, 30, 'make works in the middle';

    my grammar X5 {
        proto token TOP {*}
        token TOP:x { foo { make 45 } }
    }
    is X5.parse("foo").ast, 45, 'make works in multiregexes';
}

{
    my grammar X6 {
        token a { <?> }
        proto token b {*}
        token b:x { <?> }
        proto token c {*}
        token c:x { <?> }
    }

    my class A6 {
        method a($/) { make 1 }
        method b($/) { make 2 }
        method c:x ($/) { make 3 }
    }

    is X6.parse("", :actions(A6), :rule<a>).ast, 1,
        'action methods work (simple rule)';
    #is X6.parse("", :actions(A6), :rule<b>).ast, 2,
    #   'action methods work (proto rule)';
    is X6.parse("", :actions(A6), :rule<c>).ast, 3,
        'action methods work (candidate rule)';
}

{
    my @foo = 1; #OK
    my $tgt; #OK
    my %into;
    $tgt = %into<foo> = True;
    ok %into<foo>, "2011-01-13 list assignment parsefail";
}

{
    is +"12" - 5, 7, "Str.Numeric works";
    is 6 +& 3, 2, "+& works";
    is 6 +^ 3, 5, "+^ works";
    is 6 +| 3, 7, "+| works";
    is 6 +< 3, 48, "+< works";
    is 6 +> 1, 3, "+> works";
    is +^6, -7, "+^ works";
    is ord('0'), 48, "ord works";
    is chr(65), 'A', "chr works";
}

{
    is [ 1..5 ], "1 2 3 4 5", "Ranges work in list context";
    is [ 1 ..^ 5 ], "1 2 3 4", "Tail exclusion works";
    is [ 1 ^.. 5 ], "2 3 4 5", "Head exclusion works";
    is [ 1 ^..^ 5 ], "2 3 4", "Dual exclusion works";
    is [ ^5 ], "0 1 2 3 4", "Shorthand form works";
    is ((5 .. *)[3]), 8, "Infinite ranges can be iterated";
    ok 3 ~~ 1..4, "Range checking works (+)";
    ok 5 !~~ 1..4, "Range checking works (-)";
}

{
    my $i = 0;
    1 < ($i++; 2) < 3;
    is $i, 1, "Chained comparisons only evaluate terms once";

    my $foo = [5];
    for $foo { .shift }
    is +$foo, 0, ".method works";

    my $x = 5;
    $x ~~ .++;
    is $x, 6, "~~ topicalization works";

    my $y;
    given 12 { $y = $_ }
    is $y, 12, "prefix given works";

    $y = $_ given 24;
    is $y, 24, "postfix given works";

    my $z = '';
    $z ~= $_ for 1, 2, 3;
    is $z, '123', "postfix for works";

    my $k = '';
    given 12 {
        $k ~= 1 when 12;
        $k ~= 2 when * > 5;
        $k ~= 3 when * <= 5;
    }
    is $k, '12', "postfix when works";

    $k = '';
    given 12 {
        when 9 { $k ~= 1 }
        when * > 6 { $k ~= 2 }
        when * > 3 { $k ~= 3 }
    }
    is $k, '2', "normal when works";

    given my $g { #OK
        $_ = 'abc';
        s/b/d/;
        is $_, 'adc', 'simple s/// works';
        is $/, 'b', 's/// sets $/';
        $k = 'bac';
        $k ~~ s/c/g/;
        is $k, 'bag', '~~ s/// works';
        $_ = 'abc';
        s/(\w)/$0$0/;
        is $_, 'aabc', 's/// can refer to $/';
        $_ = 'abc';
        ok ?(s/b/x/), 's/// is true if replacing';
        $_ = 'abc';
        ok !(s/d/x/), 's/// is false if not replacing';
        is $_, 'abc', '... and target unchanged';
        $_ = 'abc';
        s!a!xx!;
        is $_, 'xxbc', 's/// with alternate delims works';
        $_ = 'abc123';
        s{b} = 'g' ~ 'k';
        is $_, 'agkc123', 's{} = works';
        $_ = 'abc123';
        s{\D+} = $/ ~ $/;
        is $_, 'abcabc123', 's{} = can refer to $/';
        $_ = 'abc123';
        s{\d+} *= 2;
        is $_, 'abc246', 'metaoperator s{} works';
    }
}

{
    my $a = 0;
    {
        A: while True {
            $a++; while True { last A }; $a++;
            last;
        }
    }
    is $a, 1, "last with label works";
    sub funlp($fn) {
        A: while True {
            $fn(A)
        }
    }
    my $b = 0;
    funlp(-> $o {
        $b++; funlp(-> $i { last $o }); $b++; #OK
        last;
    });
    is $b, 1, "last with label object is not fooled by names";
    my $c = 0;
    funlp(-> $o { #OK
        $c++; funlp(-> $i { last "A" }); $c++; #OK
        last;
    });
    is $c, 2, "last with name picks innermost";

    sub loopy () { True }
    ok loopy, "can call functions starting with 'loop'";

    my @a = "abc" ~~ /abc/;
    is +@a, 0, "capture-less matches return no items in list context";
    @a = "abc" ~~ /(a)(b)(c)/;
    is +@a, 3, "capturing matches return catures in list context";
}

{
    is ~[4,0 Z+ 2,0 Z+ 1,0], "7 0", "Z+ works";
    is ~[1,2 X+ 3,4 X+ 5,6], "9 10 10 11 10 11 11 12", "X+ works";
    is ~[4,0 Z  2,0 Z  1,0], "4 2 1 0 0 0", "Z works";
    is ~[1,2 X  3,4 X  5,6], "1 3 5 1 3 6 1 4 5 1 4 6 2 3 5 2 3 6 2 4 5 2 4 6", "X works";

    ok "{1}" ~~ Str, "string interpolation stringifies";
    is q:to[A] , " x\n", "q:to strips equal whitespace";
     x
    A
}

{
    my class X1 {
        my @foo = 4, 5, 6;
        method a() { @foo }
        method b() { [ 1, 2, 3 ] }
        method test() {
            is +[ @.b ], 3, '@.foo syntax listifies';
            is +[ $.a ], 1, '$.foo syntax itemifies';
        }
    }
    X1.test;

    constant $foo = 1, 2, 3;
    constant @bar = 4;
    constant %baz = a => 3, c => 6;

    is +[ $foo ], 1, '$-constants itemize';
    is +@bar, 1, '@-constants listize';
    is %baz<c>, 6, '%-constants hashize';

    my class X2 {
        method test($x) { $x * $x }
    }

    my class X3 is X2 {
        method test($) {
            is callsame(), 25, "callsame() works";
            is callwith(self, 6), 36, "callwith() works";
        }
    }

    X3.test(5);
}

{
    "f" ~~ /<alpha>?/;
    ok $<alpha>.^isa(Match), "? returns match directly on success";

    my $r = &return;
    sub dfoo()  { return; }
    sub dbar()  { return 5; }
    sub dquux() { return 5, 10; }
    sub foo()   { $r(); }
    sub bar()   { $r(5); }
    sub quux()  { $r(5, 10); }
    is +[ foo ], 0, "can return no values (i)";
    is +[ bar ], 1, "can return one value (i)";
    ok (bar() == 5), "one value isn't wrapped (i)";
    is +[ quux ], 2, "can return two values (i)";
    is +[ dfoo ], 0, "can return no values";
    is +[ dbar ], 1, "can return one value";
    ok (dbar() == 5), "one value isn't wrapped";
    is +[ dquux ], 2, "can return two values";
}

{
    constant %bar = (:a, :b);
    is +[ %bar ], 2, "constant hashes flatten";
}

{
    package Foo7426 { }
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
    my $log = '';
    my $ret = '';
    my $nada = sub (|) { Any };
    my $var := Proxy.new(FETCH => -> $ { $log ~= "F"; $ret },
        STORE => -> $, $x { $log ~= $x });

    $ret = 5; $log = "";
    my $a = $var;
    is $a, 5, "fetches work";
    is $log, "F", "fetch called once only";

    $ret = 3; $log = "";
    $var = 9;
    is $log, "9", "stores work";

    $log = "";
    $a = Proxy.new(BIND => { $log ~= "B" }, FETCH => $nada, STORE => $nada);
    is $log, "", "bind not called spuriously (1)";

    $log = "";
    $a ::= Proxy.new(BIND => { $log ~= "B" }, FETCH => $nada, STORE => $nada);
    is $log, "", "bind not called spuriously (2)";

    $log = "";
    my $b := Proxy.new(BIND => { $log ~= "B" }, FETCH => $nada, STORE => $nada); #OK
    is $log, "B", "bind called when needed (bind)";

    $log = "";
    Proxy.new(BIND => { $log ~= "B" }, FETCH => $nada, STORE => $nada) = 5;
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
        multi method b1(Bool) { "bool" }
        multi method b1(Str) { "str" }

        multi method b2(Bool) { "bool" }
        multi method b2(Any) { "any" }

        multi method b3(Any) { "any" }
        multi method b3(Bool) { "bool" }

        multi method b4(Bool, Any) { "doom" }
        multi method b4(Any, Bool) { "doom" }
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
    multi foo(Str) { "str" }
    multi foo(Bool) { "bool" }

    is foo(True), "bool", "multisubs work (1)";
    is foo("abc"), "str", "multisubs work (2)";

    {
        multi foo(Any) { "any" }
        is foo(True), "bool", "augmenting multisubs works (1)";
        is foo(5), "any", "augmenting multisubs works (2)";

        {
            proto foo($) {*}
            multi foo(Any) { "any2" }
            is foo(True), "any2", "proto-shadowing works";
        }

        {
            sub foo(Any) { "any3" }
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

{
    grammar G4675 {
        token TOP { abc }
    }
    G4675.parse("abc");
    ok $/, '.parse sets $/ (1)';
    G4675.parse("def");
    nok $/, '.parse sets $/ (2)';
}

{
    is "a1b2c".subst(/\d/, 'd'), 'adb2c', '.subst works';
    is "a1b2c".subst(:global, /\d/, 'd'), 'adbdc', '.subst works with :g';
    is "a1b2c".subst(/\d/, {$/+1}, :g), 'a2b3c', '.subst works with $/';

    ok Str.^can("subst"), "Str can subst";
    ok Str.^can("defined"), "Str can defined";
    nok Str.^can("quux"), "Str cannot quux";

    rxtest /z .* y [ a ::> x || . ]/, "z.*y[a::>x||.]",
        ("zyax", "zyb", "zyaxya"), ("zya",);
    rxtest /z [ [ a ::> x || . ] | . y ]/, "z[[a::>x||.]|.y]", ("zay",), ();
}

{
    my $i = 0;
    L1: while $i < 10 { L1.last if $i == 5; $i++ }
    is $i, 5, "method .last works";

    L2: for 2,3,4 { $i = $_; last L2 if $i == 3; }
    is $i, 3, "last in for works";
}

{
    my ($str, $v1, $v2);
    $str = "abcdef";
    substr-rw($str,1,2) = "xy";
    is $str, "axydef", "lvalue substr is functional";
    $str = "abcdef";
    substr-rw($str,1,2) = "jklmno";
    is $str, "ajklmnodef", "lvalue substr can change string length";
    $str = "abcdef";
    $v1 := substr-rw($str,1,2);
    $str = "xyzw";
    is $v1, "yz", "substr return values read lazily";
    $str = "abcdef";
    $v1 := substr-rw($str,0,1);
    $v2 := substr-rw($str,3,1);
    $v1 = "xx";
    $v2 = "yy";
    is $str, "xxbyydef", "substr thunks track by index";
}

{
    my class A {
        multi method foo(Str, Any) { "A" }
    }
    my class B is A {
        multi method foo(Any, Str) { "B" }
    }
    is B.foo("x","y"), "B", "MRO used as tiebreaker";
    multi bar(Str, Any) { "X" } #OK
    {
        multi bar(Any, Str) { "Y" }
        is bar("a","b"), "Y", "depth used as tiebreaker";
    }

    my $ok;
    given 1 {
        when 2 { }
        default { $ok = True }
    }
    ok $ok, "default works";
}

{
    my @q;
    sub capture() { push @q, caller.hints('$_') }

    $_ := 5;
    capture;
    for 6 { capture }
    capture given 7;
    capture;

    is @q[0], 5, 'can capture $CALLER::_ from run-once block';
    is @q[1], 6, 'can capture $_ from a run-many block';
    is @q[2], 7, 'can capture temporary $_ from postfix given';
    is @q[3], 5, '$_ not disturbed by given';
}

{
    $_ := "baar";
    ok (/a+/ ?? True !! False), "Regex.Bool works";
    is $/.chars, 2, 'Regex.Bool sets $/ properly';

    is 'ab-c'.split(/<.ws>/).join('|'), '|ab|-|c|',
        'zero-width split works correctly';
}

{
    ok ![1,2,3].flattens, "[1,2,3] non-flatteny";
    ok [1,2,3].list.flattens, "[1,2,3].list flatteny";

    is Array.perl, "Array", ".perl: Array";
    is [].perl, "[]", ".perl: []";
    is [1].perl, "[1]", ".perl: [1]";
    is [1,2,3].perl, "[1, 2, 3]", ".perl: [1,2,3]";
    is @([1,2,3]).perl, "[1, 2, 3].list", '.perl: @([1,2,3])';

    is Hash.perl, "Hash", ".perl: Hash";
    is {a => 1}.perl, '{"a" => 1}', '.perl: {a => 1}';
    is %({a => 1}).perl, '{"a" => 1}.hash', '.perl: %({a => 1})';

    is Num.perl, "Num", ".perl: Num";
    is 5.perl, "5", ".perl: 5";

    is Str.perl, "Str", ".perl: Str";
    is "foo".perl, '"foo"', '.perl: "foo"';

    is Capture.perl, "Capture", '.perl: Capture';
    is (\1).perl, '\(1)', '.perl: \1';
    is (\(1, :x)).perl, '\(1, |{"x" => Bool::True})', '.perl: \(1, :x)';
    is (\(:x)).perl, '\(|{"x" => Bool::True})', '.perl: \(:x)';

    is Parcel.perl, "Parcel", '.perl: Parcel';
    is ().perl, '()', '.perl: ()';
    is (1,).perl, '(1, )', '.perl: (1,)';
    is (1,2,3).perl, '(1, 2, 3)', '.perl: (1,2,3)';
    is $(1,2,3).perl, '$(1, 2, 3)', '.perl: $(1,2,3)';
}

{
    $_ := 5;
    if True {
        is $_, 5, '$_ passes into ifs';
    }

    if False {
    } else {
        is $_, 5, '$_ passes into elses';
    }

    my $i = 1;
    while $i-- {
        is $_, 5, '$_ passes into whiles';
    }
    given 8 {
        default {
            is $_, 8, '$_ passes into whens/defaults';
        }
    }
    {
        is $_, 5, '$_ passes into bare blocks';
    }

    sub foo($x is rw) { $x }
    dies_ok { foo 5 }, "cannot rw-bind constant";

    my @foo = 1,2,3; #OK
    is "@foo", '@foo', '@-vars do not interpolate';

    my $x; my $y;
    ok $x =:= $x, '$x =:= $x';
    nok $x =:= $y, '$x !=:= $y';
    $x := $y;
    ok $x =:= $y, '$x =:= $y (after $x := $y)';

    my class A {
        has $.x;
        method foo($bar:) { $.x } #OK
    }
    is A.new(x => 5).foo, 5, "explicit invocants don't break self";
}

{
    my class Foo {
        has @.bar;
        has %.baz;
        has @.quux = 1,2,3;
    }
    isa_ok Foo.new.bar, Array, '@.bar initializes as an array';
    isa_ok Foo.new.baz, Hash, '%.baz initializes as a hash';
    is +[ Foo.new(bar => (1,2,4)).bar ], 3, '@.bar initializes with list context';
    is +[ Foo.new(bar => 5).bar ], 1, '@.bar can initialize from a single item';
    is +[ Foo.new.quux ], 3, '@.quux with init list works';

    my $str = '';
    for 1,2,3,4 -> $x, $y { $str ~= "$x|$y," }
    is $str, "1|2,3|4,", 'multivariable for works';

    is "moo".subst('o','a',:g), "maa", '.subst can take Str';
    is 'Hello'.substr(1), 'ello', '.substr can take 1 arguaent';
    is hash((a => 1)).perl, '{"a" => 1}.hash', '&hash works (1)';
    is hash((a => 1, b => 2)).<b>, 2, '&hash works (2)';
    is hash({a => 1}).perl, '{"a" => 1}.hash', '&hash works (3)';

    my %hash = "foo", 5;
    is %hash<foo>, 5, "Hash.LISTSTORE can take keys and values separately";
    dies_ok { %hash = "pie" }, "keys must be matched";
}

{
    class X7140::X1122 { }
    my X7140::X1122 $obj .= new;
    isa_ok $obj, X7140::X1122, 'Type constraints via :: work';

    sub foo(X7140::X1122 $) { }
    lives_ok { foo($obj) }, 'Parameter :: constraints work (1)';
    dies_ok { foo(5) }, 'Parameter :: constraints work (1)';

    sub bar(@x) {} #OK
    lives_ok { bar <a b c> }, '<> splitting counts as one argument';

    my class Foo { method foo() { 12 } }
    is Foo.?foo, 12, '.? works (successful)';
    is +[Foo.?bar], 0, '.? works (unsuccessful, list)';

    my $k = 2;
    my $st = '';
    while $k -> $z { $st ~= $z; $k = False }
    is $st, '2', 'while loops can take ->';

    $st = ''; $k = False;
    until $k -> $z { $st ~= $z; $k = True }
    is $st, False, 'until loops can take ->';

    $st = '';
    unless False -> $z { $st ~= $z }
    is $st, False, 'unless can take ->';

    $st = ''; $k = True;
    repeat until $k -> $z { $st ~= ($z // 5); $k = !$k; }
    is $st, 5~False, 'repeat until (prefix) can take ->';

    $st = ''; $k = True;
    repeat -> $z { $st ~= ($z // 5); $k = !$k; } until $k;
    is $st, 5~False, 'repeat until (postfix) can take ->';
}

{
    sub foo($x is copy) { $x++; $x }
    is foo(5), 6, "is copy works (non inline)";
    my $y;
    for 5 -> $k is copy { $k++; $y = $k }
    is $y, 6, "is copy works (inline)";
}

{
    is "filename0000.jpg".succ, "filename0001.jpg", "filename0000.jpg test";
    is "000".succ, "001", "basic succ";
    is "009".succ, "010", ".succ with carry";
    is "099".succ, "100", ".succ with cascading carry";
    is "a99".succ, "b00", ".succ with carry across types";
    is "z99".succ, "aa00", ".succ with extending (non-digit)";
    is "99".succ, "100", ".succ with extending (digit)";

    is "001".pred, "000", "basic .pred";
    is "010".pred, "009", ".pred with borrow";
    is "100".pred, "099", "cascading borrow";
    is "b00".pred, "a99", "borrow across types";

    is "--99--".succ, "--100--", "lengthening shifts";
    is "--00--".succ, "--01--", "not lengthening, no shift";
    is "00.00".succ, "01.00", "dot sets endpoint";

    is +["a" .. "z"], 26, "char ranges work";

    is_deeply [[<za zz az aa>].sort(*.flip)], [<aa za az zz>],
        'Automatic Schwartzian transform works';
}

is 10.abs, 10, "10.abs == 10";
is (-10).abs, 10, "(-10).abs == 10";
is (-10).abs.WHAT.gist, 10.WHAT.gist, "(-10).abs and 10 have same WHAT";

{
    my $big = 2 ** 200 - 42;
    is $big.abs, $big, '$big.abs == $big';
    is (-$big).abs, $big, '(-$big).abs == $big';
    is (-$big).abs.WHAT.gist, $big.WHAT.gist, '(-$bi).abs and $big have same WHAT';
}

ok (10/2).abs == 10/2, "(10/2).abs == 10/2";
ok (-10/2).abs == 10/2, "(-10/2).abs == 10/2";
is (-10/2).abs.WHAT.gist, (10/2).WHAT.gist, "(-10/2).abs and 10/2 have same WHAT";

ok FatRat.new(10,2).abs == FatRat.new(10,2), "(10/2).abs == 10/2";
ok FatRat.new(-10,2).abs == FatRat.new(10,2), "(-10/2).abs == 10/2";
is FatRat.new(-10,2).abs.WHAT.gist, FatRat.gist, "FatRat.new(-10,2).abs.WHAT";

ok (10.Num).abs == 10.Num, "(10.Num).abs == 10.Num";
ok (-10.Num).abs == 10.Num, "(-10.Num).abs == 10.Num";
is (-10.Num).abs.WHAT.gist, (10.Num).WHAT.gist, "(-10.Num).abs and 10.Num have same WHAT";

ok (10 + 5i).abs == 125.sqrt, "(10 + 5i).abs == 125.sqrt";
ok (-10 - 5i).abs == 125.sqrt, "(-10 - 5i).abs == 125.sqrt";
is (-10 - 5i).abs.WHAT.gist, "Num()", "(-10 - 5i).abs is a Num";

ok (10 + 5i).re == 10, "(10 + 5i).re == 10";
ok (10 + 5i).im == 5, "(10 + 5i).im == 5";

ok (10/3).numerator == 10, "(10/3).numerator == 10";
ok (10/3).denominator == 3, "(10/3).denominator == 3";
is FatRat.new(10,3).WHAT.gist, FatRat.gist, "FatRat.new returns FatRat";
ok FatRat.new(10,3).numerator == 10, "FatRat.new(10,3).numerator == 10";
ok FatRat.new(10,3).denominator == 3, "FatRat.new(10,3).denominator == 3";

class {
    is (sub () {}).WHAT.gist, Sub.gist, "sub gets correct class";
    is ({ $_ * $_ }).WHAT.gist, Block.gist, "bare block gets correct class";
    is /a+/.WHAT.gist, Regex.gist, "regex gets correct class";
    is (method a () {}).WHAT.gist, Method.gist, "method gets correct class";
    is (submethod b () {}).WHAT.gist, Submethod.gist, "submethod gets correct class";
};

is one(1,2).perl, 'one(1, 2)', '.perl roundtrips one()';
is any(1,2).perl, 'any(1, 2)', '.perl roundtrips any()';
is all(1,2).perl, 'all(1, 2)', '.perl roundtrips all()';
is none(1,2).perl, 'none(1, 2)', '.perl roundtrips none()';

is (1 & 2).perl, 'all(1, 2)', '& means all';
is (1 | 2).perl, 'any(1, 2)', '| means any';
is (1 ^ 2).perl, 'one(1, 2)', '^ means one';

is (1, 2).any.perl, 'any(1, 2)', 'Any.any means any()';
is (1, 2).one.perl, 'one(1, 2)', 'Any.one means one()';
is (1, 2).all.perl, 'all(1, 2)', 'Any.all means all()';
is (1, 2).none.perl, 'none(1, 2)', 'Any.none means none()';

is { a => True }.any.perl, any("a").perl, 'Hash.any means any(keys)';

ok  ?(all( True,  True)), 'all(True, True)';
nok ?(all( True, False)), '!all(True, False)';
nok ?(all(False, False)), '!all(False, False)';

ok  ?(any( True,  True)), 'any(True, True)';
ok  ?(any( True, False)), 'any(True, False)';
nok ?(any(False, False)), '!any(False, False)';

nok ?(one( True,  True)), '!one(True, True)';
ok  ?(one( True, False)), 'one(True, False)';
nok ?(one(False, False)), '!one(False, False)';

nok ?(none( True,  True)), '!none(True, True)';
nok ?(none( True, False)), 'none(True, False)';
ok  ?(none(False, False)), 'none(False, False)';

is ((1 & 3) + 1).perl, 'all(2, 4)', '+ autothreads all';
is ((1 | 3) + 1).perl, 'any(2, 4)', '+ autothreads any';
is (1 == (1 | 3)).perl, 'any(Bool::True, Bool::False)', '== autothreads';

is (4 & 9).sqrt.perl, 'all(2e0, 3e0)', '.sqrt autothreads';
is (4 & 9).Bool.perl, 'Bool::True', '.Bool does not autothread';
is (40 & 90).substr(0,1).perl, 'all("4", "9")', '.substr with arguments autothreads';
is ((2 | 4) + (1 & 2)).perl, 'all(any(3, 5), any(4, 6))',
    '& takes precedence in multiple autothreading';

{
    sub f1($x) { ?$x }
    sub f2(Mu $x) { ?$x }
    sub f3($x,$y) { $x ~ $y }
    is f1(1 | 0).perl, 'any(Bool::True, Bool::False)',
        'simple autothreading of only sub';
    is f2(1 | 0).perl, 'Bool::True', 'non-autothreading of sub that takes Mu';
    is f3((1 | 0), (1 | 0)).perl, 'any(any("11", "10"), any("01", "00"))',
        'autothreading of multi-arg only sub';
}

nok ?("foo" !eq any("foo","bar")), "foo !eq (foo | bar)";
ok ?("foo" !eq any("quux","bar")), "foo !eq (quux | bar)";
nok ?(2 != any(2, 4)), "2 != (2|4)";
ok ?(2 != any(0, 4)), "2 != (0|4)";

ok 2 ~~ any(Int, Num), "junctional ~~ works";

{
    my package Thing {
        our sub foo() { 42 }
        is foo(), 42, 'direct call to our sub works';
        is &OUR::foo.(), 42, 'call via &OUR::foo works';
        is OUR::foo, 42, 'call via OUR::foo works';
    }

    is Thing::foo, 42, 'call via Thing::foo works for inside declared sub';
    our sub Thing::bar() { 84 }
    is Thing::bar, 84, 'call via Thing::bar for outside declared works';
}

ok $?FILE ~~ /test2?\.pl/, '$?FILE works';
