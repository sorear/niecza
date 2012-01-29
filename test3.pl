use Test;

{
    "fooxbar" ~~ /x/;
    is $/.prematch, "foo", ".prematch works";
    is $/.postmatch, "bar", ".postmatch works";

    is ("foo" ~~ /(f)/).kv.join("|"), '0|f', '.kv sees positional';
    is ("foo" ~~ /$<x>=[f]/).kv.join("|"), 'x|f', '.kv sees names';
    is ("foo" ~~ /(f)/).keys.join("|"), '0', '.keys sees positional';
    is ("foo" ~~ /$<x>=[f]/).keys.join("|"), 'x', '.keys sees names';
    is ("foo" ~~ /(f)/).values.join("|"), 'f', '.values sees positional';
    is ("foo" ~~ /$<x>=[f]/).values.join("|"), 'f', '.values sees names';

    "abcdefg" ~~ / b $<x>=[c] d (e) f /;
    is $/.caps».key.join('|'), 'x|0', '.caps works (keys)';
    is $/.caps».value.join('|'), 'c|e', '.caps works (values)';
    is $/.chunks».key.join('|'), '~|x|~|0|~', '.chunks works (keys)';
    is $/.chunks».value.join('|'), 'b|c|d|e|f', '.chunks works (values)';
}

{
    "foo" ~~ /./;
    is "+a+".subst(/\w/, { uc $/ }), '+A+', 'can use $/ in subst() arg';
    is ~$/, 'f', '$/ restored after subst';
    $_ = "baa5ar";
    s/\d//;
    is ~$/, '5', 's/// resets $/';
}

{
    my $hello = "'/'hello'/'(<-[\\\\/\\\\.]>+)";
    my $reg  = / ^ <hello=$hello> $/;
    "/hello/bug" ~~ $reg;
    is $/<hello>[0], 'bug', '<foo=$bar> works';
}

# http://irclog.perlgeek.de/perl6/2011-12-29#i_4894154
{
    "ab" ~~ / $<a>=[ <alpha>+ ] /;
    is $<alpha>[1], "b", "<foo>+ inside string capture works";
}

# the Rakudo-inherited algorithm failed on this one.
{
    my class A { }
    my class B is A { }
    my class C is B { }

    multi foo(A $, A $) { "AA" }
    multi foo(A $, B $) { "AB" }
    multi foo(A $, C $) { "AC" }
    multi foo(B $, A $) { "BA" }
    multi foo(C $, A $) { "CA" }

    dies_ok { foo(B, C) }, "hard case of catching ambiguous dispatch";
}

{
    sub foo(Any:U $) { } #OK
    sub bar(Any:D $) { } #OK
    sub moo(Any:_ $) { } #OK
    sub cow(Any:T $) { } #OK

    multi qux(Any:U $) { "U" }
    multi qux(Any:D $) { "D" }

    lives_ok { eval "foo(Int)" }, ":U allows type objects";
    dies_ok  { eval "foo(5)" },   ":U denies concrete objects";

    lives_ok { eval "bar(5)" },   ":D allows concrete objects";
    dies_ok  { eval "bar(Int)" }, ":D denies type objects";

    lives_ok { eval "moo(5)" },   ":_ allows concrete objects";
    lives_ok { eval "moo(Int)" }, ":_ allows type objects";

    lives_ok { eval "cow(Int)" }, ":T allows type objects";
    dies_ok  { eval "cow(5)" },   ":T denies concrete objects";

    is qux(Int), 'U', 'multi can discriminate on :U/:D (1)';
    is qux(5),   'D', 'multi can discriminate on :U/:D (2)';
}

{
    sub foo(%x) { } #OK
    dies_ok { foo(3) }, '%x needs Associative';
}

{
    my grammar Z5 {
        token TOP { fo* | <k(5)> }
        token k( $ ) { foo }
        token p:x ($x) { {$*ret = $x} }
    }
    ok Z5.parse('foo')<k>.defined, 'arguments do not disturb LTM';
    my $*ret;
    "foo" ~~ / :lang(Z5) <p(9)> /;
    is $*ret, 9, 'protoregex dispatchers pass arguments to callees';
}
