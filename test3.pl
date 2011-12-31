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
    sub foo(Any:U $) { }
    sub bar(Any:D $) { }
    sub moo(Any:_ $) { }
    sub cow(Any:T $) { }

    multi qux(Any:U $) { "U" }
    multi qux(Any:D $) { "D" }

    eval_lives_ok "foo(Int)", ":U allows type objects";
    eval_dies_ok  "foo(5)",   ":U denies concrete objects";

    eval_lives_ok "bar(5)",   ":D allows concrete objects";
    eval_dies_ok  "bar(Int)", ":D denies type objects";

    eval_lives_ok "moo(5)",   ":_ allows concrete objects";
    eval_lives_ok "moo(Int)", ":_ allows type objects";

    eval_lives_ok "cow(Int)", ":T allows type objects";
    eval_dies_ok  "cow(5)",   ":T denies concrete objects";

    is qux(Int), 'U', 'multi can discriminate on :U/:D (1)';
    is qux(5),   'D', 'multi can discriminate on :U/:D (2)';
}
