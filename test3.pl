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
