# vim: ft=perl6
use Test;

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

done-testing;
