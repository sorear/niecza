# vim: ft=perl6
use Test;
use MONKEY_TYPING;

augment class Cursor {
    method suppose($rx) {
        my $*IN_SUPPOSE = True;
        my $*FATALS = 0;
        my @*WORRIES;
        my %*WORRIES;
        my $*HIGHWATER = -1;
        my $*HIGHEXPECT = {};
        try {
            my @ret := $rx(self);
            if (@ret) { return @( self, ) }
        };
        return ();
    }
}

augment class Parcel {
    method LISTSTORE(*@in) {
        my @values = @in;

        # TODO: proper (sized) handling of sub-parcels
        Q:CgOp {
            (rnull
              (letn i    (i 0)
                    tgts (unbox fvarlist (@ {self}))
                    ntgt (fvarlist_length (l tgts))
                    tgt  (null var)
                (whileloop 0 0 (< (l i) (l ntgt))
                  (prog
                    (l tgt (fvarlist_item (l i) (l tgts)))
                    (l i (+ (l i) (i 1)))
                    (ternary (var_islist (l tgt))
                      (prog
                        (sink (methodcall (l tgt) LISTSTORE {@values.clone}))
                        (sink {@values = Nil}))
                      (assign (l tgt) {@values ?? @values.shift !! Any}))))))
        };

        @in;
    }
}

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
    ok "foo" ~~ / f <.suppose o> oo /, ".suppose works (T)";

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

# {
#     our role Stop4717[$a] {
#         token foo { $a }
#     }
#
#     grammar X {
#         token TOP { [ <foo> | foo ]: x }
#     }
#
#     ok (X but OUR::Stop4717["foobar"]).parse("foobarx"),
#         "LTM works through parameterized role variables";
# }

#is $?FILE, 'test.pl', '$?FILE works';
#is $?ORIG.substr(0,5), '# vim', '$?ORIG works';

# {
#     {
#         our $x = 5; #OK
#     }
#     ok $::x == 5, '$::x finds our variable';
# 
#     package Fao { our $y = 6; } #OK
#     ok $::Fao::y == 6, '$::Fao::y works as $Fao::y';
# 
#     { class Mao { } }
#     ok ::Mao.new.defined, 'can use classes via ::Mao';
# }
# 
# {
#     my $x = 7; #OK
#     ok $::x == 7, '$::x can find lexicals';
#     class A3 {
#         method moo { 42 }
#         class B4 {
#             ok ::A3.moo, '::A3 can find outer classes';
#         }
#     }
# }

done-testing;
