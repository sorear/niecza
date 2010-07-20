# vim: ft=perl6
my class Cursor {
    # has $!str
    # has $!from
    method str() { $!str }
    method from() { $!from }
}

PRE-INIT {
    Mu.HOW.add-method(Q:CgOp { (w (clr_string "flat")) },
        anon method flat() { self, });
    Parcel.HOW.add-method(Q:CgOp { (w (clr_string "flat")) },
        anon method flat() {
            my @x := self;
            List.RAWCREATE("flat", 1, "items", LLArray.new(),
                "rest", LLArray.new(@x));
        });
    List.HOW.add-method(Q:CgOp { (w (clr_string "flat")) },
        anon method flat() {
            my @x := self;
            List.RAWCREATE("flat", 1, "items", LLArray.new(),
                "rest", LLArray.new(@x));
        });
}

sub _rxlazymap($cs, $sub) {
    my class LazyIterator is Iterator {
        method back() { $!back }
        # $!valid $!value $!next  $!fun $!back
        method validate() {
            $!valid = 1;
            if ! $!back.valid {
                $!back.validate;
            }
            my $bv = $!back.value;
            my $bn = $!back.next;
            #say "In lazy map iterator";
            if $bv.^isa(EMPTY) {
                $!value = EMPTY;
            } else {
                #say "Going to pass " ~ $bv;
                my $f = $!fun;
                # XXX horrible. we're making the value act @-ish, maybe
                Q:CgOp {
                    (prog
                      [setindex value (getfield slots (cast DynObject
                            (@ (l self)))) (subcall (@ (l $f)) (l $bv))]
                      [null Variable])
                };
                $!next = LazyIterator.RAWCREATE("valid", 0, "next", Any,
                    "value", Any, "back", Any, "fun", $!fun);
                $!next.back = $bn;
            }
        }
    }

    my $lit = LazyIterator.RAWCREATE("valid", 0, "value", Any,
        "next", Any, "back", Any, "fun", $sub);
    $lit.back = $cs.flat.iterator;

    my @l := List.RAWCREATE("flat", 1, "items", LLArray.new(), "rest",
        LLArray.new($lit));
    @l.fill(1);
    @l;
}

sub _rxstar($C, $sub) {
    _rxlazymap($sub($C), sub ($C) { _rxstar($C, $sub) }), $C
}

sub _rxstr($C, $str) {
    #say "_rxstr : " ~ ($C.str ~ (" @ " ~ ($C.from ~ (" ? " ~ $str))));
    if $C.from + $str.chars <= $C.str.chars &&
            $C.str.substr($C.from, $str.chars) eq $str {
        Cursor.RAWCREATE("str", $C.str, "from", $C.from + $str.chars);
    } else {
        Nil;
    }
}

# regex { a b* c }
sub rxtest($C) {
    _rxlazymap(_rxstr($C, 'a'), sub ($C) { _rxlazymap(_rxstar($C, sub ($C) { _rxstr($C, 'b') }), sub ($C) { _rxstr($C, 'c') }) })
}

sub test($str) {
    my $i = 0;
    my $win = 0;
    while !$win && $i <= $str.chars {
        my $C = Cursor.RAWCREATE("str", $str, "from", $i);
        if rxtest($C) {
            $win = 1;
        }
        $i++;
    }
    say $str ~ (" ... " ~ $win);
}

test "xaaabc";
test "xbc";
test "abbbc";
test "ac";
test "aabb";
test "abx";
