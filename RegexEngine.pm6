my module RegexEngine;
# vim: ft=perl6
my class Cursor is export {
    # has $!str
    # has $!from
    method str() { $!str }
    method from() { $!from }
}

# Outside a regex, a result is a lazy list.
# Inside a regex, a result is a bare iterator, or a double return (if
# ratcheting).

sub _rxexport($cs) is export {
    my class ExportIterator is Iterator {
        # $!valid $!value $!next  $!fun
        method validate() {
            $!valid = 1;
            $!value = ($!fun)() || EMPTY;
            $!next = ExportIterator.RAWCREATE("valid", 0, "next", Any,
                "value", Any, "fun", $!fun);
        }
    }

    my $lit = ExportIterator.RAWCREATE("valid", 0, "value", Any,
        "next", Any, "fun", $cs);
    my @l := List.RAWCREATE("flat", 1, "items", LLArray.new(), "rest",
        LLArray.new($lit));
    @l.fill(1);
    @l;
}

sub _rxlazymap($cs, $sub) is export {
    my $k = sub { Any };
    #say "in rxlazymap (1)";
    sub get() {
        #say "in rxlazymap (2)";
        $k && ($k() || do {
            #say "in rxlazymap (3)";
            $k = $cs();
            $k = ($k && $sub($k));
            #say "in rxlazymap (4)";
            get();
        })
    }
}

sub _rxdisj($cs1, $cs2) is export {
    my $k1 = $cs1;
    my $k2 = $cs2;
    sub {
        #say "in rxdisj (1)";
        $k1() || ($k2 && do {
            $k1 = $k2;
            $k2 = Any;
            #say "in rxdisj (2)";
            $k1();
        })
    }
}

sub _rxone($C) is export {
    my $k = $C;
    sub {
        my $x = $k;
        $k = Any;
        #say "in rxone" ~ $x;
        $x;
    }
}

sub _rxnone is export { Any };

sub _rxstar($C, $sub) is export {
    #say "in rxstar recursion";
    _rxdisj(_rxlazymap($sub($C), sub ($C) { _rxstar($C, $sub) }),
            _rxone($C));
}

sub _rxopt($C, $sub) is export {
    _rxdisj($sub($C), _rxone($C))
}

sub _rxplus($C, $sub) is export {
    _rxlazymap($sub($C), sub ($C) { _rxstar($C, $sub) })
}

sub _rxstr($C, $str) is export {
    #say "_rxstr : " ~ ($C.str ~ (" @ " ~ ($C.from ~ (" ? " ~ $str))));
    if $C.from + $str.chars <= $C.str.chars &&
            $C.str.substr($C.from, $str.chars) eq $str {
        _rxone(Cursor.RAWCREATE("str", $C.str, "from", $C.from + $str.chars));
    } else {
        &_rxnone;
    }
}

my class Regex is Sub is export {
    method ACCEPTS($str) {
        my $i = 0;
        my $win = 0;
        while !$win && $i <= $str.chars {
            my $C = Cursor.RAWCREATE("str", $str, "from", $i);
            if (self)($C) {
                $win = 1;
            }
            $i++;
        }
        $win;
    }
}
