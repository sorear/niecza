# vim: ft=perl6
use Test;

# these are immutable, though we may wind up reusing them in some cases by
# uniqueness rules (TBD)
my class Cursor {
    has $.str;
    has $.from;
}

# Outside a regex, a result is a lazy list.
# Inside a regex, a result is a coroutiney thing (details will change)

sub _rxexport($cs) { unfold({ $cs() // EMPTY }) }

sub _rxlazymap($cs, $sub) {
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

sub _rxdisj($cs1, $cs2) {
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

sub _rxone($C) {
    my $k = $C;
    sub {
        my $x = $k;
        $k = Any;
        #say "in rxone" ~ $x;
        $x;
    }
}

sub _rxnone { Any };

sub _rxstar($C, $sub) {
    #say "in rxstar recursion";
    _rxdisj(_rxlazymap($sub($C), sub ($C) { _rxstar($C, $sub) }),
            _rxone($C));
}

sub _rxopt($C, $sub) {
    _rxdisj($sub($C), _rxone($C))
}

sub _rxplus($C, $sub) {
    _rxlazymap($sub($C), sub ($C) { _rxstar($C, $sub) })
}

sub _rxstr($C, $str) {
    #say "_rxstr : " ~ ($C.str ~ (" @ " ~ ($C.from ~ (" ? " ~ $str))));
    if $C.from + $str.chars <= $C.str.chars &&
            $C.str.substr($C.from, $str.chars) eq $str {
        _rxone(Cursor.RAWCREATE("str", $C.str, "from", $C.from + $str.chars));
    } else {
        &_rxnone;
    }
}

my class Regex is Sub {
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

my class Grammar is Cursor {
    method parse($text) {
        my @results := self.RAWCREATE("str", $text, "from", 0).TOP\
            .grep({ $_.from == $_.str.chars });
        @results ?? @results.shift !! Any; # TODO List.at-pos
    }
}

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

done-testing;
