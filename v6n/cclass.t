# Unit tests for the only really isolated module (so far)

use Test;
use CClass;
use MONKEY_TYPING;

sub ord($x) { Q:CgOp { (rawscall Builtins,Kernel.Ord {$x}) } }
sub chr($x) { Q:CgOp { (rawscall Builtins,Kernel.Chr {$x}) } }
sub infix:<+&>($x, $y) { Q:CgOp { (rawscall Builtins,Kernel.NumAnd {$x} {$y}) } }
sub infix:<+|>($x, $y) { Q:CgOp { (rawscall Builtins,Kernel.NumOr {$x} {$y}) } }
sub infix:<+^>($x, $y) { Q:CgOp { (rawscall Builtins,Kernel.NumXor {$x} {$y}) } }
sub infix:<< +< >>($x, $y) { Q:CgOp { (rawscall Builtins,Kernel.NumLShift {$x} {$y}) } }
sub infix:<< +> >>($x, $y) { Q:CgOp { (rawscall Builtins,Kernel.NumRShift {$x} {$y}) } }
sub prefix:<< +^ >>($x) { Q:CgOp { (rawscall Builtins,Kernel.NumCompl {$x}) } }

sub category($char) { Q:CgOp { (rawscall Builtins,Kernel.UniCat {$char}) } }

augment class CClass {
    method accepts($ch) {
        my $chi = ord $ch;

        my $mask = 0;
        my $i = 0;
        while ($i < @( $.terms )) {
            last if ($.terms[$i] > $chi);
            $mask = $.terms[$i+1];
            $i += 2;
        }

        my $ci = category($chi);

        return $mask +& (1 +< $ci);
    }
}

sub cctest($ccexp, $cc, $ys, $ns) {
    constant $?TRANSPARENT = 1;
    for @$ys -> $y {
        ok $cc.accepts($y), "$ccexp accepts {ord $y}";
    }

    for @$ns -> $n {
        ok !$cc.accepts($n), "$ccexp rejects {ord $n}";
    }
}

cctest 'CClass->range("b", "d")', CClass.range("b", "d"),
    ['b', 'c', 'd'],
    ["\0", 'a', 'e', ' ', "\x3000"];
cctest 'CClass->range("+", "+")', CClass.range("+", "+"),
    ['+'],
    ["*", ",", " ", "a", "9"];
cctest 'CClass->range("c", "b")', CClass.range("c", "b"),
    [],
    ["a", "b", "c", "d", "\0"];
cctest '$CClass::Empty', $CClass::Empty,
    [],
    ["\0", "A"];
cctest 'CClass->enum("A","E","I","O","U")', CClass.enum("A","E","I","O","U"),
    ["A", "E", "I", "O", "U"],
    ["a", "e", "i", "o", "u", "@", "B", "C", "D", "F", "V"];
cctest 'CClass->catm("Lu")', CClass.catm("Lu"),
    ["A", "Z", "\xc6"],
    ["a", "+", "3", " ", "\0", "\x4E03"];
cctest 'CClass->catm("Lu", "Ll")', CClass.catm("Lu", "Ll"),
    ["A", "a", "Z", "z"],
    ["+", "9", "\n", "\x4E00"];
cctest 'CClass->range("a", "e")->plus(CClass->range("d", "g"))',
    CClass.range("a", "e").plus(CClass.range("d", "g")),
    [< a b c d e f g >],
    [< ` h >]; # `
cctest 'CClass->range("a", "e")->plus(CClass->range("b", "d"))',
    CClass.range("a", "e").plus(CClass.range("b", "d")),
    [< a b c d e >],
    [< ` f >]; # `
cctest 'CClass->range("a", "c")->plus(CClass->range("d", "f"))',
    CClass.range("a", "c").plus(CClass.range("d", "f")),
    [< a b c d e f >],
    [< ` g >]; # `
cctest 'CClass->range("e", "g")->negate', CClass.range("e", "g").negate,
    [< + a b c d h i >],
    [< e f g >]; # `
cctest '$CClass::Digit', $CClass::Digit,
    [< 0 1 2 3 4 5 6 7 8 9 >],
    [< + - A Z >, "\0", "\n"];
cctest '$CClass::Word', $CClass::Word,
    [< _ 0 9 A Z a z >, "\x4E00"],
    [' ', ',', '-', "\n"];

done_testing;
