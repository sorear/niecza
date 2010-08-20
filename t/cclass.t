# Unit tests for the only really isolated module (so far)

use lib 'src';
use Test::More;
use CClass;

use Unicode::UCD 'charinfo';

sub CClass::accepts {
    my ($self, $ch) = @_;

    my $mask = 0;
    for (my $i = 0; $i < @$self; $i += 2) {
        last if ($self->[$i] > ord($ch));
        $mask = $self->[$i+1];
    }

    my $ci = charinfo(ord($ch));

    return $mask & (1 << $CClass::Gc{ $ci ? $ci->{category} : 'Cn' });
}

sub cctest {
    my ($ccexp, $ys, $ns) = @_;
    my $cc = eval $ccexp;

    for my $y (@$ys) {
        my $inf = charinfo(ord($y));
        ok $cc->accepts($y), sprintf "%s accepts U+%s %s",
            $ccexp, $inf->{code}, $inf->{name};
    }

    for my $n (@$ns) {
        my $inf = charinfo(ord($n));
        ok !$cc->accepts($n), sprintf "%s rejects U+%s %s",
            $ccexp, $inf->{code}, $inf->{name};
    }
}

sub flat { join "|", @{ $_[0] } }

cctest 'CClass->range("b", "d")', ['b', 'c', 'd'],
    ["\0", 'a', 'e', ' ', "\x{3000}"];
cctest 'CClass->range("+", "+")', ['+'],
    ["*", ",", " ", "a", "9"];
cctest 'CClass->range("c", "b")', [],
    ["a", "b", "c", "d", "\0"];
cctest '$CClass::Empty', [], ["\0", "A"];
cctest 'CClass->enum("A","E","I","O","U")', ["A", "E", "I", "O", "U"],
    ["a", "e", "i", "o", "u", "@", "B", "C", "D", "F", "V"];
cctest 'CClass->catm("Lu")', ["A", "Z", "\xc6"],
    ["a", "+", "3", " ", "\0", "\x{4E03}"];
cctest 'CClass->catm("Lu", "Ll")', ["A", "a", "Z", "z"],
    ["+", "9", "\n", "\x{4E00}"];
cctest 'CClass->range("a", "e")->plus(CClass->range("d", "g"))',
    [qw/a b c d e f g/], [qw/` h/];
cctest 'CClass->range("a", "e")->plus(CClass->range("b", "d"))',
    [qw/a b c d e/], [qw/` f/];
cctest 'CClass->range("a", "c")->plus(CClass->range("d", "f"))',
    [qw/a b c d e f/], [qw/` g/];
cctest 'CClass->range("e", "g")->negate', [qw/+ a b c d h i/],
    [qw/e f g/];
cctest '$CClass::Digit', [qw/0 1 2 3 4 5 6 7 8 9/],
    [qw/+ - A Z/, "\0", "\n"];
cctest '$CClass::Word', [qw/_ 0 9 A Z a z/, "\x{4E00}"], [' ', ',', '-', "\n"];

done_testing;
