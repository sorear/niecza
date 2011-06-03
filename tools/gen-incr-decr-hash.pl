use v6;

my @alphabets;
push @alphabets, [ord('0')..ord('9')];
push @alphabets, [ord('A')..ord('Z')];
push @alphabets, [ord('a')..ord('z')];
# avoid FINAL SIGMAs for Greek
push @alphabets, [grep * != 0x3a2, ord('Α')..ord('Ω')];
push @alphabets, [grep * != 0x3c2, ord('α')..ord('ω')];
push @alphabets, [ord('Ⅰ')..ord('Ⅻ')]; #  clock roman uc (U+2160..U+216b)
push @alphabets, [ord('ⅰ')..ord('ⅻ')]; #  clock roman lc
push @alphabets, [ord('①')..ord('⑳')]; #  circled digits (U+2460..U+2473)
push @alphabets, [ord('⑴')..ord('⒇')]; #  parenth digits (U+2474..U+2487)
push @alphabets, [ord('⒜')..ord('⒵')]; #  parenth lc     (U+249c..U+24b5)
push @alphabets, [ord('⚀')..ord('⚅')]; #  die faces      (U+2680..U+2685)
push @alphabets, [map &ord, "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲン".comb];

my @ranges;

for @alphabets -> $r {
    say "$r is out of order!" unless [<] @$r;
    my $last = $r[*-1];
    my $first = $r[0];
    while @$r {
        my @range = shift($r);
        push @range, shift($r) while $r && $r[0] == @range[*-1]+1;
        push @ranges, [
            @range[0],
            @range[*-1],
            $last,
            $r ?? $r[0] !! $first
        ];
        $last = @ranges[*-1][1];
    }
}

say @ranges.sort(*[0] - *[1]).map(*.list).join(", ");
