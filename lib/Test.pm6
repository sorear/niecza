my module Test;

constant $?TRANSPARENT = 1;

class Builder {
    has $.current-test;

    method new() {
        $*TEST-BUILDER;
    }

    method blame() {
        my $frame = caller;
        while $frame.hints('$?TRANSPARENT') {
            $frame = $frame.caller;
        }
        $frame.file ~ " line " ~ $frame.line;
    }

    method !output($text) {
        say $text;
    }

    method reset() {
        $.current-test = 1;
    }

    method note($m) {
        self!output("# " ~ $m);
        0;
    }

    method ok($bool, $tag) {
        my $not = $bool ?? "" !! "not ";
        self!output($not ~ "ok " ~ $.current-test++ ~ " - " ~ $tag.split("\n").join("\n#"));
        if !$bool { self.note(self.blame); }
    }

    method expected-tests($num) {
        self!output("1.." ~ $num);
    }

    # XXX multi!
    method plan($x) {
        if $x ~~ Num {
            self.expected-tests($x);
        } elsif $x ~~ Whatever {
            # no effect
        } else {
            die "Invalid argument to plan";
        }
    }

    method done-testing {
        self!output("1.." ~ ($.current-test - 1));
    }
}

$GLOBAL::TEST-BUILDER = Builder.CREATE;
$GLOBAL::TEST-BUILDER.reset;

sub ok($bool, $tag) is export { $*TEST-BUILDER.ok($bool, $tag) }
sub is($a, $b, $tag) is export { $*TEST-BUILDER.ok($a eq $b, $tag) }
sub plan($num) is export { $*TEST-BUILDER.plan($num) }
sub done-testing() is export { $*TEST-BUILDER.done-testing }
sub done_testing() is export { $*TEST-BUILDER.done-testing }
sub skip($number,$reason) is export {
    my $i = 0;
    while ($i < $number) {
        $*TEST-BUILDER.ok(1, "skip "~$reason);
        $i++;
    }
}

# TODO standardize me
sub rxtest($rgx, $rgxname, @y, @n) is export {
    for @y {
        my $k = $_ ~~ Pair ?? $_.key !! $_;
        my $v = $_ ~~ Pair ?? $_.value !! $_;
        ok $k ~~ $rgx, "$rgxname ~~ $v";
    }
    for @n {
        my $k = $_ ~~ Pair ?? $_.key !! $_;
        my $v = $_ ~~ Pair ?? $_.value !! $_;
        ok !($k ~~ $rgx), "$rgxname !~~ $v";
    }
}


