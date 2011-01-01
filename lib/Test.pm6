my module Test;

constant $?TRANSPARENT = 1;

class Builder {
    has $.current-test;
    has $!set-plan;

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
        my $desc;
        if $tag {
            $desc = " - " ~ $tag.split("\n").join("\n#");
        } else {
            $desc = '';
        }
        self!output($not ~ "ok " ~ $.current-test++ ~ $desc);
        if !$bool { self.note(self.blame); }
    }

    method expected-tests($num) {
        self!output("1.." ~ $num);
    }

    # XXX multi!
    method plan($x) {
        $!set-plan = 1;
        if $x ~~ Num {
            self.expected-tests($x);
        } else {
            die "Invalid argument to plan";
        }
    }

    method done {
        if !($!set-plan) {
            self!output("1.." ~ ($.current-test - 1));
        }
    }
}

$GLOBAL::TEST-BUILDER = Builder.CREATE;
$GLOBAL::TEST-BUILDER.reset;

sub ok($bool, $tag?) is export { $*TEST-BUILDER.ok($bool, $tag) }
sub pass($tag?) is export { $*TEST-BUILDER.ok(1, $tag) }
sub flunk($tag?) is export { $*TEST-BUILDER.ok(0, $tag) }
sub is($got, $expected, $tag?) is export {

    # avoid comparing twice
    my $equal = $got eq $expected;

    $*TEST-BUILDER.ok($equal, $tag);
    if !$equal {
        $*TEST-BUILDER.note('   Failed test');
        $*TEST-BUILDER.note('          got: '~$got);
        $*TEST-BUILDER.note('     expected: '~$expected);
    }
}
sub plan($num) is export { $*TEST-BUILDER.plan($num) }
sub done() is export { $*TEST-BUILDER.done }
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


