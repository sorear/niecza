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

    # TODO: Generalize this.
    method todo($tag, $reason) {
        self!output("not ok {$.current-test++} - $tag # TODO $reason");
    }
    method skip($reason) {
        self!output("ok {$.current-test++} # skip $reason");
    }

    method expected-tests($num) {
        self!output("1.." ~ $num);
    }

    method plan($x) {
        $!set-plan = 1;
        if $x ~~ Cool {
            self.expected-tests(+$x);
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

$GLOBAL::TEST-BUILDER = Builder.bless(*);
$GLOBAL::TEST-BUILDER.reset;

sub cmp_ok(\$a, $fn, \$b, $tag?) is export { ok($fn($a, $b), $tag); }
sub ok(\$bool, $tag?) is export { $*TEST-BUILDER.ok(?$bool, $tag) }
sub nok(\$bool, $tag?) is export { $*TEST-BUILDER.ok(!$bool, $tag) }
sub skip_rest($tag?) is export { } #OK
sub pass($tag?) is export { $*TEST-BUILDER.ok(1, $tag); True }
sub flunk($tag?) is export { $*TEST-BUILDER.ok(0, $tag) }
sub isa_ok(Mu $obj, Mu $type, $tag?) is export { $*TEST-BUILDER.ok($obj.^isa($type), $tag) }
sub is_deeply($a,$b,$c) is export { is $a.perl, $b.perl, $c }
sub is(Mu $got, Mu $expected, $tag?) is export {

    # avoid comparing twice
    my $equal = (~$got) eq (~$expected);

    $*TEST-BUILDER.ok($equal, $tag);
    if !$equal {
        $*TEST-BUILDER.note('   Failed test');
        $*TEST-BUILDER.note('          got: ' ~ ~$got);
        $*TEST-BUILDER.note('     expected: ' ~ ~$expected);
    }
}
sub isnt(Mu $got, Mu $expected, $tag?) is export { $*TEST-BUILDER.ok($got ne $expected, $tag) }
sub lives_ok($code,$why?) is export {
    my $lived = False;
    try { $code.(); $lived = True; }
    $*TEST-BUILDER.ok($lived, $why);
}
sub dies_ok($code,$why?) is export {
    my $lived = False;
    try { $code.(); $lived = True; }
    $*TEST-BUILDER.ok(!$lived, $why);
}
# bit of a hack: forces an inferior runloop to make CONTROL catchable
my class NoControlEval {
    method Bool() {
        my $rn = False;
        (sub () { eval $*code; $rn++ })();
        die "illegal return" unless $rn;
        True;
    }
}
sub eval_dies_ok($*code, $why?) is export {
    my $lived = False;
    try { ?NoControlEval; $lived = True; }
    $*TEST-BUILDER.ok(!$lived, $why);
}
sub eval_lives_ok($*code, $why?) is export {
    my $lived = False;
    try { ?NoControlEval; $lived = True; }
    $*TEST-BUILDER.ok($lived, $why);
}
sub diag($str) is export { $*TEST-BUILDER.note($str) }
sub is_approx(Mu $got, Mu $expected, $desc = '') is export {
    my $test = ($got - $expected).abs <= 1/100000;
    $*TEST-BUILDER.ok(?$test, $desc);
    unless $test {
        $*TEST-BUILDER.note("got:      $got");
        $*TEST-BUILDER.note("expected: $expected");
    }
    ?$test;
}
sub plan($num) is export { $*TEST-BUILDER.plan($num) }
sub done() is export { $*TEST-BUILDER.done }
sub skip($reason,$number) is export {
    $*TEST-BUILDER.skip($reason) for ^$number;
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


