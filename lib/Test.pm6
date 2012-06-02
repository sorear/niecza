my module Test;

constant $?TRANSPARENT = 1;

class Builder {
    has $!current-test;
    has $!set-plan;
    has $!todo-up-to = 0;
    has $!todo-reason;

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
        $!current-test = 1;
    }

    method note($m) {
        self!output("# " ~ $m);
        0;
    }

    method ok($bool, $tag) {
        my $not = $bool ?? "" !! "not ";
        my $desc;
        if $tag {
            $desc = " - " ~ $tag.subst('#', '\#').split("\n").join("\n#");
        } else {
            $desc = '';
        }
        if $!todo-up-to >= $!current-test {
            $desc ~= " # TODO $!todo-reason";
        }
        self!output($not ~ "ok " ~ $!current-test++ ~ $desc);
        if !$bool { self.note(self.blame); }
        $bool;
    }

    method todo($reason, $count) {
        $!todo-reason = $reason;
        $!todo-up-to = $!current-test + $count - 1; # todo(1) should stop after cur
    }

    method skip($reason) {
        self!output("ok {$!current-test++} # SKIP $reason");
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
            self!output("1.." ~ ($!current-test - 1));
        }
    }
}

INIT {
    $GLOBAL::TEST-BUILDER = Builder.bless(*);
    $GLOBAL::TEST-BUILDER.reset;
}

sub cmp_ok(\a, $fn, \b, $tag?) is export { ok($fn(a, b), $tag); }
sub ok(\bool, $tag?) is export { $*TEST-BUILDER.ok(?bool, $tag) }
sub nok(\bool, $tag?) is export { $*TEST-BUILDER.ok(!bool, $tag) }
sub skip_rest($tag?) is export { } #OK
sub pass($tag?) is export { $*TEST-BUILDER.ok(1, $tag); True }
sub flunk($tag?) is export { $*TEST-BUILDER.ok(0, $tag) }
sub isa_ok(Mu $obj, Mu $type, $tag?) is export { $*TEST-BUILDER.ok($obj.^isa($type), $tag) }
sub is_deeply($a,$b,$c) is export { is $a.perl, $b.perl, $c }
sub is(\got, \expected, $tag?) is export {

    # avoid comparing twice
    my $sgot  = ~(got // '');
    my $sexpexted = ~(expected // '');
    my $equal = $sgot eq $sexpexted;

    $*TEST-BUILDER.ok($equal, $tag);
    if !$equal {
        $*TEST-BUILDER.note('   Failed test');
        $*TEST-BUILDER.note("          got: $sgot");
        $*TEST-BUILDER.note("     expected: $sexpexted");
    }
}
sub isnt(Mu $got, Mu $expected, $tag?) is export { $*TEST-BUILDER.ok($got ne $expected, $tag) }
# Runs $code, trapping various failure modes and returning applicable.
sub no-control($code, :$diag) {
    my ($died, $warned);
    {
        CATCH   {
            $*TEST-BUILDER.note("Exception: $_") if $diag;
            default { $died = True }
        }
        CONTROL {
            if .[0] == 11 {
                $warned = True;
                return; # NIECZA - causes &warn to return
            }
            when .[0] != 11 { $died = True } # exits block
        }
        $code.();
    }
    $died ?? "die" !! $warned ?? "warn" !! "";
}
sub lives_ok($code,$why?) is export {
    $*TEST-BUILDER.ok(no-control($code, :diag) ne "die", $why);
}
sub dies_ok($code,$why?) is export {
    $*TEST-BUILDER.ok(no-control($code) eq "die", $why);
}
sub succeeds_ok($code,$why?,:$ignore = ()) is export {
    $*TEST-BUILDER.ok(?(no-control($code, :diag) eq any("", @$ignore)), $why);
}
sub fails_ok($code,$why?,:$expect = <die warn fail>) is export {
    $*TEST-BUILDER.ok(?(no-control($code) eq any(@$expect)), $why);
}
sub eval_lives_ok($code,$why?) is export {
    $*TEST-BUILDER.ok(no-control({ eval $code }, :diag) ne "die", $why);
}
sub eval_dies_ok($code,$why?) is export {
    $*TEST-BUILDER.ok(no-control({ eval $code }) eq "die", $why);
}
sub eval_dies_with_error($code, $error_pattern, $why?) is export {
    my $died_with_error;
    try {
        CATCH {
            $died_with_error = "$_" ~~ $error_pattern;
        }
        eval $code;
    }
    $*TEST-BUILDER.ok($died_with_error, $why);
}
sub eval_succeeds_ok($code,$why?,:$ignore = ()) is export {
    $*TEST-BUILDER.ok(?(no-control({ eval $code }) eq any("", @$ignore)), $why);
}
sub eval_fails_ok($code,$why?,:$expect = <die warn fail>) is export {
    $*TEST-BUILDER.ok(?(no-control({ eval $code }) eq any(@$expect)), $why);
}
sub diag($str) is export { $*TEST-BUILDER.note($str) }
sub is_approx(Mu $got, Mu $expected, $desc = '') is export {
    my $tol = $expected.abs < 1e-6 ?? 1e-5 !! $expected.abs * 1e-6;
    my $test = ($got - $expected).abs <= $tol;
    $*TEST-BUILDER.ok(?$test, $desc);
    unless $test {
        $*TEST-BUILDER.note("got:      $got");
        $*TEST-BUILDER.note("expected: $expected");
    }
    ?$test;
}
sub todo($reason="", $count = 1) is export { $*TEST-BUILDER.todo($reason, $count) }
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


