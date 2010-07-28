module Test; # XXX our due to the STD pad bug

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
        $frame.file ~ (" line " ~ $frame.line);
    }

    method _output($text) {
        say $text;
    }

    method reset() {
        $.current-test = 1;
    }

    method note($m) {
        self._output("# " ~ $m);
        0;
    }

    method ok($bool, $tag) {
        my $not = $bool ?? "" !! "not ";
        self._output($not ~ ("ok " ~ ($.current-test++ ~ (" - " ~ $tag))));
        if !$bool { self.note(self.blame); }
    }

    method expected-tests($num) {
        self._output("1.." ~ $num);
    }
}

$GLOBAL::TEST-BUILDER = Builder.CREATE;
$GLOBAL::TEST-BUILDER.reset;

sub ok($bool, $tag) is export { $*TEST-BUILDER.ok($bool, $tag) }
sub plan($num) is export { $*TEST-BUILDER.expected-tests($num) }
