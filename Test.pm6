module Test; # XXX our due to the STD pad bug

constant $?TRANSPARENT = 1;

sub blame() {
    my $frame = caller;
    while $frame.hints('$?TRANSPARENT') {
        $frame = $frame.caller;
    }
    $frame.file ~ (" line " ~ $frame.line);
}

my $testnum = 1;
sub ok($bool, $tag) is export {
    my $not = (if $bool { "" } else { "not " });
    say ($not ~ ("ok " ~ ($testnum++ ~ (" - " ~ $tag))));
    if !$bool { say ("# " ~ blame()); }
}

sub plan($num) is export {
    say ("1.." ~ $num);
}
