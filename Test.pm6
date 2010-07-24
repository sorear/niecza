my module Test;

my $testnum = 1;
sub ok($bool, $tag) is export {
    my $not = (if $bool { "" } else { "not " });
    say ($not ~ ("ok " ~ ($testnum++ ~ (" - " ~ $tag))));
}

sub plan($num) is export {
    say ("1.." ~ $num);
}
