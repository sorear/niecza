use warnings;
use strict;
use 5.010;
use utf8;

my %callers;
my %tcall;

my $i = 0;

while (<STDIN>) {
    chomp;
    unless ($i--) {
        $i = 10000;
        syswrite STDERR, ".";
    }
    my ($caller, $callee) = split /\t/;
    $callers{$callee}{$caller}++;
    $tcall{$callee}++;
}

syswrite STDERR, "\n";

my $all = 0;
for (values %tcall) { $all += $_; }

say "Total calls: $all";
say "";

for my $func (sort { $tcall{$b} <=> $tcall{$a} } keys %tcall) {
    say "Function: $func";
    my $t = $tcall{$func};
    my $c = $callers{$func};
    printf "Calls: %s (%.6f%%)\n", $tcall{$func}, 100 * $t / $all;
    print "Called from:\n";

    for my $caller (sort { $c->{$b} <=> $c->{$a} } keys %$c) {
        printf "    %30s %7d %.6f%%\n", $caller, $c->{$caller},
            100 * $c->{$caller} / $t;
    }
    print "\n";
}
