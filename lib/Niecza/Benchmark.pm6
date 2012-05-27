module Niecza::Benchmark;

my ($prologue, $baseline);

sub mktimer($code is copy) {
    $code = "$code ;" x 20;
    $code = '\qq[$prologue]; sub ($__j is copy) { my $__i = times[0]; (\qq[$code]) while --$__j; times[0] - $__i } #OK';
    eval $code;
}

sub fnum($n) { $n ~~ /:i e/ ?? $n !! substr($n,0,6) }

sub bench($code) is export {
    my $block = mktimer $code;

    my $runs = 1;
    $runs *= 2 until $block($runs) >= 0.2;

    say $code;

    my @times;
    my $total = 0;
    until $total >= 5 {
        push @times, $block($runs);
        $total += @times[*-1];
    }

    my $baseblock = $baseline * $runs; # using *loop* count

    $runs *= 20; # compensate for repetition above

    # mean and sd time per run-block, in secs
    my $mean = ($total / @times);
    my $sd = sqrt(([+] ((@times «-» $mean) «**» 2)) / (@times - 1));

    my $mean_µs = ($mean - $baseblock) / $runs * 1e6;
    my $sd_µs = ($sd / $runs) * 1e6;

    say "  ==> {fnum $mean_µs} ± {fnum $sd_µs} µs";
    say "      [{map &fnum, @times}] ($runs runs) - {fnum $baseblock}";
}

sub start is export ($p) {
    $prologue = $p;
    say 'prologue :: ', $p;
    $baseline = mktimer('')(1_000_001) / 1e6;
    bench '';
}
