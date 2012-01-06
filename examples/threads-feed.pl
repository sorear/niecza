use Threads; my $a <== map { print "a"; $_ }, 1..20; print "b" while (shift @($a)); $a.read.DESTROY;
