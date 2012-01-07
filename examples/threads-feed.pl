use Threads;
my $a <== map { print "a"; $_ }, 1..20;
for @($a) { print "b" };
$a.read.DESTROY;
say '.';
$a <== 1..20;
say map { $_ }, $a;
$a.read.DESTROY;
