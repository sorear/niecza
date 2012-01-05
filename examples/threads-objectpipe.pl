use Threads;
my ($read, $write) = objectpipe;
my $t = Threads::Thread.new({
    for 1..200 -> $i { $write.put($i~'-'~time); };
    CATCH { default { say "pipe closed..." } }
});
{
    for 1..5 {
        say $read.get() for 1..20;
        sleep 1
    }
    $read.DESTROY();
    $read = Nil;
}
$t.join();
