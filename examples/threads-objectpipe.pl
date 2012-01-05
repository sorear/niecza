use Threads;
my ($read, $write) = objectpipe;
my $t = Threads::Thread.new({
    my $last = 0;
    for 1..200 -> $i { $write.put($i~'-'~time); $last = $i; };
    CATCH { default { say "pipe closed... sent $last items" } }
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
say "compare the number of the last consumed item, with the last sent item."
