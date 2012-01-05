use Threads;
my ($read, $write) = objectpipe;
{
    Threads::Thread.new({
        for 1..200 -> $i { $write.put($i~'-'~time); };
        CATCH { when Str { .say } }
    });
}
{
    for 1..5 {
        say $read.get() for 1..20;
        sleep 1
    }
    $read.DESTROY();
    $read = Nil;
}
