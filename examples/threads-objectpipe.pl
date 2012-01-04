use Threads;
my $a = Threads::ObjectPipe.new();
Threads::Thread.new({ 
    for 1..100 -> $i { $a.put($i~'-'~time) }
});
for 1..5 -> $i {
    say $a.get() for 1..20;
    sleep 1
}
