my $fsw = CLR::<System.IO.FileSystemWatcher,System,PublicKeyToken=b77a5c561934e089>.new;

$fsw.Path = '.';

sub handler($, $event) {
    say "$event.ChangeType() ($event.Name())";
}

$fsw.add_Changed(&handler);
$fsw.add_Deleted(&handler);
$fsw.add_Created(&handler);

$fsw.EnableRaisingEvents = True;

+lines;
