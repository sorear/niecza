my $sock = IO::Socket::INET.new( localport => 9999, :listen );
while $sock.accept -> $new {
    say "<< $new.get() >>";
    $new.close;
}
