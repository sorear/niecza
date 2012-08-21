
class IO::Socket::INET {
    has $!sock;
    has Str $!buffer = '';

    # TODO: This code is horribly broken, especially if a character gets cut by
    # a buffer or packet boundry, or you want to switch from char to binary mode
    # doing it right seems to require some kind of Decoder class

    method recv (Cool $chars = Inf) {
        die('Socket not available') unless $!sock;

        if $!buffer.chars < $chars {
            $!buffer ~= self.read(2048).decode('UTF-8');
        }

        if $!buffer.chars > $chars {
            my $rec  = $!buffer.substr(0, $chars);
            $!buffer = $!buffer.substr($chars);
            $rec
        } else {
            my $rec = $!buffer;
            $!buffer = '';
            $rec;
        }
    }

    method read(IO::Socket::INET:D: Cool $bufsize) {
        die('Socket not available') unless $!sock;
        Q:CgOp { (box Buf (socket_read (unbox socket {$!sock}) (unbox int {$bufsize.Int}))) }
    }

    #method poll(Int $bitmask, $seconds) {
    #    $!sock.poll(
    #        nqp::unbox_i($bitmask), nqp::unbox_i($seconds.floor),
    #        nqp::unbox_i((($seconds - $seconds.floor) * 1000).Int)
    #    );
    #}

    method send (Cool $string) {
        self.write($string.encode('UTF-8'));
    }

    method write(Buf:D $buf) {
        die('Socket not available') unless $!sock;
        Q:CgOp { (rnull (socket_write (unbox socket {$!sock}) (unbox blob {$buf}))) }
    }

    method close () {
        die("Not connected!") unless $!sock;
        Q:CgOp { (rnull (socket_close (unbox socket {$!sock}))) };
        $!sock = Any;
    }

    my module sock {
        # XXX these constants are backend-sensitive.
        constant PF_LOCAL       = 0;
        constant PF_UNIX        = 1;
        constant PF_INET        = 2;
        constant PF_INET6       = 0x17;
        #constant PF_MAX         = 4;
        #constant SOCK_PACKET    = 0;
        constant SOCK_STREAM    = 1;
        constant SOCK_DGRAM     = 2;
        constant SOCK_RAW       = 3;
        constant SOCK_RDM       = 4;
        constant SOCK_SEQPACKET = 5;
        constant SOCK_MAX       = 6;
        constant PROTO_TCP      = 6;
        constant PROTO_UDP      = 17;
    }

    has Str $.host;
    has Int $.port = 80;
    has Str $.localhost;
    has Int $.localport;
    has Bool $.listen;
    has $.family = sock::PF_INET;
    has $.proto = sock::PROTO_TCP;
    has $.type = sock::SOCK_STREAM;
    has Str $.input-line-separator is rw = "\n";
    has Int $.ins = 0;

    my sub v4-split($uri) {
        return $uri.split(':', 2);
    }

    my sub v6-split($uri) {
        my ($host, $port) = ($uri ~~ /^'[' (.+) ']' \: (\d+)$/)[0,1];
        return $host ?? ($host, $port) !! $uri;
    }

    method new (*%args is copy) {
        die "Nothing given for new socket to connect or bind to" unless %args<host> || %args<listen>;

        if %args<host>  {
            my ($host, $port) = %args<family> && %args<family> == sock::PF_INET6()
                ?? v6-split(%args<host>)
                !! v4-split(%args<host>);
            if $port {
                %args<port> //= $port;
                %args<host> = $host;
            }
        }
        if %args<localhost> {
            my ($peer, $port) = %args<family> && %args<family> == sock::PF_INET6()
                ?? v6-split(%args<localhost>)
                !! v4-split(%args<localhost>);
            if $port {
                %args<localport> //= $port;
                %args<localhost> = $peer;
            }
        }

        %args<listen>.=Bool if %args<listen> :exists;

        #TODO: Learn what protocols map to which socket types and then determine which is needed.
        self.bless(*, |%args)!initialize()
    }

    method !initialize() {
        $!sock = Q:CgOp { (box Any (socket_new (unbox int {$.family}) (unbox int {$.type}) (unbox int {$.proto}))) };

        #Quoting perl5's SIO::INET:
        #If Listen is defined then a listen socket is created, else if the socket type,
        #which is derived from the protocol, is SOCK_STREAM then connect() is called.
        if $.listen || $.localhost || $.localport {
            Q:CgOp { (rnull (socket_bind (unbox socket {$!sock}) (unbox str {$.localhost || "0.0.0.0"}) (unbox int {$.localport || 0}))) };
        }

        if $.listen {
            Q:CgOp { (rnull (socket_listen (unbox socket {$!sock}) (unbox int {20}))) };
        }
        elsif $.type == sock::SOCK_STREAM {
            Q:CgOp { (rnull (socket_connect (unbox socket {$!sock}) (unbox str {$.host}) (unbox int {$.port}))) };
        }

        self;
    }

    method get() {
        ++$!ins;
        my $inbuf = '';
        my $irs = $!input-line-separator;
        my $irslen = chars($irs);
        until substr($inbuf, chars($inbuf)-$irslen, $irslen) eq $irs {
            $inbuf ~= (self.recv(1) || return $inbuf);
        }
        substr($inbuf, 0, chars($inbuf)-$irslen);
    }

    method lines() {
        gather { take self.get() };
    }

    method !setsock($ns) {
        $!sock = $ns;
        $!buffer = '';
        self;
    }

    method accept() {
        my $new_sock := self.WHAT.bless(*, :$!family, :$!proto, :$!type);
        $new_sock!setsock( Q:CgOp { (box Any (socket_accept (unbox socket {$!sock}))) } );
    }

    #method remote_address() {
    #    return nqp::p6box_s(nqp::getattr(self, $?CLASS, '$!sock').remote_address());
    #}

    #method local_address() {
    #    return nqp::p6box_s(nqp::getattr(self, $?CLASS, '$!sock').local_address());
    #}
}

my $sock = IO::Socket::INET.new( localport => 9999, :listen );
while $sock.accept -> $new {
    say "<< $new.get() >>";
    $new.close;
}
