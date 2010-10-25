class STDStash { # does Associative
    has $!guts;

    method new(*@pairs) {
        my $new = self.CREATE;
        my %bits = @pairs;
        $new!guts = %bits;
        $new;
    }

    method at-key($str) { # is rw
        $!guts.at-key($str)
    }

    # XXX delegate delete-key, exists-key

    method id()    { $!guts.at-key('!id').at-pos(0) // '???' }
    method idref() { $!guts.at-key('!id') }
    method file()  { $!guts.at-key('!file') }
    method line()  { $!guts.at-key('!line') }
}
