class STDStash { # does Associative
    has $!guts;

    method new(*%bits) {
        self.CREATE(guts => %bits);
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
