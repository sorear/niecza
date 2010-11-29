class Stash { # does Associative
    has $!guts;

    method new(*@pairs) {
        my $new = self.CREATE;
        my %bits = @pairs;
        $new!guts = %bits;
        $new;
    }

    method hash() { unitem(self) }
    method keys() { $!guts.keys }
    method list() { $!guts.list }
    method flat() { $!guts.flat }
    method iterator () { self.list.iterator }

    method at-key($str) { $!guts.at-key($str) }
    method exists-key($str) { $!guts.exists-key($str) }
    method delete-key($str) { $!guts.delete-key($str) }

    method id()    { $!guts.at-key('!id').at-pos(0) // '???' }
    method idref() { $!guts.at-key('!id') }
    method file()  { $!guts.at-key('!file') }
    method line()  { $!guts.at-key('!line') }
}
