class NAME {
    has $!guts;

    method new(*%foo) {
        my $new = self.CREATE;
        $new!guts = %foo;
        $new;
    }

    method at-key($k) { self!guts.at-key($k) }
    method delete-key($k) { self!guts.delete-key($k) }
    method exists-key($k) { self!guts.exists-key($k) }

    method name () { self.<name> }
    method xlex () { self.<xlex> }
    method olex () { self.<olex> }
    method of   () { self.<of>   }
    method file () { self.<file> }
    method line () { self.<line> }
}
