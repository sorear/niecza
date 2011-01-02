class NAME is Hash {
    method new(*%foo) {
        my $new = Q:CgOp { (box (@ {NAME}) (varhash_new)) };
        unitem($new) = unitem(%foo);
        $new;
    }

    method name () { self.<name> }
    method xlex () { self.<xlex> }
    method olex () { self.<olex> }
    method of   () { self.<of>   }
    method file () { self.<file> }
    method line () { self.<line> }
}
