class Stash is Hash {
    method new(*@pairs) {
        my $new = Q:CgOp { (box (@ {Stash}) (varhash_new)) };
        unitem($new) = @pairs;
        $new;
    }

    method id()    { self.<!id>.[0] // '???' }
    method idref() { self.<!id> }
    method file()  { self.<!file> }
    method line()  { self.<!line> }
}
