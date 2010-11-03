# vim: ft=perl6
my class Mu {
}

my class Any is Mu {
}

my class Sub {
    has $!outer;
    has $!info;
}

my class Iterator {
    has $!value; # is vvarlist
}

my class IterCursor {
}

constant Nil = Any;

my class List {
    has $!items;
    has $!rest;

    method iterator () {
        Q:CgOp {
            (letn it (vvarlist_new_empty)
              (vvarlist_append (l it) (getslot items vvarlist (@ {self})))
              (vvarlist_append (l it) (getslot rest vvarlist (@ {self})))
              (box Iterator (l it)))
        };
    }

    method push ($val) {
        Q:CgOp {
            (rnull
              (vvarlist_push (getslot items vvarlist (@ {self})) {$val}))
        };
    }
}

my class Parcel {
    has $!value;

    method iterator () {
        Q:CgOp {
            (box Iterator (iter_fromparcel (unbox fvarlist (@ {self})) (i 0)))
        };
    }

    method for (&cb) {
        Q:CgOp {
            (letn it (unbox vvarlist (@ {self.iterator}))
                  cb (@ {&cb})
              (whileloop 0 0 (iter_hasflat (l it))
                (sink (subcall (l cb) (vvarlist_shift (l it)))))
              {Nil})
        };
    }
}

sub infix:<,>(\|@p) { @p }

my class Array is List {
    method new() {
        Q:CgOp {
            (letn n (obj_newblank (class_ref mo Array))
              (setslot items (l n) (vvarlist_new_empty))
              (setslot rest  (l n) (vvarlist_new_empty))
              (newrwlistvar (l n)))
        };
    }
}


{
    my $*stub;
    my @arr;
    sub twice(&f) { -> { f; f } }
    twice(twice(twice({ @arr.push(Any) })))();

    &infix:<,>(@arr).for( -> $x {
        &infix:<,>(@arr).for( -> @y {
            Q:CgOp { (rnull (say (s "Hello"))) };
        })
    })
}
