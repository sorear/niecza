# vim: ft=perl6
my class Mu {
    method defined() {
        Q:CgOp { (box Bool (obj_is_defined (@ {self}))) }
    }
    method RAWCREATE(\|$vars) { Q:CgOp {
        (letn ar  (unbox fvarlist (@ {$vars}))
              max (fvarlist_length (l ar))
              i   (int 1)
              obj (obj_newblank (obj_llhow (@ (fvarlist_item (i 0) (l ar)))))
          [whileloop 0 0 (< (l i) (l max)) (prog
              [setslot
                (unbox str (@ (fvarlist_item (l i) (l ar)))) (l obj)
                (nsw (@ (fvarlist_item (+ (l i) (int 1)) (l ar))))]
              [l i (+ (l i) (int 2))])]
          [ns (l obj)])
    } }
}

my class Any is Mu {
    method list () { self.iterator.list }
}

my class CallFrame { }

my class Bool {
    has $!value;
    method Bool () { self }
}

my class Sub {
    has $!outer;
    has $!info;
}

my class Iterator {
    has $!value; # is vvarlist

    method list () {
        Q:CgOp {
            (letn n (obj_newblank (class_ref mo List))
              (iter_to_list (l n) (unbox vvarlist (@ {self})))
              (newrwlistvar (l n)))
        }
    }
}

my class IterCursor {
}

sub infix:<=>(\$a, \$b) { Q:CgOp { (prog [assign {$a} {$b}] {$a}) } }

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
            (box Iterator (vvarlist_from_fvarlist (unbox fvarlist (@ {self})) (i 0)))
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

my class EMPTY { }

my class GatherItCursor is IterCursor {
    has $!frame;
    has $!reify;

    method reify() {
        my $*nextframe;
        $!reify // ($!reify = (
            Q:CgOp {
                (letn getv (cotake (cast frame (@ {$!frame})))
                  (box Parcel (ternary (== (@ {EMPTY}) (@ (l getv)))
                    (fvarlist_new)
                    (fvarlist_new
                      (l getv)
                      {GatherItCursor.RAWCREATE("frame", $*nextframe, "reify", Any)}))))
            }));
    }
}

sub _gather($fr) {
    &infix:<,>(GatherItCursor.RAWCREATE("frame", $fr, "reify", Any)).list
}

my class Str {
    has $!value;
    method Str () { self }
}

sub say($str) { Q:CgOp {
    (rnull [say (unbox str (@ {$str}))])
} }
sub take($p) { # should be \|$p
    Q:CgOp { (take (l $p)) }
}

{
    my $*stub;
    my @arr := gather { say "A"; take "B"; say "C"; take "D"; say "E" };
    &infix:<,>("x",@arr,"y",@arr,"z").for(&say);
}
