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

    method flat () {
        Q:CgOp {
            (letn n (obj_newblank (class_ref mo List))
              (iter_to_list (l n) (iter_flatten (unbox vvarlist (@ {self}))))
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

    method new() {
        Q:CgOp {
            (letn n (obj_newblank (obj_llhow (@ {self})))
              (setslot items (l n) (vvarlist_new_empty))
              (setslot rest  (l n) (vvarlist_new_empty))
              (newrwlistvar (l n)))
        };
    }

    method iterator () {
        Q:CgOp {
            (letn it (vvarlist_new_empty)
              (vvarlist_append (l it) (getslot items vvarlist (@ {self})))
              (vvarlist_append (l it) (getslot rest vvarlist (@ {self})))
              (box Iterator (l it)))
        };
    }

    method !count-items() { Q:CgOp {
        (box Num (cast num (vvarlist_count (getslot items vvarlist (@ {self})))))
    } }

    method !shift-item() { Q:CgOp {
        (vvarlist_shift (getslot items vvarlist (@ {self})))
    } }

    method !item-at-pos($ix) { Q:CgOp {
        (vvarlist_item (cast int (unbox num (@ {$ix}))) (getslot items vvarlist (@ {self})))
    } }

    method !pop-item() { Q:CgOp {
        (vvarlist_pop (getslot items vvarlist (@ {self})))
    } }

    method !push-iterator(\$x) { Q:CgOp {
        (rnull (vvarlist_push (getslot rest vvarlist (@ {self})) {$x}))
    } }

    method !push-item(\$x) { Q:CgOp {
        (rnull (vvarlist_push (getslot items vvarlist (@ {self})) {$x}))
    } }

    method !unshift-item(\$x) { Q:CgOp {
        (rnull (vvarlist_unshift (getslot items vvarlist (@ {self})) {$x}))
    } }
    method !fill ($count) { Q:CgOp {
        (letn ct (cast int (unbox num (@ {$count})))
              items (getslot items vvarlist (@ {self}))
              rest  (getslot rest  vvarlist (@ {self}))
          (whileloop 0 0
            (ternary (> (l ct) (vvarlist_count (l items)))
              (iter_hasarg (l rest)) (b 0))
            (vvarlist_push (l items) (vvarlist_shift (l rest))))
          (box Bool (<= (l ct) (vvarlist_count (l items)))))
    } }

    method eager () { self!fill(999_999_999); self }

    method flat () { self.iterator.flat }

    method at-pos($ix) {
        self!fill($ix+1) ?? self!item-at-pos($ix) !! Any
    }

    # TODO: use *@foo
    method push (\$v) {
        self.eager;
        self!push-item(anon $new = $v);
    }

    method unshift (\$v) {
        self!unshift-item(anon $new = $v);
    }
}

my class Num {
    has $!value;
    method Str () { Q:CgOp {
        (box Str (num_to_string (unbox num (@ {self}))))
    } }
    method Bool() { Q:CgOp {
        (box Bool (compare != (double 0) (unbox num (@ {self}))))
    } }
    method Numeric() { self }
    method dump() { self.Str }
}
sub infix:<+>($l,$r) { Q:CgOp {
    (box Num (+ (unbox num (@ {$l.Numeric})) (unbox num (@ {$r.Numeric}))))
} }

sub infix:<->($l,$r) { Q:CgOp {
    (box Num (- (unbox num (@ {$l.Numeric})) (unbox num (@ {$r.Numeric}))))
} }

sub infix:<*>($l,$r) { Q:CgOp {
    (box Num (* (unbox num (@ {$l.Numeric})) (unbox num (@ {$r.Numeric}))))
} }

sub infix:</>($l,$r) { Q:CgOp {
    (box Num (/ (unbox num (@ {$l.Numeric})) (unbox num (@ {$r.Numeric}))))
} }

sub infix:<< < >>($l,$r) { Q:CgOp {
    (box Bool (< (unbox num (@ {$l.Numeric})) (unbox num (@ {$r.Numeric}))))
} }

sub infix:<< > >>($l,$r) { Q:CgOp {
    (box Bool (> (unbox num (@ {$l.Numeric})) (unbox num (@ {$r.Numeric}))))
} }

sub infix:<< <= >>($l,$r) { Q:CgOp {
    (box Bool (<= (unbox num (@ {$l.Numeric})) (unbox num (@ {$r.Numeric}))))
} }

sub infix:<< >= >>($l,$r) { Q:CgOp {
    (box Bool (>= (unbox num (@ {$l.Numeric})) (unbox num (@ {$r.Numeric}))))
} }

sub infix:<< == >>($l,$r) { Q:CgOp {
    (box Bool (== (unbox num (@ {$l.Numeric})) (unbox num (@ {$r.Numeric}))))
} }

sub infix:<< != >>($l,$r) { Q:CgOp {
    (box Bool (!= (unbox num (@ {$l.Numeric})) (unbox num (@ {$r.Numeric}))))
} }

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

my class Array is List { }
my class Seq is List { }

my class EMPTY { }

my class GatherIterator is IterCursor {
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
                      {GatherIterator.RAWCREATE("frame", $*nextframe, "reify", Any)}))))
            }));
    }
}

sub _gather($fr) {
    &infix:<,>(GatherIterator.RAWCREATE("frame", $fr, "reify", Any)).list
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
    my @arr := &infix:<,>((&infix:<,>("a","b")), "c");
    say @arr.list.at-pos(1);
    say @arr.list.flat.at-pos(1);
}
