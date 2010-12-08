use MONKEY_TYPING;
augment class List {
    method push(\|$args) { Q:CgOp {
        (letn iter (vvarlist_from_fvarlist (unbox fvarlist (@ {$args})))
              targ (getslot rest vvarlist (@ {self}))
          (vvarlist_shift (l iter))
          (ternary (== (i 0) (vvarlist_count (l targ)))
            (l targ (getslot items vvarlist (@ {self})))
            (prog))
          (whileloop 0 0 (iter_hasflat (l iter))
            (vvarlist_push (l targ) (vvarlist_shift (l iter))))
          {Nil})
    } }
}
my @arr;
my $i = 0;
while $i < 3162 { @arr.push($i); $i++ }
for @arr -> \$x {
    for @arr -> \$y {
    }
}
