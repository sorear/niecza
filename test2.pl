# vim: ft=perl6
use Test;

PRE-INIT {
    Any.HOW.add-method("at-pos", anon method at-pos($ix) {
        ($ix == 0) ?? self !! die("Invalid index on non-list")
    });

    Array.HOW.add-method("!extend", anon method !extend is rawcall {
        Q:CgOp {
            (letn i (unbox List<Variable> (getattr items (@ (pos 0))))
                  ct (- (cast Int32 (unbox Double (@ (pos 1))))
                        (getfield Count (l i)))
              (ternary (>= (l ct) (int 0)) [prog]
                [die "Autovivification collision"])
              (whileloop 0 0 (!= (l ct) (int 0))
                (prog
                  (l ct (- (l ct) (int 1)))
                  (rawcall (l i) Add (newrwscalar (@ (l Any))))))
              (rawcall (l i) Add (pos 2))
              (null Variable))
        };
    });

    Array.HOW.add-method("at-pos", anon method at-pos($ix) {
        self!fill($ix+1)
            ?? $!items.at-pos($ix)
            !! Any!butWHENCE(sub () is rawcall {
                self!extend($ix, Q:CgOp { (pos 0) });
            });
    });
}

sub postcircumfix:<[ ]> is rawcall {
    my $index ::= Q:CgOp { (pos 1) };

    (Q:CgOp { (pos 0) }).defined
        ?? (Q:CgOp { (pos 0) }).at-pos($index)
        !! Any!butWHENCE(sub () is rawcall {
            my $ar := Q:CgOp { (getindex (int 0) (getfield pos
                                 (getfield outer (callframe)))) };
            $ar.defined && die("Autovivification collision");
            $ar = Array.new;
            $ar!extend($index, Q:CgOp { (pos 0) });
        });
}

{
    sub postcircumfix:<[ ]>($a, $b, $c) { $a ~ "|" ~ $b ~ "|" ~ $c }
    is 1[2,3], "1|2|3", "can call postcircumfix [ ]";
}

{
    sub postcircumfix:<{ }>($a, $b, $c) { $a ~ "|" ~ $b ~ "|" ~ $c }
    is 1{2,3}, "1|2|3", 'can call postcircumfix { }';
}

my @arr = <a b c>;
is @arr.join("|"), 'a|b|c', "word splitter works";

my @narr;
@narr[0];
ok +@narr == 0, "rvalue reference to out of range value does not add";
@narr[2] = 5;
ok +@narr == 3, "assigning to element 2 makes length 3";
ok !(@narr[0].defined), "first element undefined";
ok !(@narr[1].defined), "second element undefined";
ok @narr[2] == 5, "third element properly assigned";

my @darr;
@darr[1][1];
ok +@darr == 0, "rvalue nested reference, no effect";
@darr[2][2] = 'pie';
ok +@darr == 3, "outer level vivifies elements";
ok @darr[2] ~~ Array, "inner Array created";
is @darr[2][2], 'pie', "inner value retained";

done-testing;
