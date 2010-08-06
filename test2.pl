# vim: ft=perl6
use Test;

# exactly like List, but flattens, and with "is copy" semantics on stuff
my class Seq is List {
    method !elem($x) { my $y = $x; $y }
    method Seq { self }
}

my class Array is List {
    method new() {
        Array.RAWCREATE("flat", 1, "items", LLArray.new, "rest", LLArray.new);
    }
}

PRE-INIT {
    Q:CgOp { (prog (rawsset Kernel.ArrayP (@ (l Array))) (null Variable)) };

    List.HOW.add-method("Seq", anon method Seq() {
        Seq.RAWCREATE("flat", 1, "items", LLArray.new,
            "rest", LLArray.new(self.iterator));
    });
    List.HOW.add-method("Numeric", anon method Numeric () { self.elems });
    List.HOW.add-method("push", anon method push(*@items) {
        $!rest.push(@items.Seq.eager.iterator)
    });
    List.HOW.add-method("pop", anon method pop() {
        self.eager;
        $!items.pop;
    });
}

sub prefix:<+>($x) { $x.Numeric }

my @x;

ok @x ~~ Array, '@x isa Array';
ok @x.elems == 0, 'no elements';
ok +@x == 0, 'no elements (+)';

@x.push(5);
ok @x.elems == 1, 'one element now';
ok @x.shift == 5, 'element removed is 5';
@x.push(7,8);
ok @x.elems == 2, "added two elements";
ok @x.shift == 7, "removed first correctly";
ok @x.shift == 8, "removed second correctly";
ok @x.elems == 0, "no elements again";

my $k = 2;
@x.push($k);
$k = 3;
ok @x.shift == 2, "push copies";

@x.push(11,12);
ok @x.pop == 12, "pop is LIFO (1)";
ok @x.pop == 11, "pop is LIFO (2)";
ok +@x == 0, "pop removed all elements";

done-testing;
