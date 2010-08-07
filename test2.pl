# vim: ft=perl6
use Test;

PRE-INIT {
    Any.HOW.add-method('!butWHENCE', anon method !butWHENCE($cr) {
        Q:CgOp { (rawnew Variable (bool 1) (bool 1) (bool 0) (@ (l $cr))
                   (rawscall Kernel.MakeSC (@ (l self)))) }
    });
}

sub infix:<:=> is rawcall { Q:CgOp {
    (prog [bind (bool 0) (pos 0) (pos 1)] [pos 0]) } }
sub infix:<::=> is rawcall { Q:CgOp {
    (prog [bind (bool 1) (pos 0) (pos 1)] [pos 0]) } }

my $a = 0;
my $b = 0;
my $c = 0;
my $d = 0;
my $e = 0;
my $f = 0;

(Any!butWHENCE({ $a = 1 }));
my $x := (Any!butWHENCE({ $b = 1 }));
my $y ::= (Any!butWHENCE({ $c = 1 }));
my $z = (Any!butWHENCE({ $d = 1 }));
(Any!butWHENCE({ $e = 1 })) = 2;
(Any!butWHENCE({ $f = 1 })) := 3;

ok !$a, "no autovivification in void context";
ok $b, "autovivification after rw bind";
ok !$c, "no autovivification after ro bind";
ok !$d, "no autovivification after rvalue context";
ok $e, "autovivification after lvalue context";
ok $f, "autovivification after bvalue context";

done-testing;
