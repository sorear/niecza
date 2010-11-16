# vim: ft=perl6
use MONKEY_TYPING;

sub first(\$x) { for $x -> $elt { return $elt }; Any }
sub infix:<max>($a,$b) { $a < $b ?? $b !! $a }

augment class Mu {
    method typename() {  # should be ^name
        Q:CgOp { (box Str (obj_typename (@ {self}))) }
    }
}

augment class Cool {
    method comb($rx) {
        my $str = self.Str;
        my $C = Cursor.new($str);
        my $i = 0;
        my @out;
        while $i < $str.chars {
            my $M = first($rx($C.cursor($i++)));
            if $M {
                $i max= $M.to;
                push @out, $M.Str;
            } else {
            }
        }
        @out
    }
}

say "a b c".comb(/./).dump;
say "foo bar".comb(/\w+/).dump;
