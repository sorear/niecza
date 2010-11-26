# vim: ft=perl6
use MONKEY_TYPING;
augment class Hash {
    method at-key($key) {
        my $ks ::= Q:CgOp { (obj_asstr {$ks}) };
        Q:CgOp {
            (letn vh (unbox varhash (@ {self}))
                  ky (unbox str (@ {$ks}))
              (ternary (varhash_contains_key (l vh) (l ky))
                (varhash_getindex (l ky) (l vh))
                {Any!Any::butWHENCE(sub (\$var) {
                  Q:CgOp {
                      (letn d [unbox varhash (@ {self})]
                            k [obj_getstr {$ks}]
                        [varhash_setindex (l k) (l d) {$var}]
                        [null var])
                  }})}))
        }
    }
}

augment class Array {
}

my $i = 0;
my %hash;
%hash{$i} = $i until ($i++) == 1000000;
