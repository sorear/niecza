# vim: ft=perl6
use Test;

sub _rxalt($C, $lad, $k, *@alts) {
    sub lbody($ix) { @alts[$ix]($C, $k) }

    Q:CgOp {
        (letn csr   (unbox Cursor (@ (l $C)))
              lexer (rawnew Lexer (@ (l $C)) (clr_string "")
                                  (unwrap 'LAD[]' (@ (l $lad))))
              fates (rawcall (l lexer) Run (getfield backing (l csr))
                                           (getfield pos (l csr)))
              i     (int 0)
              nfate (getfield Length (l fates))
          (whileloop 0 0 (< (l i) (l nfate)) (prog
            (sink (subcall (@ (l &lbody))
                    (box Num (cast Double (getindex (l i) (l fates))))))
            (l i (+ (l i) (int 1)))))
          (null Variable))
    };
}

my grammar G5 {
    token a { foo }
    token b { foobar }
    token c { <a> | <b> }
    token d { <c> x }

    token e { x <e> x | y }

    token TOP { A <d> | E <e> }
}

ok G5.parse('Afoobarx'), 'LTM works even through subrules';
ok G5.parse('Exxyxx'), 'recursivity does not crash LTM';

done-testing;
