use STD;

class Niecza;
grammar Grammar is STD { # viv doesn't handle :: in definitions well atm

method p6class () { ::Niecza::Grammar::P6 }

grammar P6 is STD::P6 {
    method unitstart() {
        %*LANG<Q> = ::Niecza::Grammar::Q ;
        %*LANG<MAIN> = ::Niecza::Grammar::P6 ;
        self;
    }

    token statement_prefix:sym<PRE-INIT>
        { :my %*MYSTERY; <sym> <.spacey> <blast> <.explain_mystery> }
    token statement_control:sym<PRELUDE>
        { <sym> <.spacey> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:NIL))> }
}

grammar Q is STD::Q {
    #}

    multi method tweak(:$NIL!) { self.cursor_fresh( ::Niecza::Grammar::NIL ) }
}

grammar NIL is STD {
}

}
