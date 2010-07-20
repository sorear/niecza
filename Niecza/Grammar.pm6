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
}

grammar Q is STD::Q {
    #}

    multi method tweak(:$CgOp!) { self.cursor_fresh( ::Niecza::Grammar::CgOp)}
}

# an OPP is planned, but I need a better way to write action methods first,
# since the OPP would necessarily use the same rule names as STD::P6
grammar CgOp is STD {
    rule nibbler { <cgexp> }

    token category:cgexp { <sym> }
    proto token cgexp { <...> }

    token cgopname { <-[ ' " ( ) \[ \] \s ]> + }

    token cgexp:op { <[ ( \[ ]>:s {} <cgopname> [ <cgexp> ]* <[ ) \] ]> }
    token cgexp:name { <cgopname> }
    token cgexp:quote { <?before <[ ' " ]>> {} [ :lang(%*LANG<MAIN>) <quote> ] }
    token cgexp:decint { <decint> }
    token cgexp:bad { <!before <[ ) \] ]> > {}
        [ <?stdstopper> <.panic "Missing cgop"> ]
        <.panic: "Unparsable cgop">
    }
}

}

# vim: ft=
