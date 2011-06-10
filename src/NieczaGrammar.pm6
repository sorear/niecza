use STD;

grammar NieczaGrammar is STD;

grammar CgOp is STD {
    rule nibbler { <cgexp> }

    token category:cgexp { <sym> }
    proto token cgexp {*}

    token cgopname { <-[ ' " ( ) { } \[ \] \s ]> + }

    token cgexp:op { <[ ( \[ ]>:s {} <cgopname> [ <cgexp> ]* <[ ) \] ]> }
    token cgexp:name { <cgopname> }
    token cgexp:quote { <?before <[ ' " ]>> {} [ :lang(%*LANG<MAIN>) <quote> ] }
    token cgexp:decint { <decint> }
    token cgexp:p6exp { :lang(%*LANG<MAIN>) '{' ~ '}' <statementlist> }
    token cgexp:bad { <!before <[ ) \] ]> > {}
        [ <?stdstopper> <.panic: "Missing cgop"> ]
        <.panic: "Unparsable cgop">
    }
}

grammar Q is STD::Q { #} {
    method tweak(:$CgOp, *%_) {
        if $CgOp.defined { self.cursor_fresh(NieczaGrammar::CgOp) }
        else { nextwith(self, |%_) }
    }
}

grammar P6 is STD::P6 {
    method unitstart() {
        %*LANG<Q> = NieczaGrammar::Q ;
        %*LANG<MAIN> = NieczaGrammar::P6 ;
        self;
    }
}

method p6class () { NieczaGrammar::P6 }
