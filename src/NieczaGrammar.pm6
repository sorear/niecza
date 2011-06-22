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
        my $top = $*unit.setting_ref;
        my $rtop = $top && $*unit.deref($top);
        $*CURLEX{'!sub'} = ::Metamodel::StaticSub.new(
            unit => $*unit,
            outerx => $top,
            cur_pkg => $*unit.abs_pkg('GLOBAL').xref,
            name => "mainline",
            run_once => !$rtop || $rtop.run_once);
        $*CURLEX{'!sub'}.add_my_name('$_') if !$top;
        $*CURLEX{'!sub'}.add_hint('$?FILE');
        $*CURLEX{'!sub'}.signature = ::GLOBAL::Sig.simple();
        $*unit.mainline = $*CURLEX<!sub>;

        %*LANG<Q> = ::NieczaGrammar::Q ;
        %*LANG<MAIN> = ::NieczaGrammar::P6 ;

        my $h = self;
        loop (my $C = $*CURLEX<!sub>; $C && $C.unit.name ne 'CORE'; $C.=outer) {
            for $C.lexicals.keys -> $lex {
                $h.check_categorical($lex);
                $h = $h.cursor_fresh(%*LANG<MAIN>);
            }
        }

        self;
    }
}

method p6class () { NieczaGrammar::P6 }
