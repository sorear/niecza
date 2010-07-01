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

# mnemonic characters: (@, !, =) fetch, store, lvalue.
# (l) lexical (L) raw lexical
grammar NIL is STD {
    rule nibbler { [ <insn> ]* }

    token category:insn { <sym> }
    proto token insn { <...> }

    token varid { [ <sigil> <twigil>? ]? <identifier> }

    token clrid { [ \w+ ] ** '.' <clrgeneric>? <clrqual>* }
    token clrgeneric { '<' <clrid> ** ',' '>' }
    token clrqual { '[]' }

    token num { \d+ }

    token up { '^' * }

    token insn:lextypes {
        'LEXICALS:' :s [ [ <varid> ] ** ',' ':' <clrid> ] ** ',' \n
    }

    token insn:string_lv { "=" <?before "'"> [ :lang(%*LANG<MAIN>) <quote> ] }
    token insn:clr_string { <?before "'"> [ :lang(%*LANG<MAIN>) <quote> ] }
    token insn:label { ':'  {} <num> }
    token insn:goto  { '->' {} <num> }

    token insn:lex_lv { 'l=' {} <up> <varid> }
    token insn:rawlexget { 'L@' {} <varid> }
    token insn:rawlexput { 'L!' {} <varid> }
    token insn:how { <sym> }
    token insn:fetchlv { '@' }
    token insn:dup_fetchlv { 'dup@' }
    token insn:pos { '=[' <?> ~ ']' <num> }
    token insn:clone_lex { 'CLONE:' :s [ <varid> ] ** ',' \n }
    token insn:copy_lex { 'COPY:' :s [ <varid> ] ** ',' \n }
    token insn:call_method { '.method/' {} <num> }
    token insn:call_sub { '.call/' {} <num> }
    token insn:tail_call_sub { '.tailcall/' {} <num> }
    token insn:unwrap { <sym> ':' {} <clrid> }
    token insn:new { <sym> '/' {} <num> ':' <clrid> }
    token insn:clr_field_get { '@.' {} <varid> }
    token insn:clr_field_set { '!.' {} <varid> }
    token insn:clr_index_get { '@[' {} <varid>? ']' }
    token insn:clr_index_set { '![' {} <varid>? ']' }
    token insn:cast { <sym> ':' {} <clrid> }
    token insn:clr_call_direct { '.plaincall/' {} <num> ':' <clrid> }
    token insn:return { <sym> '/' <[ 0 1 ]> }
    token insn:push_null { 'null:' {} <clrid> }
}

}
