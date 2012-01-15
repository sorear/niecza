our ($Op, $OpAttribute, $OpBareBlock, $OpBuiltin, $OpCallLike, $OpCallMethod,
     $OpCallSub, $OpCatchyWrapper, $OpCgOp, $OpConditional, $OpConstantDecl,
     $OpContextVar, $OpDoOnceLoop, $OpForLoop, $OpGather, $OpGeneralConst,
     $OpGeneralLoop, $OpGetBlock, $OpGetSlot, $OpHereStub, $OpImmedForLoop,
     $OpIndirectVar, $OpLabelled, $OpLetVar, $OpLexical, $OpMakeCursor, $OpNum,
     $OpParen, $OpRegexBody, $OpRequire, $OpShortCircuit, $OpSimplePair,
     $OpSimpleParcel, $OpStart, $OpStateDecl, $OpStatementList,
     $OpStringLiteral, $OpTemporize, $OpTry, $OpWhatever, $OpWhateverCode,
     $OpWhen, $OpWhileLoop, $OpYada, $OpYouAreHere);

our ($RxOp, $RxOpAlt, $RxOpAny, $RxOpBefore, $RxOpCut, $RxOpConj, $RxOpCutLTM,
     $RxOpCutBrack, $RxOpCutRule, $RxOpConfineLang, $RxOpCapturing,
     $RxOpCClassElem, $RxOpCheckBlock, $RxOpEndpoint, $RxOpListPrim,
     $RxOpNone, $RxOpNotBefore, $RxOpNewline, $RxOpProtoRedis, $RxOpQuantifier,
     $RxOpSubrule, $RxOpString, $RxOpSequence, $RxOpSigspace, $RxOpSeqAlt,
     $RxOpSaveValue, $RxOpStringCap, $RxOpSym, $RxOpStatement, $RxOpSetLang,
     $RxOpTilde, $RxOpVoidBlock, $RxOpVarString, $RxOpZeroWidth,
     $RxOpZeroWidthCCs);

our ($Operator, $Operator_Method, $Operator_Replicate, $Operator_FlipFlop,
     $Operator_SmartMatch, $Operator_Comma, $Operator_Binding,
     $Operator_ShortCircuit, $Operator_Ternary, $Operator_Temp,
     $Operator_DotEq, $Operator_Mixin, $Operator_Let, $Operator_PostCall,
     $Operator_Function, $Operator_CompoundAssign);

our ($CgOp, $CClass, $Sig, $SigParameter, $OptRxSimple, $OptBeta, $Actions);

class NieczaActions;

use OpHelpers;

our $CCTrace = %*ENV<NIECZA_CC_TRACE> // False;

# XXX Niecza  Needs improvement
method sym_categorical($/) { self.FALLBACK($<name>, $/) }
method bracket_categorical($/) { self.FALLBACK($<name>, $/) }
method FALLBACK($meth, $/) {
    my $S = $<sym>;

    if substr($meth,0,7) eq 'prefix:' {
        make $Operator.funop($/, q:s'&prefix:<$S>', 1);
    } elsif substr($meth,0,14) eq 'postcircumfix:' {
        make $Operator.funop($/, q:s'&postcircumfix:<$S>', 1, @( $<semilist>.ast ));
    } elsif substr($meth,0,10) eq 'circumfix:' {
        make mkcall($/, q:s'&circumfix:<$S>', @( $<semilist>.ast ));
    } elsif substr($meth,0,8) eq 'postfix:' {
        make $Operator.funop($/, q:s'&postfix:<$S>', 1);
    } elsif substr($meth,0,6) eq 'infix:' {
        make $Operator.funop($/, q:s'&infix:<$S>', 2);
    } elsif substr($meth,0,5) eq 'term:' {
        make mkcall($/, q:s'&term:<$S>');
    } else {
        $/.CURSOR.sorry("Action method $meth not yet implemented");
    }
}

method ws($ ) { }
method alpha($ ) { }
method is_ok($ ) { }
method dumbsmart($ ) { }
method normspace($ ) { }
method vws($ ) { }
method unv($ ) { }
method begid($ ) { }
method comment($ ) { }
method comment:sym<#>($ ) { }
method comment:sym<#`(...)>($ ) { }
method opener($ ) { }
method starter($ ) { }
method keyspace($ ) { }
method spacey($ ) { }
method unspacey($ ) { }
method unsp($ ) { }
method nofun($ ) { }
method curlycheck($ ) { }
method pod_comment($ ) { }
method infixstopper($ ) { }
method vnum($ ) { }
method version($ ) { }
method version:sym<v>($ ) { }

method category($ ) { }
method category:category ($ ) { }
method category:sigil ($ ) { }
method category:twigil ($ ) { }
method category:special_variable ($ ) { }
method category:comment ($ ) { }
method category:version ($ ) { }
method category:module_name ($ ) { }
method category:value ($ ) { }
method category:term ($ ) { }
method category:strtonum ($ ) { }
method category:quote ($ ) { }
method category:prefix ($ ) { }
method category:infix ($ ) { }
method category:postfix ($ ) { }
method category:dotty ($ ) { }
method category:circumfix ($ ) { }
method category:postcircumfix ($ ) { }
method category:quote_mod ($ ) { }
method category:trait_mod ($ ) { }
method category:type_declarator ($ ) { }
method category:scope_declarator ($ ) { }
method category:package_declarator ($ ) { }
method category:multi_declarator ($ ) { }
method category:routine_declarator ($ ) { }
method category:regex_declarator ($ ) { }
method category:statement_prefix ($ ) { }
method category:statement_control ($ ) { }
method category:statement_mod_cond ($ ) { }
method category:statement_mod_loop ($ ) { }
method category:infix_prefix_meta_operator ($ ) { }
method category:infix_postfix_meta_operator ($ ) { }
method category:infix_circumfix_meta_operator ($ ) { }
method category:postfix_prefix_meta_operator ($ ) { }
method category:prefix_postfix_meta_operator ($ ) { }
method category:prefix_circumfix_meta_operator ($ ) { }
method category:terminator ($ ) { }
method category:metachar ($ ) { }
method category:backslash ($ ) { }
method category:assertion ($ ) { }
method category:quantifier ($ ) { }
method category:mod_internal ($ ) { }

method sign($ ) { }

sub from_base($str, $base) {
    my $acc = 0;
    my $punto = -1;
    for $str.lc.comb -> $ch {
        next if $ch eq '_';
        if $ch eq '.' { $punto = 0; next; }
        $punto++ if $punto >= 0;
        $acc = $acc * $base + ($ch ge 'a' ?? ord($ch) - 87 !! ord($ch) - 48);
    }
    $punto >= 0 ?? $acc / ($base ** $punto) !! $acc
}

method decint($/) { make from_base($/, 10) }
method hexint($/) { make from_base($/, 16) }
method octint($/) { make from_base($/, 8) }
method binint($/) { make from_base($/, 2) }
method integer($/) {
    $<decint> andthen make $<decint>.ast;
    $<octint> andthen make $<octint>.ast;
    $<hexint> andthen make $<hexint>.ast;
    $<binint> andthen make $<binint>.ast;
}

method decints($/) { make [ map *.ast, @$<decint> ] }
method hexints($/) { make [ map *.ast, @$<hexint> ] }
method octints($/) { make [ map *.ast, @$<octint> ] }
method binints($/) { make [ map *.ast, @$<binint> ] }

method escale ($/) { }
method dec_number ($/) {
    if $<escale> { make +((~$/).comb(/<-[_]>/).join("")) }
    else { make from_base($/, 10) }
}

method alnumint ($/) { }
method radint($/) {
    $<rad_number> && make $<rad_number>.ast;
    $<integer> && make $<integer>.ast;
}
method rad_number ($/) {
    if $<circumfix> {
        $/.CURSOR.sorry("Runtime base conversions NYI");
        make 0;
        return;
    }
    my $radix = +$<radix>;
    my $value = $<int> ?? from_base($<int>, $radix) !! 0;
    if $<frac> -> $fr {
        my $shift = $fr.chars - $fr.comb(/_/);
        $value += (from_base($fr, $radix) / ($radix ** $shift));
    }
    if $<base> {
        $value = $value * $<base>.ast ** ($<exp> ?? $<exp>.ast !! 0);
        $value = $value.Num; # exponential notation is always imprecise here
    }
    make $value;
}

method number($/) {
    my $child = $<integer> // $<dec_number> // $<rad_number>;
    if !defined $child {
        make $/ eq 'NaN' ?? (0e0/0e0) !! Inf;
    } else {
        given $child.ast {
            when Num { make $_ }
            when Int { make [ 10, ~$_ ] }
            when Rat { make [ 10, "{.numerator}/{.denominator}" ] }
        }
    }
}

method charname($/) {
    if $<radint> {
        if $<radint>.ast !~~ (Int & 0 .. 0x10FFFF) {
            $/.CURSOR.sorry("Numeric character identifiers must be integers between 0 and 0x10FFFF");
            make ' ';
        } else {
            make chr($<radint>.ast);
        }
    } else {
        my $chr = ' ';
        $/.CURSOR.trymop({ $chr = $*backend.get_codepoint(~$/) });
        make $chr;
    }
}
method charnames($/) { make join "", map *.ast, @$<charname> }
method charspec($/) {
    if $<charnames> { make $<charnames>.ast }
    else {
        my $str = ~$/;
        if do { my $/; $str ~~ /^\d/ } {
            make chr(+$str);
        } else {
            make chr(ord($str) +& 31);
        }
    }
}

# Value :: Op
method value($ ) { }
method value:number ($/){ make $OpNum.new(pos=>$/, value => $<number>.ast)}
method value:quote ($/) { make $<quote>.ast }

method ident($ ) { }
method identifier($ ) { }
method label($/) {
    $/.CURSOR.trymop({
        $*CURLEX<!sub>.add_label(~$<identifier>, |mnode($/));
    });
    make ~$<identifier>;
}

# Either String Op
method morename($/) {
    make ($<identifier> ?? ~$<identifier> !! $<EXPR> ?? $<EXPR>.ast !! Any);
}

method get_Any() { $*CURLEX<!sub>.compile_get_pkg('CORE', 'Any') }
method post_constraint($/) { }
method type_constraint($/) {
    if $<value> {
        my $val = $<value>.ast.const_value;
        $val // $/.CURSOR.sorry("Value constraint is not constant");
        make { value => $val // $OpNum.new(value => 0).const_value };
    } elsif $<typename> {
        make $<typename>.ast;
    } else {
        make { where => $<EXPR>.ast };
    }
}

method typename($/) {
    constant %masks = ':_' => $Sig::ANY_DEF, ':T' => $Sig::UNDEF_ONLY,
        ':U' => $Sig::UNDEF_ONLY, ':D' => $Sig::DEF_ONLY;

    $/.CURSOR.sorry('WHENCE blocks not allowed on declarative type names')
        if $<whence>;

    if $<ident> {
        $/.CURSOR.sorry('::?CLASS syntax NYI');
        make { tmode => 0, type => self.get_Any };
        return;
    }

    my $tmode = 0;
    my $long = $<longname>;
    my ($type) = self.process_name($long);

    if !$type {
        $/.CURSOR.sorry('A type must be provided');
        $type = self.get_Any;
    }

    if $<typename> {
        $/.CURSOR.sorry('Coercive declarations NYI');
    }

    for @( $long<colonpair> ) -> $cp {
        if %masks{$cp} -> $mask {
            $/.CURSOR.sorry("You may only specify one of :_ :D :U :T") if $tmode;
            $tmode +|= $mask;
        }
    }

    make { :$type, :$tmode };
}

# { dc: Bool, names: [Either String Op] }
method name($/) {
    my @names = map *.ast, @$<morename>;
    unshift @names, ~$<identifier> if $<identifier>;
    make { dc => !$<identifier>, names => @names };
}

method longname($ ) { } # look at the children yourself
method deflongname($ ) { }

method is_pseudo_pkg {
    $_ eq any < MY OUR CORE DYNAMIC GLOBAL CALLER OUTER UNIT SETTING
        PROCESS COMPILING PARENT CLR >;
}

# this is to be the one place where names are processed

# MODES
# declaring: returns (Package, Str) or Op(defer, returns full Str)
# method:    returns Op(defer, returns full Str)
# reference: returns Package or Op(defer, bvalish lexicoid)

# OPTIONS
# clean:     remove :sym<xyz>

method process_name($/, :$declaring, :$defer, :$clean) {
    return () unless defined $/;

    my @ns = @( $<name>.ast<names> );
    my $ext = '';
    my $trail = @ns && !defined @ns[*-1];
    pop @ns if $trail;

    if !$clean {
        for @( $<colonpair> ) {
            $ext ~= $_.ast<ext> // (
                $_.CURSOR.sorry("Invalid colonpair for name extension");
                "";
            )
        }
    }

    for $defer ?? () !! @ns.grep($Op) {
        $_ = ~self.trivial_eval($/, $_);
        # XXX should this always stringify?
        if $_ ~~ Cool {
            $_ = ~$_;
        } else {
            $_ = "XXX";
            $/.CURSOR.sorry("Name components must evaluate to strings");
        }
    }

    if $declaring {
        # class :: is ... { } is a placeholder for a lack of name
        return () if $trail && !@ns;
        $/.CURSOR.sorry("Illegal explicit declaration of a symbol table")
            if $trail;
        die "Unimplemented" if $defer;
        return () unless @ns;
        my $head = pop(@ns) ~ $ext;
        return Any, $head unless @ns;

        # the remainder is assumed to name an existing or new package
        my $pkg;
        $/.CURSOR.trymop({
            $pkg = $*CURLEX<!sub>.compile_get_pkg(@ns, :auto);
        });
        return $pkg, $head;
    }
    else {
        if $defer {
            # The stuff returned here is processed by the variable rule,
            # and also by method call generation

            goto "dyn" if $trail;
            goto "dyn" if $_.^isa($Op) for @ns;
            my $pkg;
            my @tail = @ns;
            my $head = pop(@tail) ~ $ext;
            unless @tail {
                return { name => $head } unless @tail;
            }
            try { $pkg = $*CURLEX<!sub>.compile_get_pkg(@tail, :auto) };
            goto "dyn" unless $pkg;

            return { name => $head, pkg => $pkg };
dyn:
            my @bits = map { $_, '::' }, @ns;
            pop @bits if @bits;
            push @bits, '::' if $trail;
            push @bits, $ext;
            return { iname => mkstringycat($/, @bits) };
        }

        $/.CURSOR.sorry("Class required, but symbol table name used instead")
            if $trail;
        return () unless @ns;
        my $head = pop(@ns) ~ $ext;
        my $pkg;
        $/.CURSOR.trymop({
            $pkg = $*CURLEX<!sub>.compile_get_pkg(@ns, $head);
        });
        return $pkg;
    }
}
method subshortname($/) {
    if $<colonpair> {
        my $n = ~$<category>;
        for @( $<colonpair> ) {
            $n ~= $_.ast<ext> // (
                $_.CURSOR.sorry("Invalid colonpair for name extension");
                "";
            );
        }
        make { name => $n };
    } else {
        make $<desigilname>.ast;
    }
}

method sublongname($/) {
    if $<sigterm> {
        $/.CURSOR.sorry("Sigterm sublongnames NYI");
        make { name => "" };
    } else {
        make $<subshortname>.ast;
    }
}

method desigilname($/) {
    if $<variable> {
        make { ind => self.do_variable_reference($/, $<variable>.ast) };
    } else {
        make self.process_name($<longname>, :defer);
    }
}

method stopper($ ) { }

method quote_mod:w  ($) { }
method quote_mod:ww ($) { }
method quote_mod:p  ($) { }
method quote_mod:x  ($) { }
method quote_mod:to ($) { }
method quote_mod:s  ($) { }
method quote_mod:a  ($) { }
method quote_mod:h  ($) { }
method quote_mod:f  ($) { }
method quote_mod:c  ($) { }
method quote_mod:b  ($) { }

# quote :: Op
method quote:sym<" "> ($/) { make $<nibble>.ast }
method quote:sym<' '> ($/) { make $<nibble>.ast }
method quote:qq ($/) { make $<quibble>.ast }
method quote:q ($/) { make $<quibble>.ast }
method quote:Q ($/) { make $<quibble>.ast }
method quote:s ($/) { make $<pat>.ast }

method rxembed($/, $op, $) {
    self.inliney_call($/,
        self.thunk_sub($op, params => ['$¢']),
        $OpMakeCursor.new);
}

method op_for_regex($/, $rxop) {
    my @lift = $rxop.oplift;
    my $ltm = $OptRxSimple.run_lad($rxop.lad);
    my ($orxop, $mb) = $OptRxSimple.run($rxop);
    my $sub = self.thunk_sub($OpRegexBody.new(pos=>$/,
            canback => $mb, pre => @lift, rxop => $orxop),
        class => 'Regex', params => ['self'], :$ltm);
    $sub.add_my_name('$/');
    self.block_expr($/, $sub);
}

method quote:sym</ /> ($/) { make self.op_for_regex($/, $<nibble>.ast) }
method quote:rx ($/) {
    self.extract_rx_adverbs(False, False, $<quibble>);
    make self.op_for_regex($/, $<quibble>.ast);
}
method quote:m  ($/) {
    make $OpCallMethod.new(pos=>$/, name => 'match',
            receiver => mklex($/, '$_'),
            args => [
                self.op_for_regex($/, $<quibble>.ast),
                self.extract_rx_adverbs(True, False, $<quibble>) ]);
}

method encapsulate_regex($/, $rxop, :$passcut = False) {
    my @lift = $rxop.oplift;
    my $lad = $rxop.lad;
    my ($nrxop, $mb) = $OptRxSimple.run($rxop);
    my $subop = self.thunk_sub(
        $OpRegexBody.new(canback => $mb, pre => @lift, :$passcut,
            rxop => $nrxop), ltm => $lad, class => 'Regex', params => ['self']);
    $subop = $OpCallSub.new(pos=>$/, invocant => self.block_expr($/, $subop),
        positionals => [ $OpMakeCursor.new(pos=>$/) ]);
    $RxOpSubrule.new(regex => $subop, ltm => $lad);
}

method regex_block($/) {
    if $<onlystar> {
        return Nil;
    }
    if $<quotepair> {
        $/.CURSOR.sorry('Regex adverbs NYI');
    }
    make $<nibble>.ast;
}

method regex_def_1 ($, $/ = $*cursor) {
    self.install_sub($/, $*CURLEX<!sub>, scope => $*SCOPE, class => 'Regex',
        method_type => (($*SCOPE || 'has') eq 'has') ?? 'normal' !! Any,
        longname => $<deflongname>, multiness => $*MULTINESS);
}

method regex_def_2 ($, $/ = $*cursor) {
    if $<signature> > 1 {
        $/.CURSOR.sorry("Too many signatures on regex");
    }

    for map *.ast, @$<trait> -> $t {
        if $t<unary> || $t<binary> || $t<defequiv> || $t<of> {
            # Ignored for now
        }
        elsif defined $t<endsym> {
            %*RX<endsym> = $t<endsym>;
        }
        else {
            $/.CURSOR.sorry("Unhandled regex trait $t.keys.[0]");
        }
    }

    if $*CURLEX<!multi> eq 'proto' {
        @*MEMOS[0]<proto_endsym>{$*CURLEX<!cleanname>} = %*RX<endsym>
            if defined $*CURLEX<!cleanname>;
    } else {
        %*RX<endsym> //= @*MEMOS[0]<proto_endsym>{$*CURLEX<!cleanname>} if
            defined $*CURLEX<!cleanname>;
    }

    %*RX<dba> = $*CURLEX<!name> // 'anonymous regex';
}

method regex_def($/) {
    my $ast = $<regex_block>.ast;

    if $<regex_block><onlystar> {
        $ast = $RxOpProtoRedis.new(name => $*CURLEX<!name>);
    }

    my @lift = $ast.oplift;
    my $ltm = $OptRxSimple.run_lad($ast.lad);
    $*CURLEX<!sub>.set_ltm($ltm);
    ($ast, my $mb) = $OptRxSimple.run($ast);
    if $<regex_block><onlystar> {
        $*CURLEX<!sub>.finish_dispatcher('regex');
    } else {
        $*CURLEX<!sub>.finish($OpRegexBody.new(pos=>$/, pre => @lift,
            name => ($*CURLEX<!name> // ''), rxop => $ast, canback => $mb));
    }
    make $OpLexical.new(pos=>$/, name => $*CURLEX<!sub>.outervar);
}

method regex_declarator:regex ($/) { make $<regex_def>.ast }
method regex_declarator:rule  ($/) { make $<regex_def>.ast }
method regex_declarator:token ($/) { make $<regex_def>.ast }

# :: RxOp
method atom($/) {
    if $<metachar> {
        make $<metachar>.ast;
    } else {
        make $RxOpString.new(text => ~$/,
            igcase => %*RX<i>, igmark => %*RX<a>);
    }
}

method quantified_atom($/) { # :: RxOp
    my $atom = $<atom>.ast;
    my $q    = $<quantifier> ?? $<quantifier>.ast !! Any;

    return Nil unless $atom;

    if %*RX<r> {
        # no quantifier at all?  treat it as :
        $q //= { mod => '' };
        # quantifier without explicit :? / :! gets :
        $q<mod> //= '';
    }

    if defined $q<min> {
        my @z = $atom;
        if $<separator> {
            if $q<sep>:exists {
                $/.CURSOR.sorry("Cannot use two separators in one quantified_atom");
            }
            for %( $<separator>.ast ) { $q{.key} = .value }
        }
        push @z, $q<sep> if defined $q<sep>;
        # parsing quirk, x #`(1) ** #`(2) y, the 1* position is counted
        # as $<normspace> but the 2* is parsed by the quantifier
        if ($q<general> || @z[1]) && %*RX<s> && ($q<space> || $<normspace>) {
            if @z[1] {
                @z[1] = $RxOpSequence.new(zyg => [
                    $RxOpSigspace.new, @z[1], $RxOpSigspace.new]);
            } else {
                push @z, $RxOpSigspace.new;
            }
        }
        $atom = $RxOpQuantifier.new(min => $q<min>, max => $q<max>,
            nonlisty => $q<nonlisty>, closure => $q<closure>,
            opsep => $q<opsep>, zyg => [@z],
            minimal => ($q<mod> && $q<mod> eq '?'));
    }

    if defined($q<mod>) && $q<mod> eq '' {
        $atom = $RxOpCut.new(zyg => [$atom]);
    }

    if defined $q<tilde> {
        my ($closer, $inner) = @( $q<tilde> );
        $closer = $closer.zyg[0] if $closer.^isa($RxOpCut) &&
            $closer.zyg[0].^isa($RxOpString);
        if !$closer.^isa($RxOpString) {
            $/.CURSOR.sorry("Non-literal closers for ~ NYI");
            make $RxOpNone.new;
            return;
        }
        $atom = $RxOpSequence.new(zyg => [$atom,
            $RxOpTilde.new(closer => $closer.text, dba => %*RX<dba>,
                zyg => [$inner])]);
    }

    make $atom;
}

# :: Context hash interpreted by quantified_atom
method quantifier:sym<*> ($/) { make { min => 0, mod => $<quantmod>.ast } }
method quantifier:sym<+> ($/) { make { min => 1, mod => $<quantmod>.ast } }
method quantifier:sym<?> ($/) { make { min => 0, max => 1, mod => $<quantmod>.ast, :nonlisty } }
method quantifier:sym<:> ($/) { make { mod => '' } }
method quantifier:sym<~> ($/) {
    make { tilde => [ map *.ast, @($<quantified_atom>) ] }
}
method quantifier:sym<**> ($/) {
    my $h = $<embeddedblock> ?? { min => 0, closure =>
                self.inliney_call($/, $<embeddedblock>.ast) } !!
            $<quantified_atom> ?? { min => 1, sep => $<quantified_atom>.ast } !!
            { min => +~$0, max => ($1 ?? +~$1 !!
                defined($/.index('..')) ?? Any !! +~$0) };
    $h<mod> = $<quantmod>.ast;
    $h<general> = True;
    $h<space> = ?($<normspace>);
    make $h;
}

method separator($/) {
    make { sep   => $<quantified_atom>.ast,
           space => ?($<normspace>),
           opsep => (substr($/.orig, $/.from+1, 1) // '') eq '%' };
}

method quantmod($/) {
    my $t = ~$/;
    if $t eq '' { make Any; return Nil }
    if substr($t,0,1) eq ':' { $t = substr($t,1,chars($t)-1) }
    if $t eq '+' { $t = '' }
    make $t;
}

method quant_atom_list($/) {
    make $RxOpSequence.new(zyg => [ map *.ast, @( $<quantified_atom> ) ]);
}

method LISTrx($/) {
    state %LISTrx_types = (
        '&'  => $RxOpConj,
        '|'  => $RxOpAlt,
        '&&' => $RxOpConj, # these are treated as the same!
        '||' => $RxOpSeqAlt,
    );

    make %LISTrx_types{$<delims>[0]<sym>}.new(zyg =>
        [ map *.ast, @( $<list> ) ], dba => %*RX<dba>);
}

method regex_infix:sym<|> ($/) {}
method regex_infix:sym<||> ($/) {}
method regex_infix:sym<&> ($/) {}
method regex_infix:sym<&&> ($/) {}

method metachar:sigwhite ($/) {
    make (%*RX<s> ?? $RxOpSigspace.new !! $RxOpSequence.new);
}
method metachar:unsp ($/) { make $RxOpSequence.new }

method metachar:sym<{ }> ($/) {
    $/.CURSOR.trymop({
        $<embeddedblock>.ast.add_my_name('$¢', :noinit, |mnode($/));
        $<embeddedblock>.ast.set_signature($Sig.simple('$¢'));
    });

    make $RxOpVoidBlock.new(block => self.inliney_call($/,
        $<embeddedblock>.ast, $OpMakeCursor.new(pos=>$/)));
}

method metachar:mod ($/) {
    # most of these have only parse-time effects
    make (($<mod_internal>.ast ~~ $RxOp) ?? $<mod_internal>.ast !! $RxOpSequence.new);
}

method metachar:sym<::> ($/) { make $RxOpCutLTM.new }
method metachar:sym«::>» ($/) { make $RxOpCutBrack.new }
method metachar:sym<:::> ($/) { make $RxOpCutRule.new }

method metachar:sym<[ ]> ($/) {
    make $RxOpConfineLang.new(zyg => [$<nibbler>.ast]);
}

method metachar:sym<( )> ($/) {
    make self.rxcapturize($/, %*RX<paren>++,
        self.encapsulate_regex($/, $<nibbler>.ast, passcut => True));
}

method metachar:sym« <( » ($/) {
    make $RxOpEndpoint.new(pos=>$/, :type<from>)
}
method metachar:sym« )> » ($/) {
    make $RxOpEndpoint.new(pos=>$/, :type<to>)
}
method metachar:sym« << » ($/) { make $RxOpZeroWidth.new(type => '<<') }
method metachar:sym« >> » ($/) { make $RxOpZeroWidth.new(type => '>>') }
method metachar:sym< « > ($/) { make $RxOpZeroWidth.new(type => '<<') }
method metachar:sym< » > ($/) { make $RxOpZeroWidth.new(type => '>>') }
method metachar:sym<{*}> ($/) {
    make $RxOpProtoRedis.new(name => '', :!cutltm);
    return;
}

method metachar:qw ($/) {
    my $cif = $<circumfix>.ast;
    my @words = $cif.^isa($OpParen) ?? @( $cif.inside.items ) !! $cif;
    @words = map *.text, @words;

    make $RxOpAlt.new(zyg => [ map { $RxOpString.new(text => $_,
            igcase => %*RX<i>, igmark => %*RX<a>) }, @words ], dba => %*RX<dba>);
}

method metachar:sym«< >» ($/) { make $<assertion>.ast }
method metachar:sym<\\> ($/) {
    my $cc = $<backslash>.ast;
    make ($cc.^isa(Str) ??
        $RxOpString.new(text => $cc,
            igcase => %*RX<i>, igmark => %*RX<a>) !!
        self.cc_to_rxop($cc));
}

method metachar:sym<.> ($/) { make $RxOpAny.new }
method metachar:sym<^> ($/) { make $RxOpZeroWidth.new(type => '^'); }
method metachar:sym<^^> ($/) { make $RxOpZeroWidth.new(type => '^^'); }
method metachar:sym<$> ($/) { make $RxOpZeroWidth.new(type => '$'); }
method metachar:sym<$$> ($/) { make $RxOpZeroWidth.new(type => '$$'); }

method metachar:sym<' '> ($/) {
    if ! $<quote>.ast.^isa($OpStringLiteral) {
        make $RxOpVarString.new(ops => self.rxembed($/, $<quote>.ast, True));
        return Nil;
    }
    make $RxOpString.new(text => $<quote>.ast.text, igcase => %*RX<i>,
        igmark => %*RX<a>);
}

method metachar:sym<" "> ($/) {
    if ! $<quote>.ast.^isa($OpStringLiteral) {
        make $RxOpVarString.new(ops => self.rxembed($/, $<quote>.ast, True));
        return Nil;
    }
    make $RxOpString.new(text => $<quote>.ast.text, igcase => %*RX<i>,
        igmark => %*RX<a>);
}

method metachar:var ($/) {
    sub _isnum { $_ ~~ /^\d+$/ }
    if $<binding> {
        my $a = $<binding><quantified_atom>.ast.uncut;
        my $cid = $<variable>.ast.<capid>;

        if !defined $cid {
            $/.CURSOR.sorry("Non-Match bindings NYI");
            make $RxOpSequence.new;
            return Nil;
        }

        if $a.^isa($RxOpVoidBlock) {
            make $RxOpSaveValue.new(capid => $cid, block => $a.block);
            return Nil;
        }

        if _isnum($cid) {
            %*RX<paren> = $cid + 1;
        }

        make self.rxcapturize($/, $cid, $a);
        return;
    }

    my $kind = 'scalar_var';
    given substr($<variable>,0,1) {
        when '$' { $kind = 'scalar_var'; }
        when '@' { $kind = 'list_var'; }
        default  {
            $/.CURSOR.sorry('Only $ and @ variables may be used in regexes for now');
        }
    }
    make $RxOpListPrim.new(name => ~$<variable>, type => $kind,
        ops => self.rxembed($/,
            self.do_variable_reference($/, $<variable>.ast), True));
}

method rxcapturize($M, $name, $rxop is copy) { #OK not used
    if !$rxop.^isa($RxOpCapturing) {
        # $<foo>=[...]
        $rxop = $RxOpStringCap.new(zyg => [$rxop]);
    }

    # $<foo>=(...)
    # XXX might not quite be right
    if +$rxop.captures == 1 && $rxop.captures.[0] ~~ /^\d+$/ {
        return $rxop.clone(captures => [$name]);
    }

    return $rxop.clone(captures => [ $name, @( $rxop.captures ) ]);
}

# UTS18 specifies a rule for "pulling up" negations in character classes,
# so we have to delay the negation, it seems; [0] = neg [1] = RxOp

method negate_cc($exp) { [ !$exp[0], $exp[1] ] }
method void_cc() { [False, $RxOpCClassElem.new(cc => $CClass::Empty)] }
method cclass_cc($cc) { [False, $RxOpCClassElem.new(:$cc)] }
method neg_cclass_cc($cc) { [True, $RxOpCClassElem.new(:$cc)] }
method string_cc($str) {
    $str.codes == 1 ?? self.cclass_cc($CClass.enum($str)) !!
        [False, $RxOpString.new(text => $str)];
}

# TODO: implement this more directly
method xor_cc($lhs, $rhs) {
    self.or_cc(self.and_cc($lhs, self.negate_cc($rhs)),
               self.and_cc(self.negate_cc($lhs), $rhs));
}

method and_cc($lhs, $rhs) {
    self.negate_cc(self.or_cc(self.negate_cc($lhs), self.negate_cc($rhs)));
}

method or_cc($lhs, $rhs) {
    say "or($lhs[1].typename(), $rhs[1].typename())" if $CCTrace;
    my $ccl = $lhs[1] ~~ $RxOpCClassElem;
    my $ccr = $rhs[1] ~~ $RxOpCClassElem;
    if $lhs[0] {
        if $rhs[0] {
            $ccl && $ccr ??
                self.neg_cclass_cc($lhs[1].cc.minus($rhs[1].cc.negate)) !!
                [ True, $RxOpConj.new(zyg => [ $lhs[1], $rhs[1] ]) ];
        } else { # !L | R = !(L & !R)
            $ccl && $ccr ??
                self.neg_cclass_cc($lhs[1].cc.minus($rhs[1].cc)) !!
                [ True, $RxOpSequence.new(zyg => [
                    $RxOpNotBefore.new(zyg => [ $rhs[1] ]), $lhs[1] ]) ];
        }
    } else {
        if $rhs[0] {
            self.or_cc($rhs, $lhs);
        } else {
            $ccl && $ccr ??
                self.cclass_cc($lhs[1].cc.plus($rhs[1].cc)) !!
                [ False, $RxOpAlt.new(dba => 'character class',
                    zyg => [$lhs[1], $rhs[1]]) ];
        }
    }
}

method cc_to_rxop($z) {
    say "do_cc $z[1].typename()" if $CCTrace;
    if $z[0] && $z[1] ~~ $RxOpCClassElem {
        return $RxOpCClassElem.new(cc => $z[1].cc.negate);
    }
    return $z[0] ?? $RxOpSequence.new(zyg => [
        $RxOpNotBefore.new(zyg => [$z[1]]), $RxOpAny.new]) !! $z[1];
}

method cclass_expr($/) {
    my @ops = @$<op>;
    my @zyg = map *.ast, @$<cclass_union>;
    for @ops -> $op {
        my $z1 = shift @zyg;
        my $z2 = shift @zyg;
        unshift @zyg, ($op eq '^') ?? self.xor_cc($z1,$z2) !! self.or_cc($z1,$z2);
    }
    say "cclass_expr @zyg[0][1].typename()" if $CCTrace;
    make @zyg[0];
}

method cclass_union($/) {
    my ($a, @zyg) = map *.ast, @$<cclass_add>;
    for @zyg { $a = self.and_cc($a, $_) }
    say "cclass_union $a[1].typename()" if $CCTrace;
    make $a;
}

method cclass_add($/) {
    my ($a, @zyg) = map *.ast, @$<cclass_elem>;
    if $<sign> eq '-' { $a = self.negate_cc($a) }
    for @$<op> {
        $a = ($_ eq '+') ?? self.or_cc($a, shift(@zyg))
                         !! self.and_cc($a, self.negate_cc(shift(@zyg)));
    }
    say "cclass_add $a[1].typename()" if $CCTrace;
    make $a;
}

method cclass_elem:name ($/) {
    make (substr($<name>,0,10) eq 'INTERNAL::') ??
        self.cclass_cc($CClass.internal(substr($<name>,10))) !!
        [False, $RxOpSubrule.new(captures => [], method => ~$<name>)];

    say ":name $<name> $/.ast[1].typename()" if $CCTrace;
}

method cclass_elem:sym<[ ]> ($/) {
    make $<nibble>.ast;
    say ":[] $/.ast[1].typename()" if $CCTrace;
}

method cclass_elem:sym<( )> ($/) {
    make $<cclass_expr>.ast;
}

method cclass_elem:property ($/) {
    my $body = self.thunk_sub($<colonpair>.ast<term>, name => ~$<colonpair>);
    $body.outer.create_static_pad;
    make self.void_cc;
    $/.CURSOR.trymop({
        make self.cclass_cc($CClass.new(terms => [ $body.run_BEGIN_CC ]));
    });
}

method cclass_elem:quote ($/) {
    if ! $<quote>.ast.^isa($OpStringLiteral) {
        make $RxOpVarString.new(ops => self.rxembed($/, $<quote>.ast, True));
        return;
    }
    if !%*RX<i> && !%*RX<a> {
        make self.string_cc($<quote>.ast.text);
        return;
    }
    make [False, $RxOpString.new(text => $<quote>.ast.text,
        igcase => %*RX<i>, igmark => %*RX<a>)];
}

method decapturize($/) {
    if !$<assertion>.ast.^isa($RxOpCapturing) {
        return $<assertion>.ast;
    }
    $<assertion>.ast.clone(captures => []);
}

method cclass_elem($ ) {}

method assertion:name ($/) {
    my ($pname) = self.process_name($<longname>, :defer);
    my $name = ~$<longname>;

    if !$pname {
        $pname = { name => 'alpha' };
        $/.CURSOR.sorry('Method call requires a method name');
    }

    my @lex = $*CURLEX<!sub>.lookup_lex("&$name");
    my $is_lexical = substr($/.orig, $/.from-1, 1) ne '.' &&
        @lex && @lex[0] eq 'sub' && @lex[4].is_regex;

    if $<assertion> {
        make $<assertion>.ast;
    } elsif $name eq 'sym' {
        $/.CURSOR.sorry("<sym> is only valid in multiregexes")
            unless defined %*RX<sym>;
        make $RxOpSym.new(igcase => %*RX<i>, igmark => %*RX<a>,
            text => %*RX<sym> // '', endsym => %*RX<endsym>);
    } elsif $name eq 'before' {
        make $RxOpBefore.new(zyg => [$<nibbler>.ast]);
        return Nil;
    } elsif $name eq 'after' {
        my @l = $<nibbler>.ast.tocclist;
        if grep { !defined $_ }, @l {
            $/.CURSOR.sorry("Unsuppored elements in after list");
            make $RxOpSequence.new;
            return Nil;
        }
        make $RxOpZeroWidthCCs.new(neg => False, after => True, ccs => @l);
        return;
    } elsif !$<nibbler> && !$<arglist> && !$pname<pkg> && !$pname<iname> &&
            !$is_lexical {
        make $RxOpSubrule.new(method => $pname<name>);
    } else {
        my $args = $<nibbler> ??
            [ self.op_for_regex($/, $<nibbler>.ast) ] !!
            $<arglist> ?? $<arglist>.ast !! [];

        if $pname<iname> {
            $/.CURSOR.sorry('Indirect method calls NYI');
            $pname = {name => 'alpha'};
        }

        my $callop;
        if $is_lexical {
            $callop = $OpCallSub.new(invocant => mklex($/, "&$name"),
                positionals => [ mklex($/, '$¢'), @$args ]);
        } else {
            $callop = $Operator_Method.new(name => $pname<name>, :$args,
                package => $pname<pkg>).with_args($/, mklex($/, '$¢'));
        }

        my $regex = self.rxembed($/, $callop, True);

        make $RxOpSubrule.new(regex => $regex);
    }
    make self.rxcapturize($/, ~$<longname>, $/.ast);
}

method assertion:variable ($/) {
    given substr($/,0,1) {
        when '&' {
            if $<variable>.ast ~~ $OpCallSub {
                make $RxOpSubrule.new(pos=>$/, regex =>
                    $OpCallSub.new(pos=>$/,
                        invocant => $<variable>.ast.invocant,
                        args => [ $OpMakeCursor.new(pos=>$/),
                            @( $<variable>.ast.args ) ]));
            } else {
                make $RxOpSubrule.new(pos=>$/, regex =>
                    $OpCallSub.new(pos=>$/, invocant => $<variable>.ast,
                        positionals => [ $OpMakeCursor.new(pos=>$/) ]));
            }
        }
        when '$' {
            make $RxOpListPrim.new(type => 'scalar_asn',
                ops => self.rxembed($/, $<variable>.ast, True));
        }
        when '@' {
            make $RxOpListPrim.new(type => 'list_asn',
                ops => self.rxembed($/, $<variable>.ast, True));
        }
        default {
            make $RxOpNone.new;
            $/.CURSOR.sorry("Sigil $_ is not allowed for regex assertions");
        }
    }
}

method assertion:method ($/) {
    if $<dottyop> {
        make $RxOpSubrule.new(pos=>$/, regex =>
            self.rxembed($/, $<dottyop>.ast.with_args($/,
                    $OpMakeCursor.new(pos=>$/)), True));
    } else {
        make self.decapturize($/);
    }
}

method assertion:sym<?> ($/) {
    if $<assertion> {
        make $<assertion>.reduced eq 'assertion:sym<{ }>' ??
            $RxOpCheckBlock.new(block => $<assertion>.ast.ops, :!negate) !!
            $RxOpBefore.new(zyg => [self.decapturize($/)]);
    } else {
        make $RxOpSequence.new;
    }
}

method assertion:sym<!> ($/) {
    if $<assertion> {
        make $<assertion>.reduced eq 'assertion:sym<{ }>' ??
            $RxOpCheckBlock.new(block => $<assertion>.ast.ops, :negate) !!
            $RxOpNotBefore.new(zyg => [self.decapturize($/)]);
    } else {
        make $RxOpNone.new;
    }
}

method assertion:sym<{ }> ($/) {
    $/.CURSOR.trymop({
        $<embeddedblock>.ast.add_my_name('$¢', :noinit, |mnode($/));
        $<embeddedblock>.ast.set_signature($Sig.simple('$¢'));
    });

    make $RxOpListPrim.new(type => 'scalar_asn', ops => self.inliney_call($/,
        $<embeddedblock>.ast, $OpMakeCursor.new(pos=>$/)));
}

method assertion:sym<:> ($/) { make self.cc_to_rxop($<cclass_expr>.ast) }
method assertion:sym<[> ($/) { make self.cc_to_rxop($<cclass_expr>.ast) }
method assertion:sym<-> ($/) { make self.cc_to_rxop($<cclass_expr>.ast) }
method assertion:sym<+> ($/) { make self.cc_to_rxop($<cclass_expr>.ast) }

# These have effects only in the parser, so no ast is correct.
method mod_value($ ) {}
method mod_internal:sym<:i> ($ ) {}
method mod_internal:sym<:!i> ($ ) {}
method mod_internal:sym<:i( )> ($ ) {}
method mod_internal:sym<:0i> ($ ) {}
method mod_internal:sym<:s> ($ ) {}
method mod_internal:sym<:!s> ($ ) {}
method mod_internal:sym<:s( )> ($ ) {}
method mod_internal:sym<:0s> ($ ) {}
method mod_internal:sym<:r> ($ ) {}
method mod_internal:sym<:!r> ($ ) {}
method mod_internal:sym<:r( )> ($ ) {}
method mod_internal:sym<:0r> ($ ) {}
method mod_internal:sym<:a> ($ ) {}
method mod_internal:sym<:!a> ($ ) {}
method mod_internal:sym<:a( )> ($ ) {}
method mod_internal:sym<:0a> ($ ) {}

method mod_internal:sym<:my> ($/) {
    make $RxOpStatement.new(stmt => $<statement>.ast );
}

method mod_internal:p6adv ($/) {
    my ($k, $v) = $<quotepair><k v>;

    if !$v.^isa(Match) {
        $/.CURSOR.sorry(":$k requires an expression argument");
        make $RxOpNone.new;
        return Nil;
    }
    $v = $v.ast;

    if $k eq 'lang' {
        make $RxOpSetLang.new(expr => self.rxembed($/, $v, True));
    } elsif $k eq 'dba' {
        while True {
            if $v.^isa($OpParen) { $v = $v.inside; redo }
            if $v.^isa($OpStatementList) && +$v.children == 1
                { $v = $v.children.[0]; redo }
            last;
        }
        if !$v.^isa($OpStringLiteral) {
            $/.CURSOR.sorry(":dba requires a literal string");
            make $RxOpNone.new;
            return Nil;
        }
        %*RX<dba> = $v.text;
    }
}

method backslash:qq ($/) { make $<quote>.ast }
method post_backslash($/) {
    # XXX confine $/ resetting
    sub _isupper { $_ ~~ /^<[ A .. Z ]>/ }
    sub _islower { $_ ~~ /^<[ a .. z ]>/ }
    if $/.ast.^isa($CClass) {
        make self.cclass_cc($/.ast);
    }
    if _isupper($/) && _islower($<sym>) {
        if $/.ast.^isa(Str) {
            make self.string_cc($/.ast);
        }
        make self.negate_cc($/.ast);
    }
}
method backslash:x ($/) {
    if $<hexint> {
        make chr($<hexint>.ast);
    } else {
        make (join "", map *.&chr, @( $<hexints>.ast ));
    }
    self.post_backslash($/);
}
method backslash:o ($/) {
    if $<octint> {
        make chr($<octint>.ast);
    } else {
        make (join "", map *.&chr, @( $<octints>.ast ));
    }
    self.post_backslash($/);
}
method backslash:sym<\\> ($/) { make ~$<text> }
method backslash:stopper ($/) { make ~$<text> }
method backslash:unspace ($/) { make "" }
method backslash:misc ($/) { make ($<text> // ~$<litchar>) }
method backslash:sym<0> ($/) { make "\0" }
method backslash:a ($/) { make "\a"; self.post_backslash($/) }
method backslash:b ($/) { make "\b"; self.post_backslash($/) }
method backslash:c ($/) { make $<charspec>.ast; self.post_backslash($/); }
method backslash:d ($/) { make $CClass::Digit; self.post_backslash($/) }
method backslash:e ($/) { make "\e"; self.post_backslash($/) }
method backslash:f ($/) { make "\f"; self.post_backslash($/) }
method backslash:h ($/) { make $CClass::HSpace; self.post_backslash($/) }
method backslash:n ($/) {
    if $/.CURSOR.can('backslash:d') {
        # HACK - only use this form when we're looking for regexy stuff
        make [False, $RxOpNewline.new];
        self.post_backslash($/)
    } else {
        make "\n";
    }
}
method backslash:r ($/) { make "\r"; self.post_backslash($/) }
method backslash:s ($/) { make $CClass::Space; self.post_backslash($/) }
method backslash:t ($/) { make "\t"; self.post_backslash($/) }
method backslash:v ($/) { make $CClass::VSpace; self.post_backslash($/) }
method backslash:w ($/) { make $CClass::Word; self.post_backslash($/) }

method escape:sym<\\> ($/) { make $<item>.ast }
method escape:sym<{ }> ($/) { make self.inliney_call($/, $<embeddedblock>.ast) }
method escape:sym<$> ($/) { make $<EXPR>.ast }
method escape:sym<@> ($/) { make $<EXPR>.ast }
method escape:sym<%> ($/) { make $<EXPR>.ast }
method escape:ch ($/) { make ~$<ch> }
method escape:ws ($/) { make "" }
my class RangeSymbol { };
method escape:sym<..> ($/) { make RangeSymbol }

# XXX I probably shouldn't have used "Str" for this action method name
method Str($match?) { "NieczaActions" } #OK not used
method process_nibble($/, @bits, $prefix?) {
    my @acc;
    for @bits -> $n {
        my $ast = ($n.ast // ~$n);

        if $ast ~~ $CClass {
            $n.CURSOR.sorry("Cannot use a character class in a string");
            $ast = "";
        }

        if $ast !~~ $Op && defined($prefix) && $prefix ne "" {
            my $start_nl = !$n.from || "\r\n".index(
                substr($n.orig, $n.from-1, 1)).defined;
            $ast = $ast.split(/ ^^ [ <?{ $start_nl }> || <?after <[\x0A\x0D]> > ]
                <before \h>[ $prefix || \h+ ]/).join("");
        }

        push @acc, $ast;
    }

    my $post = $/.CURSOR.postprocessor;
    make mkstringycat($/, @acc);

    if $post eq 'null' {
        # already OK
    }
    # actually quotewords is a bit trickier than this...
    elsif $post eq 'words' || $post eq 'quotewords' {
        my $sl = $/.ast;
        if !$sl.^isa($OpStringLiteral) {
            make $OpCallMethod.new(pos=>$/, :name<words>, receiver => $sl);
        }
        else {
            my @tok = $sl.text.words;
            @tok = map { $OpStringLiteral.new(pos=>$/, text => $_) }, @tok;

            make ((@tok == 1) ?? @tok[0] !! $OpParen.new(pos=>$/,
                inside => $OpSimpleParcel.new(pos=>$/, items => @tok)));
        }
    }
    elsif $post eq 'path' {
        # TODO could stand to be a lot fancier.
        make $OpCallMethod.new(pos=>$/, receiver => $/.ast, :name<IO>);
    }
    elsif $post eq 'run' {
        make mkcall($/, '&rungather', $/.ast);
    }
    else {
        $/.CURSOR.sorry("Unhandled postprocessor $post");
    }

    $/.ast;
}

method process_tribble(@bits) {
    my @cstack;
    my @mstack;
    for @bits -> $b {
        if $b.ast.^isa(Str) {
            next if $b.ast eq "";
        }
        push @mstack, $b.CURSOR;
        push @cstack, $b.ast;
        if @cstack >= 2 && @cstack[*-2] ~~ RangeSymbol {
            if @cstack == 2 {
                @mstack[0].sorry(".. requires a left endpoint");
                return self.void_cc;
            }
            for 1, 3 -> $i {
                if (@cstack[*-$i] !~~ Str) || (@cstack[*-$i].codes != 1) {
                    @mstack[*-$i].sorry(".. endpoint must be a single character");
                    return self.void_cc;
                }
            }
            my $new = [False, $RxOpCClassElem.new(cc =>
                $CClass.range(@cstack[*-3], @cstack[*-1]))];
            pop(@cstack); pop(@cstack); pop(@cstack); push(@cstack, $new);
            pop(@mstack); pop(@mstack);
        }
    }
    if @cstack && @cstack[*-1] ~~ RangeSymbol {
        @mstack[*-1].sorry(".. requires a right endpoint");
        return self.void_cc;
    }
    my $retcc = self.void_cc;
    for @cstack {
        $retcc = self.or_cc($retcc, ($_ ~~ Str) ?? self.string_cc($_) !! $_);
    }
    $retcc;
}

method nibbler($/, $prefix?) {
    sub iscclass($cur) {
        # XXX XXX
        ?$cur.^can('ccstate');
    }
    if $/.CURSOR.^isa(::STD::Regex) {
        make $<EXPR>.ast;
    } elsif $/.CURSOR.^isa(::NieczaGrammar::CgOp) {
        if $*SAFEMODE {
            $/.CURSOR.sorry('Q:CgOp not allowed in safe mode');
            make $OpStatementList.new;
            return Nil;
        }
        make $OpCgOp.new(pos=>$/, optree => $<cgexp>.ast);
    } elsif iscclass($/.CURSOR) {
        make self.process_tribble($<nibbles>);
    } else {
        make self.process_nibble($/, $<nibbles>, $prefix);
    }
}

method split_circumfix ($/) {
    my $sl = $<nibble>.ast;

    if !$sl.^isa($OpStringLiteral) {
        make $OpCallMethod.new(pos=>$/, name => "words", receiver => $sl);
        return Nil;
    }

    my @tok = $sl.text.words;
    @tok = map { $OpStringLiteral.new(pos=>$/, text => $_) }, @tok;

    make ((@tok == 1) ?? @tok[0] !!
        $OpSimpleParcel.new(pos=>$/, items => @tok));
}
method circumfix:sym«< >» ($/)   { make $<nibble>.ast }
method circumfix:sym«<< >>» ($/) { make $<nibble>.ast }
method circumfix:sym<« »> ($/)   { make $<nibble>.ast }

method circumfix:sym<( )> ($/) {
    my @kids = @( $<semilist>.ast );
    if @kids == 1 && @kids[0].^isa($OpWhateverCode) {
        # XXX in cases like * > (2 + *), we *don't* want the parens to disable
        # syntactic specialization, since they're required for grouping
        make @kids[0];
    } elsif !@kids {
        # an empty StatementList returns Nil, but () needs to be defined...
        make $OpParen.new(pos=>$/, inside =>
            $OpSimpleParcel.new(items => []));
    } else {
        make $OpStatementList.new(pos=>$/, children => @kids);
    }
}

method circumfix:sym<[ ]> ($/) {
    my @kids = @( $<semilist>.ast );
    make mkcall($/, '&_array_constructor',
        $OpStatementList.new(pos=>$/, children => @kids));
}

# XXX This fails to catch {; ... } because it runs after empty statement
# elimination.
method check_hash($/) {
    my $do = $<pblock><blockoid>.ast;

    return False if $<pblock>.ast.arity;

    return False unless $do.^isa($OpStatementList);
    return True if $do.children == 0;
    return False if $do.children > 1;

    $do = $do.children[0];
    my @bits = $do.^isa($OpSimpleParcel) ?? @( $do.items ) !! $do;

    return True if @bits[0].^isa($OpSimplePair);

    if @bits[0].^isa($OpBuiltin) && @bits[0].name eq 'pair' {
        return True;
    }

    if @bits[0].^isa($OpGeneralConst) && @bits[0].value.starts_with_pair {
        return True;
    }

    if @bits[0].^isa($OpLexical) && substr(@bits[0].name,0,1) eq '%' {
        return True;
    }

    return False;
}

method circumfix:sym<{ }> ($/) {
    my $var = self.gensym;
    $*CURLEX<!sub>.add_my_sub($var, $<pblock>.ast);
    make $OpBareBlock.new(pos=>$/, :$var);

    if self.check_hash($/) {
        make mkcall($/, '&_hash_constructor',
            $OptBeta.make_call($var));
    }
}

method circumfix:sigil ($/) {
    # XXX duplicates logic in variable
    if $<semilist>.ast.elems == 0 {
        if $<sigil> eq '$' {
            make $OpShortCircuit.new(pos=>$/, kind => '//',
                args => [ $OpCallMethod.new(name => 'ast',
                            receiver => mklex($/, '$/')),
                          $OpCallMethod.new(name => 'Str',
                            receiver => mklex($/, '$/')) ] );
        } elsif $<sigil> eq any < @ % > {
            make self.docontext($/, ~$<sigil>, mklex($/, '$/'));
        } else {
            make mklex($/, 'Mu');
            $/.CURSOR.sorry("Missing argument for contextualizer");
        }
        return;
    }
    self.circumfix:sym<( )>($/);
    make self.docontext($/, ~$<sigil>, $/.ast);
}

method infix_prefix_meta_operator:sym<!> ($/) {
    make $<infixish>.ast.meta_not;
}
method infix_prefix_meta_operator:sym<R> ($/) {
    make $<infixish>.ast.meta_fun($/, '&reverseop', 2);
}
method infix_prefix_meta_operator:sym<Z> ($/) {
    make $<infixish> ?? $<infixish>.ast.meta_fun($/, '&zipop', 2) !!
        $Operator.funop($/, '&infix:<Z>', 2);
}
method infix_prefix_meta_operator:sym<X> ($/) {
    make $<infixish> ?? $<infixish>.ast.meta_fun($/, '&crossop', 2) !!
        $Operator.funop($/, '&infix:<X>', 2);
}
method infix_prefix_meta_operator:sym<S> ($/) {
    make $<infixish>.ast.meta_fun($/, '&seqop', 2);
}

method infix_circumfix_meta_operator:sym<« »> ($/) {
    make $<infixish>.ast.meta_fun($/, '&hyper', 2,
        mkbool(substr($/,0,1) eq '«'), mkbool(substr($/,chars($/)-1,1) eq '»'));
}
method infix_circumfix_meta_operator:sym«<< >>» ($/) {
    make $<infixish>.ast.meta_fun($/, '&hyper', 2,
        mkbool(substr($/,0,2) eq '<<'),
        mkbool(substr($/,chars($/)-2,2) eq '>>'));
}

method postfix_prefix_meta_operator:sym< » > ($/) { } #handled in POST
method prefix_postfix_meta_operator:sym< « > ($/) { } #handled in PRE

method infixish($/) {
    if $<colonpair> || $<regex_infix> {
        return Nil; # handled elsewhere
    }

    if $<assign_meta_operator> {
        # TODO: there should probably be at least a potential for others

        make $<infix>.ast.meta_assign;
    } else {
        make $<infix>.ast;
    }
}

method infix:sym<...> ($/) {
    # STD parses ...^ in the ... rule
    make $Operator.funop($/, '&infix:<' ~ $/ ~ '>', 2);
}
method infix:sym<xx> ($/) { make $Operator_Replicate.new }
method infix:sym<ff>($/) { make $Operator_FlipFlop.new() }
method infix:sym<fff>($/) { make $Operator_FlipFlop.new(:sedlike) }
method infix:sym<ff^>($/) { make $Operator_FlipFlop.new(:excl_rhs) }
method infix:sym<fff^>($/) { make $Operator_FlipFlop.new(:excl_rhs, :sedlike) }
method infix:sym<^ff>($/) { make $Operator_FlipFlop.new(:excl_lhs) }
method infix:sym<^fff>($/) { make $Operator_FlipFlop.new(:excl_lhs, :sedlike) }
method infix:sym<^ff^>($/) { make $Operator_FlipFlop.new(:excl_lhs, :excl_rhs) }
method infix:sym<^fff^>($/) { make $Operator_FlipFlop.new(:excl_lhs,
    :excl_rhs, :sedlike) }
method infix:sym<~~> ($/) { make $Operator_SmartMatch.new }
method infix:sym<,>($/) { make $Operator_Comma.new }
method infix:sym<:=>($/) { make $Operator_Binding.new(:!readonly) }
method infix:sym<::=>($/) { make $Operator_Binding.new(:readonly) }
method infix:sym<&&>($/) { make $Operator_ShortCircuit.new(kind => '&&') }
method infix:sym<and>($/) { make $Operator_ShortCircuit.new(kind => '&&') }
method infix:sym<||>($/) { make $Operator_ShortCircuit.new(kind => '||') }
method infix:sym<or>($/) { make $Operator_ShortCircuit.new(kind => '||') }
method infix:sym<//>($/) { make $Operator_ShortCircuit.new(kind => '//') }
method infix:sym<orelse>($/) { make $Operator_ShortCircuit.new(kind => '//') }
method infix:sym<andthen>($/) { make $Operator_ShortCircuit.new(kind => 'andthen') }
method infix:sym<?? !!>($/) { make $Operator_Ternary.new(middle => $<EXPR>.ast) }
method infix:sym<.=> ($/) { make $Operator_DotEq.new }
method infix:does ($/) {
    make $Operator_Mixin.new(function => mklex($/, '&infix:<does>'));
}

method infix:but ($/) {
    make $Operator_Mixin.new(function => mklex($/, '&infix:<but>'));
}

method prefix:temp ($/) { make $Operator_Temp.new }
method prefix:let ($/) { make $Operator_Let.new }

method statement_control:TEMP ($/) {
    $*CURLEX<!sub>.noninlinable;
    make $OpTemporize.new(pos=>$/, mode => 2,
        var => self.inliney_call($/, $<block>.ast));
}

method INFIX($/) {
    my $fn = $<infix>.ast;
    my ($st,$lhs,$rhs) = self.whatever_precheck($fn, $<left>.ast, $<right>.ast);

    make $fn.with_args($/, $lhs, $rhs);

    if $fn.assignish {
        # Assignments to has and state declarators are rewritten into
        # an appropriate phaser
        if $lhs.^isa($OpStateDecl) {
            my $cv = self.gensym;
            $*CURLEX<!sub>.add_state_name(Str, $cv);
            make mklet($lhs, -> $ll {
                $OpStatementList.new(pos=>$/, children => [
                    $OpStart.new(condvar => $cv, body =>
                        $fn.with_args($/, $ll, $rhs)),
                    $ll]) });
        }
        elsif $lhs.^isa($OpAttribute) {
            my $init = self.thunk_sub($rhs,
                :name($lhs.initializer.name ~ " init"));
            $init.set_outervar(my $ov = self.gensym);
            $*CURLEX<!sub>.add_my_sub($ov, $init);
            $lhs.initializer.add_initializer($lhs.name, $init);
            make $OpStatementList.new;
        }
        elsif $lhs.^isa($OpConstantDecl) && !$lhs.init {
            my $sig = substr($lhs.name, 0, 1);
            if defined '$@&%'.index($sig) {
                self.init_constant($lhs, self.docontext($/, $sig, $rhs));
            } else {
                self.init_constant($lhs, $rhs);
            }
            make $lhs;
        }
    }
    make self.whatever_postcheck($/, $st, $/.ast);
}

method CHAIN($/) {
    my @args;
    my @ops;
    my $i = 0;
    while True {
        push @args, $<chain>[$i++].ast;
        last if $i == $<chain>;
        push @ops,  $<chain>[$i++]<infix>.ast;
    }

    my ($st, @vargs) = self.whatever_precheck(@ops[0], @args);

    sub reduce($/) {
        my $fa = shift @vargs;
        my $fo = shift @ops;
        if @ops {
            mklet($fa, -> $lhs { mklet(@vargs[0], -> $rhs {
                @vargs[0] = $rhs;
                $OpShortCircuit.new(pos=>$/, kind => '&&', args =>
                    [ $fo.with_args($/, $lhs, $rhs), reduce($/) ]) }) })
        } else {
            $fo.with_args($/, $fa, @vargs[0])
        }
    }

    make self.whatever_postcheck($/, $st, reduce($/));
}

method LIST($/) {
    if $/.CURSOR.^isa(::STD::Regex) {
        self.LISTrx($/);
        return Nil;
    }
    # STD guarantees that all elements of delims have the same sym
    # the last item may have an ast of undef due to nulltermish
    my $fn = $<delims>[0].ast;
    my ($st, @pos) = self.whatever_precheck($fn,
        grep *.&defined, map *.ast, @( $<list> ));

    make self.whatever_postcheck($/, $st, $fn.with_args($/, @pos));
}

method POSTFIX($/) {
    # adverbs have undef ast
    my ($st, $arg) = self.whatever_precheck($<op>.ast // '', $<arg>.ast);
    if $<op><colonpair> {
        if $arg.^isa($OpCallLike) {
            make $arg.adverb($<op><colonpair>.ast<term>);
            make self.whatever_postcheck($/, $st, $/.ast);
        } else {
            $/.CURSOR.sorry("You can't adverb that");
            make $OpStatementList.new;
        }
        return Nil;
    }
    make $<op>.ast.with_args($/, $arg);
    make self.whatever_postcheck($/, $st, $/.ast);
}

method PREFIX($/) {
    my ($st, $arg) = self.whatever_precheck($<op>.ast, $<arg>.ast);
    make self.whatever_postcheck($/, $st, $<op>.ast.with_args($/, $arg));
}

method assign_meta_operator($ ) {}

method semilist_to_args($/) {
    if $/.ast > 1 {
        $/.CURSOR.sorry('Slice lookups NYI');
        return [];
    }
    my $al = $/.ast.[0];

    if !defined $al {
        return [];
    } elsif $al && $al.^isa($OpSimpleParcel) {
        return $al.items;
    } else {
        return [$al];
    }
}

method postcircumfix:sym<[ ]> ($/) {
    make $Operator.funop($/, '&postcircumfix:<[ ]>', 1, @( $<semilist>.ast ));
}
method postcircumfix:sym<{ }> ($/) {
    make $Operator.funop($/, '&postcircumfix:<{ }>', 1, @( $<semilist>.ast ));
}
method postcircumfix:sym«< >» ($/) {
    make $Operator.funop($/, '&postcircumfix:<{ }>', 1, $<nibble>.ast);
}
method postcircumfix:sym<( )> ($/) {
    make $Operator_PostCall.new(args => $<semiarglist>.ast[0]);
}

method postop($/) {
    make $<postcircumfix> ?? $<postcircumfix>.ast !! $<postfix>.ast;
}

method POST($/) {
    make $<dotty>.ast  if $<dotty>;
    make $<privop>.ast if $<privop>;
    make $<postop>.ast if $<postop>;

    for @$<postfix_prefix_meta_operator> {
        make $/.ast.meta_fun($/, '&hyperunary', 1);
    }
}

method PRE($/) {
    make $<prefix>.ast if $<prefix>;
    make $<prefix_circumfix_meta_operator>.ast
        if $<prefix_circumfix_meta_operator>;

    for @$<prefix_postfix_meta_operator> {
        make $/.ast.meta_fun($/, '&hyperunary', 1);
    }
}

method methodop($/) {
    if $<longname> {
        my ($c) = self.process_name($<longname>, :defer);
        make $Operator_Method.new(name => 'die');
        unless $c {
            $/.CURSOR.sorry("Method call requires a name");
            return;
        }
        if $c<iname> {
            $/.CURSOR.sorry("Indirectly named method calls NYI");
            return;
        }
        make $Operator_Method.new(name => $c<name>, package => $c<pkg>);
    } elsif $<quote> {
        make $Operator_Method.new(name => $<quote>.ast);
    } elsif $<variable> {
        make $Operator_Function.new(function =>
            self.do_variable_reference($/, $<variable>.ast));
    }

    $/.ast.args = $<args>.ast[0] if $<args>;
    $/.ast.args = $<arglist>.ast if $<arglist>;
}

method dottyopish ($/) { make $<term>.ast }
method dottyop($/) {
    if $<colonpair> {
        $/.CURSOR.sorry("Colonpair dotties NYI");
        make $Operator.funop($/, '&postfix:<++>', 1);
        return Nil;
    }

    make $<methodop>.ast if $<methodop>;
    make $<postop>.ast if $<postop>;
}

method privop($/) {
    if $<methodop>.ast.^isa($Operator_Function) {
        $/.CURSOR.sorry("! privacy marker only affects search, and as such is meaningless with a method reference.");
    } else {
        make $<methodop>.ast.clone(:private);
    }
}

method dotty:sym<.> ($/) { make $<dottyop>.ast }

method dotty:sym<.*> ($/) {
    if $<sym> eq '.=' {
        make $<dottyop>.ast.meta_assign;
        return;
    }
    if !$<dottyop>.ast.^isa($Operator_Method) || $<dottyop>.ast.meta {
        $/.CURSOR.sorry("Modified method calls can only be used with actual methods");
        make $Operator.funop($/, '&postfix:<++>', 1);
        return Nil;
    }
    if $<sym> eq '.^' || $<sym> eq '.?' {
        make $<dottyop>.ast.clone(:meta(substr($<sym>,1)));
    } else {
        $/.CURSOR.sorry("NYI dottyop form $<sym>");
        make $Operator.funop($/, '&postfix:<++>', 1);
    }
}

method coloncircumfix($/) { make $<circumfix>.ast }

sub qpvalue($ast) {
    if $ast.^isa($OpSimpleParcel) {
        join " ", map &qpvalue, @( $ast.items )
    } elsif $ast.^isa($OpStringLiteral) {
        $ast.text;
    } elsif $ast.^isa($OpParen) {
        qpvalue($ast.inside);
    } else {
        "XXX"
    }
}

method colonpair($/) {
    my $n;
    if $/ eq any <:_ :U :D :T> {
        $n = "";
    } elsif !$<v>.^isa(Match) {
        $n = ":" ~ ($<v> ?? '' !! '!') ~ $<k>;
    } else {
        $n = ":" ~ $<k> ~ "<" ~ qpvalue($<v>.ast // ~$<v>) ~ ">";
    }
    my $tv = $<v>.^isa(Match) ?? ($<v>.ast // ~$<v>) !!
        $OpLexical.new(name => $<v> ?? 'True' !! 'False');

    if $tv ~~ Str {
        if substr($<v>,1,1) eq '<' {
            $tv = mkcall($/, '&postcircumfix:<{ }>',
                $OpLexical.new(name => '$/'),
                $OpStringLiteral.new(text => ~$<k>));
        } else {
            $tv = self.do_variable_reference($/,
                { sigil => ~$<v><sigil>,
                    twigil => ($<v><twigil> ?? ~$<v><twigil> !! ''),
                    name => $<k> });
        }
    }

    make { ext => $n, term => $OpSimplePair.new(key => $<k>, value => $tv) };
}

method fatarrow($/) {
    make $OpSimplePair.new(key => ~$<key>, value => $<val>.ast);
}

my %_nowhatever = (map { ($_ => True) }, ('&infix:<,>', '&infix:<..>',
    '&infix:<...>', '&infix:<=>', '&infix:<xx>'));
method whatever_precheck($op, *@args) {
    return ([], @args) if ($op.^isa($Operator) ?? !$op.whatever_curry !! %_nowhatever{$op});
    my @vars;
    my @args_ = @args;
    for @args_ -> $a is rw {
        die "invalid undef here" if !$a;
        if $a.^isa($OpWhatever) {
            push @vars, $a.slot;
            $a = $OpLexical.new(name => $a.slot);
        } elsif $a.^isa($OpWhateverCode) {
            push @vars, @( $a.vars );
            $a = $OpCallSub.new(
                invocant => $OpLexical.new(name => $a.slot),
                args => [ map { $OpLexical.new(name => $_) }, @($a.vars) ]);
        }
    }
    $( @vars ), @args_;
}

method whatever_postcheck($/, $st, $term) {
    if @$st {
        my $slot = self.gensym;

        my $body = $*unit.create_sub(
            outer => $*CURLEX<!sub>,
            class => 'WhateverCode',
            in_class => $*CURLEX<!sub>.in_class,
            cur_pkg => $*CURLEX<!sub>.cur_pkg);

        $body.add_my_name($_, :noinit) for @$st;
        $body.set_signature($Sig.simple(@$st));
        $body.set_transparent;
        $body.finish($term);

        $*CURLEX<!sub>.add_my_sub($slot, $body);

        $OpWhateverCode.new(ops => Any, vars => $st, :$slot, pos=>$/);
    } else {
        $term;
    }
}

# term :: Op
method term:value ($/) { make $<value>.ast }

method package_var($/, $slot, $name, $path) {
    $/.CURSOR.trymop({
        $/.CURSOR.check_categorical($slot);
        my $ref = $path.^can('FALLBACK') ?? $path !!
            $*CURLEX<!sub>.compile_get_pkg(@$path, :auto);
        $*CURLEX<!sub>.add_common_name($slot, $ref, $name, |mnode($/));
        $/.CURSOR.mark_used($slot);
    });
    $OpLexical.new(pos=>$/, name => $slot);
}

method term:name ($/) {
    my ($name) = self.process_name($<longname>, :defer);

    if $<args> {
        $name<name>  = '&' ~ $name<name> if $name<name>;
        $name<iname> = mkstringycat($/, '&', $name<iname>) if $name<iname>;
    }

    if $name<iname> {
        make $OpIndirectVar.new(pos=>$/, name => $name<iname>);
    }
    elsif $name<pkg> {
        make self.package_var($/, self.gensym, $name<name>, $name<pkg>);
    }
    elsif self.is_pseudo_pkg($name<name>) {
        make $OpIndirectVar.new(pos=>$/,
            name => $OpStringLiteral.new(text => $name<name>));
    }
    else {
        make mklex($/, $name<name>);
    }

    my @pc = @( $<postcircumfix> );
    if @pc && @pc[0].substr(0,1) eq '[' {
        make mkcall($/, '&_param_role_inst', $/.ast, @( @pc[0].ast.args ));
        shift @pc;
    } elsif $<args> {
        my $sal = $<args>.ast // [];
        # TODO: support zero-D slicels

        if $sal > 1 {
            $/.CURSOR.sorry("Slicel lists are NYI");
            return;
        }

        make $OpCallSub.new(pos=>$/, invocant => $/.ast,
            args => $sal[0] // []);
    }

    if @pc {
        make @pc[0].ast.with_args($/, $/.ast);
    }
}

method check_type_args($/) {
    if $<postcircumfix> -> $pc {
        make mkcall($/, '&_param_role_inst', $/.ast, @( $pc.ast.args ));
    }
}
method term:identifier ($/) {
    my $id  = ~$<identifier>;
    my $sal = $<args> ?? ($<args>.ast // []) !! [];
    # TODO: support zero-D slicels

    if $sal > 1 {
        $/.CURSOR.sorry("Slicel lists are NYI");
        make $OpStatementList.new;
        return;
    }

    if self.is_pseudo_pkg($id) {
        make $OpIndirectVar.new(pos=>$/,
            name => $OpStringLiteral.new(text => $id));
        self.check_type_args($/);
        return;
    }
    my $is_name = $/.CURSOR.is_name(~$<identifier>);

    if $is_name && $<args>.chars == 0 {
        make mklex($/, $id);
        self.check_type_args($/);
        return;
    }

    my $args = $sal[0] // [];

    make mklex($/, $is_name ?? $id !! '&' ~ $id);
    self.check_type_args($/);
    make $OpCallSub.new(pos=>$/, invocant => $/.ast, :$args);
}

method term:sym<self> ($/) { make mklex($/, 'self') }
method term:circumfix ($/) { make $<circumfix>.ast }
method term:scope_declarator ($/) { make $<scope_declarator>.ast }
method term:multi_declarator ($/) { make $<multi_declarator>.ast }
method term:package_declarator ($/) { make $<package_declarator>.ast }
method term:routine_declarator ($/) { make $<routine_declarator>.ast }
method term:regex_declarator ($/) { make $<regex_declarator>.ast }
method term:type_declarator ($/) { make $<type_declarator>.ast }
method term:dotty ($/) { make $<dotty>.ast.with_args($/,
    $OpLexical.new(name => '$_')) }
method term:capterm ($/) { make $<capterm>.ast }
method term:sigterm ($/) { make $<sigterm>.ast }
method term:statement_prefix ($/) { make $<statement_prefix>.ast }
method term:variable ($/) {
    make self.do_variable_reference($/, $<variable>.ast);
}
method term:sym<...> ($/) { make $OpYada.new(pos=>$/, kind => '...') }
method term:sym<???> ($/) { make $OpYada.new(pos=>$/, kind => '???') }
method term:sym<!!!> ($/) { make $OpYada.new(pos=>$/, kind => '!!!') }
method term:sym<*> ($/) {
    make $OpWhatever.new(pos=>$/, slot => self.gensym)
}
method term:lambda ($/) {
    make self.block_expr($/, $<pblock>.ast);
}

method term:colonpair ($/) {
    if $<colonpair> > 1 {
        $/.CURSOR.sorry("Multi colonpair syntax not yet understood"); #XXX
        make $OpStatementList.new;
        return Nil;
    }
    make $<colonpair>[0].ast<term>;
}

method term:fatarrow ($/) { make $<fatarrow>.ast }

method term:reduce ($/) {
    my $assoc = $<op><O><assoc>;
    make $OpCallSub.new(pos=>$/,
        invocant => mklex($/, '&reduceop'),
        args => [
            mkbool($<triangle> ne ''), mkbool($assoc eq 'list'),
            mkbool($assoc eq 'right'), mkbool($assoc eq 'chain'),
            $<op>.ast.as_function($/), @( $<args>.ast.[0] // [] )
        ]);
}

method do_variable_reference($M, $v) {
    if $v<term> {
        return $v<term>;
    }

    my $tw = $v<twigil>;
    my $sl = $v<sigil> ~ $tw ~ $v<name>;
    my $list = $v<sigil> eq '@';
    my $hash = $v<sigil> eq '%';

    if defined($v<pkg>) && $tw ~~ /<[*=~?^:]>/ {
        $M.CURSOR.sorry("Twigil $tw cannot be used with qualified names");
        return $OpStatementList.new;
    }

    if $tw eq '!' {
        my $pclass;
        if $v<pkg> {
            $pclass = $v<pkg>;
        } elsif $*CURLEX<!sub>.lookup_lex($sl) {
            return mklex($M, $sl);
        } elsif $*CURLEX<!sub>.in_class -> $c {
            $pclass = $c;
        } else {
            $M.CURSOR.sorry("Cannot resolve class for private method");
        }
        if $pclass && !$pclass.trusts($*CURLEX<!sub>.cur_pkg) {
            $M.CURSOR.sorry("Cannot call private method '$v<name>' on $pclass.name() because it does not trust $*CURLEX<!sub>.cur_pkg.name()");
        }
        $OpGetSlot.new(pos=>$M, object => mklex($M, 'self'),
            type => $pclass, name => $sl);
    }
    elsif $tw eq '.' {
        if defined $v<pkg> {
            $M.CURSOR.sorry('$.Foo::bar syntax NYI');
            return $OpStatementList.new;
        }

        self.docontext($M, $v<sigil>, $OpCallMethod.new(pos=>$M,
            name => $v<name>, receiver => mklex($M, 'self')));
    }
    # no twigil in lex name for these
    elsif $tw eq '^' || $tw eq ':' {
        mklex($M, $v<sigil> ~ $v<name>, :$hash, :$list);
    }
    elsif $tw eq '*' {
        $OpContextVar.new(pos=>$M, name => $sl);
    }
    elsif $tw eq '' || $tw eq '?' {
        if defined($v<pkg>) {
            self.package_var($M, self.gensym, $sl, $v<pkg>);
        } elsif $tw eq '?' && $sl eq '$?POSITION' {
            mkcall($M, '&infix:<..^>',
                $OpNum.new(pos=>$M, value => [10, ~$M.from]),
                $OpNum.new(pos=>$M, value => [10, ~$M.to]));
        } elsif $tw eq '?' && $sl eq '$?LINE' {
            $OpNum.new(pos=>$M, value => [10, ~$M.cursor.lineof($M.from)]);
        } elsif $tw eq '?' && $sl eq '$?FILE' {
            $OpStringLiteral.new(pos=>$M, text => $*FILE<name>);
        } elsif $tw eq '?' && $sl eq '$?ORIG' {
            $OpStringLiteral.new(pos=>$M, text => $M.orig);
        } elsif $tw eq '?' && $sl eq '&?BLOCK' {
            $*CURLEX<!sub>.noninlinable;
            $OpGetBlock.new(pos=>$M)
        } elsif $tw eq '?' && $sl eq '&?ROUTINE' {
            $*CURLEX<!sub>.noninlinable;
            $OpGetBlock.new(pos=>$M, :routine)
        } else {
            mklex($M, $sl, :$hash, :$list);
        }
    }
    else {
        $M.CURSOR.sorry("Unhandled reference twigil $tw");
    }
}

method docontext($M, $sigil, $term) {
    if $sigil !~~ /<[\$\@\%\&]>/ {
        $M.CURSOR.sorry("Unhandled conext character $sigil");
    }
    my $method = ($sigil eq '$' || $sigil eq '&') ?? 'item' !!
                 ($sigil eq '@') ?? 'list' !!
                                   'hash';

    $OpBuiltin.new(pos=>$M, name => $method, args => [$term]);
}

method docontextif($/, $sigil, $op) {
    $sigil eq '$' ?? $op !! self.docontext($/, $sigil, $op)
}
method variable($/) {
    my $sigil =  $<sigil>  ?? ~$<sigil> !! substr(~$/, 0, 1);
    my $twigil = $<twigil> ?? $<twigil><sym> !! '';

    my ($name, $pkg);
    my ($dsosl) = $<desigilname> ?? $<desigilname>.ast !!
        $<sublongname> ?? $<sublongname>.ast !!
        $<longname> ?? self.process_name($<longname>, :defer) !!
        Any;
    if defined($dsosl<ind>) {
        make { term => self.docontext($/, $sigil, $dsosl<ind>) };
        return;
    } elsif defined($dsosl<iname>) {
        make { term => $OpIndirectVar.new(pos=>$/,
            name => mkstringycat($/, $sigil ~ $twigil, $dsosl<iname>)) };
        return;
    } elsif defined $dsosl {
        ($name, $pkg) = $dsosl<name pkg>;
    } elsif $<infixish> {
        make { term => $<infixish>.ast.as_function($/) };
        return;
    } elsif $<special_variable> {
        $name = substr(~$<special_variable>, 1);
    } elsif $<index> {
        make { capid => $<index>.ast, term =>
            self.docontextif($/, $sigil,
                mkcall($/, '&postcircumfix:<[ ]>',
                    $OpLexical.new(name => '$/'),
                    $OpNum.new(value => $<index>.ast)))
        };
        return Nil;
    } elsif $<postcircumfix>[0] {
        if $<postcircumfix>[0].reduced eq 'postcircumfix:sym<< >>' { #XXX fiddly
            make { capid => $<postcircumfix>[0].ast.args[0].text, term =>
                self.docontextif($/, $sigil,
                    mkcall($/, '&postcircumfix:<{ }>',
                        $OpLexical.new(name => '$/'),
                        @( $<postcircumfix>[0].ast.args)))
            };
            return;
        } else {
            if $<postcircumfix>[0].ast.args[0] -> $arg {
                make { term => self.docontext($/, $sigil, $arg) };
            } elsif $sigil eq '$' {
                make { term => $OpShortCircuit.new(pos=>$/, kind => '//',
                    args => [ $OpCallMethod.new(name => 'ast',
                                receiver => mklex($/, '$/')),
                              $OpCallMethod.new(name => 'Str',
                                receiver => mklex($/, '$/')) ] ) };
            } elsif $sigil eq any < @ % > {
                make { term => self.docontext($/, $sigil, mklex($/, '$/')) };
            } else {
                make { term => mklex($/, 'Mu') };
                $/.CURSOR.sorry("Missing argument for contextualizer");
            }
            return;
        }
    } else {
        $name = '';
    }

    make {
        sigil => $sigil, twigil => $twigil, name => $name, pkg => $pkg
    };
}

method special_variable:sym<$/> ($/) {}
method special_variable:sym<$!> ($/) {}
method special_variable:sym<$¢> ($/) {}

method param_sep ($/) {}

# :: { list : Bool, hash : Bool  slot : Maybe[Str], names : [Str] }
method named_param($/) {
    my %rt;
    if $<name> {
        if $<named_param> {
            %rt = %( $<named_param>.ast );
        } else {
            %rt = %( $<param_var>.ast );
            %rt<names> = []; # completely replace
        }
        %rt<names> = [ @( %rt<names> // [] ), ~$<name> ]
            unless %rt<names> && %rt<names>.grep(~$<name>);
    } else {
        %rt = %( $<param_var>.ast );
        if !%rt<names> {
            $/.CURSOR.sorry("Abbreviated named parameter must have a name");
        }
    }
    %rt<flags> +&= +^$Sig::POSITIONAL;
    make %rt;
}

# :: { flags : Int, slot : Maybe[Str] }
method param_var($/) {
    if $<signature> {
        make { slot => Any, names => [], subsig => $<signature>.ast,
            flags => $Sig::POSITIONAL +
                (substr($/,0,1) eq '[' ?? $Sig::IS_LIST !! 0) };
        return;
    }
    my $twigil = $<twigil> ?? ~$<twigil> !! '';
    my $sigil = ~$<sigil>;
    my $list = $sigil eq '@';
    my $hash = $sigil eq '%';
    my $name = $<name> ?? ~$<name> !! Any;

    my $slot;
    if $twigil eq '' {
        $slot = defined($name) ?? ($sigil ~ $name) !! Any;
    } elsif $twigil eq '*' {
        $slot = "$sigil*" ~ "$name";
    } else {
        $/.CURSOR.sorry("Unhandled parameter twigil $twigil");
        make { };
        return Nil;
    }

    if ($sigil ne '$' && $sigil ne '@' && $sigil ne '%' && $sigil ne '&') {
        $/.CURSOR.sorry('Non bare scalar targets NYI');
        make { }
        return Nil;
    }

    $/.CURSOR.trymop({
        $/.CURSOR.check_categorical($slot);
        $*CURLEX<!sub>.add_my_name($slot, :$list, :$hash, |mnode($/),
            noinit => ?($*SIGNUM)) if defined($slot);
    });

    make { :$slot, names => defined($name) ?? [ $name ] !! [],
        flags => ($list ?? $Sig::IS_LIST !! 0) + ($hash?? $Sig::IS_HASH !! 0) +
            $Sig::POSITIONAL };
}

method parameter($/) {
    my $sorry;
    my $p = $<param_var> // $<named_param>;
    my $p_ast = $p ?? $p.ast !! { names => [], flags => $Sig::POSITIONAL };
    my $flags = $p_ast<flags>;

    $flags +|= $Sig::READWRITE if $*SIGNUM && $*CURLEX<!rw_lambda>;

    for @( $<trait> ) -> $trait {
        if $trait.ast<rw> { $flags +|= $Sig::READWRITE }
        elsif $trait.ast<copy> { $flags +|= $Sig::IS_COPY }
        elsif $trait.ast<parcel> { $flags +|= $Sig::RWTRANS }
        elsif $trait.ast<readonly> { $flags +&= +^$Sig::READWRITE }
        else {
            $trait.CURSOR.sorry('Unhandled trait ' ~ $trait.ast.keys.[0]);
        }
    }

    my $default = $<default_value> ?? $<default_value>.ast !! Any;
    $default.set_name("$/ init") if $default;

    my $tag = $<quant> ~ ':' ~ $<kind>;
    if    $tag eq '**:*' { $sorry = "Slice parameters NYI" }
    elsif $tag eq '*:*'  { $flags +|= ($flags +& $Sig::IS_HASH) ?? $Sig::SLURPY_NAM !! $Sig::SLURPY_POS }
    elsif $tag eq '|:*'  { $flags +|= $Sig::SLURPY_CAP }
    elsif $tag eq '\\:!' { $flags +|= $Sig::RWTRANS }
    elsif $tag eq '\\:?' { $flags +|= ($Sig::RWTRANS + $Sig::OPTIONAL) }
    elsif $tag eq ':!'   { }
    elsif $tag eq ':*'   { $flags +|= $Sig::OPTIONAL }
    elsif $tag eq ':?'   { $flags +|= $Sig::OPTIONAL }
    elsif $tag eq '?:?'  { $flags +|= $Sig::OPTIONAL }
    elsif $tag eq '!:!'  { }
    elsif $tag eq '!:?'  { $flags +|= $Sig::OPTIONAL }
    elsif $tag eq '!:*'  { }
    else                 { $sorry = "Confusing parameters ($tag)" }
    if $sorry { $/.CURSOR.sorry($sorry); }

    if defined $p_ast<slot> {
        # TODO: type constraint here
    }

    make $SigParameter.new(name => ~$/, mdefault => $default,
        |$p_ast, :$flags);

    for @<type_constraint> -> $tc {
        if $tc.ast<where> {
            push ($/.ast.where //= []), self.thunk_sub($tc.ast<where>.ast);
        } elsif $tc.ast<value> {
            $/.ast.tclass = $tc.ast<value>.get_type;
            push ($/.ast.where //= []), self.thunk_sub(
                $OpGeneralConst.new(value => $tc.ast<value>));
        } else {
            $/.CURSOR.sorry("Parameter coercion NYI") if $tc.ast<as>;
            my $type = $tc.ast<type>;
            if $type.kind eq 'subset' {
                push ($/.ast.where //= []), self.thunk_sub(
                    $OpGeneralConst.new(value => $type.get_type_var));
                $type = $type.get_basetype while $type.kind eq 'subset';
            }
            $/.ast.tclass = $type;
            $/.ast.flags +|= $tc.ast<tmode>;
        }
    }

    for @<post_constraint> -> $pc {
        # XXX this doesn't seem to be specced anywhere, but it's
        # Rakudo-compatible and shouldn't hurt
        if $pc<bracket> {
            $/.ast.flags +&= +^$Sig::IS_HASH;
            $/.ast.flags +|= $Sig::IS_LIST;
        }

        if $pc<signature> -> $ssig {
            $ssig.CURSOR.sorry('Cannot have more than one sub-signature for a pparameter') if $/.ast.subsig;
            $/.ast.subsig = $pc<signature>.ast;
        } else {
            push ($/.ast.where //= []), self.thunk_sub($pc<EXPR>.ast);
        }
    }
}

# signatures exist in several syntactic contexts so just make an object for now
method signature($/) {
    if $<type_constraint> {
        # ignore for now
    }

    if $<param_var> {
        $<param_var>.ast<flags> +|= $Sig::SLURPY_PCL;
        my $sig = $Sig.new(params => [ $SigParameter.new(
                name => ~$<param_var>, |$<param_var>.ast) ]);
        $*CURLEX<!sub>.set_signature($sig) if $*SIGNUM;
        make $sig;
        return;
    }

    my @p = map *.ast, @( $<parameter> );
    my @ps = @( $<param_sep> );
    my $ign = False;
    loop (my $i = 0; $i < @p; $i++) {
        @p[$i].flags +|= $Sig::MULTI_IGNORED if $ign;
        if $i >= @ps {
        } elsif defined @ps[$i].index(':') {
            $/.CURSOR.sorry('Only the first parameter may be invocant') if $i;
            $*CURLEX<!sub>.add_my_name('self', :noinit, |mnode($/));
            @p[$i].flags +|= $Sig::INVOCANT
        } elsif defined @ps[$i].index(';;') {
            $ign = True;
        } elsif !defined @ps[$i].index(',') {
            $/.CURSOR.sorry("Parameter separator @ps[$i] NYI");
        }
    }

    state %mlike = (:Method, :Submethod, :Regex);
    if $*SIGNUM && %mlike{$*CURLEX<!sub>.class} &&
            (!@p || !(@p[0].flags +& $Sig::INVOCANT)) {
        $*CURLEX<!sub>.add_my_name('self', :noinit, |mnode($/));
        unshift @p, $SigParameter.new(name => 'self',
            flags => $Sig::INVOCANT + $Sig::POSITIONAL);
    }

    for @p {
        if !defined(.tclass) && $*SIGNUM {
            if (.flags +& $Sig::INVOCANT) && $*CURLEX<!sub>.methodof {
                my $cl = $*CURLEX<!sub>.methodof;
                # XXX type checking against roles NYI
                if $cl.kind eq none <role prole> {
                    .tclass = $cl;
                }
            } elsif !$*CURLEX<!sub>.is_routine {
                .tclass = $*CURLEX<!sub>.compile_get_pkg('Mu');
            }
        }
    }

    my $sig = $Sig.new(params => @p);
    $*CURLEX<!sub>.set_signature($sig) if $*SIGNUM;
    make $sig;
}

method multisig($/) {
    if $<signature> != 1 {
        $/.CURSOR.sorry("Multiple signatures NYI");
        return Nil;
    }
    make $<signature>[0].ast;
}

method cgopname($/) { }

method cgexp:name ($/) { make ~$<cgopname> }
method cgexp:p6exp ($/) { make $<statementlist>.ast }
method cgexp:decint ($/) { make $<decint>.ast }
method cgexp:quote ($/) {
    if !$<quote>.ast.^isa($OpStringLiteral) {
        $/.CURSOR.sorry("Strings used in CgOp code must be compile time constants");
        make "";
        return Nil;
    }
    make $<quote>.ast.text;
}

my %opshortcut = (
    '@'   => [ 'fetch' ],
    'l'   => [ 'letvar' ],
    'ns'  => [ 'newscalar' ],
    'nsw' => [ 'newrwscalar' ],
    's'   => [ 'str' ],
    'i'   => [ 'int' ],
    'b'   => [ 'bool' ],
    'd'   => [ 'double' ],
    '=='  => [ 'compare', '==' ], '!=' => [ 'compare', '!=' ],
    '>='  => [ 'compare', '>=' ], '<=' => [ 'compare', '<=' ],
    '<'   => [ 'compare', '<' ],  '>'  => [ 'compare', '>' ],
    '+'   => [ 'arith', '+' ],    '-'  => [ 'arith', '-' ],
    '*'   => [ 'arith', '*' ],    '/'  => [ 'arith', '/' ],
);

method cgexp:op ($/) {
    my $l = ~$<cgopname>;
    my @p = @( %opshortcut{$l} // [ $l ] );
    make [@p, map *.ast, @( $<cgexp> )];
}

method apostrophe($/) {}
method quibble($/) {
    if ($<babble><B>[0].hereinfo) {
        my $stub = $OpHereStub.new(node => Any);
        make $stub;
        $<babble><B>[0].hereinfo.[1][0] = sub ($delim, $lang, $/) { #OK
            my $nws    = (~$<stopper>).index($delim);
            my $prefix = (~$<stopper>).substr(0, $nws);

            self.nibbler($<nibbler>, $prefix);
            $stub.node = $<nibbler>.ast;
        };
    } else {
        make $<nibble>.ast;
    }
}
method sibble($/) {
    my $regex = self.op_for_regex($/, $<left>.ast);
    my $repl;
    if $<infixish> {
        if $<infixish> eq '=' {
            $repl = $<right>.ast;
        } elsif $<infixish>.ast ~~ $Operator_CompoundAssign {
            $repl = $<infixish>.ast.base.with_args($/,
                mkcall($/, '&prefix:<~>', $OpLexical.new(name => '$/')),
                $<right>.ast);
        } else {
            $/.CURSOR.sorry("Unhandled operator in substitution");
            $repl = mklex($/, 'Any');
        }
    } else {
        $repl = $<right>.ast;
    }
    $repl = self.block_expr($/, self.thunk_sub($repl));
    make $OpCallMethod.new(pos=>$/, receiver => mklex($/, '$_'),
        name => 'subst',
        args => [ $regex, $repl, self.extract_rx_adverbs(True, True, $/),
            $OpSimplePair.new(key => 'inplace', value => mklex($/,'True'))]);
}
method tribble($/) {}
method babble($/) {}
method quotepair($/) {}

method quotepair_term($/) {
    my $v;
    if $<v> ~~ Match {
        $v = $<v>.ast
    } elsif $<v> ~~ Str {
        $v = $OpNum.new(value => [10, $<v>]);
    } else {
        $v = mklex($/, $<v> ?? "True" !! "False");
    }
    $OpSimplePair.new(pos=>$/, key => $<k>, value => $v);
}

method extract_rx_adverbs($ismatch, $issubst, $match) {
    my $qps = ($match ~~ List) ?? $match !! $match<babble><quotepair>;
    return () if !$qps;

    my @ok;
    my @nyi;
    my @args;
    my @internal = < sigspace s ratchet r ignorecase i >;

    push @nyi, < ignoreaccent a bytes codes graphs chars Perl5 P5 >;

    if $issubst {
        push @nyi, < sameaccent aa samecase ii >;
        push @ok,  < g global p pos c continue x nth st nd rd th >;
    }

    if $ismatch {
        push @nyi, < overlap ov exhaustive ex global g rw >;
        push @ok, < continue c pos p nth st nd rd th >;
    }

    for @$qps -> $qp {
        if @internal.grep($qp<k>) {
            # handled by rx compiler
        } elsif @ok.grep($qp<k>) {
            push @args, self.quotepair_term($qp);
        } elsif @nyi.grep($qp<k>) {
            $qp.CURSOR.sorry("Regex modifier $qp<k> not yet implemented");
        } else {
            $qp.CURSOR.sorry("Regex modifier $qp<k> not valid on { $issubst ?? "substitution" !! $ismatch ?? "match" !! "regex literal" }");
        }
    }

    @args
}

method capture($ ) {}
method capterm($/) {
    my @args;
    if $<capture> {
        my $x = $<capture><EXPR>.ast;
        if $x.^isa($OpSimpleParcel) {
            @args = @($x.items);
        } else {
            @args = $x;
        }
    } elsif $<termish> {
        @args = $OpParen.new(pos=>$/, inside => $<termish>.ast);
    }
    make $OpCallSub.new(pos=>$/, invocant => mklex($/, '&_make_capture'),
        args => @args);
}

# We can't do much at blockoid reduce time because the context is unknown.
# Roles and subs need somewhat different code gen
method blockoid($/) {
    # XXX horrible cheat, but my data structures aren't up to the task of
    # $::UNIT being a class body &c.
    if $/ eq '{YOU_ARE_HERE}' {
        $*unit.set_bottom($*CURLEX<!sub>);
        $*CURLEX<!sub>.create_static_pad;
        $*CURLEX<!sub>.noninlinable;

        loop (my $l = $*CURLEX<!sub>; $l; $l.=outer) {
            # this isn't *quite* right, as it will cause declaring
            # anything more in the same scope to fail.
            # ... and we have to be careful not to mark anon_0 used
            # or installing this very block will fail!
            substr($_,0,4) ne 'anon' and $/.CURSOR.mark_used($_)
                for $l.lex_names;
        }

        make $OpYouAreHere.new(pos=>$/, unitname => $*UNITNAME);
    } else {
        make $<statementlist>.ast;
    }
}
method lambda($/) {}
method embeddedblock($/) {
    $*CURLEX<!sub>.finish($<statementlist>.ast);
    $*CURLEX<!sub>.set_signature($Sig.simple());
    make $*CURLEX<!sub>;
}

method sigil:sym<&> ($/) {}
method sigil:sym<@> ($/) {}
method sigil:sym<%> ($/) {}
method sigil:sym<$> ($/) {}

method twigil:sym<=> ($/) {}
method twigil:sym<!> ($/) {}
method twigil:sym<.> ($/) {}
method twigil:sym<~> ($/) {}
method twigil:sym<*> ($/) {}
method twigil:sym<?> ($/) {}
method twigil:sym<^> ($/) {}
method twigil:sym<:> ($/) {}

method terminator:sym<)> ($/) {}
method terminator:sym<;> ($/) {}
method terminator:sym<]> ($/) {}
method terminator:sym<}> ($/) {}
method terminator:sym<if> ($/) {}
method terminator:sym<unless> ($/) {}
method terminator:sym<for> ($/) {}
method terminator:sym<until> ($/) {}
method terminator:sym<then> ($/) {}
method terminator:sym<again> ($/) {}
method terminator:sym<repeat> ($/) {}
method terminator:sym<while> ($/) {}
method terminator:sym<else> ($/) {}
method terminator:sym<given> ($/) {}
method terminator:sym<when> ($/) {}
method terminator:sym« --> » ($/) {}
method terminator:sym<!!> ($/) {}

method stdstopper($/) {}
method unitstopper($/) {}
method eat_terminator($/) {}

method scoped($/) {
    make ($<declarator> // $<regex_declarator> //
        $<package_declarator> // $<multi_declarator>).ast;
}

# :: Op
method declarator($/) {
    if $<signature> {
        temp $*SCOPE ||= 'my';
        my $sub = $*CURLEX<!sub>;

        my @p = @( $<signature>.ast.params );
        # TODO: keep the original signature around somewhere := can find it
        # TODO: fanciness checks
        for @p -> \$param {
            my $slot = $param.slot;
            $sub.delete_lex($slot) if defined($slot);
            $slot //= self.gensym;
            $slot = self.gensym if $*SCOPE eq 'anon';
            my $list = ?($param.flags +& $Sig::IS_LIST);
            my $hash = ?($param.flags +& $Sig::IS_HASH);
            my $type = $param.tclass;

            if $*SCOPE eq 'state' {
                $sub.add_state_name($slot, self.gensym, :$list, :$hash,
                    typeconstraint => $type, |mnode($/));
                $param = $OpLexical.new(name => $slot, pos=>$/);
            } elsif $*SCOPE eq 'our' {
                $param = self.package_var($/, $slot, $slot, ['OUR']);
            } else {
                $sub.add_my_name($slot, :$list, :$hash,
                    typeconstraint => $type, |mnode($/));
                $param = $OpLexical.new(name => $slot, pos=>$/);
            }
        }
        make $OpSimpleParcel.new(pos=>$/, items => @p);
        make $OpStateDecl.new(pos=>$/, inside => $/.ast)
            if $*SCOPE eq 'state';
        return;
    }
    make $<variable_declarator> ?? $<variable_declarator>.ast !!
         $<routine_declarator>  ?? $<routine_declarator>.ast !!
         $<regex_declarator>    ?? $<regex_declarator>.ast !!
         $<type_declarator>.ast;
}

method scope_declarator:my ($/) { make $<scoped>.ast }
method scope_declarator:our ($/) { make $<scoped>.ast }
method scope_declarator:augment ($/) { make $<scoped>.ast }
method scope_declarator:supersede ($/) { make $<scoped>.ast }
method scope_declarator:has ($/) { make $<scoped>.ast }
method scope_declarator:state ($/) { make $<scoped>.ast }
method scope_declarator:anon ($/) { make $<scoped>.ast }

method multi_declarator:null  ($/) { make $<declarator>.ast }
method multi_declarator:multi ($/) { make ($<declarator> // $<routine_def>).ast}
method multi_declarator:proto ($/) { make ($<declarator> // $<routine_def>).ast}
method multi_declarator:only  ($/) { make ($<declarator> // $<routine_def>).ast}

method add_accessor($/, $name, $store_name, $lexical, $public) {
    my $ns = $*CURLEX<!sub>.body_of;
    my $nb = $*unit.create_sub(
        outer      => $*CURLEX<!sub>,
        name       => $name,
        cur_pkg    => $*CURLEX<!sub>.cur_pkg,
        class      => 'Method');
    $nb.set_transparent;
    $nb.add_my_name('self', noinit => True);
    $nb.set_signature($Sig.simple('self'));
    $nb.finish($lexical ?? $OpLexical.new(name => $store_name) !!
        $OpGetSlot.new(name => $store_name, type => $ns,
            object => $OpLexical.new(name => 'self')));
    $*CURLEX<!sub>.create_static_pad; # for protosub instance

    $/.CURSOR.trymop({
        my $ac = self.gensym;
        $nb.set_outervar($ac);
        $*CURLEX<!sub>.add_my_sub($ac, $nb, |mnode($/));
        $ns.add_method($*backend.sub_visibility("private"), $name, $nb,
            |mnode($/));
        if $public {
            $ns.add_method(0, $name, $nb, |mnode($/));
        }
    });
}

method add_attribute($/, $barename, $sigil, $accessor, $type) {
    my $ns = $*CURLEX<!sub>.body_of;
    my $name = $sigil ~ '!' ~ $barename;
    $/.CURSOR.sorry("Attribute $name declared outside of any class"),
        return $OpStatementList.new unless $ns;
    $/.CURSOR.sorry("Attribute $name declared in an augment"),
        return $OpStatementList.new if defined $*AUGMENT_BUFFER;

    if !$ns.CAN('add_attribute') {
        $/.CURSOR.sorry("A $ns.WHAT() cannot have attributes");
        return $OpStatementList.new
    }

    self.add_accessor($/, $barename, $name, False, $accessor);
    $/.CURSOR.trymop({
        $ns.add_attribute($name, $sigil, +$accessor, $type, |mnode($/));
    });

    $OpAttribute.new(name => $name, initializer => $ns);
}

method variable_declarator($/) {
    if $*MULTINESS {
        $/.CURSOR.sorry("Multi variables NYI");
    }

    my $scope = $*SCOPE // 'my';

    my $start;
    for @$<trait> -> $t {
        if $t.ast<rw> {
        } elsif $t.ast<dynamic> {
        } elsif $t.ast<start> && $*SCOPE eq 'state' {
            $start = $t.ast<start>;
        } else {
            $/.CURSOR.sorry("Trait $t.ast.keys.[0] not available on variables");
        }
    }
    if $<post_constraint> || $<postcircumfix> || $<semilist> {
        $/.CURSOR.sorry("Postconstraints, and shapes on variable declarators NYI");
    }

    if $scope eq 'augment' || $scope eq 'supersede' {
        $/.CURSOR.sorry("Illogical scope $scope for simple variable");
    }

    my $typeconstraint;
    if $*OFTYPE {
        my $of = $*OFTYPE.ast;
        $*OFTYPE.CURSOR.sorry("Only simple types may be attached to variables")
            if !$of<type> || $of<tmode> || $of<as>;
        $typeconstraint = $of<type> // self.get_Any;
        $/.CURSOR.sorry("Common variables are not unique definitions and may not have types") if $scope eq 'our';
    }

    my $v = $<variable>.ast;
    my $t = $v<twigil>;
    my $list = $v<sigil> && $v<sigil> eq '@';
    my $hash = $v<sigil> && $v<sigil> eq '%';
    if ($t && defined "?=~^:".index($t)) {
        $/.CURSOR.sorry("Variables with the $t twigil cannot be declared " ~
            "using $scope; they are created " ~
            ($t eq '?' ?? "using 'constant'." !!
             $t eq '=' ?? "by parsing POD blocks." !!
             $t eq '~' ?? "by 'slang' definitions." !!
             "automatically as parameters to the current block."));
    }

    if ($scope ne any <has our my>) && ($t eq '.' || $t eq '!') {
        $/.CURSOR.sorry("Twigil $t is only valid with scopes has, our, or my.");
        $scope = 'has';
    }

    if !defined($v<name>) && ($scope ne any < my anon state >) {
        $/.CURSOR.sorry("Scope $scope requires a name");
        $v<name> = "anon";
    }

    if defined($v<pkg>) || defined($v<iname>) {
        $/.CURSOR.sorry(":: syntax is only valid when referencing variables, not when defining them.");
    }

    my $name = defined($v<name>) ?? $v<sigil> ~ $v<twigil> ~ $v<name> !! "";
    # otherwise identical to my
    my $slot = ($scope eq 'anon' || !defined($v<name>))
        ?? self.gensym !! $name;

    if ($scope eq any <our my>) && $t eq any < . ! > {
        $slot = $name = $v<sigil> ~ '!' ~ $v<name>;
        self.add_accessor($/, $v<name>, $slot, True, $t eq '.');
    }

    if $scope eq 'has' {
        make self.add_attribute($/, $v<name>, $v<sigil>, $t eq '.',
            $typeconstraint);
    } elsif $scope eq 'state' {
        $/.CURSOR.trymop({
            $/.CURSOR.check_categorical($slot);
            $*CURLEX<!sub>.add_state_name($slot, self.gensym, :$list,
                :$hash, :$typeconstraint, |mnode($/));
        });
        make $OpStateDecl.new(pos=>$/, inside =>
            $OpLexical.new(pos=>$/, name => $slot, :$list, :$hash));
    } elsif $scope eq 'our' {
        make self.package_var($/, $slot, $slot, ['OUR']);
    } else {
        $/.CURSOR.trymop({
            $/.CURSOR.check_categorical($slot);
            $*CURLEX<!sub>.add_my_name($slot, :$list, :$hash,
                :$typeconstraint, |mnode($/));
        });
        make $OpLexical.new(pos=>$/, name => $slot, :$list, :$hash);
    }

    if $start {
        my $cv = self.gensym;
        $*CURLEX<!sub>.add_state_name(Str, $cv);
        make mklet($/.ast, -> $ll {
            $OpStatementList.new(pos=>$/, children => [
                $OpStart.new(condvar => $cv, body =>
                    self.inliney_call($/, $start, $ll)), $ll ]) });
    }
}

method trivial_eval($/, $ast) {
    if $ast.^isa($OpSimpleParcel) {
        [,] map { self.trivial_eval($/, $_) }, @( $ast.items )
    } elsif $ast.^isa($OpSimplePair) {
        $ast.key => self.trivial_eval($/, $ast.value)
    } elsif $ast.^isa($OpStringLiteral) {
        $ast.text;
    } elsif $ast.^isa($OpParen) {
        self.trivial_eval($/, $ast.inside);
    } elsif $ast.^isa($OpStatementList) {
        my @l = @( $ast.children ); pop @l;
        self.trivial_eval($/, $_) for @l;
        $ast.children ?? self.trivial_eval($/, $ast.children[*-1]) !! Nil;
    } elsif $ast.^isa($OpNum) && $ast.value !~~ Array {
        $ast.value.Num
    } elsif $ast.^isa($OpNum) && $ast.value ~~ Array && $ast.value[0] == 10 {
        (+$ast.value[1]).Int # well not quite
    } else {
        $/.CURSOR.sorry("Compile time expression is insufficiently trivial {$ast.WHAT.perl}");
        "XXX"
    }
}

method type_declarator:subset ($/) {
    my ($basetype) = self.process_name($*OFTYPE<longname>);
    $basetype //= $*CURLEX<!sub>.compile_get_pkg('CORE', 'Any');
    my @exports;

    for map *.ast, @$<trait> -> $t {
        if $t<export> {
            push @exports, @( $t<export> );
        } elsif $t<of> {
            $basetype = $t<of>;
        } else {
            $/.CURSOR.sorry("Unsupported subset trait $t.keys()");
        }
    }

    my $body = self.thunk_sub($<EXPR> ?? $<EXPR>.ast !! mklex($/, 'True'));

    my ($lexvar, $obj) = "Any";

    $/.CURSOR.trymop({
        ($lexvar, $obj) = self.do_new_package($/, scope => $*SCOPE,
            name => $<longname>, class => 'subset', :@exports);

        $*CURLEX<!sub>.create_static_pad;

        $obj.set_basetype($basetype);
        $obj.set_where($body);
    });

    make mklex($/, $lexvar);
}

method make_constant($/, $scope, $name) {
    # hints must be lexically scoped
    $scope := $scope || (substr($name,1,1) eq '?' ?? 'my' !! 'our');

    my $slot = ($scope eq 'my' || $scope eq 'our') ?? $name !! self.gensym;

    $/.CURSOR.trymop({
        $/.CURSOR.check_categorical($slot);
        if $scope eq 'our' {
            $*CURLEX<!sub>.add_common_name($slot, $*CURLEX<!sub>.cur_pkg,
                $name, |mnode($/));
        } else {
            $*CURLEX<!sub>.add_hint($slot, |mnode($/));
        }
    });

    $OpConstantDecl.new(pos=>$/, name => $slot, init => False);
}

method make_constant_into($/, $pkg, $name, $rhs) {
    my $slot = self.gensym;
    $/.CURSOR.trymop({
        $*CURLEX<!sub>.add_common_name($slot, $pkg, $name, |mnode($/));
    });
    self.init_constant($OpConstantDecl.new(pos=>$/, name => $slot,
        init => False), $rhs);
}

method init_constant($con, $rhs) {
    my $body = self.thunk_sub($rhs, name => "$con.name() init");
    $body.outer.create_static_pad;
    $body.run_BEGIN($con.name);
    $con.init = True;
    $con;
}

method type_declarator:constant ($/) {
    if $*MULTINESS {
        $/.CURSOR.sorry("Multi variables NYI");
    }
    my $name  = ~($<identifier> // $<variable> // self.gensym);

    make self.make_constant($/, $*SCOPE, $name);
}

# note: named and unnamed enums are quite different beasts
method type_declarator:enum ($/) {
    my $scope = $*SCOPE || 'our';

    my @exports;
    for map *.ast, @$<trait> -> $t {
        if $t<export> {
            push @exports, @( $t<export> );
        } else {
            $/.CURSOR.sorry("Unsupported enum trait $t.keys()");
        }
    }

    my @pairs = self.trivial_eval($/, $<term>.ast);
    my $last = -1;
    my ($has_ints, $has_strs);
    for @pairs {
        if $_ !~~ Pair {
            my $key = $_;
            my $value = $last.succ;
            $_ = $key => $value;
        }
        given $last = .value {
            when Int { $has_ints = True; }
            when Str { $has_strs = True; }
            default  { $/.CURSOR.sorry("Enum values must be Int or Str"); }
        }
    }
    if $has_ints && $has_strs {
        $/.CURSOR.sorry("Enum may not contain both Int and Str values");
    }

    my ($basetype) = self.process_name($*OFTYPE<longname>);
    $basetype //= $*CURLEX<!sub>.compile_get_pkg('CORE', $has_strs ?? 'Str' !! 'Int');
    my $kindtype = $has_strs ?? 'StrBasedEnum' !! 'IntBasedEnum';

    if $<name> && $<name>.reduced eq 'longname' && $scope ne 'anon' {
        # Longnamed enum is a kind of type definition

        my ($lexvar, $obj);
        $/.CURSOR.trymop({
            ($lexvar, $obj) = self.do_new_package($/, :$scope,
                class => 'class', name => $<longname>, :@exports);

            $obj.add_super($*CURLEX<!sub>.compile_get_pkg($kindtype));
            $obj.add_super($basetype);

            my $nb = $*unit.create_sub(
                outer      => $*CURLEX<!sub>,
                name       => $obj.name ~ '.enums',
                cur_pkg    => $*CURLEX<!sub>.cur_pkg,
                class      => 'Method');

            $nb.set_transparent;

            my $nbvar = self.gensym;
            $nb.add_my_name('self', noinit => True);
            $nb.set_signature($Sig.simple('self'));
            $nb.finish(self.init_constant(
                self.make_constant($/, 'anon', Any),
                $OpCallMethod.new(name => 'new',
                    receiver => mklex($/, 'EnumMap'), args => [$<term>.ast])));
            $*CURLEX<!sub>.create_static_pad;
            $*CURLEX<!sub>.add_my_sub($nbvar, $nb, |mnode($/));
            $obj.add_method(0, 'enums', $nb, |mnode($/));
            $obj.close;

            for @pairs {
                self.make_constant_into($/, $obj, .key, rhs =>
                    $OpCallSub.new(invocant => mklex($/, $lexvar),
                        args => [ $OpStringLiteral.new(text => .key) ]));
            }

            for @pairs {
                self.init_constant(self.make_constant($/, $scope, .key),
                    $OpCallSub.new(invocant => mklex($/, $lexvar),
                        args => [ $OpStringLiteral.new(text => .key) ]));
            }
        });

        make mklex($/, $lexvar);
    } else {
        make self.init_constant(
            self.make_constant($/, $<name> ?? $scope !! 'anon', ~$<name>),
            $OpCallMethod.new(pos=>$/, name => 'new',
                receiver => mklex($/, 'EnumMap'),
                args => [$<term>.ast])),
    }
}

method package_declarator:class ($/) { make $<package_def>.ast }
method package_declarator:grammar ($/) { make $<package_def>.ast }
method package_declarator:role ($/) { make $<package_def>.ast }
method package_declarator:slang ($/) { make $<package_def>.ast }
method package_declarator:module ($/) { make $<package_def>.ast }
method package_declarator:package ($/) { make $<package_def>.ast }
method package_declarator:knowhow ($/) { make $<package_def>.ast }

method package_declarator:sym<also> ($/) {
    self.process_block_traits($/, $<trait>);
    make $OpStatementList.new;
}

method package_declarator:require ($/) {
    if $<EXPR> {
        $/.CURSOR.sorry('Expressional forms of require NYI');
        make $OpStatementList.new;
        return Nil;
    }
    make $OpRequire.new(pos=>$/, unit => ~$<module_name>);
}

method package_declarator:trusts ($/) {
    if defined $<module_name>.ast<args> {
        $/.CURSOR.sorry("Cannot trust a specific role instance");
    }
    my ($trustee) = self.process_name($<module_name><longname>);
    $/.CURSOR.trymop({
        $*CURLEX<!sub>.cur_pkg.add_trustee($trustee) if $trustee;
    });
    make $OpStatementList.new;
}

method process_block_traits($/, @tr) {
    my $sub = $*CURLEX<!sub>;
    my $pack = $sub.body_of;
    for @tr -> $T {
        my $tr = $T.ast;
        if $pack && $tr<name> {
            my $super = $tr<name>;

            $T.CURSOR.sorry("superclass $super.name() declared outside of any class"),
                next unless $sub.body_of;
            $T.CURSOR.sorry("superclass $super.name() declared in an augment"),
                next if defined $*AUGMENT_BUFFER;
            $T.CURSOR.sorry("cannot declare a superclass in this kind of package"),
                next if !$pack.CAN('add_super');

            $T.CURSOR.trymop({
                $pack.add_super($super);
            });
        } elsif $pack && $tr<does> {
            my $role = $tr<does>;

            $T.CURSOR.sorry("role $role.name() used outside of any class"), next
                unless $sub.body_of;
            $T.CURSOR.sorry("role $role.name() used in an augment"),
                next if defined $*AUGMENT_BUFFER;
            $T.CURSOR.sorry("cannot use a role in this kind of package"),
                next if !$pack.CAN('add_role');

            $T.CURSOR.trymop({
                $pack.add_role($role);
            });
        } elsif $pack && $tr<export> {
            my @exports = @( $tr<export> );
            $sub.outer.add_exports($pack.name, $pack, @exports);
        } elsif !$pack && $tr<export> {
            my @exports = @( $tr<export> );
            $sub.outer.add_exports($sub.outervar, $sub, @exports);
            $sub.set_extend('exported', @exports);
            $sub.outer.create_static_pad;
            $/.CURSOR.mark_used($sub.outervar)
                if defined $sub.outervar;
        } elsif !$pack && $tr<nobinder> {
            $sub.set_signature(Any);
        } elsif !$pack && $tr<pure> {
            $sub.outer.create_static_pad;
            $sub.set_extend('pure', True);
        } elsif !$pack && grep { defined $tr{$_} }, <looser tighter equiv> {
            my $rel = $tr.keys.[0];
            my $to  = $tr.values.[0];
            $to = $to.inside if $to ~~ $OpParen;
            $to = $to.children[0] if $to ~~ $OpStatementList && $to.children == 1;

            my $oprec;
            if $to ~~ $OpLexical {
                $oprec = $T.CURSOR.function_O($to.name);
            } elsif $to ~~ $OpStringLiteral && $sub.name ~~ /^(\w+)\:\<.*\>$/ {
                $oprec = $T.CURSOR.cat_O(~$0, $to.text);
            } else {
                $T.CURSOR.sorry("Cannot interpret operator reference");
                next;
            }
            unless $sub.get_extend('prec') {
                $T.CURSOR.sorry("Target does not seem to be an operator");
                next;
            }
            unless $oprec {
                $T.CURSOR.sorry("No precedence available for reference target");
                next;
            }
            if $rel eq 'equiv' {
                $sub.set_extend('prec', $oprec.kv);
            } else {
                my %prec = $sub.get_extend('prec');
                %prec<prec> = $oprec.<prec>;
                %prec<prec> ~~ s/\=/<=/ if $rel eq 'looser';
                %prec<prec> ~~ s/\=/>=/ if $rel eq 'tighter';
                $sub.set_extend('prec', %prec.kv);
            }
        } elsif !$pack && $tr<assoc> {
            my $arg = ~self.trivial_eval($T, $tr<assoc>);
            my %prec = $sub.get_extend('prec');
            unless %prec {
                $T.CURSOR.sorry("Target does not seem to be an operator");
                next;
            }
            unless $arg eq any < left right non list unary chain > {
                $T.CURSOR.sorry("Invalid associativity $arg");
                next;
            }
            %prec<assoc> = $arg;
            $sub.set_extend('prec', %prec.kv);
        } elsif !$pack && $tr<Niecza::absprec> {
            my $arg = ~self.trivial_eval($T, $tr<Niecza::absprec>);
            my %prec = $sub.get_extend('prec');
            unless %prec {
                $T.CURSOR.sorry("Target does not seem to be an operator");
                next;
            }
            %prec<prec> = $arg;
            %prec<dba> = "like $sub.name()";
            $sub.set_extend('prec', %prec.kv);
        } elsif !$pack && $tr<Niecza::builtin> {
            $sub.set_extend('builtin',
                self.trivial_eval($T, $tr<Niecza::builtin>));
        } elsif !$pack && $tr<return_pass> {
            $sub.set_return_pass;
        } elsif !$pack && $tr<of> {
        } elsif !$pack && $tr<rw> {
        } elsif !$pack && $tr<unsafe> {
            $sub.set_unsafe;
        } else {
            $T.CURSOR.sorry("Unhandled trait $tr.keys[0] for this context");
        }
    }
}

# normally termish's ast is not used, but it becomes the used ast under
# nulltermish.
method termish($/) { make $<term>.ast }
method nulltermish($/) {}
method EXPR($/) { make $<root>.ast }
method modifier_expr($/) { make $<EXPR>.ast }
method default_value($/) { make self.thunk_sub($<EXPR>.ast) }
method thunk_sub($code, :$params = [], :$name, :$class, :$ltm) {
    my $n = $*unit.create_sub(
        name => $name // 'ANON',
        class => $class // 'Block',
        outer => $*CURLEX<!sub>,
        cur_pkg => $*CURLEX<!sub>.cur_pkg,
        in_class => $*CURLEX<!sub>.in_class);
    $n.set_transparent;
    $n.set_ltm($ltm) if $ltm;
    $n.add_my_name($_, :noinit) for @$params;
    $n.set_signature($Sig.simple(@$params));
    $n.finish($code);
    $n;
}

method arglist($/) {
    $/.CURSOR.sorry("Invocant handling is NYI") if $*INVOCANT_IS;
    my $x = $<EXPR> && $<EXPR>.ast;

    if !defined $x {
        make [];
    } elsif $x && $x.^isa($OpSimpleParcel) {
        make $x.items;
    } else {
        make [$x];
    }
}

method semiarglist($/) { make [ map *.ast, @( $<arglist> ) ] }

method args($/) {
    if $<moreargs> || $<semiarglist> && $<arglist> {
        $/.CURSOR.sorry("Interaction between semiargs and args is not understood");
        make [];
        return Nil;
    }

    make $<semiarglist> ?? $<semiarglist>.ast !!
        $<arglist> ?? [ $<arglist>.ast ] !! Any;
}

method statement($/) {
    if $<label> {
        make $OpLabelled.new(pos=>$/, name => $<label>.ast,
            stmt => $<statement>.ast);
        return;
    }

    make ($<statement_control> ?? $<statement_control>.ast !!
        $<EXPR> ?? $<EXPR>.ast !! $OpStatementList.new);

    if $<statement_mod_cond> {
        my ($sym, $exp) = @( $<statement_mod_cond>.ast );

        if $sym eq 'if' {
            make $OpConditional.new(pos=>$/, check => $exp,
                true => $/.ast, false => Any);
        } elsif $sym eq 'unless' {
            make $OpConditional.new(pos=>$/, check => $exp,
                false => $/.ast, true => Any);
        } elsif $sym eq 'when' {
            make $OpConditional.new(pos=>$/,
                check => $OpCallMethod.new(name => 'ACCEPTS',
                    receiver => $exp, positionals => [ mklex($/, '$_') ]),
                true => $/.ast, false => Any);
        } else {
            $/.CURSOR.sorry("Unhandled statement modifier $sym");
            make $OpStatementList.new;
            return Nil;
        }
    }

    if $<statement_mod_loop> {
        my ($sym, $exp) = @( $<statement_mod_loop>.ast );

        if $sym eq 'while' {
            make $OpWhileLoop.new(pos=>$/, check => $exp,
                body => $/.ast, until => False, once => False);
        } elsif $sym eq 'until' {
            make $OpWhileLoop.new(pos=>$/, check => $exp,
                body => $/.ast, until => True, once => False);
        } elsif $sym eq 'given' {
            make mktemptopic($/, $exp, $/.ast);
        } elsif $sym eq 'for' {
            # XXX laziness, comprehensions
            my $var = self.gensym;
            make $OpImmedForLoop.new(pos=>$/, :$var, source => $exp,
                sink => mktemptopic($/, $OpLetVar.new(name => $var), $/.ast));
        } else {
            $/.CURSOR.sorry("Unhandled statement modifier $sym");
            make $OpStatementList.new;
            return Nil;
        }
    }
}

method statement_mod_cond($/) { make [ ~$<sym>, $<modifier_expr>.ast ] }
method statement_mod_loop($/) { make [ ~$<sym>, $<modifier_expr>.ast ] }

method statement_mod_cond:if ($/)     { self.statement_mod_cond($/) }
method statement_mod_cond:unless ($/) { self.statement_mod_cond($/) }
method statement_mod_cond:when ($/)   { self.statement_mod_cond($/) }
method statement_mod_loop:while ($/)  { self.statement_mod_loop($/) }
method statement_mod_loop:until ($/)  { self.statement_mod_loop($/) }
method statement_mod_loop:for ($/)    { self.statement_mod_loop($/) }
method statement_mod_loop:given ($/)  { self.statement_mod_loop($/) }

method statementlist($/) {
    make $OpStatementList.new(pos=>$/, children =>
        [ map *.statement_level, map *.ast, @( $<statement> ) ]);
}

method semilist($/) { make [ map *.ast, @( $<statement> ) ] }

method module_name:normal ($/) {
    # name-extension stuff is just ignored on module names for now
    make {
        name => ~$<longname><name>,
        args => $<arglist> ?? $<arglist>.ast !! Any };
}

# passes the $cond to the $block if it accepts a parameter, otherwise just
# runs the block.  Hack - we consider a block to have a used parameter
# iff it has a lambda symbol.
method if_block($/, $cond, $pb) {
    if defined $pb<lambda> {
        make self.inliney_call($/, $pb.ast, $cond);
    } else {
        make self.inliney_call($/, $pb.ast);
    }
}

# This handles the branches of an if statement by induction.  At least one
# if must be provided, since "else -> $x { }" needs the previous value.
method if_branches($/, *@branches) {
    my $branch = shift @branches;
    mklet($branch.ast[0], -> $cond {
        $OpConditional.new(pos=>$/, check => $cond,
            true  => self.if_block($/, $cond, $branch<pblock>),
            false => @branches ?? self.if_branches($/, @branches) !!
                $<else> ?? self.if_block($/, $cond, $<else>) !!
                Any);
    });
}

method statement_control:if ($/) {
    make self.if_branches($/, $<xblock>, @( $<elsif> ));
}

method statement_control:unless ($/) {
    make mklet($<xblock>.ast[0], -> $cond {
        $OpConditional.new(pos=>$/, check => $cond,
            false => self.if_block($/, $cond, $<xblock><pblock>)) });
}

# Hack - $OpWhileLoop binds the condition to "!cond"
method statement_control:while ($/) {
    make $OpWhileLoop.new(pos=>$/, check => $<xblock>.ast[0],
        body => self.if_block($/, $OpLetVar.new(name => '!cond'),
            $<xblock><pblock>), :!until, :!once,
            :need_cond(defined $<xblock><pblock><lambda>));
}

method statement_control:until ($/) {
    make $OpWhileLoop.new(pos=>$/, check => $<xblock>.ast[0],
        body => self.if_block($/, $OpLetVar.new(name => '!cond'),
            $<xblock><pblock>), :until, :!once,
            :need_cond(defined $<xblock><pblock><lambda>));
}

method statement_control:repeat ($/) {
    my $until = $<wu> eq 'until';
    my $check = $<xblock> ?? $<xblock>.ast[0] !! $<EXPR>.ast;
    my $pb = $<xblock> ?? $<xblock><pblock> !! $<pblock>;
    my $body  = self.if_block($/, $OpLetVar.new(name => '!cond'), $pb);
    make $OpWhileLoop.new(pos=>$/, :$check, :$until, :$body, :once,
            :need_cond(defined $pb<lambda>));
}

method statement_control:loop ($/) {
    my $body = self.inliney_call($/, $<block>.ast);
    # XXX wrong interpretation
    my $init = $0 && $0<e1> ?? $0<e1>.ast !! Any;
    my $cond = $0 && $0<e2> ?? $0<e2>.ast !! Any;
    my $step = $0 && $0<e3> ?? $0<e3>.ast !! Any;

    make $OpGeneralLoop.new(pos=>$/, :$body, :$init, :$cond, :$step);
}

method statement_control:for ($/) {
    make $OpForLoop.new(pos=>$/, source => $<xblock>.ast[0],
        sink => self.block_expr($/, $<xblock>.ast[1]).name);
}

method statement_control:given ($/) {
    make self.inliney_call($/, $<xblock>.ast[1], $<xblock>.ast[0]);
}

method statement_control:default ($/) {
    make $OpWhen.new(pos=>$/, match => mklex($/, 'True'),
        body => self.inliney_call($/, $<block>.ast));
}

method statement_control:when ($/) {
    make $OpWhen.new(pos=>$/, match => $<xblock>.ast[0],
        body => self.inliney_call($/, $<xblock>.ast[1]));
}

method statement_control:use ($/) {
    make $OpStatementList.new;
    return if $<version>; # just ignore these

    my $name = $<module_name>.ast<name>;
    my $args = $<arglist> ?? $<arglist>.ast !! [];

    if defined $<module_name>.ast.<args> {
        $/.CURSOR.sorry("'use' of an instantiated role not yet understood");
        return;
    }

    if $args {
        $/.CURSOR.sorry("'use' with arguments NYI");
        return;
    }

    if ($name eq 'MONKEY_TYPING' || $name eq 'fatal' || $name eq 'lib') {
        return;
    }

    my $u2 = $*unit.need_unit($name);

    my $module = $u2.mainline.compile_get_pkg($name.split('::'));
    my $exp;
    try $exp = $*unit.rel_pkg($module, 'EXPORT', 'DEFAULT');

    # in the :: case, $module will usually be visible via GLOBAL
    if !defined($name.index('::')) {
        $*CURLEX<!sub>.add_my_stash($name, $module);
    }

    return unless $exp;

    my $h = $/.CURSOR;
    my $mnode = mnode($/);
    for $*unit.list_stash($exp.who) -> $uname, $obj {
        if !$obj || $obj.kind eq 'sub' {
            $*CURLEX<!sub>.add_common_name($uname, $exp, $uname);
        } else {
            $*CURLEX<!sub>.add_my_stash($uname, $obj);
        }
        if $uname ~~ /\:\(/ && !$*CURLEX<!sub>.has_lexical($/.prematch) {
            $*CURLEX<!sub>.create_static_pad;
            $*CURLEX<!sub>.add_dispatcher($/.prematch, |$mnode);
        }

        $h.check_categorical($uname);
        $h = $h.cursor_fresh(%*LANG<MAIN>);
    }
}

method do_new_package($/, :$sub = $*CURLEX<!sub>, :$scope!, :$name!, :$class!,
        :$exports) {

    $scope := $scope || 'our';
    if $scope ne 'our' && $scope ne 'my' && $scope ne 'anon' {
        $/.CURSOR.sorry("Invalid packageoid scope $scope");
        $scope := 'anon';
    }

    my ($pkg, $head) = self.process_name($name, :declaring, :clean);

    if defined($pkg) && $scope ne 'our' {
        $/.CURSOR.sorry("Pathed definitions require our scope");
        $scope := 'our';
    }

    if !$head {
        $scope := 'anon';
        $head  := 'ANON';
    }

    my $npkg;
    my $lexname;
    $/.CURSOR.trymop({
        my $old;
        if $scope ne 'anon' && !$pkg && $sub.has_lexical($head) {
            my @linfo = $sub.lookup_lex($head);
            die "Cannot resume definition - $head not a packageoid"
                unless @linfo[0] eq 'package';
            $old = @linfo[4];
        } elsif defined $pkg {
            $old = $*unit.get($pkg.who, $head);
        }

        my $lexed_already;

        if $old && $old.kind eq $class && !$old.closed {
            $npkg = $old;
            $lexed_already = True;
        } elsif $scope eq 'our' {
            my $opkg = $pkg // $sub.cur_pkg;
            $npkg = $*unit.create_type(name => $head, :$class,
                who => $opkg.who ~ '::' ~ $head);
            $*unit.bind($opkg.who, $head, $npkg, |mnode($/));
        } else {
            my $id = $*unit.anon_stash;
            $npkg = $*unit.create_type(name => $head, :$class,
                who => "::$id");
            $*unit.bind("", $id, $npkg, |mnode($/));
        }

        $lexname = (!$lexed_already && $scope ne 'anon' && !defined($pkg))
            ?? $head !! self.gensym;

        $sub.add_my_stash($lexname, $npkg, |mnode($/));
        $sub.add_exports($head, $npkg, @$exports) if $exports;
    });

    $lexname, $npkg
}

method open_package_def($, $/ = $*cursor) {
    my $sub = $*CURLEX<!sub>;

    if $*MULTINESS {
        $/.CURSOR.sorry("Multi variables NYI");
    }

    if $*SCOPE eq 'augment' {
        my ($obj) = self.process_name($<longname>, :clean);
        $*AUGMENT_BUFFER = [];

        $/.CURSOR.trymop({
            die "Augment requires a target" unless $obj;
            die "Illegal augment of a role" if $obj.kind eq 'role' | 'prole';

            $sub.set_body_of($obj);
            $sub.set_in_class($obj);
            $sub.set_cur_pkg($obj);
            $sub.set_name("augment-$obj.name()");
        });
    } else {
        my $class = $*PKGDECL;
        if $class eq 'role' {
            my $sig = $<signature> ?? $<signature>.ast !! $Sig.simple();
            unshift $sig.params, $SigParameter.simple('$?CLASS');
            $sub.add_my_name('$?CLASS', :noinit);
            $sub.set_signature($sig);
            $class = 'prole';
        }

        $/.CURSOR.trymop({
            my ($lexvar, $obj) = self.do_new_package($/, sub => $sub.outer,
                :$class, name => $<longname>, scope => $*SCOPE);

            $sub.set_outervar($lexvar);
            $sub.set_body_of($obj);
            $sub.set_in_class($obj);
            $sub.set_cur_pkg($obj);

            self.process_block_traits($/, $<trait>);
            $sub.set_name($*PKGDECL ~ "-" ~ $obj.name);
        });
    }
}

method package_def ($/) {
    my $sub = $*CURLEX<!sub>;
    my $obj = $sub.body_of;

    my $bodyvar = self.gensym;
    $sub.outer.add_my_sub($bodyvar, $sub);
    my $ast = ($<blockoid> // $<statementlist>).ast;

    if defined $*AUGMENT_BUFFER {
        # generate an INIT block to do the augment
        my $ph = $*unit.create_sub(
            outer      => $sub,
            cur_pkg    => $sub.cur_pkg,
            name       => "phaser-$sub.name()",
            class      => 'Code',
            run_once   => $sub.run_once);

        my @ops;
        for @( $*AUGMENT_BUFFER ) -> $mode, $name, $sym {
            push @ops, $CgOp._addmethod($CgOp.letvar('!mo'), $mode,
                $CgOp.str($name), $CgOp.fetch($CgOp.scopedlex($sym)));
        }
        my $fin = $CgOp.letn('!mo', $CgOp.class_ref('mo', $obj),
            @ops, $CgOp._invalidate($CgOp.letvar('!mo')), $CgOp.corelex('Nil'));

        $ph.finish($OpCgOp.new(op => $fin));
        $sub.create_static_pad;
        $ph.set_phaser($*backend.phaser('INIT'));

        make $OpCallSub.new(pos=>$/, invocant => mklex($/, $bodyvar));
    }
    else {
        if $<stub> {
            $*unit.stub_stash($/.from, $obj);

            make mklex($/, $*CURLEX<!sub>.outervar);
        }
        else {
            $/.CURSOR.trymop({ $obj.close; });

            if $obj.kind eq 'prole' {
                # return the frame object so that role instantiation can
                # find the cloned methods
                $ast = $OpStatementList.new(pos=>$/, children => [
                    $ast, mkcall($/, '&callframe') ]);
                $sub.create_static_pad;
                $obj.set_instantiation_block($sub);

                make mklex($/, $*CURLEX<!sub>.outervar);
            } else {
                make $OpStatementList.new(pos=>$/, children => [
                    $OpCallSub.new(invocant => mklex($/, $bodyvar)),
                    $OpLexical.new(name => $*CURLEX<!sub>.outervar) ]);
            }
        }
    }

    $sub.finish($ast);
}

method trait_mod:will ($/) {
    make { ~$<identifier> => $<pblock>.ast };
}

method trait_mod:is ($/) {
    my $trait = ~$<longname>;
    my $noparm;

    if $/.CURSOR.is_name($trait) {
        my ($name) = self.process_name($<longname>);
        make { name => $name };
        $noparm = 'Superclasses cannot have parameters';
    } elsif $trait eq 'export' {
        make { export => [ 'DEFAULT', 'ALL' ] };
        $noparm = 'Export tags NYI';
    } elsif $trait eq 'endsym' {
        my $text;
        if !$<circumfix> || !$<circumfix>.ast.^isa($OpStringLiteral) {
            $/.CURSOR.sorry("Argument to endsym must be a literal string");
        } else {
            $text = $<circumfix>.ast.text;
        }
        make { endsym => $text };
    } elsif $trait eq 'rawcall' {
        make { nobinder => True };
    } elsif $trait eq 'return-pass' { # &return special
        make { return_pass => 1 };
    } elsif $trait eq 'parcel' {
        make { rwt => 1 };
    } elsif $<circumfix> {
        make { $trait => $<circumfix>.ast };
    } else {
        make { $trait => True };
    }

    if $noparm && $<circumfix> {
        $/.CURSOR.sorry($noparm);
    }
}

method trait_mod:does ($/) {
    my $does = $<typename>.ast;
    $*OFTYPE.CURSOR.sorry("Only simple types may be used here")
        if !$does<type> || $does<tmode> || $does<as>;
    make { does => $does<type> // self.get_Any };
}

method trait_mod:of ($/) {
    my $of = $<typename>.ast;
    $*OFTYPE.CURSOR.sorry("Only simple types may be used here")
        if !$of<type> || $of<tmode> || $of<as>;
    make { of => $of<type> // self.get_Any };
}

method trait ($/) {
    if $<colonpair> {
        $/.CURSOR.sorry('Colonpair traits NYI');
        make { };
        return Nil;
    }

    make $<trait_mod>.ast;
}

method routine_declarator:sub ($/) { make $<routine_def>.ast }
method routine_declarator:method ($/) { make $<method_def>.ast }
method routine_declarator:submethod ($/) { make $<method_def>.ast }

my $next_anon_id = 0;
method gensym() { 'anon_' ~ ($next_anon_id++) }
method genid()  { ($next_anon_id++) }

method block_expr($/, $pb) {
    my $name = self.gensym;
    $*CURLEX<!sub>.add_my_sub($name, $pb);
    mklex($/, $name);
}

method inliney_call($/, $block, *@parms) {
    my $sym = self.gensym;
    $*CURLEX<!sub>.add_my_sub($sym, $block);
    $OptBeta.make_call($sym, @parms);
}

# this is intended to be called after parsing the longname for a sub,
# but before the signature.  export, etc are handled by the sub/package
# trait handler
# Try to make binary ordering DTRT; also, global uniqueness
method multi_suffix() {
    my $num = self.genid;
    $*UNITNAME.subst('::','.',:g) ~ " " ~ chr(ord('a')+chars($num)) ~ $num
}

method install_sub($/, $sub, :$multiness is copy, :$scope is copy, :$class,
        :$longname, :$method_type is copy, :$contextual is copy) {

    $multiness ||= 'only';
    my ($pkg, $name) = self.process_name($longname, :declaring);

    if !$scope {
        if !defined($name) {
            $scope = 'anon';
        } elsif defined($pkg) {
            $scope = 'our';
        } elsif defined($method_type) {
            $scope = 'has';
        } else {
            $scope = 'my';
        }
    }

    if $class eq 'Regex' {
        my $/;
        $*CURLEX<!name> = $name;
        $*CURLEX<!cleanname !sym> =
            !defined($name) ?? (Str, Str) !!
            ($name ~~ /\:sym\<(.*)\>/) ?? ($name.substr(0, $/.from), ~$0) !!
            ($name ~~ /\:(\w+)/) ?? ($name.substr(0, $/.from), ~$0) !!
            ($name, Str);
        %*RX<sym> = $*CURLEX<!sym>;
        $multiness = 'multi' if defined $*CURLEX<!sym>;
        $*CURLEX<!multi> = $multiness;
    }

    if $scope ne 'my' && $scope ne 'our' && $scope ne 'anon' && $scope ne 'has' {
        $/.CURSOR.sorry("Illegal scope $scope for subroutine");
        $scope = 'anon';
    }

    if $scope eq 'has' && !defined($method_type) {
        $/.CURSOR.sorry('has scope-type is only valid for methods');
        $scope = 'anon';
    }

    if $scope ne 'anon' && !defined($name) {
        $/.CURSOR.sorry("Scope $scope requires a name");
        $scope = 'anon';
    }

    if $scope ne 'our' && defined($pkg) {
        $/.CURSOR.sorry("Double-colon-qualified subs must be our");
        $scope = 'our';
    }

    if $scope eq 'anon' && $multiness ne 'only' {
        $/.CURSOR.sorry("Multi routines must have a name");
        $multiness = 'only';
    }

    if $contextual && (defined($method_type) || $scope ne 'my') {
        $/.CURSOR.sorry("Context-named routines must by purely my-scoped");
        $contextual = False;
    }

    $method_type = Str if $scope eq 'anon';

    my $method_targ = $method_type && $sub.outer.body_of;
    if !$method_targ && defined($method_type) {
        $/.CURSOR.sorry("Methods must be used in some kind of package");
        $method_type = Str;
    }

    if $method_targ && !$method_targ.CAN('add_method') {
        $/.CURSOR.sorry("A {$method_targ.kind} cannot have methods added");
        $method_type = Str;
        $method_targ = Any;
    }

    if $name ~~ $Op && (!defined($method_type) || $scope ne 'has' ||
            $method_targ.kind ne 'prole') {
        $/.CURSOR.sorry("Computed names are only implemented for parametric roles");
        $name = "placeholder";
    }

    my $bindlex = $scope eq 'my' || ($scope eq 'our' && !$pkg);

    $sub.set_name(defined($method_type) ?? $method_targ.name ~ "." ~ $name !!
        ($name // 'ANON'));
    $sub.set_class($class);

    my $std = $/.CURSOR;
    {
        my $/;
        if $sub.name ~~ /^(\w+)\:\<(.*)\>$/ {
            my %new = %( $std.default_O(~$0, ~$1) );
            $sub.set_extend('prec', %new.kv);
        }
    }

    my Str $symbol;
    $/.CURSOR.trymop({
        if $bindlex && $class eq 'Regex' {
            $symbol = '&' ~ $name;
            my $proto = $symbol;
            { my $/; $proto ~~ s/\:.*//; }
            $sub.outer.add_dispatcher($proto, |mnode($/))
                if $multiness ne 'only' && !$sub.outer.has_lexical($proto);
            $symbol ~= ":(!proto)" if $multiness eq 'proto';
        } elsif $bindlex {
            $symbol = '&' ~ $name;
            $/.CURSOR.check_categorical($symbol);
            if $multiness ne 'only' && !$sub.outer.has_lexical($symbol) {
                $sub.outer.add_dispatcher($symbol, |mnode($/))
            }

            given $multiness {
                when 'multi' { $symbol ~= ":({ self.multi_suffix })"; }
                when 'proto' { $symbol ~= ":(!proto)"; }
                default {
                    $/.CURSOR.check_categorical($symbol);
                }
            }
        } else {
            $symbol = self.gensym;
        }

        $sub.set_outervar($symbol);
        $sub.set_methodof(defined($method_type) ?? $method_targ !! Any);
        $sub.outer.add_my_sub($symbol, $sub, |mnode($/));

        # make recursion easier
        if defined($name) && $scope eq 'anon' {
            $sub.add_alias('&' ~ $name, $symbol, |mnode($/));
            $/.CURSOR.mark_used('&' ~ $name); # no real need to use the alias
        }

        if $multiness ne 'only' || $scope eq 'our' || $method_type {
            $/.CURSOR.mark_used($symbol);
        }

        if defined($method_type) || $scope eq 'our' {
            $sub.outer.create_static_pad;
        }

        if defined($method_type) {
            my $mode = 0;
            given $method_type {
                when 'sub'      { $mode += 2 }
                when 'normal'   { $mode += 0 }
                when 'private'  { $mode += 1 }
                default         { die "Unimplemented method type $_" }
            }
            given $multiness {
                when 'only'     { $mode += 0 }
                when 'proto'    { $mode += 4 }
                when 'multi'    { $mode += 8 }
                default         { die "Unimplemented multiness $_" }
            }
            if defined $*AUGMENT_BUFFER {
                push $*AUGMENT_BUFFER, $mode, $name, $symbol;
            } else {
                $method_targ.add_method($mode, $name, $sub, |mnode($/));
            }
        }

        if $scope eq 'our' {
            $*unit.bind(($pkg // $sub.outer.cur_pkg).who,
                "&$name", $sub);
        }


        if $bindlex && $multiness eq 'multi' {
            my $pname = $symbol;
            $pname ~~ s/\:\( .*/:(!proto)/;
            if $sub.outer.has_lexical($pname) {
                my @oinfo = $sub.outer.lookup_lex($pname);
                if @oinfo && @oinfo[0] eq 'sub' && @oinfo[4].get_extend('exported') -> @ex {
                    $sub.outer.add_exports($symbol, $sub, @ex);
                }
            }
        }
    });
}

# always a sub, though sometimes it's an implied sub after multi/proto/only
method routine_def_1 ($, $/ = $*cursor) {
    self.install_sub($/, $*CURLEX<!sub>, scope => $*SCOPE, class => 'Sub',
        longname => $<deflongname>, multiness => $*MULTINESS,
        contextual => ($<sigil> && $<sigil> eq '&*'));
}

method routine_def_2 ($, $/ = $*cursor) {
    if $<multisig> > 1 {
        $/.CURSOR.sorry("You may only use *one* signature");
    }
    $*CURLEX<!sub>.set_signature($<multisig> ?? $<multisig>[0].ast !! Any);
    self.process_block_traits($/, $<trait>);
}

method method_def_1 ($, $/ = $*cursor) {
    my $type = $<type> ?? ~$<type> !! '';
    if $type ne '' && $*HAS_SELF eq 'partial' {
        $type = '';
        $/.CURSOR.sorry("Type symbols cannot be used with submethod");
    }

    self.install_sub($/, $*CURLEX<!sub>, scope => $*SCOPE,
        method_type => ($type eq '^' ?? 'meta' !! $type eq '!' ?? 'private' !!
            $*HAS_SELF eq 'partial' ?? 'sub' !! 'normal'),
        longname => $<longname>, multiness => $*MULTINESS,
        :class($*HAS_SELF eq 'partial' ?? 'Submethod' !! 'Method'));
}

method method_def_2 ($, $/ = $*cursor) {
    if $<multisig> > 1 {
        $/.CURSOR.sorry("You may only use *one* signature");
    }
    $*CURLEX<!sub>.set_signature($<multisig> ?? $<multisig>[0].ast !! Any);
    self.process_block_traits($/, $<trait>);
}

method is_dispatcher($blockoid) {
    $blockoid.Str ~~ m:pos(0) / '{' \s* '*' \s* '}' /;
}
method finish_method_routine ($/) {
    if self.is_dispatcher($<blockoid>) {
        $*CURLEX<!sub>.finish_dispatcher('multi');
    } else {
        $*CURLEX<!sub>.finish($<blockoid>.ast);
    }
    make $OpLexical.new(pos=>$/, name => $*CURLEX<!sub>.outervar);
}
method routine_def ($/) { self.finish_method_routine($/) }
method method_def ($/)  { self.finish_method_routine($/) }

method block($/) {
    $*CURLEX<!sub>.finish($<blockoid>.ast);
    make $*CURLEX<!sub>
}

# :: Body
method pblock($/) {
    #my $rw = $<lambda> && $<lambda> eq '<->'; TODO
    $*CURLEX<!sub>.finish($<blockoid>.ast);
    make $*CURLEX<!sub>;
}

method xblock($/) { make [ $<EXPR>.ast, $<pblock>.ast ] }

# returns Body of 0 args
method blast($/) {
    if $<block> {
        make $<block>.ast;
    } else {
        make self.thunk_sub($<statement>.ast.statement_level);
    }
}

method statement_prefix:do ($/) {
    make $OpDoOnceLoop.new(pos=>$/,
        body => self.inliney_call($/, $<blast>.ast));
}
method statement_prefix:gather ($/) {
    make $OpGather.new(pos=>$/,
        var => self.block_expr($/, $<blast>.ast).name);
}
method statement_prefix:try ($/) {
    make $OpTry.new(pos=>$/, body => self.inliney_call($/, $<blast>.ast));
}

method statement_prefix:START ($/) {
    my $cv = self.gensym;
    $*CURLEX<!sub>.add_state_name(Str, $cv);
    make $OpStart.new(pos=>$/, condvar => $cv, body =>
        self.inliney_call($/, $<blast>.ast));
}

sub phaser($/, $ph, :$unique, :$topic, :$csp) {
    my $sub = ($<blast> // $<block>).ast;

    if $unique {
        $/.CURSOR.sorry("Limit one $ph phaser per block, please.")
            if $sub.outer.contains_phaser($*backend.phaser($ph));
        my $code = ($<blast><statement> // $<blast><block><blockoid> // $<block><blockoid>).ast;
        # TODO avoid double finishing
        $sub.finish($OpCatchyWrapper.new(inner => $code));
    }

    $sub.outer.noninlinable;

    if $topic {
        $sub.has_lexical('$_') || $sub.add_my_name('$_');
        $sub.parameterize_topic;
        $sub.set_signature($Sig.simple('$_'));
    }
    $*CURLEX<!sub>.create_static_pad if $csp;
    $sub.set_phaser($*backend.phaser($ph));
    make $OpStatementList.new;
}

method statement_control:CATCH ($/) { phaser($/, 'CATCH', :unique, :topic) }
method statement_control:CONTROL ($/) { phaser($/, 'CONTROL', :unique, :topic) }
method statement_prefix:PRE ($/) { phaser($/, 'PRE') }
method statement_prefix:POST ($/) { phaser($/, 'POST', :topic) }
method statement_prefix:KEEP ($/) { phaser($/, 'KEEP', :topic) }
method statement_prefix:UNDO ($/) { phaser($/, 'UNDO', :topic) }
method statement_prefix:ENTER ($/) { phaser($/, 'ENTER') }
method statement_prefix:LEAVE ($/) { phaser($/, 'LEAVE', :topic) }

method statement_prefix:CHECK ($/) { phaser($/, 'CHECK', :csp) }
method statement_prefix:END ($/) { phaser($/, 'END', :csp) }
method statement_prefix:INIT ($/) { phaser($/, 'INIT', :csp) }

method statement_prefix:BEGIN ($/) {
    # MAJOR HACK - allows test code like BEGIN { @*INC.push: ... } to work
    repeat while False {
        my $c = ($<blast><statement> || $<blast><block><blockoid>).ast;

        last unless $c ~~ $OpStatementList;
        last unless $c.children == 1;
        my $d = $c.children.[0];
        last unless $d ~~ $OpCallMethod;
        last unless $d.receiver ~~ $OpContextVar;
        last unless $d.receiver.name eq '@*INC';
        last if $d.private || $d.ismeta;
        last unless $d.name eq any <push unshift>;
        last unless +$d.getargs == 1;
        last unless defined my $str = self.trivial_eval($/, $d.getargs.[0]);
        @*INC."$d.name()"($str);
        make $OpStatementList.new;
        return;
    }

    $*CURLEX<!sub>.create_static_pad;
    my $con = self.make_constant($/, 'anon', 'BEGIN');
    $<blast>.ast.run_BEGIN($con.name);
    $con.init = True;
    make $con;
}

method comp_unit($/) {
    $*CURLEX{'!sub'}.finish($<statementlist>.ast);

    make $*unit;
}

INIT { $Actions = NieczaActions }
