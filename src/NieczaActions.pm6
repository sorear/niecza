class NieczaActions;

use CgOp;
use Op;
use RxOp;
use Sig;
use CClass;
use OpHelpers;
use Operator;

# XXX Niecza  Needs improvement
method sym_categorical($/) { self.FALLBACK($<name>, $/) }
method bracket_categorical($/) { self.FALLBACK($<name>, $/) }
method FALLBACK($meth, $/) {
    my $S = $<sym>;

    if substr($meth,0,7) eq 'prefix:' {
        make Operator.funop($/, q:s'&prefix:<$S>', 1);
    } elsif substr($meth,0,14) eq 'postcircumfix:' {
        make Operator.funop($/, q:s'&postcircumfix:<$S>', 1, @( $<semilist>.ast ));
    } elsif substr($meth,0,10) eq 'circumfix:' {
        make mkcall($/, q:s'&circumfix:<$S>', @( $<semilist>.ast ));
    } elsif substr($meth,0,8) eq 'postfix:' {
        make Operator.funop($/, q:s'&postfix:<$S>', 1);
    } elsif substr($meth,0,6) eq 'infix:' {
        make Operator.funop($/, q:s'&infix:<$S>', 2);
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

# XXX It's wrong to be converting values into numbers at this stage, because
# it makes the output dependant on the host perl's numerics capability.
sub from_base($str, $base) {
    my $acc = 0;
    for $str.lc.comb -> $ch {
        next if $ch eq '_';
        $acc = $acc * $base + ($ch ge 'a' ?? ord($ch) - 87 !! ord($ch) - 48);
    }
    $acc
}

method decint($/) { make from_base($/, 10) }
method hexint($/) { make from_base($/, 16) }
method octint($/) { make from_base($/, 8) }
method binint($/) { make from_base($/, 2) }
method integer($/) {
    $<decint> andthen make [10, ~$<decint>];
    $<octint> andthen make [8,  ~$<octint>];
    $<hexint> andthen make [16, ~$<hexint>];
    $<binint> andthen make [2,  ~$<binint>];
}

method decints($/) { make [ map *.ast, @$<decint> ] }
method hexints($/) { make [ map *.ast, @$<hexint> ] }
method octints($/) { make [ map *.ast, @$<octint> ] }
method binints($/) { make [ map *.ast, @$<binint> ] }

method escale ($/) { }
method dec_number ($/) {
    if $<escale> { make +((~$/).comb(/<-[_]>/).join("")) }
    else { make [10, ~$/] }
}

method number($/) {
    my $child = $<integer> // $<dec_number> // $<rad_number>;
    make (defined($child) ?? $child.ast !!
        $/ eq 'NaN' ?? (0e0/0e0) !! Inf);
}

# Value :: Op
method value($ ) { }
# TODO: Implement the rest of the numeric hierarchy once MMD exists
method value:number ($/){ make ::Op::Num.new(|node($/), value => $<number>.ast)}
method value:quote ($/) { make $<quote>.ast }

# make ~$/ is default
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
    make ($<identifier> ?? $<identifier>.ast !! $<EXPR> ?? $<EXPR>.ast !! Any);
}

method typename($ ) { }
method type_constraint($ ) { }

# { dc: Bool, names: [Either String Op] }
method name($/) {
    my @names = map *.ast, @$<morename>;
    unshift @names, $<identifier>.ast if $<identifier>;
    make { dc => !$<identifier>, names => @names };
}

method longname($ ) { } # look at the children yourself
method deflongname($ ) { }

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

    for $defer ?? () !! @ns.grep(Op) {
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
            goto "dyn" if $_.^isa(Op) for @ns;
            my $pkg;
            my @tail = @ns;
            my $head = pop(@tail) ~ $ext;
            unless @tail {
                goto "dyn" if $head eq any < MY OUR CORE DYNAMIC GLOBAL CALLER OUTER UNIT SETTING PROCESS COMPILING PARENT CLR >;
                return { name => $head } unless @tail;
            }
            try { $pkg = $*CURLEX<!sub>.compile_get_pkg(@tail, :auto) };
            goto "dyn" unless $pkg;

            return { name => $head, pkg => $pkg };
dyn:
            my @bits = map { $_, '::' }, @ns;
            pop @bits if @bits;
            push @bits, '::' if $trail;
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
        ::Op::MakeCursor.new);
}

method op_for_regex($/, $rxop) {
    my @lift = $rxop.oplift;
    my ($orxop, $mb) = ::GLOBAL::OptRxSimple.run($rxop);
    my $sub = self.thunk_sub(::Op::RegexBody.new(|node($/),
            canback => $mb, pre => @lift, rxop => $orxop),
        class => 'Regex', params => ['self']);
    $sub.add_my_name('$/');
    self.block_expr($/, $sub);
}

method quote:sym</ /> ($/) { make self.op_for_regex($/, $<nibble>.ast) }
method quote:rx ($/) {
    self.extract_rx_adverbs(False, False, $<quibble>);
    make self.op_for_regex($/, $<quibble>.ast);
}
method quote:m  ($/) {
    make ::Op::CallMethod.new(|node($/), name => 'match',
            receiver => mklex($/, '$_'),
            args => [
                self.op_for_regex($/, $<quibble>.ast),
                self.extract_rx_adverbs(True, False, $<quibble>) ]);
}

method encapsulate_regex($/, $rxop, :$goal, :$passcut = False,
        :$passcap = False) {
    my @lift = $rxop.oplift;
    my $lad = $rxop.lad;
    my ($nrxop, $mb) = ::GLOBAL::OptRxSimple.run($rxop);
    if defined $goal {
        unshift @lift, ::Op::LexicalBind.new(|node($/), :name<$*GOAL>,
            rhs => ::Op::StringLiteral.new(text => $goal));
    }
    my $subop = self.thunk_sub(
        ::Op::RegexBody.new(canback => $mb, pre => @lift, :$passcut, :$passcap,
            rxop => $nrxop), ltm => $lad, class => 'Regex', params => ['self']);
    $/.CURSOR.trymop({ $subop.add_my_name('$*GOAL') if defined($goal); });
    $subop = ::Op::CallSub.new(|node($/), invocant => self.block_expr($/, $subop),
        positionals => [ ::Op::MakeCursor.new(|node($/)) ]);
    ::RxOp::Subrule.new(regex => $subop, :$passcap, _passcapzyg => $nrxop,
        _passcapltm => $lad);
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

    if $*CURLEX<!multi> eq 'proto' {
        if ($<signature> && $<signature>[0].ast.params != 1) ||
                !$<regex_block><onlystar> {
            $/.CURSOR.sorry('Only {*} protoregexes with no parameters are supported');
        }

        $ast = ::RxOp::ProtoRedis.new(name => $*CURLEX<!name>);
    }

    my @lift = $ast.oplift;
    my $ltm = ::GLOBAL::OptRxSimple.run_lad($ast.lad);
    $*CURLEX<!sub>.set_ltm($ltm);
    ($ast, my $mb) = ::GLOBAL::OptRxSimple.run($ast);
    $*CURLEX<!sub>.finish(::Op::RegexBody.new(|node($/), pre => @lift,
        name => ($*CURLEX<!name> // ''), rxop => $ast, canback => $mb));
    make ::Op::Lexical.new(|node($/), name => $*CURLEX<!sub>.outervar);
}

method regex_declarator:regex ($/) { make $<regex_def>.ast }
method regex_declarator:rule  ($/) { make $<regex_def>.ast }
method regex_declarator:token ($/) { make $<regex_def>.ast }

# :: RxOp
method atom($/) {
    if $<metachar> {
        make $<metachar>.ast;
    } else {
        make ::RxOp::String.new(text => ~$/,
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
                @z[1] = ::RxOp::Sequence.new(zyg => [
                    ::RxOp::Sigspace.new, @z[1], ::RxOp::Sigspace.new]);
            } else {
                push @z, ::RxOp::Sigspace.new;
            }
        }
        $atom = ::RxOp::Quantifier.new(min => $q<min>, max => $q<max>,
            nonlisty => $q<nonlisty>, closure => $q<closure>,
            opsep => $q<opsep>, zyg => [@z],
            minimal => ($q<mod> && $q<mod> eq '?'));
    }

    if defined($q<mod>) && $q<mod> eq '' {
        $atom = ::RxOp::Cut.new(zyg => [$atom]);
    }

    if defined $q<tilde> {
        my ($closer, $inner) = @( $q<tilde> );
        $closer = $closer.zyg[0] if $closer.^isa(::RxOp::Cut) &&
            $closer.zyg[0].^isa(::RxOp::String);
        if !$closer.^isa(::RxOp::String) {
            $/.CURSOR.sorry("Non-literal closers for ~ NYI");
            make ::RxOp::None.new;
            return Nil;
        }
        $inner = self.encapsulate_regex($/, $inner, passcut => True,
            goal => $closer.text, passcap => True);
        $atom = ::RxOp::Sequence.new(zyg => [$atom,
            ::RxOp::Tilde.new(closer => $closer.text, dba => %*RX<dba>,
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
    make ::RxOp::Sequence.new(zyg => [ map *.ast, @( $<quantified_atom> ) ]);
}

my %LISTrx_types = (
    '&'  => ::RxOp::Conj,
    '|'  => ::RxOp::Alt,
    '&&' => ::RxOp::SeqConj,
    '||' => ::RxOp::SeqAlt,
);

method LISTrx($/) {
    make %LISTrx_types{$<delims>[0]<sym>}.new(zyg =>
        [ map *.ast, @( $<list> ) ], dba => %*RX<dba>);
}

method regex_infix:sym<|> ($/) {}
method regex_infix:sym<||> ($/) {}
method regex_infix:sym<&> ($/) {}
method regex_infix:sym<&&> ($/) {}

method metachar:sigwhite ($/) {
    make (%*RX<s> ?? ::RxOp::Sigspace.new !! ::RxOp::Sequence.new);
}
method metachar:unsp ($/) { make ::RxOp::Sequence.new }

method metachar:sym<{ }> ($/) {
    $/.CURSOR.trymop({
        $<embeddedblock>.ast.add_my_name('$¢', :noinit, |mnode($/));
        $<embeddedblock>.ast.set_signature(Sig.simple('$¢'));
    });

    make ::RxOp::VoidBlock.new(block => self.inliney_call($/,
        $<embeddedblock>.ast, ::Op::MakeCursor.new(|node($/))));
}

method metachar:mod ($/) {
    # most of these have only parse-time effects
    make (($<mod_internal>.ast ~~ RxOp) ?? $<mod_internal>.ast !! ::RxOp::Sequence.new);
}

method metachar:sym<::> ($/) { make ::RxOp::CutLTM.new }
method metachar:sym«::>» ($/) { make ::RxOp::CutBrack.new }
method metachar:sym<:::> ($/) { make ::RxOp::CutRule.new }

method metachar:sym<[ ]> ($/) {
    make ::RxOp::ConfineLang.new(zyg => [$<nibbler>.ast]);
}

method metachar:sym<( )> ($/) {
    make self.rxcapturize($/, %*RX<paren>++,
        self.encapsulate_regex($/, $<nibbler>.ast, passcut => True));
}

method metachar:sym« <( » ($/) { make ::RxOp::MarkFrom.new }
method metachar:sym« )> » ($/) { make ::RxOp::MarkTo.new }
method metachar:sym« << » ($/) { make ::RxOp::ZeroWidth.new(type => '<<') }
method metachar:sym« >> » ($/) { make ::RxOp::ZeroWidth.new(type => '>>') }
method metachar:sym< « > ($/) { make ::RxOp::ZeroWidth.new(type => '<<') }
method metachar:sym< » > ($/) { make ::RxOp::ZeroWidth.new(type => '>>') }

method metachar:qw ($/) {
    my $cif = $<circumfix>.ast;
    my @words = $cif.^isa(::Op::Paren) ?? @( $cif.inside.items ) !! $cif;
    @words = map *.text, @words;

    make ::RxOp::Alt.new(zyg => [ map { ::RxOp::String.new(text => $_,
            igcase => %*RX<i>, igmark => %*RX<a>) }, @words ], dba => %*RX<dba>);
}

method metachar:sym«< >» ($/) { make $<assertion>.ast }
method metachar:sym<\\> ($/) {
    my $cc = $<backslash>.ast;
    make ($cc.^isa(CClass) ??
        ::RxOp::CClassElem.new(cc => $cc,
            igcase => %*RX<i>, igmark => %*RX<a>) !!
        ::RxOp::String.new(text => $cc,
            igcase => %*RX<i>, igmark => %*RX<a>));
}

method metachar:sym<.> ($/) { make ::RxOp::Any.new }
method metachar:sym<^> ($/) { make ::RxOp::ZeroWidth.new(type => '^'); }
method metachar:sym<^^> ($/) { make ::RxOp::ZeroWidth.new(type => '^^'); }
method metachar:sym<$> ($/) { make ::RxOp::ZeroWidth.new(type => '$'); }
method metachar:sym<$$> ($/) { make ::RxOp::ZeroWidth.new(type => '$$'); }

method metachar:sym<' '> ($/) {
    if ! $<quote>.ast.^isa(::Op::StringLiteral) {
        make ::RxOp::VarString.new(ops => self.rxembed($/, $<quote>.ast, True));
        return Nil;
    }
    make ::RxOp::String.new(text => $<quote>.ast.text, igcase => %*RX<i>,
        igmark => %*RX<a>);
}

method metachar:sym<" "> ($/) {
    if ! $<quote>.ast.^isa(::Op::StringLiteral) {
        make ::RxOp::VarString.new(ops => self.rxembed($/, $<quote>.ast, True));
        return Nil;
    }
    make ::RxOp::String.new(text => $<quote>.ast.text, igcase => %*RX<i>,
        igmark => %*RX<a>);
}

method metachar:var ($/) {
    sub _isnum { $_ ~~ /^\d+$/ }
    if $<binding> {
        my $a = $<binding><quantified_atom>.ast.uncut;
        my $cid = $<variable>.ast.<capid>;

        if !defined $cid {
            $/.CURSOR.sorry("Non-Match bindings NYI");
            make ::RxOp::Sequence.new;
            return Nil;
        }

        if $a.^isa(::RxOp::VoidBlock) {
            make ::RxOp::SaveValue.new(capid => $cid, block => $a.block);
            return Nil;
        }

        if _isnum($cid) {
            %*RX<paren> = $cid + 1;
        }

        make self.rxcapturize($/, $cid, $a);
        return Nil;
    }
    make ::RxOp::VarString.new(param => ~$<variable>,
        ops => self.rxembed($/, self.do_variable_reference($/, $<variable>.ast), True));
}

method rxcapturize($M, $name, $rxop is copy) {
    if !$rxop.^isa(::RxOp::Capturing) {
        # $<foo>=[...]
        $rxop = self.encapsulate_regex($M, $rxop, passcut => True,
            passcap => True);
    }

    # $<foo>=(...)
    # XXX might not quite be right
    if +$rxop.captures == 1 && $rxop.captures.[0] ~~ /^\d+$/ {
        return $rxop.clone(captures => [$name]);
    }

    return $rxop.clone(captures => [ $name, @( $rxop.captures ) ]);
}

method do_cclass($/) {
    my @cce = @( $<cclass_elem> );

    my $rxop;
    for @cce {
        my $sign = $_.<sign> ne '-';
        my $exp =
            ($_.<name> && substr($_.<name>,0,10) eq 'INTERNAL::') ??
                ::RxOp::CClassElem.new(cc => CClass.internal(substr($_.<name>,10))) !!
            $_.<quibble> ??
                ::RxOp::CClassElem.new(cc => $_.<quibble>.ast) !!
            ::RxOp::Subrule.new(captures => [], method => ~$_.<name>);

        if $exp.^isa(::RxOp::CClassElem) && (!$rxop || $rxop.^isa(::RxOp::CClassElem)) {
            if $sign {
                $rxop = $rxop ?? ::RxOp::CClassElem.new(cc => $exp.cc.plus($rxop.cc)) !! $exp;
            } else {
                $rxop = ::RxOp::CClassElem.new(cc => ($rxop ?? $rxop.cc !! $CClass::Full).minus($exp.cc));
            }
        } elsif $sign {
            $rxop = $rxop ?? ::RxOp::SeqAlt.new(zyg => [ $exp, $rxop ], dba => %*RX<dba>) !! $exp;
        } else {
            $rxop = ::RxOp::Sequence.new(zyg => [
                ::RxOp::NotBefore.new(zyg => [ $exp ]),
                $rxop // ::RxOp::Any.new]);
        }
    }

    make $rxop;
}

method decapturize($/) {
    if !$<assertion>.ast.^isa(::RxOp::Capturing) {
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

    if $<assertion> {
        make $<assertion>.ast;
    } elsif $name eq 'sym' {
        $/.CURSOR.sorry("<sym> is only valid in multiregexes")
            unless defined %*RX<sym>;
        make ::RxOp::Sym.new(igcase => %*RX<i>, igmark => %*RX<a>,
            text => %*RX<sym> // '', endsym => %*RX<endsym>);
    } elsif $name eq 'before' {
        make ::RxOp::Before.new(zyg => [$<nibbler>[0].ast]);
        return Nil;
    } elsif $name eq 'after' {
        my @l = $<nibbler>[0].ast.tocclist;
        if grep { !defined $_ }, @l {
            $/.CURSOR.sorry("Unsuppored elements in after list");
            make ::RxOp::Sequence.new;
            return Nil;
        }
        make ::RxOp::ZeroWidthCCs.new(neg => False, after => True, ccs => @l);
        return Nil;
    } elsif !$<nibbler> && !$<arglist> && !$pname<pkg> && !$pname<iname> {
        make ::RxOp::Subrule.new(method => $pname<name>);
    } else {
        my $args = $<nibbler> ??
            [ self.op_for_regex($/, $<nibbler>.ast) ] !!
            $<arglist> ?? $<arglist>.ast !! [];

        if $pname<iname> {
            $/.CURSOR.sorry('Indirect method calls NYI');
            $pname = {name => 'alpha'};
        }

        my $callop = ::Operator::Method.new(name => $pname<name>, :$args,
            package => $pname<pkg> && $pname<pkg>.xref)\
                .with_args($/, mklex($/, '$¢'));

        my $regex = self.rxembed($/, $callop, True);

        make ::RxOp::Subrule.new(regex => $regex);
    }
    make self.rxcapturize($/, ~$<longname>, $/.ast);
}

# actually we need a few more special cases here.
method assertion:variable ($/) {
    make ::RxOp::Subrule.new(|node($/), regex =>
        ::Op::CallSub.new(|node($/), invocant => $<variable>.ast,
            positionals => [ ::Op::MakeCursor.new(|node($/)) ]));
}

method assertion:method ($/) {
    if $<dottyop> {
        $/.CURSOR.sorry("Dottyop assertions NYI");
        make ::RxOp::None.new;
        return Nil;
    }
    make self.decapturize($/);
}

method assertion:sym<?> ($/) {
    if $<assertion> {
        make ::RxOp::Before.new(zyg => [self.decapturize($/)]);
    } else {
        make ::RxOp::Sequence.new;
    }
}

method assertion:sym<!> ($/) {
    if $<assertion> {
        make ::RxOp::NotBefore.new(zyg => [self.decapturize($/)]);
    } else {
        make ::RxOp::None.new;
    }
}

method assertion:sym<{ }> ($/) {
    $/.CURSOR.trymop({
        $<embeddedblock>.ast.add_my_name('$¢', :noinit, |mnode($/));
        $<embeddedblock>.ast.set_signature(Sig.simple('$¢'));
    });

    make ::RxOp::CheckBlock.new(block => self.inliney_call($/,
        $<embeddedblock>.ast, ::Op::MakeCursor.new(|node($/))));
}

method assertion:sym<[> ($/) { self.do_cclass($/) }
method assertion:sym<-> ($/) { self.do_cclass($/) }
method assertion:sym<+> ($/) { self.do_cclass($/) }

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
    make ::RxOp::Statement.new(stmt => $<statement>.ast );
}

method mod_internal:p6adv ($/) {
    my ($k, $v) = $<quotepair><k v>;

    if !$v.^isa(Match) {
        $/.CURSOR.sorry(":$k requires an expression argument");
        make ::RxOp::None.new;
        return Nil;
    }
    $v = $v.ast;

    if $k eq 'lang' {
        make ::RxOp::SetLang.new(expr => self.rxembed($/, $v, True));
    } elsif $k eq 'dba' {
        while True {
            if $v.^isa(::Op::Paren) { $v = $v.inside; redo }
            if $v.^isa(::Op::StatementList) && +$v.children == 1
                { $v = $v.children.[0]; redo }
            last;
        }
        if !$v.^isa(::Op::StringLiteral) {
            $/.CURSOR.sorry(":dba requires a literal string");
            make ::RxOp::None.new;
            return Nil;
        }
        %*RX<dba> = $v.text;
    }
}

sub post_backslash($/) {
    # XXX confine $/ resetting
    sub _isupper { $_ ~~ /^<[ A .. Z ]>$/ }
    sub _islower { $_ ~~ /^<[ a .. z ]>$/ }
    if _isupper($/) && _islower($<sym>) {
        if $/.ast.^isa(Str) && chars($/.ast) != 1 {
            $/.CURSOR.sorry("Improper attempt to negate a string");
            return Nil;
        }
        make CClass.enum($/.ast) if $/.ast.^isa(Str);
        make $/.ast.negate;
    }
}
method backslash:qq ($/) { make $<quote>.ast }
method backslash:x ($/) {
    if $<hexint> {
        make chr($<hexint>.ast);
    } else {
        make (join "", map *.&chr, @( $<hexints>.ast ));
    }
    post_backslash($/);
}
method backslash:o ($/) {
    if $<octint> {
        make chr($<octint>.ast);
    } else {
        make (join "", map *.&chr, @( $<octints>.ast ));
    }
    post_backslash($/);
}
method backslash:sym<\\> ($/) { make ~$<text> }
method backslash:stopper ($/) { make ~$<text> }
method backslash:unspace ($/) { make "" }
method backslash:misc ($/) { make ($<text> // ~$<litchar>) }
# XXX h, v, s, needs spec clarification
method backslash:sym<0> ($/) { make "\0" }
method backslash:a ($/) { make "\a"; post_backslash($/) }
method backslash:b ($/) { make "\b"; post_backslash($/) }
method backslash:d ($/) { make $CClass::Digit; post_backslash($/) }
method backslash:e ($/) { make "\e"; post_backslash($/) }
method backslash:f ($/) { make "\f"; post_backslash($/) }
method backslash:h ($/) { make $CClass::HSpace; post_backslash($/) }
method backslash:n ($/) { make "\n"; post_backslash($/) }
method backslash:r ($/) { make "\r"; post_backslash($/) }
method backslash:s ($/) { make $CClass::Space; post_backslash($/) }
method backslash:t ($/) { make "\t"; post_backslash($/) }
method backslash:v ($/) { make $CClass::VSpace; post_backslash($/) }
method backslash:w ($/) { make $CClass::Word; post_backslash($/) }

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
        my $ast = $n.ast;

        if $ast ~~ CClass {
            $n.CURSOR.sorry("Cannot use a character class in a string");
            $ast = "";
        }

        if $ast !~~ Op && defined($prefix) && $prefix ne "" {
            my $start_nl = !$n.from || "\r\n".index(
                substr($n.orig, $n.from-1, 1)).defined;
            $ast = $ast.split(/ ^^ [ <?{ $start_nl }> || <?after <[\r\n]> > ]
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
        if !$sl.^isa(::Op::StringLiteral) {
            make ::Op::CallMethod.new(|node($/), :name<words>, receiver => $sl);
        }
        else {
            my @tok = $sl.text.words;
            @tok = map { ::Op::StringLiteral.new(|node($/), text => $_) }, @tok;

            make ((@tok == 1) ?? @tok[0] !! ::Op::Paren.new(|node($/),
                inside => ::Op::SimpleParcel.new(|node($/), items => @tok)));
        }
    }
    elsif $post eq 'path' {
        # TODO could stand to be a lot fancier.
        make ::Op::CallMethod.new(|node($/), receiver => $/.ast, :name<IO>);
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
            if chars($b.ast) > 1 {
                $b.CURSOR.sorry("Cannot use >1 character strings as cclass elements");
                return $CClass::Empty;
            }
        }
        push @mstack, $b.CURSOR;
        push @cstack, $b.ast;
        if @cstack >= 2 && @cstack[*-2] ~~ RangeSymbol {
            if @cstack == 2 {
                @mstack[0].sorry(".. requires a left endpoint");
                return $CClass::Empty;
            }
            for 1, 3 -> $i {
                if @cstack[*-$i] !~~ Str {
                    @mstack[*-$i].sorry(".. endpoint must be a single character");
                    return $CClass::Empty;
                }
            }
            my $new = CClass.range(@cstack[*-3], @cstack[*-1]);
            pop(@cstack); pop(@cstack); pop(@cstack); push(@cstack, $new);
            pop(@mstack); pop(@mstack);
        }
    }
    if @cstack && @cstack[*-1] ~~ RangeSymbol {
        @mstack[*-1].sorry(".. requires a right endpoint");
        return $CClass::Empty;
    }
    my $ret = $CClass::Empty;
    for @cstack { $ret = $ret.plus($_) }
    $ret;
}

method nibbler($/, $prefix?) {
    sub iscclass($cur) {
        my $*CCSTATE = '';
        my $ok = False;
        # XXX XXX
        try { $cur.ccstate(".."); $ok = True };
        $ok
    }
    if $/.CURSOR.^isa(::STD::Regex) {
        make $<EXPR>.ast;
    } elsif $/.CURSOR.^isa(::NieczaGrammar::CgOp) {
        if $*SAFEMODE {
            $/.CURSOR.sorry('Q:CgOp not allowed in safe mode');
            make ::Op::StatementList.new;
            return Nil;
        }
        make ::Op::CgOp.new(|node($/), optree => $<cgexp>.ast);
    } elsif iscclass($/.CURSOR) {
        make self.process_tribble($<nibbles>);
    } else {
        make self.process_nibble($/, $<nibbles>, $prefix);
    }
}

method split_circumfix ($/) {
    my $sl = $<nibble>.ast;

    if !$sl.^isa(::Op::StringLiteral) {
        make ::Op::CallMethod.new(|node($/), name => "words", receiver => $sl);
        return Nil;
    }

    my @tok = $sl.text.words;
    @tok = map { ::Op::StringLiteral.new(|node($/), text => $_) }, @tok;

    make ((@tok == 1) ?? @tok[0] !!
        ::Op::SimpleParcel.new(|node($/), items => @tok));
}
method circumfix:sym«< >» ($/)   { make $<nibble>.ast }
method circumfix:sym«<< >>» ($/) { make $<nibble>.ast }
method circumfix:sym<« »> ($/)   { make $<nibble>.ast }

method circumfix:sym<( )> ($/) {
    my @kids = @( $<semilist>.ast );
    if @kids == 1 && @kids[0].^isa(::Op::WhateverCode) {
        # XXX in cases like * > (2 + *), we *don't* want the parens to disable
        # syntactic specialization, since they're required for grouping
        make @kids[0];
    } elsif !@kids {
        # an empty StatementList returns Nil, but () needs to be defined...
        make ::Op::Paren.new(|node($/), inside =>
            ::Op::SimpleParcel.new(items => []));
    } else {
        make ::Op::StatementList.new(|node($/), children => @kids);
    }
}

method circumfix:sym<[ ]> ($/) {
    my @kids = @( $<semilist>.ast );
    make mkcall($/, '&_array_constructor',
        ::Op::StatementList.new(|node($/), children => @kids));
}

# XXX This fails to catch {; ... } because it runs after empty statement
# elimination.
method check_hash($/) {
    my $do = $<pblock><blockoid>.ast;

    return False unless $do.^isa(::Op::StatementList);
    return True if $do.children == 0;
    return False if $do.children > 1;

    $do = $do.children[0];
    my @bits = $do.^isa(::Op::SimpleParcel) ?? @( $do.items ) !! $do;

    return True if @bits[0].^isa(::Op::SimplePair);

    if @bits[0].^isa(::Op::Builtin) && @bits[0].name eq 'pair' {
        return True;
    }

    if @bits[0].^isa(::Op::Lexical) && substr(@bits[0].name,0,1) eq '%' {
        return True;
    }

    return False;
}

method circumfix:sym<{ }> ($/) {
    my $var = self.gensym;
    $*CURLEX<!sub>.add_my_sub($var, $<pblock>.ast);
    make ::Op::BareBlock.new(|node($/), :$var);

    if self.check_hash($/) {
        make mkcall($/, '&_hash_constructor',
            ::GLOBAL::OptBeta.make_call($var));
    }
}

method circumfix:sigil ($/) {
    self.circumfix:sym<( )>($/); # XXX
    make self.docontext($/, ~$<sigil>, $/.ast);
}

method infix_prefix_meta_operator:sym<!> ($/) {
    make $<infixish>.ast.meta_not;
}
method infix_prefix_meta_operator:sym<R> ($/) {
    make $<infixish>.ast.meta_fun($/, '&reverseop', 2);
}
method infix_prefix_meta_operator:sym<Z> ($/) {
    make $<infixish> ?? $<infixish>[0].ast.meta_fun($/, '&zipop', 2) !!
        Operator.funop($/, '&infix:<Z>', 2);
}
method infix_prefix_meta_operator:sym<X> ($/) {
    make $<infixish> ?? $<infixish>[0].ast.meta_fun($/, '&crossop', 2) !!
        Operator.funop($/, '&infix:<X>', 2);
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

my %loose2tight = (
    '&&' => '&&', '||' => '||', '//' => '//', 'andthen' => 'andthen',
    'orelse' => '//', 'and' => '&&', 'or' => '||',
);

method infix:sym<...> ($/) {
    # STD parses ...^ in the ... rule
    make Operator.funop($/, '&infix:<' ~ $/ ~ '>', 2);
}
method infix:sym<ff>($/) { make Operator::FlipFlop.new() }
method infix:sym<fff>($/) { make Operator::FlipFlop.new(:sedlike) }
method infix:sym<ff^>($/) { make Operator::FlipFlop.new(:excl_rhs) }
method infix:sym<fff^>($/) { make Operator::FlipFlop.new(:excl_rhs, :sedlike) }
method infix:sym<^ff>($/) { make Operator::FlipFlop.new(:excl_lhs) }
method infix:sym<^fff>($/) { make Operator::FlipFlop.new(:excl_lhs, :sedlike) }
method infix:sym<^ff^>($/) { make Operator::FlipFlop.new(:excl_lhs, :excl_rhs) }
method infix:sym<^fff^>($/) { make Operator::FlipFlop.new(:excl_lhs,
    :excl_rhs, :sedlike) }
method infix:sym<~~> ($/) { make ::Operator::SmartMatch.new }
method infix:sym<,>($/) { make ::Operator::Comma.new }
method infix:sym<:=>($/) { make ::Operator::Binding.new(:!readonly) }
method infix:sym<::=>($/) { make ::Operator::Binding.new(:readonly) }
method infix:sym<&&>($/) { make ::Operator::ShortCircuit.new(kind => '&&') }
method infix:sym<and>($/) { make ::Operator::ShortCircuit.new(kind => '&&') }
method infix:sym<||>($/) { make ::Operator::ShortCircuit.new(kind => '||') }
method infix:sym<or>($/) { make ::Operator::ShortCircuit.new(kind => '||') }
method infix:sym<//>($/) { make ::Operator::ShortCircuit.new(kind => '//') }
method infix:sym<orelse>($/) { make ::Operator::ShortCircuit.new(kind => '//') }
method infix:sym<andthen>($/) { make ::Operator::ShortCircuit.new(kind => 'andthen') }
method infix:sym<?? !!>($/) { make ::Operator::Ternary.new(middle => $<EXPR>.ast) }
method infix:sym<.=> ($/) { make ::Operator::DotEq.new }

method prefix:temp ($/) { make ::Operator::Temp.new }
method prefix:let ($/) { make ::Operator::Let.new }

method statement_control:TEMP ($/) {
    $*CURLEX<!sub>.noninlinable;
    make ::Op::Temporize.new(|node($/), mode => 2,
        var => self.inliney_call($/, $<block>.ast));
}

method INFIX($/) {
    my $fn = $<infix>.ast;
    my ($st,$lhs,$rhs) = self.whatever_precheck($fn, $<left>.ast, $<right>.ast);

    make $fn.with_args($/, $lhs, $rhs);

    if $fn.assignish {
        # Assignments to has and state declarators are rewritten into
        # an appropriate phaser
        if $lhs.^isa(Op::StateDecl) {
            my $cv = self.gensym;
            $*CURLEX<!sub>.add_state_name(Str, $cv);
            make mklet($lhs, -> $ll {
                Op::StatementList.new(|node($/), children => [
                    Op::Start.new(condvar => $cv, body =>
                        $fn.with_args($/, $ll, $rhs)),
                    $ll]) });
        }
        elsif $lhs.^isa(::Op::Attribute) {
            my $init = self.thunk_sub($rhs,
                :name($lhs.initializer.name ~ " init"));
            $init.set_outervar(my $ov = self.gensym);
            $*CURLEX<!sub>.add_my_sub($ov, $init);
            $lhs.initializer.add_initializer($lhs.name, $init);
            make ::Op::StatementList.new;
        }
        elsif $lhs.^isa(::Op::ConstantDecl) && !$lhs.init {
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
                ::Op::ShortCircuit.new(|node($/), kind => '&&', args =>
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
    my ($st, $arg) = self.whatever_precheck($<op>.ast, $<arg>.ast);
    if $<op><colonpair> {
        if $arg.^isa(::Op::CallLike) {
            make $arg.adverb($<op><colonpair>.ast<term>);
            make self.whatever_postcheck($/, $st, $/.ast);
        } else {
            $/.CURSOR.sorry("You can't adverb that");
            make ::Op::StatementList.new;
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
    } elsif $al && $al.^isa(::Op::SimpleParcel) {
        return $al.items;
    } else {
        return [$al];
    }
}

method postcircumfix:sym<[ ]> ($/) {
    make Operator.funop($/, '&postcircumfix:<[ ]>', 1, @( $<semilist>.ast ));
}
method postcircumfix:sym<{ }> ($/) {
    make Operator.funop($/, '&postcircumfix:<{ }>', 1, @( $<semilist>.ast ));
}
method postcircumfix:sym«< >» ($/) {
    make Operator.funop($/, '&postcircumfix:<{ }>', 1, $<nibble>.ast);
}
method postcircumfix:sym<( )> ($/) {
    make ::Operator::PostCall.new(args => $<semiarglist>.ast[0]);
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
        make ::Operator::Method.new(name => 'die');
        unless $c {
            $/.CURSOR.sorry("Method call requires a name");
            return;
        }
        if $c<iname> {
            $/.CURSOR.sorry("Indirectly named method calls NYI");
            return;
        }
        make ::Operator::Method.new(name => $c<name>, package => $c<pkg>);
    } elsif $<quote> {
        make ::Operator::Method.new(name => $<quote>.ast);
    } elsif $<variable> {
        make ::Operator::Function.new(function =>
            self.do_variable_reference($/, $<variable>.ast));
    }

    $/.ast.args = $<args>[0].ast[0] if $<args>[0];
    $/.ast.args = $<arglist>[0].ast if $<arglist>[0];
}

method dottyopish ($/) { make $<term>.ast }
method dottyop($/) {
    if $<colonpair> {
        $/.CURSOR.sorry("Colonpair dotties NYI");
        make Operator.funop($/, '&postfix:<++>', 1);
        return Nil;
    }

    make $<methodop>.ast if $<methodop>;
    make $<postop>.ast if $<postop>;
}

method privop($/) {
    if $<methodop>.ast.^isa(::Operator::Function) {
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
    if !$<dottyop>.ast.^isa(::Operator::Method) || $<dottyop>.ast.meta {
        $/.CURSOR.sorry("Modified method calls can only be used with actual methods");
        make Operator.funop($/, '&postfix:<++>', 1);
        return Nil;
    }
    if $<sym> eq '.^' || $<sym> eq '.?' {
        make $<dottyop>.ast.clone(:meta(substr($<sym>,1)));
    } else {
        $/.CURSOR.sorry("NYI dottyop form $<sym>");
        make Operator.funop($/, '&postfix:<++>', 1);
    }
}

method coloncircumfix($/) { make $<circumfix>.ast }

sub qpvalue($ast) {
    if $ast.^isa(::Op::SimpleParcel) {
        join " ", map &qpvalue, @( $ast.items )
    } elsif $ast.^isa(::Op::StringLiteral) {
        $ast.text;
    } elsif $ast.^isa(::Op::Paren) {
        qpvalue($ast.inside);
    } else {
        "XXX"
    }
}

method colonpair($/) {
    my $n;
    if !$<v>.^isa(Match) {
        $n = ":" ~ ($<v> ?? '' !! '!') ~ $<k>;
    } else {
        $n = ":" ~ $<k> ~ "<" ~ qpvalue($<v>.ast) ~ ">";
    }
    my $tv = $<v>.^isa(Match) ?? $<v>.ast !!
        ::Op::Lexical.new(name => $<v> ?? 'True' !! 'False');

    if $tv ~~ Str {
        if substr($<v>,1,1) eq '<' {
            $tv = mkcall($/, '&postcircumfix:<{ }>',
                ::Op::Lexical.new(name => '$/'),
                ::Op::StringLiteral.new(text => ~$<k>));
        } else {
            $tv = self.do_variable_reference($/,
                { sigil => ~$<v><sigil>,
                    twigil => ($<v><twigil> ?? ~$<v><twigil>[0] !! ''),
                    name => $<k> });
        }
    }

    make { ext => $n, term => ::Op::SimplePair.new(key => $<k>, value => $tv) };
}

method fatarrow($/) {
    make ::Op::SimplePair.new(key => ~$<key>, value => $<val>.ast);
}

my %_nowhatever = (map { ($_ => True) }, ('&infix:<,>', '&infix:<..>',
    '&infix:<...>', '&infix:<=>', '&infix:<xx>'));
method whatever_precheck($op, *@args) {
    return ([], @args) if ($op.^isa(Operator) ?? !$op.whatever_curry !! %_nowhatever{$op});
    my @vars;
    my @args_ = @args;
    for @args_ -> $a is rw {
        die "invalid undef here" if !$a;
        if $a.^isa(::Op::Whatever) {
            push @vars, $a.slot;
            $a = ::Op::Lexical.new(name => $a.slot);
        } elsif $a.^isa(::Op::WhateverCode) {
            push @vars, @( $a.vars );
            $a = ::Op::CallSub.new(
                invocant => ::Op::Lexical.new(name => $a.slot),
                args => [ map { ::Op::Lexical.new(name => $_) }, @($a.vars) ]);
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
        $body.set_signature(::GLOBAL::Sig.new(params => [
            map { ::Sig::Parameter.new(slot => $_, name => $_) }, @$st ]));
        $body.set_transparent;
        $body.finish($term);

        $*CURLEX<!sub>.add_my_sub($slot, $body);

        ::Op::WhateverCode.new(ops => Any, vars => $st, :$slot, |node($/));
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
    ::Op::Lexical.new(|node($/), name => $slot);
}

method term:name ($/) {
    my ($name) = self.process_name($<longname>, :defer);

    if $<args> {
        $name<name>  = '&' ~ $name<name> if $name<name>;
        $name<iname> = mkstringycat($/, '&', $name<iname>) if $name<iname>;
    }

    if $name<iname> {
        make ::Op::IndirectVar.new(|node($/), name => $name<iname>);
    }
    elsif $name<pkg> {
        make self.package_var($/, self.gensym, $name<name>, $name<pkg>);
    } else {
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

        make ::Op::CallSub.new(|node($/), invocant => $/.ast,
            args => $sal[0] // []);
    }

    if @pc {
        make @pc[0].ast.with_args($/, $/.ast);
    }
}

method term:identifier ($/) {
    my $id  = $<identifier>.ast;
    my $sal = $<args> ?? ($<args>.ast // []) !! [];
    # TODO: support zero-D slicels

    if $sal > 1 {
        $/.CURSOR.sorry("Slicel lists are NYI");
        make ::Op::StatementList.new;
        return;
    }

    if $id eq any < MY OUR CORE DYNAMIC GLOBAL CALLER OUTER UNIT SETTING PROCESS COMPILING PARENT CLR > {
        make Op::IndirectVar.new(|node($/),
            name => Op::StringLiteral.new(text => $id));
        return;
    }
    my $is_name = $/.CURSOR.is_name(~$<identifier>);

    if $is_name && $<args>.chars == 0 {
        make mklex($/, $id);
        return;
    }

    my $args = $sal[0] // [];

    make ::Op::CallSub.new(|node($/),
        invocant => mklex($/, $is_name ?? $id !! '&' ~ $id),
        args => $args);
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
    ::Op::Lexical.new(name => '$_')) }
method term:capterm ($/) { make $<capterm>.ast }
method term:sigterm ($/) { make $<sigterm>.ast }
method term:statement_prefix ($/) { make $<statement_prefix>.ast }
method term:variable ($/) {
    make self.do_variable_reference($/, $<variable>.ast);
}
method term:sym<...> ($/) { make ::Op::Yada.new(|node($/), kind => '...') }
method term:sym<???> ($/) { make ::Op::Yada.new(|node($/), kind => '???') }
method term:sym<!!!> ($/) { make ::Op::Yada.new(|node($/), kind => '!!!') }
method term:sym<*> ($/) {
    make ::Op::Whatever.new(|node($/), slot => self.gensym)
}
method term:lambda ($/) {
    make self.block_expr($/, $<pblock>.ast);
}

method term:colonpair ($/) {
    if $<colonpair> > 1 {
        $/.CURSOR.sorry("Multi colonpair syntax not yet understood"); #XXX
        make ::Op::StatementList.new;
        return Nil;
    }
    make $<colonpair>[0].ast<term>;
}

method term:fatarrow ($/) { make $<fatarrow>.ast }

method term:reduce ($/) {
    my $assoc = $<op><O><assoc>;
    make Op::CallSub.new(|node($/),
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
        return ::Op::StatementList.new;
    }

    if $tw eq '!' {
        my $pclass;
        if $v<pkg> {
            $pclass = $v<pkg>.xref;
        } elsif $*CURLEX<!sub>.in_class -> $c {
            $pclass = $c;
        } else {
            $M.CURSOR.sorry("Cannot resolve class for private method");
        }
        self.docontext($M, $v<sigil>, ::Op::CallMethod.new(|node($M),
            name => $v<name>, private => True, receiver => mklex($M, 'self'),
            :$pclass));
    }
    elsif $tw eq '.' {
        if defined $v<pkg> {
            $M.CURSOR.sorry('$.Foo::bar syntax NYI');
            return ::Op::StatementList.new;
        }

        self.docontext($M, $v<sigil>, ::Op::CallMethod.new(|node($M),
            name => $v<name>, receiver => mklex($M, 'self')));
    }
    # no twigil in lex name for these
    elsif $tw eq '^' || $tw eq ':' {
        mklex($M, $v<sigil> ~ $v<name>, :$hash, :$list);
    }
    elsif $tw eq '*' {
        ::Op::ContextVar.new(|node($M), name => $sl);
    }
    elsif $tw eq '' || $tw eq '?' {
        if defined($v<pkg>) {
            self.package_var($M, self.gensym, $sl, $v<pkg>);
        } elsif $tw eq '?' && $sl eq '$?POSITION' {
            mkcall($M, '&infix:<..^>',
                ::Op::Num.new(|node($M), value => [10, ~$M.from]),
                ::Op::Num.new(|node($M), value => [10, ~$M.to]));
        } elsif $tw eq '?' && $sl eq '$?LINE' {
            ::Op::Num.new(|node($M), value => [10, ~$M.cursor.lineof($M.from)]);
        } elsif $tw eq '?' && $sl eq '$?FILE' {
            ::Op::StringLiteral.new(|node($M), text => $*FILE<name>);
        } elsif $tw eq '?' && $sl eq '$?ORIG' {
            ::Op::StringLiteral.new(|node($M), text => $M.orig);
        } elsif $tw eq '?' && $sl eq '&?BLOCK' {
            $*CURLEX<!sub>.noninlinable;
            ::Op::GetBlock.new(|node($M))
        } elsif $tw eq '?' && $sl eq '&?ROUTINE' {
            $*CURLEX<!sub>.noninlinable;
            ::Op::GetBlock.new(|node($M), :routine)
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

    ::Op::Builtin.new(|node($M), name => $method, args => [$term]);
}

method variable($/) {
    my $sigil =  $<sigil>  ?? ~$<sigil> !! substr(~$/, 0, 1);
    my $twigil = $<twigil> ?? $<twigil>[0]<sym> !! '';

    my ($name, $pkg);
    my ($dsosl) = $<desigilname> ?? $<desigilname>.ast !!
        $<sublongname> ?? $<sublongname>.ast !!
        $<longname> ?? self.process_name($<longname>, :defer) !!
        Any;
    if defined($dsosl<ind>) {
        make { term => self.docontext($/, $sigil, $dsosl<ind>) };
        return;
    } elsif defined($dsosl<iname>) {
        make { term => ::Op::IndirectVar.new(|node($/),
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
            mkcall($/, '&postcircumfix:<[ ]>',
                ::Op::Lexical.new(name => '$/'),
                ::Op::Num.new(value => $<index>.ast))
        };
        return Nil;
    } elsif $<postcircumfix> {
        if $<postcircumfix>[0].reduced eq 'postcircumfix:sym<< >>' { #XXX fiddly
            make { capid => $<postcircumfix>[0].ast.args[0].text, term =>
                mkcall($/, '&postcircumfix:<{ }>',
                    ::Op::Lexical.new(name => '$/'),
                    @( $<postcircumfix>[0].ast.args))
            };
            return;
        } else {
            make { term => self.docontext($/, $sigil, $<postcircumfix>[0].ast.args[0]) };
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
    %rt<positional> = False;
    make %rt;
}

# :: { list : Bool, hash : Bool, slot : Maybe[Str] }
method param_var($/) {
    if $<signature> {
        $/.CURSOR.sorry('Sub-signatures NYI');
        make { };
        return Nil;
    }
    my $twigil = $<twigil> ?? ~$<twigil>[0] !! '';
    my $sigil =  ~$<sigil>;
    my $list = $sigil eq '@';
    my $hash = $sigil eq '%';
    my $name =   $<name> ?? ~$<name>[0] !! Any;

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

    make { :$list, :$hash, :$slot,
        names => defined($name) ?? [ $name ] !! [] }
}

# :: Sig::Parameter
method parameter($/) {
    my $rw = ?( $*SIGNUM && $*CURLEX<!rw_lambda> );
    my $copy = False;
    my $sorry;
    my $slurpy = False;
    my $slurpycap = False;
    my $optional = False;
    my $rwt = False;
    my $type;

    if $<type_constraint> {
        ($type) = self.process_name($<type_constraint>[0]<typename><longname>);
    }

    for @( $<trait> ) -> $trait {
        if $trait.ast<rw> { $rw = True }
        elsif $trait.ast<copy> { $copy = True }
        elsif $trait.ast<parcel> { $rwt = True }
        elsif $trait.ast<readonly> { $rw = False }
        else {
            $trait.CURSOR.sorry('Unhandled trait ' ~ $trait.ast.keys.[0]);
        }
    }

    if $<post_constraint> > 0 {
        $/.sorry('Parameter post constraints NYI');
        make ::Sig::Parameter.new;
        return Nil;
    }

    my $default = $<default_value> ?? $<default_value>[0].ast !! Any;
    $default.set_name("$/ init") if $default;

    my $tag = $<quant> ~ ':' ~ $<kind>;
    if    $tag eq '**:*' { $sorry = "Slice parameters NYI" }
    elsif $tag eq '*:*'  { $slurpy = True }
    elsif $tag eq '|:*'  { $slurpycap = True }
    elsif $tag eq '\\:!' { $rwt = True }
    elsif $tag eq '\\:?' { $rwt = True; $optional = True }
    elsif $tag eq ':!'   { }
    elsif $tag eq ':*'   { $optional = True }
    elsif $tag eq ':?'   { $optional = True }
    elsif $tag eq '?:?'  { $optional = True }
    elsif $tag eq '!:!'  { }
    elsif $tag eq '!:?'  { $optional = True }
    elsif $tag eq '!:*'  { }
    else                 { $sorry = "Confusing parameters ($tag)" }
    if $sorry { $/.CURSOR.sorry($sorry); }
    my $p = $<param_var> // $<named_param>;

    if defined $p.ast<slot> {
        # TODO: type constraint here
    }

    make ::Sig::Parameter.new(name => ~$/, mdefault => $default,
        :$optional, :$slurpy, :$rw, tclass => $type,
        :$slurpycap, rwtrans => $rwt, is_copy => $copy, |$p.ast);
}

# signatures exist in several syntactic contexts so just make an object for now
method signature($/) {
    if $<type_constraint> {
        # ignore for now
    }

    if $<param_var> {
        my $sig = Sig.new(params => [ ::Sig::Parameter.new(
                name => ~$<param_var>, |$<param_var>.ast,
                full_parcel => True) ]);
        $*CURLEX<!sub>.set_signature($sig) if $*SIGNUM;
        make $sig;
        return;
    }

    my @p = map *.ast, @( $<parameter> );
    my @ps = @( $<param_sep> );
    my $ign = False;
    loop (my $i = 0; $i < @p; $i++) {
        @p[$i].multi_ignored = $ign;
        if $i >= @ps {
        } elsif defined @ps[$i].index(':') {
            $/.CURSOR.sorry('Only the first parameter may be invocant') if $i;
            $*CURLEX<!sub>.add_my_name('self', :noinit, |mnode($/));
            @p[$i].invocant = True;
        } elsif defined @ps[$i].index(';;') {
            $ign = True;
        } elsif !defined @ps[$i].index(',') {
            $/.CURSOR.sorry("Parameter separator @ps[$i] NYI");
        }
    }

    state %mlike = (:Method, :Submethod, :Regex);
    if $*SIGNUM && %mlike{$*CURLEX<!sub>.class} && (!@p || !@p[0].invocant) {
        $*CURLEX<!sub>.add_my_name('self', :noinit, |mnode($/));
        unshift @p, ::Sig::Parameter.new(name => 'self', :invocant);
    }

    for @p {
        if !defined(.tclass) && $*SIGNUM {
            if .invocant && $*CURLEX<!sub>.methodof {
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

    my $sig = Sig.new(params => @p);
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
    if !$<quote>.ast.^isa(::Op::StringLiteral) {
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
        my $stub = ::Op::HereStub.new(node => Any);
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
        } elsif $<infixish>.ast ~~ ::Operator::CompoundAssign {
            $repl = $<infixish>.ast.base.with_args($/,
                mkcall($/, '&prefix:<~>', ::Op::Lexical.new(name => '$/')),
                $<right>.ast);
        } else {
            $/.CURSOR.sorry("Unhandled operator in substitution");
            $repl = mklex($/, 'Any');
        }
    } else {
        $repl = $<right>.ast;
    }
    $repl = self.block_expr($/, self.thunk_sub($repl));
    make ::Op::CallMethod.new(|node($/), receiver => mklex($/, '$_'),
        name => 'subst',
        args => [ $regex, $repl, self.extract_rx_adverbs(True, True, $/),
            ::Op::SimplePair.new(key => 'inplace', value => mklex($/,'True'))]);
}
method tribble($/) {}
method babble($/) {}
method quotepair($/) {}

method quotepair_term($/) {
    my $v;
    if $<v> ~~ Match {
        $v = $<v>.ast
    } elsif $<v> ~~ Str {
        $v = ::Op::Num.new(value => [10, $<v>]);
    } else {
        $v = mklex($/, $<v> ?? "True" !! "False");
    }
    ::Op::SimplePair.new(|node($/), key => $<k>, value => $v);
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
        my $x = $<capture>[0]<EXPR>.ast;
        if $x.^isa(::Op::SimpleParcel) {
            @args = @($x.items);
        } else {
            @args = $x;
        }
    } elsif $<termish> {
        @args = ::Op::Paren.new(|node($/), inside => $<termish>.ast);
    }
    make ::Op::CallSub.new(|node($/), invocant => mklex($/, '&_make_capture'),
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

        loop (my $l = $*CURLEX<!sub>; $l; $l.=outer) {
            # this isn't *quite* right, as it will cause declaring
            # anything more in the same scope to fail.
            $/.CURSOR.mark_used($_) for $l.lex_names;
        }

        make ::Op::YouAreHere.new(|node($/), unitname => $*UNITNAME);
    } else {
        make $<statementlist>.ast;
    }
}
method lambda($/) {}
method embeddedblock($/) {
    $*CURLEX<!sub>.finish($<statementlist>.ast);
    $*CURLEX<!sub>.set_signature(Sig.simple());
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
            my $list = $param.list;
            my $hash = $param.hash;
            my $type = $param.tclass;

            if $*SCOPE eq 'state' {
                $sub.add_state_name($slot, self.gensym, :$list, :$hash,
                    typeconstraint => $type, |mnode($/));
                $param = Op::Lexical.new(name => $slot, |node($/));
            } elsif $*SCOPE eq 'our' {
                $param = self.package_var($/, $slot, $slot, ['OUR']);
            } else {
                $sub.add_my_name($slot, :$list, :$hash,
                    typeconstraint => $type, |mnode($/));
                $param = Op::Lexical.new(name => $slot, |node($/));
            }
        }
        make Op::SimpleParcel.new(|node($/), items => @p);
        make Op::StateDecl.new(|node($/), inside => $/.ast)
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

method add_attribute($/, $name, $sigil, $accessor, $type) {
    my $ns = $*CURLEX<!sub>.body_of;
    $/.CURSOR.sorry("Attribute $name declared outside of any class"),
        return ::Op::StatementList.new unless $ns;
    $/.CURSOR.sorry("Attribute $name declared in an augment"),
        return ::Op::StatementList.new if defined $*AUGMENT_BUFFER;

    if !$ns.CAN('add_attribute') {
        $/.CURSOR.sorry("A $ns.WHAT() cannot have attributes");
        return ::Op::StatementList.new
    }

    my $nb = $*unit.create_sub(
        outer      => $*CURLEX<!sub>,
        name       => $name,
        cur_pkg    => $*CURLEX<!sub>.cur_pkg,
        class      => 'Method');
    $nb.set_transparent;
    $nb.add_my_name('self', noinit => True);
    $nb.set_signature(Sig.simple('self'));
    $nb.finish(::Op::GetSlot.new(name => $name,
        object => ::Op::Lexical.new(name => 'self')));
    $*CURLEX<!sub>.create_static_pad; # for protosub instance
    my $at;

    $/.CURSOR.trymop({
        my $ac = self.gensym;
        $nb.set_outervar($ac);
        $*CURLEX<!sub>.add_my_sub($ac, $nb, |mnode($/));
        $ns.add_attribute($name, $sigil, +$accessor, $type, |mnode($/));
        $ns.add_method(::Metamodel::SubVisibility::private, $name, $nb,
            |mnode($/));
        if $accessor {
            $ns.add_method(0, $name, $nb, |mnode($/));
        }
        $at = True;
    });

    $at ?? ::Op::Attribute.new(name => $name, initializer => $ns) !!
        ::Op::StatementList.new;
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
        ($typeconstraint) = self.process_name($*OFTYPE<longname>);
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

    if $scope ne 'has' && ($t eq '.' || $t eq '!') {
        $/.CURSOR.sorry("Twigil $t is only valid on attribute definitions ('has').");
    }

    if !defined($v<name>) && $scope ne any < my anon state > {
        $/.CURSOR.sorry("Scope $scope requires a name");
    }

    if defined($v<pkg>) || defined($v<iname>) {
        $/.CURSOR.sorry(":: syntax is only valid when referencing variables, not when defining them.");
    }

    my $name = defined($v<name>) ?? $v<sigil> ~ $v<twigil> ~ $v<name> !! "";
    # otherwise identical to my
    my $slot = ($scope eq 'anon' || !defined($v<name>))
        ?? self.gensym !! $name;

    if $scope eq 'has' {
        make self.add_attribute($/, $v<name>, $v<sigil>, $t eq '.',
            $typeconstraint);
    } elsif $scope eq 'state' {
        $/.CURSOR.trymop({
            $/.CURSOR.check_categorical($slot);
            $*CURLEX<!sub>.add_state_name($slot, self.gensym, :$list,
                :$hash, :$typeconstraint, |mnode($/));
        });
        make Op::StateDecl.new(|node($/), inside =>
            Op::Lexical.new(|node($/), name => $slot, :$list, :$hash));
    } elsif $scope eq 'our' {
        make self.package_var($/, $slot, $slot, ['OUR']);
    } else {
        $/.CURSOR.trymop({
            $/.CURSOR.check_categorical($slot);
            $*CURLEX<!sub>.add_my_name($slot, :$list, :$hash,
                :$typeconstraint, |mnode($/));
        });
        make ::Op::Lexical.new(|node($/), name => $slot, :$list, :$hash);
    }

    if $start {
        my $cv = self.gensym;
        $*CURLEX<!sub>.add_state_name(Str, $cv);
        make mklet($/.ast, -> $ll {
            Op::StatementList.new(|node($/), children => [
                Op::Start.new(condvar => $cv, body =>
                    self.inliney_call($/, $start, $ll)), $ll ]) });
    }
}

method trivial_eval($/, $ast) {
    if $ast.^isa(::Op::SimpleParcel) {
        [,] map { self.trivial_eval($/, $_) }, @( $ast.items )
    } elsif $ast.^isa(::Op::SimplePair) {
        $ast.key => self.trivial_eval($/, $ast.value)
    } elsif $ast.^isa(::Op::StringLiteral) {
        $ast.text;
    } elsif $ast.^isa(::Op::Paren) {
        self.trivial_eval($/, $ast.inside);
    } elsif $ast.^isa(::Op::StatementList) {
        my @l = @( $ast.children ); pop @l;
        self.trivial_eval($/, $_) for @l;
        $ast.children ?? self.trivial_eval($/, $ast.children[*-1]) !! Nil;
    } elsif $ast.^isa(::Op::Num) && $ast.value !~~ Array {
        $ast.value.Num
    } elsif $ast.^isa(::Op::Num) && $ast.value ~~ Array && $ast.value[0] == 10 {
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
            name => $<longname>, class => ::Metamodel::Subset,
            :@exports);

        $*CURLEX<!sub>.create_static_pad;

        $obj.basetype = $basetype.xref;
        $obj.where = $body.xref;
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

    ::Op::ConstantDecl.new(|node($/), name => $slot, init => False);
}

method make_constant_into($/, $pkg, $name, $rhs) {
    my $slot = self.gensym;
    $/.CURSOR.trymop({
        $*CURLEX<!sub>.add_common_name($slot, $pkg, $name, |mnode($/));
    });
    self.init_constant(::Op::ConstantDecl.new(|node($/), name => $slot,
        init => False), $rhs);
}

method init_constant($con, $rhs) {
    my $body = self.thunk_sub(
        ::Op::LexicalBind.new(name => $con.name, :$rhs),
        name => "$con.name() init");
    $body.outer.create_static_pad;
    $con.init = True;
    $body.set_phaser(+::Metamodel::Phaser::UNIT_INIT);
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
            $nb.set_signature(Sig.simple('self'));
            $nb.finish(self.init_constant(
                self.make_constant($/, 'anon', Any),
                ::Op::CallMethod.new(name => 'new',
                    receiver => mklex($/, 'EnumMap'), args => [$<term>.ast])));
            $*CURLEX<!sub>.create_static_pad;
            $*CURLEX<!sub>.add_my_sub($nbvar, $nb, |mnode($/));
            $obj.add_method(0, 'enums', $nb, |mnode($/));
            $obj.close;

            for @pairs {
                self.make_constant_into($/, $obj, .key, rhs =>
                    ::Op::CallSub.new(invocant => mklex($/, $lexvar),
                        args => [ ::Op::StringLiteral.new(text => .key) ]));
            }

            for @pairs {
                self.init_constant(self.make_constant($/, $scope, .key),
                    ::Op::CallSub.new(invocant => mklex($/, $lexvar),
                        args => [ ::Op::StringLiteral.new(text => .key) ]));
            }
        });

        make mklex($/, $lexvar);
    } else {
        make self.init_constant(
            self.make_constant($/, $<name> ?? $scope !! 'anon', ~$<name>),
            ::Op::CallMethod.new(|node($/), name => 'new',
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
    make ::Op::StatementList.new;
}

method package_declarator:require ($/) {
    if $<EXPR> {
        $/.CURSOR.sorry('Expressional forms of require NYI');
        make ::Op::StatementList.new;
        return Nil;
    }
    make ::Op::Require.new(|node($/), unit => ~$<module_name>);
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
        } elsif $pack && $tr<export> {
            my @exports = @( $tr<export> );
            $sub.outer.add_exports($pack.name, $pack, @exports);
        } elsif !$pack && $tr<export> {
            my @exports = @( $tr<export> );
            $sub.outer.add_exports('&'~$sub.name, $sub, @exports);
            $sub.outer.create_static_pad;
            $/.CURSOR.mark_used($sub.outervar)
                if defined $sub.outervar;
        } elsif !$pack && $tr<nobinder> {
            $sub.set_signature(Any);
        } elsif !$pack && grep { defined $tr{$_} }, <looser tighter equiv> {
            my $rel = $tr.keys.[0];
            my $to  = $tr.values.[0];
            $to = $to.inside if $to ~~ ::Op::Paren;
            $to = $to.children[0] if $to ~~ ::Op::StatementList && $to.children == 1;

            my $oprec;
            if $to ~~ ::Op::Lexical {
                $oprec = $T.CURSOR.function_O($to.name);
            } elsif $to ~~ ::Op::StringLiteral && $sub.name ~~ /^(\w+)\:\<.*\>$/ {
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
    $n.set_signature(Sig.simple(@$params));
    $n.finish($code);
    $n;
}

method arglist($/) {
    $/.CURSOR.sorry("Invocant handling is NYI") if $*INVOCANT_IS;
    my $x = $<EXPR> && $<EXPR>.ast;

    if !defined $x {
        make [];
    } elsif $x && $x.^isa(::Op::SimpleParcel) {
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
        $<arglist> ?? [ $<arglist>[0].ast ] !! Any;
}

method statement($/) {
    if $<label> {
        make ::Op::Labelled.new(|node($/), name => $<label>.ast,
            stmt => $<statement>.ast);
        return;
    }

    make ($<statement_control> ?? $<statement_control>.ast !!
        $<EXPR> ?? $<EXPR>.ast !! ::Op::StatementList.new);

    if $<statement_mod_cond> {
        my ($sym, $exp) = @( $<statement_mod_cond>[0].ast );

        if $sym eq 'if' {
            make ::Op::Conditional.new(|node($/), check => $exp,
                true => $/.ast, false => Any);
        } elsif $sym eq 'unless' {
            make ::Op::Conditional.new(|node($/), check => $exp,
                false => $/.ast, true => Any);
        } elsif $sym eq 'when' {
            make ::Op::Conditional.new(|node($/),
                check => ::Op::CallMethod.new(name => 'ACCEPTS',
                    receiver => $exp, positionals => [ mklex($/, '$_') ]),
                true => $/.ast, false => Any);
        } else {
            $/.CURSOR.sorry("Unhandled statement modifier $sym");
            make ::Op::StatementList.new;
            return Nil;
        }
    }

    if $<statement_mod_loop> {
        my ($sym, $exp) = @( $<statement_mod_loop>[0].ast );

        if $sym eq 'while' {
            make ::Op::WhileLoop.new(|node($/), check => $exp,
                body => $/.ast, until => False, once => False);
        } elsif $sym eq 'until' {
            make ::Op::WhileLoop.new(|node($/), check => $exp,
                body => $/.ast, until => True, once => False);
        } elsif $sym eq 'given' {
            make mktemptopic($/, $exp, $/.ast);
        } elsif $sym eq 'for' {
            # XXX laziness, comprehensions
            my $var = self.gensym;
            make ::Op::ImmedForLoop.new(|node($/), :$var, source => $exp,
                sink => mktemptopic($/, ::Op::LetVar.new(name => $var), $/.ast));
        } else {
            $/.CURSOR.sorry("Unhandled statement modifier $sym");
            make ::Op::StatementList.new;
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
    make ::Op::StatementList.new(|node($/), children =>
        [ map *.statement_level, map *.ast, @( $<statement> ) ]);
}

method semilist($/) { make [ map *.ast, @( $<statement> ) ] }

method module_name:normal ($/) {
    # name-extension stuff is just ignored on module names for now
    make {
        name => ~$<longname><name>,
        args => $<arglist> ?? $<arglist>[0].ast !! Any };
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
        ::Op::Conditional.new(|node($/), check => $cond,
            true  => self.if_block($/, $cond, $branch<pblock>),
            false => @branches ?? self.if_branches($/, @branches) !!
                $<else> ?? self.if_block($/, $cond, $<else>[0]) !!
                Any);
    });
}

method statement_control:if ($/) {
    make self.if_branches($/, $<xblock>, @( $<elsif> ));
}

method statement_control:unless ($/) {
    make mklet($<xblock>.ast[0], -> $cond {
        ::Op::Conditional.new(|node($/), check => $cond,
            false => self.if_block($/, $cond, $<xblock><pblock>)) });
}

# Hack - Op::WhileLoop binds the condition to "!cond"
method statement_control:while ($/) {
    make ::Op::WhileLoop.new(|node($/), check => $<xblock>.ast[0],
        body => self.if_block($/, ::Op::LetVar.new(name => '!cond'),
            $<xblock><pblock>), :!until, :!once,
            :need_cond(defined $<xblock><pblock><lambda>));
}

method statement_control:until ($/) {
    make ::Op::WhileLoop.new(|node($/), check => $<xblock>.ast[0],
        body => self.if_block($/, ::Op::LetVar.new(name => '!cond'),
            $<xblock><pblock>), :until, :!once,
            :need_cond(defined $<xblock><pblock><lambda>));
}

method statement_control:repeat ($/) {
    my $until = $<wu> eq 'until';
    my $check = $<xblock> ?? $<xblock>.ast[0] !! $<EXPR>.ast;
    my $pb = $<xblock> ?? $<xblock><pblock> !! $<pblock>;
    my $body  = self.if_block($/, ::Op::LetVar.new(name => '!cond'), $pb);
    make ::Op::WhileLoop.new(|node($/), :$check, :$until, :$body, :once,
            :need_cond(defined $pb<lambda>));
}

method statement_control:loop ($/) {
    my $body = self.inliney_call($/, $<block>.ast);
    # XXX wrong interpretation
    my $init = $0 && $0[0]<e1>[0] ?? $0[0]<e1>[0].ast !! Any;
    my $cond = $0 && $0[0]<e2>[0] ?? $0[0]<e2>[0].ast !! Any;
    my $step = $0 && $0[0]<e3>[0] ?? $0[0]<e3>[0].ast !! Any;

    make ::Op::GeneralLoop.new(|node($/), :$body, :$init, :$cond, :$step);
}

method statement_control:for ($/) {
    make ::Op::ForLoop.new(|node($/), source => $<xblock>.ast[0],
        sink => self.block_expr($/, $<xblock>.ast[1]).name);
}

method statement_control:given ($/) {
    make self.inliney_call($/, $<xblock>.ast[1], $<xblock>.ast[0]);
}

method statement_control:default ($/) {
    make ::Op::When.new(|node($/), match => mklex($/, 'True'),
        body => self.inliney_call($/, $<block>.ast));
}

method statement_control:when ($/) {
    make ::Op::When.new(|node($/), match => $<xblock>.ast[0],
        body => self.inliney_call($/, $<xblock>.ast[1]));
}

method statement_control:use ($/) {
    make ::Op::StatementList.new;
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
    for $*unit.list_stash($exp.who) -> $uname, $obj {
        if !$obj || $obj.kind eq 'sub' {
            $*CURLEX<!sub>.add_common_name($uname, $exp, $uname);
        } else {
            $*CURLEX<!sub>.add_my_stash($uname, $obj);
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
        if $class eq 'role' && $<signature> {
            $sub.set_signature($<signature>.ast);
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
            push @ops, CgOp._addmethod(CgOp.letvar('!mo'), $mode,
                CgOp.str($name), CgOp.fetch(CgOp.scopedlex($sym)));
        }
        my $fin = CgOp.letn('!mo', CgOp.class_ref('mo', $obj),
            @ops, CgOp._invalidate(CgOp.letvar('!mo')), CgOp.corelex('Nil'));

        $ph.finish(::Op::CgOp.new(op => $fin));
        $sub.create_static_pad;
        $ph.set_phaser(+::Metamodel::Phaser::INIT);

        make ::Op::CallSub.new(|node($/), invocant => mklex($/, $bodyvar));
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
                $ast = ::Op::StatementList.new(|node($/), children => [
                    $ast, mkcall($/, '&callframe') ]);
                $sub.create_static_pad;
                $obj.set_instantiation_block($sub);

                make mklex($/, $*CURLEX<!sub>.outervar);
            } else {
                make ::Op::StatementList.new(|node($/), children => [
                    ::Op::CallSub.new(invocant => mklex($/, $bodyvar)),
                    ::Op::Lexical.new(name => $*CURLEX<!sub>.outervar) ]);
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
        if !$<circumfix> || !$<circumfix>[0].ast.^isa(::Op::StringLiteral) {
            $/.CURSOR.sorry("Argument to endsym must be a literal string");
        } else {
            $text = $<circumfix>[0].ast.text;
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

method trait_mod:of ($/) {
    make { of => self.process_name($<typename><longname>) }
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
    ::GLOBAL::OptBeta.make_call($sym, @parms);
}

# this is intended to be called after parsing the longname for a sub,
# but before the signature.  export, etc are handled by the sub/package
# trait handler
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

    if $name ~~ Op && (!defined($method_type) || $scope ne 'has' ||
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
            $proto ~~ s/\:.*//;
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
                when 'multi' { $symbol ~= ":({ self.gensym })"; }
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

method routine_def ($/) {
    $*CURLEX<!sub>.finish($<blockoid>.ast);
    make ::Op::Lexical.new(|node($/), name => $*CURLEX<!sub>.outervar);
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

method method_def ($/) {
    $*CURLEX<!sub>.finish($<blockoid>.ast);
    make ::Op::Lexical.new(|node($/), name => $*CURLEX<!sub>.outervar);
}

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
    make Op::DoOnceLoop.new(|node($/),
        body => self.inliney_call($/, $<blast>.ast));
}
method statement_prefix:gather ($/) {
    make ::Op::Gather.new(|node($/),
        var => self.block_expr($/, $<blast>.ast).name);
}
method statement_prefix:try ($/) {
    make ::Op::Try.new(|node($/), body => self.inliney_call($/, $<blast>.ast));
}

method statement_prefix:START ($/) {
    my $cv = self.gensym;
    $*CURLEX<!sub>.add_state_name(Str, $cv);
    make ::Op::Start.new(|node($/), condvar => $cv, body =>
        self.inliney_call($/, $<blast>.ast));
}

sub phaser($/, $ph, :$unique, :$topic, :$csp) {
    my $sub = ($<blast> // $<block>).ast;

    if $unique {
        $/.CURSOR.sorry("Limit one $ph phaser per block, please.")
            if $sub.outer.contains_phaser(+::Metamodel::Phaser.($ph));
        my $code = ($<blast><statement> // $<blast><block><blockoid> // $<block><blockoid>).ast;
        # TODO avoid double finishing
        $sub.finish(::Op::CatchyWrapper.new(inner => $code));
    }

    $sub.outer.noninlinable;

    if $topic {
        $sub.has_lexical('$_') || $sub.add_my_name('$_');
        $sub.parameterize_topic;
        $sub.set_signature(Sig.simple('$_'));
    }
    $*CURLEX<!sub>.create_static_pad if $csp;
    $sub.set_phaser(+::Metamodel::Phaser.($ph));
    make ::Op::StatementList.new;
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

# XXX 'As soon as possible' isn't quite soon enough here
method statement_prefix:BEGIN ($/) {
    $*CURLEX<!sub>.create_static_pad;
    $<blast>.ast.set_phaser(+::Metamodel::Phaser::UNIT_INIT);
    make ::Op::StatementList.new;

    # MAJOR HACK - allows test code like BEGIN { @*INC.push: ... } to work
    repeat while False {
        my $c = ($<blast><statement> || $<blast><block><blockoid>).ast;

        last unless $c ~~ Op::StatementList;
        last unless $c.children == 1;
        my $d = $c.children.[0];
        last unless $d ~~ Op::CallMethod;
        last unless $d.receiver ~~ Op::ContextVar;
        last unless $d.receiver.name eq '@*INC';
        last if $d.private || $d.ismeta;
        last unless $d.name eq any <push unshift>;
        last unless +$d.getargs == 1;
        last unless defined my $str = self.trivial_eval($/, $d.getargs.[0]);
        @*INC."$d.name()"($str);
    }
}

method comp_unit($/) {
    $*CURLEX{'!sub'}.finish($<statementlist>.ast);

    make $*unit;
}
