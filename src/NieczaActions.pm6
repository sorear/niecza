class NieczaActions;

use Op;
use RxOp;
use Sig;
use CClass;
use OpHelpers;
use Operator;

sub mnode($M) {
    $M.^isa(Match) ??
        { file => $*FILE<name>, line => $M.CURSOR.lineof($M.from), pos => $M.from } !!
        { file => $*FILE<name>, line => $M.lineof($M.pos), pos => $M.pos }
}

# XXX Niecza  Needs improvement
method FALLBACK($meth, $/) {
    if $meth eq '::($name)' { # XXX STD miscompilation
        my $p = $<O><prec>;
        if $p eq 't=' { # additive
            make Operator.funop('&infix:<' ~ $<sym> ~ '>', 2);
        } elsif $p eq 'y=' && $<semilist> {
            make Operator.funop('&postcircumfix:<' ~ $<sym> ~ '>', 1, @( $<semilist>.ast ));
        } elsif $p eq 'y=' {
            make Operator.funop('&postfix:<' ~ $<sym> ~ '>', 1);
        } elsif $p eq 'v=' || $p eq 'o=' {
            make Operator.funop('&prefix:<' ~ $<sym> ~ '>', 1);
        } elsif $p eq 'z=' && !$<semilist> {
            make mkcall($/, '&term:<' ~ $<sym> ~ '>');
        } elsif $p eq 'z=' {
            make mkcall($/, '&circumfix:<' ~ $<sym> ~ '>', @( $<semilist>.ast ));
        }
    } elsif substr($meth,0,7) eq 'prefix:' {
        make Operator.funop('&prefix:<' ~ $<sym> ~ '>', 1);
    } elsif substr($meth,0,8) eq 'postfix:' {
        make Operator.funop('&postfix:<' ~ $<sym> ~ '>', 1);
    } elsif substr($meth,0,6) eq 'infix:' {
        make Operator.funop('&infix:<' ~ $<sym> ~ '>', 2);
    } elsif substr($meth,0,5) eq 'term:' {
        make mkcall($/, '&term:<' ~ $<sym> ~ '>');
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
method label($ ) { }
method identifier($ ) { }

# Either String Op
method morename($/) {
    make ($<identifier>[0] ?? $<identifier>[0].ast !! $<EXPR>[0].ast);
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

# Turns a name like ::Foo::Bar:sym[ 'x' ] into
# { name => 'Bar:sym<x>', path => [ 'Foo '] }
# path can be undefined for a simple name like $x, which goes straight to pad
# pass $clean if you want to ignore adverbs entirely - currently needed for
# package names
method unqual_longname($/, $what, $clean?) {
    my $h = self.mangle_longname($/, $clean);
    if $h<path> {
        $/.CURSOR.sorry($what);
        return "";
    }
    return $h<name>;
}

method simple_longname($/) {
    my $r = self.mangle_longname($/);
    ($r<path>:exists) ?? [ @($r<path>), $r<name> ] !! [ 'MY', $r<name> ];
}

method mangle_longname($/, $clean?) {
    my @ns = @( $<name>.ast<names> );
    my $n = pop @ns;

    if !$clean {
        for @( $<colonpair> ) {
            $n ~= $_.ast<ext> // (
                $_.CURSOR.sorry("Invalid colonpair for name extension");
                "";
            )
        }
    }

    my @path = ($<name>.ast.<dc> || @ns) ?? (path => @ns) !! ();
    { name => $n, @path };
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
        make self.mangle_longname($<longname>);
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
    {
        my $*paren = 0;
        my $*dba = 'anonymous rule';
        my $*symtext;
        my $*endsym;
        $rxop.check
    }
    my ($orxop, $mb) = ::GLOBAL::OptRxSimple.run($rxop);
    self.block_expr($/, self.thunk_sub(::Op::RegexBody.new(|node($/),
            canback => $mb, pre => @lift, rxop => $orxop),
        class => 'Regex', params => ['self']));
}

method quote:sym</ /> ($/) { make self.op_for_regex($/, $<nibble>.ast) }
method quote:rx ($/) {
    self.extract_rx_adverbs(False, False, $<quibble>);
    make self.op_for_regex($/, $<quibble>.ast);
}
method quote:m  ($/) {
    make ::Op::CallMethod.new(|node($/), name => 'ACCEPTS',
            receiver => self.op_for_regex($/, $<quibble>.ast),
            args => [ mklex($/, '$_'),
                self.extract_rx_adverbs(True, False, $<quibble>) ]);
}

method encapsulate_regex($/, $rxop, :$goal, :$passcut = False,
        :$passcap = False) {
    my @lift = $rxop.oplift;
    my $lad = $rxop.lad;
    my ($nrxop, $mb) = ::GLOBAL::OptRxSimple.run($rxop);
    if defined $goal {
        unshift @lift, ::Op::Bind.new(|node($/), readonly => True,
            lhs => mklex($/, '$*GOAL'),
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
    sub _symtext($name) {
        ($name ~~ /\:sym\<(.*)\>/) ?? ($name.substr(0, $/.from), ~$0) !!
            ($name ~~ /\:(\w+)/) ?? ($name.substr(0, $/.from), ~$0) !!
            ($name, Str);
    }

    my ($m,$p) = $<deflongname> ??
        self.mangle_longname($<deflongname>).<name path> !! ();
    my $multiness = $*MULTINESS || 'only';

    $*CURLEX<!name> = $m;
    if $m ~~ Op {
        $*CURLEX<!name> = $*CURLEX<!cleanname> = '::($name)';
    } elsif defined $m {
        $*CURLEX<!cleanname !sym> = _symtext($m);
        $multiness = 'multi' if defined $*CURLEX<!sym>;
    }
    $*CURLEX<!multi> = $multiness;

    self.install_sub($/, $*CURLEX<!sub>, scope => $*SCOPE, name => $m,
        method_type => (($*SCOPE || 'has') eq 'has') ?? 'normal' !! Any,
        path => $p, :$multiness, class => 'Regex');
}

method regex_def_2 ($, $/ = $*cursor) {
    if $<signature> > 1 {
        $/.CURSOR.sorry("Too many signatures on regex");
    }
}

method regex_def($/) {
    my $endsym;
    for map *.ast, @$<trait> -> $t {
        if $t<unary> || $t<binary> || $t<defequiv> || $t<of> {
            # Ignored for now
        }
        elsif defined $t<endsym> {
            $endsym = $t<endsym>;
        }
        else {
            $/.CURSOR.sorry("Unhandled regex trait $t.keys.[0]");
        }
    }

    if $*CURLEX<!multi> eq 'proto' {
        if ($<signature> && $<signature>[0].ast.params != 1) ||
                !$<regex_block><onlystar> {
            $/.CURSOR.sorry('Only {*} protoregexes with no parameters are supported');
        }
        @*MEMOS[0]<proto_endsym>{$*CURLEX<!cleanname>} = $endsym
            if defined $*CURLEX<!cleanname>;
    } else {
        $endsym //= @*MEMOS[0]<proto_endsym>{$*CURLEX<!cleanname>} if
            defined $*CURLEX<!cleanname>;
    }

    my $ast = $<regex_block>.ast;
    if $*CURLEX<!multi> eq 'proto' {
        $ast = ::RxOp::ProtoRedis.new(name => $*CURLEX<!name>);
    }

    {
        my $*paren = 0;
        my $*symtext = $*CURLEX<!sym>;
        my $*endsym = $endsym;
        my $*dba = $*CURLEX<!name> // 'anonymous regex';
        $ast.check;
    }
    my @lift = $ast.oplift;
    $*CURLEX<!sub>.ltm = ::GLOBAL::OptRxSimple.run_lad($ast.lad);
    ($ast, my $mb) = ::GLOBAL::OptRxSimple.run($ast);
    $*CURLEX<!sub>.add_my_name('$*/');
    $*CURLEX<!sub>.code = ::Op::RegexBody.new(|node($/), pre => @lift,
        name => ($*CURLEX<!name> // ''), rxop => $ast, canback => $mb);
    make mklex($/, $*CURLEX<!sub>.outervar);
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
        push @z, $q<sep> if defined $q<sep>;
        $atom = ::RxOp::Quantifier.new(min => $q<min>, max => $q<max>,
            nonlisty => $q<nonlisty>,
            zyg => [@z], minimal => ($q<mod> && $q<mod> eq '?'));
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
    # XXX can't handle normspace well since it's not labelled 1*/2*
    my $h = $<embeddedblock> ?? { min => 0, cond =>
                self.inliney_call($/, $<embeddedblock>.ast) } !!
            $<quantified_atom> ?? { min => 1, sep => $<quantified_atom>.ast } !!
            { min => +~$0, max => ($1 ?? +~$1 !!
                defined($/.index('..')) ?? Any !! +~$0) };
    $h<mod> = $<quantmod>.ast;
    make $h;
}

method quantmod($/) {
    my $t = ~$/;
    if $t eq '' { make Any; return Nil }
    if substr($t,0,1) eq ':' { $t = substr($t,1,chars($t)-1) }
    if $t eq '+' {
        $/.CURSOR.sorry('STD parses + as a quantmod but there is nothing at all in S05 to explain what it should _do_'); #XXX
        make Any;
        return Nil;
    }
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
        $<embeddedblock>.ast.signature = Sig.simple('$¢');
        $<embeddedblock>.ast.add_my_name('$¢', :noinit, |mnode($/));
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
    make self.rxcapturize($/, Any, self.encapsulate_regex($/, $<nibbler>.ast,
            passcut => True));
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

        make self.rxcapturize($/, $cid, $a);
        return Nil;
    }
    make ::RxOp::VarString.new(param => ~$<variable>,
        ops => self.rxembed($/, self.do_variable_reference($/, $<variable>.ast), True));
}

method rxcapturize($/, $name, $_rxop) {
    my $rxop = $_rxop;
    if !$rxop.^isa(::RxOp::Capturing) {
        # $<foo>=[...]
        $rxop = self.encapsulate_regex($/, $rxop, passcut => True,
            passcap => True);
    }

    # $<foo>=(...)
    if +$rxop.captures == 1 && !defined($rxop.captures.[0]) {
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
            $rxop = $rxop ?? ::RxOp::SeqAlt.new(zyg => [ $exp, $rxop ]) !! $exp;
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
    my $name = self.unqual_longname($<longname>, "Qualified method calls NYI");
    if $<assertion> {
        make $<assertion>[0].ast;
    } elsif $name eq 'sym' {
        make ::RxOp::Sym.new(igcase => %*RX<i>, igmark => %*RX<a>);
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
    } elsif !$<nibbler>[0] && !$<arglist>[0] {
        make ::RxOp::Subrule.new(method => $name);
    } else {
        my $args = $<nibbler> ??
            [ self.op_for_regex($/, $<nibbler>[0].ast) ] !!
            $<arglist>[0].ast;

        my $callop = ::Op::CallMethod.new(|node($/),
            receiver => mklex($/, '$¢'),
            name => $name,
            args => $args);

        my $regex = self.rxembed($/, $callop, True);

        make ::RxOp::Subrule.new(regex => $regex);
    }
    make self.rxcapturize($/, $name, $/.ast);
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
        $<embeddedblock>.ast.signature = Sig.simple('$¢');
        $<embeddedblock>.ast.add_my_name('$¢', :noinit, |mnode($/));
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

sub mkstringycat($/, *@strings) {
    my @a;
    for @strings -> $s {
        my $i = ($s !~~ Op) ?? ::Op::StringLiteral.new(|node($/),
            text => $s) !! $s;

        # this *might* belong in an optimization pass
        if @a && @a[*-1] ~~ ::Op::StringLiteral &&
                $i ~~ ::Op::StringLiteral {
            @a[*-1] = ::Op::StringLiteral.new(|node($/),
                text => (@a[*-1].text ~ $i.text));
        } else {
            push @a, $i;
        }
    }
    if @a == 0 {
        return ::Op::StringLiteral.new(|node($/), text => "");
    } elsif  @a == 1 {
        return (@a[0] ~~ ::Op::StringLiteral) ?? @a[0] !!
            mkcall($/, '&prefix:<~>', @a[0]);
    } else {
        return mkcall($/, '&infix:<~>', @a);
    }
}
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
            $ast = $ast.split(/^^<before \h>[ $prefix || \h+ ]/).join("");
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
        make ::Op::CallMethod(|node($/), receiver => $/.ast, :name<IO>);
    }
    elsif $post eq 'run' {
        make mkcall($/, 'rungather', $/.ast);
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
    my $do = $<pblock>.ast.code;

    return False unless $do.^isa(::Op::StatementList);
    return True if $do.children == 0;
    return False if $do.children > 1;

    $do = $do.children[0];
    my @bits = $do.^isa(::Op::SimpleParcel) ?? @( $do.items ) !! $do;

    return True if @bits[0].^isa(::Op::SimplePair);

    if @bits[0].^isa(::Op::CallSub) &&
            @bits[0].invocant.^isa(::Op::Lexical) &&
            @bits[0].invocant.name eq '&infix:<=>>' {
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
        Operator.funop('&infix:<Z>', 2);
}
method infix_prefix_meta_operator:sym<X> ($/) {
    make $<infixish> ?? $<infixish>[0].ast.meta_fun($/, '&crossop', 2) !!
        Operator.funop('&infix:<X>', 2);
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

method prefix_circumfix_meta_operator:reduce ($/) {
    my $assoc = $<s><op><O><assoc>;
    my $op = $<s><op>.ast;
    my $tr = substr($/,1,1) eq '\\';
    make $op.meta_fun($/, '&reduceop', 1, mkbool($tr), mkbool($assoc eq 'list'),
        mkbool($assoc eq 'right'), mkbool($assoc eq 'chain'));
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
    make Operator.funop('&infix:<' ~ $/ ~ '>', 2);
}
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

method INFIX($/) {
    my $fn = $<infix>.ast;
    my ($st,$lhs,$rhs) = self.whatever_precheck($fn, $<left>.ast, $<right>.ast);

    make $fn.with_args($/, $lhs, $rhs);

    if $fn.assignish {
        # Assignments to has and state declarators are rewritten into
        # an appropriate phaser
        if $lhs.^isa(::Op::Lexical) && $lhs.state_decl {
            my $cv = self.gensym;
            $*CURLEX<!sub>.add_state_name(Str, $cv);
            make ::Op::StatementList.new(|node($/), children => [
                ::Op::Start.new(condvar => $cv, body => $/.ast),
                ::Op::Lexical.new(name => $lhs.name)]);
        }
        elsif $lhs.^isa(::Op::Attribute) && !defined($lhs.initializer.ivar) {
            my $init = self.thunk_sub($rhs,
                :name($lhs.initializer.name ~ " init"));
            $lhs.initializer.ivar = self.gensym;
            $*CURLEX<!sub>.add_my_sub($lhs.initializer.ivar, $init);
            $lhs.initializer.ibody = $init.xref;
            make $lhs;
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

    sub reduce() {
        my $fa = shift @vargs;
        my $fo = shift @ops;
        if @ops {
            mklet($fa, -> $lhs { mklet(@vargs[0], -> $rhs {
                @vargs[0] = $rhs;
                ::Op::ShortCircuit.new(|node($/), kind => '&&', args =>
                    [ $fo.with_args($/, $lhs, $rhs), reduce() ]) }) })
        } else {
            $fo.with_args($/, $fa, @vargs[0])
        }
    }

    make self.whatever_postcheck($/, $st, reduce());
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
    make Operator.funop('&postcircumfix:<[ ]>', 1, @( $<semilist>.ast ));
}
method postcircumfix:sym<{ }> ($/) {
    make Operator.funop('&postcircumfix:<{ }>', 1, @( $<semilist>.ast ));
}
method postcircumfix:sym«< >» ($/) {
    make Operator.funop('&postcircumfix:<{ }>', 1, $<nibble>.ast);
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
        my $c = self.mangle_longname($<longname>);
        make ::Operator::Method.new(name => $c<name>, path => $c<path>);
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
        make Operator.funop('&postfix:<++>', 1);
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
        make Operator.funop('&postfix:<++>', 1);
        return Nil;
    }
    if $<sym> eq '.^' || $<sym> eq '.?' {
        make $<dottyop>.ast.clone(:meta(substr($<sym>,1)));
    } else {
        $/.CURSOR.sorry("NYI dottyop form $<sym>");
        make Operator.funop('&postfix:<++>', 1);
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
                ::Op::ContextVar.new(name => '$*/'),
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

        my $body = ::Metamodel::StaticSub.new(
            outerx => $*CURLEX<!sub>.xref,
            class => 'WhateverCode',
            unit => $*unit,
            transparent => True,
            code => $term,
            in_class => $*CURLEX<!sub>.in_class,
            cur_pkg => $*CURLEX<!sub>.cur_pkg);

        $body.signature = ::GLOBAL::Sig.new(params => [
            map { ::Sig::Parameter.new(slot => $_, name => $_) }, @$st ]);
        $body.add_my_name($_, :noinit) for @$st;

        $*CURLEX<!sub>.add_child($body);
        $*CURLEX<!sub>.add_my_sub($slot, $body);

        ::Op::WhateverCode.new(ops => Any, vars => $st, :$slot, |node($/));
    } else {
        $term;
    }
}

# term :: Op
method term:value ($/) { make $<value>.ast }

method package_var($/, $slot, $name, $path, :$list, :$hash) {
    $/.CURSOR.trymop({
        $/.CURSOR.check_categorical($slot);
        $*CURLEX<!sub>.add_common_name($slot,
            $*CURLEX<!sub>.find_pkg($path), $name, |mnode($/));
    });
    ::Op::PackageVar.new(|node($/), :$slot, :$name, :$path, :$list, :$hash);
}

method term:name ($/) {
    my ($id, $path) = self.mangle_longname($<longname>).<name path>;

    $id = '&' ~ $id if $<args>;

    if defined $path {
        make self.package_var($/, self.gensym, $id, $path);
    } else {
        make mklex($/, $id);
    }

    if $<postcircumfix> {
        make mkcall($/, '&_param_role_inst', $/.ast,
            @( $<postcircumfix>[0].ast.args ));
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

method do_variable_reference($M, $v) {
    if $v<term> {
        return $v<term>;
    }

    my $tw = $v<twigil>;
    my $sl = $v<sigil> ~ $tw ~ $v<name>;
    my $list = $v<sigil> eq '@';
    my $hash = $v<sigil> eq '%';

    if defined($v<rest>) && $tw ~~ /<[*=~?^:]>/ {
        $M.CURSOR.sorry("Twigil $tw cannot be used with qualified names");
        return ::Op::StatementList.new;
    }

    if $tw eq '!' {
        my $pclass;
        if $v<rest> {
            $pclass = $*unit.get_item($*CURLEX<!sub>.find_pkg($v<rest>));
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
        if defined $v<rest> {
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
        if defined($v<rest>) {
            self.package_var($M, self.gensym, $sl, $v<rest>,
                hash => ($v<sigil> eq '%'), list => ($v<sigil> eq '@'))
        } elsif $tw eq '?' && $sl eq '$?POSITION' {
            mkcall($M, '&infix:<..^>',
                ::Op::Num.new(|node($M), value => [10, ~$M.from]),
                ::Op::Num.new(|node($M), value => [10, ~$M.to]));
        } elsif $tw eq '?' && $sl eq '$?LINE' {
            ::Op::Num.new(|node($M), value => [10, ~$M.cursor.lineof($M.from)]);
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

    my ($name, $rest);
    my $dsosl = $<desigilname> ?? $<desigilname>.ast !!
        $<sublongname> ?? $<sublongname>.ast !!
        Any;
    if defined($dsosl) && defined($dsosl<ind>) {
        make { term => self.docontext($/, $sigil, $dsosl<ind>) };
        return;
    } elsif defined $dsosl {
        ($name, $rest) = $dsosl<name path>;
    } elsif $<name> {
        # Both these cases are marked XXX in STD.  I agree.  What are they for?
        if $<name>[0].ast<dc> {
            $/.CURSOR.sorry("*ONE* pair of leading colons SHALL BE ENOUGH");
            make { term => ::Op::StatementList.new };
            return;
        }
        if substr(~$/,0,3) eq '$::' {
            $rest = $<name>[0].ast.<names>;
            $name = pop $rest;
        } else {
            if $<name>[0].ast<names> > 1 {
                $/.CURSOR.sorry("Nonsensical attempt to qualify a self-declared named parameter detected");
                make { term => ::Op::StatementList.new };
                return;
            }
            $name = $<name>[0].ast<names>[0];
            $twigil = ':';
        }
    } elsif $<special_variable> {
        $name = substr(~$<special_variable>, 1);
        $twigil = '*' if $name eq '/' or $name eq '!';
    } elsif $<index> {
        make { capid => $<index>.ast, term =>
            mkcall($/, '&postcircumfix:<[ ]>',
                ::Op::ContextVar.new(name => '$*/'),
                ::Op::Num.new(value => $<index>.ast))
        };
        return Nil;
    } elsif $<postcircumfix> {
        if $<postcircumfix>[0].reduced eq 'postcircumfix:sym<< >>' { #XXX fiddly
            make { capid => $<postcircumfix>[0].ast.args[0].text, term =>
                mkcall($/, '&postcircumfix:<{ }>',
                    ::Op::ContextVar.new(name => '$*/'),
                    @( $<postcircumfix>[0].ast.args))
            };
            return;
        } else {
            make { term => self.docontext($/, $sigil, $<postcircumfix>[0].ast.args[0]) };
            return;
        }
    } else {
        $/.CURSOR.sorry("Non-simple variables NYI");
        make { term => ::Op::StatementList.new };
        return;
    }

    make {
        sigil => $sigil, twigil => $twigil, name => $name, rest => $rest
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
    $twigil = '*' if $name && ($name eq '/' || $name eq '!');

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
    my $rw = False;
    my $copy = False;
    my $sorry;
    my $slurpy = False;
    my $slurpycap = False;
    my $optional = False;
    my $rwt = False;
    my $type;

    if $<type_constraint> {
        my $t = self.simple_longname($<type_constraint>[0]<typename><longname>);
        $type = $*unit.get_item($*CURLEX<!sub>.find_pkg($t));
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
    $*unit.deref($default).set_name("$/ init") if $default;

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
        $*CURLEX<!sub>.signature = $sig if $*SIGNUM;
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
                my $cl = $*unit.deref($*CURLEX<!sub>.methodof);
                # XXX type checking against roles NYI
                if $cl !~~ ::Metamodel::Role &&
                        $cl !~~ ::Metamodel::ParametricRole {
                    .tclass = $cl.xref;
                }
            } elsif !$*CURLEX<!sub>.returnable {
                .tclass = $*unit.get_item($*CURLEX<!sub>.find_pkg(['MY','Mu']));
            }
        }
    }

    my $sig = Sig.new(params => @p);
    $*CURLEX<!sub>.signature = $sig if $*SIGNUM;
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
                mkcall($/, '&prefix:<~>', ::Op::ContextVar.new(name => '$*/')),
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
        push @nyi, < sameaccent aa samecase ii th st nd rd nth x >;
        push @ok,  < g global >;
    }

    if $ismatch {
        push @nyi, < overlap ov exhaustive ex continue c pos p global g rw >;
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
        $*unit.bottom_ref = $*CURLEX<!sub>.xref;
        $*CURLEX<!sub>.strong_used = True;
        $*CURLEX<!sub>.create_static_pad;
        make ::Op::YouAreHere.new(|node($/), unitname => $*UNITNAME);
    } else {
        make $<statementlist>.ast;
    }
}
method lambda($/) {}
method embeddedblock($/) {
    $*CURLEX<!sub>.code = $<statementlist>.ast;
    $*CURLEX<!sub>.signature = Sig.simple();
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
        my @p = @( $<signature>.ast.params );
        # TODO: keep the original signature around somewhere := can find it
        for @p {
            # TODO: fanciness checks
            $_ = mklex($/, .slot, list => .list, hash => .hash);
        }
        make ::Op::SimpleParcel.new(|node($/), items => @p);
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
        return ::Op::StatementList.new if $*CURLEX<!sub>.augmenting;

    $ns = $*unit.deref($ns);

    if !$ns.^can('add_attribute') {
        $/.CURSOR.sorry("A $ns.WHAT() cannot have attributes");
        return ::Op::StatementList.new
    }

    my $nb = ::Metamodel::StaticSub.new(
        transparent=> True,
        unit       => $*unit,
        outerx     => $*CURLEX<!sub>.xref,
        name       => $name,
        cur_pkg    => $*CURLEX<!sub>.cur_pkg,
        class      => 'Method',
        signature  => Sig.simple('self'),
        code       => ::Op::GetSlot.new(name => $name,
            object => ::Op::Lexical.new(name => 'self')));
    $nb.add_my_name('self', noinit => True);
    $*CURLEX<!sub>.create_static_pad; # for protosub instance
    $nb.strong_used = True;
    $*CURLEX<!sub>.add_child($nb);
    my $at;

    $/.CURSOR.trymop({
        $*CURLEX<!sub>.add_my_sub($name ~ '!a', $nb, |mnode($/));
        $ns.add_method('only', 'private', $name, $name ~ '!a', $nb.xref);
        if $accessor {
            $ns.add_method('only', 'normal', $name, $name ~ '!a', $nb.xref);
        }
        $at = $ns.add_attribute($name, $sigil, +$accessor, Any, Any, $type);
    });

    $at ?? ::Op::Attribute.new(name => $name, initializer => $at) !!
        ::Op::StatementList.new;
}

method variable_declarator($/) {
    if $*MULTINESS {
        $/.CURSOR.sorry("Multi variables NYI");
    }
    for @$<trait> -> $t {
        if $t.ast<rw> {
        } else {
            $/.CURSOR.sorry("Trait $t.ast.keys.[0] not available on variables");
        }
    }
    if $<post_constraint> || $<postcircumfix> || $<semilist> {
        $/.CURSOR.sorry("Postconstraints, and shapes on variable declarators NYI");
    }

    my $scope = $*SCOPE // 'my';

    if $scope eq 'augment' || $scope eq 'supersede' {
        $/.CURSOR.sorry("Illogical scope $scope for simple variable");
    }

    my $typeconstraint;
    if $*OFTYPE {
        $typeconstraint = self.simple_longname($*OFTYPE<longname>);
        $/.CURSOR.sorry("Common variables are not unique definitions and may not have types") if $scope eq 'our';
    }

    my $v = $<variable>.ast;
    my $t = $v<twigil>;
    my $list = $v<sigil> eq '@';
    my $hash = $v<sigil> eq '%';
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

    if defined $v<rest> {
        $/.CURSOR.sorry(":: syntax is only valid when referencing variables, not when defining them.");
    }

    my $name = $v<sigil> ~ $v<twigil> ~ $v<name>;
    # otherwise identical to my
    my $slot = ($scope eq 'anon') ?? self.gensym !! $name;
    my $res_tc = $typeconstraint ??
        $*unit.get_item($*CURLEX<!sub>.find_pkg($typeconstraint)) !! Any;

    if $scope eq 'has' {
        make self.add_attribute($/, $v<name>, $v<sigil>, $t eq '.', $res_tc);
    } elsif $scope eq 'state' {
        $/.CURSOR.trymop({
            $/.CURSOR.check_categorical($slot);
            $*CURLEX<!sub>.add_state_name($slot, self.gensym, :$list,
                :$hash, typeconstraint => $res_tc, |mnode($/));
        });
        make mklex($/, $slot, :$list, :$hash, :state_decl);
    } elsif $scope eq 'our' {
        make self.package_var($/, $slot, $slot, ['OUR'], :$list, :$hash);
    } else {
        $/.CURSOR.trymop({
            $/.CURSOR.check_categorical($slot);
            $*CURLEX<!sub>.add_my_name($slot, :$list, :$hash,
                typeconstraint => $res_tc, |mnode($/));
        });
        make mklex($/, $slot, :$list, :$hash);
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
    my $ourname = Array; my $lexvar = self.gensym; my $name;
    my $scope = $*SCOPE;
    if $scope && $scope ne 'our' && $scope ne 'my' && $scope ne 'anon' {
        $/.CURSOR.sorry("Invalid subset scope $scope");
        $scope = 'anon';
    }
    if $<longname> {
        $scope ||= 'my';
        my $r = self.mangle_longname($<longname>[0], True);
        $name = $r<name>;
        if ($r<path>:exists) && $scope ne 'our' {
            $/.CURSOR.sorry("Block name $<longname> requires our scope");
            $scope = 'our';
        }
        if $scope eq 'our' {
            $ourname = ($r<path>:exists) ?? $r<path> !! ['OUR'];
            $ourname = [ @$ourname, $name ];
        } elsif $scope eq 'my' {
            $lexvar  = $name;
        }
    } else {
        if ($scope || 'anon') ne 'anon' {
            $/.CURSOR.sorry("Cannot have a non-anon subset with no name");
        }
        $name = 'ANON';
    }

    my $basetype = $*OFTYPE ?? self.simple_longname($*OFTYPE<longname>) !!
        ['MY', 'Any'];
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

    my $body = self.thunk_sub($<EXPR> ?? $<EXPR>[0].ast !! mklex($/, 'True'));

    my @ns = $ourname ?? @( $*CURLEX<!sub>.find_pkg($ourname) ) !!
        $*unit.anon_stash;

    $/.CURSOR.trymop({
        $*unit.create_stash([@ns]);
        $*CURLEX<!sub>.add_my_stash($lexvar, [@ns], |mnode($/));
        $*CURLEX<!sub>.add_pkg_exports($*unit, $name, [@ns], @exports);
        $*CURLEX<!sub>.create_static_pad;

        $basetype = $*unit.get_item($*CURLEX<!sub>.find_pkg([@$basetype]));
        my $obj = ::Metamodel::Subset.new(:$name, where => $body.xref,
            :$basetype);
        $*unit.bind_item([@ns], $obj.xref);
        $obj.exports = [ [@ns] ];
    });

    make mklex($/, $lexvar);
}

method make_constant($/, $scope, $name, $path) {
    $scope := $scope || 'our';

    my $slot = ($scope eq 'my' || $scope eq 'our' && !$path) ?? $name !!
        self.gensym;

    $/.CURSOR.trymop({
        $/.CURSOR.check_categorical($slot);
        if $scope eq 'our' {
            $*CURLEX<!sub>.add_common_name($slot,
                $*CURLEX<!sub>.find_pkg($path // ['OUR']), $name, |mnode($/));
        } else {
            $*CURLEX<!sub>.add_hint($slot, |mnode($/));
        }
    });

    ::Op::ConstantDecl.new(|node($/), name => $slot, init => False);
}

method make_constant_into($/, $rpath, $name, $rhs) {
    my $slot = self.gensym;
    $/.CURSOR.trymop({
        $*CURLEX<!sub>.add_common_name($slot, $rpath, $name, |mnode($/));
    });
    self.init_constant(::Op::ConstantDecl.new(|node($/), name => $slot,
        init => False), $rhs);
}

method init_constant($con, $rhs) {
    my $body = self.thunk_sub($rhs, name => "$con.name() init");
    $body.is_phaser = 2;
    $body.hint_hack = [ $*CURLEX<!sub>.xref, $con.name ];
    $body.outer.create_static_pad;
    $con.init = True;
    $con;
}

method type_declarator:constant ($/) {
    if $*MULTINESS {
        $/.CURSOR.sorry("Multi variables NYI");
    }
    my $name  = ~($<identifier> // $<variable> // self.gensym);

    make self.make_constant($/, $*SCOPE || 'our', $name, Array);
}

# note: named and unnamed enums are quite different beasts
method type_declarator:enum ($/) {
    my $scope = $*SCOPE;
    if $scope && $scope ne 'our' && $scope ne 'my' && $scope ne 'anon' {
        $/.CURSOR.sorry("Invalid enum scope $scope");
        $scope = 'anon';
    }

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

    my $basetype = $*OFTYPE ?? self.simple_longname($*OFTYPE<longname>) !!
        [ 'MY', $has_strs ?? 'Str' !! 'Int' ];

    if $<name> && $<name>.reduced eq 'longname'&& ($scope ||= 'our') ne 'anon' {
        # Longnamed enum is a kind of type definition

        my $ourpath = Array;
        my $lexvar = self.gensym;
        my $bindlex = False;
        my $r = self.mangle_longname($<longname>[0], True);
        my $name = $r<name>;
        if ($r<path>:exists) && $scope ne 'our' {
            $/.CURSOR.sorry("Enum name $<longname> requires our scope");
            $scope = 'our';
        }

        if $scope eq 'our' {
            $ourpath = ($r<path>:exists) ?? $r<path> !! ['OUR'];
            if !($r<path>:exists) {
                $lexvar  = $name;
                $bindlex = True;
            }
        } elsif $scope eq 'my' {
            $lexvar  = $name;
            $bindlex = True;
        }

        $/.CURSOR.trymop({
            my @ns = $ourpath ?? (@( $*CURLEX<!sub>.find_pkg($ourpath) ), $name) !!
                $*unit.anon_stash;
            $*unit.create_stash([@ns]);
            $*CURLEX<!sub>.add_my_stash($lexvar, [@ns], |mnode($/));
            my $obj  = ::Metamodel::Class.new(:$name);
            $obj.exports = [ [@ns] ];
            $*unit.bind_item([@ns], $obj.xref);

            $obj.add_super($*unit.get_item($*CURLEX<!sub>.find_pkg(
                ['MY', ($has_strs ?? 'Str' !! 'Int') ~ "BasedEnum"])));
            $obj.add_super($*unit.get_item($*CURLEX<!sub>.find_pkg($basetype)));

            my $nb = ::Metamodel::StaticSub.new(
                transparent=> True,
                unit       => $*unit,
                outerx     => $*CURLEX<!sub>.xref,
                name       => $name,
                cur_pkg    => $*CURLEX<!sub>.cur_pkg,
                class      => 'Method',
                signature  => Sig.simple('self'),
                code       => self.init_constant(
                    self.make_constant($/, 'anon', Any, Any),
                    ::Op::CallMethod.new(name => 'new',
                        receiver => mklex($/, 'EnumMap'), args => [$<term>.ast])));

            $nb.add_my_name('self', noinit => True);
            $*CURLEX<!sub>.create_static_pad;
            $nb.strong_used = True;
            $*CURLEX<!sub>.add_child($nb);
            $*CURLEX<!sub>.add_my_sub($lexvar ~ '!enums', $nb, |mnode($/));
            $obj.add_method('only', 'normal', 'enums', $lexvar ~ '!enums',
                $nb.xref);
            $obj.close;

            for @pairs {
                self.make_constant_into($/, @ns, .key, rhs =>
                    ::Op::CallSub.new(invocant => mklex($/, $lexvar),
                        args => [ ::Op::StringLiteral.new(text => .key) ]));
            }

            for @pairs {
                self.init_constant(self.make_constant($/, $scope, .key, Any),
                    ::Op::CallSub.new(invocant => mklex($/, $lexvar),
                        args => [ ::Op::StringLiteral.new(text => .key) ]));
            }
        });

        make mklex($/, $lexvar);
    } else {
        make self.init_constant(
            self.make_constant($/, $<name> ?? $scope !! 'anon', ~$<name>, Any),
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
    for map *.ast, @tr -> $tr {
        if $pack && ($tr<name>:exists) {
            my ($name, $path) = $tr<name path>;

            $/.CURSOR.sorry("superclass $name declared outside of any class"),
                next unless $sub.body_of;
            $/.CURSOR.sorry("superclass $name declared in an augment"),
                next if $sub.augmenting;

            $/.CURSOR.trymop({
                $*unit.deref($pack).add_super($*unit.get_item($sub.find_pkg(
                    [ @($path // ['MY']), $name ])));
            });
        } elsif $pack && $tr<export> {
            my @exports = @( $tr<export> );
            $sub.outer.add_pkg_exports($*unit, $*unit.deref($pack).name,
                $sub.cur_pkg, @exports);
        } elsif !$pack && $tr<export> {
            my @exports = @( $tr<export> );
            $sub.outer.add_exports($*unit, '&' ~ $sub.name, @exports);
            $sub.strong_used = True;
            $sub.outer.create_static_pad;
            $sub.exports //= [];
            push $sub.exports, [ @($sub.outer.find_pkg(
                ['OUR','EXPORT',$_])), '&' ~ $sub.name ] for @exports;
        } elsif !$pack && $tr<nobinder> {
            $sub.signature = Any;
        } elsif !$pack && $tr<return_pass> {
            $sub.returnable = False;
        } elsif !$pack && $tr<of> {
        } elsif !$pack && $tr<rw> {
        } elsif !$pack && $tr<unsafe> {
            $sub.unsafe = True;
        } else {
            $/.CURSOR.sorry("Unhandled trait $tr.keys[0] for this context");
        }
    }
}

# normally termish's ast is not used, but it becomes the used ast under
# nulltermish.
method termish($/) { make $<term>.ast }
method nulltermish($/) {}
method EXPR($/) { make $<root>.ast }
method modifier_expr($/) { make $<EXPR>.ast }
method default_value($/) { make self.thunk_sub($<EXPR>.ast).xref }
method thunk_sub($code, :$params = [], :$name, :$class, :$ltm) {
    my $n = ::Metamodel::StaticSub.new(
        outerx => $*CURLEX<!sub>.xref,
        class => $class // 'Block',
        unit => $*unit,
        name => $name // 'ANON',
        transparent => True,
        code => $code,
        ltm => $ltm,
        in_class => $*CURLEX<!sub>.in_class,
        cur_pkg => $*CURLEX<!sub>.cur_pkg);
    $n.signature = Sig.simple(@$params);
    $n.add_my_name($_, :noinit) for @$params;
    $n.add_my_name('$*/') if $class eq 'Regex';
    $*CURLEX<!sub>.add_child($n);
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
        $*CURLEX<!sub>.add_label(~$<label><identifier>);
        make ::Op::Labelled.new(|node($/), name => ~$<label><identifier>,
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

    my @can = @( $u2.mainline.find_pkg([$name.split('::')]) );
    my @exp = (@can, 'EXPORT', 'DEFAULT');

    # XXX I am not sure how need binding should work in the :: case
    if $name !~~ /"::"/ {
        $*CURLEX<!sub>.lexicals{$name} =
            ::Metamodel::Lexical::Stash.new(path => @can);
    }

    for $*unit.list_stash(@exp) -> $tup {
        my $uname = $tup[0];
        my $lex;
        if $tup[1] eq 'var' {
            if $tup[2] && !$tup[2][0] {
                $lex = ::Metamodel::Lexical::Common.new(path => @exp, name => $uname);
            } elsif $tup[2] {
                $lex = ::Metamodel::Lexical::Stash.new(path => [@exp, $uname]);
            }
        } elsif $tup[1] eq 'graft' {
            $lex = ::Metamodel::Lexical::Stash.new(path => $tup[2]);
        } else {
            die "weird return";
        }

        $*CURLEX<!sub>.lexicals{$uname} = $lex;
    }
}

method open_package_def($, $/ = $*cursor) {
    my %_decl2mclass = (
        package => ::Metamodel::Package,
        class   => ::Metamodel::Class,
        module  => ::Metamodel::Module,
        grammar => ::Metamodel::Grammar,
        role    => ::Metamodel::Role,
    );
    my $sub = $*CURLEX<!sub>;

    if $*MULTINESS {
        $/.CURSOR.sorry("Multi variables NYI");
    }

    my $scope = $*SCOPE;
    if !$<longname> {
        $scope = 'anon';
    }

    if $scope eq 'supersede' {
        $/.CURSOR.sorry('Supercede is not yet supported');
        $scope = 'our';
    }
    if $scope eq 'has' || $scope eq 'state' {
        $/.CURSOR.sorry("Illogical scope $scope for package block");
        $scope = 'our';
    }

    if $scope eq 'augment' {
        my $r = self.mangle_longname($<longname>[0], True);
        my $name = $r<name>;
        my @augpkg = @( $r<path> // ['MY'] );

        my $pkg = $sub.outer.find_pkg([ @augpkg, $name ]);
        my $so  = $*unit.get_item($pkg);
        my $dso = $*unit.deref($so);

        if $dso.^isa(::Metamodel::Role) {
            $/.CURSOR.panic("Illegal augment of a role");
        }

        my @ah = $so;
        $sub.augment_hack = @ah;
        $sub.body_of = $sub.in_class = $so;
        $sub.cur_pkg = $pkg;
        $sub.augmenting = True;
        $sub.set_name("augment-$dso.name()");
    } else {
        my ($name, $ourpkg);
        my $type = %_decl2mclass{$*PKGDECL};
        if ($*PKGDECL//'role') eq 'role' && $<signature> {
            $sub.signature = $<signature>.ast;
            $type = ::Metamodel::ParametricRole;
        }
        my @ns;
        if $<longname> {
            my $r = self.mangle_longname($<longname>[0], True);
            $name = $r<name>;
            if ($r<path>:exists) && $scope ne 'our' {
                $/.CURSOR.sorry("Block name $<longname> requires our scope");
                $scope = 'our';
            }
            if $scope eq 'our' {
                $ourpkg = ($r<path>:exists) ?? $r<path> !! ['OUR'];
            }
            if !$r<path> && ($*CURLEX<!sub>.outer.lexicals.{$r<name>} ~~ ::Metamodel::Lexical::Stash) {
                @ns = @( $*CURLEX<!sub>.outer.find_pkg(['MY',$r<name>]) );
            }
            $*CURLEX<!sub>.outervar = ($scope eq 'anon' || ($r<path>:exists))
                ?? self.gensym !! $name;
        } else {
            $*CURLEX<!sub>.outervar = self.gensym;
            $name = 'ANON';
        }

        my $old = @ns ?? $*unit.get_item([@ns]) !! Any;

        if $old && ($old.[0] ne $*unit.name || $*unit.deref($old).closed) {
            $/.CURSOR.panic("Redefinition of class [@ns]");
        }
        my $obj;
        if $old {
            $obj = $*unit.deref($old);
        } else {
            @ns = $ourpkg ?? (@( $sub.outer.find_pkg($ourpkg) ), $name) !!
                $*unit.anon_stash;

            $*unit.create_stash([@ns]);

            $/.CURSOR.trymop({
                $sub.outer.add_my_stash($*CURLEX<!sub>.outervar, [@ns],
                    |mnode($/));
                $obj = $type.new(:$name);
                $obj.exports = [ [@ns] ];
                $*unit.bind_item([@ns], $obj.xref);
            });
        }

        $sub.body_of = $sub.in_class = $obj.xref;
        $sub.cur_pkg = [@ns];

        self.process_block_traits($/, $<trait>);
        $sub.set_name($*PKGDECL ~ "-" ~ $obj.name);
    }
}

method package_def ($/) {
    my $sub = $*CURLEX<!sub>;

    my $bodyvar = self.gensym;
    $sub.outer.add_my_sub($bodyvar, $sub);
    $sub.code = ($<blockoid> // $<statementlist>).ast;

    if $sub.augmenting {
        my $ah = $sub.augment_hack;
        $sub.augment_hack = Any;

        my $ph = ::Metamodel::StaticSub.new(
            unit       => $*unit,
            outerx     => $sub.xref,
            cur_pkg    => [ 'GLOBAL' ],
            name       => 'ANON',
            is_phaser  => 0,
            augment_hack => $ah,
            class      => 'Code',
            code       => ::Op::StatementList.new(children => []),
            run_once   => $sub.run_once);
        $sub.create_static_pad;
        $sub.add_child($ph);

        make ::Op::CallSub.new(|node($/), invocant => mklex($/, $bodyvar));
    }
    else {
        my $obj = $*unit.deref($sub.body_of);

        if $*DECLARAND<stub> {
            push $*unit.stubbed_stashes, ($obj => $/.CURSOR);

            make mklex($/, $*CURLEX<!sub>.outervar);
        }
        else {
            $obj.close;

            if $obj ~~ ::Metamodel::ParametricRole {
                $sub.parametric_role_hack = $obj.xref;
                $sub.add_my_name('*params', :noinit);
                $sub.create_static_pad;

                make mklex($/, $*CURLEX<!sub>.outervar);
            } else {
                make ::Op::StatementList.new(|node($/), children => [
                    ::Op::CallSub.new(invocant => mklex($/, $bodyvar)),
                    ::Op::Lexical.new(name => $*CURLEX<!sub>.outervar) ]);
            }
        }
    }
}

method trait_mod:is ($/) {
    my $trait = ~$<longname>;
    my $noparm;

    if $/.CURSOR.is_name($trait) {
        make self.mangle_longname($<longname>);
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
    } else {
        make { $trait => True };
    }

    if $noparm && $<circumfix> {
        $/.CURSOR.sorry($noparm);
    }
}
method trait_mod:of ($/) {
    make { of => self.simple_longname($<typename><longname>) }
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
        :$path, :$name is copy, :$method_type is copy, :$contextual is copy) {

    $multiness ||= 'only';
    $path := Any if $name ~~ Op; #Hack

    if !$scope {
        if !defined($name) {
            $scope = 'anon';
        } elsif defined($path) {
            $scope = 'our';
        } elsif defined($method_type) {
            $scope = 'has';
        } else {
            $scope = 'my';
        }
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

    if $scope ne 'our' && defined($path) {
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
    if $method_targ {
        $method_targ = $*unit.deref($method_targ);
    } elsif defined($method_type) {
        $/.CURSOR.sorry("Methods must be used in some kind of package");
        $method_type = Str;
    }

    if $name ~~ Op && (!defined($method_type) || $scope ne 'has' ||
            $method_targ !~~ ::Metamodel::ParametricRole) {
        $/.CURSOR.sorry("Computed names are only implemented for parametric roles");
        $name = "placeholder";
    }

    my $bindlex = $scope eq 'my' || ($scope eq 'our' && !$path);

    $sub.set_name(($name ~~ Op) ?? '::($name)' !!
        defined($method_type) ?? $method_targ.name ~ "." ~ $name !!
        ($name // 'ANON'));
    $sub.class = $class;
    $sub.returnable = True;

    my Str $symbol;
    $/.CURSOR.trymop({
        if $bindlex && $class eq 'Regex' {
            $symbol = '&' ~ $name;
            my $proto = $symbol;
            $proto ~~ s/\:.*//;
            $sub.outer.add_dispatcher($proto, |mnode($/))
                if $multiness ne 'only' && !$sub.outer.lexicals.{$proto};
            $symbol ~= ":(!proto)" if $multiness eq 'proto';
        } elsif $bindlex {
            $symbol = '&' ~ $name;
            if $multiness ne 'only' && !$sub.outer.lexicals.{$symbol} {
                $/.CURSOR.check_categorical($symbol);
                $sub.outer.add_dispatcher($symbol, |mnode($/))
            }

            given $multiness {
                when 'multi' { $symbol ~= ":({ self.gensym })"; }
                when 'proto' { $symbol ~= ":(!proto)"; }
            }
        } else {
            $/.CURSOR.check_categorical($symbol);
            $symbol = self.gensym;
        }

        $sub.outervar = $symbol;
        $sub.methodof = defined($method_type) ?? $method_targ.xref !! Any;
        $sub.outer.add_my_sub($symbol, $sub, |mnode($/));

        if defined($method_type) || $scope eq 'our' {
            $sub.strong_used = True;
            $sub.outer.create_static_pad;
        }

        if defined($method_type) {
            if $sub.outer.augment_hack {
                push $sub.outer.augment_hack,
                    [ $multiness, $method_type, $name, $symbol, $sub.xref ];
            } else {
                $method_targ.add_method($multiness, $method_type, $name,
                    $symbol, $sub.xref);
            }
        }

        if $scope eq 'our' {
            $sub.exports = [[@($sub.outer.find_pkg($path // ['OUR'])), '&'~$name]];
        }
    });
}

# always a sub, though sometimes it's an implied sub after multi/proto/only
method routine_def_1 ($, $/ = $*cursor) {
    my $cx = $<sigil> && $<sigil>[0] eq '&*';

    my ($m,$p) = $<deflongname>[0] ??
        self.mangle_longname($<deflongname>[0]).<name path> !! ();

    self.install_sub($/, $*CURLEX<!sub>, scope => $*SCOPE, name => $m,
        path => $p, contextual => $cx, multiness => $*MULTINESS, :class<Sub>);
}

method routine_def_2 ($, $/ = $*cursor) {
    if $<multisig> > 1 {
        $/.CURSOR.sorry("You may only use *one* signature");
    }
    $*CURLEX<!sub>.signature = $<multisig> ?? $<multisig>[0].ast !! Any;
    self.process_block_traits($/, $<trait>);
}

method routine_def ($/) {
    $*CURLEX<!sub>.code = $<blockoid>.ast;
    make mklex($/, $*CURLEX<!sub>.outervar);
}

method method_def_1 ($, $/ = $*cursor) {
    my $type = $<type> ?? ~$<type> !! '';
    if $type ne '' && $*HAS_SELF eq 'partial' {
        $type = '';
        $/.CURSOR.sorry("Type symbols cannot be used with submethod");
    }

    my ($m,$p) = $<longname> ??
        self.mangle_longname($<longname>).<name path> !! ();

    self.install_sub($/, $*CURLEX<!sub>, scope => $*SCOPE, name => $m,
        method_type => ($type eq '^' ?? 'meta' !! $type eq '!' ?? 'private' !!
            $*HAS_SELF eq 'partial' ?? 'sub' !! 'normal'),
        path => $p, multiness => $*MULTINESS,
        :class($*HAS_SELF eq 'partial' ?? 'Submethod' !! 'Method'));
}

method method_def_2 ($, $/ = $*cursor) {
    if $<multisig> > 1 {
        $/.CURSOR.sorry("You may only use *one* signature");
    }
    $*CURLEX<!sub>.signature = $<multisig> ?? $<multisig>[0].ast !! Any;
    self.process_block_traits($/, $<trait>);
}

method method_def ($/) {
    $*CURLEX<!sub>.code = $<blockoid>.ast;
    make mklex($/, $*CURLEX<!sub>.outervar);
}

method block($/) {
    $*CURLEX<!sub>.code = $<blockoid>.ast;
    make $*CURLEX<!sub>
}

# :: Body
method pblock($/) {
    #my $rw = $<lambda> && $<lambda> eq '<->'; TODO
    $*CURLEX<!sub>.code = $<blockoid>.ast;
    make $*CURLEX<!sub>;
}

method xblock($/) { make [ $<EXPR>.ast, $<pblock>.ast ] }

# returns Body of 0 args
method blast($/) {
    if $<block> {
        make $<block>.ast;
    } else {
        make self.thunk_sub($<statement>.ast);
    }
}

method statement_prefix:do ($/) {
    make self.inliney_call($/, $<blast>.ast);
}
method statement_prefix:gather ($/) {
    $<blast>.ast.gather_hack = True;
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

# TODO: retain and return a value
method statement_prefix:INIT ($/) {
    $<blast>.ast.is_phaser = 0;
    $*CURLEX<!sub>.create_static_pad;
    make ::Op::StatementList.new;
}
# XXX 'As soon as possible' isn't quite soon enough here
method statement_prefix:BEGIN ($/) {
    $<blast>.ast.is_phaser = 2;
    $*CURLEX<!sub>.create_static_pad;
    make ::Op::StatementList.new;
}
method statement_prefix:CHECK ($/) {
    $<blast>.ast.is_phaser = 2;
    $*CURLEX<!sub>.create_static_pad;
    make ::Op::StatementList.new;
}

method statement_prefix:END ($/) {
    $<blast>.ast.is_phaser = 1;
    $*CURLEX<!sub>.create_static_pad;
    make ::Op::StatementList.new;
}

method comp_unit($/) {
    $*CURLEX{'!sub'}.code = $<statementlist>.ast;
    $*CURLEX{'!sub'}.close;

    make $*unit;
}
