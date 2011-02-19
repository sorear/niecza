# a07a966de52560ebbeea470bc29373a08182a869

class NieczaActions;

use Op;
use RxOp;
use Body;
use Unit;
use Sig;
use CClass;
use OptRxSimple;
use OpHelpers;
use Operator;

method get_op_sym($M) {
    if $M.reduced eq '::($name)' { # XXX STD miscompilation
        return ~$M;
    } elsif $M.reduced ~~ /\:sym\<(.*)\>/ {
        return ~$0;
    } elsif $M.reduced ~~ /\:(\w+)/ {
        return ~$0;
    } elsif $M.reduced eq 'PRE' {
        return self.get_op_sym($M<prefix>); # TODO: replace with better metaop
    } else {
        die "Cannot extract operator symbol ($M) ($M.reduced())";
    }
}

# XXX Niecza  Needs improvement
method FALLBACK($meth, $/) {
    if $meth eq '::($name)' { # XXX STD miscompilation
        my $p = $<O><prec>;
        if $p eq 't=' { # additive
            make Operator.funop('&infix:<' ~ self.get_op_sym($/) ~ '>', 2);
        } elsif $p eq 'y=' && $<semilist> {
            my $sym = $*GOAL eq '}' ?? '{ }' !! $*GOAL eq ']' ?? '[ ]' !!
                die "Unhandled postcircumfix ending in $*GOAL";
            make Operator.funop('&postcircumfix:<' ~ $sym ~ '>', 1, @( $<semilist>.ast ));
        } elsif $p eq 'y=' {
            make Operator.funop('&postfix:<' ~ self.get_op_sym($/) ~ '>', 1);
        } elsif $p eq 'v=' || $p eq 'o=' {
            make Operator.funop('&prefix:<' ~ self.get_op_sym($/) ~ '>', 1);
        }
    } elsif substr($meth,0,7) eq 'prefix:' {
        make Operator.funop('&prefix:<' ~ self.get_op_sym($/) ~ '>', 1);
    } elsif substr($meth,0,8) eq 'postfix:' {
        make Operator.funop('&postfix:<' ~ self.get_op_sym($/) ~ '>', 1);
    } elsif substr($meth,0,6) eq 'infix:' {
        make Operator.funop('&infix:<' ~ self.get_op_sym($/) ~ '>', 2);
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

method decints($/) { make [ map *.ast, @$<decint> ] }
method hexints($/) { make [ map *.ast, @$<hexint> ] }
method octints($/) { make [ map *.ast, @$<octint> ] }
method binints($/) { make [ map *.ast, @$<binint> ] }
method integer($/) {
    make ($<decint> // $<octint> // $<hexint> // $<binint>).ast
}

method escale ($/) { }
method dec_number ($/) {
    make +((~$/).comb(/<-[_]>/).join(""));
}

# XXX niecza rats will break this
method number($/) {
    my $child = $<integer> // $<dec_number> // $<rad_number>;
    make (defined($child) ?? $child.ast !!
        $child eq 'NaN' ?? ((1/0) / (1/0)) !! (1/0));
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

method transparent($/, $op, :$once = False, :$ltm, :$class = 'Sub',
    :$type = 'sub', :$sig = Sig.simple) {
    ::Op::SubDef.new(|node($/), var => self.gensym, :$once,
        body => Body.new(
            transparent => True,
            :$ltm, :$class, :$type, signature => $sig, do => $op));
}

method rxembed($/, $op, $trans) {
    ::Op::CallSub.new(|node($/),
        positionals => [ ::Op::MakeCursor.new(|node($/)) ],
        invocant => ::Op::SubDef.new(|node($/),
            var  => self.gensym,
            once => True,
            body => Body.new(
                transparent => $trans,
                type  => 'rxembedded',
                class => 'Sub',
                signature => Sig.simple('$¢'),
                do => $op)));
}

method op_for_regex($/, $rxop) {
    my @lift = $rxop.oplift;
    {
        my $*paren = 0;
        my $*dba = 'anonymous rule';
        $rxop.check
    }
    my ($orxop, $mb) = OptRxSimple.run($rxop);
    self.transparent($/, ::Op::RegexBody.new(|node($/), canback => $mb,
            pre => @lift, rxop => $orxop),
        class => 'Regex', type => 'regex', sig => Sig.simple.for_method);
}

method quote:sym</ /> ($/) { make self.op_for_regex($/, $<nibble>.ast) }

method encapsulate_regex($/, $rxop, :$goal, :$passcut = False,
        :$passcap = False) {
    my @lift = $rxop.oplift;
    my $lad = $rxop.lad;
    my ($nrxop, $mb) = OptRxSimple.run($rxop);
    # XXX do this in the signature so it won't be affected by transparent
    my @parm = ::Sig::Parameter.new(slot => 'self', name => 'self', readonly => True);
    if defined $goal {
        push @parm, ::Sig::Parameter.new(slot => '$*GOAL', name => '$*GOAL',
            readonly => True, positional => False, optional => True);
        unshift @lift, ::Op::Bind.new(|node($/), readonly => True,
            lhs => ::Op::Lexical.new(name => '$*GOAL'),
            rhs => ::Op::StringLiteral.new(text => $goal));
    }
    my $subop = self.transparent($/,
        ::Op::RegexBody.new(canback => $mb, pre => @lift, :$passcut, :$passcap,
            rxop => $nrxop), ltm => $lad, class => 'Regex', type => 'regex',
        sig => Sig.new(params => @parm));
    $subop = ::Op::CallSub.new(|node($/), invocant => $subop,
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

method regex_def($/) {
    sub _symtext($name) {
        ($name ~~ /\:sym\<(.*)\>/) ?? ~$0 !!
            ($name ~~ /\:(\w+)/) ?? ~$0 !!
            Str; #XXX
    }
    my ($name, $path) = $<deflongname> ??
        self.mangle_longname($<deflongname>[0]).<name path> !! Nil;
    my $cname;
    if defined($path) && $path == 0 && $name.^isa(Op) {
        $cname = $name;
        $name = ~$<deflongname>[0];
        $path = Any;
    }

    my $scope = (!defined($name)) ?? "anon" !! ($*SCOPE || "has");

    if $<signature> > 1 {
        $/.CURSOR.sorry("Multiple signatures on a regex NYI");
        return Nil;
    }

    if $cname && $scope ne 'has' {
        $/.CURSOR.sorry("Only has regexes may have computed names");
        make ::Op::StatementList.new;
        return Nil;
    }

    my $isproto;
    my $symtext = ($cname || !defined($name)) ?? Str !! _symtext($name);
    if $*MULTINESS eq 'proto' {
        if $<signature> || !$<regex_block><onlystar> || $scope ne 'has' {
            $/.CURSOR.sorry("Only simple {*} protoregexes with no parameters are supported");
            return Nil;
        }
        $isproto = True;
    } else {
        my $m2 = defined($symtext) ?? 'multi' !! 'only';
        if $*MULTINESS && $*MULTINESS ne $m2 {
            $/.CURSOR.sorry("Inferred multiness disagrees with explicit");
            return Nil;
        }
    }

    if defined($path) && $scope ne 'our' {
        $/.CURSOR.sorry("Putting a regex in a package requires using the our scope.");
        return Nil;
    }

    my $sig = $<signature> ?? $<signature>[0].ast !! Sig.simple;

    if $scope eq 'state' || $scope eq 'supercede' || $scope eq 'augment' {
        $/.CURSOR.sorry("Nonsensical scope $scope for regex");
        return Nil;
    }

    if $scope eq 'our' {
        $/.CURSOR.sorry("our regexes NYI");
        return Nil;
    }

    my $var = ($scope eq 'anon' || $scope eq 'has') ?? self.gensym
        !! '&' ~ $name;

    my $ast = $<regex_block>.ast;
    if $isproto {
        $ast = ::RxOp::ProtoRedis.new(name => $name);
    }

    {
        my $*paren = 0;
        my $*symtext = $symtext;
        my $*dba = $name // 'anonymous regex';
        $ast.check;
    }
    my $lad = OptRxSimple.run_lad($ast.lad);
    my @lift = $ast.oplift;
    ($ast, my $mb) = OptRxSimple.run($ast);
    make ::Op::SubDef.new(|node($/),
        var  => $var,
        method_too => ($scope eq 'has' ?? ['normal', $cname // $name] !! Any),
        body => Body.new(
            ltm   => $lad,
            returnable => True,
            class => 'Regex',
            type  => 'regex',
            name  => $name // 'ANONrx',
            signature => $sig.for_method,
            do => ::Op::RegexBody.new(|node($/), pre => @lift,
                name => ($name // ''), rxop => $ast, canback => $mb)));
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
    my $q    = $<quantifier> ?? $<quantifier>[0].ast !! Any;

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
    my $h =
        $1 ?? { min => +~$0, max => +~$1[0] } !!
        ($0 && defined($/.index('..'))) ?? { min => +~$0 } !!
        $0 ?? { min => +~$0, max => +~$0 } !!
        $<embeddedblock> ?? { min => 0, cond => $<embeddedblock>.ast } !!
        { min => 1, sep => $<quantified_atom>.ast };
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
    my $inv = $<embeddedblock>.ast.invocant;
    $inv.body.type = 'rxembedded';
    $inv.body.signature = Sig.simple('$¢');
    $inv.once = True;
    $inv = ::Op::CallSub.new(|node($/), invocant => $inv, positionals => [ ::Op::MakeCursor.new(|node($/)) ]);
    make ::RxOp::VoidBlock.new(block => $inv);
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
    my @words = $cif.^isa(::Op::SimpleParcel) ?? @( $cif.items ) !! $cif;
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
            receiver => ::Op::Lexical.new(name => '$¢'),
            name => $name,
            args => $args);

        my $regex = self.rxembed($/, $callop, True);

        make ::RxOp::Subrule.new(regex => $regex);
    }
    make self.rxcapturize($/, $name, $/.ast);
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
    my $inv = $<embeddedblock>.ast.invocant;
    $inv.body.type = 'rxembedded';
    $inv.body.signature = Sig.simple('$¢');
    $inv.once = True;
    $inv = ::Op::CallSub.new(|node($/), invocant => $inv, positionals => [ ::Op::MakeCursor.new(|node($/)) ]);
    make ::RxOp::CheckBlock.new(block => $inv);
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

    if !$v.^isa(List) {
        $/.CURSOR.sorry(":$k requires an expression argument");
        make ::RxOp::None.new;
        return Nil;
    }
    $v = $v[0].ast;

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
method escape:sym<{ }> ($/) { make $<embeddedblock>.ast }
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

        if $ast !~~ Op && defined $prefix {
            $ast = $ast.split(/^^<before \s>[ $prefix || \s* ]/).join("");
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

            make ((@tok == 1) ?? @tok[0] !!
                ::Op::SimpleParcel.new(|node($/), items => @tok));
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
    my $do = $<pblock>.ast.do;

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
    $<pblock>.ast.type = 'bare';
    make ::Op::BareBlock.new(|node($/), var => self.gensym,
        body => $<pblock>.ast);

    if self.check_hash($/) {
        make mkcall($/, '&_hash_constructor',
            ::Op::CallSub.new(|node($/), invocant =>
                    self.block_to_closure($/, $<pblock>.ast, once => True)));
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
            make ::Op::StatementList.new(|node($/), children => [
                ::Op::Start.new(condvar => $cv, body => $/.ast),
                ::Op::Lexical.new(name => $lhs.name)]);
        }
        elsif $lhs.^isa(::Op::Attribute) && !$lhs.initializer {
            $lhs.initializer = self.sl_to_block('bare', $rhs,
                subname => $lhs.name ~ " init");
            make $lhs;
        }
        elsif $lhs.^isa(::Op::ConstantDecl) && !$lhs.init {
            my $sig = substr($lhs.name, 0, 1);
            if defined '$@&%'.index($sig) {
                $lhs.init = self.docontext($/, $sig, $rhs)
            } else {
                $lhs.init = $rhs;
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
    if !$<dottyop>.ast.^isa(::Operator::Method) {
        $/.CURSOR.sorry("Modified method calls can only be used with actual methods");
        make Operator.funop('&postfix:<++>', 1);
        return Nil;
    }
    if $<sym> eq '.^' {
        make $<dottyop>.ast.clone(:meta);
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
            $tv = ::Op::CallMethod.new(name => 'at-key',
                receiver => ::Op::ContextVar.new(name => '$*/'),
                args => [::Op::StringLiteral.new(text => ~$<k>)]);
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
            $a = $a.ops;
        }
    }
    $( @vars ), @args_;
}

method whatever_postcheck($/, $st, $term) {
    if @$st {
        ::Op::WhateverCode.new(ops => $term, vars => $st,
            slot => self.gensym, |node($/));
    } else {
        $term;
    }
}

# term :: Op
method term:value ($/) { make $<value>.ast }

method term:name ($/) {
    my ($id, $path) = self.mangle_longname($<longname>).<name path>;

    if $<args> {
        $/.CURSOR.sorry("Unsupported form of term:name");
        make ::Op::StatementList.new;
        return Nil;
    }

    if defined $path {
        make ::Op::PackageVar.new(|node($/), name => $id,
            slot => self.gensym, path => $path);
    } else {
        make ::Op::Lexical.new(|node($/), name => $id);
    }

    if $<postcircumfix> {
        make mkcall($/, '&_param_role_inst', $/.ast,
            @( $<postcircumfix>[0].ast.args ));
    }
}

method term:identifier ($/) {
    my $id  = $<identifier>.ast;
    my $sal = $<args> ?? ($<args>.ast // []) !! [];
    # TODO: support zero-D slicels

    if $sal > 1 {
        $/.CURSOR.sorry("Slicel lists are NYI");
        make ::Op::StatementList.new;
        return Nil;
    }

    if $/.CURSOR.is_name(~$<identifier>) {
        make ::Op::Lexical.new(|node($/), name => $id);
        return Nil;
    }

    my $args = $sal[0] // [];

    make ::Op::CallSub.new(|node($/),
        invocant => ::Op::Lexical.new(name => '&' ~ $id),
        args => $args);
}

method term:sym<self> ($/) { make ::Op::Lexical.new(|node($/), name => 'self') }
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
    $<pblock>.ast.type = 'pointy';
    make self.block_to_closure($/, $<pblock>.ast);
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

    if defined($v<rest>) && $tw ~~ /<[*=~?^:]>/ {
        $M.CURSOR.sorry("Twigil $tw cannot be used with qualified names");
        return ::Op::StatementList.new;
    }

    if $tw eq '!' {
        self.docontext($M, $v<sigil>, ::Op::CallMethod.new(|node($M),
            name => $v<name>, private => True, receiver => mklex($M, 'self'),
            ppath => $v<rest>));
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
        ::Op::Lexical.new(|node($M), name => $v<sigil> ~ $v<name>);
    }
    elsif $tw eq '*' {
        ::Op::ContextVar.new(|node($M), name => $sl);
    }
    elsif $tw eq '' || $tw eq '?' {
        if defined($v<rest>) {
            ::Op::PackageVar.new(path => $v<rest>, name => $sl,
                slot => self.gensym, |node($M));
        } else {
            ::Op::Lexical.new(|node($M), name => $sl);
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
        return Nil;
    } elsif defined $dsosl {
        ($name, $rest) = $dsosl<name path>;
    } elsif $<name> {
        # Both these cases are marked XXX in STD.  I agree.  What are they for?
        if $<name>[0].ast<dc> {
            $/.CURSOR.sorry("*ONE* pair of leading colons SHALL BE ENOUGH");
            make { term => ::Op::StatementList.new };
            return Nil;
        }
        if substr(~$/,0,3) eq '$::' {
            $rest = $<name>[0].ast.<names>;
            $name = pop $rest;
        } else {
            if $<name>[0].ast<names> > 1 {
                $/.CURSOR.sorry("Nonsensical attempt to qualify a self-declared named parameter detected");
                make { term => ::Op::StatementList.new };
                return Nil;
            }
            $name = $<name>[0].ast<names>[0];
            $twigil = ':';
        }
    } elsif $<special_variable> {
        $name = substr(~$<special_variable>, 1);
        $twigil = '*' if $name eq '/' or $name eq '!';
    } elsif $<index> {
        make { capid => $<index>.ast, term =>
            ::Op::CallMethod.new(|node($/), name => 'at-pos',
                receiver => ::Op::ContextVar.new(name => '$*/'),
                positionals => [ ::Op::Num.new(value => $<index>.ast) ])
        };
        return Nil;
    } elsif $<postcircumfix> {
        if $<postcircumfix>[0].reduced eq 'postcircumfix:sym<< >>' { #XXX fiddly
            make { capid => $<postcircumfix>[0].ast.args[0].text, term =>
                ::Op::CallMethod.new(|node($/), name => 'at-key',
                    receiver    => ::Op::ContextVar.new(name => '$*/'),
                    positionals => $<postcircumfix>[0].ast.args)
            };
            return Nil;
        } else {
            $/.CURSOR.sorry("Contextualizer variables NYI");
            make { term => ::Op::StatementList.new };
            return Nil;
        }
    } else {
        $/.CURSOR.sorry("Non-simple variables NYI");
        make { term => ::Op::StatementList.new };
        return Nil;
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
    sub good($a, $b is rw) { $a ~~ /^<[@$%]><[.*!]>?(.*)/ && ($b = [~$0]; True) }
    if $<name> {
        if $<named_param> {
            %rt = %( $<named_param>.ast );
        } else {
            %rt = %( $<param_var>.ast );
        }
        %rt<names> = [ @( %rt<names> // [] ), ~$<name> ];
    } else {
        %rt = %( $<param_var>.ast );
        if %rt<slot> && good(%rt<slot>, %rt<names>) {
        } else {
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
    make { list => ($sigil eq '@'), hash => ($sigil eq '%'), :$slot };
}

# :: Sig::Parameter
method parameter($/) {
    my $rw = False;
    my $sorry;
    my $slurpy;
    my $slurpycap;
    my $optional;
    my $rwt;

    for @( $<trait> ) -> $trait {
        if $trait.ast<rw> { $rw = True }
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

    make ::Sig::Parameter.new(name => ~$/, :$default,
        :$optional, :$slurpy, readonly => !$rw,
        :$slurpycap, rwtrans => $rwt, |$p.ast);
}

# signatures exist in several syntactic contexts so just make an object for now
method signature($/) {
    if $<type_constraint> {
        $/.CURSOR.sorry("Return type constraints NYI");
        return Nil;
    }

    if $<param_var> {
        make Sig.new(params => [ ::Sig::Parameter.new(
                name => ~$<param_var>, |$<param_var>.ast,
                full_parcel => True) ]);
        return Nil;
    }

    my $exp = 0;
    for @( $<param_sep> ) -> $sep {
        if defined $sep.index(':') {
            $exp = 1;
        } elsif !defined $sep.index(',') {
            $/.CURSOR.sorry("Parameter separator $sep NYI");
            return Nil;
        }
    }

    make Sig.new(explicit_inv => ?$exp, params =>
        [map *.ast, @( $<parameter> )]);
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
    $repl = self.transparent($/, $repl);
    make mkcall($/, '&_substitute', mklex($/, '$_'), $regex, $repl);
}
method tribble($/) {}
method babble($/) {}
method quotepair($/) {}

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
        make ::Op::YouAreHere.new(|node($/), unitname => $*UNITNAME);
    } else {
        make $<statementlist>.ast;
    }
}
method lambda($/) {}
method embeddedblock($/) {
    make self.block_to_immediate($/, 'bare',
        self.sl_to_block('bare', $<statementlist>.ast,
            signature => Sig.simple()));
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
            $_ = ::Op::Lexical.new(|node($/), name => $_.slot, list => $_.list,
                hash => $_.hash, declaring => True);
        }
        make ::Op::SimpleParcel.new(|node($/), items => @p);
        return Nil;
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

method multi_declarator:multi ($/) { make ($<declarator> // $<routine_def>).ast}
method multi_declarator:proto ($/) { make ($<declarator> // $<routine_def>).ast}
method multi_declarator:only  ($/) { make ($<declarator> // $<routine_def>).ast}

method variable_declarator($/) {
    if $*MULTINESS {
        $/.CURSOR.sorry("Multi variables NYI");
    }
    if $<trait> || $<post_constraint> || $<postcircumfix> || $<semilist> {
        $/.CURSOR.sorry("Traits, postconstraints, and shapes on variable declarators NYI");
    }

    my $scope = $*SCOPE // 'my';

    if $scope eq 'augment' || $scope eq 'supersede' {
        $/.CURSOR.sorry("Illogical scope $scope for simple variable");
    }

    my $v = $<variable>.ast;
    my $t = $v<twigil>;
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

    if $scope eq 'has' {
        make ::Op::Attribute.new(|node($/), name => $v<name>,
            accessor => $t eq '.');
    } elsif $scope eq 'state' {
        make ::Op::Lexical.new(|node($/), name => $slot, state_decl => True,
            state_backing => self.gensym, declaring => True,
            list => $v<sigil> eq '@', hash => $v<sigil> eq '%');
    } elsif $scope eq 'our' {
        make ::Op::PackageVar.new(|node($/), name => $slot, slot => $slot,
            path => [ 'OUR' ]);
    } else {
        make ::Op::Lexical.new(|node($/), name => $slot, declaring => True,
            list => $v<sigil> eq '@', hash => $v<sigil> eq '%');
    }
}

method type_declarator:constant ($/) {
    if $*MULTINESS {
        $/.CURSOR.sorry("Multi variables NYI");
    }
    my $scope = $*SCOPE // 'my';
    if !$<identifier> && !$<variable> {
        $/.CURSOR.sorry("Anonymous constants NYI"); #wtf?
        return Nil;
    }
    my $slot  = ~($<identifier> // $<variable>);

    make ::Op::ConstantDecl.new(|node($/), name => $slot,
        path => ($scope eq 'our' ?? [ 'OUR' ] !! Array));
}

method package_declarator:class ($/) { make $<package_def>.ast }
method package_declarator:grammar ($/) { make $<package_def>.ast }
method package_declarator:role ($/) { make $<package_def>.ast }
method package_declarator:slang ($/) { make $<package_def>.ast }
method package_declarator:module ($/) { make $<package_def>.ast }
method package_declarator:package ($/) { make $<package_def>.ast }
method package_declarator:knowhow ($/) { make $<package_def>.ast }

method package_declarator:sym<also> ($/) {
    make ::Op::StatementList.new(|node($/), children =>
        self.process_package_traits($/, Any, $<trait>));
}

method package_declarator:require ($/) {
    if $<EXPR> {
        $/.CURSOR.sorry('Expressional forms of require NYI');
        make ::Op::StatementList.new;
        return Nil;
    }
    make ::Op::Require.new(|node($/), unit => ~$<module_name>);
}

method process_package_traits($/, $export, @tr) {
    my @r;

    for @tr -> $trait {
        if $trait.ast.<name>:exists {
            push @r, ::Op::Super.new(|node($/), name => $trait.ast.<name>,
                path => $trait.ast.<path>);
        } elsif $trait.ast.<export> {
            if defined $export {
                push $export, @( $trait.ast.<export> );
            } else {
                $/.CURSOR.sorry('Cannot mark a class as exported outside the declarator');
            }
        } else {
            $/.CURSOR.sorry("Non-superclass traits for packageoids NYI");
        }
    }

    @r;
}

# normally termish's ast is not used, but it becomes the used ast under
# nulltermish.
method termish($/) { make $<term>.ast }
method nulltermish($/) {}
method EXPR($/) { make $<root>.ast }
method modifier_expr($/) { make $<EXPR>.ast }
method default_value($/) {
    make Body.new(transparent => True, do => $<EXPR>.ast);
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
        make ::Op::Labelled.new(|node($/), name => ~$<label><identifier>,
            stmt => $<statement>.ast);
        return Nil;
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
        my $true_block = self.block_to_closure($pb, $pb.ast, once => True);
        ::Op::CallSub.new(|node($/), invocant => $true_block,
            positionals => [$cond]);
    } else {
        self.block_to_immediate($/, 'cond', $pb.ast);
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
    make ::Op::Conditional.new(|node($/), check => $<xblock>.ast[0],
        false => self.block_to_immediate($/, 'cond', $<xblock>.ast[1]));
}

method statement_control:while ($/) {
    make ::Op::WhileLoop.new(|node($/), check => $<xblock>.ast[0],
        body => self.block_to_immediate($/, 'loop', $<xblock>.ast[1]),
        :!until, :!once);
}

method statement_control:until ($/) {
    make ::Op::WhileLoop.new(|node($/), check => $<xblock>.ast[0],
        body => self.block_to_immediate($/, 'loop', $<xblock>.ast[1]),
        :until, :!once);
}

method statement_control:repeat ($/) {
    my $until = $<wu> eq 'until';
    my $check = $<xblock> ?? $<xblock>.ast[0] !! $<EXPR>.ast;
    my $body  = self.block_to_immediate($/, 'loop',
        $<xblock> ?? $<xblock>.ast[1] !! $<pblock>.ast);
    make ::Op::WhileLoop.new(|node($/), :$check, :$until, :$body, :once);
}

method statement_control:loop ($/) {
    my $body = self.block_to_immediate($/, 'loop', $<block>.ast);
    # XXX wrong interpretation
    my $init = $0 && $0[0]<e1>[0] ?? $0[0]<e1>[0].ast !! Any;
    my $cond = $0 && $0[0]<e2>[0] ?? $0[0]<e2>[0].ast !! Any;
    my $step = $0 && $0[0]<e3>[0] ?? $0[0]<e3>[0].ast !! Any;

    make ::Op::GeneralLoop.new(|node($/), :$body, :$init, :$cond, :$step);
}

method statement_control:for ($/) {
    $<xblock>.ast[1].type = 'loop';
    make ::Op::ForLoop.new(|node($/), source => $<xblock>.ast[0],
        sink => self.block_to_closure($/, $<xblock>.ast[1]));
}

method statement_control:given ($/) {
    $<xblock>.ast[1].type = 'immed';
    make ::Op::CallSub.new(|node($/), positionals => [ $<xblock>.ast[0] ],
        invocant => self.block_to_closure($/, $<xblock>.ast[1], :once));
}

method statement_control:when ($/) {
    $<xblock>.ast[1].type = 'cond';
    make ::Op::When.new(|node($/), match => $<xblock>.ast[0],
        body => self.block_to_immediate($/, 'loop', $<xblock>.ast[1]));
}

method statement_control:use ($/) {
    make ::Op::StatementList.new;
    if $<version> {
        return Nil;
    }

    my $name = $<module_name>.ast<name>;
    my $args = $<arglist> ?? $<arglist>.ast !! [];

    if defined $<module_name>.ast.<args> {
        $/.CURSOR.sorry("'use' of an instantiated role not yet understood");
        return Nil;
    }

    if $args {
        $/.CURSOR.sorry("'use' with arguments NYI");
        return Nil;
    }

    if ($name eq 'MONKEY_TYPING' || $name eq 'fatal' || $name eq 'lib') {
        return Nil;
    }

    make ::Op::Use.new(|node($/), unit => $name);
}

my %_decl2class = (
    package => ::Op::PackageDef,
    class   => ::Op::ClassDef,
    module  => ::Op::ModuleDef,
    grammar => ::Op::GrammarDef,
    role    => ::Op::RoleDef,
);

method package_def ($/) {
    make ::Op::StatementList.new;
    if $*MULTINESS {
        $/.CURSOR.sorry("Multi variables NYI");
        return Nil;
    }
    my $scope = $*SCOPE;
    if !$<longname> {
        $scope = 'anon';
    }
    if $scope eq 'supersede' {
        $/.CURSOR.sorry('Supercede is not yet supported');
        return Nil;
    }
    if $scope eq 'has' || $scope eq 'state' {
        $/.CURSOR.sorry("Illogical scope $scope for package block");
        return Nil;
    }

    my ($name, $outervar, @augpkg);

    if $scope eq 'augment' {
        my $r = self.mangle_longname($<longname>[0]);
        $name = $r<name>;
        @augpkg = @( $r<path> // ['MY'] );
    } else {
        $name = $<longname> ??
            self.unqual_longname($<longname>[0],
                "Qualified package definitions NYI", True) !! 'ANON';
        $outervar = $scope ne 'anon' ?? $name !! self.gensym;
    }

    my $optype = %_decl2class{$*PKGDECL};
    my $blocktype = $*PKGDECL;
    my $bodyvar = self.gensym;
    # currently always install into the local stash
    my $ourpkg = ($scope eq 'our') ?? [ 'OUR::' ] !! Any;

    if $scope eq 'augment' {
        my $stmts = $<statementlist> // $<blockoid>;
        $stmts = $stmts.ast;
        my $cbody = self.sl_to_block($blocktype, $stmts, subname => "augment-" ~ ($name // 'ANON'));

        make ::Op::Augment.new(
            |node($/),
            pkg     => [@augpkg],
            name    => $name,
            bodyvar => $bodyvar,
            body    => $cbody);
    } elsif !$*DECLARAND<stub> {
        my $stmts = $<statementlist> // $<blockoid>;
        my @export;

        $stmts = ::Op::StatementList.new(children =>
            [ self.process_package_traits($/, @export, $<trait>), $stmts.ast ]);

        my $cbody = self.sl_to_block($blocktype, $stmts,
            subname => ($*PKGDECL ~ '-' ~ ($name // 'ANON')));
        make $optype.new(
            |node($/),
            signature => ($blocktype eq 'role' && $<signature> ??
                $<signature>[0].ast !! Any),
            name    => $name,
            var     => $outervar,
            exports => @export,
            bodyvar => $bodyvar,
            ourpkg  => $ourpkg,
            body    => $cbody);
    } else {
        make $optype.new(
            |node($/),
            name    => $name,
            var     => $outervar,
            ourpkg  => $ourpkg,
            stub    => True);
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
    } elsif ($trait eq 'rawcall') {
        make { nobinder => True };
    } elsif $trait eq 'return-pass' { # &return special
        make { return_pass => 1 };
    } elsif $trait eq 'rw' {
        make { rw => 1 };
    } elsif $trait eq 'parcel' {
        make { rwt => 1 };
    } elsif $trait eq 'readonly' {
        make { readonly => 1 };
    } else {
        $/.CURSOR.sorry("Unhandled trait $trait");
        make { };
    }

    if $noparm && $<circumfix> {
        $/.CURSOR.sorry($noparm);
    }
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
method routine_declarator:submethod ($/) {
    make $<method_def>.ast;
    if $/.ast.method_too.[0] ne 'normal' {
        $/.CURSOR.sorry("Call pattern decorators cannot be used with submethod");
    }
    $/.ast.method_too.[0] = 'sub';
}

my $next_anon_id = 0;
method gensym() { 'anon_' ~ ($next_anon_id++) }
method genid()  { ($next_anon_id++) }

method sl_to_block ($type, $ast, :$subname, :$returnable, :$signature) {
    Body.new(
        name      => $subname // 'ANON',
        returnable=> $returnable // ($type eq 'sub'),
        type      => $type,
        signature => $signature,
        do        => $ast);
}

method block_to_immediate($/, $type, $blk) {
    $blk.type = $type;
    ::Op::CallSub.new(|node($/),
        invocant => self.block_to_closure($/, $blk, once => True),
        positionals => []);
}

method block_to_closure($/, $body, :$outer_key, :$method_too, :$once,
        :$exports) {
    ::Op::SubDef.new(|node($/), var => ($outer_key // self.gensym),
        :$body, :$once, :$method_too, exports => ($exports // []));
}

method get_placeholder_sig($/) {
    # for some reason, STD wants to deparse this
    my @things = $*CURLEX<$?SIGNATURE>.split(", ");
    shift @things if @things[0] eq '';
    my @parms;
    for @things -> $t {
        if substr($t, 0, 9) eq '$_ is ref' {
            push @parms, ::Sig::Parameter.new(optional => True,
                slot => '$_', name => '$_');
        } elsif $t eq '*@_' {
            push @parms, ::Sig::Parameter.new(slurpy => True, slot => '@_',
                list => True, name => '*@_');
        } elsif defined '$@%&'.index(substr($t,0,1)) {
            push @parms, ::Sig::Parameter.new(slot => $t, name => $t,
                list => (substr($t,0,1) eq '@'), hash => (substr($t,0,1) eq '%'));
        } else {
            $/.CURSOR.sorry('Named placeholder parameters NYI');
        }
    }
    return Sig.new(params => @parms);
}

# always a sub, though sometimes it's an implied sub after multi/proto/only
method routine_def ($/) {
    make ::Op::StatementList.new;
    if $*MULTINESS {
        $/.CURSOR.sorry("Multi routines NYI");
    }
    if $<sigil> && $<sigil>[0] eq '&*' {
        $/.CURSOR.sorry("Contextual sub definitions NYI");
        return Nil;
    }
    my $dln = $<deflongname>[0];
    if $<multisig> > 1 {
        $/.CURSOR.sorry("Multiple multisigs (what?) NYI");
        return Nil;
    }
    my @export;
    my $return_pass = 0;
    my $signature = $<multisig> ?? $<multisig>[0].ast !!
        self.get_placeholder_sig($/);
    for @( $<trait> ) -> $t {
        if $t.ast.<export> {
            push @export, @( $t.ast<export> );
        } elsif $t.ast<nobinder> {
            $signature = Any;
        } elsif $t.ast<return_pass> {
            $return_pass = 1;
        } else {
            $/.CURSOR.sorry('Non-export sub traits NYI');
        }
    }
    my $scope = !$dln ?? 'anon' !! ($*SCOPE || 'my');
    my ($m,$p) = $dln ?? self.mangle_longname($dln).<name path> !! ();

    if $scope ne 'my' && $scope ne 'our' && $scope ne 'anon' {
        $/.CURSOR.sorry("Illegal scope $scope for subroutine");
        return Nil;
    }
    if $scope eq 'our' {
        $/.CURSOR.sorry('Package subs NYI');
        return Nil;
    } elsif $p {
        $/.CURSOR.sorry('Defining a non-our sub with a package-qualified name makes no sense');
        return Nil;
    }

    make self.block_to_closure($/,
        self.sl_to_block('sub',
            $<blockoid>.ast,
            returnable => !$return_pass,
            subname => $m,
            signature => $signature),
        outer_key => (($scope eq 'my') ?? "\&$m" !! Any),
        exports => @export);
}

method method_def ($/) {
    make ::Op::StatementList.new;
    my $scope = $*SCOPE // 'has';
    my $type = $<type> ?? ~$<type> !! '';
    $type = ($type eq ''  ?? 'normal' !!
             $type eq '^' ?? 'meta' !!
             $type eq '!' ?? 'private' !!
             (
                 $/.CURSOR.sorry("Unhandled method decoration $type");
                 return Nil;
             ));
    $scope = 'anon' if !$<longname>;
    my $name = $<longname> ?? self.unqual_longname($<longname>,
        "Qualified method definitions not understood") !! Any; #XXX

    if $<sigil> {
        $/.CURSOR.sorry("Method sgils NYI");
        return Nil;
    }
    if $type eq 'meta' {
        $/.CURSOR.sorry("Metamethod mixins NYI");
        return Nil;
    }
    if $<multisig> > 1 {
        $/.CURSOR.sorry("Multiple multisigs (what?) NYI");
        return Nil;
    }

    my $sym = ($scope eq 'my') ?? ('&' ~ $name) !! self.gensym;

    if ($scope eq 'augment' || $scope eq 'supersede' || $scope eq 'state') {
        $/.CURSOR.sorry("Illogical scope $scope for method");
        return Nil;
    }

    if ($scope eq 'our') {
        $/.CURSOR.sorry("Packages NYI");
        return Nil;
    }
    my $sig = $<multisig> ?? $<multisig>[0].ast !!
        self.get_placeholder_sig($/);

    for @( $<trait> ) -> $t {
        if ($t.ast<nobinder>) {
            $sig = Any;
        } else {
            $/.CURSOR.sorry("NYI method trait $t");
        }
    }

    my $bl = self.sl_to_block('sub', $<blockoid>.ast,
        subname => $name,
        signature => $sig ?? $sig.for_method !! Any);

    make self.block_to_closure($/, $bl, outer_key => $sym,
        method_too => ($scope ne 'anon' ?? [ $type, $name ] !! Any));
}

method block($/) { make self.sl_to_block('', $<blockoid>.ast); }

# :: Body
method pblock($/) {
    my $rw = $<lambda> && $<lambda> eq '<->';
    make self.sl_to_block('', $<blockoid>.ast,
        signature => ($<signature> ?? $<signature>.ast !!
            self.get_placeholder_sig($/)));
}

method xblock($/) { make [ $<EXPR>.ast, $<pblock>.ast ] }

# returns Body of 0 args
method blast($/) {
    if $<block> {
        make $<block>.ast;
    } else {
        make Body.new(
            transparent => True,
            do   => $<statement>.ast);
    }
}

method statement_prefix:do ($/) {
    make self.block_to_immediate($/, 'do', $<blast>.ast);
}
method statement_prefix:gather ($/) {
    $<blast>.ast.type = 'gather';
    make ::Op::Gather.new(|node($/), var => self.gensym, body => $<blast>.ast);
}
method statement_prefix:try ($/) {
    make ::Op::Try.new(|node($/), body =>
        self.block_to_immediate($/, 'try', $<blast>.ast));
}

method statement_prefix:START ($/) {
    my $cv = self.gensym;
    make ::Op::Start.new(|node($/), condvar => $cv, body =>
        self.block_to_immediate($/, 'phaser', $<blast>.ast));
}

# TODO: retain and return a value
method statement_prefix:INIT ($/) {
    $<blast>.ast.type = 'init';
    make ::Op::VoidPhaser.new(|node($/), body => $<blast>.ast);
}
# XXX 'As soon as possible' isn't quite soon enough here
method statement_prefix:BEGIN ($/) {
    $<blast>.ast.type = 'begin';
    make ::Op::VoidPhaser.new(|node($/), body => $<blast>.ast);
}
method statement_prefix:CHECK ($/) {
    $<blast>.ast.type = 'begin';
    make ::Op::VoidPhaser.new(|node($/), body => $<blast>.ast);
}

method statement_prefix:END ($/) {
    $<blast>.ast.type = 'end';
    make ::Op::VoidPhaser.new(|node($/), body => $<blast>.ast);
}

method comp_unit($/) {
    my $body;
    my $sl = $<statementlist>.ast;

    $body = self.sl_to_block('mainline', $sl, subname => 'mainline');

    make Unit.new(mainline => $body, name => $*UNITNAME,
        is_setting => ?$*YOU_WERE_HERE, setting_name => $*SETTINGNAME,
        orig => $/.orig, filename => $*FILE<name>, modtime => $*modtime);
}
