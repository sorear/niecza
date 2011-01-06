# a07a966de52560ebbeea470bc29373a08182a869

class NieczaActions;

use Op;
use OpHelpers;
use RxOp;
use Body;
use Unit;
use Sig;
use CClass;
use OptRxSimple;

sub ord($x) { Q:CgOp { (rawscall Builtins,Kernel.Ord {$x}) } }
sub chr($x) { Q:CgOp { (rawscall Builtins,Kernel.Chr {$x}) } }

# XXX Niecza  Needs improvement
method FALLBACK($meth, $/) {
    $/.cursor.sorry("Action method $meth not yet implemented");
}

sub node($M) { file => $*FILE<name>, line => $M.cursor.lineof($M.to) }

method ws($ ) { }
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
method unqual_longname($/, $what, $clean) {
    my $h = self.mangle_longname($/, $clean);
    if $h<path> {
        $/.CURSOR.sorry($what);
        return "";
    }
    return $h<name>;
}

method mangle_longname($/, $clean) {
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

# quote :: Op
method quote:sym<" "> ($/) { make $<nibble>.ast }
method quote:sym<' '> ($/) { make $<nibble>.ast }
method quote:qq ($/) { make $<quibble>.ast }
method quote:q ($/) { make $<quibble>.ast }
method quote:Q ($/) { make $<quibble>.ast }

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
    my ($orxop, $mb) = ::Optimizer::RxSimple.run($rxop);
    self.transparent($/, ::Op::RegexBody.new(|node($/), canback => $mb,
            pre => @lift, rxop => $orxop),
        class => 'Regex', type => 'regex', sig => Sig.simple.for_method);
}

method quote:sym</ /> ($/) { make self.op_for_regex($/, $<nibble>.ast) }

method encapsulate_regex($/, $rxop, :$goal, :$passcut, :$passcap) {
    my @lift = $rxop.oplift;
    my ($nrxop, $mb) = ::Optimizer::RxSimple.run($rxop);
    my $lad = $rxop.lad;
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
    if $<quotepair> {
        $/.CURSOR.sorry('Regex adverbs NYI');
    }
    make $<nibble>.ast;
}

method regex_def($/) {
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
    my $symtext =
        ($cname || !defined($name)) ?? Str !!
        ($name ~~ /\:sym\<(.*)\>/) ?? ~$0 !!
        ($name ~~ /\:(\w+)/) ?? ~$0 !!
        Str; #XXX
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

    if $scope ~~ /< state augment supersede >/ {
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
    my $lad = ::Optimizer::RxSimple.run_lad($ast.lad);
    my @lift = $ast.oplift;
    ($ast, my $mb) = ::Optimizer::RxSimple.run($ast);
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
method quantifier:sym<?> ($/) { make { min => 0, max => 1, mod => $<quantmod>.ast } }
method quantifier:sym<:> ($/) { make { mod => '' } }
method quantifier:sym<~> ($/) {
    make { tilde => [ map *.ast, @($<quantified_atom>) ] }
}
method quantifier:sym<**> ($/) {
    # XXX can't handle normspace well since it's not labelled 1*/2*
    my $h =
        $1 ?? { min => +~$0[0], max => +~$1[0] } !!
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

sub LISTrx($/) {
    make %LISTrx_types{$<delims>[0]<sym>}.new(zyg =>
        [ map *.ast, @( $<list> ) ], dba => %*RX<dba>);
}

method regex_infix:sym<|> {}
method regex_infix:sym<||> {}
method regex_infix:sym<&> {}
method regex_infix:sym<&&> {}

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
        if grep { !defined $_ } @l {
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

    if !$v.^isa(Match) {
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
        if !$v.isa(::Op::StringLiteral) {
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

method nibbler($/) {
    if $/.CURSOR.^isa(::STD::Regex) {
        make $<EXPR>.ast;
    } elsif $/.CURSOR.isa(::NieczaGrammar::CgOp) {
        if $*SAFEMODE {
            $/.CURSOR.sorry('Q:CgOp not allowed in safe mode');
            make ::Op::StatementList.new;
            return Nil;
        }
        make ::Op::CgOp.new(|node($/), optree => $<cgexp>.ast);
    } elsif ($M->can('ccstate')) { #XXX XXX try to catch cclasses
        my @nib = @{ $M->{nibbles} };
        my @bits = map { $_->{_ast} } @nib;

        for (my $i = 0; $i < @bits; $i++) {
            my $t = ref($bits[$i]);
            if (!$t) {
                if (length($bits[$i]) > 1) {
                    $nib[$i]->sorry("Cannot use a string in a character class");
                    $bits[$i] = "";
                }
            } elsif ($t eq 'SCALAR') {
                # .. hack
            } elsif ($t eq 'CClass') {
                # also ok
            } else {
                $nib[$i]->sorry("Cannot use an interpolation in a character class");
                $bits[$i] = "";
            }

            if ($bits[$i] eq "") {
                splice @bits, $i, 1;
                splice @nib, $i, 1;
                $i--;
            }
        }

        for (my $i = 0; $i < @bits; $i++) {
            next unless ref($bits[$i]) && ref($bits[$i]) eq 'SCALAR';

            for ($i-1, $i + 1) {
                if (ref($bits[$_])) {
                    $nib[$_]->sorry("Bad range endpoint");
                    $bits[$_] = "\0";
                }
            }

            splice @bits, $i-1, 3, CClass->range($bits[$i-1], $bits[$i+1]);
            splice @nib, $i-1, 2;
            $i--;
        }
        $M->{_ast} = $CClass::Empty;
        $M->{_ast} = $M->{_ast}->plus($_) for @bits;
    } else {
        # garden variety nibbler
        my @bits;
        for my $n (@{ $M->{nibbles} }) {
            my $bit = $n->isa('Str') ? $n->{TEXT} : $n->{_ast};

            if (ref($bit) && ref($bit) eq 'CClass') {
                $n->sorry("Tried to use a character class in a string");
                $bit = "";
            }

            # this *might* belong in an optimization pass
            if (!blessed($bit) && @bits && !blessed($bits[-1])) {
                $bits[-1] .= $bit;
            } else {
                push @bits, $bit;
            }
        }
        push @bits, '' unless @bits;
        @bits = map { blessed($_) ? $_ : Op::StringLiteral->new(node($M),
                text => $_) } @bits;
        $M->{_ast} = (@bits == 1) ? $bits[0] :
            Op::CallSub->new(node($M),
                invocant => Op::Lexical->new(name => '&infix:<~>'),
                positionals => \@bits);
    }
}

sub circumfix { }
sub circumfix__S_Lt_Gt { my ($cl, $M) = @_;
    my $sl = $M->{nibble}{_ast};

    if (!$sl->isa('Op::StringLiteral')) {
        $M->sorry("Runtime word splitting NYI");
        return;
    }

    my @tok = split ' ', $sl->text;
    @tok = map { Op::StringLiteral->new(node($M), text => $_) } @tok;

    $M->{_ast} = (@tok == 1) ? $tok[0] :
        Op::SimpleParcel->new(node($M), items => \@tok);
    $M->{qpvalue} = '<' . join(" ", map { $_->text } @tok) . '>'; # XXX what if there are spaces or >
}
sub circumfix__S_LtLt_GtGt { goto &circumfix__S_Lt_Gt }
sub circumfix__S_Fre_Nch { goto &circumfix__S_Lt_Gt }

sub circumfix__S_Paren_Thesis { my ($cl, $M) = @_;
    my @kids = grep { defined } @{ $M->{semilist}{_ast} };
    if (@kids == 1 && $kids[0]->isa('Op::WhateverCode')) {
        # XXX in cases like * > (2 + *), we *don't* want the parens to disable
        # syntactic specialization, since they're required for grouping
        $M->{_ast} = $kids[0];
    } else {
        $M->{_ast} = Op::StatementList->new(node($M), children =>
            [ map { Op::Paren->new(inside => $_) } @kids ]);
    }
}

sub circumfix__S_Bra_Ket { my ($cl, $M) = @_;
    my @kids = grep { defined } @{ $M->{semilist}{_ast} };
    if (! grep { !$_->isa('Op::StringLiteral') } @kids) {
        $M->{qpvalue} = "<" . join(" ", map { $_->text } @kids) . ">";
    }
    $M->{_ast} = Op::CallSub->new(node($M),
        invocant => Op::Lexical->new(node($M), name => '&_array_constructor'),
        args => [Op::StatementList->new(node($M), children =>
                [ map { Op::Paren->new(inside => $_) } @kids ])]);
}

sub check_hash { my ($cl, $M) = @_;
    my $do = $M->{pblock}{_ast}->do;

    return 0 unless $do->isa('Op::StatementList');
    return 1 if @{ $do->children } == 0;
    return 0 if @{ $do->children } > 1;

    $do = $do->children->[0];
    my @bits = $do->isa('Op::SimpleParcel') ? @{ $do->items } : ($do);

    return 1 if $bits[0]->isa('Op::SimplePair');

    if ($bits[0]->isa('Op::CallSub') &&
            $bits[0]->invocant->isa('Op::Lexical') &&
            $bits[0]->invocant->name eq '&infix:<=>>') {
        return 1;
    }

    if ($bits[0]->isa('Op::Lexical') && substr($bits[0]->name,0,1) eq '%') {
        return 1;
    }

    return 0;
}

sub circumfix__S_Cur_Ly { my ($cl, $M) = @_;
    $M->{pblock}{_ast}->type('bare');
    $M->{_ast} = Op::BareBlock->new(node($M), var => $cl->gensym,
        body => $M->{pblock}{_ast});

    if ($cl->check_hash($M)) {
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => Op::Lexical->new(node($M), name => '&_hash_constructor'),
            args => [Op::CallSub->new(node($M), invocant =>
                    $cl->block_to_closure($M, $M->{pblock}{_ast}, once => 1))]);
    }
}

sub circumfix__S_sigil { my ($cl, $M) = @_;
    circumfix__S_Paren_Thesis($cl, $M); # XXX
    $M->{_ast} = $cl->docontext($M, $M->{sigil}->Str, $M->{_ast});
}

sub infix_prefix_meta_operator { }
sub infix_prefix_meta_operator__S_Bang { my ($cl, $M) = @_;
    $M->{_ast} = Op::CallSub->new(
        invocant => Op::Lexical->new(name => '&notop'),
        args => [ $M->{infixish}{infix}{_ast} ]);
}
sub infix_prefix_meta_operator__S_R { my ($cl, $M) = @_;
    $M->{_ast} = Op::CallSub->new(
        invocant => Op::Lexical->new(name => '&reverseop'),
        args => [ $M->{infixish}{infix}{_ast} ]);
}
sub infix_prefix_meta_operator__S_Z { my ($cl, $M) = @_;
    $M->{_ast} = Op::CallSub->new(
        invocant => Op::Lexical->new(name => '&zipop'),
        args => [ $M->{infixish}{infix}{_ast} ]);
}
sub infix_prefix_meta_operator__S_S { my ($cl, $M) = @_;
    $M->{_ast} = Op::CallSub->new(
        invocant => Op::Lexical->new(name => '&seqop'),
        args => [ $M->{infixish}{infix}{_ast} ]);
}
sub infix_prefix_meta_operator__S_X { my ($cl, $M) = @_;
    $M->{_ast} = Op::CallSub->new(
        invocant => Op::Lexical->new(name => '&crossop'),
        args => [ $M->{infixish}{_ast} ]);
}

sub infixish { my ($cl, $M) = @_;
    if ($M->{colonpair}) {
        return; # handled in POST
    }

    if ($M->{infix_postfix_meta_operator}[0]) {
        # TODO: there should probably be at least a potential for others
        $M->{infix}{_ast} = Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&assignop'),
            args => [ $M->{infix}{_ast} ]);
    }
}

my %loose2tight = (
    '&&' => '&&', '||' => '||', '//' => '//', 'andthen' => 'andthen',
    'orelse' => '//', 'and' => '&&', 'or' => '||',
);

sub INFIX { my ($cl, $M) = @_;
    my $fn = $M->{infix}{_ast};
    my $s = $fn->isa('Op::Lexical') ? $fn->name :
        ($fn->isa('Op::CallSub') && $fn->invocant->isa('Op::Lexical')) ?
            $fn->invocant->name : '';
    my ($st,$l,$r) = $cl->whatever_precheck($s, $M->{left}{_ast},
        $M->{right}{_ast});

    if ($s eq '&infix:<?? !!>') { # XXX macro
        $M->{_ast} = Op::Conditional->new(node($M), check => $l,
            true => $M->{middle}{_ast}, false => $r);
    } elsif ($s eq '&infix:<:=>') {
        $M->{_ast} = Op::Bind->new(node($M), readonly => 0, lhs => $l,
            rhs => $r);
    } elsif ($s eq '&infix:<::=>') {
        $M->{_ast} = Op::Bind->new(node($M), readonly => 1, lhs => $l,
            rhs => $r);
    } elsif ($s eq '&infix:<,>') {
        #XXX STD bug causes , in setting to be parsed as left assoc
        my @r;
        push @r, $l->isa('Op::SimpleParcel') ? @{ $l->items } : ($l);
        push @r, $r->isa('Op::SimpleParcel') ? @{ $r->items } : ($r);
        $M->{_ast} = Op::SimpleParcel->new(items => \@r);
    } elsif ($s eq '&assignop' && $fn->args->[0]->isa('Op::Lexical') &&
            ($fn->args->[0]->name =~ /&infix:<(.*)>/) &&
            $loose2tight{$1}) {
        $M->{_ast} = Op::ShortCircuitAssign->new(node($M),
            kind => $loose2tight{$1}, lhs => $l, rhs => $r);
    } else {
        $M->{_ast} = Op::CallSub->new(node($M), invocant => $fn,
            positionals => [ $l, $r ]);

        if ($s eq '&infix:<=>') {
            # Assignments to has and state declarators are rewritten into
            # an appropriate phaser
            if ($l->isa('Op::Lexical') && $l->state_decl) {
                my $cv = $cl->gensym;
                $M->{_ast} = Op::StatementList->new(node($M), children => [
                    Op::Start->new(condvar => $cv, body => $M->{_ast}),
                    Op::Lexical->new(name => $l->name)]);
            }
            elsif ($l->isa('Op::Attribute') && !$l->initializer) {
                $l->initializer(
                    $cl->sl_to_block('bare', $r, subname => $l->name . " init")
                );
                $M->{_ast} = $l;
            }
            elsif ($l->isa('Op::ConstantDecl') && !$l->init) {
                $l->init($r);
                $M->{_ast} = $l;
            }
        }
    }
    $M->{_ast} = $cl->whatever_postcheck($M, $st, $M->{_ast});
}

sub CHAIN { my ($cl, $M) = @_;
    my @args;
    my @ops;
    for my $i (0 .. scalar @{ $M->{chain} }) {
        if ($i % 2) {
            push @ops, $M->{chain}[$i]{infix}{_ast};
        } else {
            push @args, $M->{chain}[$i]{_ast};
        }
    }

    my ($st, @vargs) = $cl->whatever_precheck('', @args);

    my @pairwise;
    while (@vargs >= 2) {
        push @pairwise, Op::CallSub->new(node($M),
                invocant => shift(@ops),
                positionals => [ $vargs[0], $vargs[1] ]);
        shift @vargs;
    }

    $M->{_ast} = (@pairwise > 1) ?  Op::ShortCircuit->new(node($M),
        kind => '&&', args => \@pairwise) : $pairwise[0];

    $M->{_ast} = $cl->whatever_postcheck($M, $st, $M->{_ast});
}

sub LIST { my ($cl, $M) = @_;
    if ($M->isa('STD::Regex')) {
        goto &LISTrx;
    }
    # STD guarantees that all elements of delims have the same sym
    # the last item may have an ast of undef due to nulltermish
    my $op  = $M->{delims}[0]{sym};
    my ($st, @pos) = $cl->whatever_precheck("&infix:<$op>",
        grep { defined } map { $_->{_ast} } @{ $M->{list} });

    if ($op eq ',') {
        $M->{_ast} = Op::SimpleParcel->new(node($M), items => \@pos);
    } elsif ($loose2tight{$op}) {
        $M->{_ast} = Op::ShortCircuit->new(node($M), kind => $loose2tight{$op},
            args => \@pos);
    } else {
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => Op::Lexical->new(name => "&infix:<$op>"),
            positionals => \@pos);
    }
    $M->{_ast} = $cl->whatever_postcheck($M, $st, $M->{_ast});
}

sub POSTFIX { my ($cl, $M) = @_;
    my $op = $M->{_ast};
    my ($st, $arg) = $cl->whatever_precheck('', $M->{arg}{_ast});
    if ($op->{postfix}) {
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => Op::Lexical->new(name => '&postfix:<' . $op->{postfix} . '>'),
            positionals => [ $arg ]);
    } elsif ($op->{postcircumfix}) {
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => Op::Lexical->new(name => '&postcircumfix:<' .
                $op->{postcircumfix} . '>'),
            positionals => [ $arg, @{ $op->{args} } ]);
    } elsif ($op->{name} && $op->{name} =~ /^(?:HOW|WHAT)$/) {
        if ($op->{args}) {
            $M->sorry("Interrogative operator " . $op->{name} .
                " does not take arguments");
            return;
        }
        $M->{_ast} = Op::Interrogative->new(node($M),
            receiver => $arg,
            name => $op->{name});
    } elsif ($op->{metamethod}) {
        $M->{_ast} = Op::CallMethod->new(node($M),
            receiver => $arg,
            ismeta => 1,
            name => $op->{metamethod},
            args => $op->{args} // []);
    } elsif ($op->{name}) {
        if ($op->{path} && !$op->{private}) {
            $M->sorry("Qualified references to non-private methods NYI");
        }
        $M->{_ast} = Op::CallMethod->new(node($M),
            receiver => $arg,
            private  => $op->{private},
            ppath    => $op->{path},
            name     => $op->{name},
            args     => $op->{args} // []);
    } elsif ($op->{quote}) {
        $M->{_ast} = Op::CallMethod->new(node($M),
            receiver => $arg,
            private  => $op->{private},
            name     => $op->{quote},
            args     => $op->{args} // []);
    } elsif ($op->{ref}) { # $obj.&foo
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => $op->{ref},
            args     => [ $arg, @{ $op->{args} // [] } ]);
    } elsif ($op->{postcall}) {
        if (@{ $op->{postcall} } > 1) {
            $M->sorry("Slicels NYI");
            return;
        }
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => $arg,
            args => ($op->{postcall}[0] // []));
    } elsif ($M->{colonpair}) {
        if ($arg->isa('Op::CallLike')) {
            $M->{_ast} = $arg->adverb($M->{colonpair}{_ast}{term});
        } else {
            $M->sorry("You can't adverb that");
            return;
        }
    } else {
        say join(" ", %$M);
        $M->sorry("Unhandled postop type");
    }
    $M->{_ast} = $cl->whatever_postcheck($M, $st, $M->{_ast});
}

sub PREFIX { my ($cl, $M) = @_;
    my $op = '&prefix:<' . $M->{sym} . '>';
    my $rarg = $M->{arg}{_ast};

    # Macros
    if ($op eq '&prefix:<temp>') {
        if (!$rarg->isa('Op::ContextVar') || $rarg->uplevel) {
            $M->sorry('Non-contextual case of temp NYI');
            $M->{_ast} = Op::StatementList->new;
            return;
        }
        $M->{_ast} = Op::CallSub->new(
            invocant => Op::Lexical->new(name => '&infix:<=>'),
            args => [ Op::Lexical->new(name => $rarg->name, declaring => 1,
                        hash => scalar($rarg->name =~ /^%/),
                        list => scalar($rarg->name =~ /^@/)),
                      Op::ContextVar->new(name => $rarg->name, uplevel => 1) ]);
        return;
    }

    my ($st, $arg) = $cl->whatever_precheck($op, $rarg);
    $M->{_ast} = $cl->whatever_postcheck($M, $st, Op::CallSub->new(node($M),
        invocant => Op::Lexical->new(name => $op),
        positionals => [ $M->{arg}{_ast} ]));
}

sub infix { my ($cl, $M) = @_;
    $M->{_ast} = Op::Lexical->new(name => '&infix:<' . $M->{sym} . '>');
}
sub infix__S_ANY { }

# hacked in infixish - = is the only one
sub infix_postfix_meta_operator { }
sub infix_postfix_meta_operator__S_Equal { }

sub prefix { }
sub prefix__S_ANY { }

sub postfix { }
sub postfix__S_ANY { }

sub postcircumfix { }
sub postcircumfix__S_Paren_Thesis { my ($cl, $M) = @_;
    $M->{_ast} = { postcall => $M->{semiarglist}{_ast} };
}

sub semilist_to_args { my ($cl, $M) = @_;
    if (@{ $M->{_ast} } > 1) {
        $M->sorry('Slice lookups NYI');
        return;
    }
    my ($al) = @{ $M->{_ast} };

    if (!defined $al) {
        return [];
    } elsif ($al && $al->isa('Op::SimpleParcel')) {
        return $al->items;
    } else {
        return [$al];
    }
}

sub postcircumfix__S_Bra_Ket { my ($cl, $M) = @_;
    $M->{_ast} = { postcircumfix => '[ ]', args => $M->{semilist}{_ast} };
}
sub postcircumfix__S_Cur_Ly { my ($cl, $M) = @_;
    $M->{_ast} = { postcircumfix => '{ }', args => $M->{semilist}{_ast} };
}
sub postcircumfix__S_Lt_Gt { my ($cl, $M) = @_;
    $cl->circumfix__S_Lt_Gt($M); #XXX
    delete $M->{qpvalue};
    $M->{_ast} = { postcircumfix => '{ }',
        args => [ $M->{_ast} ] };
}

sub postop { my ($cl, $M) = @_;
    $M->{_ast} = $M->{postcircumfix} ? $M->{postcircumfix}{_ast} :
        { postfix => $M->{sym} };
}
sub POST { my ($cl, $M) = @_;
    $M->{_ast} = $M->{dotty}{_ast} if $M->{dotty};
    $M->{_ast} = $M->{privop}{_ast} if $M->{privop};
    $M->{_ast} = $M->{postop}{_ast} if $M->{postop};
}

sub PRE { }

sub methodop { my ($cl, $M) = @_;
    my %r;
    if ($M->{longname}) {
        my $c = $cl->mangle_longname($M->{longname});
        @r{"name", "path"} = @$c{"name", "path"};
    }
    $r{quote} = $M->{quote}{_ast} if $M->{quote};
    $r{ref}   = $cl->do_variable_reference($M, $M->{variable}{_ast})
        if $M->{variable};

    $r{args}  = $M->{args}[0]{_ast}[0] if $M->{args}[0];
    $r{args}  = $M->{arglist}[0]{_ast} if $M->{arglist}[0];

    $M->{_ast} = \%r;
}

sub dottyop { my ($cl, $M) = @_;
    if ($M->{colonpair}) {
        $M->sorry("Colonpair dotties NYI");
        return;
    }

    $M->{_ast} = $M->{methodop}{_ast} if $M->{methodop};
    $M->{_ast} = $M->{postop}{_ast} if $M->{postop};
}

sub privop { my ($cl, $M) = @_;
    $M->{_ast} = { %{ $M->{methodop}{_ast} }, private => 1 };
}

sub dotty { }
sub dotty__S_Dot { my ($cl, $M) = @_;
    $M->{_ast} = $M->{dottyop}{_ast};
}

sub dotty__S_DotStar { my ($cl, $M) = @_;
    if ($M->{sym} eq '.^' && $M->{dottyop}{_ast}{name}) {
        $M->{_ast} = { metamethod => $M->{dottyop}{_ast}{name},
                       args => $M->{dottyop}{_ast}{args} };
    } else {
        $M->sorry('NYI dottyop form ' . $M->{sym});
    }
}

sub coloncircumfix { my ($cl, $M) = @_;
    $M->{_ast} = $M->{circumfix}{_ast};
    $M->{qpvalue} = $M->{circumfix}{qpvalue};
}

sub colonpair { my ($cl, $M) = @_;
    my $k = $M->{k};
    # STD seems to think term:name is term:sym<name>.  Needs speccy
    # clarification.  XXX
    my $n;
    if (!ref $M->{v}) {
        $n = ":" . ($M->{v} ? '' : '!') . $M->{k};
    } elsif (defined $M->{v}{qpvalue}) {
        $n = ":" . $M->{k} . $M->{v}{qpvalue};
    }
    my $tv = ref($M->{v}) ? $M->{v}{_ast} :
        Op::Lexical->new(name => $M->{v} ? 'True' : 'False');

    if (!defined $tv) {
        if (substr($M->{v}->Str,1,1) eq '<') {
            $tv = Op::CallMethod->new(name => 'at-key',
                receiver => Op::ContextVar->new(name => '$*/'),
                args => [Op::StringLiteral->new(text => $M->{k})]);
        } else {
            $tv = $cl->do_variable_reference($M,
                { sigil => $M->{v}{sigil}->Str,
                    twigil => ($M->{v}{twigil}[0] ? $M->{v}{twigil}[0]->Str : ''),
                    name => $M->{k} });
        }
    }

    $M->{_ast} = { ext => $n, term => Op::SimplePair->new(
            key => $M->{k}, value => $tv) };
}

sub fatarrow { my ($cl, $M) = @_;
    $M->{_ast} = Op::SimplePair->new(
        key => $M->{key}->Str,
        value => $M->{val}{_ast});
}

my %_nowhatever = (map { $_, 1 } ('&infix:<,>', '&infix:<..>', '&infix:<...>',
    '&infix:<=>', '&infix:<xx>'));
sub whatever_precheck { my ($cl, $op, @args) = @_;
    return ([], @args) if $_nowhatever{$op};
    my @vars;
    for (@args) {
        Carp::confess("invalid undef here") if !$_;
        if ($_->isa('Op::Whatever')) {
            push @vars, $_->slot;
            $_ = Op::Lexical->new(name => $_->slot);
        } elsif ($_->isa('Op::WhateverCode')) {
            push @vars, @{ $_->vars };
            $_ = $_->ops;
        }
    }
    (\@vars, @args);
}

sub whatever_postcheck { my ($cl, $M, $st, $term) = @_;
    if (@$st) {
        return Op::WhateverCode->new(ops => $term, vars => $st,
            slot => $cl->gensym, node($M));
    } else {
        return $term;
    }
}

# term :: Op
sub term { }

sub term__S_value { my ($cl, $M) = @_;
    $M->{_ast} = $M->{value}{_ast};
}

sub term__S_name { my ($cl, $M) = @_;
    my ($id, $path) = @{ $cl->mangle_longname($M->{longname}) }{'name','path'};

    if ($M->{args}) {
        $M->sorry("Unsupported form of term:name");
        return;
    }

    if ($path) {
        $M->{_ast} = Op::PackageVar->new(node($M), name => $id,
            slot => $cl->gensym, path => $path);
    } else {
        $M->{_ast} = Op::Lexical->new(node($M), name => $id);
    }

    if ($M->{postcircumfix}[0]) {
        # XXX SAFE::
        $M->{_ast} = Op::CallSub->new(node($M),
            invocant => Op::Lexical->new(name => '&_param_role_inst'),
            args => [ $M->{_ast}, @{ $M->{postcircumfix}[0]{_ast}{args} } ]);
    }
}

sub term__S_identifier { my ($cl, $M) = @_;
    my $id  = $M->{identifier}{_ast};
    my $sal = $M->{args}{_ast} // [];  # TODO: support zero-D slicels

    if (@$sal > 1) {
        $M->sorry("Slicel lists are NYI");
        return;
    }

    if ($M->is_name($M->{identifier}->Str)) {
        $M->{_ast} = Op::Lexical->new(node($M), name => $id);
        return;
    }

    my $args = $sal->[0] // [];

    $M->{_ast} = Op::CallSub->new(node($M),
        invocant => Op::Lexical->new(name => '&' . $id),
        args => $args);
}

sub term__S_self { my ($cl, $M) = @_;
    $M->{_ast} = Op::Lexical->new(node($M), name => 'self');
}

sub term__S_circumfix { my ($cl, $M) = @_;
    $M->{_ast} = $M->{circumfix}{_ast};
}

sub term__S_scope_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{scope_declarator}{_ast};
}

sub term__S_multi_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{multi_declarator}{_ast};
}

sub term__S_package_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_declarator}{_ast};
}

sub term__S_routine_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{routine_declarator}{_ast};
}

sub term__S_regex_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{regex_declarator}{_ast};
}

sub term__S_type_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{type_declarator}{_ast};
}

sub term__S_dotty { my ($cl, $M) = @_;
    $M->{_ast} = $M->{dotty}{_ast};
}

sub term__S_capterm { my ($cl, $M) = @_;
    $M->{_ast} = $M->{capterm}{_ast};
}

sub term__S_sigterm { my ($cl, $M) = @_;
    $M->{_ast} = $M->{sigterm}{_ast};
}

sub term__S_statement_prefix { my ($cl, $M) = @_;
    $M->{_ast} = $M->{statement_prefix}{_ast};
}

sub term__S_variable { my ($cl, $M) = @_;
    $M->{_ast} = $cl->do_variable_reference($M, $M->{variable}{_ast});
}

sub term__S_DotDotDot { my ($cl, $M) = @_;
    $M->{_ast} = Op::Yada->new(node($M), kind => '...');
}

sub term__S_BangBangBang { my ($cl, $M) = @_;
    $M->{_ast} = Op::Yada->new(node($M), kind => '!!!');
}

sub term__S_QuestionQuestionQuestion { my ($cl, $M) = @_;
    $M->{_ast} = Op::Yada->new(node($M), kind => '???');
}

sub term__S_lambda { my ($cl, $M) = @_;
    $M->{pblock}{_ast}->type('pointy');
    $M->{_ast} = $cl->block_to_closure($M, $M->{pblock}{_ast});
}

sub term__S_Star { my ($cl, $M) = @_;
    $M->{_ast} = Op::Whatever->new(node($M), slot => $cl->gensym);
}

sub term__S_colonpair { my ($cl, $M) = @_;
    if (@{ $M->{colonpair} } > 1) {
        $M->sorry("Multi colonpair syntax not yet understood"); #XXX
        return;
    }
    $M->{_ast} = $M->{colonpair}[0]{_ast}{term};
}

sub term__S_fatarrow { my ($cl, $M) = @_;
    $M->{_ast} = $M->{fatarrow}{_ast};
}

sub do_variable_reference { my ($cl, $M, $v) = @_;
    if ($v->{term}) {
        return $v->{term};
    }

    my $sl = $v->{sigil} . $v->{twigil} . $v->{name};

    if ($v->{rest} && $v->{twigil} =~ /[*=~?^:]/) {
        $M->sorry("Twigil " . $v->{twigil} . " cannot be used with " .
            "qualified names");
        return;
    }

    given ($v->{twigil}) {
        when ('!') {
            return Op::CallMethod->new(node($M), name => $v->{name},
                receiver => Op::Lexical->new(name => 'self'),
                private => 1, ppath => $v->{rest});
        }
        when ('.') {
            if ($v->{rest}) {
                $M->sorry('$.Foo::bar syntax NYI');
                return;
            }

            return Op::CallMethod->new(node($M), name => $v->{name},
                receiver => Op::Lexical->new(name => 'self'));
        }
        # no twigil in lex name for these
        when ({ '^' => 1, ":" => 1}) {
            return Op::Lexical->new(node($M), name => $v->{sigil} . $v->{name});
        }
        when ('?') {
            return Op::Lexical->new(node($M), name => $sl);
        }
        when ('*') {
            return Op::ContextVar->new(node($M), name => $sl);
        }
        when ('') {
            if ($v->{rest}) {
                return Op::PackageVar->new(path => $v->{rest}, name => $sl,
                    slot => $cl->gensym, node($M));
            } else {
                return Op::Lexical->new(node($M), name => $sl);
            }
        }
        default {
            $M->sorry("Unhandled reference twigil " . $v->{twigil});
        }
    }
}

sub docontext { my ($cl, $M, $sigil, $term) = @_;
    if ($sigil !~ /[\$\@\%]/) {
        $M->sorry("Unhandled conext character $sigil");
    }
    my $method = ($sigil eq '$') ? 'item' :
                 ($sigil eq '@') ? 'list' :
                                   'hash';

    Op::CallMethod->new(node($M), name => $method, receiver => $term);
}

sub variable { my ($cl, $M) = @_;
    my $sigil = $M->{sigil} ? $M->{sigil}->Str : substr($M->Str, 0, 1);
    my $twigil = $M->{twigil}[0] ? $M->{twigil}[0]{sym} : '';

    my ($name, $rest);
    my $dsosl = ($M->{desigilname} || $M->{sublongname} || {})->{_ast};
    if ($dsosl && $dsosl->{ind}) {
        $M->{_ast} = { term => $cl->docontext($M, $sigil, $dsosl->{ind}) };
        return;
    } elsif ($dsosl) {
        ($name, $rest) = @$dsosl{'name', 'path'};
    } elsif ($M->{name}[0]) {
        # Both these cases are marked XXX in STD.  I agree.  What are they for?
        if ($M->{name}[0]{dc}) {
            $M->sorry("*ONE* pair of leading colons SHALL BE ENOUGH");
            return;
        }
        if ($M->Str =~ /^\$::/) {
            $rest = $M->{name}[0]{_ast}{names};
            $name = pop @$rest;
        } else {
            if (@{ $M->{name}[0]{_ast}{names} } > 1) {
                $M->sorry("Nonsensical attempt to qualify a self-declared named parameter detected");
                return;
            }
            $name = $M->{name}[0]{_ast}{names}[0];
            $twigil = ':';
        }
    } elsif ($M->{special_variable}) {
        $name = substr($M->{special_variable}->Str, 1);
        $twigil = '*' if $name eq '/' or $name eq '!';
    } elsif ($M->{index}) {
        $M->{_ast} = { capid => $M->{index}{_ast}, term =>
            # maybe a little of a cheat
            $M->{_ast} = Op::CallMethod->new(node($M), name => 'at-pos',
                receiver => Op::ContextVar->new(name => '$*/'),
                positionals => [ Op::Num->new(value => $M->{index}{_ast}) ])
        };
        return;
    } elsif ($M->{postcircumfix}[0]) {
        if ($M->{postcircumfix}[0]{sym} eq '< >') {
            $M->{_ast} = { capid => $M->{postcircumfix}[0]{_ast}{args}[0]->text,
                term =>
                # maybe a little of a cheat
                $M->{_ast} = Op::CallMethod->new(node($M), name => 'at-key',
                    receiver => Op::ContextVar->new(name => '$*/'),
                    positionals => $M->{postcircumfix}[0]{_ast}{args})
            };
            return;
        } else {
            $M->sorry("Contextualizer variables NYI");
            return;
        }
    } else {
        say join " ", %$M;
        $M->sorry("Non-simple variables NYI");
        return;
    }

    $M->{_ast} = {
        sigil => $sigil, twigil => $twigil, name => $name, rest => $rest
    };
}

sub special_variable {}
sub special_variable__S_DollarSlash {}
sub special_variable__S_DollarBang {}
sub special_variable__S_Dollar_a2_ {}

sub param_sep {}

# :: { list : Bool, hash : Bool  slot : Maybe[Str], names : [Str] }
sub named_param { my ($cl, $M) = @_;
    my %rt;
    if ($M->{name}) {
        if ($M->{named_param}) {
            %rt = %{ $M->{named_param}{_ast} };
        } else {
            %rt = %{ $M->{param_var}{_ast} };
        }
        $rt{names} = [ @{ $rt{names} // [] }, $M->{name}->Str ];
    } else {
        %rt = %{ $M->{param_var}{_ast} };
        if ($rt{slot} && $rt{slot} =~ /^[\@\$\%][.*!]?(.*)/) {
            $rt{names} = [ $1 ];
        } else {
            $M->sorry("Abbreviated named parameter must have a name");
        }
    }
    $rt{positional} = 0;
    $M->{_ast} = \%rt;
}

# :: { list : Bool, hash : Bool, slot : Maybe[Str] }
sub param_var { my ($cl, $M) = @_;
    if ($M->{signature}) {
        $M->sorry('Sub-signatures NYI');
        return;
    }
    my $twigil = $M->{twigil}[0] ? $M->{twigil}[0]->Str : '';
    my $sigil = $M->{sigil}->Str;
    my $name = $M->{name}[0] ? $M->{name}[0]->Str : undef;
    $twigil = '*' if $name && ($name eq '/' || $name eq '!');

    my $slot;
    if ($twigil eq '') {
        $slot = defined($name) ? ($sigil . $name) : undef;
    } elsif ($twigil eq '*') {
        $slot = "$sigil*" . "$name";
    } else {
        $M->sorry("Unhandled parameter twigil $twigil");
        return;
    }

    if ($sigil ne '$' && $sigil ne '@' && $sigil ne '%' && $sigil ne '&') {
        $M->sorry('Non bare scalar targets NYI');
        return;
    }
    $M->{_ast} = { list => ($sigil eq '@'), hash => ($sigil eq '%'),
        slot => $slot };
}

# :: Sig::Parameter
sub parameter { my ($cl, $M) = @_;
    my $rw;

    for (@{ $M->{trait} }) {
        if ($_->{_ast}{rw}) { $rw = 1 }
        else {
            $M->sorry('Unhandled trait ' . (keys(%{ $_->{_ast} }))[0]);
        }
    }

    if (@{ $M->{post_constraint} } > 0) {
        $M->sorry('Parameter post constraints NYI');
        return;
    }

    my $default = $M->{default_value}[0] ? $M->{default_value}[0]{_ast} : undef;

    my $sorry;
    my $slurpy;
    my $slurpycap;
    my $optional;
    my $rwt;
    given ($M->{quant} . ':' . $M->{kind}) {
        when ('**:*') { $sorry = "Slice parameters NYI" }
        when ('*:*')  { $slurpy = 1 }
        when ('|:*')  { $slurpycap = 1 }
        when ('\\:!') { $rwt = 1 }
        when ('\\:?') { $rwt = 1; $optional = 1 }
        when (':!')   { }
        when (':*')   { $optional = 1 }
        when (':?')   { $optional = 1 }
        when ('?:?')  { $optional = 1 }
        when ('!:!')  { }
        when ('!:?')  { $optional = 1 }
        when ('!:*')  { }
        default       { $sorry = "Confusing parameters ($_)" }
    }
    if ($sorry) { $M->sorry($sorry); return }
    my $p = $M->{param_var} // $M->{named_param};

    $M->{_ast} = Sig::Parameter->new(name => $M->Str, default => $default,
        optional => $optional, slurpy => $slurpy, readonly => !$rw,
        slurpycap => $slurpycap, rwtrans => $rwt, %{ $p->{_ast} });
}

# signatures exist in several syntactic contexts so just make an object for now
sub signature { my ($cl, $M) = @_;
    if ($M->{type_constraint}[0]) {
        $M->sorry("Return type constraints NYI");
        return;
    }

    if ($M->{param_var}) {
        $M->{_ast} = Sig->new(params => [ Sig::Parameter->new(
                name => $M->{param_var}->Str, %{ $M->{param_var}{_ast} },
                full_parcel => 1) ]);
        return;
    }

    my $exp = 0;
    for (@{ $M->{param_sep} }) {
        if ($_->Str =~ /:/) {
            $exp = 1;
        } elsif ($_->Str !~ /[,:]/) {
            $M->sorry('Parameter separator ' . $_->Str . ' NYI');
            return;
        }
    }

    $M->{_ast} = Sig->new(explicit_inv => $exp, params =>
        [map { $_->{_ast} } @{ $M->{parameter} }]);
}

sub multisig { my ($cl, $M) = @_;
    if (@{ $M->{signature} } != 1) {
        $M->sorry("Multiple signatures NYI");
        return;
    }
    $M->{_ast} = $M->{signature}[0]{_ast};
}

sub cgopname { my ($cl, $M) = @_;
    $M->{_ast} = $M->Str;
}

sub cgexp { }
sub cgexp__S_name { my ($cl, $M) = @_;
    $M->{_ast} = $M->{cgopname}{_ast};
}

sub cgexp__S_p6exp { my ($cl, $M) = @_;
    $M->{_ast} = $M->{statementlist}{_ast};
}

sub cgexp__S_decint { my ($cl, $M) = @_;
    $M->{_ast} = $M->{decint}{_ast};
}

sub cgexp__S_quote { my ($cl, $M) = @_;
    if (!$M->{quote}{_ast}->isa('Op::StringLiteral')) {
        $M->sorry("Strings used in CgOp code must be compile time constants");
    }
    $M->{_ast} = $M->{quote}{_ast}->text;
}

my %opshortcut = (
    '@',   [ 'fetch' ],
    'l',   [ 'scopedlex' ],
    'ns',  [ 'newscalar' ],
    'nsw', [ 'newrwscalar' ],
    's',   [ 'str' ],
    'i',   [ 'int' ],
    'b',   [ 'bool' ],
    'd',   [ 'double' ],
    '==',  [ 'compare', '==' ], '!=',  [ 'compare', '!=' ],
    '>=',  [ 'compare', '>=' ], '<=',  [ 'compare', '<=' ],
    '<',   [ 'compare', '<' ],  '>',   [ 'compare', '>' ],
    '+',   [ 'arith', '+' ],    '-',   [ 'arith', '-' ],
    '*',   [ 'arith', '*' ],    '/',   [ 'arith', '/' ],
);

sub cgexp__S_op { my ($cl, $M) = @_;
    no strict 'refs';
    my $l = $M->{cgopname}{_ast};
    my @p = @{ $opshortcut{$l} // [ $l ] };
    $M->{_ast} = [@p, map { $_->{_ast} } @{ $M->{cgexp} }];
}

sub apostrophe {}
sub quibble { my ($cl, $M) = @_;
    if ($M->{babble}{B}[0]{_herelang}) { #XXX
        $M->{_ast} = Op::HereStub->new(node => $M);
    } else {
        $M->{_ast} = $M->{nibble}{_ast};
    }
}
sub tribble {}
sub babble {}
sub quotepair {}

# We can't do much at blockoid reduce time because the context is unknown.
# Roles and subs need somewhat different code gen
sub blockoid { my ($cl, $M) = @_;
    # XXX horrible cheat, but my data structures aren't up to the task of
    # $::UNIT being a class body &c.
    if ($M->Str eq '{YOU_ARE_HERE}') {
        $M->{_ast} = Op::YouAreHere->new(node($M), unitname => $::UNITNAME);
    } else {
        $M->{_ast} = $M->{statementlist}{_ast};
    }
}
sub lambda {}
sub embeddedblock { my ($cl, $M) = @_;
    $M->{_ast} = $cl->block_to_immediate($M, 'bare',
        $cl->sl_to_block('bare', $M->{statementlist}{_ast},
            signature => Sig->simple()));
}

sub sigil {}
sub sigil__S_Amp {}
sub sigil__S_Dollar {}
sub sigil__S_At {}
sub sigil__S_Percent {}

sub twigil {}
sub twigil__S_Equal {}
sub twigil__S_Bang {}
sub twigil__S_Dot {}
sub twigil__S_Tilde {}
sub twigil__S_Star {}
sub twigil__S_Question {}
sub twigil__S_Caret {}
sub twigil__S_Colon {}

sub terminator {}
sub terminator__S_Thesis {}
sub terminator__S_Semi {}
sub terminator__S_Ket {}
sub terminator__S_Ly {}
sub terminator__S_if {}
sub terminator__S_unless {}
sub terminator__S_for {}
sub terminator__S_until {}
sub terminator__S_then {}
sub terminator__S_again {}
sub terminator__S_repeat {}
sub terminator__S_while {}
sub terminator__S_else {}
sub terminator__S_BangBang {}
sub stdstopper {}
sub unitstopper {}
sub eat_terminator {}

sub scoped { my ($cl, $M) = @_;
    $M->{_ast} = ($M->{declarator} // $M->{regex_declarator} //
        $M->{package_declarator} // $M->{multi_declarator})->{_ast};
}

# :: Op
sub declarator { my ($cl, $M) = @_;
    if ($M->{signature}) {
        my @p = @{ $M->{signature}{_ast}->params };
        # TODO: keep the original signature around somewhere := can find it
        for (@p) {
            # TODO: fanciness checks
            $_ = Op::Lexical->new(node($M), name => $_->slot, list => $_->list,
                hash => $_->hash, declaring => 1);
        }
        $M->{_ast} = Op::SimpleParcel->new(node($M), items => \@p);
        return;
    }
    $M->{_ast} = $M->{variable_declarator} ? $M->{variable_declarator}{_ast} :
                 $M->{routine_declarator}  ? $M->{routine_declarator}{_ast} :
                 $M->{regex_declarator}    ? $M->{regex_declarator}{_ast} :
                 $M->{type_declarator}{_ast};
}

sub scope_declarator { my ($cl, $M) = @_;
    $M->{_ast} = $M->{scoped}{_ast};
}
sub scope_declarator__S_my {}
sub scope_declarator__S_our {}
sub scope_declarator__S_augment {}
sub scope_declarator__S_supersede {}
sub scope_declarator__S_has {}
sub scope_declarator__S_state {}
sub scope_declarator__S_anon {}

sub multi_declarator { my ($cl, $M) = @_;
    $M->{_ast} = ($M->{declarator} // $M->{routine_def})->{_ast};
}
sub multi_declarator__S_multi {}
sub multi_declarator__S_proto {}
sub multi_declarator__S_only  {}

sub variable_declarator { my ($cl, $M) = @_;
    if ($::MULTINESS) {
        $M->sorry("Multi variables NYI");
    }
    if ($M->{trait}[0] || $M->{post_constraint}[0] || $M->{shape}[0]) {
        $M->sorry("Traits, postconstraints, and shapes on variable declarators NYI");
        return;
    }

    my $scope = $::SCOPE // 'my';

    if ($scope eq 'augment' || $scope eq 'supersede') {
        $M->sorry("Illogical scope $scope for simple variable");
        return;
    }

    my $v = $M->{variable}{_ast};
    my $t = $v->{twigil};
    if ($t =~ /[?=~^:]/) {
        $M->sorry("Variables with the $t twigil cannot be declared using " .
            "$scope; they are created " .
            ($t eq '?' ? "using 'constant'." :
             $t eq '=' ? "by parsing POD blocks." :
             $t eq '~' ? "by 'slang' definitions." :
             "automatically as parameters to the current block."));
        return;
    }

    if ($scope ne 'has' && $t =~ /[.!]/) {
        $M->sorry("Twigil $t is only valid on attribute definitions ('has').");
        return;
    }

    if ($v->{rest}) {
        $M->sorry(":: syntax is only valid when referencing variables, not when defining them.");
        return;
    }

    my $name = $v->{sigil} . $v->{twigil} . $v->{name};
    # otherwise identical to my
    my $slot = ($scope eq 'anon') ? $cl->gensym : $name;

    if ($scope eq 'has') {
        $M->{_ast} = Op::Attribute->new(node($M), name => $v->{name},
            accessor => ($v->{twigil} eq '.'));
    } elsif ($scope eq 'state') {
        $M->{_ast} = Op::Lexical->new(node($M), name => $slot, state_decl => 1,
            state_backing => $cl->gensym, declaring => 1,
            list => ($v->{sigil} eq '@'), hash => ($v->{sigil} eq '%'));
    } elsif ($scope eq 'our') {
        $M->{_ast} = Op::PackageVar->new(node($M), name => $slot, slot => $slot,
            path => [ 'OUR' ]);
    } else {
        $M->{_ast} = Op::Lexical->new(node($M), name => $slot, declaring => 1,
            list => ($v->{sigil} eq '@'), hash => ($v->{sigil} eq '%'));
    }
}

sub type_declarator {}
sub type_declarator__S_constant { my ($cl, $M) = @_;
    if ($::MULTINESS) {
        $M->sorry("Multi variables NYI");
    }
    my $scope = $::SCOPE // 'my';
    if (!$M->{identifier} && !$M->{variable}) {
        $M->sorry("Anonymous constants NYI"); #wtf?
        return;
    }
    my $slot  = ($M->{identifier} // $M->{variable})->Str;

    $M->{_ast} = Op::ConstantDecl->new(node($M), name => $slot,
        path => ($::SCOPE eq 'our' ? [ 'OUR' ] : undef));
}

sub package_declarator {}
sub package_declarator__S_class { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_grammar { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_package { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_module { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_knowhow { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_role { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_slang { my ($cl, $M) = @_;
    $M->{_ast} = $M->{package_def}{_ast};
}

sub package_declarator__S_also { my ($cl, $M) = @_;
    $M->{_ast} = Op::StatementList->new(node($M), children =>
        $cl->process_package_traits($M, undef, @{ $M->{trait} }));
}

sub package_declarator__S_require { my ($cl, $M) = @_;
    if ($M->{EXPR}[0]) {
        $M->sorry('Expressional forms of require NYI');
        return;
    }
    $M->{_ast} = Op::Require->new(node($M), unit => $M->{module_name}->Str);
}

sub process_package_traits { my ($cl, $M, $export, @tr) = @_;
    my @r;

    for (@tr) {
        if (exists $_->{_ast}{name}) {
            push @r, Op::Super->new(node($M), name => $_->{_ast}{name},
                path => $_->{_ast}{path});
        } elsif ($_->{_ast}{export}) {
            if ($export) {
                push @$export, @{ $_->{_ast}{export} };
            } else {
                $M->sorry('Cannot mark a class as exported outside the declarator');
            }
        } else {
            $M->sorry("Non-superclass traits for packageoids NYI");
        }
    }

    @r;
}

sub termish {}
sub nulltermish { my ($cl, $M) = @_; # for 1,2,3,
    # XXX this is insane
    $M->{term}{_ast} = $M->{term}{term}{_ast} if $M->{term};
}
sub EXPR {}
sub modifier_expr { my ($cl, $M) = @_;
    $M->{_ast} = $M->{EXPR}{_ast};
}
sub default_value { my ($cl, $M) = @_;
    $M->{_ast} = Body->new(transparent => 1, name => 'ANON',
        do => $M->{EXPR}{_ast});
}

sub arglist { my ($cl, $M) = @_;
    $M->sorry("Invocant handling is NYI") if $::INVOCANT_IS;
    my $x = $M->{EXPR}{_ast};

    if (!defined $x) {
        $M->{_ast} = [];
    } elsif ($x && $x->isa('Op::SimpleParcel')) {
        $M->{_ast} = $x->items;
    } else {
        $M->{_ast} = [$x];
    }
}

sub semiarglist { my ($cl, $M) = @_;
    $M->{_ast} = [ map { $_->{_ast} } @{ $M->{arglist} } ];
}

sub args { my ($cl, $M) = @_;
    if ($M->{moreargs} || $M->{semiarglist} && $M->{arglist}[0]) {
        $M->sorry("Interaction between semiargs and args is not understood");
        return;
    }

    $M->{_ast} = $M->{semiarglist} ? $M->{semiarglist}{_ast} :
        $M->{arglist}[0] ? [ $M->{arglist}[0]{_ast} ] : undef;
}

sub statement { my ($cl, $M) = @_;
    if ($M->{label}) {
        $M->sorry("Labels are NYI");
        return;
    }

    $M->{_ast} = $M->{statement_control} ? $M->{statement_control}{_ast} :
                 $M->{EXPR} ? $M->{EXPR}{_ast} : undef;

    if ($M->{statement_mod_cond}[0]) {
        my ($sym, $exp) = @{ $M->{statement_mod_cond}[0]{_ast} };

        if ($sym eq 'if') {
            $M->{_ast} = Op::Conditional->new(node($M), check => $exp,
                true => $M->{_ast}, false => undef);
        } elsif ($sym eq 'unless') {
            $M->{_ast} = Op::Conditional->new(node($M), check => $exp,
                false => $M->{_ast}, true => undef);
        } else {
            $M->sorry("Unhandled statement modifier $sym");
            return;
        }
    }

    if ($M->{statement_mod_loop}[0]) {
        my ($sym, $exp) = @{ $M->{statement_mod_loop}[0]{_ast} };

        if ($sym eq 'while') {
            $M->{_ast} = Op::WhileLoop->new(node($M), check => $exp,
                body => $M->{_ast}, until => 0, once => 0);
        } elsif ($sym eq 'until') {
            $M->{_ast} = Op::WhileLoop->new(node($M), check => $exp,
                body => $M->{_ast}, until => 1, once => 0);
        } else {
            $M->sorry("Unhandled statement modifier $sym");
            return;
        }
    }
}

sub statement_mod_cond { my ($cl, $M) = @_;
    $M->{_ast} = [ $M->{sym}, $M->{modifier_expr}{_ast} ];
}
sub statement_mod_loop { my ($cl, $M) = @_;
    $M->{_ast} = [ $M->{sym}, $M->{modifier_expr}{_ast} ];
}

sub statement_mod_cond__S_if {}
sub statement_mod_cond__S_unless {}
sub statement_mod_cond__S_when {}
sub statement_mod_loop__S_while {}
sub statement_mod_loop__S_until {}
sub statement_mod_loop__S_for {}
sub statement_mod_loop__S_given {}

sub statementlist { my ($cl, $M) = @_;
    $M->{_ast} = Op::StatementList->new(node($M), children =>
        [ map { $_->statement_level } grep { defined }
            map { $_->{_ast} } @{ $M->{statement} } ]);
}

sub semilist { my ($cl, $M) = @_;
    $M->{_ast} = [  map { $_->{_ast} } @{ $M->{statement} } ];
}

sub module_name { }
sub module_name__S_normal { my ($cl, $M) = @_;
    # name-extension stuff is just ignored on module names for now
    $M->{_ast} = {
        name => $M->{longname}{name}->Str,
        args => $M->{arglist}[0] ? $M->{arglist}[0]{_ast} : undef };
}

sub statement_control { }


# passes the $cond to the $block if it accepts a parameter, otherwise just runs it
sub _if_block {
    my ($cl,$M,$cond,$block) = @_;
    if (defined $block->{lambda}) {
        my $true_block = $cl->block_to_closure($block, $block->{_ast} , once => 1);
        Op::CallSub->new(node($M),
            invocant => $true_block,
            positionals => [$cond]
        );
    } else {
        $cl->block_to_immediate($M, 'cond', $block->{_ast}),
    }

}

# handles all the if branches
sub _if_branches {
    my ($cl,$M,$previous_cond,$branch,@other_branches) = @_;
    if ($branch) {
        Op::Helpers::let($branch->{_ast}[0] => sub {
            my $cond = shift;
            Op::Conditional->new(node($M), check => $cond,
                true => _if_block($cl,$M,$cond,$branch->{pblock}),
                false => _if_branches($cl,$M,$cond,@other_branches));
        });
    } else {
        $M->{else}[0] ? _if_block($cl,$M,$previous_cond,$M->{else}[0]) : undef;
    }
}

sub statement_control__S_if {
    my ($cl, $M) = @_;
    $M->{_ast} = _if_branches($cl,$M,undef,$M->{xblock},@{$M->{elsif}});
}

sub statement_control__S_while { my ($cl, $M) = @_;
    $M->{_ast} = Op::WhileLoop->new(node($M), check => $M->{xblock}{_ast}[0],
        body => $cl->block_to_immediate($M, 'loop',$M->{xblock}{_ast}[1]),
        until => 0, once => 0);
}

sub statement_control__S_until { my ($cl, $M) = @_;
    $M->{_ast} = Op::WhileLoop->new(node($M), check => $M->{xblock}{_ast}[0],
        body => $cl->block_to_immediate($M, 'loop', $M->{xblock}{_ast}[1]),
        until => 1, once => 0);
}

sub statement_control__S_for { my ($cl, $M) = @_;
    $M->{xblock}{_ast}[1]->type('loop');
    # TODO: should use 'once'
    $M->{_ast} = Op::ForLoop->new(node($M), source => $M->{xblock}{_ast}[0],
        sink => $cl->block_to_closure($M, $M->{xblock}{_ast}[1]));
}

sub statement_control__S_use { my ($cl, $M) = @_;
    if ($M->{version}) {
        return;
    }

    my $name = $M->{module_name}{_ast}{name};
    my $args = $M->{arglist} ? $M->{arglist}{_ast} : [];

    if ($M->{module_name}{_ast}{args}) {
        $M->sorry("'use' of an instantiated role not yet understood");
        return;
    }

    if (@$args) {
        $M->sorry("'use' with arguments NYI");
        return;
    }

    if ($name eq 'MONKEY_TYPING' || $name eq 'fatal' || $name eq 'lib') {
        return;
    }

    $M->{_ast} = Op::Use->new(node($M), unit => $name);
}

# All package defs have a couple things in common - a special-ish block,
# with a special decl, and some nice runtimey code
sub package_def { my ($cl, $M) = @_;
    if ($::MULTINESS) {
        $M->sorry("Multi variables NYI");
    }
    my $scope = $::SCOPE;
    if (!$M->{longname}[0]) {
        $scope = 'anon';
    }
    if ($scope eq 'supersede') {
        $M->sorry('Supercede is not yet supported');
        return;
    }
    if ($scope eq 'has' || $scope eq 'state') {
        $M->sorry("Illogical scope $scope for package block");
        return;
    }

    my ($name, $outervar, @augpkg);

    if ($scope eq 'augment') {
        my $r = $cl->mangle_longname($M->{longname}[0]);
        $name = $r->{name};
        @augpkg = @{ $r->{path} // ['MY'] };
    } else {
        $name = $M->{longname}[0] ?
            $cl->unqual_longname($M->{longname}[0],
                "Qualified package definitions NYI", 1) : 'ANON';
        $outervar = $scope ne 'anon' ? $name : $cl->gensym;
    }

    my $optype = 'Op::' . ucfirst($::PKGDECL) . 'Def';
    my $blocktype = $::PKGDECL;
    my $bodyvar = $cl->gensym;
    # currently always install into the local stash
    my $ourpkg = ($scope eq 'our') ? [ 'OUR::' ] : undef;

    if ($scope eq 'augment') {
        my $stmts = $M->{statementlist} // $M->{blockoid};
        $stmts = $stmts->{_ast};
        my $cbody = $cl->sl_to_block($blocktype, $stmts, name => $name, subname => "augment-" . $name // 'ANON');

        $M->{_ast} = Op::Augment->new(
            node($M),
            pkg     => [@augpkg],
            name    => $name,
            bodyvar => $bodyvar,
            body    => $cbody);
    } elsif (!$M->{decl}{stub}) {
        my $stmts = $M->{statementlist} // $M->{blockoid};
        my @export;

        $stmts = Op::StatementList->new(children =>
            [ $cl->process_package_traits($M, \@export, @{ $M->{trait} }),
                $stmts->{_ast} ]);

        my $cbody = $cl->sl_to_block($blocktype, $stmts,
            name => $name, subname => ($::PKGDECL . '-' . ($name // 'ANON')));
        $M->{_ast} = $optype->new(
            node($M),
            (($blocktype eq 'role' && $M->{signature}[0]) ?
                (signature => $M->{signature}[0]{_ast}) : ()),
            name    => $name,
            var     => $outervar,
            exports => \@export,
            bodyvar => $bodyvar,
            ourpkg  => $ourpkg,
            body    => $cbody);
    } else {
        $M->{_ast} = $optype->new(
            node($M),
            name    => $name,
            var     => $outervar,
            ourpkg  => $ourpkg,
            stub    => 1);
    }
}

sub trait_mod {}
sub trait_mod__S_is { my ($cl, $M) = @_;
    my $trait = $M->{longname}->Str;
    my $noparm;

    if ($M->is_name($trait)) {
        $M->{_ast} = $cl->mangle_longname($M->{longname});
        $noparm = 'Superclasses cannot have parameters';
    } elsif ($trait eq 'export') {
        $M->{_ast} = { export => [ 'DEFAULT', 'ALL' ] };
        $noparm = 'Export tags NYI';
    } elsif ($trait eq 'rawcall') {
        $M->{_ast} = { nobinder => 1 };
    } elsif ($trait eq 'return-pass') { # &return special
        $M->{_ast} = { return_pass => 1 };
    } elsif ($trait eq 'rw') {
        $M->{_ast} = { rw => 1 };
    } else {
        $M->sorry('Unhandled trait ' . $trait);
    }

    if ($noparm && $M->{circumfix}[0]) {
        $M->sorry($noparm);
        return;
    }
}

sub trait { my ($cl, $M) = @_;
    if ($M->{colonpair}) {
        $M->sorry('Colonpair traits NYI');
        return;
    }

    $M->{_ast} = $M->{trait_mod}{_ast};
}

sub routine_declarator {}
sub routine_declarator__S_sub { my ($cl, $M) = @_;
    $M->{_ast} = $M->{routine_def}{_ast};
}
sub routine_declarator__S_method { my ($cl, $M) = @_;
    $M->{_ast} = $M->{method_def}{_ast};
}
sub routine_declarator__S_submethod { my ($cl, $M) = @_;
    $M->{_ast} = $M->{method_def}{_ast};
    if ($M->{_ast}->method_too->[0] ne 'normal') {
        $M->sorry("Call pattern decorators cannot be used with submethod");
        $M->{_ast} = undef;
        return;
    }
    $M->{_ast}->method_too->[0] = 'sub';
}

my $next_anon_id = 0;
sub gensym { 'anon_' . ($next_anon_id++) }
sub genid  { ($next_anon_id++) }

sub blockcheck { my ($cl) = @_;
}

sub sl_to_block { my ($cl, $type, $ast, %args) = @_;
    my $subname = $args{subname} // 'ANON';
    $cl->blockcheck;
    Body->new(
        name      => $subname,
        returnable=> $args{returnable} // ($type eq 'sub'),
        ($type eq 'mainline' ? (
                file => $::FILE->{name},
                text => $::ORIG) : ()),
        type      => $type,
        signature => $args{signature},
        do        => $ast);
}

sub get_outer { my ($cl, $pad) = @_;
    $STD::ALL->{ $pad->{'OUTER::'}[0] };
}

sub block_to_immediate { my ($cl, $M, $type, $blk) = @_;
    $blk->type($type);
    Op::CallSub->new(node($M),
        invocant => $cl->block_to_closure($M, $blk, once => 1),
        positionals => []);
}

sub block_to_closure { my ($cl, $M, $blk, %args) = @_;
    my $outer_key = $args{outer_key} // $cl->gensym;

    Op::SubDef->new(var => $outer_key, body => $blk, node($M),
        once => $args{once}, method_too => $args{method_too},
        exports => ($args{exports} // []));
}

sub get_placeholder_sig { my ($cl, $M) = @_;
    # for some reason, STD wants to deparse this
    my @things = split ", ", $::CURLEX->{'$?SIGNATURE'};
    shift @things if $things[0] eq '';
    my @parms;
    for (@things) {
        if ($_ =~ /^\$_ is ref/) {
            push @parms, Sig::Parameter->new(optional => 1,
                slot => '$_', name => '$_');
        } elsif ($_ eq '*@_') {
            push @parms, Sig::Parameter->new(slurpy => 1, slot => '@_',
                list => 1, name => '*@_');
        } elsif ($_ =~ /^([@\$])/) {
            push @parms, Sig::Parameter->new(slot => $_, name => $_,
                list => ($1 eq '@'));
        } else {
            $M->sorry('Named placeholder parameters NYI');
            return;
        }
    }
    return Sig->new(params => \@parms);
}

# always a sub, though sometimes it's an implied sub after multi/proto/only
sub routine_def { my ($cl, $M) = @_;
    if ($::MULTINESS) {
        $M->sorry("Multi routines NYI");
    }
    if ($M->{sigil}[0] && $M->{sigil}[0]->Str eq '&*') {
        $M->sorry("Contextuals NYI");
        return;
    }
    my $dln = $M->{deflongname}[0];
    if (@{ $M->{multisig} } > 1) {
        $M->sorry("Multiple multisigs (what?) NYI");
        return;
    }
    my @export;
    my $return_pass = 0;
    my $signature = $M->{multisig}[0] ? $M->{multisig}[0]{_ast} :
        $cl->get_placeholder_sig($M);
    for my $t (@{ $M->{trait} }) {
        if ($t->{_ast}{export}) {
            push @export, @{ $t->{_ast}{export} };
        } elsif ($t->{_ast}{nobinder}) {
            $signature = undef;
        } elsif ($t->{_ast}{return_pass}) {
            $return_pass = 1;
        } else {
            $M->sorry('Non-export sub traits NYI');
        }
    }
    my $scope = !$dln ? 'anon' : $::SCOPE || 'my';
    my ($m,$p) = $dln ? @{$cl->mangle_longname($dln)}{'name','path' } : ();

    if ($scope ne 'my' && $scope ne 'our' && $scope ne 'anon') {
        $M->sorry("Illegal scope $scope for subroutine");
        return;
    }
    if ($scope eq 'our') {
        $M->sorry('Package subs NYI');
        return;
    } elsif ($p) {
        $M->sorry('Defining a non-our sub with a package-qualified name makes no sense');
        return;
    }

    $M->{_ast} = $cl->block_to_closure($M,
            $cl->sl_to_block('sub',
                $M->{blockoid}{_ast},
                returnable => !$return_pass,
                subname => $m,
                signature => $signature),
        outer_key => (($scope eq 'my') ? "&$m" : undef),
        exports => \@export);
}

sub method_def { my ($cl, $M) = @_;
    my $scope = $::SCOPE // 'has';
    my $type = $M->{type} ? $M->{type}->Str : '';
    $type = ($type eq ''  ? 'normal' :
             $type eq '^' ? 'meta' :
             $type eq '!' ? 'private' :
             do {
                 $M->sorry("Unhandled method decoration $type");
                 return;
             });
    $scope = 'anon' if !$M->{longname};
    my $name = $M->{longname} ? $cl->unqual_longname($M->{longname},
        "Qualified method definitions not understood") : undef; #XXX

    if ($M->{sigil}) {
        $M->sorry("Method sgils NYI");
        return;
    }
    if ($type eq 'meta') {
        $M->sorry("Metamethod mixins NYI");
        return;
    }
    if (@{ $M->{multisig} } > 1) {
        $M->sorry("Multiple multisigs (what?) NYI");
        return;
    }

    my $sym = ($scope eq 'my') ? ('&' . $name) : $cl->gensym;

    if ($scope eq 'augment' || $scope eq 'supersede' || $scope eq 'state') {
        $M->sorry("Illogical scope $scope for method");
        return;
    }

    if ($scope eq 'our') {
        $M->sorry("Packages NYI");
        return;
    }
    my $sig = $M->{multisig}[0] ? $M->{multisig}[0]{_ast} :
        $cl->get_placeholder_sig($M);

    for my $t (@{ $M->{trait} }) {
        if ($t->{_ast}{nobinder}) {
            $sig = undef;
        } else {
            $M->sorry("NYI method trait " . $M->Str);
        }
    }

    my $bl = $cl->sl_to_block('sub', $M->{blockoid}{_ast},
        subname => $name,
        signature => $sig ? $sig->for_method : undef);

    $M->{_ast} = $cl->block_to_closure($M, $bl, outer_key => $sym,
        method_too => ($scope ne 'anon' ? [ $type, $name ] : undef));
}

sub block { my ($cl, $M) = @_;
    $M->{_ast} = $cl->sl_to_block('', $M->{blockoid}{_ast});
}

# :: Body
sub pblock { my ($cl, $M) = @_;
    my $rw = $M->{lambda} && $M->{lambda}->Str eq '<->';
    $M->{_ast} = $cl->sl_to_block('', $M->{blockoid}{_ast},
        signature => ($M->{signature} ? $M->{signature}{_ast} :
            $cl->get_placeholder_sig($M)));
}

sub xblock { my ($cl, $M) = @_;
    $M->{_ast} = [ $M->{EXPR}{_ast}, $M->{pblock}{_ast} ];
}

# returns Body of 0 args
sub blast { my ($cl, $M) = @_;
    if ($M->{block}) {
        $M->{_ast} = $M->{block}{_ast};
    } else {
        $M->{_ast} = Body->new(
            transparent => 1,
            name => 'ANON',
            do   => $M->{statement}{_ast});
    }
}

sub statement_prefix {}
sub statement_prefix__S_do { my ($cl, $M) = @_;
    $M->{_ast} = $cl->block_to_immediate($M, 'do', $M->{blast}{_ast});
}
sub statement_prefix__S_gather { my ($cl, $M) = @_;
    $M->{blast}{_ast}->type('gather');
    $M->{_ast} = Op::Gather->new(node($M), var => $cl->gensym,
        body => $M->{blast}{_ast});
}
sub statement_prefix__S_try { my ($cl, $M) = @_;
    $M->{_ast} = Op::Try->new(node($M), body =>
        $cl->block_to_immediate($M, 'try', $M->{blast}{_ast}));
}

sub statement_prefix__S_START { my ($cl, $M) = @_;
    my $cv = $cl->gensym;
    $M->{_ast} = Op::Start->new(node($M), condvar => $cv, body =>
        $cl->block_to_immediate($M, 'phaser', $M->{blast}{_ast}));
}

# TODO: retain and return a value
sub statement_prefix__S_INIT { my ($cl, $M) = @_;
    $M->{blast}{_ast}->type('init');
    $M->{_ast} = Op::VoidPhaser->new(node($M), body => $M->{blast}{_ast});
}
# XXX 'As soon as possible' isn't quite soon enough here
sub statement_prefix__S_BEGIN { my ($cl, $M) = @_;
    $M->{blast}{_ast}->type('begin');
    $M->{_ast} = Op::VoidPhaser->new(node($M), body => $M->{blast}{_ast});
}
*statement_prefix__S_CHECK = *statement_prefix__S_BEGIN;

sub statement_prefix__S_END { my ($cl, $M) = @_;
    $M->{blast}{_ast}->type('end');
    $M->{_ast} = Op::VoidPhaser->new(node($M), body => $M->{blast}{_ast});
}

sub comp_unit { my ($cl, $M) = @_;
    my $body;
    my $sl = $M->{statementlist}{_ast};

    $body = $cl->sl_to_block('mainline', $sl, subname => 'mainline');

    my $sn = $::SETTINGNAME; $sn =~ s/::/./g;
    $M->{_ast} = Unit->new(mainline => $body, name => $::UNITNAME,
        is_setting => (!!$::YOU_WERE_HERE), setting_name => $sn);
}

1;
