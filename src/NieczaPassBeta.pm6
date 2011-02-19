class NieczaPassBeta;

use CgOp;

# A simple Perl6 compiler generates a lot of expressions of the form
# (-> $x { block })($y), due to control structures and regexes.  Try to clean
# that up here.

method invoke($*unit) {
    # XXX enter and sigs need love
    $*unit.visit_local_subs_postorder(-> $su {
        $su.code = run_optree($su, $su.code)
    });
    $*unit;
}

sub run_optree($body, $op) {

    for $op.zyg {
        $_ = run_optree($body, $_);
    }

    return $op unless $op.^isa(::Op::CallSub) && no_named_params($op);
    my $inv = $op.invocant;
    return $op unless $inv.^isa(::Op::SubDef) && $inv.once;
    my $cbody = $body.find_lex($inv.var) or return $op;
    $cbody = $cbody.body;
    return $op unless is_removable_body($cbody);

    beta_optimize($body, $op, $inv, $cbody);
}

sub no_named_params($op) {
    if defined $op.args {
        for @( $op.args ) {
            # XXX flattening check?
            return False if $_.^isa(::Op::SimplePair);
        }
    }
    return True;
}

sub is_removable_body($body) {
    #deb $body->name, " is a candidate for beta-removal";

    if !$body.signature {
        #deb "... unsuitable because it's a raw call";
        return False;
    }

    return False if $body.strong_used;

    my @z = $body.children;
    return False if @z;

    # We can't currently handle the possibility of outer references to the
    # frame we're mangling
    for keys $body.lexicals -> $lname {
        my $lex = $body.lexicals{$lname};

        if (!$lex.^isa(::Metamodel::Lexical::Simple)) {
            #deb "... unsuitable because it has an unhandled decl $_";
            return False;
        }

        if $lname ~~ /^.?<[?*]>/ {
            #deb "... unsuitable because it has a context variable ($lname)";
            return False;
        }
    }

    return True;
}

# Applicability already checked
sub beta_optimize($body, $op, $inv, $cbody) {
    # Bind the arguments to gensyms so they won't be shadowed by anything in
    # the function
    my @args = map { [ $_, ::GLOBAL::NieczaActions.gensym ] }, @( $op.positionals );

    $body.delete_lex($inv.var);
    $*unit.xref.[$cbody.xref[1]] = Any;
    {
        my $c = $cbody.outer.zyg;
        @$c = grep { $_ !=== $cbody }, @$c;
    }

    my @pos = (map { ::Op::LetVar.new(name => $_[1]) }, @args);

    my $nop = ::Op::StatementList.new(children => [
        ::Op::SigBind.new(signature => $cbody.signature,
            positionals => @pos),
        $cbody.code]);

    my @scope;
    for sort keys $cbody.lexicals -> $dn {
        my $d = $cbody.lexicals{$dn};
        my $nm = ::GLOBAL::NieczaActions.gensym;
        my $to = $d.noinit ?? CgOp.null('var') !!
                 $d.hash   ?? CgOp.newblankhash !!
                 $d.list   ?? CgOp.newblanklist !!
                              CgOp.newblankrwscalar;
        $nop = ::Op::Let.new(var => $nm,
            to => ::Op::CgOp.new(op => $to), in => $nop);
        push @scope, $dn, $nm;
    }

    $nop = ::Op::LetScope.new(names => @scope, inner => $nop,
        transparent => $cbody.transparent);
    for reverse @args -> $a {
        $nop = ::Op::Let.new(var => $a.[1], to => $a.[0], in => $nop);
    }

    if $cbody.topicalizer {
        $nop = ::Op::TopicalHook.new(inner => $nop);
    }

    $nop;
}
