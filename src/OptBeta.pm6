class OptBeta;

use CgOp;

# A simple Perl6 compiler generates a lot of expressions of the form
# (-> $x { block })($y), due to control structures and regexes.  Try to clean
# that up here.

method make_call($var, *@params) {
    my $nonopt = ::Op::CallSub.new(
        positionals => [ @params ],
        invocant => ::Op::Lexical.new(name => $var));
    my $cbody = $*CURLEX<!sub>.find_lex($var) or return $nonopt;
    $cbody = $cbody.body;
    return $nonopt unless is_removable_body($cbody);

    beta_optimize($*CURLEX<!sub>, $var, $cbody, @params);
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
sub beta_optimize($body, $symbol, $cbody, @inpos) {
    # Bind the arguments to gensyms so they won't be shadowed by anything in
    # the function
    my @args = map { [ $_, ::GLOBAL::NieczaActions.gensym ] }, @inpos;

    $body.delete_lex($symbol);
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
    my @let;
    for sort keys $cbody.lexicals -> $dn {
        my $d = $cbody.lexicals{$dn};
        my $nm = ::GLOBAL::NieczaActions.gensym;
        my $to = $d.noinit ?? CgOp.null('var') !!
                 $d.hash   ?? CgOp.newblankhash !!
                 $d.list   ?? CgOp.newblanklist !!
                 $d.typeconstraint ?? CgOp.newtypedscalar(CgOp.class_ref("mo", @($d.typeconstraint))) !!
                              CgOp.newblankrwscalar();
        push @scope, $dn, $nm;
        push @let, [$nm, ::Op::CgOp.new(op => $to)];
    }

    $nop = ::Op::LetScope.new(names => @scope, inner => $nop,
        transparent => $cbody.transparent);

    for @let {
        $nop = ::Op::Let.new(var => $_[0], to => $_[1], in => $nop);
    }

    for reverse @args -> $a {
        $nop = ::Op::Let.new(var => $a.[1], to => $a.[0], in => $nop);
    }

    if $cbody.topicalizer {
        $nop = ::Op::TopicalHook.new(inner => $nop);
    }

    $cbody.transparent ?? $nop !! ::Op::LeaveHook.new(inner => $nop);
}
