our ($CgOp, $OpCallSub, $OpLexical, $OpCgOp, $OptBeta);

class OptBeta;

# A simple Perl6 compiler generates a lot of expressions of the form
# (-> $x { block })($y), due to control structures and regexes.  Try to clean
# that up here.

method make_call($var, *@params) {
    my $nonopt = $OpCallSub.new(
        positionals => [ @params ],
        invocant => $OpLexical.new(name => $var));
    my @lex = $*CURLEX<!sub>.lookup_lex($var) or return $nonopt;
    return $nonopt unless @lex[0] eq 'sub' && @lex[4].is_inlinable;
    @lex[4].set_inlined;

    return $OpCgOp.new(optree => [ "_inline", @lex[4], @params ]);
}

INIT { $OptBeta = OptBeta }
