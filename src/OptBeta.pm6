class OptBeta;

use CgOp;

# A simple Perl6 compiler generates a lot of expressions of the form
# (-> $x { block })($y), due to control structures and regexes.  Try to clean
# that up here.

method make_call($var, *@params) {
    my $nonopt = ::Op::CallSub.new(
        positionals => [ @params ],
        invocant => ::Op::Lexical.new(name => $var));
    my @lex = $*CURLEX<!sub>.lookup_lex($var) or return $nonopt;
    return $nonopt unless @lex[0] eq 'sub' && @lex[4].is_inlinable;
    @lex[4].set_inlined;

    return ::Op::CgOp.new(optree => [ "_inline", @lex[4], @params ]);
}
