#| The main driver for the Sprixel system.  The guts of eval live here, as does
#| the command line processor and the REPL.
class Sprixel::Compiler;

use Sprixel::Meta::Op;

method test() {
    Block.new(
        :do(Sprixel::Meta::Op::FunCall.new(
            :callee( Sprixel::Meta::Op::Var.new(:name('&say')) ),
            :arguments( Sprixel::Meta::Op::StringLiteral.new( :text("Hello, world") ) )))).codegen;
}
