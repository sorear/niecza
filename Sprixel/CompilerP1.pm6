#| The main driver for the P1 polymorph of VICIL.  This is a static compiler
#| which ties knots by statically generating metaobject tree initializers.
class Sprixel::CompilerP1;

use Sprixel::Meta::Op;

method test() {
    Block.new(
        :do(Sprixel::Meta::Op::FunCall.new(
            :callee( Sprixel::Meta::Op::Var.new(:name('&say')) ),
            :arguments( Sprixel::Meta::Op::StringLiteral.new( :text("Hello, world") ) )))).codegen;
}
