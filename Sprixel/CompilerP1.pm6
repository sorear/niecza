#| The main driver for the P1 polymorph of VICIL.  This is a static compiler
#| which ties knots by statically generating metaobject tree initializers.
class Sprixel::CompilerP1;

use Sprixel::Meta::Op;

method test() {
    #1. Make a lexical alias of System::Console:from<clr>
    #2. Define the Parcel type (which can't be defined using Perl 6 subs, since
    #   perl 6 subs need it to work.  grr.)
    #3. Define the Sub type (doesn't need any methods, since we're static-
    #   compiling the delegates)
    #4. Define Str
    #5. Set up a callout for string literals
    #6. "Hello world".say
    Block.new(
        :do(Sprixel::Meta::Op::FunCall.new(
            :callee( Sprixel::Meta::Op::Var.new(:name('&say')) ),
            :arguments( Sprixel::Meta::Op::StringLiteral.new( :text("Hello, world") ) )))).codegen;
}
