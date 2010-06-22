#| Base class for sprixel op nodes.  Op nodes represent Perl 6 in flight
#| between the compiler and the optimizer; they are somewhat lower level than
#| Perl 6 syntax and have very little context sensitivity, but also embody
#| little knowledge of the execution context.
class Sprixel::Meta::Op;

method compile() { ... }

#| Variable fetch op.  Returns the container, or something like that.  Can be
#| applied to any simple variable?
class Var is Op {
    has Str $.name; #= Name as suitable for L<Sprixel::Meta::Scope#lookup>
}

#| Simple function call.  The circularity saw forces us to have an op for this,
#| though I'd so dearly like to generate a call to postcircumfix:<( )> instead.
class FunCall is Op {
    has Op $.callee;
    has Op @.arguments;
}

#| Nothing to see here, just the words "hello world".
class StringLiteral is Op {
    has Str $.text;
}
