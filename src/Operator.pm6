# An Operator is fragment of code that can have operands plugged in;
# they abstract functions, macros, and some syntactic forms like
# method calls.
#
class Operator;

use Sig;
use OpHelpers;

method with_args ($/, *@_) { !!! }

method as_function($/) {
    $/.CURSOR.sorry("This macro cannot be used as a function");
    mklex($/, '&die');
}

method whatever_curry() { False }
method assignish() { False }

method meta_assign() { ::Operator::CompoundAssign.new(base => self); }
method meta_not() { ::Operator::MetaNot.new(base => self); }
method meta_fun($/, $fun, $arity, *@extra) {
    ::Operator::Function.new(function => mklex($/, $fun), :$arity,
        preargs => [ @extra, self.as_function($/) ])
}

method funop($/, $name, $arity, *@args) {
    ::Operator::Function.new(function => mklex($/, $name), :@args, :$arity)
}

method wrap_in_function($/) {
    my @args;
    my $i = -self.arity;
    while $i++ { push @args, ::GLOBAL::NieczaActions.gensym }
    my $do = self.with_args($/, map { mklex($/, $_) }, @args);
    ::GLOBAL::NieczaActions.block_expr($/,
        ::GLOBAL::NieczaActions.thunk_sub($do, params => @args));
}

class Function is Operator {
    has $.function; # Op; .++; use args for assuming-nature (but at end)
    has $.arity;
    constant $empty = [];
    has $.args = $empty; # Array of Op
    has $.preargs = $empty; # Array of Op

    method as_function($/) {
        if $.args || $.preargs {
            self.wrap_in_function($/)
        } else {
            $.function
        }
    }

    method with_args($/, *@args) {
        ::Op::CallSub.new(|node($/), invocant => $.function,
            positionals => [ @$.preargs, @args, @$.args ])
    }

    method !name() {
        $.function.^isa(::Op::Lexical) ?? $.function.name !! ""
    }

    method assignish() { self!name eq '&infix:<=>' }

    my %nowhatever = map { ($_ => True) }, '&infix:<..>', '&infix:<..^>',
        '&infix:<^..>', '&infix:<^..^>', '&infix:<...>', '&infix:<...^>',
        '&infix:<,>', '&infix:<=>', '&infix:<xx>';
    method whatever_curry() { !%nowhatever{self!name} }
}

class PostCall is Operator {
    # .(12); use args
    has $.args = [];

    method with_args($/, *@args) {
        ::Op::CallSub.new(|node($/),
            invocant => @args[0],
            args => [ @$.args ]);
    }

    method as_function($/) { self.wrap_in_function($/) }
    method arity() { 1 }
}

class Method is Operator {
    has $.name; # Str | Op; .foo; use args, meta, private, path
    has $.args = [];
    has $.meta; # Bool
    has $.private; # Bool
    has $.package; # Xref

    method clone(*%_) {
        self.new(name => $!name, args => $!args, meta => $!meta,
            private => $!private, package => $!package, |%_);
    }

    method as_function($/) { self.wrap_in_function($/) }
    method arity() { 1 }

    method with_args($/, *@args) {
        if $!name eq any(< HOW WHAT WHO VAR >) && !$!private && !$!meta {
            if $!args {
                $/.CURSOR.sorry("Interrogative operator $.name does not take arguments");
                return ::Op::StatementList.new;
            }
            ::Op::Interrogative.new(|node($/), receiver => @args[0],
                name => $.name);
        } else {
            $*CURLEX<!sub>.noninlinable if $!name eq 'eval';
            my $pclass;
            if $.private {
                if $.package {
                    $pclass = $.package;
                } elsif $*CURLEX<!sub>.in_class -> $c {
                    $pclass = $c;
                } else {
                    $/.CURSOR.sorry("Cannot resolve class for private method");
                }
            } else {
                $pclass = $.package;
            }
            ::Op::CallMethod.new(|node($/),
                receiver => @args[0],
                ismeta   => $.meta,
                name     => $.name,
                private  => $.private,
                pclass   => $pclass,
                args     => [ @$.args ]);
        }
    }
    method whatever_curry() { True }
}

class Operator::FlipFlop is Operator {
    has Bool $.excl_lhs;
    has Bool $.excl_rhs;
    has Bool $.sedlike;

    method with_args($/, *@args) {
        my $state_var = ::GLOBAL::NieczaActions.gensym;
        $*CURLEX<!sub>.add_state_name(Str, $state_var);
        @args[1] := mklex($/, 'False') if @args[1].^isa(::Op::Whatever);
        ::Op::FlipFlop.new(|node($/), :$state_var, :$!excl_lhs, :$!excl_rhs,
            :$!sedlike, :lhs(@args[0]), :rhs(@args[1]))
    }

    method whatever_curry() { False }
}

class ShortCircuit is Operator {
    has $.kind; # Str

    method with_args($/, *@args) {
        ::Op::ShortCircuit.new(|node($/), kind => $.kind, args => [ @args ])
    }

    method whatever_curry() { True }
}

class CompoundAssign is Operator {
    has $.base; # Operator

    method with_args($/, *@rest) {
        my $left = shift @rest;
        if $left.^isa(::Op::Lexical) {
            my $nlft = ::Op::Lexical.new(|node($/), name => $left.name);
            mkcall($/, '&infix:<=>', $left, $.base.with_args($/, $nlft, @rest));
        } else {
            mklet($left, -> $ll {
                mkcall($/, '&infix:<=>', $ll, $.base.with_args($/, $ll, @rest)) });
        }
    }

    method as_function($/) { self.wrap_in_function($/) }
    method arity() { $.base.arity }

    method assignish() { True }
}

class MetaNot is Operator {
    has $.base; # Operator

    method with_args($/, *@args) {
        mkcall($/, '&prefix:<!>', $.base.with_args($/, @args));
    }

    method as_function($/) { self.wrap_in_function($/) }
    method arity() { $.base.arity }


    method whatever_curry() { True }
}

class Binding is Operator {
    has $.readonly; # Bool

    method with_args($/, *@args) {
        @args[0].to_bind($/, ?$!readonly, @args[1]);
    }

    method assignish() { True }
}

class Comma is Operator {
    method with_args($/, *@args) {
        my @bits;
        for @args -> $a {
            push @bits, $a.^isa(::Op::SimpleParcel) ?? @( $a.items ) !! $a;
        }
        ::Op::SimpleParcel.new(|node($/), items => @bits);
    }
    method as_function($/) { mklex($/, '&infix:<,>') }
}

class Ternary is Operator {
    has $.middle; # Op
    method with_args($/, *@args) {
        ::Op::Conditional.new(|node($/), check => @args[0], true => $.middle,
            false => @args[1]);
    }
}

class Temp is Operator {
    method with_args($/, *@args) {
        my $rarg = @args[0];
        if !$rarg.^isa(::Op::ContextVar) || $rarg.uplevel {
            $*CURLEX<!sub>.noninlinable;
            return ::Op::Temporize.new(|node($/), mode => 0, var => $rarg);
        }
        my $hash = substr($rarg.name,0,1) eq '%';
        my $list = substr($rarg.name,0,1) eq '@';
        $*CURLEX<!sub>.add_my_name($rarg.name, :$hash, :$list);
        mkcall($/, '&infix:<=>',
            ::Op::Lexical.new(name => $rarg.name, :$hash, :$list),
            ::Op::ContextVar.new(name => $rarg.name, uplevel => 1));
    }
}

class Operator::Let is Operator {
    method with_args($/, *@args) {
        $*CURLEX<!sub>.noninlinable;
        return ::Op::Temporize.new(|node($/), mode => 1, var => @args[0]);
    }
}

class SmartMatch is Operator {
    method as_function($/) { mklex($/, '&infix:<~~>') }
    method with_args($/, *@args) {
        mktemptopic($/, @args[0], ::Op::CallMethod.new(receiver => @args[1],
            name => 'ACCEPTS', args => [ mklex($/, '$_') ]));
    }
}

class DotEq is Operator {
    method assignish() { True }
    method meta_assign() { die ".= may not be metaoperated" }
    method meta_not() { die ".= may not be metaoperated" }
    method with_args($/, *@args) {
        @args[1].meta_assign.with_args($/, @args[0]);
    }
}

class Operator::Replicate is Operator {
    method as_function($/) { mklex($/, '&infix:<xx>') }
    method with_args($/, *@args) {
        mkcall($/, '&_doreplicate', ::GLOBAL::NieczaActions.block_expr($/,
            ::GLOBAL::NieczaActions.thunk_sub(@args[0])), @args[1]);
    }
}

# A bit hackish; handles the macro aspects of $foo does Role(23)
class Operator::Mixin is Operator::Function {
    method with_args($/, *@args) {
        if @args[1] ~~ ::Op::CallSub {
            nextsame if @args[1].invocant ~~ ::Op::Lexical && @args[1].invocant.name eq '&_param_role_inst';
            $/.CURSOR.sorry("Can only provide exactly one initial value to a mixin") unless @args[1].getargs.elems == 1;
            ::Op::CallSub.new(|node($/), invocant => $.function,
                args => [@args[0], @args[1].invocant, ::Op::SimplePair.new(
                    key => 'value', value => @args[1].getargs[0] // mklex($/,'Nil'))]);
        } else {
            nextsame;
        }
    }
}
