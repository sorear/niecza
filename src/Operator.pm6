# An Operator is fragment of code that can have operands plugged in;
# they abstract functions, macros, and some syntactic forms like
# method calls.
#
class Operator;

has $.whatever_curry;
has $.assignish;

method with_args ($/, *@_) { !!! }

sub node($M) { { line => $M.cursor.lineof($M.to) } }

sub mklet($value, $body) {
    my $var = ::GLOBAL::NieczaActions.gensym;
    ::Op::Let.new(var => $var, to => $value,
        in => $body(::Op::LetVar.new(name => $var)));
}

sub mkcall($/, $name, *@positionals) {
    ::Op::CallSub.new(|node($/),
        invocant => ::Op::Lexical.new(|node($/), :$name), :@positionals);
}

method whatever_curry() { False }
method assignish() { False }

method meta_assign() { ::Operator::CompoundAssign.new(base => self); }
method meta_not() { ::Operator::MetaNot.new(base => self); }

method funop($name, *@args) {
    ::Operator::Function.new(function => ::Op::Lexical.new(name => $name),
        args => @args)
}

class Function is Operator {
    has $.function; # Op; .++; use args for assuming-nature (but at end)
    has $.args = []; # Array of Op

    method with_args($/, *@args) {
        ::Op::CallSub.new(|node($/), invocant => $.function,
            positionals => [ @args, @$.args ])
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
}

class Method is Operator {
    has $.name; # Str | Op; .foo; use args, meta, private, path
    has $.args = [];
    has $.meta; # Bool
    has $.private; # Bool
    has $.path; # Array of Str

    method clone(*%_) {
        self.new(name => $!name, args => $!args, meta => $!meta,
            private => $!private, path => $!path, |%_);
    }

    method with_args($/, *@args) {
        if ($.name eq 'HOW' || $.name eq 'WHAT') && !$.private && !$.meta {
            if $.args {
                $/.CURSOR.sorry("Interrogative operator $.name does not take arguments");
                return ::Op::StatementList.new;
            }
            ::Op::Interrogative.new(|node($/), receiver => @args[0],
                name => $.name);
        } else {
            if defined($.path) && !$.private {
                $/.CURSOR.sorry("Qualified references to non-private methods NYI");
            }
            ::Op::CallMethod.new(|node($/),
                receiver => @args[0],
                ismeta   => $.meta,
                name     => $.name,
                private  => $.private,
                ppath    => $.path,
                args     => [ @$.args ]);
        }
    }
    method whatever_curry() { True }
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

    method with_args($/, *@args) {
        my @margs = @args;
        my $left = @margs[0];
        if $left.^isa(::Op::Lexical) {
            @margs[0] = ::Op::Lexical.new(|node($/), name => $left.name);
            mkcall($/, '&infix:<=>', $left, $.base.with_args($/, @margs));
        } else {
            mklet($left, -> $ll {
                @margs[0] = $ll;
                mkcall($/, '&infix:<=>', $ll, $.base.with_args($/, @margs)) });
        }
    }

    method assignish() { True }
}

class MetaNot is Operator {
    has $.base; # Operator

    method with_args($/, *@args) {
        mkcall($/, '&prefix:<!>', $.base.with_args($/, @args));
    }

    method whatever_curry() { True }
}

class Binding is Operator {
    has $.readonly; # Bool

    method with_args($/, *@args) {
        ::Op::Bind.new(|node($/), readonly => $.readonly, lhs => @args[0],
            rhs => @args[1]);
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
            $/.CURSOR.sorry('Non-contextual case of temp NYI');
            return ::Op::StatementList.new;
        }
        mkcall($/, '&infix:<=>',
            ::Op::Lexical.new(name => $rarg.name, declaring => True,
                        hash => substr($rarg.name,0,1) eq '%',
                        list => substr($rarg.name,0,1) eq '@'),
            ::Op::ContextVar.new(name => $rarg.name, uplevel => 1));
    }
}
