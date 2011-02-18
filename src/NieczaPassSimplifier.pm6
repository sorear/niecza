class NieczaPassSimplifier;

# This optimization pass handles lowering calls to well-known functions

method invoke($*unit) {
    # XXX enter and sigs need love
    $*unit.visit_local_subs_postorder(-> $su {
        $su.code = run_optree($su, $su.code, 1)
    });
    $*unit
}

sub no_named_params($op) {
    if defined $op.args {
        for @( $op.args ) -> $a {
            if $a.^isa(::Op::SimplePair) {
                return Any;
            } elsif $a.^isa(::Op::CallSub) && $a.invocant.^isa(::Op::Lexical)
                    && $a.invocant.name eq '&prefix:<|>' {
                return Any;
            }
        }
    }
    ($op.args // $op.positionals);
}

sub capture_params($op) {
    if !defined $op.args {
        return ($op.positionals);
    }

    my @named;
    my @pos;

    for @( $op.args ) -> $a {
        if $a.^isa(::Op::SimplePair) {
            push @named, $a.key => $a.value;
        } elsif $a.^isa(::Op::CallSub) && $a.invocant.^isa(::Op::Lexical)
                && $a.invocant.name eq '&prefix:<|>' {
            return Nil;
        } else {
            push @pos, $a;
        }
    }

    $(@pos), @named;
}

sub is_simple_var($op) {
    $op = $op.inner while $op.^isa(::Op::Paren);
    return Any unless $op.^isa(::Op::Lexical);
    return Any if $op.declaring || $op.state_backing;
    return $op.name;
}

our %funcs = (
    '&infix:<=>'           => &do_assign,
    '&postcircumfix:<{ }>' => &do_atkey,
    '&postcircumfix:<[ ]>' => &do_atpos,
    '&chars'               => do_builtin('chars', 1),
    '&defined'             => do_builtin('defined', 1),
    '&grep'                => &do_map_grep,
    '&infix:<eq>'          => do_builtin('streq', 2),
    '&infix:<ge>'          => do_builtin('strge', 2),
    '&infix:<gt>'          => do_builtin('strgt', 2),
    '&infix:<le>'          => do_builtin('strle', 2),
    '&infix:<lt>'          => do_builtin('strlt', 2),
    '&infix:<ne>'          => do_builtin('strne', 2),
    '&infix:</>'           => do_builtin('divide', 2),
    '&infix:<->'           => do_builtin('minus', 2),
    '&infix:<*>'           => do_builtin('mul', 2),
    '&infix:<%>'           => do_builtin('mod', 2),
    '&infix:<==>'          => do_builtin('numeq', 2),
    '&infix:<>=>'          => do_builtin('numge', 2),
    '&infix:<>>'           => do_builtin('numgt', 2),
    '&infix:<<=>'          => do_builtin('numle', 2),
    '&infix:<<>'           => do_builtin('numlt', 2),
    '&infix:<!=>'          => do_builtin('numne', 2),
    '&infix:<+>'           => do_builtin('plus', 2),
    '&last'                => do_nullary_control(2),
    '&make'                => do_builtin('make', 1),
    '&map'                 => &do_map_grep,
    '&next'                => do_nullary_control(1),
    '&not'                 => do_builtin('not', 1),
    '&postfix:<++>'        => do_builtin('postinc', 1),
    '&prefix:<?>'          => do_builtin('bool', 1),
    '&prefix:<->'          => do_builtin('negate', 1),
    '&prefix:<!>'          => do_builtin('not', 1),
    '&prefix:<+>'          => do_builtin('num', 1),
    '&prefix:<~>'          => do_builtin('str', 1),
    '&proceed'             => do_nullary_control(7),
    '&redo'                => do_nullary_control(3),
    '&return'              => &do_return_take,
    '&so'                  => do_builtin('bool', 1),
    '&substr'              => do_builtin('substr3', 3),
    '&succeed'             => do_nullary_control(6),
    '&take'                => &do_return_take,
    '&_array_constructor'  => do_builtin('array_constructor', 1),
);

sub do_assign($body, $nv, $invname, $op) {
    return $op unless defined my $args = no_named_params($op);
    return $op unless $args == 2;

    if (!$nv) {
        return ::Op::Assign.new(lhs => $args[0], rhs => $args[1]);
    } elsif (defined(my $name = is_simple_var($args[0]))) {
        return ::Op::StatementList.new(children => [
                ::Op::Assign.new(lhs => $args[0], rhs => $args[1]),
                ::Op::Lexical.new(name => $name)]);
    } else {
        my $id = ::GLOBAL::NieczaActions.gensym;
        return ::Op::Let.new(var => $id, to => $args[0], in =>
            ::Op::StatementList.new(children => [
                    ::Op::Assign.new(lhs => ::Op::LetVar.new(name => $id),
                        rhs => $args[1]),
                    ::Op::LetVar.new(name => $id)]));
    }
}

sub do_builtin($name, $expect) { sub ($body, $nv, $invname, $op) {
    return $op unless defined my $args = no_named_params($op);
    return $op unless $args == $expect;
    return ::Op::Builtin.new(name => $name, args => $args);
} }

sub do_map_grep($body, $nv, $invname, $op) {
    return $op unless defined my $args = no_named_params($op);
    return $op unless $args > 0;
    return ::Op::Builtin.new(name => substr($invname, 1), args => $args);
}

sub do_return_take($body, $nv, $invname, $op) {
    return $op unless defined my $args = no_named_params($op);
    my $parcel = ($args == 1 ?? $args[0] !!
        ::Op::CallSub.new(invocant => ::Op::Lexical.new(name => '&infix:<,>'),
            positionals => [@$args]));
    return ($invname eq '&take' ??
        ::Op::Take.new(value => $parcel) !!
        ::Op::Control.new(payload => $parcel, number => 4));
}

sub do_nullary_control($number) { sub ($body, $nv, $ , $op) {
    return $op unless defined my $args = no_named_params($op);
    return $op unless $args == 0;
    return ::Op::Control.new(:$number, payload => ::Op::Lexical.new(name => 'Nil'));
} }

sub do_atkey($body, $nv, $invname, $op) {
    my ($args, %named) = capture_params($op);
    return $op unless defined($args) && $args == 2;
    my $delete = %named<delete>:delete;
    my $exists = %named<exists>:delete;
    return $op if %named;
    return $op if $delete && (!$delete.^isa(::Op::Lexical) || $delete.name ne 'True');
    return $op if $exists && (!$exists.^isa(::Op::Lexical) || $exists.name ne 'True');
    return $op if $delete && $exists;
    return ::Op::Builtin.new(name => ($delete ?? 'delete_key' !!
            $exists ?? 'exists_key' !! 'at_key'), args => $args);
}

sub do_atpos($body, $nv, $invname, $op) {
    return $op unless defined my $args = no_named_params($op);
    return $op unless $args == 2;
    return ::Op::Builtin.new(name => 'at_pos', args => $args);
}

sub run_optree($body, $op, $nv) {
    die "WTF" if !defined $nv;
    my @kids := flat($op.ctxzyg($nv));
    my $i = 0;
    while $i < @kids {
        @kids[$i] = run_optree($body, @kids[$i], @kids[$i+1]);
        $i = $i + 2;
    }

    return $op unless $op.^isa(::Op::CallSub);
    my $inv = $op.invocant;
    return $op unless $inv.^isa(::Op::Lexical);
    my $invname = $inv.name;
    my $inv_lex = $body.find_lex($invname);
    return $op unless $inv_lex && $inv_lex.^isa(::Metamodel::Lexical::SubDef)
        && $inv_lex.body.unit.is_true_setting;
    return $op unless my $func = %funcs{$invname};

    $func($body, $nv, $invname, $op);
}
