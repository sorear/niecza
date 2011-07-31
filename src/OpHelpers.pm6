module OpHelpers;

sub mnode($M) is export {
    $M.^isa(Match) ??
        { file => $*FILE<name>, line => $M.CURSOR.lineof($M.from), pos => $M.from } !!
        { file => $*FILE<name>, line => $M.lineof($M.pos), pos => $M.pos }
}

sub node($M) is export { { line => $M.CURSOR.lineof($M.pos) } }

sub mklet($value, $body) is export {
    my $var = ::GLOBAL::NieczaActions.gensym;
    ::Op::Let.new(var => $var, to => $value,
        in => $body(::Op::LetVar.new(name => $var)));
}

sub mkcall($/, $name, *@positionals) is export {
    $/.CURSOR.mark_used($name);
    $*CURLEX<!sub>.noninlinable if $name eq '&eval'; # HACK
    ::Op::CallSub.new(|node($/),
        invocant => ::Op::Lexical.new(|node($/), :$name), :@positionals);
}

sub mklex($/, $name, *%_) is export {
    $/.CURSOR.mark_used($name);
    $*CURLEX<!sub>.noninlinable if $name eq '&eval'; # HACK
    ::Op::Lexical.new(|node($/), :$name, |%_);
}

sub mkbool($i) is export { ::Op::Lexical.new(name => $i ?? 'True' !! 'False') }

sub mktemptopic($/, $item, $expr) is export {
    mklet(mklex($/, '$_'), -> $old_ {
        ::Op::StatementList.new(|node($/), children => [
            ::Op::LexicalBind.new(:name<$_>, rhs => $item),
            mklet($expr, -> $result {
                ::Op::StatementList.new(children => [
                    ::Op::LexicalBind.new(:name<$_>, rhs => $old_),
                    $result]) }) ]) });
}

sub mkstringycat($/, *@strings) is export {
    my @a;
    for @strings -> $s {
        my $i = ($s !~~ ::GLOBAL::Op) ?? ::Op::StringLiteral.new(|node($/),
            text => $s) !! $s;

        # this *might* belong in an optimization pass
        if @a && @a[*-1] ~~ ::Op::StringLiteral &&
                $i ~~ ::Op::StringLiteral {
            @a[*-1] = ::Op::StringLiteral.new(|node($/),
                text => (@a[*-1].text ~ $i.text));
        } else {
            push @a, $i;
        }
    }
    if @a == 0 {
        return ::Op::StringLiteral.new(|node($/), text => "");
    } elsif  @a == 1 {
        return (@a[0] ~~ ::Op::StringLiteral) ?? @a[0] !!
            mkcall($/, '&prefix:<~>', @a[0]);
    } else {
        return mkcall($/, '&infix:<~>', @a);
    }
}
