module OpHelpers;

sub node($M) is export { { line => $M.cursor.lineof($M.to) } }

sub mklet($value, $body) is export {
    my $var = ::GLOBAL::NieczaActions.gensym;
    ::Op::Let.new(var => $var, to => $value,
        in => $body(::Op::LetVar.new(name => $var)));
}

sub mkcall($/, $name, *@positionals) is export {
    ::Op::CallSub.new(|node($/),
        invocant => ::Op::Lexical.new(|node($/), :$name), :@positionals);
}

sub mklex($/, $name) is export { ::Op::Lexical.new(|node($/), :$name); }

sub mkbool($i) is export { ::Op::Lexical.new(name => $i ?? 'True' !! 'False') }

sub mktemptopic($/, $item, $expr) is export {
    mklet(mklex($/, '$_'), -> $old_ {
        ::Op::StatementList.new(|node($/), children => [
            # XXX should be a raw bind
            ::Op::Bind.new(:!readonly, lhs => mklex($/, '$_'), rhs => $item),
            mklet($expr, -> $result {
                ::Op::StatementList.new(children => [
                    # XXX should be a raw bind
                    ::Op::Bind.new(:!readonly, lhs => mklex($/, '$_'),
                        rhs => $old_),
                    $result]) }) ]) });
}

