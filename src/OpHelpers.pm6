our ($Actions, $OpLetVar, $OpLexical, $Op, $OpLet, $OpCallSub, $OpLexicalBind,
     $OpStatementList, $OpStringLiteral);

module OpHelpers;

sub mnode($M) is export {
    $M.^isa(Match) ??
        { file => $*FILE<name>, line => $M.CURSOR.lineof($M.from), pos => $M.from } !!
        { file => $*FILE<name>, line => $M.lineof($M.pos), pos => $M.pos }
}

sub mklet($value, $body) is export {
    my $var = $Actions.gensym;
    $OpLet.new(var => $var, to => $value,
        in => $body($OpLetVar.new(name => $var)));
}

sub mkcall($/, $name, *@positionals) is export {
    $/.CURSOR.mark_used($name);
    $*CURLEX<!sub>.noninlinable if $name eq '&EVAL'; # HACK
    $OpCallSub.new(pos=>$/,
        invocant => $OpLexical.new(pos=>$/, :$name), :@positionals);
}

sub mklex($/, $name, *%_) is export {
    $/.CURSOR.mark_used($name);
    $*CURLEX<!sub>.noninlinable if $name eq '&EVAL'; # HACK
    $OpLexical.new(pos=>$/, :$name, |%_);
}

sub mkbool($i) is export { $OpLexical.new(name => $i ?? 'True' !! 'False') }

sub mktemptopic($/, $item, $expr) is export {
    mklet(mklex($/, '$_'), -> $old_ {
        $OpStatementList.new(pos=>$/, children => [
            $OpLexicalBind.new(:name<$_>, rhs => $item),
            mklet($expr, -> $result {
                $OpStatementList.new(children => [
                    $OpLexicalBind.new(:name<$_>, rhs => $old_),
                    $result]) }) ]) });
}

sub mkstringycat($/, *@strings) is export {
    my @a;
    for @strings -> $s {
        my $i = ($s !~~ $Op) ?? $OpStringLiteral.new(pos=>$/,
            text => $s) !! $s;

        # this *might* belong in an optimization pass
        if @a && @a[*-1] ~~ $OpStringLiteral &&
                $i ~~ $OpStringLiteral {
            @a[*-1] = $OpStringLiteral.new(pos=>$/,
                text => (@a[*-1].text ~ $i.text));
        } else {
            push @a, $i;
        }
    }
    if @a == 0 {
        return $OpStringLiteral.new(pos=>$/, text => "");
    } elsif  @a == 1 {
        return (@a[0] ~~ $OpStringLiteral) ?? @a[0] !!
            mkcall($/, '&prefix:<~>', @a[0]);
    } else {
        return mkcall($/, '&infix:<~>', @a);
    }
}
