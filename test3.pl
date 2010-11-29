# vim: ft=perl6
use MONKEY_TYPING;

grammar G {
token ws {
    [
        | \h+ <![\#\s\\]> { }
        | <?before \w> <?after \w> :::
            { }
            <.sorry: "Whitespace is required between alphanumeric tokens">        # must \s+ between words
    ]
    ||
    [
    | <.unsp>
    | <.vws> <.heredoc>
    | <.unv>
    ]*
}
}
