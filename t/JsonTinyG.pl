grammar JSONGrammar {
    rule TOP        {
        [
        | <?before '{'> :: <object>
        | <?before '['> :: <array>
        ]
        { say "parsed {$/.pos} chars" }
    }
    rule object     { '{' <pairlist> '}' }
    rule pairlist   { [ <pair> [ "," <pair> ]* ]?   }
    rule pair       { <string> ':' <value>     }
    rule array      { '[' [ <value> [ "," <value> ]* ]? ']' }

    proto token value {*}
    token value:sym<number> {
        '-'?
        [ 0 | <[1..9]> <[0..9]>* ]
        [ "." <[0..9]>+ ]?
        [ <[eE]> ["+"|"-"]? <[0..9]>+ ]?
    }
    token value:sym<true>    { <sym>    }
    token value:sym<false>   { <sym>    }
    token value:sym<null>    { <sym>    }
    token value:sym<object>  { <?before '{'> :: <object> }
    token value:sym<array>   { <?before '['> :: <array>  }
    token value:sym<string>  { <?before '"'> :: <string> }

    token string {
        '"' [ <str> || \\ <str_escape> ]* '"'
    }

    token str { <-[\t\n\\\"]>+ }

    token xdigit {
        <[0..9 a..f A..F]>
    }

    token str_escape {
        <["\\/bfnrt]> || u <xdigit> <xdigit> <xdigit> <xdigit>
    }
}

JSONGrammar.parse($*IN.slurp)

# vim: ft=perl6
