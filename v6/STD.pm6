# STD.pm
#
# Copyright 2007-2010, Larry Wall
#
# You may copy this software under the terms of the Artistic License,
#     version 2.0 or later.

grammar STD:ver<6.0.0.alpha>:auth<http://perl.org>;

use STDStash;
use NAME;

package DEBUG {
    our $autolexer = False;
    our $symtab = False;
    our $fixed_length = False;
    our $fates = False;
    our $longest_token_pattern_generation = False;
    our $EXPR = False;
    our $matchers = False;
    our $trace_call = False;
    our $cursors = False;
    our $try_processing = False;
    our $mixins = False;
    our $callm_show_subnames = False;
    our $use_color = False;

    our $RED = "\e[31m";
    our $CLEAR = "\e[37m";
}

our $ALL;

=begin comment

    Contextuals used in STD
    =======================
    # per parse
    my $*ACTIONS;         # class or object which defines reduce actions
    my $*SETTINGNAME;     # name of core setting
    my $*TMP_PREFIX;      # where to put tmp files
    my @*MEMOS;           # per-position info such as ws and line number
    my $*HIGHWATER;      # where we were last looking for things
    my $*HIGHMESS;       # current parse failure message
    my $*HIGHEXPECT;     # things we were looking for at the bleeding edge
    my $*IN_PANIC;       # don't panic recursively

    # symbol table management
    our $ALL;            # all the stashes, keyed by id
    my $*CORE;            # the CORE scope
    my $*SETTING;         # the SETTING scope
    my $*GLOBAL;          # the GLOBAL scope
    my $*PROCESS;         # the PROCESS scope
    my $*UNIT;            # the UNIT scope
    my $*CURLEX;          # current lexical scope info
    my $*CURPKG;          # current package scope

    my %*MYSTERY;     # names we assume may be post-declared functions

    # tree attributes, marked as propagating up (u) down (d) or up-and-down (u/d)
    my %*LANG;            # (d) braided languages: MAIN, Q, Regex, etc

    my $*IN_DECL;     # (d) a declarator is looking for a name to declare
    my $*SCOPE = "";      # (d) which scope declarator we're under
    my $*MULTINESS;       # (d) which multi declarator we're under
    my $*PKGDECL ::= "";         # (d) current package declarator
    my $*NEWPKG;      # (u/d) new package being declared
    my $*NEWLEX;      # (u/d) new lex info being declared
    my $*DECLARAND;   # (u/d) new object associated with declaration

    my $*GOAL ::= "(eof)";  # (d) which special terminator we're most wanting
    my $*IN_REDUCE;   # (d) attempting to parse an [op] construct
    my $*IN_META;     # (d) parsing a metaoperator like [..]
    my $*QUASIMODO;   # (d) don't carp about quasi variables
    my $*LEFTSIGIL;   # (u) sigil of LHS for item vs list assignment
    my $*QSIGIL;      # (d) sigil of current interpolation

    my $*INVOCANT_OK; # (d) parsing a list that allows an invocant
    my $*INVOCANT_IS; # (u) invocant of args match

    my $*BORG;            # (u/d) who to blame if we're missing a block

=end comment

=begin notes

    Some rules are named by syntactic category plus an additional symbol
    specified in adverbial form, either in bare :name form or in :sym<name>
    form.  (It does not matter which form you use for identifier symbols,
    except that to specify a symbol "sym" you must use the :sym<sym> form
    of adverb.)  If you use the <sym> rule within the rule, it will parse the
    symbol at that point.  At the final reduction point of a rule, if $sym
    has been set, that is used as the final symbol name for the rule.  This
    need not match the symbol specified as part the rule name; that is just
    for disambiguating the name.  However, if no $sym is set, the original
    symbol will be used by default.

    Note that some of these rules are written strangely because we're
    still bootstrapping via a preprocessor, gimme5.  For instance,
    blocks that contain nested braces are delimited by double braces
    so that the preprocessor does not need to parse Perl 6 code.

    This grammar relies on transitive longest-token semantics, though
    initially we made a feeble attempt to order rules so a procedural
    interpretation of alternation could usually produce a correct parse.
    (This is becoming less true over time.)

=end notes

method p6class () { ::STD::P6 }

method TOP ($STOP = '') {
    my $lang = self.cursor_fresh( self.p6class );

    if $STOP {
        my $*GOAL ::= $STOP;
        $lang.unitstop($STOP).comp_unit;
    }
    else {
        $lang.comp_unit;
    }
}

##############
# Precedence #
##############

# The internal precedence levels are *not* part of the public interface.
# The current values are mere implementation; they may change at any time.
# Users should specify precedence only in relation to existing levels.

constant %term            = (:dba('term')            , :prec<z=>);
constant %methodcall      = (:dba('methodcall')      , :prec<y=>, :assoc<unary>, :uassoc<left>, :fiddly, :!pure);
constant %autoincrement   = (:dba('autoincrement')   , :prec<x=>, :assoc<unary>, :uassoc<non>, :!pure);
constant %exponentiation  = (:dba('exponentiation')  , :prec<w=>, :assoc<right>, :pure);
constant %symbolic_unary  = (:dba('symbolic unary')  , :prec<v=>, :assoc<unary>, :uassoc<left>, :pure);
constant %multiplicative  = (:dba('multiplicative')  , :prec<u=>, :assoc<left>, :pure);
constant %additive        = (:dba('additive')        , :prec<t=>, :assoc<left>, :pure);
constant %replication     = (:dba('replication')     , :prec<s=>, :assoc<left>, :pure);
constant %concatenation   = (:dba('concatenation')   , :prec<r=>, :assoc<list>, :pure);
constant %junctive_and    = (:dba('junctive and')    , :prec<q=>, :assoc<list>, :pure);
constant %junctive_or     = (:dba('junctive or')     , :prec<p=>, :assoc<list>, :pure);
constant %named_unary     = (:dba('named unary')     , :prec<o=>, :assoc<unary>, :uassoc<left>, :pure);
constant %structural      = (:dba('structural infix'), :prec<n=>, :assoc<non>, :diffy);
constant %chaining        = (:dba('chaining')        , :prec<m=>, :assoc<chain>, :diffy, :iffy, :pure);
constant %tight_and       = (:dba('tight and')       , :prec<l=>, :assoc<list>);
constant %tight_or        = (:dba('tight or')        , :prec<k=>, :assoc<list>);
constant %conditional     = (:dba('conditional')     , :prec<j=>, :assoc<right>, :fiddly);
constant %item_assignment = (:dba('item assignment') , :prec<i=>, :assoc<right>, :!pure);
constant %list_assignment = (:dba('list assignment') , :prec<i=>, :assoc<right>, :sub<e=>, :fiddly, :!pure);
constant %loose_unary     = (:dba('loose unary')     , :prec<h=>, :assoc<unary>, :uassoc<left>, :pure);
constant %comma           = (:dba('comma')           , :prec<g=>, :assoc<list>, :nextterm<nulltermish>, :fiddly, :pure);
constant %list_infix      = (:dba('list infix')      , :prec<f=>, :assoc<list>, :pure);
constant %list_prefix     = (:dba('list prefix')     , :prec<e=>, :assoc<unary>, :uassoc<left>);
constant %loose_and       = (:dba('loose and')       , :prec<d=>, :assoc<list>);
constant %loose_or        = (:dba('loose or')        , :prec<c=>, :assoc<list>);
constant %sequencer       = (:dba('sequencer')       , :prec<b=>, :assoc<list>, :nextterm<statement>, :fiddly);
constant %LOOSEST         = (:dba('LOOSEST')         , :prec<a=!>);
constant %terminator      = (:dba('terminator')      , :prec<a=>, :assoc<list>);

# "epsilon" tighter than terminator
#constant $LOOSEST = %LOOSEST<prec>;
constant $LOOSEST = "a=!"; # XXX preceding line is busted
constant $item_assignment_prec = 'i=';
constant $methodcall_prec = 'y=';

##############
# Categories #
##############

# Categories are designed to be easily extensible in derived grammars
# by merely adding more rules in the same category.  The rules within
# a given category start with the category name followed by a differentiating
# adverbial qualifier to serve (along with the category) as the longer name.

# The endsym context, if specified, says what to implicitly check for in each
# rule right after the initial <sym>.  Normally this is used to make sure
# there's appropriate whitespace.  # Note that endsym isn't called if <sym>
# isn't called.

my $*endsym = "null";
my $*endargs = -1;

proto token category {*}

token category:category { <sym> }

token category:sigil { <sym> }
proto token sigil {*}

token category:twigil { <sym> }
proto token twigil {*}

token category:special_variable { <sym> }
proto token special_variable {*}

token category:comment { <sym> }
proto token comment {*}

token category:version { <sym> }
proto token version {*}

token category:module_name { <sym> }
proto token module_name {*}

token category:value { <sym> }
proto token value {*}

token category:term { <sym> }
proto token term {*}

token category:strtonum { <sym> }
proto token strtonum {*}

token category:quote { <sym> }
proto token quote {*}

token category:prefix { <sym> }
proto token prefix {*}

token category:infix { <sym> }
proto token infix {*}

token category:postfix { <sym> }
proto token postfix {*}

token category:dotty { <sym> }
proto token dotty {*}

token category:circumfix { <sym> }
proto token circumfix {*}

token category:postcircumfix { <sym> }
proto token postcircumfix {*}

token category:quote_mod { <sym> }
proto token quote_mod {*}

token category:trait_mod { <sym> }
proto token trait_mod {*}

token category:type_declarator { <sym> }
proto token type_declarator {*}

token category:scope_declarator { <sym> }
proto token scope_declarator {*}

token category:package_declarator { <sym> }
proto token package_declarator {*}

token category:multi_declarator { <sym> }
proto token multi_declarator {*}

token category:routine_declarator { <sym> }
proto token routine_declarator {*}

token category:regex_declarator { <sym> }
proto token regex_declarator {*}

token category:statement_prefix { <sym> }
proto rule  statement_prefix {*}

token category:statement_control { <sym> }
proto rule  statement_control {*}

token category:statement_mod_cond { <sym> }
proto rule  statement_mod_cond {*}

token category:statement_mod_loop { <sym> }
proto rule  statement_mod_loop {*}

token category:infix_prefix_meta_operator { <sym> }
proto token infix_prefix_meta_operator {*}

# NIECZA no support for protorx with arguments (since arguments defeat LTM,
# I have no clue what this should mean)
# token category:infix_postfix_meta_operator { <sym> }
# proto token infix_postfix_meta_operator ($op) {*}

token category:infix_circumfix_meta_operator { <sym> }
proto token infix_circumfix_meta_operator {*}

token category:postfix_prefix_meta_operator { <sym> }
proto token postfix_prefix_meta_operator {*}

token category:prefix_postfix_meta_operator { <sym> }
proto token prefix_postfix_meta_operator {*}

token category:prefix_circumfix_meta_operator { <sym> }
proto token prefix_circumfix_meta_operator {*}

token category:terminator { <sym> }
proto token terminator {*}

token unspacey { <.unsp>? }
token begid { <?before \w> }
token endid { <?before <-[ \- \' \w ]> > }
token spacey { <?before <[ \s \# ]> > }
token nofun { <!before '(' | '.(' | '\\' | '\'' | '-' | "'" | \w > }

# Note, don't reduce on a bare sigil unless you don't want a twigil or
# you otherwise don't care what the longest token is.

token sigil:sym<$>  { <sym> }
token sigil:sym<@>  { <sym> }
token sigil:sym<%>  { <sym> }
token sigil:sym<&>  { <sym> }

token twigil:sym<.> { <sym> <.begid> }
token twigil:sym<!> { <sym> <.begid> }
token twigil:sym<^> { <sym> <.begid> }
token twigil:sym<:> { <sym> <.begid> }
token twigil:sym<*> { <sym> <.begid> }
token twigil:sym<?> { <sym> <.begid> }
token twigil:sym<=> { <sym> <.begid> }
token twigil:sym<~> { <sym> <.begid> }

# overridden in subgrammars
token stopper { <!> }

# hopefully we can include these tokens in any outer LTM matcher
regex stdstopper {
    :temp $*STUB = return self if @*MEMOS[self.pos]<endstmt> :exists;
    :dba('standard stopper')
    [
    | <?terminator>
    | <?unitstopper>
    | <?before <stopper> >
    | $                                 # unlikely, check last (normal LTM behavior)
    ]
    { @*MEMOS[$¢.pos]<endstmt> = @*MEMOS[$¢.pos] || 1; }
}

token longname {
    <name> {} [ <?before ':' <[ a..z A..Z _ \< \[ \« ]>> <colonpair> ]*
}

token name {
    [
    | <identifier> <morename>*
    | <morename>+
    ]
}

token morename {
    :my $*QSIGIL ::= '';
    '::'
    [
    ||  <?before '(' | <alpha> >
        [
        | <identifier>
        | :dba('indirect name') '(' ~ ')' <EXPR>
        ]
    || <?before '::'> <.panic: "Name component may not be null">
    ]?
}

##############################
# Quote primitives           #
##############################

# XXX should eventually be derived from current Unicode tables.
constant %open2close = (
"\x0028" => "\x0029",
"\x003C" => "\x003E",
"\x005B" => "\x005D",
"\x007B" => "\x007D",
"\x00AB" => "\x00BB",
"\x0F3A" => "\x0F3B",
"\x0F3C" => "\x0F3D",
"\x169B" => "\x169C",
"\x2018" => "\x2019",
"\x201A" => "\x2019",
"\x201B" => "\x2019",
"\x201C" => "\x201D",
"\x201E" => "\x201D",
"\x201F" => "\x201D",
"\x2039" => "\x203A",
"\x2045" => "\x2046",
"\x207D" => "\x207E",
"\x208D" => "\x208E",
"\x2208" => "\x220B",
"\x2209" => "\x220C",
"\x220A" => "\x220D",
"\x2215" => "\x29F5",
"\x223C" => "\x223D",
"\x2243" => "\x22CD",
"\x2252" => "\x2253",
"\x2254" => "\x2255",
"\x2264" => "\x2265",
"\x2266" => "\x2267",
"\x2268" => "\x2269",
"\x226A" => "\x226B",
"\x226E" => "\x226F",
"\x2270" => "\x2271",
"\x2272" => "\x2273",
"\x2274" => "\x2275",
"\x2276" => "\x2277",
"\x2278" => "\x2279",
"\x227A" => "\x227B",
"\x227C" => "\x227D",
"\x227E" => "\x227F",
"\x2280" => "\x2281",
"\x2282" => "\x2283",
"\x2284" => "\x2285",
"\x2286" => "\x2287",
"\x2288" => "\x2289",
"\x228A" => "\x228B",
"\x228F" => "\x2290",
"\x2291" => "\x2292",
"\x2298" => "\x29B8",
"\x22A2" => "\x22A3",
"\x22A6" => "\x2ADE",
"\x22A8" => "\x2AE4",
"\x22A9" => "\x2AE3",
"\x22AB" => "\x2AE5",
"\x22B0" => "\x22B1",
"\x22B2" => "\x22B3",
"\x22B4" => "\x22B5",
"\x22B6" => "\x22B7",
"\x22C9" => "\x22CA",
"\x22CB" => "\x22CC",
"\x22D0" => "\x22D1",
"\x22D6" => "\x22D7",
"\x22D8" => "\x22D9",
"\x22DA" => "\x22DB",
"\x22DC" => "\x22DD",
"\x22DE" => "\x22DF",
"\x22E0" => "\x22E1",
"\x22E2" => "\x22E3",
"\x22E4" => "\x22E5",
"\x22E6" => "\x22E7",
"\x22E8" => "\x22E9",
"\x22EA" => "\x22EB",
"\x22EC" => "\x22ED",
"\x22F0" => "\x22F1",
"\x22F2" => "\x22FA",
"\x22F3" => "\x22FB",
"\x22F4" => "\x22FC",
"\x22F6" => "\x22FD",
"\x22F7" => "\x22FE",
"\x2308" => "\x2309",
"\x230A" => "\x230B",
"\x2329" => "\x232A",
"\x23B4" => "\x23B5",
"\x2768" => "\x2769",
"\x276A" => "\x276B",
"\x276C" => "\x276D",
"\x276E" => "\x276F",
"\x2770" => "\x2771",
"\x2772" => "\x2773",
"\x2774" => "\x2775",
"\x27C3" => "\x27C4",
"\x27C5" => "\x27C6",
"\x27D5" => "\x27D6",
"\x27DD" => "\x27DE",
"\x27E2" => "\x27E3",
"\x27E4" => "\x27E5",
"\x27E6" => "\x27E7",
"\x27E8" => "\x27E9",
"\x27EA" => "\x27EB",
"\x2983" => "\x2984",
"\x2985" => "\x2986",
"\x2987" => "\x2988",
"\x2989" => "\x298A",
"\x298B" => "\x298C",
"\x298D" => "\x298E",
"\x298F" => "\x2990",
"\x2991" => "\x2992",
"\x2993" => "\x2994",
"\x2995" => "\x2996",
"\x2997" => "\x2998",
"\x29C0" => "\x29C1",
"\x29C4" => "\x29C5",
"\x29CF" => "\x29D0",
"\x29D1" => "\x29D2",
"\x29D4" => "\x29D5",
"\x29D8" => "\x29D9",
"\x29DA" => "\x29DB",
"\x29F8" => "\x29F9",
"\x29FC" => "\x29FD",
"\x2A2B" => "\x2A2C",
"\x2A2D" => "\x2A2E",
"\x2A34" => "\x2A35",
"\x2A3C" => "\x2A3D",
"\x2A64" => "\x2A65",
"\x2A79" => "\x2A7A",
"\x2A7D" => "\x2A7E",
"\x2A7F" => "\x2A80",
"\x2A81" => "\x2A82",
"\x2A83" => "\x2A84",
"\x2A8B" => "\x2A8C",
"\x2A91" => "\x2A92",
"\x2A93" => "\x2A94",
"\x2A95" => "\x2A96",
"\x2A97" => "\x2A98",
"\x2A99" => "\x2A9A",
"\x2A9B" => "\x2A9C",
"\x2AA1" => "\x2AA2",
"\x2AA6" => "\x2AA7",
"\x2AA8" => "\x2AA9",
"\x2AAA" => "\x2AAB",
"\x2AAC" => "\x2AAD",
"\x2AAF" => "\x2AB0",
"\x2AB3" => "\x2AB4",
"\x2ABB" => "\x2ABC",
"\x2ABD" => "\x2ABE",
"\x2ABF" => "\x2AC0",
"\x2AC1" => "\x2AC2",
"\x2AC3" => "\x2AC4",
"\x2AC5" => "\x2AC6",
"\x2ACD" => "\x2ACE",
"\x2ACF" => "\x2AD0",
"\x2AD1" => "\x2AD2",
"\x2AD3" => "\x2AD4",
"\x2AD5" => "\x2AD6",
"\x2AEC" => "\x2AED",
"\x2AF7" => "\x2AF8",
"\x2AF9" => "\x2AFA",
"\x2E02" => "\x2E03",
"\x2E04" => "\x2E05",
"\x2E09" => "\x2E0A",
"\x2E0C" => "\x2E0D",
"\x2E1C" => "\x2E1D",
"\x2E20" => "\x2E21",
"\x3008" => "\x3009",
"\x300A" => "\x300B",
"\x300C" => "\x300D",
"\x300E" => "\x300F",
"\x3010" => "\x3011",
"\x3014" => "\x3015",
"\x3016" => "\x3017",
"\x3018" => "\x3019",
"\x301A" => "\x301B",
"\x301D" => "\x301E",
"\xFD3E" => "\xFD3F",
"\xFE17" => "\xFE18",
"\xFE35" => "\xFE36",
"\xFE37" => "\xFE38",
"\xFE39" => "\xFE3A",
"\xFE3B" => "\xFE3C",
"\xFE3D" => "\xFE3E",
"\xFE3F" => "\xFE40",
"\xFE41" => "\xFE42",
"\xFE43" => "\xFE44",
"\xFE47" => "\xFE48",
"\xFE59" => "\xFE5A",
"\xFE5B" => "\xFE5C",
"\xFE5D" => "\xFE5E",
"\xFF08" => "\xFF09",
"\xFF1C" => "\xFF1E",
"\xFF3B" => "\xFF3D",
"\xFF5B" => "\xFF5D",
"\xFF5F" => "\xFF60",
"\xFF62" => "\xFF63",
);

constant %close2open = invert %open2close;

# assumes whitespace is eaten already

method peek_delimiters {
    my $pos = self.pos;
    my $startpos = $pos;
    my $char = substr(self.orig,$pos++,1);
    if $char ~~ /^\s$/ {
        self.panic("Whitespace character is not allowed as delimiter"); # "can't happen"
    }
    elsif $char ~~ /^\w$/ {
        self.panic("Alphanumeric character is not allowed as delimiter");
    }
    elsif %close2open{$char} {
        self.panic("Use of a closing delimiter for an opener is reserved");
    }
    elsif $char eq ':' {
        self.panic("Colons may not be used to delimit quoting constructs");
    }

    my $rightbrack = %open2close{$char};
    if not defined $rightbrack {
        return $char, $char;
    }
    while substr(self.orig,$pos,1) eq $char {
        $pos++;
    }
    my $len = $pos - $startpos;
    my $start = $char x $len;
    my $stop = $rightbrack x $len;
    return $start, $stop;
}

role startstop[$start,$stop] {
    token starter { $start }
    token stopper { $stop }
} # end role

role stop[$stop] {
    token starter { <!> }
    token stopper { $stop }
} # end role

role unitstop[$stop] {
    token unitstopper { $stop }
} # end role

token unitstopper { $ }

method balanced ($start,$stop) { self.mixin( STD::startstop[$start,$stop] ); }
method unbalanced ($stop) { self.mixin( STD::stop[$stop] ); }
method unitstop ($stop) { self.mixin( STD::unitstop[$stop] ); }

method truly ($bool,$opt) {
    return self if $bool;
    self.sorry("Can't negate $opt adverb");
    self;
}

token charname {
    [
    | <radint>
    | <alpha> .*? <?before \s*[ ',' | '#' | ']']>
    ] || <.sorry: "Unrecognized character name"> .*?<?terminator>
}

token charnames { \s* [<charname><.ws>] ** [','\s*] }

token charspec {
    [
    | :dba('character name') '[' ~ ']' <charnames>
    | \d+
    | <[ ?..Z \\.._ ]>
    | <?> <.sorry: "Unrecognized \\c character"> .
    ]
}

proto token backslash {*}
proto token escape {*}
token starter { <!> }
token escape:none { <!> }

# and this is what makes nibbler polymorphic...
method nibble ($lang) {
    self.cursor_fresh($lang).nibbler;
}

# note: polymorphic over many quote languages, we hope
token nibbler {
    :my $text = '';
    :my $from = self.pos;
    :my $to = $from;
    :my @nibbles = ();
    :my $multiline = 0;
    # { self.from = self.pos; } NIECZA
    [ <!before <stopper> >
        [
        || <starter> <nibbler> <stopper>
                        {
                            push @nibbles, $¢.makestr(TEXT => $text, _from => $from, _pos => $to ) if $from != $to;

                            my $n = $<nibbler>[*-1]<nibbles>;
                            my @n = @$n;

                            push @nibbles, $<starter>;
                            push @nibbles, @n;
                            push @nibbles, $<stopper>;

                            $text = '';
                            $to = $from = $¢.pos;
                        }
        || <escape>     {
                            push @nibbles, $¢.makestr(TEXT => $text, _from => $from, _pos => $to ) if $from != $to;
                            push @nibbles, $<escape>[*-1];
                            $text = '';
                            $to = $from = $¢.pos;
                        }
        || .
                        {{
                            my $ch = substr(self.orig, $¢.pos-1, 1);
                            $text ~= $ch;
                            $to = $¢.pos;
                            if $ch ~~ "\n" {
                                $multiline++;
                            }
                        }}
        ]
    ]*
    {
        push @nibbles, $¢.makestr(TEXT => $text, _from => $from, _pos => $to ) if $from != $to or !@nibbles;
        $<nibbles> = @nibbles;
        $.pos = $¢.pos;
        $<nibbler> :delete;
        $<escape> :delete;
        $<starter> :delete;
        $<stopper> :delete;
        $*LAST_NIBBLE = $¢;
        $*LAST_NIBBLE_MULTILINE = $¢ if $multiline;
    }
}

token babble ($l) {
    :my $lang = $l;
    :my $start;
    :my $stop;

    <.ws>
    [ <quotepair> <.ws>
        {
            my $kv = $<quotepair>[*-1];
            $lang = ($lang.tweak(| ($kv.<k> => $kv.<v>))
                or $lang.sorry("Unrecognized adverb :" ~ $kv.<k> ~ '(' ~ $kv.<v> ~ ')'));
        }
    ]*

    {
        ($start,$stop) = $¢.peek_delimiters();
        $lang = $start ne $stop ?? $lang.balanced($start,$stop)
                                !! $lang.unbalanced($stop);
        $<B> = [$lang,$start,$stop];
    }
}

our @herestub_queue;

class Herestub {
    has $.delim;
    has $.orignode;
    has $.lang;

    method new(:$delim, :$orignode, :$lang) {
        my $new = self.CREATE;
        $new!delim = $delim;
        $new!orignode = $orignode;
        $new!lang = $lang;
        $new;
    }
}

role herestop {
    token stopper { ^^ {} $<ws>=(\h*?) $*DELIM \h* <.unv>?? $$ \v? }
}

# XXX be sure to temporize @herestub_queue on reentry to new line of heredocs

method heredoc () {
    my $here = self;
    while my $herestub = shift @herestub_queue {
        my $*DELIM = $herestub.delim;
        my $lang = $herestub.lang.mixin( ::herestop );
        my $doc;
        if ($doc) = $here.nibble($lang) {
            $here = $doc.trim_heredoc();
            $herestub.orignode<doc> = $doc;
        }
        else {
            self.panic("Ending delimiter $*DELIM not found");
        }
    }
    return self.cursor($here.pos);  # return to initial type
}

token quibble ($l) {
    :my ($lang, $start, $stop);
    <babble($l)>
    { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }

    $start <nibble($lang)> [ $stop || <.panic: "Couldn't find terminator $stop"> ]

    {
        if $lang<_herelang> {
            push @herestub_queue,
                Herestub.new(
                    delim => $<nibble><nibbles>[0]<TEXT>,
                    orignode => $¢,
                    lang => $lang<_herelang>,
                );
        }
    }
}

token quotepair {
    :my $key;
    :my $value;

    ':'
    :dba('colon pair (restricted)')
    [
    | '!' <identifier> [ <?before '('> <.sorry: "Argument not allowed on negated pair"> <circumfix> ]?
        { $key = $<identifier>.Str; $value = 0; }
    | <identifier>
        { $key = $<identifier>.Str; }
        [
        || <.unsp>? <?before '('> <circumfix> { $value = $<circumfix>; }
        || { $value = 1; }
        ]
    | $<n>=(\d+) $<id>=(<[a..z]>+) [ <?before '('> <.sorry: "2nd argument not allowed on pair"> <circumfix> ]?
        { $key = $<id>.Str; $value = $<n>.Str; }
    ]
    { $<k> = $key; $<v> = $value; }
}

token quote:sym<' '>   { :dba('single quotes') "'" ~ "'" <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q).unbalanced("'"))> }
token quote:sym<" ">   { :dba('double quotes') '"' ~ '"' <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).unbalanced('"'))> }

token circumfix:sym<« »>   { :dba('shell-quote words') '«' ~ '»' <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).tweak(:ww).balanced('«','»'))> }
token circumfix:sym«<< >>» { :dba('shell-quote words') '<<' ~ '>>' <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).tweak(:ww).balanced('<<','>>'))> }
token circumfix:sym«< >»   { :dba('quote words') '<' ~ '>'
    [
        [ <?before 'STDIN>' > <.obs('<STDIN>', '$' ~ '*IN.lines')> ]?  # XXX fake out gimme5
        [ <?before '>' > <.obs('<>', "lines() to read input,\n  or ('') to represent the null string,\n  or () to represent Nil")> ]?
        <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q).tweak(:w).balanced('<','>'))>
    ]
}

##################
# Lexer routines #
##################

token ws {
    :temp $*STUB = return self if @*MEMOS[self.pos]<ws> :exists;
    :my $startpos = self.pos;
    :my $*HIGHEXPECT = {};

    :dba('whitespace')
    [
        | \h+ <![\#\s\\]> { @*MEMOS[$¢.pos]<ws> = $startpos; }   # common case
        | <?before \w> <?after \w> :::
            { @*MEMOS[$startpos]<ws>:delete; }
            <.sorry: "Whitespace is required between alphanumeric tokens">        # must \s+ between words
    ]
    ||
    [
    | <.unsp>
    | <.vws> <.heredoc>
    | <.unv>
    # | $ { $¢.moreinput }  NIECZA break inf loop
    ]*

    {
        if ($¢.pos == $startpos) {
            @*MEMOS[$¢.pos]<ws>:delete;
        }
        else {
            @*MEMOS[$¢.pos]<ws> = $startpos;
            @*MEMOS[$¢.pos]<endstmt> = @*MEMOS[$startpos]<endstmt>
                if @*MEMOS[$startpos]<endstmt> :exists;
        }
    }
}

token unsp {
    \\ <?before [\s|'#'] >
    :dba('unspace')
    [
    | <.vws>
    | <.unv>
    # | $ { $¢.moreinput }  NIECZA break inf loop
    ]*
}

token vws {
    :dba('vertical whitespace')
    [
        [
        | \v
        # | '#DEBUG -1' { say "DEBUG"; $*DEBUG = -1; } \V* \v
        | '<<<<<<<' :: <?before [.*? \v '=======']: .*? \v '>>>>>>>' > <.sorry: 'Found a version control conflict marker'> \V* \v
        | '=======' :: .*? \v '>>>>>>>' \V* \v   # ignore second half
        ]
    ]+
}

# We provide two mechanisms here:
# 1) define $*moreinput, or
# 2) override moreinput method
method moreinput () {
    $*moreinput.() if $*moreinput;
    self;
}

token unv {
   :dba('horizontal whitespace')
   [
   | \h+
   | <?before \h* '=' [ \w | '\\'] > ^^ <.pod_comment>
   | \h* <comment>
   ]
}

token comment:sym<#`(...)> {
    '#`' :: [ <?opener> || <.panic: "Opening bracket is required for #` comment"> ]
    <.quibble($¢.cursor_fresh( %*LANG<Q> ))>
}

token comment:sym<#(...)> {
    '#' <?opener>
    <.suppose
        <quibble($¢.cursor_fresh( %*LANG<Q> ))>
        <!before <[,;:]>* \h* [ '#' | $$ ] >   # extra stuff on line after closer?
    >
    <.worry: "Embedded comment seems to be missing backtick"> <!>
}

token comment:sym<#=(...)> {
    '#=' <?opener> ::
    <quibble($¢.cursor_fresh( %*LANG<Q> ))>
}

token comment:sym<#=> {
   '#=' :: $<attachment> = [\N*]
}

token comment:sym<#> {
   '#' {} \N*
}

token ident {
    <.alpha> \w*
}

token apostrophe {
    <[ ' \- ]>
}

token identifier {
    <.ident> [ <.apostrophe> <.ident> ]*
}

# XXX We need to parse the pod eventually to support $= variables.

token pod_comment {
    ^^ \h* '=' <.unsp>?
    [
    | 'begin' \h+ <identifier> ::
        [
        || .*? "\n" [ :r \h* '=' <.unsp>? 'end' \h+ $<identifier> » \N* ]
        || <?{ $<identifier>.Str eq 'END'}> .*
        || { my $id = $<identifier>.Str; self.panic("=begin $id without matching =end $id"); }
        ]
    | 'begin' » :: \h* [ $$ || '#' || <.sorry: "Unrecognized token after =begin"> \N* ]
        [ .*? "\n" \h* '=' <.unsp>? 'end' » \N* || { self.panic("=begin without matching =end"); } ]
        
    | 'for' » :: \h* [ <identifier> || $$ || '#' || <.sorry: "Unrecognized token after =for"> \N* ]
        [.*?  ^^ \h* $$ || .*]
    | :: 
        [ <?before .*? ^^ '=cut' » > <.panic: "Obsolescent pod format, please use =begin/=end instead"> ]?
        [<alpha>||\s||<.sorry: "Illegal pod directive">]
        \N*
    ]
}

# suppress fancy end-of-line checking
token embeddedblock {
    # encapsulate braided languages
    :temp %*LANG;
    :my $*SIGNUM;
    :my $*GOAL ::= '}';
    :temp $*CURLEX;

    :dba('embedded block')

    <.newlex>
    <.finishlex>
    '{' :: [ :lang(%*LANG<MAIN>) <statementlist> ]
    [ '}' || <.panic: "Unable to parse statement list; couldn't find right brace"> ]
}

token binints { [<.ws><binint><.ws>] ** ',' }

token binint {
    <[ 0..1 ]>+ [ _ <[ 0..1 ]>+ ]*
}

token octints { [<.ws><octint><.ws>] ** ',' }

token octint {
    <[ 0..7 ]>+ [ _ <[ 0..7 ]>+ ]*
}

token hexints { [<.ws><hexint><.ws>] ** ',' }

token hexint {
    <[ 0..9 a..f A..F ]>+ [ _ <[ 0..9 a..f A..F ]>+ ]*
}

token decints { [<.ws><decint><.ws>] ** ',' }

token decint {
    \d+ [ _ \d+ ]*
}

token integer {
    [
    | 0 [ b '_'? <binint>
        | o '_'? <octint>
        | x '_'? <hexint>
        | d '_'? <decint>
        | <decint>
            <!!{ $¢.worry("Leading 0 does not indicate octal in Perl 6; please use 0o" ~ $<decint>.Str ~ " if you mean that") }>
        ]
    | <decint>
    ]
    <!!before ['.' <?before \s | ',' | '=' | <terminator> > <.sorry: "Decimal point must be followed by digit">]? >
    [ <?before '_' '_'+\d> <.sorry: "Only isolated underscores are allowed inside numbers"> ]?
}

token radint {
    [
    | <integer>
    | <?before ':'\d> <rad_number> <?{
                        defined $<rad_number><intpart>
                        and
                        not defined $<rad_number><fracpart>
                    }>
    ]
}

token escale {
    <[Ee]> <[+\-]>? <decint>
}

# careful to distinguish from both integer and 42.method
token dec_number {
    :dba('decimal number')
    [
    | $<coeff> = [              '.' <frac=.decint> ] <escale>?
    | $<coeff> = [<int=.decint> '.' <frac=.decint> ] <escale>?
    | $<coeff> = [<int=.decint>                    ] <escale>
    ]
    [ <?before '.' \d> <.sorry: "Number contains two decimal points (missing 'v' for version number?)"> ['.'\d+]+ ]?
    [ <?before '_' '_'+\d> <.sorry: "Only isolated underscores are allowed inside numbers"> ]?
}

token alnumint {
    [ <[ 0..9 a..z A..Z ]>+ [ _ <[ 0..9 a..z A..Z ]>+ ]* ]
}

token rad_number {
    ':' $<radix> = [\d+] <.unsp>?      # XXX optional dot here?
    {}           # don't recurse in lexer
    :dba('number in radix notation')
    [
    || '<'
            [
            | $<coeff> = [                '.' <frac=.alnumint> ]
            | $<coeff> = [<int=.alnumint> '.' <frac=.alnumint> ]
            | $<coeff> = [<int=.alnumint>                    ]
            ]
            [
                '*' <base=.radint>
                [ '**' <exp=.radint> || <.sorry: "Base is missing ** exponent part"> ]
            ]?
       '>'
#      { make radcalc($<radix>, $<coeff>, $<base>, $<exp>) }
    || <?before '['> <circumfix>
    || <?before '('> <circumfix>
    || <.panic: "Malformed radix number">
    ]
}

token terminator:sym<)>
    { <sym> <O(|%terminator)> }

token terminator:sym<]>
    { ']' <O(|%terminator)> }

token terminator:sym<}>
    { '}' <O(|%terminator)> }


token opener {
  <[
\x0028 \x003C \x005B \x007B \x00AB \x0F3A \x0F3C \x169B \x2018 \x201A \x201B
\x201C \x201E \x201F \x2039 \x2045 \x207D \x208D \x2208 \x2209 \x220A \x2215
\x223C \x2243 \x2252 \x2254 \x2264 \x2266 \x2268 \x226A \x226E \x2270 \x2272
\x2274 \x2276 \x2278 \x227A \x227C \x227E \x2280 \x2282 \x2284 \x2286 \x2288
\x228A \x228F \x2291 \x2298 \x22A2 \x22A6 \x22A8 \x22A9 \x22AB \x22B0 \x22B2
\x22B4 \x22B6 \x22C9 \x22CB \x22D0 \x22D6 \x22D8 \x22DA \x22DC \x22DE \x22E0
\x22E2 \x22E4 \x22E6 \x22E8 \x22EA \x22EC \x22F0 \x22F2 \x22F3 \x22F4 \x22F6
\x22F7 \x2308 \x230A \x2329 \x23B4 \x2768 \x276A \x276C \x276E \x2770 \x2772
\x2774 \x27C3 \x27C5 \x27D5 \x27DD \x27E2 \x27E4 \x27E6 \x27E8 \x27EA \x2983
\x2985 \x2987 \x2989 \x298B \x298D \x298F \x2991 \x2993 \x2995 \x2997 \x29C0
\x29C4 \x29CF \x29D1 \x29D4 \x29D8 \x29DA \x29F8 \x29FC \x2A2B \x2A2D \x2A34
\x2A3C \x2A64 \x2A79 \x2A7D \x2A7F \x2A81 \x2A83 \x2A8B \x2A91 \x2A93 \x2A95
\x2A97 \x2A99 \x2A9B \x2AA1 \x2AA6 \x2AA8 \x2AAA \x2AAC \x2AAF \x2AB3 \x2ABB
\x2ABD \x2ABF \x2AC1 \x2AC3 \x2AC5 \x2ACD \x2ACF \x2AD1 \x2AD3 \x2AD5 \x2AEC
\x2AF7 \x2AF9 \x2E02 \x2E04 \x2E09 \x2E0C \x2E1C \x2E20 \x3008 \x300A \x300C
\x300E \x3010 \x3014 \x3016 \x3018 \x301A \x301D \xFD3E \xFE17 \xFE35 \xFE37
\xFE39 \xFE3B \xFE3D \xFE3F \xFE41 \xFE43 \xFE47 \xFE59 \xFE5B \xFE5D \xFF08
\xFF1C \xFF3B \xFF5B \xFF5F \xFF62
  ]>
}

grammar P6 is STD {

    ###################
    # Top-level rules #
    ###################

    # Note: we only check for the stopper.  We don't check for ^ because
    # we might be embedded in something else.
    rule comp_unit {
        :my $*begin_compunit = 1;
        :my $*endargs = -1;
        :my %*LANG;
        :my $*PKGDECL ::= "";
        :my $*IN_DECL = '';
        :my $*DECLARAND;
        :my $*OFTYPE;
        :my $*NEWPKG;
        :my $*NEWLEX;
        :my $*QSIGIL ::= '';
        :my $*IN_META = '';
        :my $*QUASIMODO;
        :my $*SCOPE = "";
        :my $*LEFTSIGIL;
        :my $*PRECLIM;
        :my %*MYSTERY = ();
        :my $*INVOCANT_OK;
        :my $*INVOCANT_IS;
        :my $*CURLEX;
        :my $*MULTINESS = '';
        :my $*SIGNUM = 0;
        :my $*MONKEY_TYPING = False;
        :my %*WORRIES;
        :my @*WORRIES;
        :my $*FATALS = 0;
        :my $*IN_SUPPOSE = False;

        :my $*CURPKG;
        {{

            %*LANG<MAIN>    = ::STD::P6 ;
            %*LANG<Q>       = ::STD::Q ;
            %*LANG<Quasi>   = ::STD::Quasi ;
            %*LANG<Regex>   = ::STD::Regex ;
            %*LANG<P5>      = ::STD::P5 ;
            %*LANG<P5Regex> = ::STD::P5::Regex ;

            @*WORRIES = ();
            self.load_setting($*SETTINGNAME);
            my $oid = $*SETTING.id;
            my $id = 'MY:file<' ~ $*FILE<name> ~ '>';
            $*CURLEX = STDStash.new(
                'OUTER::' => [$oid],
                '!file' => $*FILE, '!line' => 0,
                '!id' => [$id],
            );
            $ALL.{$id} = $*CURLEX;
            $*UNIT = $*CURLEX;
            $ALL.<UNIT> = $*UNIT;
            self.finishlex;
            # $¢ = self.cursor_fresh($*CURLEX<$?LANGNAME>);
        }}
        <.unitstart>
        <statementlist>
        [ <?unitstopper> || <.panic: "Confused"> ]
        # "CHECK" time...
        {
            $¢.explain_mystery();
            $¢.<LEX> = $*CURLEX;
            if @*WORRIES {
                note "Potential difficulties:\n  " ~ join( "\n  ", @*WORRIES) ~ "\n";
            }
            die "Check failed\n" if $*FATALS;
        }
    }

    # Note: because of the possibility of placeholders we can't determine arity of
    # the block syntactically, so this must be determined via semantic analysis.
    # Also, pblocks used in an if/unless statement do not treat $_ as a placeholder,
    # while most other blocks treat $_ as equivalent to $^x.  Therefore the first
    # possible place to check arity is not here but in the rule that calls this
    # rule.  (Could also be done in a later pass.)

    token pblock () {
        :temp $*CURLEX;
        :dba('parameterized block')
        [<?before <.lambda> | '{' > ||
            {{
                if $*BORG and $*BORG.<block> {
                    if $*BORG.<name> {
                        my $m = "Function '" ~ $*BORG.<name> ~ "' needs parens to avoid gobbling block" ~ $*BORG.<culprit>.locmess;
                        $*BORG.<block>.panic($m ~ "\nMissing block (apparently gobbled by '" ~ $*BORG.<name> ~ "')");
                    }
                    else {
                        my $m = "Expression needs parens to avoid gobbling block" ~ $*BORG.<culprit>.locmess;
                        $*BORG.<block>.panic($m ~ "\nMissing block (apparently gobbled by expression)");
                    }
                }
                elsif %*MYSTERY {
                    $¢.panic("Missing block (apparently gobbled by undeclared routine?)");
                }
                else {
                    $¢.panic("Missing block");
                }
            }}
        ]
        [
        | <lambda>
            <.newlex(1)>
            <signature(1)>
            <blockoid>
            <.getsig>
        | <?before '{'>
            <.newlex(1)>
            <blockoid>
            <.getsig>
        ]
    }

    # this is a hook for subclasses
    token unitstart { <?> }
    token lambda { '->' | '<->' }

    # Look for an expression followed by a required lambda.
    token xblock {
        :my $*GOAL ::= '{';
        :my $*BORG = {};
        <EXPR>
        { $*BORG.<culprit> = $*BORG<culprit> // $<EXPR>.cursor(self.pos) }
        <.ws>
        <pblock>
    }

    token block () {
        :temp $*CURLEX;
        :dba('scoped block')
        [ <?before '{' > || <.panic: "Missing block"> ]
        <.newlex>
        <blockoid>
        <.checkyada>
    }

    token blockoid {
        # encapsulate braided languages
        :temp %*LANG;
        :my $*SIGNUM;

        <.finishlex>
        [
        | '{YOU_ARE_HERE}' <.you_are_here>
        | :dba('block') '{' ~ '}' <statementlist> :: <.curlycheck>
        | <?terminator> <.panic: 'Missing block'>
        | <?> <.panic: "Malformed block">
        ]
    }

    token curlycheck {
        [
        || <?before \h* $$>  # (usual case without comments)
            { @*MEMOS[$¢.pos]<endstmt> = 2; }
        || <?before \h* <[\\,:]>>
        || <.unv> $$
            { @*MEMOS[$¢.pos]<endstmt> = 2; }
        || <.unsp>? { @*MEMOS[$¢.pos]<endargs> = 1; }
        ]
    }

    token regex_block {
        # encapsulate braided languages
        :temp %*LANG;
        :temp %*RX;

        :my $lang = %*LANG<Regex>;
        :my $*GOAL ::= '}';

        [ <quotepair> <.ws>
            {
                my $kv = $<quotepair>[*-1];
                $lang = ($lang.tweak($kv.<k>, $kv.<v>)
                    or $lang.panic("Unrecognized adverb :" ~ $kv.<k> ~ '(' ~ $kv.<v> ~ ')'));
            }
        ]*

        [
        | '{*}' <?{ $*MULTINESS eq 'proto' }> { $¢.<onlystar> = 1 }
        | [
            '{'
            <nibble( $¢.cursor_fresh($lang).unbalanced('}') )>
            [ '}' || <.panic: "Unable to parse regex; couldn't find right brace"> ]
          ]
        ]

        <.curlycheck>
    }

    # statement semantics
    rule statementlist {
        :my $*INVOCANT_OK = 0;
        :temp $*MONKEY_TYPING;
        :dba('statement list')

        [
        | $
        | <?before <[\)\]\}]>>
        | [<statement><eat_terminator> ]*
                { self.mark_sinks($<statement>) }
        ]
    }

    # embedded semis, context-dependent semantics
    rule semilist {
        :my $*INVOCANT_OK = 0;
        :dba('semicolon list')
        [
        | <?before <[\)\]\}]>>
        | [<statement><eat_terminator> ]*
        ]
    }


    token label {
        :my $label;
        <identifier> ':' <?before \s> <.ws>

        [ <?{ $¢.is_name($label = $<identifier>.Str) }>
          <.sorry("Illegal redeclaration of '$label'")>
        ]?

        # add label as a pseudo constant
        {{ $¢.add_constant($label,self.label_id); }}

    }

    token statement {
        :my $*endargs = -1;
        :my $*QSIGIL ::= 0;
        <!before <[\)\]\}]> >
        <!stopper>

        # this could either be a statement that follows a declaration
        # or a statement that is within the block of a code declaration
        :lang( %*LANG<MAIN> )
        <!!{ $*LASTSTATE = $¢.pos; }>

        [
        | <label> <statement>
        | <statement_control>
        | <EXPR>
            :dba('statement end')
            [
            || <?{ (@*MEMOS[$¢.pos]<endstmt> // 0) == 2 }>   # no mod after end-line curly
            ||
                :dba('statement modifier')
                <.ws>
                [
                | <statement_mod_loop>
                    {{
                        my $sp = $<EXPR><statement_prefix>;
                        if $sp and $sp<sym> eq 'do' {
                           my $s = $<statement_mod_loop>[0]<sym>;
                           $¢.obs("do...$s" ,"repeat...$s");
                        }
                    }}
                | <statement_mod_cond>
                    :dba('statement modifier loop')
                    [
                    || <?{ (@*MEMOS[$¢.pos]<endstmt> // 0) == 2 }>
                    || <.ws> <statement_mod_loop>?
                    ]
                ]?
            ]
        | <?before ';'>
        | <?before <stopper> >
        | {} <.panic: "Bogus statement">
        ]

        # Is there more on same line after a block?
        [ <?{ (@*MEMOS[@*MEMOS[$¢.pos]<ws>//$¢.pos]<endargs>//0) == 1 }>
            \h*
            <!before ';' | ')' | ']' | '}' >
            <!infixstopper>
            { $*HIGHWATER = $¢.pos = @*MEMOS[$¢.pos]<ws>//$¢.pos; }
            <.panic: "Strange text after block (missing comma, semicolon, comment marker?)">
        ]?
    }

    token eat_terminator {
        [
        || ';'
        || <?{ (@*MEMOS[$¢.pos]<endstmt>//0) >= 2 }> <.ws>
        || <?before ')' | ']' | '}' >
        || $
        || <?stopper>
        || <?before <.suppose <statement_control> > > <.backup_ws> { $*HIGHWATER = -1; } <.panic: "Missing semicolon">
        || <.panic: "Confused">
        ]
    }

    # undo any line transition
    method backup_ws () {
        if @*MEMOS[self.pos]<ws> {
            return self.cursor(@*MEMOS[self.pos]<ws>);
        }
        return self;
    }

    #####################
    # statement control #
    #####################

    token statement_control:need {
        :my $longname;
        <sym><.spacey>:s
        [
        |<version>
        |<module_name>
            {{
                my $*IN_DECL = 'use';
                my $*SCOPE = 'use';
                $longname = $<module_name>[*-1]<longname>;
                $¢.do_need($longname<name>);
            }}
        ] ** ','
    }

    token statement_control:import {
        :my $*IN_DECL = 'use';
        :my $*SCOPE = 'use';
        <sym><.spacey> <.ws>
        <term>
        [
        || <.spacey> <arglist>
            {{
                my %*MYSTERY;
                $¢.do_import($<term>, $<arglist>);
                $¢.explain_mystery();
            }}
        || {{ $¢.do_import($<term>, ''); }}
        ]
        <.ws>
    }

    token statement_control:use {
        :my $longname;
        :my $*IN_DECL = 'use';
        :my $*SCOPE = 'use';
        :my %*MYSTERY;
        <sym><.spacey> <.ws>
        [
        | <version>
        | <module_name>
            {{
                $longname = $<module_name><longname>;
                if $longname.Str eq 'MONKEY_TYPING' {
                    $*MONKEY_TYPING = True;
                }
            }}
            [
            || <.spacey> <arglist>
                {{
                    $¢.do_use($longname<name>, $<arglist>);
                }}
            || {{ $¢.do_use($longname<name>, ''); }}
            ]
        ]
        <.ws>
        <.explain_mystery>
    }


    token statement_control:no {
        :my %*MYSTERY;
        <sym><.spacey> <.ws>
        <module_name>[<.spacey><arglist>]?
        <.ws>
        <.explain_mystery>
    }


    token statement_control:if {
        <sym><.spacey> :s
        <xblock>
        [
            [
            | 'else'\h*'if' <.sorry: "Please use 'elsif'">
            | 'elsif'<?spacey> <elsif=.xblock>
            ]
        ]*
        [
            'else'<?spacey> <else=.pblock>
        ]?
    }


    token statement_control:unless {
        <sym><.spacey> :s
        <xblock>
        [ <!before 'else'> || <.panic: "\"unless\" does not take \"else\" in Perl 6; please rewrite using \"if\""> ]
    }


    token statement_control:while {
        <sym><.spacey> :s
        [ <?before '(' ['my'? '$'\w+ '=']? '<' '$'?\w+ '>' ')'>   #'
            <.panic: "This appears to be Perl 5 code"> ]?
        <xblock>
    }


    token statement_control:until {
        <sym><.spacey> :s
        <xblock>
    }


    token statement_control:repeat {
        <sym><.spacey> :s
        [
            | $<wu>=['while'|'until']<.spacey>
              <xblock>
            | <pblock>
              $<wu>=['while'|'until'][<.spacey>||<.panic: "Whitespace required after keyword">] <EXPR>
        ]
    }

    token statement_control:loop {
        <sym><.spacey> <.ws>
        $<eee> = (
            '(' [ :s
                <e1=.EXPR>? ';'
                <e2=.EXPR>? ';'
                <e3=.EXPR>?
            ')'||<.panic: "Malformed loop spec">]
            [ <?before '{' > <.sorry: "Whitespace required before block"> ]?
        )? <.ws>
        <block>
    }


    token statement_control:for {
        <sym><.spacey> :s
        [ <?before 'my'? '$'\w+ '(' >
            <.panic: "This appears to be Perl 5 code"> ]?
        [ <?before '(' <.EXPR>? ';' <.EXPR>? ';' <.EXPR>? ')' >
            <.obs('C-style "for (;;)" loop', '"loop (;;)"')> ]?
        <xblock>
    }

    token statement_control:foreach {
        <sym><.spacey> <.obs("'foreach'", "'for'")>
    }

    token statement_control:given {
        <sym><.spacey> :s
        <xblock>
    }
    token statement_control:when {
        <sym><.spacey> :s
        <?dumbsmart>
        <xblock>
    }
    rule statement_control:default {<sym><.spacey> <block> }

    token statement_prefix:BEGIN   { :my %*MYSTERY; <sym> <blast> <.explain_mystery> }
    token statement_prefix:CHECK   { <sym> <blast> }
    token statement_prefix:INIT    { <sym> <blast> }
    token statement_prefix:START   { <sym> <blast> }
    token statement_prefix:ENTER   { <sym> <blast> }
    token statement_prefix:FIRST   { <sym> <blast> }

    token statement_prefix:END     { <sym> <blast> }
    token statement_prefix:LEAVE   { <sym> <blast> }
    token statement_prefix:KEEP    { <sym> <blast> }
    token statement_prefix:UNDO    { <sym> <blast> }
    token statement_prefix:NEXT    { <sym> <blast> }
    token statement_prefix:LAST    { <sym> <blast> }
    token statement_prefix:PRE     { <sym> <blast> }
    token statement_prefix:POST    { <sym> <blast> }

    rule statement_control:CATCH   {<sym><.spacey> <block> }
    rule statement_control:CONTROL {<sym><.spacey> <block> }
    rule statement_control:TEMP    {<sym><.spacey> <block> }

    #######################
    # statement modifiers #
    #######################

    rule modifier_expr { <EXPR> }

    rule statement_mod_cond:if     {<sym><.nofun> <modifier_expr> }
    rule statement_mod_cond:unless {<sym><.nofun> <modifier_expr> }
    rule statement_mod_cond:when   {<sym><.nofun> <?dumbsmart> <modifier_expr> }

    rule statement_mod_loop:while {<sym><.nofun> <modifier_expr> }
    rule statement_mod_loop:until {<sym><.nofun> <modifier_expr> }

    rule statement_mod_loop:for   {<sym><.nofun> <modifier_expr> }
    rule statement_mod_loop:given {<sym><.nofun> <modifier_expr> }

    ################
    # module names #
    ################

    token module_name:normal {
        <longname>
        [ <?before '['> :dba('generic role') '[' ~ ']' <arglist> ]?
    }

    token vnum {
        \d+ | '*'
    }

    token version:sym<v> {
        'v' <?before \d+> :: <vnum> ** '.' '+'?
    }

    ###############
    # Declarators #
    ###############

    token variable_declarator {
        :my $*IN_DECL = 'variable';
        :my $*DECLARAND;
        :my $var;
        <variable>
        {
            $var = $<variable>.Str;
            $¢.add_variable($var);
            $*IN_DECL = '';
        }
        [   # Is it a shaped array or hash declaration?
          #  <?{ $<sigil> eq '@' | '%' }>
            <.unsp>?
            $<shape> = [
            | '(' ~ ')' <signature>
                {{
                    my $sigil = substr($var,0,1);
                    if $sigil eq '&' {
                        $¢.sorry("The () shape syntax in routine declarations is reserved (maybe use :() to declare a longname?)");
                    }
                    elsif $sigil eq '@' {
                        $¢.sorry("The () shape syntax in array declarations is reserved");
                    }
                    elsif $sigil eq '%' {
                        $¢.sorry("The () shape syntax in hash declarations is reserved");
                    }
                    else {
                        $¢.sorry("The () shape syntax in variable declarations is reserved");
                    }
                }}
            | :dba('shape definition') '[' ~ ']' <semilist>
            | :dba('shape definition') '{' ~ '}' <semilist> <.curlycheck>
            | <?before '<'> <postcircumfix>
            ]*
        ]?
        <.ws>

        <trait>*
        <post_constraint>*
        <.getdecl>
    }

    rule scoped ($*SCOPE) {
        :dba('scoped declarator')
        [
        | <declarator>
        | <regex_declarator>
        | <package_declarator>
        | [<typename> ]+
            {
                my $t = $<typename>;
                @$t > 1 and $¢.sorry("Multiple prefix constraints not yet supported");
                $*OFTYPE = $t[0];
            }
            <multi_declarator>
        | <multi_declarator>
        ]
        || <?before <[A..Z]>><longname>{{
                my $t = $<longname>.Str;
                if not $¢.is_known($t) {
                    $¢.sorry("In $*SCOPE declaration, typename '$t' must be predeclared (or marked as declarative with :: prefix)");
                }
            }}
            <!> # drop through
        || <.panic: "Malformed $*SCOPE">
    }

    token scope_declarator:my        { <sym><.nofun> <scoped('my')> }
    token scope_declarator:our       { <sym><.nofun> <scoped('our')> }
    token scope_declarator:anon      { <sym><.nofun> <scoped('anon')> }
    token scope_declarator:state     { <sym><.nofun> <scoped('state')> }
    token scope_declarator:has       { <sym><.nofun> <scoped('has')> }
    token scope_declarator:augment   { <sym><.nofun> <scoped('augment')> }
    token scope_declarator:supersede { <sym><.nofun> <scoped('supersede')> }


    token package_declarator:class {
        :my $*PKGDECL ::= 'class';
        <sym><.spacey> <package_def>
    }

    token package_declarator:grammar {
        :my $*PKGDECL ::= 'grammar';
        <sym><.spacey> <package_def>
    }

    token package_declarator:module {
        :my $*PKGDECL ::= 'module';
        <sym><.spacey> <package_def>
    }

    token package_declarator:package {
        :my $*PKGDECL ::= 'package';
        <sym><.spacey> <package_def>
    }

    token package_declarator:role {
        :my $*PKGDECL ::= 'role';
        <sym><.spacey> <package_def>
    }

    token package_declarator:knowhow {
        :my $*PKGDECL ::= 'knowhow';
        <sym><.spacey> <package_def>
    }

    token package_declarator:slang {
        :my $*PKGDECL ::= 'slang';
        <sym><.spacey> <package_def>
    }

    token package_declarator:require {   # here because of declarational aspects
        <sym><.spacey> <.ws>
        [
        || <module_name> <.ws> <EXPR>?
        || <EXPR>
        ]
    }

    token package_declarator:trusts {
        <sym><.spacey> <.ws>
        <module_name>
    }

    token package_declarator:sym<also> {
        <sym><.spacey>:s
        <trait>+
    }

    rule package_def {
        :my $longname;
        :my $*IN_DECL = 'package';
        :my $*DECLARAND;
        :my $*NEWPKG;
        :my $*NEWLEX;
        :my $outer = $*CURLEX;
        :temp $*CURPKG;
        :temp $*CURLEX;
        { $*SCOPE = $*SCOPE || 'our'; }
        [
            [ <longname> { $longname = $<longname>[0]; $¢.add_name($longname<name>.Str); } ]?
            <.newlex>
            [ :dba('generic role')
                <?{ ($*PKGDECL//'') eq 'role' }>
                '[' ~ ']' <signature>
                { $*IN_DECL = ''; }
            ]?
            <trait>*
            <.getdecl>
            [
            || <?before '{'>
                [
                {{
                    # figure out the actual full package name (nested in outer package)
                    if $longname and $*NEWPKG {
                        my $shortname = $longname.<name>.Str;
                        if $*SCOPE eq 'our' {
                            $*CURPKG = $*NEWPKG // $*CURPKG.{$shortname ~ '::'};
                            self.deb("added our " ~ $*CURPKG.id) if $DEBUG::symtab;
                        }
                        else {
                            $*CURPKG = $*NEWPKG // $*CURPKG.{$shortname ~ '::'};
                            self.deb("added my " ~ $*CURPKG.id) if $DEBUG::symtab;
                        }
                    }
                    $*begin_compunit = 0;
                    $*UNIT<$?LONGNAME> = $*UNIT<$?LONGNAME> || ($longname ?? $longname<name>.Str !! '');
                }}
                { $*IN_DECL = ''; }
                <blockoid>
                <.checkyada>
                ]
            || <?before ';'>
                [
                || <?{ $*begin_compunit }>
                    {{
                        $longname orelse $¢.panic("Compilation unit cannot be anonymous");
                        $outer == $*UNIT or $¢.panic("Semicolon form of " ~ $*PKGDECL ~ " definition not allowed in subscope;\n  please use block form");
                        $*PKGDECL eq 'package' and $¢.panic("Semicolon form of package definition indicates a Perl 5 module; unfortunately,\n  STD doesn't know how to parse Perl 5 code yet");
                        my $shortname = $longname.<name>.Str;
                        $*CURPKG = $*NEWPKG // $*CURPKG.{$shortname ~ '::'};
                        $*begin_compunit = 0;

                        # XXX throws away any role sig above
                        $*CURLEX = $outer;

                        $*UNIT<$?LONGNAME> = $longname<name>.Str;
                    }}
                    { $*IN_DECL = ''; }
                    <statementlist>     # whole rest of file, presumably
                || <.panic: "Too late for semicolon form of " ~ $*PKGDECL ~ " definition">
                ]
            || <.panic: "Unable to parse " ~ $*PKGDECL ~ " definition">
            ]
        ] || <.panic: "Malformed $*PKGDECL">
    }

    token declarator {
        [
        | <variable_declarator>
            [ <?before <.ws>','<.ws> { @*MEMOS[$¢.pos]<declend> = $*SCOPE; }> ]?
        | '(' ~ ')' <signature> <trait>*
        | <routine_declarator>
        | <regex_declarator>
        | <type_declarator>
        ]
    }

    token multi_declarator:multi {
        :my $*MULTINESS = 'multi';
        <sym><.spacey> <.ws> [ <declarator> || <routine_def('multi')> || <.panic: 'Malformed multi'> ]
    }
    token multi_declarator:proto {
        :my $*MULTINESS = 'proto';
        <sym><.spacey> <.ws> [ <declarator> || <routine_def('proto')> || <.panic: 'Malformed proto'> ]
    }
    token multi_declarator:only {
        :my $*MULTINESS = 'only';
        <sym><.spacey> <.ws> [ <declarator> || <routine_def('only')> || <.panic: 'Malformed only'> ]
    }
    token multi_declarator:null {
        :my $*MULTINESS = '';
        <declarator>
    }

    token routine_declarator:sub       { <sym><.nofun> <routine_def('sub')> }
    token routine_declarator:method    { <sym><.nofun> <method_def> }
    token routine_declarator:submethod { <sym><.nofun> <method_def> }
    token routine_declarator:macro     { <sym><.nofun> <macro_def> }

    token regex_declarator:regex { <sym><.spacey> <regex_def(:!r,:!s)> }
    token regex_declarator:token { <sym><.spacey> <regex_def(:r,:!s)> }
    token regex_declarator:rule  { <sym><.spacey> <regex_def(:r,:s)> }

    rule multisig {
        :my $signum = 0;
        :dba('signature')
        [
            ':'?'(' ~ ')' <signature(++$signum)>
        ]
        ** '|'
    }

    method checkyada {
        try {
            my $statements = self.<blockoid><statementlist><statement>;
            my $startsym = $statements[0]<EXPR><sym> // '';
            if $startsym eq '...' { $*DECLARAND<stub> = 1 }
            elsif $startsym eq '!!!' { $*DECLARAND<stub> = 1 }
            elsif $startsym eq '???' { $*DECLARAND<stub> = 1 }
            elsif $startsym eq '*' {
                if $*MULTINESS eq 'proto' and $statements.elems == 1 {
                    self.<blockoid>:delete;
                    self.<onlystar> = 1;
                }
            }
        }
        return self;
    }

    rule routine_def ($d) {
        :temp $*CURLEX;
        :my $*IN_DECL = $d;
        :my $*DECLARAND;
        [
            [ $<sigil>=['&''*'?] <deflongname>? | <deflongname> ]?
            <.newlex(1)>
            [ <multisig> | <trait> ]*
            [ <!before '{'> <.panic: "Malformed block"> ]?
            <!{
                $*IN_DECL = '';
            }>
            <blockoid>:!s
            <.checkyada>
            <.getsig>
            <.getdecl>
        ] || <.panic: "Malformed routine">
    }

    rule method_def () {
        :temp $*CURLEX;
        :my $*IN_DECL = 'method';
        :my $*DECLARAND;
        <.newlex(1)>
        [
            [
            | $<type>=[<[ ! ^ ]>?]<longname> [ <multisig> | <trait> ]*
            | <multisig> <trait>*
            | <sigil> '.'
                :dba('subscript signature')
                [
                | '(' ~ ')' <signature>
                | '[' ~ ']' <signature>
                | '{' ~ '}' <signature> # don't need curlycheck here
                | <?before '<'> <postcircumfix>
                ]
                <trait>*
            | <?>
            ]
            { $*IN_DECL = ''; }
            <blockoid>:!s
            <.checkyada>
            <.getsig>
            <.getdecl>
        ] || <.panic: "Malformed method">
    }

    rule regex_def (:$r, :$s) {
        :temp $*CURLEX;
        :my $*IN_DECL = 'regex';
        :temp %*RX;
        :my $*DECLARAND;
        { %*RX<s> = $s; %*RX<r> = $r; }
        [
            [ '&'<deflongname>? | <deflongname> ]?
            <.newlex(1)>
            [ [ ':'?'(' <signature(1)> ')'] | <trait> ]*
            [ <!before '{'> <.panic: "Malformed block"> ]?
            { $*IN_DECL = ''; }
            <.finishlex>
            <regex_block>:!s
            <.getsig>
            <.getdecl>
        ] || <.panic: "Malformed regex">
    }

    rule macro_def () {
        :temp $*CURLEX;
        :my $*IN_DECL = 'macro';
        :my $*DECLARAND;
        [
            [ '&'<deflongname>? | <deflongname> ]?
            <.newlex(1)>
            [ <multisig> | <trait> ]*
            [ <!before '{'> <.panic: "Malformed block"> ]?
            { $*IN_DECL = ''; }
            <blockoid>:!s
            <.checkyada>
            <.getsig>
            <.getdecl>
        ] || <.panic: "Malformed macro">
    }

    rule trait {
        :my $*IN_DECL = 0;
        [
        | <trait_mod>
        | <colonpair>
        ]
    }

    token trait_mod:is {
        <sym><.spacey>:s <longname><circumfix>?  # e.g. context<rw> and Array[Int]
        {{
            if $*DECLARAND {
                my $traitname = $<longname>.Str;
                # XXX eventually will use multiple dispatch
                $*DECLARAND{$traitname} = self.gettrait($traitname, $<circumfix>);
            }
        }}
    }
    token trait_mod:hides {
        <sym><.spacey>:s <module_name>
    }
    token trait_mod:does {
        :my $*PKGDECL ::= 'role';
        <sym><.spacey>:s <module_name>
    }
    token trait_mod:will {
        <sym><.spacey>:s <identifier> <pblock>
    }

    token trait_mod:of {
        ['of'|'returns']:s <typename>
        [ <?{ $*DECLARAND<of> }> <.sorry("Extra 'of' type; already declared as type " ~ $*DECLARAND<of>.Str)> ]?
        { $*DECLARAND<of> = $<typename>; }
    }
    token trait_mod:as      { <sym><.spacey>:s <typename> }
    token trait_mod:handles { <sym><.spacey>:s <term> }

    #########
    # Nouns #
    #########

    # (for when you want to tell EXPR that infix already parsed the term)
    token nullterm {
        <?>
    }

    token nulltermish {
        :dba('null term')
        [
        | <?stdstopper>
        | <term=.termish>
            {
                $¢.<PRE>  = $<term><PRE>:delete;
                $¢.<POST> = $<term><POST>:delete;
                $¢.<~CAPS> = $<term><~CAPS>;
            }
        | <?>
        ]
    }

    token termish {
        :my $*SCOPE = "";
        :my $*MULTINESS = "";
        :my $*OFTYPE;
        :my $*VAR;
        :dba('prefix or term')
        [
        | <PRE> [ <!{ my $p = $<PRE>; my @p = @$p; @p[*-1]<O><term> and $<term> = pop @$p }> <PRE> ]*
            [ <?{ $<term> }> || <term> || <.panic("Prefix requires an argument")> ]
        | <term>
        ]

        # also queue up any postfixes
        :dba('postfix')
        [
        || <?{ $*QSIGIL }>
            [
            || <?{ $*QSIGIL eq '$' }> [ [<!before '\\'> <POST>]+! <?after <[ \] } > ) ]> > || { $<POST> = [] } ]
            ||                          [<!before '\\'> <POST>]+! <?after <[ \] } > ) ]> > 
            || { $*VAR = 0; }
            ]
        || <!{ $*QSIGIL }>
            <POST>*
        ]
        {
            self.check_variable($*VAR) if $*VAR;
            $¢.<~CAPS> = $<term><~CAPS>;
        }
    }

    token term:fatarrow           { <fatarrow> }
    token term:variable           { <variable> { $*VAR = $<variable> } }
    token term:package_declarator { <package_declarator> }
    token term:scope_declarator   { <scope_declarator> }
    token term:multi_declarator   { <?before 'multi'|'proto'|'only'> <multi_declarator> }
    token term:routine_declarator { <routine_declarator> }
    token term:regex_declarator   { <regex_declarator> }
    token term:type_declarator    { <type_declarator> }
    token term:circumfix          { <circumfix> }
    token term:dotty              { <dotty> }
    token term:value              { <value> }
    token term:capterm            { <capterm> }
    token term:sigterm            { <sigterm> }
    token term:statement_prefix   { <statement_prefix> }
    token term:colonpair          { [ <colonpair> <.ws> ]+ }

    token fatarrow {
        <key=.identifier> \h* '=>' <.ws> <val=.EXPR(item %item_assignment)>
    }

    token coloncircumfix ($front) {
        [
        | '<>' <.worry("Pair with <> really means a Nil value, not null string; use :$front" ~ "('') to represent the null string,\n  or :$front" ~ "() to represent Nil more accurately")>
        | <circumfix>
        ]
    }

    token colonpair {
        :my $key;
        :my $value;

        ':'
        :dba('colon pair')
        [
        | '!' :: [ <identifier> || <.panic: "Malformed False pair; expected identifier">]
            [ <?before <[ \[ \( \< \{ ]>> <.panic: "Extra argument not allowed; pair already has False argument"> ]?
            { $key = $<identifier>.Str; $value = 0; }
        | $<num> = [\d+] <identifier> [ <?before <[ \[ \( \< \{ ]>> <.sorry("Extra argument not allowed; pair already has argument of " ~ $<num>.Str)> <.circumfix> ]?
        | <identifier>
            { $key = $<identifier>.Str; }
            [
            || <.unsp>? :dba('pair value') <coloncircumfix($key)> { $value = $<coloncircumfix>; }
            || { $value = 1; }
            ]
        | :dba('signature') '(' ~ ')' <fakesignature>
        | <coloncircumfix('')>
            { $key = ""; $value = $<coloncircumfix>; }
        | $<var> = (
                <sigil> {}
                [
                | <twigil>? <desigilname>
                | '<' <desigilname> '>'
                ]
            )
            { $key = $<var><desigilname>.Str; $value = $<var>; $¢.check_variable($value); }
        ]
        { $<k> = $key; $<v> = $value; }
    }

    # Most of these special variable rules are there simply to catch old p5 brainos

    token special_variable:sym<$¢> { <sym> }

    token special_variable:sym<$!> { <sym> <!before \w> }

    token special_variable:sym<$!{ }> {
        '$!' '{' ~ '}' [<identifier> | <statementlist>]
        {{
            my $all = substr(self.orig, self.pos, $¢.pos - self.pos);
            my ($inside) = $all ~~ /^...\s*(.*?)\s*.$/;
            $¢.obs("Perl 5's $all construct", "a smartmatch like \$! ~~ $inside" );
        }}
    }

    token special_variable:sym<$/> {
        <sym>
        # XXX assuming nobody ever wants to assign $/ directly anymore...
        [ <?before \h* '=' <![=]> >
            <.obs('$/ variable as input record separator',
                 "the filehandle's :irs attribute")>
        ]?
    }

    token special_variable:sym<$~> {
        <sym> :: <?before \s | ',' | '=' | <terminator> >
        <.obs('$~ variable', 'Form module')>
    }

    token special_variable:sym<$`> {
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('$` variable', 'explicit pattern before <(')>
    }

    token special_variable:sym<$@> {
        <sym> <?before \W> ::
        <.obs('$@ variable as eval error', '$!')>
    }

    token special_variable:sym<$#> {
        <sym> ::
        [
        || (\w+) <.obs("\$#" ~ $0.Str ~ " variable", '@' ~ $0.Str ~ '.end')>
        || <.obs('$# variable', '.fmt')>
        ]
    }
    token special_variable:sym<$$> {
        <sym> <!alpha> :: <?before \s | ',' | <terminator> >
        <.obs('$$ variable', '$*PID')>
    }
    token special_variable:sym<$%> {
        <sym> <!before \w> <!sigil> ::
        <.obs('$% variable', 'Form module')>
    }

    # Note: this works because placeholders are restricted to lowercase
    token special_variable:sym<$^X> {
        <sigil> '^' $<letter> = [<[A..Z]>] \W
        <.obscaret($<sigil>.Str ~ '^' ~ $<letter>.Str, $<sigil>.Str, $<letter>.Str)>
    }

    token special_variable:sym<$^> {
        <sym> :: <?before \s | ',' | '=' | <terminator> >
        <.obs('$^ variable', 'Form module')>
    }

    token special_variable:sym<$&> {
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('$& variable', '$/ or $()')>
    }

    token special_variable:sym<$*> {
        <sym> :: <?before \s | ',' | '=' | <terminator> >
        <.obs('$* variable', '^^ and $$')>
    }

    token special_variable:sym<$)> {
        <sym> <?{ $*GOAL ne ')' }> <?before \s | ',' | <terminator> >
        <.obs('$) variable', '$*EGID')>
    }

    token special_variable:sym<$-> {
        <sym> :: <?before \s | ',' | '=' | <terminator> >
        <.obs('$- variable', 'Form module')>
    }

    token special_variable:sym<$=> {
        <sym> :: <?before \s | ',' | '=' | <terminator> >
        <.obs('$= variable', 'Form module')>
    }

    token special_variable:sym<@+> {
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('@+ variable', '.to method')>
    }

    token special_variable:sym<%+> {
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('%+ variable', '.to method')>
    }

    token special_variable:sym<$+[ ]> {
        '$+['
        <.obs('@+ variable', '.to method')>
    }

    token special_variable:sym<@+[ ]> {
        '@+['
        <.obs('@+ variable', '.to method')>
    }

    token special_variable:sym<@+{ }> {
        '@+{'
        <.obs('%+ variable', '.to method')>
    }

    token special_variable:sym<@-> {
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('@- variable', '.from method')>
    }

    token special_variable:sym<%-> {
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('%- variable', '.from method')>
    }

    token special_variable:sym<$-[ ]> {
        '$-['
        <.obs('@- variable', '.from method')>
    }

    token special_variable:sym<@-[ ]> {
        '@-['
        <.obs('@- variable', '.from method')>
    }

    token special_variable:sym<%-{ }> {
        '@-{'
        <.obs('%- variable', '.from method')>
    }

    token special_variable:sym<$+> {
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('$+ variable', 'Form module')>
    }

    token special_variable:sym<${^ }> {
        <sigil> '{^' :: $<text>=[.*?] '}'
        <.obscaret($<sigil>.Str ~ '{^' ~ $<text>.Str ~ '}', $<sigil>.Str, $<text>.Str)>
    }

    # XXX should eventually rely on multi instead of nested cases here...
    method obscaret (Str $var, Str $sigil, Str $name) {
        my $repl;
        if $sigil eq '$' {
            if $name eq 'MATCH'        { $repl = '$/' }
            elsif $name eq 'PREMATCH'  { $repl = 'an explicit pattern before <(' }
            elsif $name eq 'POSTMATCH' { $repl = 'an explicit pattern after )>' }
            elsif $name eq 'ENCODING'  { $repl = '$?ENCODING' }
            elsif $name eq 'UNICODE'   { $repl = '$?UNICODE' }  # XXX ???
            elsif $name eq 'TAINT'     { $repl = '$*TAINT' }
            elsif $name eq 'OPEN'      { $repl = 'filehandle introspection' }
            elsif $name eq 'N'         { $repl = '$-1' } # XXX ???
            elsif $name eq 'L'         { $repl = 'Form module' }
            elsif $name eq 'A'         { $repl = 'Form module' }
            elsif $name eq 'E'         { $repl = '$!.extended_os_error' }
            elsif $name eq 'C'         { $repl = 'COMPILING namespace' }
            elsif $name eq 'D'         { $repl = '$*DEBUGGING' }
            elsif $name eq 'F'         { $repl = '$*SYSTEM_FD_MAX' }
            elsif $name eq 'H'         { $repl = '$?FOO variables' }
            elsif $name eq 'I'         { $repl = '$*INPLACE' } # XXX ???
            elsif $name eq 'O'         { $repl = '$?OS or $*OS' }
            elsif $name eq 'P'         { $repl = 'whatever debugger Perl 6 comes with' }
            elsif $name eq 'R'         { $repl = 'an explicit result variable' }
            elsif $name eq 'S'         { $repl = 'the context function' } # XXX ???
            elsif $name eq 'T'         { $repl = '$*BASETIME' }
            elsif $name eq 'V'         { $repl = '$*PERL_VERSION' }
            elsif $name eq 'W'         { $repl = '$*WARNING' }
            elsif $name eq 'X'         { $repl = '$*EXECUTABLE_NAME' }
            else                       { $repl = "a global form such as $sigil*$name" }
        }
        elsif $sigil eq '%' {
            if $name eq 'H'            { $repl = '$?FOO variables' }
            else                       { $repl = "a global form such as $sigil*$name" }
        }
        else {
            $repl = "a global form such as $sigil*$name"
        }
        return self.obs("$var variable", $repl);
    }

    token special_variable:sym<::{ }> {
        '::' <?before '{'>
    }

    regex special_variable:sym<${ }> {
        <sigil> '{' {} $<text>=[.*?] '}'
        {{
            my $sigil = $<sigil>.Str;
            my $text = $<text>.Str;
            my $bad = $sigil ~ '{' ~ $text ~ '}';
            $text = $text - 1 if $text ~~ /^\d+$/;
            if $text !~~ /^(\w|\:)+$/ {
                return () if $*QSIGIL;
                $¢.obs($bad, $sigil ~ '(' ~ $text ~ ')');
            }
            elsif $*QSIGIL {
                $¢.obs($bad, '{' ~ $sigil ~ $text ~ '}');
            }
            else {
                $¢.obs($bad, $sigil ~ $text);
            }
        }} # always fails, don't need curlycheck here
    }

    token special_variable:sym<$[> {
        <sym> :: <?before \s | ',' | '=' | <terminator> >
        <.obs('$[ variable', 'user-defined array indices')>
    }

    token special_variable:sym<$]> {
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('$] variable', '$*PERL_VERSION')>
    }

    token special_variable:sym<$\\> {
        <sym> :: <?before \s | ',' | '=' | <terminator> >
        <.obs('$\\ variable', "the filehandle's :ors attribute")>
    }

    token special_variable:sym<$|> {
        <sym> :: <?before \s | ',' | '=' | <terminator> >
        <.obs('$| variable', ':autoflush on open')>
    }

    token special_variable:sym<$:> {
        <sym> <?before <[\x20\t\n\],=)}]> >
        <.obs('$: variable', 'Form module')>
    }

    token special_variable:sym<$;> {
        <sym> :: <?before \s | ',' | '=' | <terminator> >
        <.obs('$; variable', 'real multidimensional hashes')>
    }

    token special_variable:sym<$'> { #'
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('$' ~ "'" ~ 'variable', "explicit pattern after )\x3E")>
    }

    token special_variable:sym<$"> {
        <sym> <!{ $*QSIGIL }>
        :: <?before \s | ',' | '=' | <terminator> >
        <.obs('$" variable', '.join() method')>
    }

    token special_variable:sym<$,> {
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('$, variable', ".join() method")>
    }

    token special_variable:sym['$<'] {
        <sym> <?before \h* <[ = , ; ? : ! ) \] } ]> <!before \S* '>'> >
        <.obs('$< variable', '$*UID')>
    }

    token special_variable:sym«\$>» {
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('$> variable', '$*EUID')>
    }

    token special_variable:sym<$.> {
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('$. variable', "the filehandle's .line method")>
    }

    token special_variable:sym<$?> {
        <sym> :: <?before \s | ',' | <terminator> >
        <.obs('$? variable as child error', '$!')>
    }

    # desigilname should only follow a sigil/twigil

    token desigilname {
        [
        | <?before '$' >
            [ <?{ $*IN_DECL }> <.panic: "Can't declare an indirect variable name"> ]?
            <variable> {
                $*VAR = $<variable>;
                self.check_variable($*VAR) if substr($*VAR,1,1) ne '$';
            }
        | <?before <[\@\%\&]> <sigil>* \w > <.panic: "Invalid hard reference syntax">
        | <longname>
        ]
    }

    token variable {
        :my $*IN_META = '';
        :my $sigil = '';
        :my $twigil = '';
        :my $name;
        <?before <sigil> {
            $sigil = $<sigil>.Str;
            $*LEFTSIGIL = $*LEFTSIGIL || $sigil;
        }> {}
        [
        || <sigil> <twigil>? <?before '::' [ '{' | '<' | '(' ]> <longname> # XXX
        || '&'
            [
            | <twigil>? <sublongname> { $name = $<sublongname>.Str }
            | :dba('infix noun') '[' ~ ']' <infixish('[]')>
            ]
        || '$::' <name>? # XXX
        || '$:' <name> # XXX
        || [
            | <sigil> <twigil>? <desigilname> { $name = $<desigilname>.Str }
            | <special_variable>
            | <sigil> <index=.decint> [<?{ $*IN_DECL }> <.panic: "Can't declare a numeric variable">]?
            # Note: $() can also parse as contextualizer in an expression; should have same effect
            | <sigil> <?before '<'> <postcircumfix> [<?{ $*IN_DECL }> <.panic: "Can't declare a match variable">]?
            | <sigil> <?before '('> <postcircumfix> [<?{ $*IN_DECL }> <.panic: "Can't declare a contextualizer">]?
            | <sigil> <?{ $*IN_DECL }>
            | <?> {{
                if $*QSIGIL {
                    return ();
                }
                else {
                    $¢.sorry("Non-declarative sigil is missing its name");
                }
              }}
            ]
        ]

        { my $t = $<twigil>; $twigil = $t.[0].Str if @$t; }
        [ <?{ $twigil eq '.' }>
            [<.unsp> | '\\' | <?> ] <?before '('> <postcircumfix>
        ]?
    }



    token deflongname {
        :dba('new name to be defined')
        <name>
        [
        | <colonpair>+ { $¢.add_categorical(substr(self.orig, self.pos, $¢.pos - self.pos)) if $*IN_DECL; }
        | { $¢.add_routine($<name>.Str) if $*IN_DECL; }
        ]
    }

    token subshortname {
        [
        | <category> <colonpair>+
        | <desigilname>
        ]
    }

    token sublongname {
        <subshortname> <sigterm>?
    }

    token value:quote   { <quote> }
    token value:number  { <number> }
    token value:version { <version> }

    # Note: call this only to use existing type, not to declare type
    token typename {
        [
        | '::?'<identifier>                 # parse ::?CLASS as special case
        | <longname>
          <?{{
            my $longname = $<longname>.Str;
            if substr($longname, 0, 2) eq '::' {
                $¢.add_my_name(substr($longname, 2));
            }
            else {
                $¢.is_name($longname)
            }
          }}>
        ]
        # parametric type?
        <.unsp>? [ <?before '['> <param=.postcircumfix> ]?
        <.unsp>? [ <?before '{'> <whence=.postcircumfix> ]?
        [<.ws> 'of' <.ws> <typename> ]?
    }

    # Note, does not include <1/2> forms, which are parsed as quotewords

    token number {
        [
        | 'NaN' »
        | <integer>
        | <dec_number>
        | <rad_number>
        | 'Inf' »
        ]
    }

    # <strtonum> is (we hope) used only by Str --> Num conversions
    #  (such as those done dwimmily by quotewords)
    token strtonum:rational { <[+\-]>?<nu=.integer>'/'<de=.integer> }
    token strtonum:complex { [<[+\-]>?<re=.number>]? <[+\-]><im=.number>'\\'?'i' }
    token strtonum:number { <[+\-]>?<number> }

    ##########
    # Quotes #
    ##########

    token sibble ($l, $lang2) {
        :my ($lang, $start, $stop);
        <babble($l)>
        { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }

        $start <left=.nibble($lang)> [ $stop || <.panic: "Couldn't find terminator $stop"> ]
        [ <?{ $start ne $stop }>
            <.ws>
            [ <infixish> || <panic: "Missing assignment operator"> ]
            [ <?{ $<infixish>.Str eq '=' || $<infixish>.<assign_meta_operator> }> || <.panic: "Malformed assignment operator"> ]
            <.ws>
            <right=EXPR(item %item_assignment)>
        || 
            { $lang = $lang2.unbalanced($stop); }
            <right=.nibble($lang)> $stop || <.panic: "Malformed replacement part; couldn't find final $stop">
        ]
    }

    token tribble ($l, $lang2 = $l) {
        :my ($lang, $start, $stop);
        :my $*CCSTATE = '';
        <babble($l)>
        { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }

        $start <left=.nibble($lang)> [ $stop || <.panic: "Couldn't find terminator $stop"> ]
        { $*CCSTATE = ''; }
        [ <?{ $start ne $stop }>
            <.ws> <quibble($lang2)>
        || 
            { $lang = $lang2.unbalanced($stop); }
            <right=.nibble($lang)> $stop || <.panic: "Malformed replacement part; couldn't find final $stop">
        ]
    }

    token quasiquibble ($l) {
        :temp %*LANG;
        :my ($lang, $start, $stop);
        :my $*QUASIMODO = 0; # :COMPILING sets true
        <babble($l)>
        {
            my $B = $<babble><B>;
            ($lang,$start,$stop) = @$B;
            %*LANG<MAIN> = $lang;
        }

        [
        || <?{ $start eq '{' }> [ :lang($lang) <block> ]
        || [ :lang($lang) <starter> <statementlist> [ <stopper> || <.panic: "Couldn't find terminator $stop"> ] ]
        ]
    }

    token quote:sym<//>   {
        '/'\s*'/' <.sorry: "Null regex not allowed">
    }

    token quote:sym</ />   {
        '/' <nibble( $¢.cursor_fresh( %*LANG<Regex> ).unbalanced("/") )> [ '/' || <.panic: "Unable to parse regex; couldn't find final '/'"> ]
        <.old_rx_mods>?
    }

    # handle composite forms like qww
    token quote:qq {
        :my $qm;
        'qq'
        [
        | <quote_mod> » <!before '('> { $qm = $<quote_mod>.Str } <.ws> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).tweak($qm => 1))>
        | » <!before '('> <.ws> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq))>
        ]
    }
    token quote:q {
        :my $qm;
        'q'
        [
        | <quote_mod> » <!before '('> { $qm = $<quote_mod>.Str } <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q).tweak($qm => 1))>
        | » <!before '('> <.ws> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q))>
        ]
    }

    token quote:Q {
        :my $qm;
        'Q'
        [
        | <quote_mod> » <!before '('> { $qm = $<quote_mod>.Str } <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak($qm => 1))>
        | » <!before '('> <.ws> <quibble($¢.cursor_fresh( %*LANG<Q> ))>
        ]
    }

    token quote_mod:w  { <sym> }
    token quote_mod:ww { <sym> }
    token quote_mod:p  { <sym> }
    token quote_mod:x  { <sym> }
    token quote_mod:to { <sym> }
    token quote_mod:s  { <sym> }
    token quote_mod:a  { <sym> }
    token quote_mod:h  { <sym> }
    token quote_mod:f  { <sym> }
    token quote_mod:c  { <sym> }
    token quote_mod:b  { <sym> }

    token quote:rx {
        <sym> » <!before '('>
        <quibble( $¢.cursor_fresh( %*LANG<Regex> ) )>
        <!old_rx_mods>
    }

    token quote:m  {
        <sym> » <!before '('>
        <quibble( $¢.cursor_fresh( %*LANG<Regex> ) )>
        <!old_rx_mods>
    }

    token quote:mm {
        <sym> » <!before '('>
        <quibble( $¢.cursor_fresh( %*LANG<Regex> ).tweak(:s))>
        <!old_rx_mods>
    }

    token quote:s {
        <sym> » <!before '('>
        <pat=.sibble( $¢.cursor_fresh( %*LANG<Regex> ), $¢.cursor_fresh( %*LANG<Q> ).tweak(:qq))>
        <!old_rx_mods>
    }

    token quote:ss {
        <sym> » <!before '('>
        <pat=.sibble( $¢.cursor_fresh( %*LANG<Regex> ).tweak(:s), $¢.cursor_fresh( %*LANG<Q> ).tweak(:qq))>
        <!old_rx_mods>
    }
    token quote:tr {
        <sym> » <!before '('> <pat=.tribble( $¢.cursor_fresh( %*LANG<Q> ).tweak(:cc))>
        <!old_tr_mods>
    }

    token quote:y {
        <sym> »
        # could be defined as a function or constant
        <!{ self.is_known('&y') or self.is_known('y') }>
        <!before '('> <?before \h*\W>
        <.obs('y///','tr///')>
    }

    token old_rx_mods {
        <!after \s>
        (< i g s m x c e >+) 
        {{
            my $m = $0.Str;
            $m ~~ /i/ and $¢.worryobs('/i',':i');
            $m ~~ /g/ and $¢.worryobs('/g',':g');
            $m ~~ /m/ and $¢.worryobs('/m','^^ and $$ anchors');
            $m ~~ /s/ and $¢.worryobs('/s','. or \N');
            $m ~~ /x/ and $¢.worryobs('/x','normal default whitespace');
            $m ~~ /c/ and $¢.worryobs('/c',':c or :p');
            $m ~~ /e/ and $¢.worryobs('/e','interpolated {...} or s{} = ... form');
            $¢.obs('suffix regex modifiers','prefix adverbs');
        }}
    }

    token old_tr_mods {
        (< c d s ] >+) 
        {{
            my $m = $0.Str;
            $m ~~ /c/ and $¢.worryobs('/c',':c');
            $m ~~ /d/ and $¢.worryobs('/g',':d');
            $m ~~ /s/ and $¢.worryobs('/s',':s');
            $¢.obs('suffix transliteration modifiers','prefix adverbs');
        }}
    }

    token quote:quasi {
        <sym> » <!before '('> <quasiquibble($¢.cursor_fresh( %*LANG<Quasi> ))>
    }

    ###########################
    # Captures and Signatures #
    ###########################

    token capterm {
        '\\'
        [
        | '(' <capture>? ')'
        | <?before \S> <termish>
        | {} <.panic: "You can't backslash that">
        ]
    }

    rule capture {
        :my $*INVOCANT_OK = 1;
        <EXPR>
    }

    token sigterm {
        :dba('signature')
        ':(' ~ ')' <fakesignature>
    }

    rule param_sep { [','|':'|';'|';;'] }

    token fakesignature() {
        :temp $*CURLEX;
        :my $*DECLARAND;
        <.newlex>
        <signature>
    }

    token signature ($lexsig = 0) {
        :my $*IN_DECL = 'sig';
        :my $*zone = 'posreq';
        :my $startpos = self.pos;
        :my $*MULTINESS = 'only';
        :my $*SIGNUM = $lexsig;
        <.ws>
        [
        | '\|' [ <param_var> || <.panic: "\\| signature must contain one variable"> ]
            <.ws> [ <?before '-->' | ')' | ']' > || <.panic: "\\| signature may contain only a variable"> ]
        |   [
            | <?before '-->' | ')' | ']' | '{' | ':'\s | ';;' >
            | [ <parameter> || <.panic: "Malformed parameter"> ]
            ] ** <param_sep>
        ]
        <.ws>
        { $*IN_DECL = ''; }
        [ '-->' <.ws>
            [
            || <type_constraint>
            || <longname> <.panic("Typename " ~ $<longname>[0].Str ~ " must be predeclared")>
            || <.panic: "No type found after -->">
            ]
            <.ws>
        ]?
        {{
            $*LEFTSIGIL = '@';
            if $lexsig {
                $*CURLEX.<$?SIGNATURE> ~= '|' if $lexsig > 1;
                $*CURLEX.<$?SIGNATURE> ~= '(' ~ substr(self.orig, $startpos, $¢.pos - $startpos) ~ ')';
                $*CURLEX.<!NEEDSIG>:delete;
            }
        }}
    }

    token type_declarator:subset {
        :my $*IN_DECL = 'subset';
        :my $*DECLARAND;
        <sym><.spacey> :s
        [
            [ <longname> { $¢.add_name($<longname>[0].Str); } ]?
            <trait>*
            [where <EXPR(item %item_assignment)> ]?    # (EXPR can parse multiple where clauses)
        ] || <.panic: "Malformed subset">
    }

    token type_declarator:enum {
        :my $*IN_DECL = 'enum';
        :my $*DECLARAND;
        <sym><.spacey> <.ws>
        [
        | <name=longname> { $¢.add_name($<name>.Str); }
        | <name=variable> { $¢.add_variable($<name>.Str); }
        | <?>
        ]
        { $*IN_DECL = ''; }
        <.ws>
        <trait>* <?before <[ < ( « ]> > <term> <.ws>
            {$¢.add_enum($<name>, $<term>.Str); }
    }

    token type_declarator:constant {
        :my $*IN_DECL = 'constant';
        :my $*DECLARAND;
        <sym><.spacey> <.ws>

        [
        | <identifier> { $¢.add_name($<identifier>.Str); }
        | <variable> { $¢.add_variable($<variable>.Str); }
        | <?>
        ]
        { $*IN_DECL = ''; }
        <.ws>

        <trait>*

        [
        || <?before '='>
        || <?before <-[\n=]>*'='> <.panic: "Malformed constant"> # probable initializer later
        || <.sorry: "Missing initializer on constant declaration">
        ]

        <.getdecl>
    }


    token type_constraint {
        :my $*IN_DECL = '';
        [
        | <value>
        | <typename>
            [ <?{ $*DECLARAND<of> }> <.sorry("Extra 'of' type; already declared as type " ~ $*DECLARAND<of>.Str)> ]?
            { $*DECLARAND<of> = $<typename>; }
        | where <.ws> <EXPR(item %item_assignment)>
        ]
        <.ws>
    }

    rule post_constraint {
        :my $*IN_DECL = '';
        :dba('constraint')
        [
        | '[' ~ ']' <signature>
        | '(' ~ ')' <signature>
        | where <EXPR(item %item_assignment)>
        ]
    }

    token named_param {
        :my $*GOAL ::= ')';
        ':'
        [
        | <name=.identifier> '(' <.ws>
            [ <named_param> | <param_var> <.ws> ]
            [ ')' || <.panic: "Unable to parse named parameter; couldn't find right parenthesis"> ]
        | <param_var(1)>
        ]
    }

    token param_var($named = 0) {
        :dba('formal parameter')
        [
        | '[' ~ ']' <signature>
        | '(' ~ ')' <signature>
        | <sigil> <twigil>?
            [
                # Is it a longname declaration?
            || <?{ $<sigil>.Str eq '&' }> <?ident> {}
                <name=.sublongname>

            ||  # Is it a shaped array or hash declaration?
                <?{ $<sigil>.Str eq '@' || $<sigil>.Str eq '%' }>
                <name=.identifier>?
                <?before <[ \< \( \[ \{ ]> >
                <postcircumfix>

                # ordinary parameter name
            || <name=.identifier>
            || <name=.decint> <.panic: "Can't declare a numeric parameter">
            || $<name> = [<[/!]>]

                # bare sigil?
            ]?
            {{
                my $vname = $<sigil>.Str;
                my $t = $<twigil>;
                my $twigil = '';
                $twigil = $t.[0].Str if @$t;
                $vname ~= $twigil;
                my $n = try { $<name>[0].Str } // '';
                $vname ~= $n;
                if $twigil eq '' {
                    self.add_my_name($vname) if $n ne '';
                    # :$param is often used as a multi matcher without $param used in body
                    #   so don't count as "declared but not used"
                    $*CURLEX{$vname}<used> = 1 if $named and $n;
                }
                elsif $twigil eq '.' { }
                elsif $twigil eq '!' { }
                elsif $twigil eq '*' { }
                else {
                    self.panic("You may not use the $twigil twigil in a signature");
                }
            }}
        ]
    }

    token parameter {
        :my $kind;
        :my $quant = '';
        :my $*DECLARAND;
        :my $*OFTYPE;

        [
        | <type_constraint>+
            {{
                my $t = $<type_constraint>;
                my @t = grep { substr($^tc.Str,0,2) ne '::' }, @$t;
                @t > 1 and $¢.sorry("Multiple prefix constraints not yet supported")
            }}
            [
            | '**' <param_var>   { $quant = '**'; $kind = '*'; }
            | '*' <param_var>   { $quant = '*'; $kind = '*'; }
            | '|' <param_var>   { $quant = '|'; $kind = '*'; }
            | '\\' <param_var>  { $quant = '\\'; $kind = '!'; }
            |   [
                | <param_var>   { $quant = ''; $kind = '!'; }
                | <named_param> { $quant = ''; $kind = '*'; }
                ]
                [
                | '?'           { $quant = '?'; $kind = '?' }
                | '!'           { $quant = '!'; $kind = $kind // '!' }
                | <?>
                ]
            | <?> { $quant = ''; $kind = '!' }
            ]

        | '**' <param_var>   { $quant = '**'; $kind = '*'; }
        | '*' <param_var>   { $quant = '*'; $kind = '*'; }
        | '|' <param_var>   { $quant = '|'; $kind = '*'; }
        | '\\' <param_var>  { $quant = '\\'; $kind = '!'; }
        |   [
            | <param_var>   { $quant = ''; $kind = '!'; }
            | <named_param> { $quant = ''; $kind = '*'; }
            ]
            [
            | '?'           { $quant = '?'; $kind = '?' }
            | '!'           { $quant = '!'; $kind = $kind // '!' }
            | <?>
            ]
        | {} <longname> <.panic("In parameter declaration, typename '" ~ $<longname>.Str ~ "' must be predeclared (or marked as declarative with :: prefix)")>
        ]

        <trait>*

        <post_constraint>*

        <.getdecl>

        [
            <default_value> {{
                if $quant eq '!' { $¢.sorry("Can't put a default on a required parameter") }
                elsif $quant eq '*' { $¢.sorry("Can't put a default on a slurpy parameter") }
                elsif $quant eq '**' { $¢.sorry("Can't put a default on a slice parameter") }
                elsif $quant eq '|' { $¢.sorry("Can't put a default on an slurpy capture parameter") }
                elsif $quant eq '\\' { $¢.sorry("Can't put a default on a capture parameter") }
                $kind = '?' if $kind eq '!';
            }}
            [<?before ':' > <.sorry: "Can't put a default on the invocant parameter">]?
            [<!before <[,;)\]\{\-]> > <.sorry: "Default expression must come last">]?
        ]?
        [<?before ':'> <?{ $kind ne '!' }> <.sorry: "Invocant is too exotic">]?

        {
            $<quant> = $quant;
            $<kind> = $kind;
        }

        # enforce zone constraints
        {{
            if $kind eq '!' {
                if $*zone eq 'posopt' {
    $¢.sorry("Can't put required parameter after optional parameters");
                }
                elsif $*zone eq 'var' {
    $¢.sorry("Can't put required parameter after variadic parameters");
                }
            }
            elsif $kind eq '?' {
                if $*zone eq 'posreq' { $*zone = 'posopt' }
                elsif $*zone eq 'var' {
$¢.sorry("Can't put optional positional parameter after variadic parameters");
                }
            }
            elsif $kind eq '*' {
                $*zone = 'var';
            }
        }}
    }

    rule default_value {
        :my $*IN_DECL = '';
        '=' <EXPR(item %item_assignment)>
    }

    token statement_prefix:sink    { <sym> <blast> }
    token statement_prefix:try     { <sym> <blast> }
    token statement_prefix:quietly { <sym> <blast> }
    token statement_prefix:gather  { <sym> <blast> }
    token statement_prefix:contend { <sym> <blast> }
    token statement_prefix:async   { <sym> <blast> }
    token statement_prefix:maybe   { <sym> <blast> }
    token statement_prefix:lazy    { <sym> <blast> }
    token statement_prefix:do      { <sym> <blast> }

    token statement_prefix:lift    {
        :my $*QUASIMODO = 1;
        <sym> <blast>
    }

    # accepts blocks and statements
    token blast {
        <?before \s> <.ws>
        [
        | <block>
        | <statement>  # creates a dynamic scope but not lexical scope
        ]
    }

    #########
    # Terms #
    #########

    token term:new {
        'new' \h+ <longname> \h* <!before ':'> <.obs("C++ constructor syntax", "method call syntax")>
    }

    token term:sym<::?IDENT> {
        $<sym> = [ '::?' <identifier> ] »
        <O(|%term)>
    }

    token term:sym<Object> {
        <sym> » {}
        <.obs('Object', 'Mu as the "most universal" object type')>
    }

    token term:sym<undef> {
        <sym> » {}
        [ <?before \h*'$/' >
            <.obs('$/ variable as input record separator',
                 "the filehandle's .slurp method")>
        ]?
        [ <?before [ '(' || \h*<sigil><twigil>?\w ] >
            <.obs('undef as a verb', 'undefine function or assignment of Nil')>
        ]?
        <.obs('undef as a value', "something more specific:\n\tMu (the \"most undefined\" type object),\n\tan undefined type object such as Int,\n\tNil as an empty list,\n\t:!defined as a matcher,\n\tAny:U as a type constraint\n\tor fail() as a failure return\n\t   ")>
    }

    token term:sym<proceed>
        { <sym> » <O(|%term)> }

    token term:sym<time>
        { <sym> » <O(|%term)> }

    token term:sym<now>
        { <sym> » <O(|%term)> }

    token term:sym<self>
        { <sym> » <O(|%term)> }

    token term:sym<defer>
        { <sym> » <O(|%term)> }

    token term:rand {
        <sym> »
        [ <?before '('? \h* [\d|'$']> <.obs('rand(N)', 'N.rand or (1..N).pick')> ]?
        [ <?before '()'> <.obs('rand()', 'rand')> ]?
        <O(|%term)>
    }

    token term:sym<*>
        { <sym> <O(|%term)> }

    token term:sym<**>
        { <sym> <O(|%term)> }

    token infix:lambda {
        <?before '{' | '->' > <!{ $*IN_META }> {{
            my $needparens = 0;
            my $line = $¢.lineof($¢.pos);
            for 'if', 'unless', 'while', 'until', 'for', 'given', 'when', 'loop', 'sub', 'method' -> $loopy {
                $needparens++ if $loopy eq 'loop';
                my $m = %*MYSTERY{$loopy};
                next unless $m;
                if $line - ($m.<line>//-123) < 5 {
                    if $m.<ctx> eq '(' {
                        $¢.panic("Word '$loopy' interpreted as '$loopy" ~ "()' function call; please use whitespace " ~
                        ($needparens ?? 'around the parens' !! 'instead of parens') ~ $m<token>.locmess ~
                        "\nUnexpected block in infix position (two terms in a row)");
                    }
                    else {
                        $¢.panic("Word '$loopy' interpreted as a listop; please use 'do $loopy' to introduce the statement control word" ~ $m<token>.cursor($m<token>.from).locmess ~
                        "\nUnexpected block in infix position (two terms in a row)");
                    }
                }
            }
            return () if $*IN_REDUCE;
            my $endpos = $¢.pos;
            my $startpos = @*MEMOS[$endpos]<ws> // $endpos;

            if self.lineof($startpos) != self.lineof($endpos) {
                $¢.panic("Unexpected block in infix position (previous line missing its semicolon?)");
            }
            elsif @*MEMOS[$¢.pos-1]<baremeth> {
                $¢.panic("Unexpected block in infix position (method call needs colon or parens to take arguments)");
            }
            else {
                $¢.panic("Unexpected block in infix position (two terms in a row, or previous statement missing semicolon?)");
            }
        }}
        <O(|%term)>
    }

    token circumfix:sigil
        { :dba('contextualizer') <sigil> '(' ~ ')' <semilist> { $*LEFTSIGIL = $*LEFTSIGIL || $<sigil>.Str } <O(|%term)> }

    token circumfix:sym<( )>
        { :dba('parenthesized expression') '(' ~ ')' <semilist> <O(|%term)> }

    token circumfix:sym<[ ]>
        { :dba('array composer') '[' ~ ']' <semilist> <O(|%term)> { @*MEMOS[$¢.pos]<arraycomp> = 1; } }

    #############
    # Operators #
    #############

    token PRE {
        :dba('prefix or meta-prefix')
        [
        | <prefix>
            { $<O> = $<prefix><O>; $<sym> = $<prefix><sym> }
        | <prefix_circumfix_meta_operator>
            { $<O> = $<prefix_circumfix_meta_operator><O>; $<sym> = $<prefix_circumfix_meta_operator>.Str }
        ]
        # XXX assuming no precedence change
        
        <prefix_postfix_meta_operator>*
        <.ws>
    }

    token infixish ($in_meta = $*IN_META) {
        :my $infix;
        :my $*IN_META = $in_meta;
        <!stdstopper>
        <!infixstopper>
        :dba('infix or meta-infix')
        [
        | <colonpair> {
                $<fake> = 1;
                $<sym> = ':';
                %<O><prec> = %item_assignment<prec>;  # actual test is non-inclusive!
                %<O><assoc> = 'unary';
                %<O><dba> = 'adverb';
            }
        |   [
            | :dba('bracketed infix') '[' ~ ']' <infix=.infixish('[]')> { $<O> = $<infix><O>; $<sym> = $<infix><sym>; }
            | <infix=infix_circumfix_meta_operator> { $<O> = $<infix><O>; $<sym> = $<infix><sym>; }
            | <infix=infix_prefix_meta_operator>    { $<O> = $<infix><O>; $<sym> = $<infix><sym>; }
            | <infix>                               { $<O> = $<infix><O>; $<sym> = $<infix><sym>; }
            | {} <?dotty> <.panic: "Method call found where infix expected (omit whitespace?)">
            | {} <?postfix> <.panic: "Postfix found where infix expected (omit whitespace?)">
            ]
            [ <?before '='> <?{ $infix = $<infix>; }> <assign_meta_operator($infix)>
                   { $<O> = $<assign_meta_operator>[0]<O>; $<sym> = $<assign_meta_operator>[0]<sym>; }
            ]?

        ]
    }

    # NOTE: Do not add dotty ops beginning with anything other than dot!
    #   Dotty ops have to parse as .foo terms as well, and almost anything
    #   other than dot will conflict with some other prefix.

    # doing fancy as one rule simplifies LTM
    token dotty:sym<.*> {
        ('.' [ <[+*?=]> | '^' '!'? ]) :: <.unspacey> <dottyop>
        { $<sym> = $0.Str; }
        <O(%methodcall)>
    }

    token dotty:sym<.> {
        <sym> <.unspacey> <dottyop>
        <O(%methodcall)>
    }

    token privop {
        '!' <methodop>
        <O(%methodcall)>
    }

    token dottyopish {
        <term=.dottyop>
    }

    token dottyop {
        :dba('dotty method or postfix')
        [
        | <methodop>
        | <colonpair>
        | <!alpha> <postop> { $<O> = $<postop><O>; $<sym> = $<postop><sym>; }  # only non-alpha postfixes have dotty form
        ]
    }

    # Note, this rule mustn't do anything irreversible because it's used
    # as a lookahead by the quote interpolator.

    token POST {
        <!stdstopper>

        # last whitespace didn't end here
        <!{ @*MEMOS[$¢.pos]<ws> }>

        [ <.unsp> | '\\' ]?

        [ ['.' <.unsp>?]? <postfix_prefix_meta_operator> <.unsp>? ]*

        :dba('postfix')
        [
        | <dotty>  { $<O> = $<dotty><O>;  $<sym> = $<dotty><sym>;  $<~CAPS> = $<dotty><~CAPS>; }
        | <privop> { $<O> = $<privop><O>; $<sym> = $<privop><sym>; $<~CAPS> = $<privop><~CAPS>; }
        | <postop> { $<O> = $<postop><O>; $<sym> = $<postop><sym>; $<~CAPS> = $<postop><~CAPS>; }
        ]
        { $*LEFTSIGIL = '@'; }
    }

    method can_meta ($op, $meta) {
        !$op<O><fiddly> ||
            self.sorry("Can't " ~ $meta ~ " " ~ $op<sym> ~ " because " ~ $op<O><dba> ~ " operators are too fiddly");
        self;
    }

    regex prefix_circumfix_meta_operator:reduce {
        :my $*IN_REDUCE = 1;
        :my $op;
        <?before '['\S+']'>
        $<s> = (
            '['
            [
            || <op=.infixish('red')> <?before ']'>
            || \\<op=.infixish('tri')> <?before ']'>
            || <!>
            ]
            ']' ['«'|<?>]
        )
        { $op = $<s><op>; }

        <.can_meta($op, "reduce with")>

        [
        || <!{ $op<O><diffy> }>
        || <?{ $op<O><assoc> eq 'chain' }>
        || <.sorry("Can't reduce with " ~ $op<sym> ~ " because " ~ $op<O><dba> ~ " operators are diffy and not chaining")>
        ]

        <O(|($op<O>), |%list_prefix, assoc => 'unary', uassoc => 'left')>
        { $<sym> = $<s>.Str; }

        [ <?before '('> || <?before \s+ [ <?stdstopper> { $<O><term> = 1 } ]? > || { $<O><term> = 1 } ]
    }

    token prefix_postfix_meta_operator:sym< « >    { <sym> | '<<' }

    token postfix_prefix_meta_operator:sym< » >    {
        [ <sym> | '>>' ]
        # require >>.( on interpolated hypercall so infix:«$s»($a,$b) {...} dwims
        [<!{ $*QSIGIL }> || <!before '('> ]
    }

    token infix_prefix_meta_operator:sym<!> {
        <sym> <!before '!'> {} [ <infixish('neg')> || <.panic: "Negation metaoperator not followed by valid infix"> ]

        [
        || <?{ $<infixish>.Str eq '=' }>
           <O(|%chaining)>
           
        || <.can_meta($<infixish>, "negate")>    
           <?{ $<infixish><O><iffy> }>
           <?{ $<O> = $<infixish><O>; }>
            
        || <.panic("Can't negate " ~ $<infixish>.Str ~ " because " ~ $<infixish><O><dba> ~ " operators are not iffy enough")>
        ]
    }

    token infix_prefix_meta_operator:sym<R> {
        <sym> {} <infixish('R')>
        <.can_meta($<infixish>, "reverse the args of")>
        <?{ $<O> = $<infixish><O>; }>
    }

    token infix_prefix_meta_operator:sym<S> {
        <sym> {} <infixish('S')>
        <.can_meta($<infixish>, "sequence the args of")>
        <?{ $<O> = $<infixish><O>; }>
    }

    token infix_prefix_meta_operator:sym<X> {
        <sym> <?before \S> {}
        [ <infixish('X')>
            <.can_meta($<infixish>[0], "cross with")>
            <?{ $<O> = $<infixish>[0]<O>; $<O><prec>:delete; $<sym> ~= $<infixish>[0].Str }>
        ]?
        <O(|%list_infix, self.Opairs)>
    }

    token infix_prefix_meta_operator:sym<Z> {
        <sym> <?before \S> {}
        [ <infixish('Z')>
            <.can_meta($<infixish>[0], "zip with")>
            <?{ $<O> = $<infixish>[0]<O>; $<O><prec>:delete; $<sym> ~= $<infixish>[0].Str }>
        ]?
        <O(|%list_infix, self.Opairs)>
    }

    token infix_circumfix_meta_operator:sym<« »> {
        [
        | '«'
        | '»'
        ]
        {} <infixish('hyper')> [ '«' | '»' || <.panic: "Missing « or »"> ]
        <.can_meta($<infixish>, "hyper with")>
        <?{ $<O> := $<infixish><O>; }>
    }

    token infix_circumfix_meta_operator:sym«<< >>» {
        [
        | '<<'
        | '>>'
        ]
        {} <infixish('HYPER')> [ '<<' | '>>' || <.panic("Missing << or >>")> ]
        <.can_meta($<infixish>, "hyper with")>
        <?{ $<O> := $<infixish><O>; }>
    }

    token assign_meta_operator ($op) {
        :my %prec;
        '='
        <.can_meta($op, "make assignment out of")>
        [ <!{ $op<O><diffy> }> || <.sorry("Can't make assignment out of " ~ $op<sym> ~ " because " ~ $op<O><dba> ~ " operators are diffy")> ]
        {
            $<sym> = $op<sym> ~ '=';
            if $op<O><prec> gt %comma<prec> {
                %prec = %item_assignment;
            }
            else {
                %prec = %list_assignment;
            }
        }
        <O($op.Opairs, |%prec, dba => 'assignment operator', iffy => 0)>
    }

    token postcircumfix:sym<( )>
        { :dba('argument list') '(' ~ ')' <semiarglist> <O(|%methodcall)> }

    token postcircumfix:sym<[ ]> { :dba('subscript') '[' ~ ']' <semilist> <O(|%methodcall)> 
        {
            my $innards = $<semilist>.Str;
            if $innards ~~ /^\s*\-\d+\s*$/ {
                $¢.obs("[$innards] subscript to access from end of array","[*$innards]");
            }
        }
    }

    token postcircumfix:sym<{ }>
        { :dba('subscript') '{' ~ '}' <semilist> <O(|%methodcall)> <.curlycheck> }

    token postcircumfix:sym«< >» {
        :my $pos;
        '<'
        { $pos = $¢.pos }
        [
        || <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q).tweak(:w).balanced('<','>'))> '>'
        || <?before \h* [ \d | <sigil> | ':' ] >
           { $¢.cursor_force($pos).panic("Whitespace required before < operator") }
        || { $¢.cursor_force($pos).panic("Unable to parse quote-words subscript; couldn't find right angle quote") }
        ]
        <O(|%methodcall)>
    }

    token postcircumfix:sym«<< >>»
        { '<<' <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).tweak(:ww).balanced('<<','>>'))> [ '>>' || <.panic: "Unable to parse quote-words subscript; couldn't find right double-angle quote"> ] <O(|%methodcall)> }

    token postcircumfix:sym<« »>
        { '«' <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).tweak(:ww).balanced('«','»'))> [ '»' || <.panic: "Unable to parse quote-words subscript; couldn't find right double-angle quote"> ] <O(|%methodcall)> }

    token postop {
        | <postfix>         { $<O> = $<postfix><O>; $<sym> = $<postfix><sym>; }
        | <postcircumfix>   { $<O> = $<postcircumfix><O>; $<sym> = $<postcircumfix><sym>; }
    }

    token methodop {
        [
        | <longname>
        | <?before '$' | '@' | '&' > <variable> { $¢.check_variable($<variable>) }
        | <?before <[ ' " ]> >
            [ <!{$*QSIGIL}> || <!before '"' <-["]>*? \s > ] # dwim on "$foo."
            <quote>
            [ <?before '(' | '.(' | '\\'> || <.obs('. to concatenate strings or to call a quoted method', '~ to concatenate, or if you meant to call a quoted method, please supply the required parentheses')> ]
            { my $t = $<quote><nibble>.Str; $t ~~ /\W/ or $t ~~ /^(WHO|WHAT|WHERE|WHEN|WHY|HOW)$/ or $¢.worry("Useless use of quotes") }
        ] <.unsp>? 

        :dba('method arguments')
        [
        | ':' <?before \s | '{'> <!{ $*QSIGIL }> <arglist>
        | <?[\\(]> <args>
        | { @*MEMOS[$¢.pos]<baremeth> = 1 }
        ]?
    }

    token semiarglist {
        <arglist> ** ';'
        <.ws>
    }

    token arglist {
        :my $inv_ok = $*INVOCANT_OK;
        :my $*endargs = 0;
        :my $*GOAL ::= 'endargs';
        :my $*QSIGIL ::= '';
        <.ws>
        :dba('argument list')
        [
        | <?stdstopper>
        | <EXPR(item %list_prefix)> {{
                my $delims = $<EXPR><delims>;
                for @$delims -> $d {
                    if $d.<infix><wascolon> // '' {
                        if $inv_ok {
                            $*INVOCANT_IS = $<EXPR><list>[0];
                        }
                    }
                }
            }}
        ]
    }

    token term:lambda {
        <?before <.lambda> >
        <pblock>
        {{
            if $*BORG {
                $*BORG.<block> = $<pblock>;
            }
        }}
        <O(|%term)>
    }

    token circumfix:sym<{ }> {
        <?before '{' >
        <pblock>
        {{
            if $*BORG {
                $*BORG.<block> = $<pblock>;
            }
        }}
        <O(|%term)>
    }

    ## methodcall

    token postfix:sym<i>
        { <sym> » <O(|%methodcall)> }

    token infix:sym<.> ()
        { '.' <[\]\)\},:\s\$"']> <.obs('. to concatenate strings', '~')> }

    token postfix:sym['->'] () {
        '->'
        [
        | <brack=[ \[ \{ \( ]> <.obs("'->" ~ $<brack>.Str ~ "' as postfix dereferencer", "'." ~ $<brack>.Str ~ "' or just '" ~ $<brack>.Str ~ "' to deref, or whitespace to delimit a pointy block")>
        | <.obs('-> as postfix', 'either . to call a method, or whitespace to delimit a pointy block')>
        ]
    }

    ## autoincrement
    token postfix:sym<++>
        { <sym> <O(|%autoincrement)> }

    token postfix:sym«--» ()
        { <sym> <O(|%autoincrement)> }

    token prefix:sym<++>
        { <sym> <O(|%autoincrement)> }

    token prefix:sym«--» ()
        { <sym> <O(|%autoincrement)> }

    ## exponentiation
    token infix:sym<**>
        { <sym> <O(|%exponentiation)> }

    ## symbolic unary
    token prefix:sym<!>
        { <sym> <O(|%symbolic_unary)> }

    token prefix:sym<+>
        { <sym> <O(|%symbolic_unary)> }

    token prefix:sym<->
        { <sym> <O(|%symbolic_unary)> }

    token prefix:sym<~~>
        { <sym> <.dupprefix('~~')> <O(|%symbolic_unary)> }

    token prefix:sym<~>
        { <sym> <O(|%symbolic_unary)> }

    token prefix:sym<??>
        { <sym> <.dupprefix('??')> <O(|%symbolic_unary)> }

    token prefix:sym<?>
        { <sym> <O(|%symbolic_unary)> }

    token prefix:sym<~^>
        { <sym> <O(|%symbolic_unary)> }

    token prefix:sym<+^>
        { <sym> <O(|%symbolic_unary)> }

    token prefix:sym<?^>
        { <sym> <O(|%symbolic_unary)> }

    token prefix:sym<^^>
        { <sym> <.dupprefix('^^')> <O(|%symbolic_unary)> }

    token prefix:sym<^>
        { <sym> <O(|%symbolic_unary)> }

    token prefix:sym<||>
        { <sym> <O(|%symbolic_unary)> }

    token prefix:sym<|>
        { <sym> <O(|%symbolic_unary)> }


    ## multiplicative
    token infix:sym<*>
        { <sym> <O(|%multiplicative)> }

    token infix:sym</>
        { <sym> <O(|%multiplicative)> }

    token infix:sym<div>
        { <sym> <O(|%multiplicative)> }

    token infix:sym<%>
        { <sym> <O(|%multiplicative)> }

    token infix:sym<%%>
        { <sym> <O(|%multiplicative, iffy => 1)> }      # "is divisible by" returns Bool

    token infix:sym<mod>
        { <sym> <O(|%multiplicative)> }

    token infix:sym<+&>
        { <sym> <O(|%multiplicative)> }

    token infix:sym« << »
        { <sym> <!{ $*IN_META }> <?before \s> <.sorryobs('<< to do left shift', '+< or ~<')> <O(|%multiplicative)> }

    token infix:sym« >> »
        { <sym> <!{ $*IN_META }> <?before \s> <.sorryobs('>> to do right shift', '+> or ~>')> <O(|%multiplicative)> }

    token infix:sym<~&>
        { <sym> <O(|%multiplicative)> }

    token infix:sym<?&>
        { <sym> <O(|%multiplicative, iffy => 1)> }

    # try to allow both of >>op<< and >>op<<< without allowing op<<
    token infix:sym« ~< »
        { <sym> [ <!{ $*IN_META }> || <?before '<<'> || <!before '<'> ] <O(|%multiplicative)> }

    token infix:sym« ~> »
        { <sym> [ <!{ $*IN_META }> || <?before '>>'> || <!before '>'> ] <O(|%multiplicative)> }

    token infix:sym« +< »
        { <sym> [ <!{ $*IN_META }> || <?before '<<'> || <!before '<'> ] <O(|%multiplicative)> }

    token infix:sym« +> »
        { <sym> [ <!{ $*IN_META }> || <?before '>>'> || <!before '>'> ] <O(|%multiplicative)> }

    ## additive
    token infix:sym<+>
        { <sym> <!before '+'> <O(|%additive)> }

    token infix:sym<->
        { <sym> <!before '-'> <O(|%additive)> }

    token infix:sym<+|>
        { <sym> <O(|%additive)> }

    token infix:sym<+^>
        { <sym> <O(|%additive)> }

    token infix:sym<~|>
        { <sym> <O(|%additive)> }

    token infix:sym<~^>
        { <sym> <O(|%additive)> }

    token infix:sym<?|>
        { <sym> <O(|%additive, iffy => 1)> }

    token infix:sym<?^>
        { <sym> <O(|%additive)> }

    ## replication
    # Note: no word boundary check after x, relies on longest token for x2 xx2 etc
    token infix:sym<x>
        { <sym> <O(|%replication)> }

    token infix:sym<xx>
        { <sym> <O(|%replication)> }

    ## concatenation
    token infix:sym<~>
        { <sym> <O(|%concatenation)> }


    ## junctive and (all)
    token infix:sym<&>
        { <sym> <O(|%junctive_and, iffy => 1)> }


    ## junctive or (any)
    token infix:sym<|>
        { <sym> <O(|%junctive_or, iffy => 1)> }

    token infix:sym<^>
        { <sym> <O(|%junctive_or, iffy => 1)> }


    ## named unary examples
    # (need \s* to win LTM battle with listops)
    token prefix:sleep
        { <sym> » <?before \s*> <O(|%named_unary)> }

    token prefix:abs
        { <sym> » <?before \s*> <O(|%named_unary)> }

    token prefix:let
        { <sym> » <?before \s*> <O(|%named_unary)> }

    token prefix:temp
        { <sym> » <?before \s*> <O(|%named_unary)> }


    ## structural infix
    token infix:sym« <=> »
        { <sym> <O(|%structural, returns => 'Order')> }

    token infix:cmp
        { <sym> <O(|%structural, returns => 'Order')> }

    token infix:leg
        { <sym> <O(|%structural, returns => 'Order')> }

    token infix:but
        { <sym> <O(|%structural)> }

    token infix:does
        { <sym> <O(|%structural)> }

    token infix:sym<..>
        { <sym> [<!{ $*IN_META }> <?before ')' | ']'> <.panic: "Please use ..* for indefinite range">]? <O(|%structural)> }

    token infix:sym<^..>
        { <sym> <O(|%structural)> }

    token infix:sym<..^>
        { <sym> <O(|%structural)> }

    token infix:sym<^..^>
        { <sym> <O(|%structural)> }


    ## chaining binary
    token infix:sym<==>
        { <sym> <!before '=' > <O(|%chaining)> }

    token infix:sym<!=>
        { <sym> <?before \s> <O(|%chaining)> }

    token infix:sym« < »
        { <sym> <!before '<'> <O(|%chaining)> }

    token infix:sym« <= »
        { <sym> <O(|%chaining)> }

    token infix:sym« > »
        { <sym> <!before '>'> <O(|%chaining)> }

    token infix:sym« >= »
        { <sym> <O(|%chaining)> }

    token infix:sym<~~>
        { <sym> <O(|%chaining)> <?dumbsmart> }

    token dumbsmart {
        [ \h*
            ('True'|'False'|'Bool::True'|'Bool::False') <?before \s>
            {
                my $litbool = $0[0].Str;
                my $true = $litbool ~~ /True/;
                self.worry("Smartmatch against $litbool always " ~
                    ($true ?? 'matches' !! 'fails') ~
                    "; if you mean to test the topic for\n    truthiness, please use " ~
                    ($true ?? ':so or *.so or ?*' !! ':!so or *.not or !*') ~
                    ' instead');
            }
        ]?
    }

    # XXX should move to inside meta !
    token infix:sym<!~>
        { <sym> \s <.obs('!~ to do negated pattern matching', '!~~')> <O(|%chaining)> }

    token infix:sym<=~>
        { <sym> <.obs('=~ to do pattern matching', '~~')> <O(|%chaining)> }

    token infix:sym<eq>
        { <sym> <O(|%chaining)> }

    token infix:sym<ne>
        { <sym> <O(|%chaining)> }

    token infix:sym<lt>
        { <sym> <O(|%chaining)> }

    token infix:sym<le>
        { <sym> <O(|%chaining)> }

    token infix:sym<gt>
        { <sym> <O(|%chaining)> }

    token infix:sym<ge>
        { <sym> <O(|%chaining)> }

    token infix:sym<=:=>
        { <sym> <O(|%chaining)> }

    token infix:sym<===>
        { <sym> <O(|%chaining)> }

    token infix:sym<eqv>
        { <sym> <O(|%chaining)> }

    token infix:sym<before>
        { <sym> <O(|%chaining)> }

    token infix:sym<after>
        { <sym> <O(|%chaining)> }


    ## tight and
    token infix:sym<&&>
        { <sym> <O(|%tight_and, iffy => 1)> }


    ## tight or
    token infix:sym<||>
        { <sym> <O(|%tight_or, iffy => 1)> }

    token infix:sym<^^>
        { <sym> <O(|%tight_or, iffy => 1)> }

    token infix:sym<//>
        { <sym> <O(|%tight_or)> }

    token infix:sym<min>
        { <sym> <O(|%tight_or)> }

    token infix:sym<max>
        { <sym> <O(|%tight_or)> }


    ## conditional
    token infix:sym<?? !!> {
        :my $*GOAL ::= '!!';
        '??'
        <.ws>
        <EXPR(item %item_assignment)>
        [ '!!'
        || <?before '::'<-[=]>> <.panic: "Please use !! rather than ::">
        || <infixish> {{
                my $b = $<infixish>.Str;
                if $b eq ':' {
                    $¢.panic("Please use !! rather than $b");
                }
                else {
                    $¢.panic("Precedence of $b is too loose to use between ?? and !!; please use parens around inner expression");
                }
            }}
        || <?before \N*? [\n\N*?]?> '!!' <.sorry("Bogus code found before the !!")> <.panic("Confused")>
        || <.sorry("Found ?? but no !!")> <.panic("Confused")>
        ]
        <O(|%conditional, _reducecheck => &raise_middle)>
    }

    token infix:sym<!!> {
        <sym> ::
        [
        || <.suppose <infixish>> <.panic: "An infix may not start with !!">
        || <.panic: "Ternary !! seems to be missing its ??">
        ]
    }

    sub raise_middle($node) {
        $node<middle> = $node<infix><EXPR>;
        $node;
    }

    token infix:sym<?>
        { <sym> {} <!before '?'> <?before <-[;]>*?':'> <.obs('?: for the conditional operator', '??!!')> <O(|%conditional)> }

    token infix:sym<ff>
        { <sym> <O(|%conditional)> }

    token infix:sym<^ff>
        { <sym> <O(|%conditional)> }

    token infix:sym<ff^>
        { <sym> <O(|%conditional)> }

    token infix:sym<^ff^>
        { <sym> <O(|%conditional)> }

    token infix:sym<fff>
        { <sym> <O(|%conditional)> }

    token infix:sym<^fff>
        { <sym> <O(|%conditional)> }

    token infix:sym<fff^>
        { <sym> <O(|%conditional)> }

    token infix:sym<^fff^>
        { <sym> <O(|%conditional)> }

    ## assignment

    token infix:sym<=> ()
    {
        <sym>
        [
        || <?{ $*LEFTSIGIL eq '$' }>
            <O(|%item_assignment)>
        ||  <O(|%list_assignment)>
        ]
    }

    token infix:sym<:=>
        { <sym> <O(|%list_assignment)> }

    token infix:sym<::=>
        { <sym> <O(|%list_assignment)> }

    token infix:sym<.=> {
        <sym>
        <O(|%item_assignment,
            nextterm => 'dottyopish',
            _reducecheck => &check_doteq
        )>
    }

    sub check_doteq($node) {
        # [ <?before \w+';' | 'new'|'sort'|'subst'|'trans'|'reverse'|'uniq'|'map'|'samecase'|'substr'|'flip'|'fmt'|'pick' > || ]
        return $node if $node<left><scope_declarator>;
        my $ok = 0;

        try {
            my $methop = $node<right><methodop>;
            my $name = $methop<longname>.Str;
            if grep { $^valid eq $name }, <new clone sort subst trans reverse uniq map samecase substr flip fmt pick> {
                $ok = 1;
            }
            elsif not $methop.<args>[0] {
                $ok = 1;
            }
        };

        # NIECZA cursor_force was used here
        $node.CURSOR.cursor($node<infix>.to).worryobs('.= as append operator', '~=') unless $ok;
        $node;
    }

    token infix:sym« => »
        { <sym> <O(|%item_assignment, fiddly => 0)> }

    # Note, other assignment ops generated by assign_meta_operator rule

    ## loose unary
    token prefix:sym<so>
        { <sym> » <O(|%loose_unary)> }

    token prefix:sym<not>
        { <sym> » <O(|%loose_unary)> }

    ## list item separator
    token infix:sym<,> {
        <sym> <O(|%comma, fiddly => 0)>
        [ <?before \h*'...'> <.worry: "Comma found before apparent series operator; please remove comma (or put parens\n    around the ... listop, or use 'fail' instead of ...)"> ]?
    }

    token infix:sym<:> {
        ':' <?before \s | <terminator> >
        {
            $¢.sorry("Illegal use of colon as invocant marker") unless $*INVOCANT_OK-- or $*PRECLIM ge $item_assignment_prec;
            $<sym> = ',';
            $<wascolon> = True;
        }
        <O(|%comma)>
    }

    token infix:sym<X>
        { <sym> <O(|%list_infix)> }

    token infix:sym<Z>
        { <sym> <O(|%list_infix)> }

    token infix:sym<minmax>
        { <sym> <O(|%list_infix)> }

    token infix:sym<...>
        { <sym> <O(|%list_infix)> }

    token term:sym<...>
        { <sym> <args>? <O(|%list_prefix)> }

    token term:sym<???>
        { <sym> <args>? <O(|%list_prefix)> }

    token term:sym<!!!>
        { <sym> <args>? <O(|%list_prefix)> }

    my %deftrap = (
        :say, :print, :abs, :alarm, :chomp, :chop, :chr, :chroot, :cos,
        :defined, :eval, :exp, :glob, :lc, :lcfirst, :log, :lstat, :mkdir,
        :ord, :readlink, :readpipe, :require, :reverse, :rmdir, :sin,
        :split, :sqrt, :stat, :uc, :ucfirst, :unlink,
        :WHAT, :WHICH, :WHERE, :HOW, :WHENCE, :VAR,
    );

    # force identifier(), identifier.(), etc. to be a function call always
    token term:identifier
    {
        :my $name;
        :my $pos;
        :my $isname = 0;
        <identifier> <?before [<unsp>|'(']? > <![:]>
        {{
            $name = $<identifier>.Str;
            $pos = $¢.pos;
            $isname = $¢.is_name($name);
            $¢.check_nodecl($name) if $isname;
        }}
        <args($isname)>
        { self.add_mystery($<identifier>,$pos,substr(self.orig,$pos,1)) unless $<args><invocant>; }
        {{
            if $*BORG and $*BORG.<block> {
                if not $*BORG.<name> {
                    $*BORG.<culprit> = $<identifier>.cursor($pos);
                    $*BORG.<name> = $name;
                }
            }
            if %deftrap{$name} {
                my $al = $<args><arglist>[0];
                my $ok = 0;
                $ok = 1 if $al and $al.from != $al.to;
                $ok = 1 if $<args><semiarglist>;
                if not $ok {
                    $<identifier>.worryobs("bare '$name'", ".$name if you meant \$_, or use an explicit invocant or argument");
                }
            }
        }}
        <O(|%term)>
    }

    token args ($istype = 0) {
        :my $listopish = 0;
        :my $*GOAL ::= '';
        :my $*INVOCANT_OK = 1;
        :my $*INVOCANT_IS;
        [
    #    | :dba('argument list') '.(' ~ ')' <semiarglist>
        | :dba('argument list') '(' ~ ')' <semiarglist>
        | :dba('argument list') <.unsp> '(' ~ ')' <semiarglist>
        |  { $listopish = 1 } [<?before \s> <!{ $istype }> <.ws> <!infixstopper> <arglist>]?
        ]
        { $<invocant> = $*INVOCANT_IS; }

        :dba('extra arglist after (...):')
        [
        || <?{ $listopish }>
        || ':' <?before \s> <moreargs=.arglist>    # either switch to listopiness
        || {{ $<O> = {}; }}   # or allow adverbs (XXX needs hoisting?)
        ]
    }

    # names containing :: may or may not be function calls
    # bare identifier without parens also handled here if no other rule parses it
    token term:name
    {
        :my $name;
        :my $pos;
        <longname>
        {
            $name = $<longname>.Str;
            $pos = $¢.pos;
        }
        [
        ||  <?{
                $¢.is_name($name) or substr($name,0,2) eq '::'
            }>
            { $¢.check_nodecl($name); }

            # parametric type?
            :dba('type parameter')
            <.unsp>? [ <?before '['> <postcircumfix> ]?

            :dba('namespace variable lookup')
            [
                <?after '::'>
                <?before [ '«' | '<' | '{' | '<<' ] > <postcircumfix>
                { $*VAR = $¢.cursor_all(self.pos, $¢.pos) }
            ]?

        # unrecognized names are assumed to be post-declared listops.
        || <args> { self.add_mystery($<longname>,$pos,'termish') unless $<args><invocant>; }
            {{
                if $*BORG and $*BORG.<block> {
                    if not $*BORG.<name> {
                        $*BORG.<culprit> = $<longname>.cursor($pos);
                        $*BORG.<name> = $*BORG<name> // $name;
                    }
                }
            }}
        ]
        <O(%term)>
    }

    method check_nodecl($name) {
        if $name lt 'a' {
            @*MEMOS[self.pos]<nodecl> = $name;
        }
    }

    ## loose and
    token infix:sym<and>
        { <sym> <O(|%loose_and, iffy => 1)> }

    token infix:sym<andthen>
        { <sym> <O(|%loose_and)> }

    ## loose or
    token infix:sym<or>
        { <sym> <O(|%loose_or, iffy => 1)> }

    token infix:sym<orelse>
        { <sym> <O(|%loose_or)> }

    token infix:sym<xor>
        { <sym> <O(|%loose_or, iffy => 1)> }

    ## sequencer
    token infix:sym« <== »
        { <sym> <O(|%sequencer)> }

    token infix:sym« ==> »
        { <sym> <O(|%sequencer)> }

    token infix:sym« <<== »
        { <sym> <O(|%sequencer)> }

    token infix:sym« ==>> »
        { <sym> <O(|%sequencer)> }

    ## expression terminator
    # Note: must always be called as <?terminator> or <?before ...<terminator>...>

    token terminator:sym<;>
        { ';' <O(|%terminator)> }

    token terminator:sym<if>
        { 'if' » <.nofun> <O(|%terminator)> }

    token terminator:sym<unless>
        { 'unless' » <.nofun> <O(|%terminator)> }

    token terminator:sym<while>
        { 'while' » <.nofun> <O(|%terminator)> }

    token terminator:sym<until>
        { 'until' » <.nofun> <O(|%terminator)> }

    token terminator:sym<for>
        { 'for' » <.nofun> <O(|%terminator)> }

    token terminator:sym<given>
        { 'given' » <.nofun> <O(|%terminator)> }

    token terminator:sym<when>
        { 'when' » <.nofun> <O(|%terminator)> }

    token terminator:sym« --> »
        { '-->' <O(|%terminator)> }

    token terminator:sym<!!>
        { '!!' <?{ $*GOAL eq '!!' }> <O(|%terminator)> }

    regex infixstopper {
        :dba('infix stopper')
        [
        | <?before <stopper> >
        | <?before '!!'> <?{ $*GOAL eq '!!' }>
        | <?before '{' | <lambda> > <?{ ($*GOAL eq '{' or $*GOAL eq 'endargs') and @*MEMOS[$¢.pos]<ws> }>
        | <?{ $*GOAL eq 'endargs' and @*MEMOS[$¢.pos]<endargs> }>
        ]
    }

} # end grammar

grammar Q is STD {

    role b1 {
        token escape:sym<\\> { <sym> {} <item=.backslash> }
        token backslash:qq { <?before 'q'> { $<quote> = $¢.cursor_fresh(%*LANG<MAIN>).quote(); } }
        token backslash:sym<\\> { <text=.sym> }
        token backslash:stopper { <text=.stopper> }
        token backslash:a { <sym> }
        token backslash:b { <sym> }
        token backslash:c { <sym> <charspec> }
        token backslash:e { <sym> }
        token backslash:f { <sym> }
        token backslash:n { <sym> }
        token backslash:o { :dba('octal character') <sym> [ <octint> | '[' ~ ']' <octints> ] }
        token backslash:r { <sym> }
        token backslash:t { <sym> }
        token backslash:x { :dba('hex character') <sym> [ <hexint> | '[' ~ ']' <hexints> ] }
        token backslash:sym<0> { <sym> }
    }

    role b0 {
        token escape:sym<\\> { <!> }
    }

    role c1 {
        token escape:sym<{ }> { <?before '{'> [ :lang(%*LANG<MAIN>) <embeddedblock> ] }
    }

    role c0 {
        token escape:sym<{ }> { <!> }
    }

    role s1 {
        token escape:sym<$> {
            :my $*QSIGIL ::= '$';
            <?before '$'>
            [ :lang(%*LANG<MAIN>) <EXPR(item %methodcall)> ] || <.panic: "Non-variable \$ must be backslashed">
        }
    }

    role s0 {
        token escape:sym<$> { <!> }
    }

    role a1 {
        token escape:sym<@> {
            :my $*QSIGIL ::= '@';
            <?before '@'>
            [ :lang(%*LANG<MAIN>) <EXPR(item %methodcall)> | <!> ] # trap ABORTBRANCH from variable's ::
        }
    }

    role a0 {
        token escape:sym<@> { <!> }
    }

    role h1 {
        token escape:sym<%> {
            :my $*QSIGIL ::= '%';
            <?before '%'>
            [ :lang(%*LANG<MAIN>) <EXPR(item %methodcall)> | <!> ]
        }
    }

    role h0 {
        token escape:sym<%> { <!> }
    }

    role f1 {
        token escape:sym<&> {
            :my $*QSIGIL ::= '&';
            <?before '&'>
            [ :lang(%*LANG<MAIN>) <EXPR(item %methodcall)> | <!> ]
        }
    }

    role f0 {
        token escape:sym<&> { <!> }
    }

    role p1 {
        method postprocess ($s) { $s.parsepath }
    }

    role p0 {
        method postprocess ($s) { $s }
    }

    role w1 {
        method postprocess ($s) { $s.words }
    }

    role w0 {
        method postprocess ($s) { $s }
    }

    role ww1 {
        method postprocess ($s) { $s.words }
    }

    role ww0 {
        method postprocess ($s) { $s }
    }

    role x1 {
        method postprocess ($s) { $s.run }
    }

    role x0 {
        method postprocess ($s) { $s }
    }

    role q {
        token stopper { \' }

        token escape:sym<\\> { <sym> <item=.backslash> }

        token backslash:qq { <?before 'q'> { $<quote> = $¢.cursor_fresh(%*LANG<MAIN>).quote(); } }
        token backslash:sym<\\> { <text=.sym> }
        token backslash:stopper { <text=.stopper> }

        # in single quotes, keep backslash on random character by default
        token backslash:misc { {} (.) { $<text> = "\\" ~ $0.Str; } }

        # NIECZA multi methods NYI
        method tweak(:single(:$q), :double(:$qq), :cclass(:$cc)) {
            if    $q.defined  { self.panic("Too late for :q") }
            elsif $qq.defined { self.panic("Too late for :qq") }
            elsif $cc.defined { self.panic("Too late for :cc") }
            else { nextsame }
        }
    }

    role qq does b1 does c1 does s1 does a1 does h1 does f1 {
        token stopper { \" }
        # in double quotes, omit backslash on random \W backslash by default
        token backslash:misc { {} [ (\W) { $<text> = $0.Str; } | $<x>=(\w) <.sorry("Unrecognized backslash sequence: '\\" ~ $<x>.Str ~ "'")> ] }

        # NIECZA multi methods NYI
        method tweak(:single(:$q), :double(:$qq), :cclass(:$cc)) {
            if    $q.defined  { self.panic("Too late for :q") }
            elsif $qq.defined { self.panic("Too late for :qq") }
            elsif $cc.defined { self.panic("Too late for :cc") }
            else { nextsame }
        }
    }

    role cc {
        token stopper { \' }

        method ccstate ($s) {
            if $*CCSTATE eq '..' {
                $*CCSTATE = '';
            }
            else {
                $*CCSTATE = $s;
            }
            self;
        }

        # (must not allow anything to match . in nibbler or we'll lose track of state)
        token escape:ws { \s+ [ <?before '#'> <.ws> ]? }
        token escape:sym<#> { '#' <.panic: "Please backslash # for literal char or put whitespace in front for comment"> }

        token escape:sym<\\> { <sym> <item=.backslash>  <.ccstate('\\' ~ $<item>.Str)> }

        token escape:sym<..> { <sym>
            [
            || <?{ $*CCSTATE eq '' or $*CCSTATE eq '..' }> <.sorry: "Range missing start character on the left">
            || <?before \s* <!stopper> <!before '..'> \S >
            || <.sorry: "Range missing stop character on the right">
            ]
            { $*CCSTATE = '..'; }
        }

        token escape:sym<-> { '-' <?{ $*CCSTATE ne '' }> \s* <!stopper> \S <.obs('- as character range','..')> }
        token escape:ch { $<ch> = [\S] <.ccstate($<ch>.Str)> }

        token backslash:stopper { <text=.stopper> }
        token backslash:a { :i <sym> }
        token backslash:b { :i <sym> }
        token backslash:c { :i <sym> <charspec> }
        token backslash:d { :i <sym> { $*CCSTATE = '' } }
        token backslash:e { :i <sym> }
        token backslash:f { :i <sym> }
        token backslash:h { :i <sym> { $*CCSTATE = '' } }
        token backslash:n { :i <sym> }
        token backslash:o { :i :dba('octal character') <sym> [ <octint> | '[' ~ ']' <octints> ] }
        token backslash:r { :i <sym> }
        token backslash:s { :i <sym> { $*CCSTATE = '' } }
        token backslash:t { :i <sym> }
        token backslash:v { :i <sym> { $*CCSTATE = '' } }
        token backslash:w { :i <sym> { $*CCSTATE = '' } }
        token backslash:x { :i :dba('hex character') <sym> [ <hexint> | '[' ~ ']' <hexints> ] }
        token backslash:sym<0> { <sym> }

        # keep random backslashes like qq does
        token backslash:misc { {} [ (\W) { $<text> = $0.Str; } | $<x>=(\w) <.sorry("Unrecognized backslash sequence: '\\" ~ $<x>.Str ~ "'")> ] }

        # NIECZA multi methods NYI
        method tweak(:single(:$q), :double(:$qq), :cclass(:$cc)) {
            if    $q.defined  { self.panic("Too late for :q") }
            elsif $qq.defined { self.panic("Too late for :qq") }
            elsif $cc.defined { self.panic("Too late for :cc") }
            else { nextsame }
        }
    }

    role p5 {
        # begin tweaks (DO NOT ERASE)
        # NIECZA multi methods NYI
        method tweak(:$g, :$i, :$m, :$s, :$x, :$p, :$c) {
            if    $g.defined { self }
            elsif $i.defined { self }
            elsif $m.defined { self }
            elsif $s.defined { self }
            elsif $x.defined { self }
            elsif $p.defined { self }
            elsif $c.defined { self }
            else             { nextsame }
        }
    }


    method tweak(:single(:$q), :double(:$qq), :cclass(:$cc), :backslash(:$b),
            :scalar(:$s), :array(:$a), :hash(:$h), :function(:$f),
            :closure(:$c), :path(:$p), :exec(:$x), :words(:$w),
            :quotewords(:$ww), :heredoc(:$to), :$regex, *%unknown) {
        # NIECZA ::foo syntax is broken, no role cronies, no MMD
        if    $q.defined  { self.truly($q,  ':q'); self.mixin(q) }
        elsif $qq.defined { self.truly($qq, ':qq'); self.mixin(qq) }
        elsif $cc.defined { self.truly($cc, ':cc'); self.mixin(cc) }

        elsif $b.defined  { self.mixin($b  ?? STD::Q::b1  !! STD::Q::b0) }
        elsif $s.defined  { self.mixin($s  ?? STD::Q::s1  !! STD::Q::s0) }
        elsif $a.defined  { self.mixin($a  ?? STD::Q::a1  !! STD::Q::a0) }
        elsif $h.defined  { self.mixin($h  ?? STD::Q::h1  !! STD::Q::h0) }
        elsif $f.defined  { self.mixin($f  ?? STD::Q::f1  !! STD::Q::f0) }
        elsif $c.defined  { self.mixin($c  ?? STD::Q::c1  !! STD::Q::c0) }

        elsif $p.defined  { self.mixin($p  ?? STD::Q::p1  !! STD::Q::p0) }
        elsif $x.defined  { self.mixin($x  ?? STD::Q::x1  !! STD::Q::x0) }
        elsif $w.defined  { self.mixin($w  ?? STD::Q::w1  !! STD::Q::w0) }
        elsif $ww.defined { self.mixin($ww ?? STD::Q::ww1 !! STD::Q::ww0) }

        elsif $to.defined { self.truly($to, ':to'); self.cursor_herelang }

        elsif $regex.defined {
            %*LANG<Regex>
        }
        else {
            self.sorry("Unrecognized quote modifier: " ~ %unknown.keys.[0]);
            self;
        }
    }
}

grammar Quasi is STD::P6 {
    token term:unquote {
        :my $*QUASIMODO = 0;
        <starter><starter><starter> <.ws>
        [ <EXPR> <stopper><stopper><stopper> || <.panic: "Confused"> ]
    }

    # NIECZA
    method tweak (:$ast, :$lang, :$unquote, :$COMPILING, *%unknown) {
        if    $ast.defined { self } # XXX some transformer operating on the normal AST?
        elsif $lang.defined { self.cursor_fresh( $lang ) }
        elsif $unquote.defined { self } # XXX needs to override unquote
        elsif $COMPILING.defined { $*QUASIMODO = 1; self; } # XXX needs to lazify the lexical lookups somehow
        else {
            self.sorry("Unrecognized quasiquote modifier: " ~ %unknown.keys.[0]);
            self;
        }
    }
}

##############################
# Operator Precedence Parser #
##############################

method EXPR ($preclvl?) {
    my $preclim = $preclvl ?? $preclvl.<prec> // $LOOSEST !! $LOOSEST;
    my $*LEFTSIGIL = '';        # XXX P6
    my $*PRECLIM = $preclim;
    my @termstack;
    my @opstack;
    my $termish = 'termish';

    push @opstack, { 'O' => %terminator, 'sym' => '' };         # (just a sentinel value)

    my $here = self;
    my $S = $here.pos;
    self.deb("In EXPR, at $S") if $DEBUG::EXPR;

    sub reduce() {
        self.deb("entering reduce, termstack == ", +@termstack, " opstack == ", +@opstack) if $DEBUG::EXPR;
        my $op = pop @opstack;
        my $sym = $op<sym>;
        my $assoc = $op<O><assoc> // 'unary';
        if $assoc eq 'chain' {
            self.deb("reducing chain") if $DEBUG::EXPR;
            my @chain;
            push @chain, pop(@termstack);
            push @chain, $op;
            while @opstack {
                last if $op<O><prec> ne @opstack[*-1]<O><prec>;
                push @chain, pop(@termstack);
                push @chain, pop(@opstack);
            }
            push @chain, pop(@termstack);
            my $endpos = @chain[0].pos;
            @chain = reverse @chain if @chain > 1;
            my $startpos = @chain[0].from;
            # NIECZA had to rewrite this, check if it's working
            my $i = 0;
            my @caplist;
            for @chain -> $c {
                push @caplist, (($i %% 2) ?? 'term' !! 'op') => $c;
                $i++;
            }
            push @termstack, Match.synthetic(
                :suphash({ _arity => 'CHAIN', chain => @chain }),
                :captures(@caplist),
                :method<CHAIN>,
                :cursor($op.CURSOR),
                :from($startpos),
                :to($endpos));
        }
        elsif $assoc eq 'list' {
            self.deb("reducing list") if $DEBUG::EXPR;
            my @list;
            my @delims = $op;
            push @list, pop(@termstack);
            while @opstack {
                self.deb($sym ~ " vs " ~ @opstack[*-1]<sym>) if $DEBUG::EXPR;
                last if $sym ne @opstack[*-1]<sym>;
                if @termstack and defined @termstack[0] {
                    push @list, pop(@termstack);
                }
                else {
                    self.worry("Missing term in " ~ $sym ~ " list");
                }
                push @delims, pop(@opstack);
            }
            if @termstack and defined @termstack[0] {
                push @list, pop(@termstack);
            }
            else {
                self.worry("Missing final term in '" ~ $sym ~ "' list");
            }
            my $endpos = @list[0].pos;
            @list = reverse @list if @list > 1;
            my $startpos = @list[0].from;
            @delims = reverse @delims if @delims > 1;
            my @caps;
            if @list {
                push @caps, elem => @list[0] if @list[0];
                for 0..@delims-1 {
                    my $d = @delims[$_];
                    my $l = @list[$_+1];
                    push @caps, delim => $d;
                    push @caps, elem => $l if $l;  # nullterm?
                }
            }
            push @termstack, Match.synthetic(
                :method<LIST>, :cursor($op.CURSOR), :captures(@caps),
                :from($startpos), :to($endpos),
                :suphash({ _arity => 'LIST', delims => @delims,
                    list => @list, O => $op<O>, sym => $sym }));
        }
        elsif $assoc eq 'unary' {
            self.deb("reducing") if $DEBUG::EXPR;
            self.deb("Termstack size: ", +@termstack) if $DEBUG::EXPR;

            self.deb($op.perl) if $DEBUG::EXPR;
            my $arg = pop @termstack;
            if $arg.from < $op.from { # postfix
                push @termstack, Match.synthetic(
                    :cursor($op.CURSOR), :to($op.to), :from($arg.from),
                    :captures(arg => $arg, op => $op), :method<POSTFIX>,
                    :suphash({ _arity => 'UNARY' }));
            }
            elsif $arg.pos > $op.pos {   # prefix
                push @termstack, Match.synthetic(
                    :cursor($op.CURSOR), :to($arg.to), :from($op.from),
                    :captures(op => $op, arg => $arg), :method<PREFIX>,
                    :suphash({ _arity => 'UNARY' }));
            }
        }
        else {
            self.deb("reducing") if $DEBUG::EXPR;
            self.deb("Termstack size: ", +@termstack) if $DEBUG::EXPR;

            my $right = pop @termstack;
            my $left = pop @termstack;

            push @termstack, Match.synthetic(
                :to($right.to), :from($left.from), :cursor($op.CURSOR),
                :captures(:left($left), :infix($op), :right($right)),
                :suphash({_arity => 'BINARY'}), :method<INFIX>);

            self.deb(@termstack[*-1].dump) if $*DEBUG +& DEBUG::EXPR;
            my $ck;
            if $ck = $op<O><_reducecheck> {
                @termstack[*-1] = $ck(@termstack[*-1]);
            }
        }
    }

  TERM:
    loop {
        self.deb("In loop, at ", $here.pos) if $& DEBUG::EXPR;
        my $oldpos = $here.pos;
        $here = $here.cursor_fresh();
        $*LEFTSIGIL = @opstack[*-1]<O><prec> gt $item_assignment_prec ?? '@' !! '';     # XXX P6
        my @t = $here.$termish;

        if not @t or not $here = @t[0] or ($here.pos == $oldpos and $termish eq 'termish') {
            $here.panic("Bogus term") if @opstack > 1;
            return ();
        }
        $termish = 'termish';
        my $PRE = $here.<PRE>:delete // [];
        my $POST = $here.<POST>:delete // [];
        my @PRE = @$PRE;
        my @POST = reverse @$POST;

        # interleave prefix and postfix, pretend they're infixish
        my $M = $here;

        # note that we push loose stuff onto opstack before tight stuff
        while @PRE and @POST {
            my $postO = @POST[0]<O>;
            my $preO = @PRE[0]<O>;
            if $postO<prec> lt $preO<prec> {
                push @opstack, shift @POST;
            }
            elsif $postO<prec> gt $preO<prec> {
                push @opstack, shift @PRE;
            }
            elsif $postO<uassoc> eq 'left' {
                push @opstack, shift @POST;
            }
            elsif $postO<uassoc> eq 'right' {
                push @opstack, shift @PRE;
            }
            else {
                $here.sorry('"' ~ @PRE[0]<sym> ~ '" and "' ~ @POST[0]<sym> ~ '" are not associative');
            }
        }
        push @opstack, @PRE,@POST;

        push @termstack, $here.<term>;
        @termstack[*-1].<POST>:delete;
        self.deb("after push: " ~ (0+@termstack)) if $*DEBUG +& DEBUG::EXPR;

        last TERM if $preclim eq $methodcall_prec; # in interpolation, probably   # XXX P6

        loop {     # while we see adverbs
            $oldpos = $here.pos;
            last TERM if (@*MEMOS[$oldpos]<endstmt> // 0) == 2;   # XXX P6
            $here = $here.cursor_fresh.ws;
            my @infix = $here.cursor_fresh.infixish();
            last TERM unless @infix;
            my $infix = @infix[0];
            last TERM unless $infix.pos > $oldpos;
            
            if not $infix<sym> {
                die $infix.dump if $*DEBUG +& DEBUG::EXPR;
            }

            my $inO = $infix<O>;
            my Str $inprec = $inO<prec>;
            if not defined $inprec {
                self.deb("No prec given in infix!") if $*DEBUG +& DEBUG::EXPR;
                die $infix.dump if $*DEBUG +& DEBUG::EXPR;
                $inprec = %terminator<prec>;   # XXX lexical scope is wrong
            }

            if $inprec le $preclim {
                if $preclim ne $LOOSEST {
                    my $dba = $preclvl.<dba>;
                    my $h = $*HIGHEXPECT;
                    %$h = ();
                    $h.{"an infix operator with precedence tighter than $dba"} = 1;
                }
                last TERM;
            }

            $here = $infix.cursor_fresh.ws();

            # substitute precedence for listops
            $inO<prec> = $inO<sub> if $inO<sub>;

            # Does new infix (or terminator) force any reductions?
            while @opstack[*-1]<O><prec> gt $inprec {
                &reduce();
            }

            # Not much point in reducing the sentinels...
            last if $inprec lt $LOOSEST;

        if $infix<fake> {
            push @opstack, $infix;
            &reduce();
            next;  # not really an infix, so keep trying
        }

            # Equal precedence, so use associativity to decide.
            if @opstack[*-1]<O><prec> eq $inprec {
                my $assoc = 1;
                my $atype = $inO<assoc>;
                if $atype eq 'non'   { $assoc = 0; }
                elsif $atype eq 'left'  { &reduce() }   # reduce immediately
                elsif $atype eq 'right' { }            # just shift
                elsif $atype eq 'chain' { }            # just shift
                elsif $atype eq 'unary' { }            # just shift
                elsif $atype eq 'list'  {
                    $assoc = 0 unless $infix<sym> eqv @opstack[*-1]<sym>;
                }
                else { $here.panic('Unknown associativity "' ~ $_ ~ '" for "' ~ $infix<sym> ~ '"') }
                if not $assoc {
                   $here.sorry('"' ~ @opstack[*-1]<sym> ~ '" and "' ~ $infix.Str ~ '" are non-associative and require parens');
                }
            }

            $termish = $inO<nextterm> if $inO<nextterm>;
            push @opstack, $infix;              # The Shift
            last;
        }
    }
    &reduce() while +@opstack > 1;
    if @termstack {
        +@termstack == 1 or $here.panic("Internal operator parser error, termstack == " ~ (+@termstack));
        @termstack[0].from = self.pos;
        @termstack[0].pos = $here.pos;
    }
    self._MATCHIFYr($S, "EXPR", @termstack);
}

##########
## Regex #
##########

grammar Regex is STD {

    method tweak(:Perl5(:$P5), :overlap(:$ov), :exhaustive(:$ex),
            :continue(:$c), :pos(:$p), :sigspace(:$s), :ratchet(:$r),
            :global(:$g), :ignorecase(:$i), :ignoreaccent(:$a), :samecase(:$ii),
            :sameaccent(:$aa), :th(:st(:nd(:rd(:$nth)))), :$x, :$bytes,
            :$codes, :$graphs, :$chars, :$rw) {
        if    $P5.defined { die("Autoloading NYI") }
        elsif $ov.defined { %*RX<ov> = $ov; self }
        elsif $ex.defined { %*RX<ex> = $ex; self }
        elsif $c.defined  { %*RX<c>  = $c;  self }
        elsif $p.defined  { %*RX<p>  = $p;  self }
        elsif $s.defined  { %*RX<s>  = $s;  self }
        elsif $r.defined  { %*RX<r>  = $r;  self }
        elsif $g.defined  { %*RX<g>  = $g;  self }
        elsif $i.defined  { %*RX<i>  = $i;  self }
        elsif $a.defined  { %*RX<a>  = $a;  self }
        elsif $ii.defined { %*RX<ii> = $ii; self }
        elsif $aa.defined { %*RX<aa> = $aa; self }
        elsif $nth.defined { %*RX<nth> = $nth; self }
        elsif $x.defined  { %*RX<x>  = $x;  self }
        elsif $bytes.defined { %*RX<bytes> = $bytes; self }
        elsif $codes.defined { %*RX<codes> = $codes; self }
        elsif $graphs.defined { %*RX<graphs> = $graphs; self }
        elsif $chars.defined { %*RX<chars> = $chars; self }
        elsif $rw.defined { %*RX<rw> = $rw; self }
        else { nextsame }
    }

    token category:metachar { <sym> }
    proto token metachar {*}

    token category:backslash { <sym> }
    proto token backslash {*}

    token category:assertion { <sym> }
    proto token assertion {*}

    token category:quantifier { <sym> }
    proto token quantifier {*}

    token category:mod_internal { <sym> }
    proto token mod_internal {*}

    proto token regex_infix {*}

    # no such thing as ignored whitespace in a normal regex
    token ws { <?> }

    token normspace {
        <?before \s | '#'> [ :lang(%*LANG<MAIN>) <.ws> ]
    }

    token unsp { '\\' <?before \s | '#'> <.panic: "No unspace allowed in regex; if you meant to match the literal character, please enclose in single quotes ('" ~ substr($¢.orig,$¢.pos,1) ~ "') or use a backslashed form like \\xXX"> }  # no unspace in regexen  NIECZA: removed ord, sprintf

    rule nibbler {
        :temp %*RX;
        [ <.normspace>? < || | && & > ]?
        <EXPR>
        [
        || <?infixstopper>
        || $$ <.panic: "Regex not terminated">
        || (\W)<.sorry("Unrecognized regex metacharacter " ~ $0.Str ~ " (must be quoted to match literally)")>
        || <.panic: "Regex not terminated">
        ]
    }

    token termish {
        <.ws>
        [
        || <term=.quant_atom_list> <?{ %*RX<s> or $<term>.Str ~~ /\S/ }>
        || <normspace>?
            [
            || <?before <stopper> | <[&|~]> > <.panic: "Null pattern not allowed">
            || <?before <[ \] \) \> ]> > {{
                    my $c = substr(self.orig,$¢.pos,1);
                    if $*GOAL eq $c {
                        $¢.panic("Null pattern not allowed");
                    }
                    else {
                        $¢.panic("Unmatched closing $c");
                    }
                }}
            || $$ <.panic: "Regex not terminated">
            || \W <.sorry: "Unrecognized regex metacharacter (must be quoted to match literally)">
            || <.panic: "Regex not terminated">
            ]
        ]
    }
    token quant_atom_list {
        <quantified_atom>+
    }
    token infixish {
        <!infixstopper>
        <!stdstopper>
        <regex_infix>
        {
            $<O> = $<regex_infix><O>;
            $<sym> = $<regex_infix><sym>;
        }
    }
    regex infixstopper {
        :dba('infix stopper')
        [
        | <?before <[\) \} \]]> >
        | <?before '>' <-[>)]> >
        | <?before <stopper> >
        ]
    }

    token regex_infix:sym<||> { <sym> <O(|%tight_or)>  }
    token regex_infix:sym<&&> { <sym> <O(|%tight_and)>  }
    token regex_infix:sym<|> { <sym> <O(|%junctive_or)>  }
    token regex_infix:sym<&> { <sym> <O(|%junctive_and)>  }

    token quantified_atom {
        <!stopper>
        <!regex_infix>
        <atom>
        [ <normspace>? <quantifier> ]?
#            <?{ $<atom>.max_width }>
#                || <.panic: "Can't quantify zero-width atom">
    }

    token atom {
        :dba('regex atom')
        [
        | \w
        | <metachar> ::
        ]
    }

    # sequence stoppers
    # NIECZA XXX fail was used here.  What good is <fail> == <fail=&fail>?
    token metachar:sym« > » { '>'<!before '>'> :: <!> }
    token metachar:sym<&&>  { '&&' :: <!> }
    token metachar:sym<&>   { '&'  :: <!> }
    token metachar:sym<||>  { '||' :: <!> }
    token metachar:sym<|>   { '|'  :: <!> }
    token metachar:sym<]>   { ']'  :: <!> }
    token metachar:sym<)>   { ')'  :: <!> }
    token metachar:sym<;>   {
        ';' {}
        [
        || <?before \N*? <stopper> > <.panic: "Semicolon must be quoted">
        || <.panic: "Regex missing terminator (or semicolon must be quoted?)">
        ]
    }

    token metachar:sym<{*}> { <onlystar=.sym> <?{ $*MULTINESS eq 'proto' }> }
    token metachar:sym<[*]> { <onlystar=.sym> <?{ $*MULTINESS eq 'proto' }> }
    token metachar:quant { <quantifier> <.sorry: "Quantifier quantifies nothing"> }

    # "normal" metachars

    token metachar:sigwhite {
        <normspace>
    }
    token metachar:unsp   { <unsp> }

    token metachar:sym<{ }> {
        <?before '{'>
        <embeddedblock>
        {{ $/<sym> := <{ }> }}
    }

    token metachar:mod {
        <?before ':'>
        <mod_internal>
        { $/<sym> = $<mod_internal><sym> }
    }

    token metachar:sym<-> {
        '-' <?{ $*GOAL eq ']' }> <.sorry("Invalid regex metacharacter (must be quoted to match literally)")>
    }

    token metachar:sym<:> {
        <sym> <?before \s> <.panic: "Backtrack control ':' does not seem to have a preceding atom to control">
    }

    token metachar:sym<::> {
        <sym>
    }

    token metachar:sym«::>» {
        <sym>
    }

    token metachar:sym<:::> {
        <sym>
    }

    token metachar:sym<[ ]> {
        :dba("bracketed regex")
        '[' ~ ']' <nibbler>
        { $¢.check_old_cclass($<nibbler>.Str); }
        { $/<sym> := <[ ]>; }
    }

    token metachar:sym<( )> {
        :dba("capture parens")
        '(' ~ ')' <nibbler>
        { $/<sym> := <( )> }
    }

    token metachar:sym« <( » { '<(' }
    token metachar:sym« )> » { ')>' }

    token metachar:sym« << » { '<<' }
    token metachar:sym« >> » { '>>' }
    token metachar:sym< « > { '«' }
    token metachar:sym< » > { '»' }

    token metachar:qw {
        <?before '<' \s >  # (note required whitespace)
        <circumfix>
    }

    token metachar:sym«< >» {
        '<' ~ '>' <assertion>
    }

    token metachar:sym<\\> { <sym> <backslash> }
    token metachar:sym<.>  { <sym> }
    token metachar:sym<^^> { <sym> }
    token metachar:sym<^>  { <sym> }
    token metachar:sym<$$> {
        <sym>
        [ (\w+) <.obs("\$\$" ~ $0.Str ~ " to deref var inside a regex", "\$(\$" ~ $0.Str ~ ")")> ]?
    }
    token metachar:sym<$>  {
        '$'
        <?before
        | \s
        | '|'
        | '&'
        | ')'
        | ']'
        | '>'
        | $
        | <.stopper>
        >
    }

    token metachar:sym<' '> { <?before "'"> [:lang(%*LANG<MAIN>) <quote>] }
    token metachar:sym<" "> { <?before '"'> [:lang(%*LANG<MAIN>) <quote>] }

    token metachar:var {
        :my $*QSIGIL ::= substr(self.orig,self.pos,1);
        <!before '$$'>
        <?before <sigil>>
        [:lang(%*LANG<MAIN>) <variable> ]
        { $<sym> = $<variable>.Str; }
        [
        || $<binding> = ( \s* '=' \s* <quantified_atom> )
           { $¢.check_variable($<variable>) unless substr($<sym>,1,1) eq '<' }
        || { $¢.check_variable($<variable>) }
           [ <?before '.'? <[ \[ \{ \< ]>> <.worry: "Apparent subscript will be treated as regex"> ]?
        ]
    }

    token backslash:unspace { <?before \s> [ :lang( %*LANG<MAIN> ) <.ws> ] }

    token backslash:sym<0> { '0' <!before <[0..7]> > }

    token backslash:A { <sym> <.obs('\\A as beginning-of-string matcher', '^')> }
    token backslash:a { <sym> <.sorry: "\\a is allowed only in strings, not regexes"> }
    token backslash:B { <sym> <.obs('\\B as word non-boundary', '<!wb>')> }
    token backslash:b { <sym> <.obs('\\b as word boundary', '<?wb> (or either of « or »)')> }
    token backslash:c { :i <sym> <charspec> }
    token backslash:d { :i <sym> }
    token backslash:e { :i <sym> }
    token backslash:f { :i <sym> }
    token backslash:h { :i <sym> }
    token backslash:n { :i <sym> }
    token backslash:o { :i :dba('octal character') <sym> [ <octint> | '[' ~ ']' <octints> ] }
    token backslash:p {
        :my $s;
        :my $m;
        :my $p;
        <sym=[pP]>
        { $s = $<sym>.Str; $m = $s lt 'a' ?? '-' !! '+'; }
        [
        || (\w) { $p = $0.Str; $¢.obs("\\$s$p", '<' ~ $m ~ "is$p>"); }
        || '{' $<param>=[\w+] '}' { $p = $<param>.Str; $¢.obs("\\$s\{$p\}", '<' ~ $m ~ "is$p>"); }
        || '{' $<param>=[\w+] \= $<val>=[<-[\}]>*] '}' { $p = $<param>.Str; my $v = $<val>.Str; $¢.obs("\\$s\{$p=$v\}", '<' ~ $m ~ "is$p\('$v')>"); }
        ]
    }
    token backslash:Q { <sym> <.obs('\\Q as quotemeta', 'quotes or literal variable match')> }
    token backslash:r { :i <sym> }
    token backslash:s { :i <sym> }
    token backslash:t { :i <sym> }
    token backslash:v { :i <sym> }
    token backslash:w { :i <sym> }
    token backslash:x { :i :dba('hex character') <sym> [ <hexint> | '[' ~ ']' <hexints> ] }
    token backslash:z { <sym> <.obs('\\z as end-of-string matcher', '$')> }
    token backslash:Z { <sym> <.obs('\\Z as end-of-string matcher', '\\n?$')> }
    token backslash:misc { $<litchar>=(\W) }
    token backslash:oldbackref { (<[1..9]>\d*) { my $d = $0.Str; $¢.sorryobs("the 1-based special form '\\$d' as a backreference", "the 0-based variable '\$" ~ ($d - 1) ~ "' instead" ); } }
    token backslash:oops { <.sorry: "Unrecognized regex backslash sequence"> . }

    token assertion:sym<...> { <sym> }
    token assertion:sym<???> { <sym> }
    token assertion:sym<!!!> { <sym> }

    token assertion:sym<?> { <sym> [ <?before '>'> | <assertion> ] }
    token assertion:sym<!> { <sym> [ <?before '>'> | <assertion> ] }
    token assertion:sym<*> { <sym> [ <?before '>'> | <.ws> <nibbler> ] }

    token assertion:sym<{ }> { <embeddedblock> }

    token assertion:variable {
        <?before <sigil>>  # note: semantics must be determined per-sigil
        [:lang($¢.cursor_fresh(%*LANG<MAIN>).unbalanced('>')) <variable=.EXPR(item %LOOSEST)>]
    }

    token assertion:method {
        '.' [
            | <?before <alpha> > <assertion>
            | [ :lang($¢.cursor_fresh(%*LANG<MAIN>).unbalanced('>')) <dottyop> ]
            ]
    }

    token assertion:name { [ :lang($¢.cursor_fresh(%*LANG<MAIN>).unbalanced('>')) <longname> ]
                                    [
                                    | <?before '>' > {
                                        my $n = $<longname>.Str;
                                        if $n eq 'before' or $n eq 'after' {
                                            $¢.panic("$n requires an argument");
                                        }
                                    }
                                    | <.normspace>? <nibbler> <.ws>
                                    | '=' <assertion>
                                    | ':' [ :lang($¢.cursor_fresh(%*LANG<MAIN>).unbalanced('>')) <.ws> <arglist> ]
                                    | '(' {}
                                        [ :lang(%*LANG<MAIN>) <arglist> ]
                                        [ ')' || <.panic: "Assertion call missing right parenthesis"> ]
                                    ]?
    }

    token assertion:sym<[> { <?before '['> <cclass_elem>+ }
    token assertion:sym<+> { <?before '+'> <cclass_elem>+ }
    token assertion:sym<-> { <?before '-'> <cclass_elem>+ }
    token assertion:sym<.> { <sym> }
    token assertion:sym<,> { <sym> }
    token assertion:sym<~~> { <sym> [ <?before '>'> | \d+ | <desigilname> ] }

    token assertion:bogus { <.panic: "Unrecognized regex assertion"> }

    token sign { '+' | '-' | <?> }
    token cclass_elem {
        :my $*CCSTATE = '';
        :dba('character class element')
        <sign>
        <.normspace>?
        [
        | <name>
        | <before '['> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:cc))>
        ]
        <.normspace>?
    }

    token mod_arg { :dba('modifier argument') '(' ~ ')' [:lang(%*LANG<MAIN>) <semilist> ] }

    token mod_internal:sym<:my>    { ':' <?before ['my'|'state'|'our'|'anon'|'constant'|'temp'|'let'] \s > [:lang(%*LANG<MAIN>) <statement> <eat_terminator> ] }

    # XXX needs some generalization

    token mod_internal:sym<:i>    { $<sym>=[':i'|':ignorecase'] » { %*RX<i> = 1 } }
    token mod_internal:sym<:!i>   { $<sym>=[':!i'|':!ignorecase'] » { %*RX<i> = 0 } }
    token mod_internal:sym<:i( )> { $<sym>=[':i'|':ignorecase'] <mod_arg> { %*RX<i> = $<mod_arg>.Str.Numeric } }
    token mod_internal:sym<:0i>   { ':' (\d+) ['i'|'ignorecase'] { %*RX<i> = $0 } }

    token mod_internal:sym<:a>    { $<sym>=[':a'|':ignoreaccent'] » { %*RX<a> = 1 } }
    token mod_internal:sym<:!a>   { $<sym>=[':!a'|':!ignoreaccent'] » { %*RX<a> = 0 } }
    token mod_internal:sym<:a( )> { $<sym>=[':a'|':ignoreaccent'] <mod_arg> { %*RX<a> = $<mod_arg>.Str.Numeric } }
    token mod_internal:sym<:0a>   { ':' (\d+) ['a'|'ignoreaccent'] { %*RX<a> = $0 } }

    token mod_internal:sym<:s>    { ':s' 'igspace'? » { %*RX<s> = 1 } }
    token mod_internal:sym<:!s>   { ':!s' 'igspace'? » { %*RX<s> = 0 } }
    token mod_internal:sym<:s( )> { ':s' 'igspace'? <mod_arg> { %*RX<s> = $<mod_arg>.Str.Numeric } }
    token mod_internal:sym<:0s>   { ':' (\d+) 's' 'igspace'? » { %*RX<s> = $0 } }

    token mod_internal:sym<:r>    { ':r' 'atchet'? » { %*RX<r> = 1 } }
    token mod_internal:sym<:!r>   { ':!r' 'atchet'? » { %*RX<r> = 0 } }
    token mod_internal:sym<:r( )> { ':r' 'atchet'? » <mod_arg> { %*RX<r> = $<mod_arg>.Str.Numeric } }
    token mod_internal:sym<:0r>   { ':' (\d+) 'r' 'atchet'? » { %*RX<r> = $0 } }
 
    token mod_internal:sym<:Perl5>    { [':Perl5' | ':P5'] <.require_P5> [ :lang( $¢.cursor_fresh( %*LANG<P5Regex> ).unbalanced($*GOAL) ) <nibbler> ] }

    token mod_internal:p6adv {
        <?before ':' ['dba'|'lang'] » > [ :lang(%*LANG<MAIN>) <quotepair> ] { $/<sym> = ':' ~ $<quotepair><k> }
    }

    token mod_internal:oops { {} (':'\w+) <.sorry: "Unrecognized regex modifier " ~ $0.Str > }

    token quantifier:sym<*>  { <sym> <quantmod> }
    token quantifier:sym<+>  { <sym> <quantmod> }
    token quantifier:sym<?>  { <sym> <quantmod> }
    token quantifier:sym<:>  { <sym> {} <?before \s> }
    token quantifier:sym<**> { <sym> :: <normspace>? <quantmod> <normspace>?
        [
        | \d+ \s+ '..' <.panic: "Spaces not allowed in bare range">
        | (\d+) [ '..' [ (\d+) { $¢.panic("Empty range") if $0.Str > $1[0].Str } | '*' | <.panic: "Malformed range"> ] ]?
        | <embeddedblock>
        | <quantified_atom>
        ]
    }

    token quantifier:sym<~> {
        <sym> :: <normspace>? <quantified_atom> <normspace>? <quantified_atom>
    }

    token quantifier:sym<~~> {
        [
        | '!' <sym>
        | <sym>
        ]
        <normspace> <quantified_atom> }

    token quantmod { ':'? [ '?' | '!' | '+' ]? }

    token quantifier:sym<{N,M}> {
        {} '{' (\d+) (','?) (\d*) '}'
        {
            my $all = substr(self.orig, self.pos, $¢.pos - self.pos);
            my $repl = chars($1.Str) ??
                ($0.Str ~ '..' ~ ($2.Str || '*')) !! $0.Str;
            $¢.sorryobs($all ~ " as general quantifier", 'X**' ~ $repl);
        }
    }

} # end grammar

#################
# Symbol tables #
#################

method newlex ($needsig = 0) {
    my $oid = $*CURLEX.id;
    $ALL.{$oid} === $*CURLEX or die "internal error: current lex id is invalid";
    my $line = self.lineof(self.pos);
    my $id;
    if $*NEWLEX {
        $*NEWLEX.<OUTER::> = $*CURLEX.idref;
        $*CURLEX = $*NEWLEX;
        $*NEWLEX = 0;
        $id = $*CURLEX.id;
    }
    else {
        $id = 'MY:file<' ~ $*FILE<name> ~ '>:line(' ~ $line ~ '):pos(' ~ self.pos ~ ')';
        $*CURLEX = STDStash.new(
            'OUTER::' => [$oid],
            '!file' => $*FILE, '!line' => $line,
            '!id' => [$id],
        );
    }
    $*CURLEX.<!NEEDSIG> = 1 if $needsig;
    $*CURLEX.<!IN_DECL> = $*IN_DECL if $*IN_DECL;
    $ALL.{$id} = $*CURLEX;
    self.<LEX> = $*CURLEX;
    $*DECLARAND<curlex> = $*CURLEX if $*DECLARAND;
    self;
}

method finishlex {
    my $line = self.lineof(self.pos);
    $*CURLEX<$_> = $*CURLEX<$_> // NAME.new( name => '$_', file => $*FILE, line => $line, dynamic => 1, scope => 'my' );
    $*CURLEX<$/> = $*CURLEX<$/> // NAME.new( name => '$/', file => $*FILE, line => $line, dynamic => 1, scope => 'my' );
    $*CURLEX<$!> = $*CURLEX<$!> // NAME.new( name => '$!', file => $*FILE, line => $line, dynamic => 1, scope => 'my' );
    $*SIGNUM = 0;
    self;
}

method getsig {
    my $pv = $*CURLEX.{'%?PLACEHOLDERS'};
    my $sig;
    if $*CURLEX.<!NEEDSIG>:delete {
        if $pv {
            my $h_ = $pv.<%_>:delete;
            my $a_ = $pv.<@_>:delete;
            $sig = join ', ', (keys %$pv).sort({ substr($^a,1) leg substr($^b,1) });
            $sig ~= ', *@_' if $a_;
            $sig ~= ', *%_' if $h_;
        }
        else {
            $sig = '$_ is ref = OUTER::<$_>';
        }
        $*CURLEX.<$?SIGNATURE> = $sig;
    }
    else {
        $sig = $*CURLEX.<$?SIGNATURE>;
    }
    self.<sig> = $sig;
    self.<lex> = $*CURLEX.idref;
    if ($*DECLARAND<mult>//'') ne 'proto' {
        for keys %$*CURLEX {
            my $desc = $*CURLEX{$_};
            next unless $_ ~~ /(\$|\@|\%|\&)\w/;
            next if $_ eq '$_' or $_ eq '@_' or $_ eq '%_';
            next if $desc<used>;
            next if $desc<rebind>;
            next if $desc<dynamic>;
            next if $desc<scope> eq 'state';
            next if $desc<stub>;
            my $pos = $desc<declaredat> // self.pos;
            self.cursor($pos).worry("$_ is declared but not used");
        }
    }
    self;
}

method getdecl {
    self.<decl> = $*DECLARAND;
    self;
}

method is_name ($n, $curlex = $*CURLEX) {
    my $name = $n;
    self.deb("is_name $name") if $DEBUG::symtab;

    my $curpkg = $*CURPKG;
    return True if $name ~~ /\:\:\(/;
    my @components = self.canonicalize_name($name);
    if @components > 1 {
        return True if @components[0] eq 'COMPILING::';
        return True if @components[0] eq 'CALLER::';
        return True if @components[0] eq 'CONTEXT::';
        if $curpkg = self.find_top_pkg(@components[0]) {
            self.deb("Found lexical package ", @components[0]) if $DEBUG::symtab;
            shift @components;
        }
        else {
            self.deb("Looking for GLOBAL::<$name>") if $DEBUG::symtab;
            $curpkg = $*GLOBAL;
        }
        while @components > 1 {
            my $pkg = shift @components;
            $curpkg = $curpkg.{$pkg};
            return False unless $curpkg;
            try {
                my $outlexid = $curpkg.[0];
                return False unless $outlexid;
                $curpkg = $ALL.{$outlexid};
                return False unless $curpkg;
            };
            self.deb("Found $pkg okay") if $DEBUG::symtab;
        }
    }
    $name = shift(@components)//'';
    self.deb("Looking for $name") if $DEBUG::symtab;
    return True if $name eq '';
    my $lex = $curlex;
    while $lex {
        self.deb("Looking in ", $lex.id) if $DEBUG::symtab;
        if $lex.{$name} {
            self.deb("Found $name in ", $lex.id) if $DEBUG::symtab;
            $lex.{$name}<used> = 1;
            return True;
        }
        my $oid = $lex.<OUTER::>[0] || last;
        $lex = $ALL.{$oid};
    }
    return True if $curpkg.{$name};
    return True if $*GLOBAL.{$name};
    self.deb("$name not found") if $DEBUG::symtab;
    return False;
}

method find_stash ($n, $curlex = $*CURLEX) {
    my $name = $n;
    self.deb("find_stash $name") if $DEBUG::symtab;

    return Any if $name ~~ /\:\:\(/;
    my @components = self.canonicalize_name($name);
    if @components > 1 {
        return Any if @components[0] eq 'COMPILING::';
        return Any if @components[0] eq 'CALLER::';
        return Any if @components[0] eq 'CONTEXT::';
        if $curlex = self.find_top_pkg(@components[0]) {
            self.deb("Found lexical package ", @components[0]) if $DEBUG::symtab;
            shift @components;
        }
        else {
            self.deb("Looking for GLOBAL::<$name>") if $DEBUG::symtab;
            $curlex = $*GLOBAL;
        }
        while @components > 1 {
            my $lex = shift @components;
            $curlex = $curlex.{$lex};
            return Any unless $curlex;
            try {
                my $outlexid = $curlex.[0];
                return Any unless $outlexid;
                $curlex = $ALL.{$outlexid};
                return Any unless $curlex;
            };
            self.deb("Found $lex okay") if $DEBUG::symtab;
        }
    }
    $name = shift(@components)//'';
    return $curlex if $name eq '';

    my $lex = $curlex;
    my $old;
    while $lex {
        return $old if $old = $lex.{$name};
        my $oid = $lex.<OUTER::>[0] || last;
        $lex = $ALL.{$oid};
    }
    return $old if $old = $curlex.{$name};
    return $old if $old = $*GLOBAL.{$name};
    return Any;
}

method find_top_pkg ($name) {
    self.deb("find_top_pkg $name") if $DEBUG::symtab;
    $name ~= '::' unless $name ~~ /\:\:$/;
    if $name eq 'OUR::' {
        return $*CURPKG;
    }
    elsif $name eq 'MY::' {
        return $*CURLEX;
    }
    elsif $name eq 'OUTER::' {
        return $ALL.{$*CURLEX.<OUTER::>[0]};
    }
    elsif $name eq 'CORE::' {
        return $*CORE;
    }
    elsif $name eq 'SETTING::' {
        return $*SETTING;
    }
    elsif $name eq 'UNIT::' {
        return $*UNIT;
    }
    # everything is somewhere in lexical scope (we hope)
    my $lex = $*CURLEX;
    while $lex {
        return $lex.{$name} if $lex.{$name};
        my $oid = $lex.<OUTER::>[0] || last;
        $lex = $ALL.{$oid};
    }
    return 0;
}

method add_name ($name) {
    my $scope = $*SCOPE || 'my';
    my $pkgdecl = $*PKGDECL || 'symbol';
    return self if $scope eq 'anon' or $pkgdecl eq 'slang';
    self.deb("Adding $scope $name") if $DEBUG::symtab;
    if $scope eq 'augment' or $scope eq 'supersede' {
        self.is_name($name) or
            self.worry("Can't $scope $pkgdecl $name because it doesn't exist");
        $*MONKEY_TYPING or
            self.sorry("Can't $scope $pkgdecl $name without MONKEY_TYPING");
    }
    else {
        if $scope eq 'our' {
            self.add_our_name($name);
        }
        else {
            self.add_my_name($name);
        }
    }
    self;
}

method add_my_name ($n, $d?, $p?) {
    my $name = $n;
    self.deb("add_my_name $name in ", $*CURLEX.id) if $DEBUG::symtab;
    return self if $name ~~ /\:\:\(/;
    my $curstash = $*CURLEX;
    my @components = self.canonicalize_name($name);
    my $sid = $curstash.id // '???';
    while @components > 1 {
        my $pkg = shift @components;
        $sid ~= "::$pkg";
        my $newstash = $curstash.{$pkg} = $curstash.{$pkg} // STDStash.new(
            'PARENT::' => $curstash.idref,
            '!stub' => 1,
            '!id' => [$sid] );
        self.deb("Adding new package $pkg in ", $curstash.id) if $DEBUG::symtab;
        $curstash = $newstash;
    }
    $name = my $shortname = shift @components;
    return self unless defined $name and $name ne '';
    return self if $name eq '$' or $name eq '@' or $name eq '%';
    return self.add_categorical(substr($name,1)) if $name ~~ /^\&\w+\:/;
    if $shortname ~~ /\:/ {
        ($shortname) = ($shortname ~~ /(.*?)\:/);
    }

    # This may just be a lexical alias to "our" and such,
    # so reuse $*DECLARAND pointer if it's there.
    my $declaring = $d // NAME.new(
        name => $name,
        file => $*FILE, line => self.line,
        mult => ($*MULTINESS||'only'),
        of   => $*OFTYPE,
        scope => $*SCOPE,
    );
    my $old = $curstash.{$name};
    if $old and $old<line> and not $old<stub> {
        self.deb("$name exists, curstash = ", $curstash.id) if $*DEBUG +& DEBUG::symtab;
        my $omult = $old<mult> // '';
        if $declaring === $old {}  # already did this, probably enum
        elsif $*SCOPE eq 'use' {}
        elsif $*MULTINESS eq 'multi' and $omult ne 'only' {}
        elsif $omult eq 'proto' and $*MULTINESS ne 'proto' and $*MULTINESS ne 'only' {}
        elsif $*PKGDECL eq 'role' {}
        elsif $*SIGNUM and $old<signum> and $*SIGNUM != $old<signum> {
            $old<signum> = $*SIGNUM;
        }
        else {
            my $ofile = $old.file // 0;
            my $oline = $old.line // '???';
            my $loc = '';
            if $ofile {
                if $ofile !=== $*FILE {
                    my $oname = $ofile<name>;
                    $loc = " (see $oname line $oline)";
                }
                else {
                    $loc = " (see line $oline)";
                }
            }
            if $old.olex {
                my $rebind = $old<rebind>;
                my $truename = $old<varbind><truename>;
                self.sorry("Lexical symbol '$name' is already bound to an outer symbol$loc;\n  the implicit outer binding at line $rebind must be rewritten as $truename\n  before you can unambiguously declare a new '$name' in this scope");
            }
            elsif $name ~~ /^\w/ {
                self.sorry("Illegal redeclaration of symbol '$name'$loc");
            }
            elsif $name ~~ /^\&// {
                self.sorry("Illegal redeclaration of routine '$name'$loc") unless $name eq '&';
            }
            else {  # XXX eventually check for conformant arrays here
                self.worry("Useless redeclaration of variable $name$loc");
            }
            return self;
        }
    }
    else {
        $*DECLARAND = $curstash.{$name} = $declaring;
        $curstash.{$shortname} = $declaring unless $shortname eq $name;
        $*DECLARAND<declaredat> = self.pos;
        $*DECLARAND<inlex> = $curstash.idref;
        $*DECLARAND<signum> = $*SIGNUM if $*SIGNUM;
        $*DECLARAND<const> = $*DECLARAND<const> || 1 if $*IN_DECL eq 'constant';
        $*DECLARAND<used> = 1 if substr($name,0,1) eq '&' and %*MYSTERY{substr($name,1)};
        if !$*DECLARAND<const> and $shortname ~~ /^\w+$/ {
            $curstash.{"\&$shortname"} = $curstash.{"\&$shortname"} // $curstash.{$shortname};
            $curstash.{"\&$shortname"}<used> = 1;
            $sid ~= "::$name";
            if $name !~~ /\:\</ {
                $*NEWPKG = $curstash.{$name ~ '::'} = ($p // STDStash.new(
                    'PARENT::' => $curstash.idref,
                    '!file' => $*FILE, '!line' => self.line,
                    '!id' => [$sid] ));
            }
        }
    }
    self;
}

method add_our_name ($n) {
    my $name = $n;
    self.deb("add_our_name $name in " ~ $*CURPKG.id) if $DEBUG::symtab;
    return self if $name ~~ /\:\:\(/;
    my $curstash = $*CURPKG;
    self.deb("curstash $curstash global $*GLOBAL ", join ' ', %$*GLOBAL) if $DEBUG::symtab;
    $name = ($name ~~ /(.*?):[ver|auth]/).[0] // $name;
    my @components = self.canonicalize_name($name);
    if @components > 1 {
        my $c = self.find_top_pkg(@components[0]);
        if $c {
            shift @components;
            $curstash = $c;
        }
    }
    my $sid = $curstash.id // '???';
    while @components > 1 {
        my $pkg = shift @components;
        $sid ~= "::$pkg";
        my $newstash = $curstash.{$pkg} = $curstash.{$pkg} // STDStash.new(
            'PARENT::' => $curstash.idref,
            '!stub' => 1,
            '!id' => [$sid] );
        $curstash = $newstash;
        self.deb("Adding new package $pkg in $curstash ") if $DEBUG::symtab;
    }
    $name = my $shortname = shift @components;
    return self unless defined $name and $name ne '';
    if $shortname ~~ /\:/ {
        ($shortname) = ($shortname ~~ /^(.*?)\:/);
    }

    my $declaring = $*DECLARAND // NAME.new(
        name => $name,
        file => $*FILE, line => self.line,
        mult => ($*MULTINESS||'only'),
        of   => $*OFTYPE,
        scope => $*SCOPE,
    );
    my $old = $curstash.{$name};
    if $old and $old<line> and not $old<stub> {
        my $omult = $old<mult> // '';
        if $declaring === $old {} # already did it somehow
        elsif $*SCOPE eq 'use' {}
        elsif $*MULTINESS eq 'multi' and $omult ne 'only' {}
        elsif $omult eq 'proto' and $*MULTINESS ne 'proto' and $*MULTINESS ne 'only' {}
        elsif $*PKGDECL eq 'role' {}
        else {
            my $ofile = $old.file // 0;
            my $oline = $old.line // '???';
            my $loc = '';
            if $ofile {
                if $ofile !=== $*FILE {
                    my $oname = $ofile<name>;
                    $loc = " (from $oname line $oline)";
                }
                else {
                    $loc = " (from line $oline)";
                }
            }
            $sid = self.clean_id($sid, $name);
            if $name ~~ /^\w/ {
                self.sorry("Illegal redeclaration of symbol '$sid'$loc");
            }
            elsif $name ~~ /^\&// {
                self.sorry("Illegal redeclaration of routine '$sid'$loc") unless $name eq '&';
            }
            else {  # XXX eventually check for conformant arrays here
                # (redeclaration of identical package vars is not useless)
            }
            return self;
        }
    }
    else {
        $*DECLARAND = $curstash.{$name} = $declaring;
        $curstash.{$shortname} = $curstash{$shortname} // $declaring unless $shortname eq $name;
        $*DECLARAND<inpkg> = $curstash.idref;
        if $shortname ~~ /^\w+$/ and $*IN_DECL ne 'constant' {
            $curstash.{"\&$shortname"} = $curstash.{"\&$shortname"} // $declaring;
            $curstash.{"\&$shortname"}<used> = 1;
            $sid ~= "::$name";
            $*NEWPKG = ($curstash.{$name ~ '::'} = $curstash.{$name ~ '::'} // STDStash.new(
                'PARENT::' => $curstash.idref,
                '!file' => $*FILE, '!line' => self.line,
                '!id' => [$sid] ));
        }
    }
    self.add_my_name($n, $declaring, $curstash.{$name ~ '::'}) if $curstash === $*CURPKG;   # the lexical alias
    self;
}

method add_mystery ($token,$pos,$ctx) {
    my $name = $token.Str;
    return self if $*IN_PANIC;
    if self.is_known('&' ~ $name) or self.is_known($name) {
        self.deb("$name is known") if $DEBUG::symtab;
    }
    else {
        self.deb("add_mystery $name $*CURLEX") if $DEBUG::symtab;
        %*MYSTERY{$name}.<lex> = $*CURLEX;
        %*MYSTERY{$name}.<token> = $token;
        %*MYSTERY{$name}.<ctx> = $ctx;
        %*MYSTERY{$name}.<line> ~= ',' if %*MYSTERY{$name}.<line>;
        %*MYSTERY{$name}.<line> ~= self.lineof($pos);
    }
    self;
}

method explain_mystery() {
    my %post_types;
    my %unk_types;
    my %unk_routines;
    my $m = '';
    for keys(%*MYSTERY) -> $unk {
        my $p = %*MYSTERY{$unk}.<lex>;
        if self.is_name($unk, $p) {
            # types may not be post-declared
            %post_types{$unk} = %*MYSTERY{$unk};
            next;
        }

        next if self.is_known($unk, $p) or self.is_known('&' ~ $unk, $p);

        # just a guess, but good enough to improve error reporting
        if $unk lt 'a' {
            %unk_types{$unk} = %*MYSTERY{$unk};
        }
        else {
            %unk_routines{$unk} = %*MYSTERY{$unk};
        }
    }
    if %post_types {
        my @tmp = sort keys(%post_types);
        $m ~= "Illegally post-declared type" ~ ('s' x (@tmp != 1)) ~ ":\n";
        for @tmp {
            $m ~= "\t'$_' used at line " ~ %post_types{$_}.<line> ~ "\n";
        }
    }
    if %unk_types {
        my @tmp = sort keys(%unk_types);
        $m ~= "Undeclared name" ~ ('s' x (@tmp != 1)) ~ ":\n";
        for @tmp {
            $m ~= "\t'$_' used at line " ~ %unk_types{$_}.<line> ~ "\n";
        }
    }
    if %unk_routines {
        my @tmp = sort keys(%unk_routines);
        $m ~= "Undeclared routine" ~ ('s' x (@tmp != 1)) ~ ":\n";
        for @tmp {
            $m ~= "\t'$_' used at line " ~ %unk_routines{$_}.<line> ~ "\n";
        }
    }
    self.sorry($m) if $m;
    self;
}

method load_setting ($setting) {
    $ALL = self.load_lex($setting);

    $*CORE = $ALL<CORE>;
    $*CORE.<!id> = $*CORE.<!id> // ['CORE'];

    $*SETTING = $ALL<SETTING>;
    $*CURLEX = $*SETTING;

    $*GLOBAL = $*CORE.<GLOBAL::> = STDStash.new(
        '!file' => $*FILE, '!line' => 1,
        '!id' => ['GLOBAL'],
    );
    $*CURPKG = $*GLOBAL;
}

method is_known ($n, $curlex = $*CURLEX) {
    my $name = $n;
    self.deb("is_known $name") if $DEBUG::symtab;
    return True if $*QUASIMODO;
    return True if $*CURPKG.{$name};
    return False if $name ~~ /\:\:\(/;
    my $curpkg = $*CURPKG;
    my @components = self.canonicalize_name($name);
    if @components > 1 {
        return True if @components[0] eq 'COMPILING::';
        return True if @components[0] eq 'CALLER::';
        return True if @components[0] eq 'CONTEXT::';
        if $curpkg = self.find_top_pkg(@components[0]) {
            self.deb("Found lexical package ", @components[0]) if $DEBUG::symtab;
            shift @components;
        }
        else {
            self.deb("Looking for GLOBAL::<$name>") if $DEBUG::symtab;
            $curpkg = $*GLOBAL;
        }
        while @components > 1 {
            my $pkg = shift @components;
            self.deb("Looking for $pkg in $curpkg ", join ' ', keys(%$curpkg)) if $DEBUG::symtab;
            $curpkg = $curpkg.{$pkg};
            return False unless $curpkg;
            try {
                my $outlexid = $curpkg.[0];
                return False unless $outlexid;
                $curpkg = $ALL.{$outlexid};
                return False unless $curpkg;
            };
            self.deb("Found $pkg okay, now in $curpkg ") if $DEBUG::symtab;
        }
    }

    $name = shift(@components)//'';
    self.deb("Final component is $name") if $DEBUG::symtab;
    return True if $name eq '';
    if $curpkg.{$name} {
        self.deb("Found") if $DEBUG::symtab;
        $curpkg.{$name}<used>++;
        return True;
    }
    # leading components take us non-lexical?  assume we can't know
    return False if $curpkg !=== $*CURPKG and $curpkg<!id>[0] ~~ /^GLOBAL($|\:\:)/;

    my $varbind = { truename => '???' };
    return True if $n !~~ /\:\:/ and self.lex_can_find_name($curlex,$name,$varbind);
    self.deb("Not Found") if $DEBUG::symtab;

    return False;
}

method lex_can_find_name ($lex, $name, $varbind) {
    self.deb("Looking in ", $lex.id) if $DEBUG::symtab;
    if $lex.{$name} {
        self.deb("Found $name in ", $lex.id) if $DEBUG::symtab;
        $lex.{$name}<used>++;
        return True;
    }

    my $outlexid = $lex.<OUTER::>[0];
    return False unless $outlexid;
    my $outlex = $ALL.{$outlexid};

    if self.lex_can_find_name($outlex,$name,$varbind) {
        # fake up an alias to outer symbol to catch reclaration
        my $outname = $outlex.{$name}<name>;
        my $outfile = $outlex.{$name}<file>;
        my $outline = $outlex.{$name}<line>;
        $outname = '<' ~ $outname ~ '>' unless $outname ~~ /\:\:\</;
        $outname = "OUTER::" ~ $outname;
        $lex.{$name} = NAME.new(
            olex => $lex.idref,
            name => $outname,
            file => $outfile, line => $outline,
            rebind => self.line,
            varbind => $varbind,
            mult => 'only',
            scope => $lex.{$name}<scope>,
        );
        # the innermost lex sets this last to get correct # of OUTER::s
        $varbind.<truename> = $outname;
        return True;
    }

    return False;
}

method add_routine ($name) {
    @*MEMOS[self.pos]<wasname> = $name if self.is_name($name);
    my $vname = '&' ~ $name;
    self.add_name($vname);
    self;
}

method add_variable ($name) {
    my $scope = $*SCOPE || 'our';
    return self if $scope eq 'anon';
    if $scope eq 'our' {
        self.add_our_name($name);
    }
    else {
        self.add_my_name($name);
    }
    self;
}

method add_constant($name,$value) {
    my $*IN_DECL = 'constant';
    self.deb("add_constant $name = $value in", $*CURLEX.id) if $DEBUG::symtab;
    my $*DECLARAND;
    self.add_my_name($name);
    $*DECLARAND<value> = $value;
    self;
}

method add_placeholder($name) {
    my $decl = $*CURLEX.<!IN_DECL> // '';
    $decl = ' ' ~ $decl if $decl;
    my $*IN_DECL = 'variable';

    if $*SIGNUM {
        return self.sorry("Placeholder variable $name is not allowed in the$decl signature");
    }
    elsif my $siggy = $*CURLEX.<$?SIGNATURE> {
        return self.sorry("Placeholder variable $name cannot override existing signature $siggy");
    }
    if not $*CURLEX.<!NEEDSIG> {
        if $*CURLEX === $*UNIT {
            return self.sorry("Placeholder variable $name may not be used outside of a block");
        }
        return self.sorry("Placeholder variable $name may not be used here because the surrounding$decl block takes no signature");
    }
    if $name ~~ /\:\:/ {
        return self.sorry("Placeholder variable $name may not be package qualified");
    }

    my $varname = $name;
    my $twigil;
    my $signame;
    my @r = ($varname ~~ /(.*?)(<[ ^ : ]>?)(.*)/);
    $twigil = @r[1];
    $signame = @r[1] eq ':' ?? ':' : ''
    $varname = @r[0] ~ @r[2];
    $signame ~= $varname;
    return self if $*CURLEX.{'%?PLACEHOLDERS'}{$signame}++;

    if $*CURLEX{$varname} {
        return self.sorry("$varname has already been used as a non-placeholder in the surrounding$decl block,\n  so you will confuse the reader if you suddenly declare $name here");
    }
 
    self.add_my_name($varname);
    $*CURLEX{$varname}<used> = 1;
    self;
}

method check_variable ($variable) {
    my $name = $variable.Str;
    self.deb("check_variable $name") if $DEBUG::symtab;
    my ($sigil, $twigil, $first) = $name ~~ /(\$|\@|\%|\&)(\W*)(.?)/;
    if $twigil eq '' {
        my $ok = 0;
        $ok = $ok || $*IN_DECL;
        $ok = $ok || $sigil eq '&';
        $ok = $ok || $first lt 'A';
        $ok = $ok || self.is_known($name);
        $ok = $ok || $name ~~ /.\:\:/ && $name !~~ /MY|UNIT|OUTER|SETTING|CORE/;
        if not $ok {
            my $id = $name;
            ($id) = ($id ~~ /\W ** 0..2 (.*)/);
            if $name eq '@_' or $name eq '%_' {
                $variable.add_placeholder($name);
            }
            else {  # guaranteed fail now
                if my $scope = @*MEMOS[$variable.from]<declend> {
                    return $variable.sorry("Variable $name is not predeclared (declarators are tighter than comma, so maybe your '$scope' signature needs parens?)");
                }
                elsif $id !~~ /\:\:/ {
                    if self.is_known('@' ~ $id) {
                        return $variable.sorry("Variable $name is not predeclared (did you mean \@$id?)");
                    }
                    elsif self.is_known('%' ~ $id) {
                        return $variable.sorry("Variable $name is not predeclared (did you mean \%$id?)");
                    }
                }
                return $variable.sorry("Variable $name is not predeclared");
            }
        }
        elsif $*CURLEX{$name} {
            $*CURLEX{$name}<used>++;
        }
    }
    elsif $twigil eq '^' {
        my $*MULTINESS = 'multi';
        $variable.add_placeholder($name);
    }
    elsif $twigil eq ':' {
        my $*MULTINESS = 'multi';
        $variable.add_placeholder($name);
    }
    elsif $twigil eq '~' {
        return %*LANG.{substr($name,2,$name.chars - 2)};
    }
    elsif $twigil eq '?' {
        if $name ~~ /\:\:/ {
            my ($first) = self.canonicalize_name($name);
            $variable.worry("Unrecognized variable: $name") unless $first ~~ /^(CALLER|CONTEXT|OUTER|MY|SETTING|CORE)\:\:$/;
        }
        else {
            # search upward through languages to STD
            my $v = $variable.lookup_compiler_var($name);
            $variable.<value> = $v if $v;
        }
    }
    self;
}

method lookup_compiler_var($name, $default?) {

    # see if they did "constant $?FOO = something" earlier
    my $lex = $*CURLEX.{$name};
    if defined $lex {
        if $lex.<thunk>:exists {
            return $lex.<thunk>.();
        }
        else {
            return $lex.<value>;
        }
    }

    if    $name eq '$?FILE'     { return $*FILE<name>; }
    elsif $name eq '$?LINE'     { return self.lineof(self.pos); }
    elsif $name eq '$?POSITION' { return self.pos; }

    elsif $name eq '$?LANG'     { return item %*LANG; }

    elsif $name eq '$?LEXINFO'   { return $*CURLEX; }

    elsif $name eq '$?PACKAGE'  { return $*CURPKG; }
    elsif $name eq '$?MODULE'   { return $*CURPKG; } #  XXX should scan
    elsif $name eq '$?CLASS'    { return $*CURPKG; } #  XXX should scan
    elsif $name eq '$?ROLE'     { return $*CURPKG; } #  XXX should scan
    elsif $name eq '$?GRAMMAR'  { return $*CURPKG; } #  XXX should scan

    elsif $name eq '$?PACKAGENAME' { return $*CURPKG.id }

    elsif $name eq '$?OS'       { return 'unimpl'; }
    elsif $name eq '$?DISTRO'   { return 'unimpl'; }
    elsif $name eq '$?VM'       { return 'unimpl'; }
    elsif $name eq '$?XVM'      { return 'unimpl'; }
    elsif $name eq '$?PERL'     { return 'unimpl'; }

    elsif $name eq '$?USAGE'    { return 'unimpl'; }

    elsif $name eq '&?ROUTINE'  { return 'unimpl'; }
    elsif $name eq '&?BLOCK'    { return 'unimpl'; }

    elsif $name eq '%?CONFIG'    { return 'unimpl'; }
    elsif $name eq '%?DEEPMAGIC' { return 'unimpl'; }

    else {
        my $dynvar = self.lookup_dynvar($name);
        return $dynvar if defined $dynvar;

        return $default if defined $default;
        # (derived grammars should default to nextsame, terminating here)
        default { self.worry("Unrecognized variable: $name"); return 0; }
    }
}

####################
# Service Routines #
####################

method panic (Str $s) {
    die "Recursive panic" if $*IN_PANIC;
    $*IN_PANIC++;
    my $m;
    my $here = self;

    # Have we backed off recently?
    my $highvalid = self.pos <= $*HIGHWATER;

    $here = self.cursor($*HIGHWATER) if $highvalid;

    my $first = $here.lineof($*LAST_NIBBLE.from);
    my $last = $here.lineof($*LAST_NIBBLE.pos);
    if $first != $last {
        if $here.lineof($here.pos) == $last {
            $m ~= "(Possible runaway string from line $first)\n";
        }
        else {
            $first = $here.lineof($*LAST_NIBBLE_MULTILINE.from);
            $last = $here.lineof($*LAST_NIBBLE_MULTILINE.pos);
            # the bigger the string (in lines), the further back we suspect it
            if $here.lineof($here.pos) - $last < $last - $first  {
                $m ~= "(Possible runaway string from line $first to line $last)\n";
            }
        }
    }

    $m ~= $s;

    if substr(self.orig,$here.pos,1) ~~ /\)|\]|\}|\»/ {
        my $ma = ($m ~~ /(.*?)Confused(.*)/);
        if ($ma) {
            $m = $ma[0] ~ "Unexpected closing bracket" ~ $ma[1];
            $highvalid = False;
        }
    }

    if $highvalid {
        $m ~= $*HIGHMESS if $*HIGHMESS;
        $*HIGHMESS = $m;
    }
    else {
        # not in backoff, so at "bleeding edge", as it were... therefore probably
        # the exception will be caught and re-panicked later, so remember message
        $*HIGHMESS ~= $s ~ "\n";
    }

    $m ~= $here.locmess;
    $m ~= "\n" unless $m ~~ /\n$/;

    if $highvalid and %$*HIGHEXPECT {
        my @keys = sort keys %$*HIGHEXPECT;
        if @keys > 1 {
            $m ~= "    expecting any of:\n\t" ~ join("\n\t", sort keys %$*HIGHEXPECT) ~ "\n";
        }
        else {
            $m ~= "    expecting @keys\n" unless @keys[0] eq 'whitespace';
        }
    }
    if $m ~~ /infix|nofun/ and not $m ~~ /regex/ and not $m ~~ /infix_circumfix/ {
        my @t = $here.suppose( sub { $here.term } );
        my $conf;
        if @t {
            my $endpos = $here.pos;
            my $startpos = @*MEMOS[$endpos]<ws> // $endpos;

            if self.lineof($startpos) != self.lineof($endpos) {
                $conf = "Two terms in a row (previous line missing its semicolon?)";
            }
            elsif @*MEMOS[$here.pos - 1]<baremeth> {
                $conf = "Two terms in a row (method call requires colon or parens to take arguments)";
            }
            elsif @*MEMOS[$here.pos - 1]<arraycomp> {
                $conf = "Two terms in a row (preceding is not a valid reduce operator)";
            }
            else {
                $conf = "Two terms in a row";
            }
        }
        elsif my $type = @*MEMOS[$here.pos - 1]<nodecl> {
            my @t = $here.suppose( sub { $here.variable } );
            if @t {
                my $variable = @t[0].Str;
                $conf = "Bare type $type cannot declare $variable without a preceding scope declarator such as 'my'";
            }
        }
        if $conf && my $ma = ($m ~~ /(.*?)Confused(.*)/) {
            $m = $ma[0] ~ $conf ~ $ma[1];
        }
    }
    elsif my $type = @*MEMOS[$here.pos - 1]<wasname> {
        my @t = $here.suppose( sub { $here.identifier } );
        my $name = @t[0].Str;
        my $s = $*SCOPE ?? "'$*SCOPE'" !! '(missing) scope declarator';
        my $d = $*IN_DECL;
        $d = "$*MULTINESS $d" if $*MULTINESS and $*MULTINESS ne $d;
        my ($a,$b) = $m ~~ /(.*?)Malformed block(.*)/;
        defined($a) and $m = $a ~ "Return type $type is not allowed between '$d' and '$name'; please put it:\n  after the $s but before the '$d',\n  within the signature following the '-->' marker, or\n  as the argument of a 'returns' trait after the signature.$b";
    }

    if @*WORRIES {
        $m ~= "Other potential difficulties:\n  " ~ join( "\n  ", @*WORRIES) ~ "\n";
    }

    $*IN_PANIC--;
    die $m if $*IN_SUPPOSE;     # just throw the exception back to the supposer
    $*IN_PANIC++;

    note $Cursor::RED, '===', $Cursor::CLEAR, 'SORRY!', $Cursor::RED, '===', $Cursor::CLEAR, "\n"
        unless $*FATALS++;
    note $m;
    self.explain_mystery();

    $*IN_PANIC--;
    die "Parse failed\n";
}

regex is_ok {
    \N*? '#OK' \h*? $<okif>=[\N*?] \h*? $$
}

method worry (Str $s) {
    my $m = $s ~ self.locmess;

    # allow compile-time warning suppression with #OK some string
    my ($okmaybe) = self.suppose( sub {
        self.is_ok;
    });
    if $okmaybe {
        my $okif = $okmaybe<okif>.Str;
        return self if $okif eq '' or $s ~~ /$okif/;
    }

    push @*WORRIES, $m unless %*WORRIES{$s}++;
    self;
}

method sorry (Str $s) {
    self.deb("sorry $s") if $*DEBUG;
    note $Cursor::RED, '===', $Cursor::CLEAR, 'SORRY!', $Cursor::RED, '===', $Cursor::CLEAR, "\n"
        unless $*IN_SUPPOSE or $*FATALS++;
    if $s {
        my $m = $s;
        $m ~= self.locmess ~ "\n" unless $m ~~ /\n$/;
        if $*FATALS > 10 or $*IN_SUPPOSE {
            die $m;
        }
        else {
            note $m unless %*WORRIES{$m}++;
        }
    }
    self;
}

# "when" arg assumes more things will become obsolete after Perl 6 comes out...

method obs (Str $old, Str $new, Str $when = ' in Perl 6') {
    %$*HIGHEXPECT = ();
    self.panic("Unsupported use of $old;$when please use $new");
}

method sorryobs (Str $old, Str $new, Str $when = ' in Perl 6') {
    %$*HIGHEXPECT = ();
    self.sorry("Unsupported use of $old;$when please use $new");
    self;
}

method worryobs (Str $old, Str $new, Str $when = ' in Perl 6') {
    self.worry("Unsupported use of $old;$when please use $new");
    self;
}

method dupprefix (Str $bad) {
    my $c = substr($bad,0,1);
    self.panic("Expecting a term, but found either infix $bad or redundant prefix $c\n  (to suppress this message, please use space between $c $c)");
}

method badinfix (Str $bad) {
    self.panic("Preceding context expects a term, but found infix $bad instead");
}

# Since most keys are valid prefix operators or terms, this rule is difficult
# to reach ('say »+«' works), but it's okay as a last-ditch default anyway.
token term:sym<miscbad> {
    {} <!{ $*QSIGIL }>
    {{
        my ($bad) = $¢.suppose( sub {
            $¢.infixish;
        });
        $*HIGHWATER = -1;
        $*HIGHMESS = '';
        self.badinfix($bad.Str) if $bad;
    }}
    <!>
}

## vim: expandtab sw=4 ft=perl6
