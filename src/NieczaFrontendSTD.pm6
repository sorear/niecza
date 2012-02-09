our ($Actions, $Backend);

class NieczaFrontendSTD;

use STD;
use NieczaGrammar;

use MONKEY_TYPING;

augment class STD {
my %term            = (:dba('term')            , :prec<z=>);
my %methodcall      = (:dba('methodcall')      , :prec<y=>, :assoc<unary>, :uassoc<left>, :fiddly, :!pure);
my %symbolic_unary  = (:dba('symbolic unary')  , :prec<v=>, :assoc<unary>, :uassoc<left>, :pure);
my %additive        = (:dba('additive')        , :prec<t=>, :assoc<left>, :pure);
my %named_unary     = (:dba('named unary')     , :prec<o=>, :assoc<unary>, :uassoc<left>, :pure);

# TODO: allow variable :dba()s

method cat_O($cat, $sym) {
    my $name = "&{$cat}:<$sym>";
    self.function_O($name) // self.default_O($cat, $sym);
}

method function_O($name) {
    my @lex = self.lookup_lex($name);

    if @lex && @lex[0] eq 'dispatch' {
        @lex = self.lookup_lex($name ~ ":(!proto)");
    }

    return Any unless @lex;

    my $sub;

    if @lex[0] eq 'common' {
        $sub = $*unit.get(@lex[4], @lex[5]);
    } elsif @lex[0] eq 'sub' {
        $sub = @lex[4];
    }

    if $sub && $sub.kind eq 'sub' {
        my %ext = $sub.get_extend('prec');
        return %ext || Any;
    } else {
        return Any;
    }
}

method default_O($cat, $sym) {
    given $cat {
        when 'infix'         { return %additive }
        when 'prefix'        {
            return ($sym ~~ /^\W/) ?? %symbolic_unary !! %named_unary
        }
        when 'postfix'       { return %methodcall }
        when 'circumfix'     { return %term }
        when 'postcircumfix' { return %methodcall }
        when 'term'          { return %term }
        default {
            self.sorry("Cannot extend category:$cat with subs");
            return %additive
        }
    }
}

sub rolecache($key, $thunk) {
    state %cache;
    (%cache{$key}:exists) ?? %cache{$key} !! (%cache{$key} := $thunk())
}

method balanced ($start,$stop) { self.mixin( rolecache("B$start\0$stop", {STD::startstop[$start,$stop]}) ); }
method unbalanced ($stop) { self.mixin( rolecache("U$stop", {STD::stop[$stop]}) ); }
method unitstop ($stop) { self.mixin( rolecache("N$stop", {STD::unitstop[$stop]}) ); }

method cat_role($cat,$sym) {
    state %cat_cache;

    my $name = "{$cat}:<{$sym}>";

    if %cat_cache{$name}:exists {
        return %cat_cache{$name};
    }

    my $role;
    # need these readonly for proper LTM

    if $sym ~~ /\s+/ {
        my $sym1 ::= $sym.substr(0, $/.from);
        my $sym2 ::= $sym.substr($/.to, $sym.chars - $/.to);
        my $mname = "{$cat}:sym<$sym1 $sym2>";

        my $meth = anon token bracket_categorical () {
            :my $*GOAL := $sym2;
            $sym1 {}:s
            $<name>={$mname}
            [ :lang($¢.unbalanced($sym2)) <semilist> ]
            [ $sym2 || <.FAILGOAL($sym2, $mname, self.pos)> ]
            $<O>={ self.cat_O($cat, "$sym1 $sym2") } $<sym>={ [$sym1,$sym2] }
        }

        $role = $Backend.make_role($mname, $meth);
    } else {
        my $mname = "{$cat}:sym<$sym>";
        my $meth = anon token sym_categorical () {
            $sym $<sym>={$sym} $<name>={$mname}
            $<O>={ self.cat_O($cat, $sym) }
        }
        $role = $Backend.make_role($mname, $meth);
    }
    return (%cat_cache{$name} := $role);
}

method exists_syntax($cat, $sym) {
    return True if self.can("{$cat}:sym<{$sym}>");
    return True if self.can("{$cat}:{$sym}");
    return True if $sym eq any < ...^ >;
    return False;
}

method add_categorical($name) {
    # Signature extension, not categorical
    if $name ~~ /^\w+\:\(/ {
        return self;
    }

    # CORE names are hardcoded
    return self unless ($name ~~ /^(\w+)\: \< (.*) \> /);
    return self if $*UNITNAME eq 'CORE' && self.exists_syntax($0, $1);

    %*LANG<MAIN> = $Backend.cached_but(self.WHAT, self.cat_role($0, $1));
    self.cursor_fresh(%*LANG<MAIN>);
}

method batch_categoricals(@names) {
    my @roles;
    for @names -> $name {
        next if $name ~~ /^\w+\:\(/;
        next unless ($name ~~ /^(\w+)\: \< (.*) \> /);
        # XXX: This is maybe not 100% right as it doesn't allow user modules
        # to overwrite parsed setting macros with subs
        next if self.exists_syntax($0, $1);

        push @roles, self.cat_role($0, $1);
    }

    %*LANG<MAIN> = self.WHAT but @roles;
    self.cursor_fresh(%*LANG<MAIN>);
}

method locmess () {
    my $pos = self.pos;
    my $line = self.lineof($pos);

    if $pos >= chars(self.orig) {
        $line = $line ~ " (EOF)";
    }

    my $pre = substr(self.orig, 0, $pos);
    my $prel = chars($pre) min 40;
    $pre = substr($pre, chars($pre)-$prel, $prel);
    if ($pre ~~ /^.*\n/) {
        $pre = substr($pre, $/.to);
    }
    $pre = '<BOL>' if $pre eq '';
    my $post = substr(self.orig, $pos, (chars(self.orig)-$pos) min 40);
    if ($post ~~ /\n/) {
        $post = substr($post,0,$/.from);
    }
    $post = '<EOL>' if $post eq '';
    " at " ~ $*FILE<name> ~ " line $line:\n------> " ~ $Cursor::GREEN ~
        $pre ~ $Cursor::YELLOW ~ "\x23CF" ~ $Cursor::RED ~ $post ~
        $Cursor::CLEAR;
}

method line {
    self.lineof(self.pos);
}

method FAILGOAL ($stop, $name, $startpos) {
    my $s = "'$stop'";
    $s = '"\'"' if $s eq "'''";
    self.panic("Unable to parse $name" ~ self.cursor($startpos).locmess ~ "\nCouldn't find final $s; gave up");
}

method deb(*@str) { note @str }

method cursor_fresh($k = self) { Q:CgOp {
    (ns (cursor_fresh (cast cursor (@ {self})) (@ {$k})))
} }
method cursor_force($pos) {
    $*HIGHWATER = $pos;
    self.cursor($pos);
}

method mixin($role) { self.cursor_fresh($Backend.cached_but(self.WHAT, $role)) }

method mark_sinks(@sl) { #OK not used
    #NYI
    self
}

method you_are_here() { self }
method lineof ($p) {
    return 1 unless defined $p;
    my $line = @*LINEMEMOS[$p];
    return $line if $line;
    $line = 1; my $pos = 0;
    my $lm = @*LINEMEMOS;
    self.orig ~~ / :r [ \n { $lm[$pos++] = $line++ } ||
                        .  { $lm[$pos++] = $line } ]* /;
    $lm[$pos++] = $line;
    return $lm[$p] // 0;
}

method lookup_dynvar($) { Any } # NYI
method check_old_cclass($) { } # NYI
}

# these stash entries were created in STD so they are considered to belong
# to STD, so we have to use an INIT to change them
INIT {
    $Cursor::RED    = "\e[31m";
    $Cursor::GREEN  = "\e[32m";
    $Cursor::YELLOW = "\e[33m";
    $Cursor::CLEAR  = "\e[0m";
}

has $.lang;
has $.safemode;

method parse(:$unitname, :$filename, :$source, :$outer, :$run, :$main, :$evalmode, :$repl) {

    my $*SAFEMODE    = $.safemode;
    my $*UNITNAME    = $unitname;

    my $lang = $!lang;
    if $unitname eq 'CORE' {
        $lang = 'NULL';
        $*SAFEMODE = False;
    } elsif $unitname ne 'MAIN' {
        $lang = 'CORE';
    }

    # XXX temp() or should be contextuals
    my @save_herestub = @STD::herestub_queue;

    @STD::herestub_queue = ();

    my $*niecza_outer_ref = $outer;
    my @*MEMOS;
    my @*LINEMEMOS;
    my $*FILE = { name => $filename };
    my @*ACTIVE;
    my $*HIGHWATER = 0;
    my $*HIGHEXPECT = {};
    my $*HIGHMESS = "";
    my $*LASTSTATE = 0;
    my $*IN_PANIC = 0;
    my $*IN_SUPPOSE = 0;
    my $*FATALS = 0;

    my $*LAST_NIBBLE = 0;
    my $*LAST_NIBBLE_START = 0;
    my $*LAST_NIBBLE_MULTILINE = 0;
    my $*LAST_NIBBLE_MULTILINE_START = 0;
    my $*GOAL = "(eof)";
    my $*UNIT;
    my $*CCSTATE; my $*BORG; my %*RX; my $*XACT; my $*VAR; my $*IN_REDUCE;

    $*backend.push_compartment unless $evalmode;
    LEAVE { $*backend.pop_compartment unless $evalmode };
    my $*unit = $*backend.create_unit($unitname, $filename, $source, $main, $run);
    my $*settingref = $*niecza_outer_ref ||
        ($lang ne 'NULL' ?? $*unit.need_unit($lang).bottom !! Any);

    $*unit.abs_pkg('GLOBAL', :auto);
    $*unit.abs_pkg('PROCESS', :auto);

    NieczaGrammar.parse($source, actions => $Actions);

    @STD::herestub_queue = @save_herestub;

    $*backend.accept($*unit, :$filename, :$run, :$evalmode, :$repl);

    $evalmode ?? $*unit !! Nil;
}
