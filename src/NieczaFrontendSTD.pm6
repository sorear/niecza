class NieczaFrontendSTD;

use STD;
use NieczaGrammar;
use NieczaActions;

use MONKEY_TYPING;
augment class Match {
    method CURSOR() { Q:CgOp { (ns (rawcall UnMatch (cast cursor (@ {self})))) } }
    method cursor() { Q:CgOp { (ns (rawcall UnMatch (cast cursor (@ {self})))) } }
    method reduced() { Q:CgOp { (box Str (rawcall Reduced (cast cursor (@ {self})))) } }
    method trim_heredoc () { self } # NYI
}

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
    my $lex = self.lookup_lex($name);

    if $lex ~~ ::Metamodel::Lexical::Dispatch {
        $lex = self.lookup_lex($name ~ ":(!proto)");
    }

    my $sub;

    if $lex ~~ ::Metamodel::Lexical::Common {
        $sub = $*unit.deref($*unit.get($*unit.deref($lex.pkg), $lex.name));
    } elsif $lex ~~ ::Metamodel::Lexical::SubDef {
        $sub = $lex.body;
    }

    if $sub ~~ ::Metamodel::StaticSub {
        return $sub.extend<prec>;
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

# MOP will be used to install $*rx into appropriate method field
role sym_categorical[$name,$cat,$sym] {
    $*name = $name;
    $*rxm = anon token sym_categorical () {
        $sym $<sym>={$sym} $<name>={$name}
        $<O>={ self.cat_O($cat, $sym) }
    }
}

role bracket_categorical[$name,$cat,$sym1,$sym2] {
    $*name = $name;
    $*rxm = anon token bracket_categorical () {
        :my $*GOAL = $sym2;
        $sym1 {}:s
        $<name>={$name}
        [ :lang($¢.unbalanced($sym2)) <semilist> ]
        [ $sym2 || <.FAILGOAL($sym2, $name, self.pos)> ]
        $<O>={ self.cat_O($cat, "$sym1 $sym2") } $<sym>={ [$sym1,$sym2] }
    }
}

method add_categorical($name) {
    # Signature extension, not categorical
    if $name ~~ /^\w+\:\(/ {
        return self;
    }
    # CORE names are hardcoded
    return self if $*UNITNAME eq 'CORE';
    return self unless ($name ~~ /^(\w+)\: \< (.*) \> /);
    my $cat = ~$0;
    my $sym = ~$1;
    my ($role, $*rxm, $*name);

    if $sym ~~ /\s+/ {
        my $sym1 = $sym.substr(0, $/.from);
        my $sym2 = $sym.substr($/.to, $sym.chars - $/.to);
        $role = OUR::bracket_categorical["{$cat}:sym<$sym1 $sym2>",
            $cat, $sym1, $sym2];
    } else {
        $role = OUR::sym_categorical["{$cat}:sym<$sym>", $cat, $sym];
    }

    # $*name will be set if the role blocks are run.  If $*name is not set,
    # then a cached role was reused and there is no need to fix up method names.
    if defined $*name {
        Q:CgOp { (rnull (_addmethod (obj_llhow (@ {$role})) 8
            (obj_getstr {$*name}) (@ {$*rxm}))) };
        Q:CgOp { (rnull (_invalidate (obj_llhow (@ {$role})))) };
    }

    %*LANG<MAIN> = self.WHAT but $role;
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

method SETGOAL { }
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

method mixin($role) { self.cursor_fresh(self.WHAT but $role) }

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

augment class Cursor {
    our $RED    = "\e[31m";
    our $GREEN  = "\e[32m";
    our $YELLOW = "\e[33m";
    our $CLEAR  = "\e[0m";

}

has $.lang;
has $.safemode;

method parse(:$unitname, :$filename, :$modtime, :$source, :$outer) {

    my $*SAFEMODE    = $.safemode;
    my $*UNITNAME    = $unitname;
    my $*modtime     = $modtime;

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

    $DEBUG::EXPR = $STD::DEBUG::EXPR = $STD::DEBUG::symtab =
        $*verbose > 1;

    my $*LAST_NIBBLE = 0;
    my $*LAST_NIBBLE_START = 0;
    my $*LAST_NIBBLE_MULTILINE = 0;
    my $*LAST_NIBBLE_MULTILINE_START = 0;
    my $*GOAL = "(eof)";
    my $*UNIT;
    my $*CCSTATE; my $*BORG; my %*RX; my $*XACT; my $*VAR; my $*IN_REDUCE;

    my $*unit = $*backend.create_unit($unitname, $filename, $modtime);
    %*units{$unitname} = $*unit;
    $*unit.set_current;

    if $*niecza_outer_ref {
        $*unit.setting_ref = $*niecza_outer_ref;
        $*unit.need_unit($*unit.setting_ref.[0]);
    } elsif $lang ne 'NULL' {
        $*unit.need_unit($lang);
        $*unit.setting_ref = $*unit.get_unit($lang).bottom_ref;
    }

    $*unit.abs_pkg('GLOBAL', :auto);
    $*unit.abs_pkg('PROCESS', :auto);

    my $ast = NieczaGrammar.parse($source, actions => NieczaActions).ast;

    @STD::herestub_queue = @save_herestub;

    CALLER::<$*unit> && CALLER::<$*unit>.set_current;
    $ast;
}
