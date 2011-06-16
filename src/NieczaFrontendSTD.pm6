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
role sym_categorical[$name,$sym,$O] {
    token ::($name) () { $sym $<O>={$O} $<sym>={$sym} }
}
role bracket_categorical[$name,$sym1,$sym2,$O] {
    token ::($name) () { :my $*GOAL = $sym2; $sym1 {}:s [ :lang($¢.unbalanced($sym2)) <semilist> ] [ $sym2 || <.FAILGOAL($sym2, $name, self.pos)> ] $<O>={$O} $<sym>={[$sym1,$sym2]} }
}

method add_categorical($name) {
    # Signature extension, not categorical
    if $name ~~ /^\w+\:\(/ {
        return self;
    }
    # CORE names are hardcoded
    return self if $*UNITNAME eq 'CORE';
    return self unless ($name ~~ /^(\w+)\: <?[ \< \« ]> /);
    my $cat = ~$0;
    my $sym = substr($name, $/.to);
    if $sym ~~ /^\<\< .*: <?after \>\>>$/ {
        $sym = substr($sym, 2, $sym.chars - 4);
    }
    elsif $sym ~~ /^\< .*: <?after \>>$/ {
        $sym = substr($sym, 1, $sym.chars - 2);
    }
    elsif $sym ~~ /^\« .*: <?after \»>$/ {
        $sym = substr($sym, 1, $sym.chars - 2);
    }

    $sym ~~ s/^\s*//;
    $sym ~~ s/\s*$//;

    my $O;

    if $cat eq 'infix'            { $O = %additive }
    elsif $cat eq 'prefix'        {
        $O = ($sym ~~ /^\W/) ?? %symbolic_unary !! %named_unary
    }
    elsif $cat eq 'postfix'       { $O = %methodcall }
    elsif $cat eq 'circumfix'     { $O = %term }
    elsif $cat eq 'postcircumfix' { $O = %methodcall }
    elsif $cat eq 'term'          { $O = %term }
    else {
        self.sorry("Cannot extend category:$name with subs");
        return self;
    }

    # XXX to do this right requires .comb and .trans
    if $sym ~~ /\s+/ {
        my $sym1 = $sym.substr(0, $/.from);
        my $sym2 = $sym.substr($/.to, $sym.chars - $/.to);
        my $cname = $cat ~ ":<$sym1 $sym2>";
        %*LANG<MAIN> = self.WHAT but OUR::bracket_categorical[$cname, $sym1, $sym2, $O];
    } else {
        my $cname = $cat ~ ":<$sym>";
        %*LANG<MAIN> = self.WHAT but OUR::sym_categorical[$cname, $sym, $O];
    }
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

method sorry (Str $s) {
    self.deb("sorry $s") if $*DEBUG;
    die $s if $*IN_EVAL;
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
method panic (Str $s) {
    die "Recursive panic" if $*IN_PANIC;
    $*IN_PANIC++;
    self.deb("panic $s") if $*DEBUG;
    my $m;
    my $here = self;

    my $first = $here.lineof($*LAST_NIBBLE_START);
    my $last = $here.lineof($*LAST_NIBBLE);
    if $first != $last {
        if $here.lineof($here.pos) == $last {
            $m ~= "(Possible runaway string from line $first)\n";
        }
        else {
            $first = $here.lineof($*LAST_NIBBLE_MULTILINE_START);
            $last = $here.lineof($*LAST_NIBBLE_MULTILINE);
            # the bigger the string (in lines), the further back we suspect it
            if $here.lineof($here.pos) - $last < $last - $first  {
                $m ~= "(Possible runaway string from line $first to line $last)\n";
            }
        }
    }

    $m ~= $s;

    if substr(self.orig,$here.pos,1) ~~ /\)|\]|\}|\»/ {
        $m ~~ s|Confused|Unexpected closing bracket|;
    }

    $m ~= $here.locmess;
    $m ~= "\n" unless $m ~~ /\n$/;

    if $m ~~ /infix|nofun/ and not $m ~~ /regex/ and not $m ~~ /infix_circumfix/ {
        my @t = $here.suppose( sub { $here.term } );
        if @t {
            my $endpos = $here.pos;
            my $startpos = @*MEMOS[$endpos]<ws> // $endpos;

            if self.lineof($startpos) != self.lineof($endpos) {
                $m ~~ s|Confused|Two terms in a row (previous line missing its semicolon?)|;
            }
            elsif @*MEMOS[$here.pos - 1]<baremeth> {
                $m ~~ s|Confused|Two terms in a row (method call requires colon or parens to take arguments)|;
            }
            elsif @*MEMOS[$here.pos - 1]<arraycomp> {
                $m ~~ s|Confused|Two terms in a row (preceding is not a valid reduce operator)|;
            }
            else {
                $m ~~ s|Confused|Two terms in a row|;
            }
        }
        elsif my $type = @*MEMOS[$here.pos - 1]<nodecl> {
            my @t = $here.suppose( sub { $here.variable } );
            if @t {
                my $variable = @t[0].Str;
                $m ~~ s|Confused|Bare type $type cannot declare $variable without a preceding scope declarator such as 'my'|;
            }
        }
    }
    elsif my $type = @*MEMOS[$here.pos - 1]<wasname> {
        my @t = $here.suppose( sub { $here.identifier } );
        my $name = @t[0].Str;
        my $s = $*SCOPE ?? "'$*SCOPE'" !! '(missing) scope declarator';
        my $d = $*IN_DECL;
        $d = "$*MULTINESS $d" if $*MULTINESS and $*MULTINESS ne $d;
        $m ~~ s|Malformed block|Return type $type is not allowed between '$d' and '$name'; please put it:\n  after the $s but before the '$d',\n  within the signature following the '-->' marker, or\n  as the argument of a 'returns' trait after the signature.|;
    }

    if @*WORRIES {
        $m ~= "Other potential difficulties:\n  " ~ join( "\n  ", @*WORRIES) ~ "\n";
    }

    $*IN_PANIC--;
    die $m if $*IN_SUPPOSE || $*IN_EVAL;     # just throw the exception back to the supposer
    $*IN_PANIC++;

    note $Cursor::RED, '===', $Cursor::CLEAR, 'SORRY!', $Cursor::RED, '===', $Cursor::CLEAR, "\n"
        unless $*FATALS++;
    note $m;
    self.explain_mystery();

    $*IN_PANIC--;
    note "Parse failed\n";
    exit 1;
}
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

    my $*unit = ::Metamodel::Unit.new(name => $unitname,
        ns => ::Metamodel::Namespace.new, :$filename, :$modtime);

    if $*niecza_outer_ref {
        $*unit.setting_ref = $*niecza_outer_ref;
        $*unit.need_unit($*unit.setting_ref.[0]);
    } elsif $lang ne 'NULL' {
        $*unit.need_unit($lang);
        $*unit.setting_ref = $*unit.get_unit($lang).bottom_ref;
    }
    %*units{$unitname} = $*unit;
    $*unit.tdeps{$unitname} = [$filename, $modtime];

    $*unit.create_stash(['GLOBAL']);
    $*unit.create_stash(['PROCESS']);

    my $ast = NieczaGrammar.parse($source, actions => NieczaActions).ast;

    @STD::herestub_queue = @save_herestub;

    $ast;
}
