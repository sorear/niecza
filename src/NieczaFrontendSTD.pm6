class NieczaFrontendSTD;

use STD;
use Stash;
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
our $ALL;

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

method add_enum($type,$expr) {
    return self unless $type;
    return self unless $expr;
    my $typename = $type.Str;
    my $*IN_DECL ::= 'constant';
    # XXX complete kludge, really need to eval EXPR
    # $expr =~ s/:(\w+)<\S+>/$1/g;  # handle :name<string>
    for $expr.comb(/ <[ a..z A..Z _ ]> \w* /) -> $n {
        self.add_name($typename ~ "::$n");
        self.add_name($n);
    }
    self
}

method canonicalize_name($n) {
    my $M;
    my $name = $n;
    if $M = head(/(< $ @ % & >)( \^ || \: <!before \:> )/(Cursor.new($name))) {
        $name = $M[0] ~ substr($name, $M.to);
    }
    if $name.chars >= 2 && substr($name, $name.chars - 2, 2) ~~ / \: < U D _ > / {
        $name = $name.substr(0, $name.chars - 2);
    }
    return $name unless $name ~~ /::/;
    self.panic("Cannot canonicalize a run-time name at compile time: $name") if $name ~~ / '::(' /;

    if $name ~~ /^ (< $ @ % & > < ! * = ? : ^ . >?) (.* '::')/ {
        $name = $1 ~ "<" ~ $0 ~ substr($name, $/.to) ~ ">";
    }
    my $vname;
    if ($name ~~ /'::<'/) && $name.substr($name.chars - 1, 1) eq '>' {
        $name = substr($name, 0, $/.from);
        $vname = substr($name, $/.to, $name.chars - $/.to - 1);
    }
    my @components;
    while $name ~~ / '::' / {
        push @components, $name.substr(0, $/.to);
        $name = substr($name, $/.to);
    }
    push @components, $name;
    shift(@components) while @components and @components[0] eq '';
    if (defined $vname) {
        if @components {
            my $last := @components[* - 1];
            $last ~= '::' if $last.chars < 2 || $last.substr($last.chars - 2, 2) ne '::';
        }
        push(@components, $vname) if defined $vname;
    }
    @components;
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

method gettrait($traitname,$param) {
    my $text;
    if @$param {
        $text = $param.[0].Str;
        ($text ~~ s/^\<(.*)\>$/$0/) ||
            ($text ~~ s/^\((.*)\)$/$0/);
    }
    if ($traitname eq 'export') {
        if (defined $text) {
            while $text ~~ s/\:// { }
        }
        else {
            $text = 'DEFAULT';
        }
        self.set_export($text);
        $text;
    }
    elsif (defined $text) {
        $text;
    }
    else {
        1;
    }
}

method set_export($text) {
    my $textpkg = $text ~ '::';
    my $name = $*DECLARAND<name>;
    my $xlex = $STD::ALL{ $*DECLARAND<inlex>[0] };
    $*DECLARAND<export> = $text;
    my $sid = $*CURLEX.idref;
    my $x = $xlex<EXPORT::> // Stash.new( 'PARENT::' => $sid, '!id' => [$sid.[0] ~ '::EXPORT'] );
    $xlex<EXPORT::> = $x;
    $x{$textpkg} = $x{$textpkg} // Stash.new( 'PARENT::' => $x.idref, '!id' => [$sid.[0] ~ '::EXPORT::' ~ $text] );
    $x{$textpkg}{$name} = $*DECLARAND;
    $x{$textpkg}{'&'~$name} = $*DECLARAND
            if $name ~~ /^\w/ and $*IN_DECL ne 'constant';
    self;
}

# only used for error reporting
method clean_id ($idx, $name) {
    my $id = $idx;
    my $file = $*FILE<name>;

    $id ~= '::';
    $id ~~ s/^'MY:file<CORE.setting>'.*?'::'/CORE::/;
    $id ~~ s/^MY\:file\<\w+\.setting\>.*?\:\:/SETTING::/;
    $id ~~ s/^MY\:file\<$file\>$/UNIT/;
    $id ~~ s/\:pos\(\d+\)//;
    $id ~= "<$name>";
    $id;
}

class LABEL {
    has $.file;
    has $.pos;
}

method label_id() {
    my $l = LABEL.new;
    $l.pos = self.pos;
    $l.file = $*FILE<name>;
    $l;
}

method do_import($m, $args) { #, perl6.vim stupidity
    my @imports;
    my $module = $m.Str;
    if $module ~~ /(class|module|role|package)\s+(\S+)/ {
        $module = ~$1;
    }

    my $pkg = self.find_stash($module);
    if $pkg<really> {
        $pkg = $pkg<really><UNIT>;
    }
    else {
        $pkg = self.find_stash($module ~ '::');
    }
    if $args {
        my $text = $args.Str;
        return self unless $text;
        while $text ~~ s/^\s*\:?(OUR|MY|STATE|HAS|AUGMENT|SUPERSEDE)?\<(.*?)\>\,?// {
            my $scope = lc($0 // 'my');
            my $imports = $1.Str;
            my $*SCOPE = $scope;
            @imports = $imports.comb(/\S+/);
            for @imports -> $i {
                my $imp = $i;
                if $pkg {
                    if $imp ~~ s/^\:// {
                        my @tagimports;
                        try { @tagimports = $pkg<EXPORT::>{$imp}.keys }
                        self.do_import_aliases($pkg, @tagimports);
                    }
                    elsif $pkg{$imp}<export> {
                        self.add_my_name($imp, $pkg{$imp});
                    }
                    elsif $pkg{'&'~$imp}<export> {
                        $imp = '&' ~ $imp;
                        self.add_my_name($imp, $pkg{$imp});
                    }
                    elsif $pkg{$imp} {
                        self.worry("Cannot import $imp because it's not exported by $module");
                        next;
                    }
                }
                else {
                    self.add_my_name($imp);
                }
            }
        }
    }
    else {
        return self unless $pkg;
        try { @imports = $pkg<EXPORT::><DEFAULT::>.keys };
        my $*SCOPE = 'my';
        self.do_import_aliases($pkg, @imports);
    }

    self;
}
method do_import_aliases($pkg, *@names) {
#    say "attempting to import @names";
    for @names -> $n {
        next if $n ~~ /^\!/;
        next if $n ~~ /^PARENT\:\:/;
        next if $n ~~ /^OUTER\:\:/;
        self.add_my_name($n, $pkg{$n});
    }
    self;
}

method you_are_here() { $*YOU_WERE_HERE = $*CURLEX; self }
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
method do_use($module,$args) {
    self.do_need($module);
    self.do_import($module,$args);
    self;
}

method do_need($mo) {
    my $module = $mo.Str;
    my $topsym;
    $topsym = self.sys_load_modinfo($module);
    if !defined $topsym {
        self.panic("Could not load $module");
    }
    self.add_my_name($module);
    $*DECLARAND<really> = $topsym;
    self;
}

method sys_load_modinfo($module) {
    # These are handled within the grammar
    if $module eq 'MONKEY_TYPING' || $module eq 'lib' ||
            $module eq 'fatal' {
        return { };
    }
    $*module_loader.($module).create_syml;
}

method load_lex($setting) {
    if $*niecza_outer_ref {
        my $sub = ::Metamodel::Unit.deref($*niecza_outer_ref);
        return $sub.unit.create_syml($sub);
    }

    if $setting eq 'NULL' {
        my $id = "MY:file<NULL.pad>:line(1):pos(0)";
        my $core = Stash.new('!id' => [$id], '!file' => 'NULL.pad',
            '!line' => 1);
        return Stash.new('CORE' => $core, 'MY:file<NULL.pad>' => $core,
            'SETTING' => $core, $id => $core);
    }

    $*module_loader.($setting).create_syml;
}

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

    my $*SETTINGNAME = $.lang;
    my $*SAFEMODE    = $.safemode;
    my $*UNITNAME    = $unitname;
    my $*modtime     = $modtime;

    if $unitname eq 'CORE' {
        $*SETTINGNAME = 'NULL';
        $*SAFEMODE = False;
    } elsif $unitname ne 'MAIN' {
        $*SETTINGNAME = 'CORE';
    }

    # XXX temp() or should be contextuals
    my @save_herestub = @STD::herestub_queue;
    my $save_all = $STD::ALL;

    $STD::ALL = {};
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
    my $*SETTING; my $*CORE; my $*GLOBAL; my $*UNIT; my $*YOU_WERE_HERE;
    my $*CCSTATE; my $*BORG; my %*RX; my $*XACT; my $*VAR; my $*IN_REDUCE;

    my $*unit = ::Metamodel::Unit.new(name => $unitname,
        ns => ::Metamodel::Namespace.new, :$filename, :$modtime);

    if $*niecza_outer_ref {
        $*unit.setting_ref = $*niecza_outer_ref;
        $*unit.need_unit($*unit.setting_ref.[0]);
    } elsif $*SETTINGNAME ne 'NULL' {
        $*unit.need_unit($*SETTINGNAME);
        $*unit.setting_ref = $*unit.get_unit($*SETTINGNAME).bottom_ref;
    }
    %*units{$unitname} = $*unit;
    $*unit.tdeps{$unitname} = [$filename, $modtime];

    $*unit.create_stash(['GLOBAL']);
    $*unit.create_stash(['PROCESS']);

    my $ast = NieczaGrammar.parse($source, actions => NieczaActions).ast;

    $STD::ALL = $save_all;
    @STD::herestub_queue = @save_herestub;

    $ast;
}
