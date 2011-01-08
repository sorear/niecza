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

    method lookup_dynvar($name) { Any } # NYI
    method check_old_cclass($text) { } # NYI
    method do_use($module,$args) {
        self.do_need($module);
        self.do_import($module,$args);
        self;
    }

    method do_need($mo) {
        my $module = $mo.Str;
        my $topsym;
        try { $topsym = self.sys_load_modinfo($module); }
        if !$topsym {
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
        if $setting eq 'NULL' {
            my $id = "MY:file<NULL.pad>:line(1):pos(0)";
            my $core = Stash.new('!id' => [$id], '!file' => 'NULL.pad',
                '!line' => 1);
            return Stash.new('CORE' => $core, 'MY:file<NULL.pad>' => $core,
                'SETTING' => $core, $id => $core);
        }

        $*module_loader.($setting).create_syml;
    }
}

augment class Cursor {
    our $RED    = "\e[31m";
    our $GREEN  = "\e[32m";
    our $YELLOW = "\e[33m";
    our $CLEAR  = "\e[37m";
}

has $.lang;
has $.safemode;
has $.unitname;
has $.loader;

method parse(:$filename, :$source) {

    my $*SETTINGNAME = $.lang;
    my $*SAFEMODE    = $.safemode;
    my $*UNITNAME    = $.unitname;
    my $*module_loader = $.loader;

    # XXX temp() or should be contextuals
    my @save_herestub = @STD::herestub_queue;
    my $save_all = $STD::ALL;

    $STD::ALL = {};
    @STD::herestub_queue = ();

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

    $DEBUG::EXPR = False;
    $STD::DEBUG::EXPR = False;
    $STD::DEBUG::symtab = False;

    my $*LAST_NIBBLE = 0;
    my $*LAST_NIBBLE_START = 0;
    my $*LAST_NIBBLE_MULTILINE = 0;
    my $*LAST_NIBBLE_MULTILINE_START = 0;
    my $*GOAL = "(eof)";
    my $*SETTING; my $*CORE; my $*GLOBAL; my $*UNIT; my $*YOU_WERE_HERE;
    my $*CCSTATE; my $*BORG; my %*RX; my $*XACT; my $*VAR; my $*IN_REDUCE;

    my $ast = NieczaGrammar.parse($source, actions => NieczaActions).ast;

    $STD::ALL = $save_all;
    @STD::herestub_queue = @save_herestub;

    $ast;
}
