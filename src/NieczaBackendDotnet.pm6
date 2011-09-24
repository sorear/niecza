class NieczaBackendDotnet;

use NAMOutput;
use JSYNC;
use NieczaPassSimplifier;
use Metamodel;

has $.safemode = False;
has $.obj_dir;
has $.run_args = [];

# The purpose of the backend is twofold.  It must be able to accept
# and process units; and it must be able to retrieve processed units
# at a later time.

# Return Metamodel::Unit, undefined if unit not available.  The caller
# will check tdeps, load them if needed, and maybe even discard the
# returned unit.
method get_unit($name) {
    my $file = ($name.split('::').join('.') ~ ".nam").IO\
        .relative($.obj_dir);
    $file.e ?? NAMOutput.load($file.slurp) !! ::Metamodel::Unit;
}

# Save a unit.  If $main is true, it is being considered as a main
# module; if $run, it should be auto-run.  Main modules do not need
# to be retrievable.
method save_unit($name, $unit) {
    my $file = ($name.split('::').join('.') ~ ".nam").IO\
        .relative($.obj_dir);
    $file.spew(NAMOutput.run($unit));
}

sub upcalled(@strings) {
    given @strings[0] {
        when "eval" {
            my $*IN_EVAL = True;
            # XXX NieczaException is eaten by boundary
            try {
                $*compiler.compile_string(@strings[1], True, :evalmode,
                    :outer([@strings[2], +@strings[3]]));
                return "";
            }
            return $!;
        }
        say "upcall: @strings.join('|')";
        "ERROR";
    }
}

method new(*%_) {
    Q:CgOp { (rnull (rawscall Niecza.Downcaller,CompilerBlob.InitSlave {&upcalled})) };
    nextsame;
}

sub downcall(*@args) {
    Q:CgOp { (rawscall Niecza.Downcaller,CompilerBlob.DownCall {@args}) }
}

method accept($unitname, $unit, :$main, :$run, :$evalmode, :$repl) { #OK not used
    downcall("safemode") if $.safemode;
    if $run {
        downcall("setnames", $*PROGRAM_NAME // '???',
            $*orig_file // '(eval)') unless $repl;
        downcall("run_unit", $unit.peer, ?$evalmode, @$!run_args);
        if $repl {
            downcall("replrun");
        }
        $*repl_outer = $unit.get_mainline if $repl;
        return;
    }
    downcall("save_unit", $unit.peer, ?$main);
    $*repl_outer = $unit.get_mainline if $repl;
}

method post_save($name, :$main) {
    my $fname = $name.split('::').join('.');
    downcall("post_save",
        $.obj_dir, $fname ~ ".nam", $fname ~ ($main ?? ".exe" !! ".dll"),
        $main ?? "1" !! "0");
}

class Unit { ... }
class StaticSub { ... }
class Type { ... }

class StaticSub {
    has $.peer;
    method WRAP($p) { $p && self.new(peer => $p) }
    method lex_names() { downcall("lex_names", $!peer) }
    method lookup_lex($name, $file?, $line?) {
        my @ret = downcall("sub_lookup_lex", $!peer, $name, $file, $line//0);
        return unless @ret;
        @ret[4] = Type.new(peer => @ret[4]) if @ret[0] eq 'package';
        @ret[5] = Type.new(peer => @ret[5]) if @ret[0] eq 'simple' && @ret[5];
        @ret[4] = StaticSub.new(peer => @ret[4]) if @ret[0] eq 'sub';
        @ret;
    }
    method set_outervar($v) { downcall("sub_set_outervar", $!peer, ~$v) }
    method set_class($n)    { downcall("sub_set_class", $!peer, ~$n) }
    method set_name($v)     { downcall("sub_set_name", $!peer, ~$v) }
    method set_methodof($m) { downcall("sub_set_methodof", $!peer, $m && $m.peer) }
    method set_in_class($m) { downcall("sub_set_in_class", $!peer, $m && $m.peer) }
    method set_body_of($m)  { downcall("sub_set_body_of", $!peer, $m && $m.peer) }

    method name()     { downcall("sub_name", $!peer) }
    method outer()    { StaticSub.WRAP(downcall("sub_outer", $!peer)) }
    method class()    { downcall("sub_class", $!peer) }
    method run_once() { downcall("sub_run_once", $!peer) }
    method cur_pkg()  { Type.WRAP(downcall("sub_cur_pkg", $!peer)) }
    method in_class() { Type.WRAP(downcall("sub_in_class", $!peer)) }
    method body_of()  { Type.WRAP(downcall("sub_body_of", $!peer)) }
    method outervar() { downcall("sub_outervar", $!peer) }
    method methodof() { Type.WRAP(downcall("sub_methodof", $!peer)) }

    method unused_lexicals() { downcall("unused_lexicals", $!peer) }
    method parameterize_topic() { downcall("sub_parameterize_topic", $!peer) }
    method unit() { Unit.new(peer => downcall("sub_get_unit", $!peer)) }
    method to_unit() { StaticSub.new(peer => downcall("sub_to_unit", $!peer)) }
    method is($o) { downcall("equal_handles", $!peer, $o.peer) }
    method is_routine() { downcall("sub_is_routine", $!peer) }
    method has_lexical($name) { downcall("sub_has_lexical", $!peer, $name) }
    method lexical_used($name) { downcall("sub_lexical_used", $!peer, $name) }
    method set_signature($sig) {
        my @args;
        if !$sig {
            downcall("sub_no_signature", $!peer);
            return;
        }
        for @( $sig.params ) {
            my $flags = 0;
            # keep synced with SIG_F_ constants
            if .rwtrans       { $flags +|= 8 }
            if .rw            { $flags +|= 2 }

            if .hash || .list { $flags +|= 16 }
            if .defouter      { $flags +|= 4096 }
            if .invocant      { $flags +|= 8192 }
            if .multi_ignored { $flags +|= 16384 }
            if .is_copy       { $flags +|= 32768 }
            if .list          { $flags +|= 65536 }
            if .hash          { $flags +|= 131072 }
            if .tclass        { $flags +|= 1 }
            if .mdefault      { $flags +|= 32 }
            if .optional      { $flags +|= 64 }
            if .positional    { $flags +|= 128 }
            if .slurpy        { $flags +|= (.hash ?? 512 !! 256) }
            if .slurpycap     { $flags +|= 1024 }
            if .full_parcel   { $flags +|= 2048 }

            push @args, $flags, .name, .slot, @( .names ), Str,
                .mdefault, .tclass;
        }
        downcall("set_signature", $!peer, @args);
    }

    # TODO: prevent foo; sub foo { } from warning undefined
    # needs a %*MYSTERY check when evaluating unused variables
    method _addlex_result(*@args) {
        given @args[0] {
            when 'collision' {
                my ($ , $slot, $nf,$nl,$of,$ol) = @args;
                my $l = Metamodel.locstr($of, $ol, $nf, $nl);
                if $slot ~~ /^\w/ {
                    die "Illegal redeclaration of symbol '$slot'$l";
                } elsif $slot ~~ /^\&/ {
                    die "Illegal redeclaration of routine '$slot.substr(1)'$l";
                } else {
                    $*worry.("Useless redeclaration of variable $slot$l");
                }
            }
            when 'already-bound' {
                my ($ , $slot, $count, $line, $nf,$nl,$of,$ol) = @args;
                my $truename = $slot;
                $truename ~~ s/<?before \w>/OUTER::/ for ^$count;
                die "Lexical symbol '$slot' is already bound to an outer symbol{Metamodel.locstr($of, $ol, $nf, $nl)};\n  the implicit outer binding at line $line must be rewritten as $truename\n  before you can unambiguously declare a new '$slot' in this scope";
            }
        }
    }

    method add_my_name($name, :$file, :$line, :$pos, :$noinit, :$defouter,
            :$roinit, :$list, :$hash, :$typeconstraint) {
        self._addlex_result(downcall("add_my_name", $!peer, ~$name,
            ~($file//''), +($line//0), +($pos// -1),
            $typeconstraint && $typeconstraint.peer,
            ($noinit ?? 1 !! 0) + ($roinit ?? 2 !! 0) + ($defouter ?? 4 !! 0) +
            ($list ?? 8 !! 0) + ($hash ?? 16 !! 0)));
    }
    method add_hint($name, :$file, :$line, :$pos) {
        self._addlex_result(downcall("add_hint", $!peer, ~$name,
            ~($file//''), +($line//0), +($pos// -1)));
    }
    method add_label($name, :$file, :$line, :$pos) {
        self._addlex_result(downcall("add_label", $!peer, ~$name,
            ~($file//''), +($line//0), +($pos// -1)));
    }
    method add_dispatcher($name, :$file, :$line, :$pos) {
        self._addlex_result(downcall("add_dispatcher", $!peer, ~$name,
            ~($file//''), +($line//0), +($pos// -1)));
    }
    method add_common_name($name, $pkg, $pname, :$file, :$line, :$pos) {
        self._addlex_result(downcall("add_common_name", $!peer, ~$name,
            ~($file//''), +($line//0), +($pos// -1), $pkg.peer, ~$pname));
    }
    method add_state_name($name, $backing, :$file, :$line, :$pos, :$noinit,
            :$defouter, :$roinit, :$list, :$hash, :$typeconstraint) {
        self._addlex_result(downcall("add_state_name", $!peer, ~$name,
            ~($file//''), +($line//0), +($pos// -1),
            $typeconstraint && $typeconstraint.peer,
            ($noinit ?? 1 !! 0) + ($roinit ?? 2 !! 0) + ($defouter ?? 4 !! 0) +
            ($list ?? 8 !! 0) + ($hash ?? 16 !! 0),
            $backing));
    }
    method add_my_stash($name, $pkg, :$file, :$line, :$pos) {
        self._addlex_result(downcall("add_my_stash", $!peer, ~$name,
            ~($file//''), +($line//0), +($pos// -1), $pkg.peer));
    }
    method add_my_sub($name, $body, :$file, :$line, :$pos) {
        self._addlex_result(downcall("add_my_sub", $!peer, ~$name,
            ~($file//''), +($line//0), +($pos// -1), $body.peer));
    }

    method finish($ops) { 
        $ops := NieczaPassSimplifier.invoke_incr(self, $ops);
        downcall("sub_finish", $!peer, to-json($ops.cgop(self)));
    }
}

class Type {
    has $.peer;
    method WRAP($p) { $p && self.new(peer => $p) }
    method is_package() { downcall("type_is_package", $!peer) }
    method closed() { downcall("type_closed", $!peer) }
}

class Unit {
    has $.peer;
    method WRAP($p) { $p && self.new(peer => $p) }
    method name() { downcall("unit_get_name", $!peer) }
    method stubbed_stashes() {
        downcall("unit_stubbed_stashes", $!peer).map({ $_ ~~ Int ?? $_ !! Type.new(peer => $_)})
    }
    method stub_stash($pos, $type) { downcall("unit_stub_stash", $pos, $type.peer) }
    method set_current() { downcall("set_current_unit", $!peer) }
    method set_mainline($sub) { downcall("set_mainline", $sub.peer) }
    method abs_pkg(*@names, :$auto) {
        Type.new(peer => downcall("rel_pkg", ?$auto, Any, @names))
    }
    method rel_pkg($pkg, *@names, :$auto) {
        Type.new(peer => downcall("rel_pkg", ?$auto, $pkg.peer, @names))
    }
    method get($pkg, $name) {
        my ($p,$k) = downcall("get_name", $pkg, $name);
        $k ?? Type.new(peer => $p) !! StaticSub.new(peer => $p)
    }

    method create_sub(:$name, :$class, :$outer, :$cur_pkg, :$in_class,
            :$run_once) {
        StaticSub.new(peer => downcall("create_sub", ~($name // 'ANON'),
            $outer && $outer.peer, ~($class // 'Sub'), $cur_pkg.peer,
            $in_class && $in_class.peer, ?$run_once))
    }
}

method create_unit($name, $filename, $modtime, $main, $run) {
    Unit.new(peer => downcall("new_unit", ~$name, ~$filename, ~$modtime,
            ~$!obj_dir, ?$main, ?$run));
}
