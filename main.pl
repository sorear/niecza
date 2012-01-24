# the following was adapted from Geoffrey Broadwell's code in Rakudo nom
# * $?USAGE
#   * Create $?USAGE at compile time
#   * Make $?USAGE available globally
# * Command-line parsing
#   * Allow both = and space before argument of double-dash args
#   * Comma-separated list values
#   * Allow exact Perl 6 forms, quoted away from shell
# * Fix remaining XXXX

# TODO (sorear): add True, False to val(); (eval) becomes -e; change param
# names to be name-of-var or ""; Str.perl escaping

use MONKEY_TYPING;
augment class Code {
    method candidates() { Q:CgOp { (code_candidates (@ {self})) } }
    method signature()  { Q:CgOp { (code_signature (@ {self})) } }
    method candidates_matching(|$cap) { grep *.accepts_capture($cap), self.candidates }
    method accepts_capture($cap) { Q:CgOp { (code_accepts_capture (@ {self}) (@ {$cap})) } }
    method name() { Q:CgOp { (code_name (@ {self})) } }
}
augment class Routine {
    method perl() {
        self // nextsame;
        my $perl = self.^name.lc();
        if self.name() -> $n {
            $perl ~= " $n";
        }
        $perl ~= self.signature().perl.substr(1);
        $perl ~= ' { ... }';
        $perl
    }
}
augment class Signature {
    method params() { Q:CgOp { (sig_params (@ {self})) } }
    method arity() { Q:CgOp { (box Int (sig_arity (@ {self}))) } }
    method count() { Q:CgOp { (box Int (sig_count (@ {self}))) } }
    # XXX TODO: Parameter separators.
    method perl() {
        self // nextsame;
        ':(' ~ join(', ', self.params».perl) ~ ')';
    }
}
augment class Parameter {
    # Value processing
    our constant HASTYPE    = 1;
    our constant MULTI_IGNORED = 16384;
    our constant ANY_DEF    =  0x40000;
    our constant UNDEF_ONLY =  0x80000;
    our constant DEF_ONLY   =  0xC0000;
    our constant TYPE_ONLY  = 0x100000;
    our constant DEF_MASK   = 0x1C0000;

    # Value binding
    our constant READWRITE  = 2;
    our constant RWTRANS    = 8;
    our constant INVOCANT   = 8192;
    our constant IS_COPY    = 32768;
    our constant IS_LIST    = 65536;
    our constant IS_HASH    = 131072;
    our constant CALLABLE   = 0x20_0000;

    # Value source
    our constant HASDEFAULT = 32;
    our constant OPTIONAL   = 64;
    our constant DEFOUTER   = 4096;
    our constant POSITIONAL = 128;
    our constant SLURPY_POS = 256;
    our constant SLURPY_NAM = 512;
    our constant SLURPY_CAP = 1024;
    our constant SLURPY_PCL = 2048;

    method named() { !!! }
    method named_names() { !!! }
    method type() { !!! }
    method optional() { !!! }
    method positional() { !!! }
    method value_constraint_list() { !!! }
    method name() { !!! }
    method slurpy() { !!! }

    # no constraint_list!  niecza's SubInfo constraints don't reflect well :|
    method parcel() { !!! }
    method capture() { !!! }
    method rw() { !!! }
    method copy() { !!! }
    method readonly() { !!! }
    method invocant() { !!! }
    method default() { !!! }

    # XXX TODO: A few more bits :-)
    multi method perl(Parameter:D:) {
        my $perl = '';
        my $flags = self.flags;
        my $type = self.type.^name;
        if $flags +& IS_LIST {
            # XXX Need inner type
        }
        elsif $flags +& IS_HASH {
            # XXX Need inner type
        }
        else {
            $perl = $type;
            if $flags +& DEF_ONLY {
                $perl ~= ':D';
            } elsif $flags +& UNDEF_ONLY {
                $perl ~= ':U';
            } elsif $flags +& TYPE_ONLY {
                $perl ~= ':T';
            }
            $perl ~= ' ';
        }
        if self.name -> $name {
            if $flags +& SLURPY_CAP {
                $perl ~= '|' ~ $name;
            } elsif $flags +& RWTRANS {
                $perl ~= '\\' ~ $name;
            } else {
                my $default = self.default();
                if self.named_names -> @names {
                    my $short = $name.substr(1);
                    $name = ':' ~ $name if $short eq any @names;
                    for @names {
                        next if $_ eq $short;
                        $name = ':' ~ $_ ~ '(' ~ $name ~ ')';
                    }
                    $name ~= '!' unless self.optional;
                } elsif self.optional && !$default {
                    $name ~= '?';
                } elsif self.slurpy {
                    $name = '*' ~ $name;
                }
                $perl ~= $name;
                if $!flags +& READWRITE {
                    $perl ~= ' is rw';
                } elsif $!flags +& IS_COPY {
                    $perl ~= ' is copy';
                }
                $perl ~= ' = { ... }' if $default;
                if self.sub_signature -> $sub {
                    $perl ~= ' ' ~ $sub.perl;
                }
            }
        }
        $perl
    }
}
augment class ClassHOW {
    method name($) { Q:CgOp { (box Str (obj_typename (stab_what (unbox stable (@ {self}))))) } }
}

my sub MAIN_HELPER() {
    # Do we have a MAIN at all?
    my $m = CALLER::<&MAIN>;
    return unless $m;

    # Convert raw command line args into positional and named args for MAIN
    my sub process-cmd-args (@args is copy) {
        my (@positional-arguments, %named-arguments);
        while (@args) {
            my $passed-value = @args.shift;
            if $passed-value ~~ /^ ( '--' | '-' | ':' ) ('/'?) (<-[0..9\.]> .*) $/ {
                my ($switch, $negate, $arg) = (~$0, ?((~$1).chars), ~$2);

                if $arg.index('=').defined  {
                    my ($name, $value) = $arg.split('=', 2);
                    $value = val($value);
                    $value = $value but False if $negate;
                    %named-arguments.push: $name => $value;
                } else {
                    %named-arguments.push: $arg => !$negate;
                }
            } else {
                @args.unshift($passed-value) unless $passed-value eq '--';
                @positional-arguments.push: @args.map: &val;
                last;
            }
        }

        return @positional-arguments, %named-arguments;
    }

    # Generate $?USAGE string (default usage info for MAIN)
    my sub gen-usage () {
        my @help-msgs;
        my $prog-name = $*PROGRAM_NAME eq '-e' ?? "-e '...'" !! $*PROGRAM_NAME;
        for $m.candidates -> $sub {
            my (@required-named, @optional-named, @positional);
            for $sub.signature.params -> $param {
                my $argument;
                if $param.named {
                    my @names  = $param.named_names.reverse;
                    $argument  = @names.map({($^n.chars == 1 ?? '-' !! '--') ~ $^n}).join('|');
                    $argument ~= "=<{$param.type.^name}>" unless $param.type === Bool;
                    if $param.optional {
                        @optional-named.push("[$argument]");
                    }
                    else {
                        @required-named.push($argument);
                    }
                }
                else {
                    my $constraints  = $param.value_constraint_list;
                    $argument = $constraints  ??       $constraints                !!
                                $param.name   ?? '<' ~ $param.name.substr(1) ~ '>' !!
                                                 '<' ~ $param.type.^name     ~ '>' ;

                    $argument = "[$argument ...]" if $param.slurpy;
                    $argument = "[$argument]"     if $param.optional;
                    @positional.push($argument);
                }
            }
            my $msg = join(' ', $prog-name, @required-named, @optional-named, @positional);
            @help-msgs.push($msg);
        }
        my $usage = "Usage:\n" ~ @help-msgs.map('  ' ~ *).join("\n");
        return $usage;
    }

    # Process command line arguments
    my ($p, $n) = process-cmd-args(@*ARGS).lol;

    # Generate default $?USAGE message
    my $USAGE = gen-usage();

    # If dispatch to MAIN is possible, do so
    if $m.candidates_matching(|@($p), |%($n)).elems {
        return $m(|@($p), |%($n));
    }

    # We could not find the correct MAIN to dispatch to!
    # Let's try to run a user defined USAGE sub
    my $h = CALLER::<&USAGE>;
    return $h() if $h;

    # We could not find a user defined USAGE sub!
    # Let's display the default USAGE message
    if ($n<help>) {
        $*OUT.say($USAGE);
        exit 1;
    }
    else {
        $*ERR.say($USAGE);
        exit 2;
    }
}
