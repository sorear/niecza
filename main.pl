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
##    method accepts_capture($cap) { Q:CgOp { (code_accepts_capture (@ {self}) (@ {$cap})) } }
}

for &die.candidates { say .signature.perl }
for &splice.candidates { say .signature.perl }

#`〈
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
〉
