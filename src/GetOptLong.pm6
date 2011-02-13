# GNU getopt_long compatible options parser

module GetOptLong;

sub GetOptions(*@pairs, :$permute = True, :onerror($onerror_), :onarg($onarg_)) is export {
    my @nonopt;
    my $onerror = $onerror_ // sub ($message) {
        note $message;
        exit 1;
    };
    my $onarg = $onarg_ // sub ($arg) {
        push @nonopt, $arg;
        if !$permute {
            push @nonopt, @*ARGS;
            @*ARGS = ();
        }
    };
    my @unpk;
    sub pick_long_option($st) {
        my @cand = grep { chars($_[0]) > 1 &&
            substr($_[0],0,chars($st)) eq $st }, @unpk;
        $onerror.("Ambiguous long option --$st; could be any of {map *[0], @cand}") if @cand > 1;
        $onerror.("No match for long option --$st") if !@cand;
        @cand[0];
    }
    sub pick_short_option($st) {
        my @cand = grep { $_[0] eq $st }, @unpk;
        $onerror.("No match for short option -$st") if !@cand;
        @cand[0];
    }
    for @pairs -> $p {
        my $key = $p.key;
        my $type = '';
        if $key ~~ /<[:=]>s$/ {
            $type = ~$/;
            $key = substr($key, 0, $/.from);
        }
        for $key.split('|') {
            push @unpk, [ $_, $type, $p.value ];
        }
    }

    while @*ARGS {
        my $opt = shift @*ARGS;
        if $opt eq '--' {
            $onarg.(shift @*ARGS) while @*ARGS;
            last;
        }
        elsif substr($opt, 0, 2) eq '--' {
            if $opt ~~ /'='/ {
                my $obl = pick_long_option(substr($opt, 2, $/.from - 2));
                $onerror.("Long option --$obl[0] does not accept an argument")
                    if $obl[1] eq '';
                $obl[2].(substr($opt, $/.to));
            } else {
                my $obl = pick_long_option(substr($opt, 2));
                if $obl[1] eq '=s' {
                    $onerror.("Argument required for long option --$obl[0]")
                        unless @*ARGS;
                    $obl[2].(shift @*ARGS);
                } else {
                    $obl[2].(Str);
                }
            }
        }
        elsif chars($opt) > 1 && substr($opt, 0, 1) eq '-' {
            $opt = substr($opt, 1);
            while $opt ne '' {
                my $obl = pick_short_option(substr($opt, 0, 1));
                $opt = substr($opt, 1);
                if $obl[1] eq '' || $obl[1] eq ':s' && $opt eq '' {
                    $obl[2].(Str);
                }
                elsif $opt ne '' {
                    $obl[2].($opt);
                    $opt = '';
                }
                else {
                    $onerror.("Argument required for short option -$obl[0]")
                        unless @*ARGS;
                    $obl[2].(shift @*ARGS);
                }
            }
        }
        else {
            $onarg.($opt);
        }
    }

    @*ARGS = @nonopt;
}
