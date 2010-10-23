# vim: ft=perl6
use Test;
use MONKEY_TYPING;

package DEBUG { our $EXPR = False }

grammar WithOPP {
    # This is a pretty straightforward LR parser with a few Perl 6 extensions.
    # To use this grammar in your own code, you will need to define a termish
    # and an infix method.  termish will not appear in the resulting AST; it
    # is merely a subprogram used to collect $<term>, @<PRE>, and @<POST>
    # captures.  infix should produce $<O> and $<sym> captures; $<O> is a
    # Positional carrying precedence and associativity info, while $<sym>
    # is used for error reporting.

    method deb(*@bits) { note( &infix:<~>(|@bits) ) }

    # XXX P6
    my $LOOSEST = "a=!";
    my $item_assignment_prec = 'i=';
    my $methodcall_prec = 'y=';
    my %terminator      = (:dba('terminator'), :prec<a=>, :assoc<list>);

    method EXPR ($preclvl?) {
        my $preclim = $preclvl ?? $preclvl.<prec> // $LOOSEST !! $LOOSEST;
        my $*LEFTSIGIL = '';        # XXX P6
        my $*PRECLIM = $preclim;
        my @termstack;
        my @opstack;
        my $termish = 'termish';

        sub _top(@a) { @a[ @a.elems - 1 ] }

        my $state;
        my $here;

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
                    last if $op<O><prec> ne _top(@opstack)<O><prec>;
                    push @chain, pop(@termstack);
                    push @chain, pop(@opstack);
                }
                push @chain, pop(@termstack);
                my $endpos = @chain[0].to;
                @chain = reverse @chain if (+@chain) > 1;
                my $startpos = @chain[0].from;
                my $i = True;
                my @caplist;
                for @chain -> $c {
                    push @caplist, ($i ?? 'term' !! 'op') => $c;
                    $i = !$i;
                }
                push @termstack, Match.synthetic(
                    :captures(@caplist, :_arity<CHAIN>, :chain(@chain)),
                    :method<CHAIN>,
                    :cursor(self),
                    :from($startpos),
                    :to($endpos));
            }
            elsif $assoc eq 'list' {
                self.deb("reducing list") if $DEBUG::EXPR;
                my @list;
                my @delims = $op;
                push @list, pop(@termstack);
                while @opstack {
                    self.deb($sym ~ " vs " ~ _top(@opstack)<sym>) if $DEBUG::EXPR;
                    last if $sym ne _top(@opstack)<sym>;
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
                my $endpos = @list[0].to;
                @list = reverse @list if (+@list) > 1;
                my $startpos = @list[0].from;
                @delims = reverse @delims if (+@delims) > 1;
                my @caps;
                if @list {
                    push @caps, (elem => @list[0]) if @list[0];
                    my $i = 0;
                    while $i < (+@delims)-1 {
                        my $d = @delims[$i];
                        my $l = @list[$i+1];
                        push @caps, (delim => $d);
                        push @caps, (elem => $l) if $l;  # nullterm?
                        $i++;
                    }
                }
                push @termstack, Match.synthetic(
                    :method<LIST>, :cursor(self),
                    :from($startpos), :to($endpos),
                    :captures(@caps, :_arity<LIST>, :delims(@delims),
                        :list(@list), :O($op<O>), :sym($sym)));
            }
            elsif $assoc eq 'unary' {
                self.deb("reducing") if $DEBUG::EXPR;
                self.deb("Termstack size: ", +@termstack) if $DEBUG::EXPR;

                my $arg = pop @termstack;
                if $arg.from < $op.from { # postfix
                    push @termstack, Match.synthetic(
                        :cursor(self), :to($op.to), :from($arg.from),
                        :captures(arg => $arg, op => $op, _arity => 'UNARY'),
                        :method<POSTFIX>);
                }
                elsif $arg.to > $op.to {   # prefix
                    push @termstack, Match.synthetic(
                        :cursor(self), :to($arg.to), :from($op.from),
                        :captures(op => $op, arg => $arg, _arity => 'UNARY'),
                        :method<PREFIX>);
                }
            }
            else {
                self.deb("reducing") if $DEBUG::EXPR;
                self.deb("Termstack size: ", +@termstack) if $DEBUG::EXPR;

                my $right = pop @termstack;
                my $left = pop @termstack;

                push @termstack, Match.synthetic(
                    :to($right.to), :from($left.from), :cursor(self),
                    :captures(:left($left), :infix($op), :right($right),
                        :_arity<BINARY>), :method<INFIX>);

    #           self.deb(_top(@termstack).dump) if $DEBUG::EXPR;
                my $ck;
                if $ck = $op<O><_reducecheck> {
                    _top(@termstack) = $ck(_top(@termstack));
                }
            }
        }

        sub termstate() {
            $here.deb("Looking for a term") if $DEBUG::EXPR;
            $here.deb("Top of opstack is ", _top(@opstack).dump) if $DEBUG::EXPR;
            $*LEFTSIGIL = _top(@opstack)<O><prec> gt $item_assignment_prec
                ?? '@' !! '';     # XXX P6
            my $term =
                ($termish eq 'termish') ?? $here.termish.head !!
                ($termish eq 'nulltermish') ?? $here.nulltermish.head !!
                ($termish eq 'statement') ?? $here.statement.head !!
                ($termish eq 'dottyopish') ?? $here.dottyopish.head !!
                die "weird value of $termish";

            if not $term {
                $here.deb("Didn't find it") if $DEBUG::EXPR;
                $here.panic("Bogus term") if (+@opstack) > 1;
                return 2;
            }
            $here.deb("Found term to {$term.to}") if $DEBUG::EXPR;
            $here = $here.cursor($term.to);
            $termish = 'termish';
            my @PRE = @( $term<PRE> // [] );
            my @POST = reverse @( $term<POST> // [] );

            # interleave prefix and postfix, pretend they're infixish
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

            push @termstack, $term<term>;
            $here.deb("after push: " ~ (+@termstack)) if $DEBUG::EXPR;

            say "methodcall break" if $preclim eq $methodcall_prec; # in interpolation, probably   # XXX P6
            $state = &infixstate;
            return 0;
        }

        # std bug sees infixstate as unused
        sub infixstate() { #OK
            $here.deb("Looking for an infix") if $DEBUG::EXPR;
            return 1 if (@*MEMOS[$here.pos]<endstmt> // 0) == 2;  # XXX P6
            $here = $here.cursor($here.ws.head.to);
            my $infix = $here.infixish.head;
            return 1 unless $infix;

            my $inO = $infix<O>;
            my $inprec = $inO<prec>;
            if not defined $inprec {
                die "No prec given in infix!";
            }

            if $inprec le $preclim {
                if $preclim ne $LOOSEST {
                    my $dba = $*prevlevel.<dba>;
                    my $h = $*HIGHEXPECT;
                    %$h = ();
                    $h.{"an infix operator with precedence tighter than $dba"} = 1;
                }
                return 1;
            }

            $here = $here.cursor($infix.to);
            $here = $here.cursor($here.ws.head.to);

            # substitute precedence for listops
            $inO<prec> = $inO<sub> if $inO<sub>;

            # Does new infix (or terminator) force any reductions?
            while _top(@opstack)<O><prec> gt $inprec {
                reduce;
            }

            # Not much point in reducing the sentinels...
            return 1 if $inprec lt $LOOSEST;

            if $infix<fake> {
                push @opstack, $infix;
                reduce();
                return 0;  # not really an infix, so keep trying
            }

            # Equal precedence, so use associativity to decide.
            if _top(@opstack)<O><prec> eq $inprec {
                my $assoc = 1;
                my $atype = $inO<assoc>;
                if $atype eq 'non'   { $assoc = 0; }
                elsif $atype eq 'left'  { reduce() }   # reduce immediately
                elsif $atype eq 'right' { }            # just shift
                elsif $atype eq 'chain' { }            # just shift
                elsif $atype eq 'unary' { }            # just shift
                elsif $atype eq 'list'  {
                    $assoc = 0 unless $infix<sym> eq _top(@opstack)<sym>;
                }
                else { $here.panic('Unknown associativity "' ~ $_ ~ '" for "' ~ $infix<sym> ~ '"') }
                if not $assoc {
                   $here.sorry('"' ~ _top(@opstack)<sym> ~ '" and "' ~ $infix.Str ~ '" are non-associative and require parens');
                }
            }

            $termish = $inO<nextterm> if $inO<nextterm>;
            push @opstack, $infix;              # The Shift
            $state = &termstate;
            return 0;
        }

        push @opstack, { 'O' => %terminator, 'sym' => '' };         # (just a sentinel value)
        self.deb(@opstack.dump) if $DEBUG::EXPR;

        $here = self;
        self.deb("In EXPR, at {$here.pos}") if $DEBUG::EXPR;

        my $stop = 0;
        $state = &termstate;
        until $stop {
            $here.deb("At {$here.pos}, {@opstack.dump}; {@termstack.dump}") if $DEBUG::EXPR;
            $stop = $state();
        }
        $here.deb("Stop code $stop") if $DEBUG::EXPR;
        return () if $stop == 2;
        reduce() while +@opstack > 1;
        $here.deb("After final reduction, ", @termstack.dump, @opstack.dump) if $DEBUG::EXPR;

        if @termstack {
            +@termstack == 1 or $here.panic("Internal operator parser error, termstack == " ~ (+@termstack));
            return @( Match.synthetic(:to($here.pos), :from(self.pos),
                    :cursor(self), :method<EXPR>,
                    :captures( root => @termstack[0] )), );
        }
        return ();
    }
}

grammar EXPRTest is WithOPP {
    token infixish {
        $<assoc>=( < chain list left right non > ) ','
        $<prio>=( \w+ )
        $<sym>={ $<prio> }
        $<O>={ { assoc => $<assoc>.Str, prec => $<prio>.Str } }
    }
    token unary {
        ':' $<assoc>=( < left right non > ) ','
        $<prio>=( \w+ ) ':'
        $<sym>={ $<prio> }
        $<O>={ { uassoc => $<assoc>.Str, prec => $<prio>.Str } }
    }
    token term { '@' }
    token termish { <PRE=.unary>* <term> <POST=.unary>* }

    rule TOP { <EXPR> }
}

#say EXPRTest.parse(' @ ').dump;
#say EXPRTest.parse(' @ left,b @ left,b @ ').dump;
say EXPRTest.parse(' @ left,b @ left,c @ ').dump;
say EXPRTest.parse(' @ left,c @ right,b @ ').dump;
#say EXPRTest.parse(' @ chain,c @ chain,c @ ').dump;
#say EXPRTest.parse(' @ list,c @ list,c @ ').dump;
#say EXPRTest.parse(' :non,b:@:non,c: ').dump;
#say EXPRTest.parse(' :left,b:@:left,b: ').dump;
