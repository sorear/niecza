use strict;
use warnings;
use 5.010;

{
    package CodeGen;
    use Moose;

    our $file = '';
    our $line = 0;

    # Beta will do this using reflection
    my %typedata = (
        IP6 =>
            { Isa          => [m => 'Boolean'],
              Does         => [m => 'Boolean'],
              GetTypeName  => [m => 'String'],
              GetTypeObject=> [m => 'IP6'],
              GetMO        => [m => 'DynMetaObject'],
              IsDefined    => [m => 'Boolean'],
              HOW          => [c => 'IP6'] },
        DynObject =>
            { klass        => [f => 'DynMetaObject'],
              GetSlot      => [m => 'object'],
              SetSlot      => [m => 'Void'],
              slots        => [f => 'Dictionary<string,Object>'] },

        DynMetaObject =>
            { Complete     => [m => 'Void'],
              HasMRO       => [m => 'Boolean'],
              AddMultiRegex=> [m => 'Void'],
              AddMethod    => [m => 'Void'],
              AddSuperclass=> [m => 'Void'],
              AddAttribute => [m => 'Void'],
              typeObject   => [f => 'IP6'],
              how          => [f => 'IP6'],
              name         => [f => 'String'] },

        'Double' =>
            { ToString     => [m => 'String'] },
        'Variable' =>
            { islist       => [f => 'Boolean'],
              Fetch        => [m => 'IP6' ] },
        'BValue' =>
            { v            => [f => 'Variable' ] },
        'VivClosure' =>
            { v            => [f => 'IP6'] },
        'CLRImportObject' =>
            { val          => [f => 'Object'] },
        'String' =>
            { Length       => [f => 'Int32'],
              Substring    => [m => 'String'] },
        'System.Text.StringBuilder' =>
            { Append       => [m => 'Void'],
              ToString     => [m => 'String'] },
        'Frame' =>
            { pos          => [f => 'Variable[]'],
              rx           => [f => 'RxFrame'],
              caller       => [f => 'Frame'],
              outer        => [f => 'Frame'],
              proto        => [f => 'Frame'],
              lex          => [f => 'Dictionary<string,object>'],
              ExecutingLine=> [m => 'Int32'],
              ExecutingFile=> [m => 'String'],
              ExtractNamed => [m => 'Variable'],
              LexicalFind  => [m => 'Variable'] },
        'RxFrame' =>
            { Exact        => [m => 'Boolean'],
              Exact1       => [m => 'Boolean'],
              GetClass     => [m => 'DynMetaObject'],
              IncQuant     => [m => 'Void'],
              GetQuant     => [m => 'Int32'],
              OpenQuant    => [m => 'Void'],
              CloseQuant   => [m => 'Int32'],
              CommitGroup  => [m => 'Void'],
              CommitRule   => [m => 'Void'],
              CommitAll    => [m => 'Void'],
              PushCutGroup => [m => 'Void'],
              PopCutGroup  => [m => 'Void'],
              GetCursorList=> [m => 'Variable'],
              SetCursorList=> [m => 'Void'],
              LTMPushAlts  => [m => 'Void'],
              PushCapture  => [m => 'Void'],
              MakeCursor   => [m => 'Cursor'],
              MakeMatch    => [m => 'Cursor'],
              SetPos       => [m => 'Void'],
              Backtrack    => [c => 'Void'],
              End          => [c => 'Void'] },
        'Cursor' =>
            { At           => [m => 'Cursor'],
              pos          => [f => 'Int32'],
              from         => [f => 'Int32'],
              GetKey       => [m => 'Variable'],
              backing      => [f => 'String'],
              SimpleWS     => [m => 'Variable'] },
        'Lexer' =>
            { Run          => [m => 'Int32[]'] },
        'VarDeque' =>
            { Push         => [m => 'Void'],
              Unshift      => [m => 'Void'],
              UnshiftN     => [m => 'Void'],
              Pop          => [m => 'Variable'],
              Shift        => [m => 'Variable'],
              Count        => [m => 'Int32'] },

        'System.IO.File.ReadAllText' => [m => 'System.String'],

        'Lexer.RunProtoregex'  => [m => 'IP6[]'],
        'Lexer.GetLexer'       => [m => 'Lexer'],
        'Kernel.SearchForHandler' => [c => 'Variable'],
        'Kernel.Die'           => [c => 'Void'],
        'Kernel.CoTake'        => [c => 'Variable'],
        'Kernel.Take'          => [c => 'Variable'],
        'Kernel.GatherHelper'  => [c => 'Frame'],
        'Kernel.ContextHelper' => [m => 'Variable'],
        'Kernel.StrP'          => [f => 'IP6'],
        'Kernel.CallFrameMO'   => [f => 'DynMetaObject'],
        'Kernel.Process'       => [f => 'Variable'],
        'Kernel.Global'        => [f => 'Variable'],
        'Kernel.PackageLookup' => [m => 'BValue'],
        'Kernel.SlurpyHelper'  => [m => 'VarDeque'],
        'Kernel.NewBoundVar'   => [c => 'Variable'],
        'Kernel.Assign'        => [c => 'Void'],
        'Kernel.DefaultNew'    => [m => 'Variable'],
        'Kernel.NewROScalar'   => [m => 'Variable'],
        'Kernel.NewRWScalar'   => [m => 'Variable'],
        'Kernel.NewRWListVar'  => [m => 'Variable'],
        'Console.WriteLine'    => [m => 'Void'],
        'Console.Error.WriteLine'    => [m => 'Void'],
        'System.Environment.Exit'     => [m => 'Void'],
        'String.Concat'        => [m => 'String'],
        'Kernel.AnyP'          => [f => 'IP6'],
        'Kernel.ArrayP'        => [f => 'IP6'],
        'Kernel.HashP'         => [f => 'IP6'],
        'Kernel.SubMO'         => [f => 'DynMetaObject'],
        'Kernel.ScalarMO'      => [f => 'DynMetaObject'],
        'Kernel.MainlineContinuation' => [f => 'DynBlockDelegate'],
        'Kernel.MakeSub'       => [m => 'IP6'],
        'Kernel.BoxAny'        => [m => 'Variable'],
        'Kernel.UnboxAny'      => [m => 'object'],
    );

    sub know_sfield {
        my ($class, $n, $ty) = @_;
        $typedata{$n} = [ f => $ty ];
    }

    sub _generic_infer {
        /Dictionary<(.*),(.*)>/ && return {
            ContainsKey         => [ m => 'Boolean' ],
        };
        /List<(.*)>/ && return {
            Add                 => [m => 'Void'],
            Insert              => [m => 'Void'],
            RemoveAt            => [m => 'Void'],
            Count               => [f => 'Int32'],
        };
        /(.*)\[\]/ && return {
            Length              => [f => 'Int32'],
        };
    }

    sub _striptype {
        my ($self, $name) = @_;
        $name =~ s/:.*//;
        $name;
    }

    sub _typedata {
        my ($self, $types, @path) = @_;

        if (!defined ($path[-1])) {
            Carp::confess("Undefined path in _typedata");
        }

        if ($path[-1] =~ /(.*):(.),(.*)/) {
            return $1, $2, $3;
        }

        for ($path[0]) { $typedata{$_} //= _generic_infer; }

        my $cursor = \%typedata;
        for (@path) { $cursor = $cursor->{$_}; }
        if (!defined $cursor) {
            die "No type data for " . join(":", @path);
        }
        if (index($types, $cursor->[0]) < 0) {
            die "Expected [$types] for " . join(":", @path) . " but got " .
                $cursor->[0];
        }
        if (length($types) > 1) {
            return $path[-1], @$cursor;
        } else {
            return $path[-1], $types, $cursor->[1];
        }
    }

    has ops       => (is => 'ro', required => 1);
    has csname    => (isa => 'Str', is => 'ro');
    has minlets   => (isa => 'Int', is => 'ro', default => 0);
    has usednamed => (isa => 'Bool', is => 'ro', default => 0);

    has depth     => (isa => 'Int', is => 'rw', default => 0);
    has maxdepth  => (isa => 'Int', is => 'rw', default => 0);
    has savedepth => (isa => 'Int', is => 'rw', default => 0);
    has numlabels => (isa => 'Int', is => 'rw', default => 1);
    has numips    => (isa => 'Int', is => 'rw', default => 1);
    has lineinfo  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has stacktype => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has stackterm => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has resulttype =>(isa => 'Maybe[Str]', is => 'rw', default => '');
    has labelname => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has buffer    => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has unreach   => (isa => 'Bool', is => 'rw', default => 0);
    has outcap    => (isa => 'Bool', is => 'rw', default => 0);

    has letstack  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has lettypes  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has numlets   => (isa => 'Int', is => 'rw', default => 0);
    has consttab  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has ehspans   => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has ehlabels  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });

    has savedstks => (isa => 'HashRef', is => 'ro', default => sub { +{} });

    # These are the sub-primitives.  Their very inteface exposes volatile
    # details of the codegen.

    sub qm {
        my $out = $_[0];
        $out =~ s/"/""/g;
        "@\"$out\"";
    }

    sub _savestackstate {
        my ($self, $lbl) = @_;
        my %save;
        if (@{ $self->stacktype }) {
            print for @{ $self->buffer };
            print(YAML::XS::Dump($self->stacktype));
            print(YAML::XS::Dump($self->stackterm));
            Carp::confess "Invalid operation of CPS converter";
        }
        $save{lettypes} = [ @{ $self->lettypes } ];
        $save{letstack} = [ @{ $self->letstack } ];
        $self->savedstks->{$lbl} = \%save;
    }

    sub _restorestackstate {
        my ($self, $lbl) = @_;
        my $save = $self->savedstks->{$lbl};
        @{ $self->letstack } = @{ $save->{letstack} };
        @{ $self->lettypes } = @{ $save->{lettypes} };
    }

    sub _emit {
        my ($self, $line) = @_;
        push @{ $self->buffer }, "    $line;\n";
    }

    sub _push {
        my ($self, $ty, $expr) = @_;
        Carp::confess('Untyped push') unless defined $ty;
        push @{ $self->stacktype }, $ty;
        push @{ $self->stackterm }, $expr;
    }

    sub _popn {
        my ($self, $nr) = @_;
        if ($nr > @{ $self->stacktype }) {
            print for @{ $self->buffer };
            Carp::confess "stack undeflow";
        }
        (splice @{ $self->stackterm }, -$nr, $nr),
            (splice @{ $self->stacktype }, -$nr, $nr);
    }

    sub _cpscall {
        my ($self, $rt, $expr) = @_;
        die "Invalid operation of CPS converter" if $self->depth;
        my $n = $self->ip;
        $self->_emit("th.ip = $n");
        $self->_emit("return $expr");
        $self->lineinfo->[$n] = $line;
        push @{ $self->buffer }, "case $n:\n";
        die "Broken call $expr" if !defined($rt);
        $self->resulttype($rt);
    }

    # These functions are usable from user code, but still depend on volatiles.

    sub result {
        my ($self) = @_;
        $self->_push("object", "th.resultSlot");
        $self->cast($self->resulttype);
    }

    sub set_result {
        my ($self) = @_;
        if (!@{ $self->stacktype }) {
            print for @{ $self->buffer };
        }
        $self->resulttype($self->stacktype->[-1]);
        $self->_emit("th.resultSlot = " . ($self->_popn(1))[0]);
    }

    sub _lexn {
        my ($which) = @_;
        ($which < 4) ? ("th.lex$which") : ("th.lexn[" . ($which - 4) . "]");
    }

    sub push_let {
        my ($self, $which) = @_;
        my ($v, $ty) = $self->_popn(1);
        $self->_emit(_lexn($self->minlets + @{$self->letstack}) . " = $v");
        push @{$self->letstack}, $which;
        push @{$self->lettypes}, $ty;
        if (@{$self->letstack} > $self->numlets) {
            $self->numlets(scalar @{$self->letstack});
        }
    }

    sub drop_let {
        my ($self, $which) = @_;
        die "Let consistency error" if $which ne $self->letstack->[-1];
        pop @{ $self->letstack };
        pop @{ $self->lettypes };
    }

    sub peek_let {
        my ($self, $which) = @_;
        my $i = @{ $self->letstack } - 1;
        while ($i >= 0 && $self->letstack->[$i] ne $which) { $i-- }
        $self->_push("object", _lexn($self->minlets + $i));
        $self->cast($self->lettypes->[$i]);
    }

    sub poke_let {
        my ($self, $which) = @_;
        my $i = @{ $self->letstack } - 1;
        while ($i >= 0 && $self->letstack->[$i] ne $which) { $i-- }
        my ($v) = $self->_popn(1);
        $self->_emit(_lexn($self->minlets + $i) . " = $v");
    }

    sub const {
        my ($self) = @_;
        my ($val, $type) = $self->_popn(1);
        my $knum = @{ $self->consttab };
        my $name = "K_" . $self->csname . "_$knum";
        push @{ $self->consttab }, "$type $name = $val";
        $self->_push($type, $name);
    }

    sub has_let {
        my ($self, $which) = @_;
        my $i = @{ $self->letstack } - 1;
        while ($i >= 0 && $self->letstack->[$i] ne $which) { $i-- }
        $i >= 0;
    }

    sub label {
        my ($self) = @_;
        my $n = $self->numlabels;
        $self->numlabels($n + 1);
        return $n;
    }

    sub ip {
        my ($self) = @_;
        my $n = $self->numips;
        $self->numips($n + 1);
        return $n;
    }

    sub labelhere {
        my ($self, $n) = @_;
        my $ip = $self->labelname->{$n} = $self->ip;
        $self->_restorestackstate($n) if $self->savedstks->{$n};
        push @{ $self->buffer }, "    goto case $ip;\n" unless $self->unreach;
        push @{ $self->buffer }, "case $ip:\n";
        $self->lineinfo->[$ip] = $line;
        $self->unreach(0);
    }

    sub goto {
        my ($self, $n) = @_;
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    goto case \@\@L$n;\n";
        $self->unreach(1);
    }

    sub cgoto {
        my ($self, $n) = @_;
        my ($top) = $self->_popn(1);
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    if ($top) { goto case \@\@L$n; }\n";
    }

    sub ncgoto {
        my ($self, $n) = @_;
        my ($top) = $self->_popn(1);
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    if (!$top) { goto case \@\@L$n; }\n";
    }

    sub ehspan {
        my ($self, $type, $lab, $lidl, $ls, $le, $lg) = @_;
        my $lid = -1;
        if (defined $lab) {
            $lid = @{ $self->ehlabels };
            $self->labelname->{$lidl} = $lid if defined $lidl;
        }
        push @{ $self->ehspans }, "\@\@L$ls", "\@\@L$le", $type,
            "\@\@L$lg", $lid;
    }

    sub hintget {
        my ($self, $type, $data, $name) = @_;
        $self->_push("object", "$data.hints[" . qm($name) . "]");
        $self->cast($type);
    }

    sub rtpadget {
        my ($self, $type, $order, $name) = @_;
        $self->callframe;
        my ($frame) = $self->_popn(1);
        $self->_push("object", $frame . (".outer" x $order) . ".lex[" . qm($name) . "]");
        $self->cast($type);
    }

    sub rtpadput {
        my ($self, $order, $name) = @_;
        $self->callframe;
        my ($val, $frame) = $self->_popn(2);
        $self->_emit($frame . (".outer" x $order) . ".lex[" . qm($name) . "] = $val");
    }

    sub rtpadgeti {
        my ($self, $type, $order, $name) = @_;
        $self->callframe;
        my ($frame) = $self->_popn(1);
        my $tag = $name < 4 ? "lex$name" : ("lexn[" . ($name - 4) . "]");
        $self->_push("object", $frame . (".outer" x $order) . ".$tag");
        $self->cast($type);
    }

    sub rtpadputi {
        my ($self, $order, $name) = @_;
        $self->callframe;
        my ($val, $frame) = $self->_popn(2);
        my $tag = $name < 4 ? "lex$name" : ("lexn[" . ($name - 4) . "]");
        $self->_emit($frame . (".outer" x $order) . ".$tag = $val");
    }

    sub callframe {
        my ($self) = @_;
        my $frame = 'th';
        if ($self->has_let('protopad')) {
            $self->peek_let('protopad');
        } else {
            $self->_push("Frame", $frame);
        }
    }

    sub drop {
        my ($self) = @_;
        $self->_emit(($self->_popn(1))[0]);
    }

    sub pos {
        my ($self, $num) = @_;
        if (! defined($num)) {
            $num = ($self->_popn(1))[0];
        }
        $self->_push('Variable', "th.pos[$num]");
    }

    sub _prepcall {
        my ($self, @sig) = @_;
        my $quick = 1;
        for (@sig) { $quick &&= ($_ eq '') }
        my ($inv, @vals) = $self->_popn(1 + scalar(@sig));
        $#vals = $#sig;
        if ($quick) {
            return $inv, "new Variable[] { " . join(", ", @vals) . "}", "null";
        } else {
            # TODO: optimize harder
            $self->outcap(1);
            $self->_emit("_inv = $inv");
            $self->_emit("_pos = new List<Variable>()");
            $self->_emit("_nam = new Dictionary<string,Variable>()");
            for (my $ix = 0; $ix < @sig; $ix++) {
                if ($sig[$ix] eq '') {
                    $self->_emit("_pos.Add($vals[$ix])");
                } elsif (substr($sig[$ix],0,1) eq ':') {
                    my $n = qm(substr($sig[$ix],1));
                    $self->_emit("_nam[$n] = $vals[$ix]");
                } elsif ($sig[$ix] eq 'flatpos') {
                    $self->_emit("_pos.AddRange($vals[$ix])");
                } elsif ($sig[$ix] eq 'flatnam') {
                    $self->_emit("Kernel.AddMany(_nam, $vals[$ix])");
                } else {
                    die "weird sig bit $sig[$ix]";
                }
            }
            return "_inv", "_pos.ToArray()", "_nam";
        }
    }

    sub call_method {
        my ($self, $name, @sig) = @_;
        my ($inv, $pos, $nam) = $self->_prepcall(@sig);
        $self->_cpscall('Variable', "$inv.InvokeMethod(th, ".qm($name).", $pos, $nam)");
    }

    sub call_sub {
        my ($self, @sig) = @_;
        my ($inv, $pos, $nam) = $self->_prepcall(@sig);
        $self->_cpscall('Variable', "$inv.Invoke(th, $pos, $nam)");
    }

    sub tail_call_sub {
        my ($self, @sig) = @_;
        my ($inv, $pos, $nam) = $self->_prepcall(@sig);
        $self->_emit("return $inv.Invoke(th.caller, $pos, $nam)");
        $self->unreach(1);
    }

    sub clr_bool {
        my ($self, $v) = @_;
        $self->_push('System.Boolean', $v ? 'true' : 'false');
    }

    sub clr_new {
        my ($self, $class, $nargs) = @_;
        my @args = reverse map { ($self->_popn(1))[0] } 1 .. $nargs;
        $self->_push($class, "new $class(" . join(", ", @args) . ")");
    }

    sub clr_new_arr {
        my ($self, $class, $nitems) = @_;
        my @args = reverse map { ($self->_popn(1))[0] } 1 .. $nitems;
        $self->_push($class . "[]", "new $class []{" . join(", ", @args) . "}");
    }

    sub clr_new_zarr {
        my ($self, $class) = @_;
        my ($nitems) = $self->_popn(1);
        $self->_push($class . "[]", "(new $class [$nitems])");
    }

    sub clr_string {
        my ($self, $text) = @_;
        $self->_push('System.String', qm($text));
    }

    sub clr_char {
        my ($self, $val) = @_;
        $self->_push('Char', "((char)" . ord($val) . ")");
    }

    sub clr_int {
        my ($self, $val) = @_;
        $self->_push('Int32', $val);
    }

    sub clr_double {
        my ($self, $val) = @_;
        $self->_push('System.Double', "((Double)$val)");
    }

    sub labelid {
        my ($self, $lbl) = @_;
        $self->_push('Int32', "\@\@L$lbl");
    }

    sub clr_arith {
        my ($self, $op) = @_;
        my ($a1, $a2, $ty1, $ty2) = $self->_popn(2);
        if ($ty1 ne $ty2) {
            die "Overloaded operations not yet supported";
        }
        $self->_push($ty1, "($a1 $op $a2)");
    }

    sub clr_compare {
        my ($self, $op) = @_;
        my ($a1, $a2) = $self->_popn(2);
        $self->_push('Boolean', "($a1 $op $a2)");
    }

    sub clr_field_get {
        my ($self, $f) = @_;
        my ($obj, $oty) = $self->_popn(1);
        my ($nm, $cl, $ty) = $self->_typedata('f', $oty, $f);
        $self->_push($ty, "($obj.$nm)");
    }

    sub clr_field_set {
        my ($self, $f) = @_;
        my ($obj, $val) = $self->_popn(2);
        $self->_emit("$obj.$f = $val");
    }

    sub clr_sfield_get {
        my ($self, $f) = @_;
        my ($nm, $cl, $ty) = $self->_typedata('f', $f);
        $self->_push($ty, "$nm");
    }

    sub clr_sfield_set {
        my ($self, $f) = @_;
        my ($val) = $self->_popn(1);
        $self->_emit($self->_striptype($f) . " = $val");
    }

    sub attr_var {
        my ($self, $f) = @_;
        my ($obj) = $self->_popn(1);
        $self->_cpscall('Variable', "$obj.GetAttribute(th, " . qm($f) . ")");
    }

    sub clr_index_get {
        my ($self, $f) = @_;
        if ($f) {
            $self->clr_string($f);
        }
        my ($obj, $ix, $oty, $ixty)  = $self->_popn(2);
        my $ty  = ($oty =~ /^Dictionary<.*,(.*)>$/) ? $1 :
                  ($oty =~ /^VarDeque$/) ? 'Variable' :
                  ($oty =~ /^(.*)\[\]$/) ? $1 :
                  ($oty =~ /^List<(.*)>$/) ? $1 :
                  die "type inference needs more hacks $oty";
        $self->_push($ty, "($obj" . "[$ix])");
    }

    sub clr_index_set {
        my ($self, $f) = @_;
        my ($val) = $self->_popn(1);
        my ($ix)  = $self->_popn(1) unless $f;
        my ($obj) = $self->_popn(1);
        $self->_emit("$obj" . "[" . ($f ? qm($f) : $ix) . "] = ($val)");
    }

    sub cast {
        my ($self, $type) = @_;
        $self->_push($type, "(($type)" . ($self->_popn(1))[0] . ")");
    }

    sub fgoto {
        my ($nv, $self) = @_;
        $self->_cpscall(($nv ? 'Variable' : 'Void'), ($self->_popn(1))[0]);
    }

    sub clr_call_direct {
        my ($self, $name, $nargs) = @_;
        my ($nm, $cl, $rt) = $self->_typedata('cm', $name);
        my @args = reverse map { ($self->_popn(1))[0] } 1 .. $nargs;
        if ($cl eq 'c') {
            $self->_cpscall($rt,
                "$nm(" . join(", ", "th", @args) . ")");
        } elsif ($rt ne 'Void') {
            $self->_push($rt, "$nm(" . join(", ", @args) . ")");
        } else {
            $self->_emit("$nm(" . join(", ", @args) . ")");
        }
    }

    sub clr_call_virt {
        my ($self, $name, $nargs) = @_;
        my @args = reverse map { ($self->_popn(1))[0] } 1 .. $nargs;
        my ($nm, $cl, $rt) = $self->_typedata('cm', $self->stacktype->[-1],
            $name);
        my ($inv) = $self->_popn(1);
        if ($cl eq 'c') {
            $self->_cpscall($rt,
                "$inv.$nm(" . join(", ", "th", @args) . ")");
        } elsif ($rt ne 'Void') {
            $self->_push($rt, "$inv.$nm(" . join(", ", @args) . ")");
        } else {
            $self->_emit("$inv.$nm(" . join(", ", @args) . ")");
        }
    }

    sub rxbprim {
        my ($self, $name, $nargs) = @_;
        my @args = reverse map { ($self->_popn(1))[0] } 1 .. $nargs;
        $self->_emit("if (!th.rx.$name(" . join(", ", @args) . ")) goto case \@\@Lbacktrack");
    }

    sub rxpushb {
        my ($self, $tag, $label) = @_;
        push @{ $self->buffer }, "    th.rx.PushBacktrack(\@\@L$label);\n";
    }

    sub return {
        my ($self, $nv) = @_;
        return if $self->unreach;
        if ($nv) {
            $self->_emit("th.caller.resultSlot = " . ($self->_popn(1))[0]);
        }
        $self->_emit("return th.caller");
        $self->unreach(1);
    }

    sub push_null {
        my ($self, $ty) = @_;
        # (Int32)null isn't liked much
        $self->_push($ty, $ty eq 'Int32' ? "0" : "null");
    }

    ###

    sub subinfo_ctor_args {
        my ($self, $outersi, $ltm) = @_;
        for (@{ $self->buffer }, @{ $self->consttab }, @{ $self->ehspans }) {
            s/\@\@L(\w+)/$self->labelname->{$1}/eg;
        }
        (CgOp::clr_string("$::UNITNAME " . $self->csname),
         CgOp::rawnewarr('int', map { CgOp::int($_//0) } @{ $self->lineinfo }),
         CgOp::rawsget($::UNITNAME . '.' . $self->csname . ':f,DynBlockDelegate'),
         $outersi, $ltm,
         CgOp::rawnewarr('int', map { CgOp::int($_) } @{ $self->ehspans }),
         (@{ $self->ehlabels } ? CgOp::rawnewarr('string',
              map { CgOp::clr_string($_) } @{ $self->ehlabels }) :
              CgOp::null('string[]')));
    }

    sub csharp {
        my ($self) = @_;
        my $t = '';
        my $name = $self->csname;
        $t .= " " x 4 . "public static Frame $name(Frame th) {\n";
        if ($self->outcap) {
            $t .= " " x 8 . "IP6 _inv; List<Variable> _pos; Dictionary<string,Variable> _nam;\n";
        }
        $t .= " " x 8 . "switch (th.ip) {\n";
        $t .= " " x 12 . "case 0:\n";
        if ($self->numlets + $self->minlets > 4) {
            $t .= " " x 16 . "th.lexn = new object[" .
                ($self->numlets + $self->minlets - 4) . "];\n";
        }
        if ($self->usednamed) {
            $t .= " " x 16 . "if (th.lex == null) th.lex = new Dictionary<string,object>();\n";
        }
        for (@{ $self->buffer }, @{ $self->consttab }, @{ $self->ehspans }) {
            s/\@\@L(\w+)/$self->labelname->{$1}/eg;
        }
        $t .= " " x 12 . $_ for @{ $self->buffer };
        $t .= " " x 12 . "default:\n";
        $t .= " " x 16 . "return Kernel.Die(th, \"Invalid IP\");\n";
        $t .= " " x 8 . "}\n";
        $t .= " " x 4 . "}\n";
        for (@{ $self->consttab }) {
            $t .= " " x 4 . "private static $_;\n";
        }
        $t;
    }

    sub BUILD {
        my $self = shift;
        $self->ops->var_cg($self);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
