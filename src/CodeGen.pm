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
              IsDefined    => [m => 'Boolean'],
              HOW          => [c => 'IP6'] },
        DynObject =>
            { klass        => [f => 'DynMetaObject'],
              slots        => [f => 'Dictionary<string,Object>'] },

        DynMetaObject =>
            { BuildC3MRO   => [m => 'Void'],
              HasMRO       => [m => 'Boolean'],
              AddMultiRegex=> [m => 'Void'],
              typeObject   => [f => 'IP6'],
              how          => [f => 'IP6'],
              local        => [f => 'Dictionary<string,IP6>'],
              local_attr   => [f => 'Dictionary<string,IP6>'],
              superclasses => [f => 'List<DynMetaObject>'],
              name         => [f => 'String'] },

        'Double' =>
            { ToString     => [m => 'String'] },
        'Variable' =>
            { islist       => [f => 'Boolean'] },
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
              IncQuant     => [m => 'Void'],
              GetQuant     => [m => 'Int32'],
              OpenQuant    => [m => 'Void'],
              CloseQuant   => [m => 'Int32'],
              CommitGroup  => [m => 'Void'],
              Backtrack    => [c => 'Void'],
              End          => [c => 'Void'] },
        'Niecza.FatalException' =>
            { SearchForHandler => [c => 'Void'] },
        'Niecza.LexoticControlException' =>
            { SearchForHandler => [c => 'Void'] },
        'Cursor' =>
            { At           => [m => 'Cursor'],
              pos          => [f => 'Int32'],
              backing      => [f => 'String'],
              SimpleWS     => [m => 'Variable'] },
        'Lexer' =>
            { Run          => [m => 'Int32[]'] },

        'System.IO.File.ReadAllText' => [m => 'System.String'],

        'Lexer.RunProtoregex'  => [m => 'IP6[]'],
        'Kernel.Die'           => [c => 'Void'],
        'Kernel.CoTake'        => [c => 'Variable'],
        'Kernel.Take'          => [c => 'Variable'],
        'Kernel.GatherHelper'  => [c => 'Frame'],
        'Kernel.ContextHelper' => [m => 'Variable'],
        'Kernel.StrP'          => [f => 'IP6'],
        'Kernel.CallFrameMO'   => [f => 'DynMetaObject'],
        'Kernel.Process'       => [f => 'Variable'],
        'Kernel.Global'        => [f => 'Variable'],
        'Kernel.PackageLookup' => [m => 'Variable'],
        'Kernel.SlurpyHelper'  => [m => 'List<Variable>'],
        'Kernel.Bind'          => [c => 'Void'],
        'Kernel.BindNewScalar' => [c => 'Variable'],
        'Kernel.BindNewList'   => [c => 'Variable'],
        'Kernel.Assign'        => [c => 'Void'],
        'Kernel.Fetch'         => [c => 'IP6'],
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
        'Kernel.MakeSC'        => [m => 'IP6'],
        'Kernel.BoxAny'        => [m => 'Variable'],
        'Kernel.UnboxAny'      => [m => 'object'],
    );

    sub know_sfield {
        my ($class, $n, $ty) = @_;
        $typedata{$n} = [ f => $ty ];
    }

    sub know_module {
        my ($class, $mname) = @_;
        # for settings
        $typedata{ $mname . '.Environment' } = [ f => 'Frame' ];
        $typedata{ $mname . '.Installer' } = [ f => 'IP6' ];
        # for importable modules
        $typedata{ $mname . '.Type' } = [ f => 'Variable' ];
        $typedata{ $mname . '.Stash' } = [ f => 'Variable' ];
        # for all
        $typedata{ $mname . '.Initialize' } = [ m => 'Void' ];
    }
    __PACKAGE__->know_module('NULL');

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

    sub _typedata {
        my ($self, $types, @path) = @_;

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

    has csname    => (isa => 'Str', is => 'ro');
    has entry     => (isa => 'Bool', is => 'ro', default => 0);
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
    has lex2type  => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has buffer    => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has unreach   => (isa => 'Bool', is => 'rw', default => 0);
    has outcap    => (isa => 'Bool', is => 'rw', default => 0);

    has letstack  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has lettypes  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has numlets   => (isa => 'Int', is => 'rw', default => 0);
    has minlets   => (isa => 'Int', is => 'ro', default => 0);
    has body      => (isa => 'Body', is => 'ro');
    has bodies    => (isa => 'ArrayRef', is => 'ro', default => sub { [] });

    has savedstks => (isa => 'HashRef', is => 'ro', default => sub { +{} });

    has ops => (is => 'ro', required => 1);

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
        die "Invalid operation of CPS converter" if @{ $self->stacktype };
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
        Carp::confess "Stack underflow" if $nr > @{ $self->stacktype };
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

    sub rawlexget {
        my ($self, $name) = @_;
        $self->_push("object", "th.lex[" . qm($name) . "]");
        $self->cast($self->lex2type->{$name}[0]);
    }

    sub rawlexput {
        my ($self, $name) = @_;
        $self->_emit("th.lex[" . qm($name) . "] = " . ($self->_popn(1))[0]);
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
        $self->_emit("$f = $val");
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
        $self->_emit("if (!th.rx.$name(" . join(", ", @args) . ")) return th.rx.Backtrack(th)");
    }

    sub rxpushb {
        my ($self, $tag, $label) = @_;
        push @{ $self->buffer }, "    th.rx.PushBacktrack(" . qm($tag) .
            ", " . ($label ? "\@\@L$label" : "-1") . ");\n";
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

    sub close_sub {
        my ($self, $body, $withclass) = @_;

        $self->_emit($body->csname . "_info.PutHint(\"?file\", " . qm($body->file) . ")") if $body->type eq 'mainline';
        my $ob = $self->bodies->[-2];
        if ($ob) {
            $self->_emit($body->csname . "_info.outer = " . $ob->csname . "_info");
        }
        if ($withclass) {
            my ($cl) = $self->_popn(1);
            $self->_emit($body->csname . "_info.mo = ((DynObject)$cl).klass");
        }

        $self->drop_let('protopad') if $body->needs_protopad;
        pop @{ $self->bodies };
    }

    sub sub_obj {
        my ($self, $bodyname) = @_;
        $self->callframe;
        my ($op) = $self->_popn(1);
        $self->_push('IP6', "Kernel.MakeSub(${bodyname}_info, $op)");
    }

    sub proto_var {
        my ($self, $name) = @_;
        my ($type, $kind, $data) = @{ $self->bodies->[-1]->lexical->{$name} };
        if ($kind == 3) {
            $self->clr_sfield_set($data);
            $self->clr_sfield_get($data . ":f,IP6");
            $self->clr_call_direct('Kernel.NewROScalar', 1);
            $self->clr_sfield_set($data . "_var");
            return;
        }
        if ($kind == 1) {
            $self->clr_sfield_set($data);
            return;
        }
        if ($kind == 2) {
            my ($pv) = $self->_popn(1);
            $self->_emit("$data.PutHint(" . qm($name) . ", $pv)");
            return;
        }
        $self->peek_let('protopad');
        my ($pv, $pp) = $self->_popn(2);
        $self->_emit("$pp.lex[" . qm($name) . "] = ($pv)");
    }

    # XXX a bit too much integration here
    sub set_ltm {
        my ($self, $bodyn) = @_;
        my ($ltm) = $self->_popn(1);
        $self->_emit("${bodyn}_info.ltm = $ltm");
    }

    # somewhat misnamed; it generally controls the binding context in BOOT
    sub open_protopad {
        my ($self, $body) = @_;
        if ($body->needs_protopad) {
            $self->push_null('Frame');
            $self->peek_let('protopad');
            $self->_push('SubInfo', $body->csname . "_info");
            $self->clr_new('Frame', 3);
            $self->push_let('protopad');
        }
        push @{ $self->bodies }, $body;
    }

    ###

    sub write {
        my ($self) = @_;
        if ($self->body) {
            my $l = $self->body->lexical;
            for my $ve (sort keys %$l) {
                next unless ref $l->{$ve} eq 'ARRAY';
                my ($ty, $k, $d) = @{ $l->{$ve} };
                if ($k == 1) {
                    $d =~ s/.*\.//;
                    print ::NIECZA_OUT " " x 4, "public static $ty $d;\n";
                } elsif ($k == 3) {
                    $d =~ s/.*\.//;
                    print ::NIECZA_OUT " " x 4, "public static IP6 $d;\n";
                    print ::NIECZA_OUT " " x 4, "public static Variable ${d}_var;\n";
                }
            }
        }
        my $name = $self->csname;
        my $vis  = ($self->entry ? 'public' : 'private');
        print ::NIECZA_OUT " " x 4, "$vis static Frame $name(Frame th) {\n";
        if ($self->outcap) {
            print ::NIECZA_OUT " " x 8, "IP6 _inv; List<Variable> _pos; Dictionary<string,Variable> _nam;\n";
        }
        print ::NIECZA_OUT " " x 8, "if (Kernel.TraceCont) { Console.WriteLine(th.DepthMark() + \"$::UNITNAME : $name @ \" + th.ip); }\n";
        print ::NIECZA_OUT " " x 8, "switch (th.ip) {\n";
        print ::NIECZA_OUT " " x 12, "case 0:\n";
        if ($self->numlets + $self->minlets > 4) {
            print ::NIECZA_OUT " " x 16, "th.lexn = new object[",
                ($self->numlets + $self->minlets - 4), "];\n";
        }
        for (@{ $self->buffer }) {
            s/\@\@L(\w+)/$self->labelname->{$1}/eg;
        }
        print ::NIECZA_OUT " " x 12, $_ for @{ $self->buffer };
        print ::NIECZA_OUT " " x 12, "default:\n";
        print ::NIECZA_OUT " " x 16, "throw new Exception(\"Invalid IP\");\n";
        print ::NIECZA_OUT " " x 8, "}\n";
        print ::NIECZA_OUT " " x 4, "}\n";
        print ::NIECZA_OUT " " x 4, "private static int[] ${name}_lines = {",
            join (", ", map { ($_ // 0) } @{ $self->lineinfo }), "};\n";
        print ::NIECZA_OUT " " x 4, "private static SubInfo ${name}_info = ",
            "new SubInfo(${name}_lines, ${name}, null, null, null);\n";
    }

    sub BUILD {
        my $self = shift;
        $self->ops->var_cg($self);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
