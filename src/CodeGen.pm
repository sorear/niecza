use strict;
use warnings;
use 5.010;

use CgOpToCLROp;

{
    package CodeGen;
    use Moose;

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
              lexn         => [f => 'object[]'],
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
              Item         => [i => 'Variable'],
              UnshiftN     => [m => 'Void'],
              Pop          => [m => 'Variable'],
              Shift        => [m => 'Variable'],
              Count        => [m => 'Int32'] },

        'System.IO.File.ReadAllText' => [m => 'System.String'],

        'Lexer.RunProtoregex'  => [m => 'IP6[]'],
        'Lexer.GetLexer'       => [m => 'Lexer'],
        'Kernel.SearchForHandler' => [c => 'Variable'],
        'Kernel.Die'           => [c => 'Variable'],
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
            Item                => [ i => $2 ],
        };
        /List<(.*)>/ && return {
            Add                 => [m => 'Void'],
            Insert              => [m => 'Void'],
            RemoveAt            => [m => 'Void'],
            Count               => [f => 'Int32'],
            Item                => [i => $1],
        };
        /(.*)\[\]/ && return {
            Length              => [f => 'Int32'],
            Item                => [i => $1],
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

    has numlabels => (isa => 'Int', is => 'rw', default => 1);
    has numips    => (isa => 'Int', is => 'rw', default => 1);
    has lineinfo  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has resulttype=>(isa => 'Maybe[Str]', is => 'rw', default => '');
    has labelname => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has buffer    => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has unreach   => (isa => 'Bool', is => 'rw', default => 0);
    has outcap    => (isa => 'Bool', is => 'rw', default => 0);

    has letstack  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has lettypes  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has numlets   => (isa => 'Int', is => 'rw', default => 0);
    has consttab  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has linestack => (isa => 'ArrayRef', is => 'ro', default => sub { [0] });
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

    sub _odds {
        my @o; for (my $i = 1; $i < @_; $i+=2 ) { push @o, $_[$i] } @o
    }

    sub _savestackstate {
        my ($self, $lbl) = @_;
        my %save;
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

    sub _cpscall {
        my ($self, $rt, $expr) = @_;
        my $n = $self->ip;
        $self->_emit("th.ip = $n");
        $self->_emit("return $expr");
        $self->lineinfo->[$n] = $self->linestack->[-1];
        push @{ $self->buffer }, "case $n:\n";
        die "Broken call $expr" if !defined($rt);
        $self->resulttype($rt);
    }

    # These functions are usable from user code, but still depend on volatiles.

    sub result {
        my ($self) = @_;
        $self->cast($self->resulttype, "object", "th.resultSlot");
    }

    sub set_result {
        my ($self, $ty, $it) = @_;
        $self->resulttype($ty);
        $self->_emit("th.resultSlot = $it");
    }

    sub _lexn {
        my ($which) = @_;
        ($which < 4) ? ("th.lex$which") : ("th.lexn[" . ($which - 4) . "]");
    }

    sub push_line {
        my ($self, $line) = @_;
        push @{ $self->linestack }, $line;
    }

    sub pop_line {
        my ($self) = @_;
        pop @{ $self->linestack };
    }

    sub push_let {
        my ($self, $which, $ty, $v) = @_;
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
        $self->cast($self->lettypes->[$i], "object",
            _lexn($self->minlets + $i));
    }

    sub poke_let {
        my ($self, $which, $ty, $v) = @_;
        my $i = @{ $self->letstack } - 1;
        while ($i >= 0 && $self->letstack->[$i] ne $which) { $i-- }
        $self->_emit(_lexn($self->minlets + $i) . " = $v");
    }

    sub const {
        my ($self, $type, $val) = @_;
        my $knum = @{ $self->consttab };
        my $name = "K_" . $self->csname . "_$knum";
        push @{ $self->consttab }, "$type $name = $val";
        $type, $name;
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
        $self->lineinfo->[$ip] = $self->linestack->[-1];
        $self->unreach(0);
    }

    sub goto {
        my ($self, $n) = @_;
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    goto case \@\@L$n;\n";
        $self->unreach(1);
    }

    sub cgoto {
        my ($self, $n, $ty, $top) = @_;
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    if ($top) { goto case \@\@L$n; }\n";
    }

    sub ncgoto {
        my ($self, $n, $ty, $top) = @_;
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
        $self->cast($type, "object", "$data.hints[" . qm($name) . "]");
    }

    sub rtpadget {
        my ($self, $type, $order, $name) = @_;
        $self->cast($type, "object", "th" . (".outer" x $order) . ".lex[" . qm($name) . "]");
    }

    sub rtpadput {
        my ($self, $order, $name, $ty, $val) = @_;
        $self->_emit("th" . (".outer" x $order) . ".lex[" . qm($name) . "] = $val");
    }

    sub rtpadgeti {
        my ($self, $type, $order, $name) = @_;
        my $tag = $name < 4 ? "lex$name" : ("lexn[" . ($name - 4) . "]");
        $self->cast($type, "object", "th" . (".outer" x $order) . ".$tag");
    }

    sub rtpadputi {
        my ($self, $order, $name, $type, $val) = @_;
        my $tag = $name < 4 ? "lex$name" : ("lexn[" . ($name - 4) . "]");
        $self->_emit("th" . (".outer" x $order) . ".$tag = $val");
    }

    sub callframe {
        my ($self) = @_;
        "Frame", "th";
    }

    sub drop {
        my ($self, $ty, $val) = @_;
        $self->_emit($val);
    }

    sub pos {
        my ($self, $num, $vty, $val) = @_;
        'Variable', "th.pos[" . ($num // $val) . "]";
    }

    sub _prepcall {
        my ($self, $sig, @args) = @_;
        my $quick = 1;
        for (@$sig) { $quick &&= ($_ eq '') }
        my ($inv, @vals) = _odds(@args);
        if ($quick) {
            return $inv, "new Variable[] { " . join(", ", @vals) . "}", "null";
        } else {
            # TODO: optimize harder
            $self->outcap(1);
            $self->_emit("_inv = $inv");
            $self->_emit("_pos = new List<Variable>()");
            $self->_emit("_nam = new Dictionary<string,Variable>()");
            for (my $ix = 0; $ix < @$sig; $ix++) {
                if ($sig->[$ix] eq '') {
                    $self->_emit("_pos.Add($vals[$ix])");
                } elsif (substr($sig->[$ix],0,1) eq ':') {
                    my $n = qm(substr($sig->[$ix],1));
                    $self->_emit("_nam[$n] = $vals[$ix]");
                } elsif ($sig->[$ix] eq 'flatpos') {
                    $self->_emit("_pos.AddRange($vals[$ix])");
                } elsif ($sig->[$ix] eq 'flatnam') {
                    $self->_emit("Kernel.AddMany(_nam, $vals[$ix])");
                } else {
                    die "weird sig bit $sig->[$ix]";
                }
            }
            return "_inv", "_pos.ToArray()", "_nam";
        }
    }

    sub call_method {
        my ($self, $name, $sig, @args) = @_;
        my ($inv, $pos, $nam) = $self->_prepcall($sig, @args);
        $self->_cpscall('Variable', "$inv.InvokeMethod(th, ".qm($name).", $pos, $nam)");
    }

    sub call_sub {
        my ($self, $sig, @args) = @_;
        my ($inv, $pos, $nam) = $self->_prepcall($sig, @args);
        $self->_cpscall('Variable', "$inv.Invoke(th, $pos, $nam)");
    }

    sub tail_call_sub {
        my ($self, $sig, @args) = @_;
        my ($inv, $pos, $nam) = $self->_prepcall($sig, @args);
        $self->_emit("return $inv.Invoke(th.caller, $pos, $nam)");
        $self->unreach(1);
    }

    sub clr_bool {
        my ($self, $v) = @_;
        'System.Boolean', $v ? 'true' : 'false';
    }

    sub clr_new {
        my ($self, $class, $nargs, @args) = @_;
        $class, "new $class(" . join(", ", _odds @args) . ")";
    }

    sub clr_new_arr {
        my ($self, $class, $nitems, @args) = @_;
        $class . "[]", "new $class []{" .  join(", ", _odds @args) . "}";
    }

    sub clr_new_zarr {
        my ($self, $class, $nity, $nitems) = @_;
        $class . "[]", "(new $class [$nitems])";
    }

    sub clr_string {
        my ($self, $text) = @_;
        'System.String', qm($text);
    }

    sub clr_char {
        my ($self, $val) = @_;
        'Char', "((char)" . ord($val) . ")";
    }

    sub clr_int {
        my ($self, $val) = @_;
        'Int32', $val;
    }

    sub clr_double {
        my ($self, $val) = @_;
        'System.Double', "((Double)$val)";
    }

    sub labelid {
        my ($self, $lbl) = @_;
        'Int32', "\@\@L$lbl";
    }

    sub clr_arith {
        my ($self, $op, $ty1, $a1, $ty2, $a2) = @_;
        if ($ty1 ne $ty2) {
            die "Overloaded operations not yet supported";
        }
        $ty1, "($a1 $op $a2)";
    }

    sub clr_compare {
        my ($self, $op, $ty1, $a1, $ty2, $a2) = @_;
        'Boolean', "($a1 $op $a2)";
    }

    sub clr_field_get {
        my ($self, $f, $oty, $obj) = @_;
        my ($nm, $cl, $ty) = $self->_typedata('f', $oty, $f);
        $ty, "($obj.$nm)";
    }

    sub clr_field_set {
        my ($self, $f, $oty, $obj, $vty, $val) = @_;
        $self->_emit("$obj.$f = $val");
    }

    sub clr_sfield_get {
        my ($self, $f) = @_;
        my ($nm, $cl, $ty) = $self->_typedata('f', $f);
        $ty, "$nm";
    }

    sub clr_sfield_set {
        my ($self, $f, $vty, $val) = @_;
        $self->_emit($self->_striptype($f) . " = $val");
    }

    sub attr_var {
        my ($self, $f, $oty, $obj) = @_;
        $self->_cpscall('Variable', "$obj.GetAttribute(th, " . qm($f) . ")");
    }

    sub clr_index_get {
        my ($self, $f) = splice @_, 0, 2;
        my ($oty, $obj, $ixty, $ix) = defined($f) ?
            (@_, '', qm($f)) : @_;
        my ($nm, $cl, $ty) = $self->_typedata('i', $oty, 'Item');
        $ty, "($obj" . "[$ix])";
    }

    sub clr_index_set {
        my ($self, $f) = splice @_, 0, 2;
        my ($oty, $obj) = splice(@_,0,2);
        my ($ixty, $ix) = splice(@_,0,2) unless $f;
        my ($vty, $val) = splice(@_,0,2);
        $self->_emit("$obj" . "[" . (defined($f)? qm($f) : $ix) . "] = ($val)");
    }

    sub cast {
        my ($self, $type, $vty, $val) = @_;
        $type, "(($type)$val)";
    }

    sub clr_call_direct {
        my ($self, $name, $nargs, @args) = @_;
        my ($nm, $cl, $rt) = $self->_typedata('cm', $name);
        if ($cl eq 'c') {
            $self->_cpscall($rt,
                "$nm(" . join(", ", "th", _odds @args) . ")");
        } elsif ($rt ne 'Void') {
            return ($rt, "$nm(" . join(", ", _odds @args) . ")");
        } else {
            $self->_emit("$nm(" . join(", ", _odds @args) . ")");
        }
    }

    sub clr_call_virt {
        my ($self, $name, $nargs, $ity, $inv, @args) = @_;
        my ($nm, $cl, $rt) = $self->_typedata('cm', $ity, $name);
        if ($cl eq 'c') {
            $self->_cpscall($rt,
                "$inv.$nm(" . join(", ", "th", _odds @args) . ")");
        } elsif ($rt ne 'Void') {
            return ($rt, "$inv.$nm(" . join(", ", _odds @args) . ")");
        } else {
            $self->_emit("$inv.$nm(" . join(", ", _odds @args) . ")");
        }
    }

    sub rxbprim {
        my ($self, $name, $nargs, @args) = @_;
        $self->_emit("if (!th.rx.$name(" . join(", ", _odds @args) . ")) goto case \@\@Lbacktrack");
    }

    sub rxpushb {
        my ($self, $tag, $label) = @_;
        push @{ $self->buffer }, "    th.rx.PushBacktrack(\@\@L$label);\n";
    }

    sub return {
        my ($self, $nv, $rty, $rval) = @_;
        return if $self->unreach;
        if ($nv) {
            $self->_emit("th.caller.resultSlot = $rval");
        }
        $self->_emit("return th.caller");
        $self->unreach(1);
    }

    sub push_null {
        my ($self, $ty) = @_;
        # (Int32)null isn't liked much
        $ty, $ty eq 'Int32' ? "0" : "null";
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
        CgOpToCLROp::codegen($self, $self->ops);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
