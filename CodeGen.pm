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
            { HOW          => [c => 'IP6'] },
        DynObject =>
            { klass        => [f => 'DynMetaObject'],
              slots        => [f => 'Dictionary<string,Object>'] },

        DynMetaObject =>
            { BuildC3MRO   => [m => 'Void'],
              HasMRO       => [m => 'Boolean'],
              typeObject   => [f => 'IP6'],
              how          => [f => 'IP6'],
              local        => [f => 'Dictionary<string,IP6>'],
              local_attr   => [f => 'Dictionary<string,IP6>'],
              superclasses => [f => 'List<DynMetaObject>'],
              name         => [f => 'String'] },

        'List<DynMetaObject>' =>
            { Add          => [m => 'Void'] },
        'List<Variable>' =>
            { Add          => [m => 'Void'],
              Insert       => [m => 'Void'],
              RemoveAt     => [m => 'Void'],
              Count        => [f => 'Int32'] },
        'LValue[]' =>
            { Length       => [f => 'Int32'] },
        'Double' =>
            { ToString     => [m => 'String'] },
        'Variable' =>
            { lv           => [f => 'LValue'] },
        'LValue' =>
            { islist       => [f => 'Boolean'] },
        'CLRImportObject' =>
            { val          => [f => 'Object'] },
        'String' =>
            { Length       => [f => 'Int32'],
              Substring    => [m => 'String'] },
        'System.Text.StringBuilder' =>
            { Append       => [m => 'Void'],
              ToString     => [m => 'String'] },
        'Frame' =>
            { pos          => [f => 'LValue[]'],
              caller       => [f => 'Frame'],
              outer        => [f => 'Frame'],
              proto        => [f => 'Frame'],
              lex          => [f => 'Dictionary<string,object>'],
              ExecutingLine=> [m => 'Int32'],
              ExecutingFile=> [m => 'String'],
              LexicalFind  => [m => 'Variable'] },
        'Niecza.FatalException' =>
            { SearchForHandler => [c => 'Void'] },
        'Niecza.LexoticControlException' =>
            { SearchForHandler => [c => 'Void'] },

        'System.IO.File.ReadAllText' => [m => 'System.String'],

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
        'Kernel.SubMO'         => [f => 'DynMetaObject'],
        'Kernel.ScalarMO'      => [f => 'DynMetaObject'],
        'Kernel.MainlineContinuation' => [f => 'DynBlockDelegate'],
        'Kernel.MakeSub'       => [m => 'IP6'],
        'Kernel.BoxAny'        => [m => 'Variable'],
        'Kernel.UnboxAny'      => [m => 'object'],
    );

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

    sub _typedata {
        my ($self, $types, @path) = @_;
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
            return @$cursor;
        } else {
            return $cursor->[1];
        }
    }

    has csname    => (isa => 'Str', is => 'ro');
    has entry     => (isa => 'Bool', is => 'ro', default => 0);
    has depth     => (isa => 'Int', is => 'rw', default => 0);
    has maxdepth  => (isa => 'Int', is => 'rw', default => 0);
    has savedepth => (isa => 'Int', is => 'rw', default => 0);
    has numlabels => (isa => 'Int', is => 'rw', default => 1);
    has fileinfo  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has lineinfo  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has stacktype => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has stackterm => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has resulttype =>(isa => 'Maybe[Str]', is => 'rw', default => '');
    has labelname => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has lex2type  => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has buffer    => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has unreach   => (isa => 'Bool', is => 'rw', default => 0);

    has letdepths => (isa => 'HashRef', is => 'ro', default => sub { +{} });
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
        $save{letdepths} = { %{ $self->letdepths } };
        $self->savedstks->{$lbl} = \%save;
    }

    sub _restorestackstate {
        my ($self, $lbl) = @_;
        my $save = $self->savedstks->{$lbl};
        %{ $self->letdepths } = %{ $save->{letdepths} };
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
        my $n = $self->label;
        $self->_emit("th.ip = $n");
        $self->_emit("return $expr");
        $self->lineinfo->[$n] = $line;
        $self->fileinfo->[$n] = $file;
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

    sub push_let {
        my ($self, $which) = @_;
        my $var = "let!${which}!" . ($self->letdepths->{$which}++);
        $self->lex2type->{$var} = $self->stacktype->[-1];
        $self->rawlexput($var);
    }

    sub pop_let {
        my ($self, $which) = @_;
        my $var = "let!${which}!" . (--$self->letdepths->{$which});
        $self->rawlexget($var);
    }

    sub drop_let {
        my ($self, $which) = @_;
        --$self->letdepths->{$which};
    }

    sub peek_let {
        my ($self, $which) = @_;
        my $var = "let!${which}!" . ($self->letdepths->{$which} - 1);
        $self->rawlexget($var);
    }

    sub label {
        my ($self) = @_;
        my $n = $self->numlabels;
        $self->numlabels($n + 1);
        return $n;
    }

    sub labelhere {
        my ($self, $n) = @_;
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        $self->_restorestackstate($n) if $self->savedstks->{$n};
        push @{ $self->buffer }, "    goto case $n;\n" unless $self->unreach;
        push @{ $self->buffer }, "case $n:\n";
        $self->lineinfo->[$n] = $line;
        $self->fileinfo->[$n] = $file;
        $self->unreach(0);
    }

    sub goto {
        my ($self, $n) = @_;
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    goto case $n;\n";
        $self->unreach(1);
    }

    sub cgoto {
        my ($self, $n) = @_;
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        my ($top) = $self->_popn(1);
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    if ($top) { goto case $n; }\n";
    }

    sub ncgoto {
        my ($self, $n) = @_;
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        my ($top) = $self->_popn(1);
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    if (!$top) { goto case $n; }\n";
    }

    sub rawlexget {
        my ($self, $name) = @_;
        $self->_push("object", "th.lex[" . qm($name) . "]");
        $self->cast($self->lex2type->{$name});
    }

    sub rawlexput {
        my ($self, $name) = @_;
        $self->_emit("th.lex[" . qm($name) . "] = " . ($self->_popn(1))[0]);
    }

    sub lexget {
        my ($self, $order, $name) = @_;
        my $frame = 'th.';
        if ($self->letdepths->{'protopad'}) {
            $frame = '((Frame)th.lex[' .
                qm('let!protopad!' . ($self->letdepths->{'protopad'} - 1)) .
                ']).';
        }
        # XXX need a better type tracking system
        $self->_push("object", $frame . ("outer." x $order) .
            "lex[" . qm($name) . "]");
        $self->cast(($order ? 'Variable' : ($self->lex2type->{$name}
                    // 'Variable')));
    }

    sub lexput {
        my ($self, $order, $name) = @_;
        my $frame = 'th.';
        if ($self->letdepths->{'protopad'}) {
            $frame = '((Frame)th.lex[' .
                qm('let!protopad!' . ($self->letdepths->{'protopad'} - 1)) .
                ']).';
        }
        $self->_emit($frame . ("outer." x $order) . "lex[" . qm($name) . "] = " . ($self->_popn(1))[0]);
    }

    sub callframe {
        my ($self) = @_;
        my $frame = 'th';
        if ($self->letdepths->{'protopad'}) {
            $frame = '((Frame)th.lex[' .
                qm('let!protopad!' . ($self->letdepths->{'protopad'} - 1)) .
                '])';
        }
        $self->_push("Frame", $frame);
    }

    sub drop {
        my ($self) = @_;
        $self->_emit(($self->_popn(1))[0]);
    }

    # the use of scalar here is a little bit wrong; semantically it's closer
    # to the old notion of ¢foo.  doesn't matter much since it's not exposed
    # at the Perl 6 level.
    sub pos {
        my ($self, $num) = @_;
        if (! defined($num)) {
            $num = ($self->_popn(1))[0];
        }
        $self->_push('Variable',
            "new Variable(false, Variable.Context.Scalar, th.pos[$num])");
    }

    sub protolget {
        my ($self, $name) = @_;
        $self->_push('object', "th.proto.lex[" . qm($name) . "]");
        $self->cast('Variable');
    }

    sub call_method {
        my ($self, $nv, $name, $numargs) = @_;
        my @args = reverse map { ($self->_popn(1))[0] }
                (1 .. $numargs + 1);  # invocant LV
        my ($inv) = $self->_popn(1);
        $self->_cpscall(($nv ? 'Variable' : 'Void'),
            "$inv.InvokeMethod(th, " . qm($name) . ", new LValue[" .
            scalar(@args) . "] { " . join(", ", map { "$_.lv" } @args) .
            " }, null)");
    }

    sub call_sub {
        my ($self, $nv, $numargs) = @_;
        my @args = reverse map { ($self->_popn(1))[0] } (1 .. $numargs);
        my ($inv) = $self->_popn(1);
        $self->_cpscall(($nv ? 'Variable' : 'Void'),
            "$inv.Invoke(th, new LValue[" . scalar(@args) . "] { " .
            join(", ", map { "$_.lv" } @args) . " }, null)");
    }

    sub tail_call_sub {
        my ($self, $numargs) = @_;
        my @args = reverse map { ($self->_popn(1))[0] } (1 .. $numargs);
        my ($inv) = $self->_popn(1);
        $self->_emit("return $inv.Invoke(th.caller, new LValue[" .
            scalar(@args) . "] { " . join(", ", map { "$_.lv" } @args) .
            " }, null)");
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

    sub clr_string {
        my ($self, $text) = @_;
        $self->_push('System.String', qm($text));
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
        my $ty = $self->_typedata('f', $oty, $f);
        $self->_push($ty, "($obj.$f)");
    }

    sub clr_field_set {
        my ($self, $f) = @_;
        my ($obj, $val) = $self->_popn(2);
        $self->_emit("$obj.$f = $val");
    }

    sub clr_sfield_get {
        my ($self, $f) = @_;
        my $ty = $self->_typedata('f', $f);
        $self->_push($ty, "$f");
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

    sub clr_call_direct {
        my ($self, $name, $nargs) = @_;
        my ($cl, $rt) = $self->_typedata('cm', $name);
        my @args = reverse map { ($self->_popn(1))[0] } 1 .. $nargs;
        if ($cl eq 'c') {
            $self->_cpscall($rt,
                "$name(" . join(", ", "th", @args) . ")");
        } elsif ($rt ne 'Void') {
            $self->_push($rt, "$name(" . join(", ", @args) . ")");
        } else {
            $self->_emit("$name(" . join(", ", @args) . ")");
        }
    }

    sub clr_call_virt {
        my ($self, $name, $nargs) = @_;
        my @args = reverse map { ($self->_popn(1))[0] } 1 .. $nargs;
        my ($cl, $rt) = $self->_typedata('cm', $self->stacktype->[-1], $name);
        my ($inv) = $self->_popn(1);
        if ($cl eq 'c') {
            $self->_cpscall($rt,
                "$inv.$name(" . join(", ", "th", @args) . ")");
        } elsif ($rt ne 'Void') {
            $self->_push($rt, "$inv.$name(" . join(", ", @args) . ")");
        } else {
            $self->_emit("$inv.$name(" . join(", ", @args) . ")");
        }
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
        my ($self, $body) = @_;

        $self->_push('object', $body->csname . "_lines");
        $self->proto_var('?lines');
        $self->_push('object', $body->csname . "_files");
        $self->proto_var('?files');

        $self->pop_let('protopad');
        pop @{ $self->bodies };
        $self->peek_let('protopad');
        my ($pp, $op) = $self->_popn(2);
        $self->_push('IP6', "Kernel.MakeSub(new DynBlockDelegate(" .
            $body->csname . "), $pp, $op)");
    }

    sub proto_var {
        my ($self, $name) = @_;
        $self->peek_let('protopad');
        my ($pv, $pp) = $self->_popn(2);
        $self->_emit("$pp.lex[" . qm($name) . "] = ($pv)");
    }

    # These are completely derived.

    sub scopelex {
        my ($self, $name, $set) = @_;
        my $body = $self->body // $self->bodies->[-1];
        my $order = 0;
        if ($self->letdepths->{$name}) {
            $name = "let!${name}!" . ($self->letdepths->{$name} - 1);
        } else {
            $order = $body->lex_level($name);
            if ($order < 0) {
                #print STDERR YAML::XS::Dump ($body);
                die "Internal error: failed to resolve lexical $name in " . $body->name;
            }
        }
        if ($set) {
            $self->lexput($order, $name);
        } else {
            $self->lexget($order, $name);
        }
    }

    sub open_protopad {
        my ($self, $body) = @_;
        $self->peek_let('protopad');
        $self->clr_new('Frame', 1);
        $self->push_let('protopad');
        push @{ $self->bodies }, $body;
    }

    ###

    sub write {
        my ($self) = @_;
        my $name = $self->csname;
        my $vis  = ($self->entry ? 'public' : 'private');
        print ::NIECZA_OUT " " x 4, "$vis static Frame $name(Frame th) {\n";
        print ::NIECZA_OUT " " x 8, "if (Kernel.TraceCont) { Console.WriteLine(\"Entering $::UNITNAME : $name @ \" + th.ip); }\n";
        print ::NIECZA_OUT " " x 8, "switch (th.ip) {\n";
        print ::NIECZA_OUT " " x 12, "case 0:\n";
        print ::NIECZA_OUT " " x 12, $_ for @{ $self->buffer };
        print ::NIECZA_OUT " " x 12, "default:\n";
        print ::NIECZA_OUT " " x 16, "throw new Exception(\"Invalid IP\");\n";
        print ::NIECZA_OUT " " x 8, "}\n";
        print ::NIECZA_OUT " " x 4, "}\n";
        if ($name ne 'BOOT') {
            print ::NIECZA_OUT " " x 4, "private static int[] ${name}_lines = {",
                join (", ", map { ($_ // 0) } @{ $self->lineinfo }), "};\n";
            print ::NIECZA_OUT " " x 4, "private static string[] ${name}_files = {",
                join (", ", map { qm($_ // "") } @{ $self->fileinfo }), "};\n";
        }
    }

    sub BUILD {
        my $self = shift;
        $self->ops->var_cg($self);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
