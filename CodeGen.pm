use 5.010;
use MooseX::Declare;

class CodeGen {
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
              superclasses => [f => 'List<DynMetaObject>'],
              name         => [f => 'String'] },

        'List<DynMetaObject>' =>
            { Add          => [m => 'Void'] },
        'List<Variable>' =>
            { Add          => [m => 'Void'],
              Insert       => [m => 'Void'],
              RemoveAt     => [m => 'Void'],
              Count        => [f => 'Int32'] },
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

        'Kernel.SlurpyHelper'  => [c => 'List<Variable>'],
        'Kernel.Bind'          => [c => 'Void'],
        'Kernel.Assign'        => [c => 'Void'],
        'Kernel.Fetch'         => [c => 'IP6'],
        'Kernel.NewROScalar'   => [m => 'Variable'],
        'Kernel.NewRWScalar'   => [m => 'Variable'],
        'Kernel.NewRWListVar'  => [m => 'Variable'],
        'Console.WriteLine'    => [m => 'Void'],
        'Console.Error.WriteLine'    => [m => 'Void'],
        'Environment.Exit'     => [m => 'Void'],
        'String.Concat'        => [m => 'String'],
        'Kernel.SubMO'         => [f => 'DynMetaObject'],
        'Kernel.ScalarMO'      => [f => 'DynMetaObject'],
        'Kernel.MainlineContinuation' => [f => 'DynBlockDelegate'],
        'Kernel.MakeSub'       => [m => 'IP6'],
        'Kernel.BoxAny'        => [m => 'Variable'],
        'Kernel.UnboxAny'      => [m => 'object'],
    );

    method _typedata ($types, @path) {
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

    has name      => (isa => 'Str', is => 'ro');
    has uid       => (isa => 'Int', is => 'ro', default => sub { ++(state $i) });
    has entry     => (isa => 'Bool', is => 'ro', default => 0);
    has depth     => (isa => 'Int', is => 'rw', default => 0);
    has maxdepth  => (isa => 'Int', is => 'rw', default => 0);
    has savedepth => (isa => 'Int', is => 'rw', default => 0);
    has numlabels => (isa => 'Int', is => 'rw', default => 1);
    has stacktype => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has labelname => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has lex2type  => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has buffer    => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has unreach   => (isa => 'Bool', is => 'rw', default => 0);

    has auxdepths => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has auxtypes  => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has body      => (isa => 'Body', is => 'ro');
    has bodies    => (isa => 'ArrayRef', is => 'ro', default => sub { [] });

    has savedstks => (isa => 'HashRef', is => 'ro', default => sub { +{} });

    has ops => (is => 'ro', required => 1);

    # These are the sub-primitives.  Their very inteface exposes volatile
    # details of the codegen.

    sub qm { "\"" . $_[0] . "\"" }

    # always called after _saveall
    method _savestackstate ($lbl) {
        my %save;
        $save{depth} = $self->depth;
        $save{stacktype} = [ @{ $self->stacktype } ];
        $save{auxdepths} = { %{ $self->auxdepths } };
        $self->savedstks->{$lbl} = \%save;
    }

    method _restorestackstate ($lbl) {
        my $save = $self->savedstks->{$lbl};
        $self->depth($save->{depth});
        $self->savedepth($save->{depth});
        @{ $self->stacktype } = @{ $save->{stacktype} };
        %{ $self->auxdepths } = %{ $save->{auxdepths} };
    }

    method _emit ($line) {
        #push @{ $self->buffer }, sprintf "    // d=%d md=%d sd=%d nl=%d\n",
        #    $self->depth, $self->maxdepth, $self->savedepth, $self->numlabels;
        push @{ $self->buffer }, "    $line;\n";
    }

    method _undercheck ($margin) {
        Carp::confess "Stack underflow" if $margin > $self->depth;
        if ($self->depth - $margin < $self->savedepth) {
            for my $n ($self->depth - $margin .. $self->savedepth - 1) {
                $self->_emit("s$n = th.lex[\"s$n\"]");
            }
            $self->savedepth($self->depth - $margin);
        }
    }

    method _overcheck ($margin) {
        if ($self->depth + $margin > $self->maxdepth) {
            $self->maxdepth($self->depth + $margin);
        }
    }

    method _peek () {
        $self->_undercheck(1);
        my $ty = @{ $self->stacktype }[-1];
        return "(($ty)s" . ($self->depth - 1) . ")";
    }

    method _pop () {
        $self->_undercheck(1);
        my $ty = pop @{ $self->stacktype };
        $self->depth($self->depth - 1);
        return "(($ty)s" . ($self->depth) . ")";
    }

    method _push ($ty, $expr) {
        $self->_overcheck(1);
        my $n = $self->depth;
        $self->_emit("s$n = $expr");
        $self->depth($n + 1);
        Carp::confess('Untyped push') unless defined $ty;
        push @{ $self->stacktype }, $ty;
    }

    method _saveall () {
        for my $i ($self->savedepth .. $self->depth - 1) {
            $self->_emit("th.lex[\"s$i\"] = s$i");
        }
        $self->savedepth($self->depth);
    }

    method _cpscall ($rt, $expr) {
        $self->_saveall;
        my $n = $self->label;
        $self->_emit("th.resultSlot = null");
        $self->_emit("th.ip = $n");
        $self->_emit("return $expr");
        push @{ $self->buffer }, "case $n:\n";
        $self->_push($rt, 'th.resultSlot') if defined $rt;
    }

    # These functions are usable from user code, but still depend on volatiles.

    method swap () {
        $self->_undercheck(2);
        $self->_overcheck(1);
        my $n = $self->depth;

        $self->_emit(sprintf "s%d = s%d", $n, $n-2);
        $self->_emit(sprintf "s%d = s%d", $n-2, $n-1);
        $self->_emit(sprintf "s%d = s%d", $n-1, $n);
        @{ $self->stacktype }[-1,-2] = @{ $self->stacktype }[-2,-1];
    }

    method new_aux ($name, $type) {
        $self->auxdepths->{$name} = 0;
        $self->auxtypes->{$name} = $type;
    }

    method push_aux ($which) {
        my $var = "aux!${which}!" . ($self->auxdepths->{$which}++);
        $self->lextypes($var, $self->auxtypes->{$which});
        $self->rawlexput($var);
    }

    method pop_aux ($which) {
        my $var = "aux!${which}!" . (--$self->auxdepths->{$which});
        $self->rawlexget($var);
    }

    method peek_aux ($which) {
        my $var = "aux!${which}!" . ($self->auxdepths->{$which} - 1);
        $self->rawlexget($var);
    }

    method label () {
        my $n = $self->numlabels;
        $self->numlabels($n + 1);
        return $n;
    }

    method labelhere ($n) {
        $self->_saveall;
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        $self->_restorestackstate($n) if $self->savedstks->{$n};
        push @{ $self->buffer }, "    goto case $n;\n" unless $self->unreach;
        push @{ $self->buffer }, "case $n:\n";
        $self->unreach(0);
    }

    method goto ($n) {
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        $self->_saveall;
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    goto case $n;\n";
        $self->unreach(1);
    }

    method cgoto ($n) {
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        my $top = $self->_pop;
        $self->_saveall;
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    if ($top) { goto case $n; }\n";
    }

    method ncgoto ($n) {
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        my $top = $self->_pop;
        $self->_saveall;
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    if (!$top) { goto case $n; }\n";
    }

    method lextypes (%args) {
        #say STDERR "lextypes: @args";
        my $body = $self->body // $self->bodies->[-1];
        if ($body) {
            for (keys %args) { $body->lexical->{$_} = 1 }
        }
        %{ $self->lex2type } = (%{ $self->lex2type }, %args);
    }

    method rawlexget ($name, @) {
        $self->_push($self->lex2type->{$name}, "th.lex[" . qm($name) . "]");
    }

    method rawlexput ($name, @) {
        $self->_emit("th.lex[" . qm($name) . "] = " . $self->_pop);
    }

    method lexget ($order, $name) {
        my $frame = 'th.';
        if ($self->auxdepths->{'protopad'}) {
            $frame = '((Frame)th.lex[' .
                qm('aux!protopad!' . ($self->auxdepths->{'protopad'} - 1)) .
                ']).';
        }
        # XXX need a better type tracking system
        $self->_push(($order ? 'Variable' : ($self->lex2type->{$name} // 'Variable')),
            $frame . ("outer." x $order) . "lex[" . qm($name) . "]");
    }

    method lexput ($order, $name) {
        my $frame = 'th.';
        if ($self->auxdepths->{'protopad'}) {
            $frame = '((Frame)th.lex[' .
                qm('aux!protopad!' . ($self->auxdepths->{'protopad'} - 1)) .
                ']).';
        }
        $self->_emit($frame . ("outer." x $order) . "lex[" . qm($name) . "] = " . $self->_pop);
    }

    method callframe () {
        my $frame = 'th';
        if ($self->auxdepths->{'protopad'}) {
            $frame = '((Frame)th.lex[' .
                qm('aux!protopad!' . ($self->auxdepths->{'protopad'} - 1)) .
                '])';
        }
        $self->_push("Frame", $frame);
    }

    method dup () {
        my $c = $self->_peek;
        $self->_push($self->stacktype->[-1], $c);
    }

    method drop () {
        $self->_pop;
    }

    # the use of scalar here is a little bit wrong; semantically it's closer
    # to the old notion of ¢foo.  doesn't matter much since it's not exposed
    # at the Perl 6 level.
    method pos ($num) {
        $self->_push('Variable',
            "new Variable(false, Variable.Context.Scalar, th.pos[$num])");
    }

    method protolget ($name) {
        $self->_push('Variable', "th.proto.lex[" . qm($name) . "]");
    }

    method call_method ($nv, $name, $numargs) {
        my @args = reverse map { $self->_pop } (1 .. $numargs + 1);  # invocant LV
        my $inv = $self->_pop;
        $self->_cpscall(($nv ? 'Variable' : undef), "$inv.InvokeMethod(th, " . qm($name) . ", new LValue[" . scalar(@args) . "] { " . join(", ", map { "$_.lv" } @args) . " }, null)");
    }

    method call_sub ($nv, $numargs) {
        my @args = reverse map { $self->_pop } (1 .. $numargs);
        my $inv = $self->_pop;
        $self->_cpscall(($nv ? 'Variable' : undef), "$inv.Invoke(th, new LValue[" . scalar(@args) . "] { " . join(", ", map { "$_.lv" } @args) . " }, null)");
    }

    method tail_call_sub ($numargs) {
        my @args = reverse map { $self->_pop } (1 .. $numargs);
        my $inv = $self->_pop;
        $self->_emit("return $inv.Invoke(th.caller, new LValue[" . scalar(@args) . "] { " . join(", ", map { "$_.lv" } @args) . " }, null)");
        $self->unreach(1);
    }

    method clr_bool ($v) {
        $self->_push('System.Boolean', $v ? 'true' : 'false');
    }

    method clr_new ($class, $nargs) {
        my @args = reverse map { $self->_pop } 1 .. $nargs;
        $self->_push($class, "new $class(" . join(", ", @args) . ")");
    }

    method clr_string ($text) {
        $self->_push('System.String', qm($text));
    }

    method clr_int ($val) {
        $self->_push('System.Int32', $val);
    }

    method clr_double ($val) {
        $self->_push('System.Double', "((Double)$val)");
    }

    method clr_arith ($op) {
        my $ty = $self->stacktype->[-1];
        if ($ty ne $self->stacktype->[-2]) {
            die "Overloaded operations not yet supported";
        }
        my $a2 = $self->_pop;
        my $a1 = $self->_pop;
        $self->_push($ty, "$a1 $op $a2");
    }

    method clr_compare ($op) {
        my $a2 = $self->_pop;
        my $a1 = $self->_pop;
        $self->_push('Boolean', "$a1 $op $a2");
    }

    method clr_field_get ($f) {
        my $ty = $self->_typedata('f', $self->stacktype->[-1], $f);
        my $obj = $self->_pop;
        $self->_push($ty, "$obj.$f");
    }

    method clr_field_set ($f) {
        my $val = $self->_pop;
        my $obj = $self->_pop;
        $self->_emit("$obj.$f = $val");
    }

    method clr_sfield_get ($f) {
        my $ty = $self->_typedata('f', $f);
        $self->_push($ty, "$f");
    }

    method clr_sfield_set ($f) {
        my $val = $self->_pop;
        $self->_emit("$f = $val");
    }

    method attr_var ($f) {
        my $obj = $self->_pop;
        $self->_cpscall('Variable', "$obj.GetAttribute(th, " . qm($f) . ")");
    }

    method clr_index_get ($f?) {
        if ($f) {
            $self->clr_string($f);
        }
        my $ix  = $self->_pop;
        my $oty = $self->stacktype->[-1];
        my $ty  = ($oty =~ /^Dictionary<.*,(.*)>$/) ? $1 :
                  ($oty =~ /^(.*)\[\]$/) ? $1 :
                  ($oty =~ /^List<(.*)>$/) ? $1 :
                  die "type inference needs more hacks";
        my $obj = $self->_pop;
        $self->_push($ty, "$obj" . "[$ix]");
    }

    method clr_index_set ($f?) {
        my $val = $self->_pop;
        my $ix  = $self->_pop unless $f;
        my $obj = $self->_pop;
        $self->_emit("$obj" . "[" . ($f ? qm($f) : $ix) . "] = $val");
    }

    method cast ($type) {
        $self->_push($type, "(($type)" . $self->_pop . ")");
    }

    method clr_call_direct ($name, $nargs) {
        my ($cl, $rt) = $self->_typedata('cm', $name);
        my @args = reverse map { $self->_pop } 1 .. $nargs;
        if ($cl eq 'c') {
            $self->_cpscall(($rt eq 'Void' ? undef : $rt),
                "$name(" . join(", ", "th", @args) . ")");
        } elsif ($rt ne 'Void') {
            $self->_push($rt, "$name(" . join(", ", @args) . ")");
        } else {
            $self->_emit("$name(" . join(", ", @args) . ")");
        }
    }

    method clr_call_virt ($name, $nargs) {
        my @args = reverse map { $self->_pop } 1 .. $nargs;
        my ($cl, $rt) = $self->_typedata('cm', $self->stacktype->[-1], $name);
        my $inv = $self->_pop;
        if ($cl eq 'c') {
            $self->_cpscall(($rt eq 'Void' ? undef : $rt),
                "$inv.$name(" . join(", ", "th", @args) . ")");
        } elsif ($rt ne 'Void') {
            $self->_push($rt, "$inv.$name(" . join(", ", @args) . ")");
        } else {
            $self->_emit("$inv.$name(" . join(", ", @args) . ")");
        }
    }

    method return ($nv) {
        return if $self->unreach;
        if ($nv) {
            $self->_emit("th.caller.resultSlot = " . $self->_pop);
        }
        $self->_emit("return th.caller");
        $self->unreach(1);
    }

    method push_null ($ty) {
        $self->_push($ty, "null");
    }

    method close_sub ($bodycg) {
        $self->pop_aux('protopad');
        pop @{ $self->bodies };
        $self->peek_aux('protopad');
        my $op = $self->_pop;
        my $pp = $self->_pop;
        $self->_push('IP6', "Kernel.MakeSub(new DynBlockDelegate(" .
            $bodycg->csname . "), $pp, $op)");
    }

    method proto_var ($name) {
        $self->peek_aux('protopad');
        my $pp = $self->_pop;
        my $pv = $self->_pop;
        $self->_emit("$pp.lex[" . qm($name) . "] = $pv");
    }

    # These are completely derived.

    method scopelex ($name, $set) {
        my $body = $self->body // $self->bodies->[-1];
        my ($order, $scope) = (0, $body);
        while ($scope && !$scope->lexical->{$name}) {
            $scope = $scope->outer;
            $order++;
        }
        if (!$scope) {
            die "Failed to resolve lexical $name in " . $body->name;
        }
        if ($set) {
            $self->lexput($order, $name);
        } else {
            $self->lexget($order, $name);
        }
    }

    method open_protopad ($body) {
        $self->peek_aux('protopad');
        $self->clr_new('Frame', 1);
        $self->push_aux('protopad');
        push @{ $self->bodies }, $body;
    }

    ###

    method csname () {
        return $self->name if $self->entry;
        my @name = split /\W+/, $self->name;
        shift @name if @name && $name[0] eq '';
        join("", (map { ucfirst $_ } @name), "_", $self->uid, "C");
    }

    method write () {
        my $name = $self->csname;
        my $vis  = ($self->entry ? 'public' : 'private');
        print ::NIECZA_OUT " " x 4, "$vis static Frame $name(Frame th) {\n";
        print ::NIECZA_OUT " " x 8, "if (Kernel.TraceCont) { Console.WriteLine(\"Entering $name @ \" + th.ip); }\n";
        if ($self->maxdepth) {
            print ::NIECZA_OUT " " x 8, "object " . join(", ", map { "s$_" }
                0 .. ($self->maxdepth - 1)) . ";\n";
        }
        print ::NIECZA_OUT " " x 8, "switch (th.ip) {\n";
        print ::NIECZA_OUT " " x 12, "case 0:\n";
        print ::NIECZA_OUT " " x 12, $_ for @{ $self->buffer };
        print ::NIECZA_OUT " " x 12, "default:\n";
        print ::NIECZA_OUT " " x 16, "throw new Exception(\"Invalid IP\");\n";
        print ::NIECZA_OUT " " x 8, "}\n";
        print ::NIECZA_OUT " " x 4, "}\n";
    }

    method BUILD {
        #say STDERR YAML::XS::Dump($self->ops);
        $self->ops->var_cg($self);
    }
}

1;
