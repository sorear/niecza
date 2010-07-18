use strict;
use warnings;
use 5.010;

{
    package CodeGen;
    use Moose;

    # Beta will do this using reflection
    my %typedata = (
        IP6 =>
            { HOW          => [c => 'IP6'] },
        DynObject =>
            { klass        => [f => 'DynMetaObject'],
              slots        => [f => 'Dictionary<string,Object>'] },

        DynMetaObject =>
            { BuildC3MRO   => [m => 'Void'],
              typeObject   => [f => 'IP6'],
              how          => [f => 'IP6'],
              local        => [f => 'Dictionary<string,IP6>'],
              superclasses => [f => 'List<DynMetaObject>'],
              name         => [f => 'String'] },

        'List<DynMetaObject>' =>
            { Add          => [m => 'Void'] },
        'Double' =>
            { ToString     => [m => 'String'] },
        'Variable' =>
            { lv           => [f => 'LValue'] },
        'CLRImportObject' =>
            { val          => [f => 'Object'] },

        'Kernel.Bind'          => [c => 'Void'],
        'Kernel.Assign'        => [c => 'Void'],
        'Kernel.Fetch'         => [c => 'IP6'],
        'Kernel.NewROScalar'   => [m => 'Variable'],
        'Kernel.NewRWScalar'   => [m => 'Variable'],
        'Kernel.NewRWListVar'  => [m => 'Variable'],
        'Kernel.NewWeakScalar' => [m => 'Variable'],
        'Kernel.NewCaptureVar' => [m => 'Variable'],
        'Console.WriteLine'    => [m => 'Void'],
        'String.Concat'        => [m => 'String'],
        'Kernel.SubMO'         => [f => 'DynMetaObject'],
        'Kernel.ScalarMO'      => [f => 'DynMetaObject'],
        'Kernel.MainlineContinuation' => [f => 'DynBlockDelegate'],
        'Kernel.MakeSub'       => [m => 'IP6'],
        'Kernel.BoxAny'        => [m => 'Variable'],
        'Kernel.UnboxAny'      => [m => 'object'],
    );

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
    sub _savestackstate {
        my ($self, $lbl) = @_;
        my %save;
        $save{depth} = $self->depth;
        $save{stacktype} = [ @{ $self->stacktype } ];
        $save{auxdepths} = { %{ $self->auxdepths } };
        $self->savedstks->{$lbl} = \%save;
    }

    sub _restorestackstate {
        my ($self, $lbl) = @_;
        my $save = $self->savedstks->{$lbl};
        $self->depth($save->{depth});
        $self->savedepth($save->{depth});
        @{ $self->stacktype } = @{ $save->{stacktype} };
        %{ $self->auxdepths } = %{ $save->{auxdepths} };
    }

    sub _emit {
        my ($self, $line) = @_;
        #push @{ $self->buffer }, sprintf "    // d=%d md=%d sd=%d nl=%d\n",
        #    $self->depth, $self->maxdepth, $self->savedepth, $self->numlabels;
        push @{ $self->buffer }, "    $line;\n";
    }

    sub _undercheck {
        my ($self, $margin) = @_;
        Carp::confess "Stack underflow" if $margin > $self->depth;
        if ($self->depth - $margin < $self->savedepth) {
            for my $n ($self->depth - $margin .. $self->savedepth - 1) {
                $self->_emit("s$n = th.lex[\"s$n\"]");
            }
            $self->savedepth($self->depth - $margin);
        }
    }

    sub _overcheck {
        my ($self, $margin) = @_;
        if ($self->depth + $margin > $self->maxdepth) {
            $self->maxdepth($self->depth + $margin);
        }
    }

    sub _peek {
        my ($self) = @_;
        $self->_undercheck(1);
        my $ty = @{ $self->stacktype }[-1];
        return "(($ty)s" . ($self->depth - 1) . ")";
    }

    sub _pop {
        my ($self) = @_;
        $self->_undercheck(1);
        my $ty = pop @{ $self->stacktype };
        $self->depth($self->depth - 1);
        return "(($ty)s" . ($self->depth) . ")";
    }

    sub _push {
        my ($self, $ty, $expr) = @_;
        $self->_overcheck(1);
        my $n = $self->depth;
        $self->_emit("s$n = $expr");
        $self->depth($n + 1);
        Carp::confess('Untyped push') unless defined $ty;
        push @{ $self->stacktype }, $ty;
    }

    sub _saveall {
        my ($self) = @_;
        for my $i ($self->savedepth .. $self->depth - 1) {
            $self->_emit("th.lex[\"s$i\"] = s$i");
        }
        $self->savedepth($self->depth);
    }

    sub _cpscall {
        my ($self, $rt, $expr) = @_;
        $self->_saveall;
        my $n = $self->label;
        $self->_emit("th.resultSlot = null");
        $self->_emit("th.ip = $n");
        $self->_emit("return $expr");
        push @{ $self->buffer }, "case $n:\n";
        $self->_push($rt, 'th.resultSlot') if defined $rt;
    }

    # These functions are usable from user code, but still depend on volatiles.

    sub swap {
        my ($self) = @_;
        $self->_undercheck(2);
        $self->_overcheck(1);
        my $n = $self->depth;

        $self->_emit(sprintf "s%d = s%d", $n, $n-2);
        $self->_emit(sprintf "s%d = s%d", $n-2, $n-1);
        $self->_emit(sprintf "s%d = s%d", $n-1, $n);
        @{ $self->stacktype }[-1,-2] = @{ $self->stacktype }[-2,-1];
    }

    sub new_aux {
        my ($self, $name, $type) = @_;
        $self->auxdepths->{$name} = 0;
        $self->auxtypes->{$name} = $type;
    }

    sub push_aux {
        my ($self, $which) = @_;
        my $var = "aux!${which}!" . ($self->auxdepths->{$which}++);
        $self->lextypes($var, $self->auxtypes->{$which});
        $self->rawlexput($var);
    }

    sub pop_aux {
        my ($self, $which) = @_;
        my $var = "aux!${which}!" . (--$self->auxdepths->{$which});
        $self->rawlexget($var);
    }

    sub peek_aux {
        my ($self, $which) = @_;
        my $var = "aux!${which}!" . ($self->auxdepths->{$which} - 1);
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
        $self->_saveall;
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        $self->_restorestackstate($n) if $self->savedstks->{$n};
        push @{ $self->buffer }, "    goto case $n;\n" unless $self->unreach;
        push @{ $self->buffer }, "case $n:\n";
        $self->unreach(0);
    }

    sub goto {
        my ($self, $n) = @_;
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        $self->_saveall;
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    goto case $n;\n";
        $self->unreach(1);
    }

    sub cgoto {
        my ($self, $n) = @_;
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        my $top = $self->_pop;
        $self->_saveall;
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    if ($top) { goto case $n; }\n";
    }

    sub ncgoto {
        my ($self, $n) = @_;
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        my $top = $self->_pop;
        $self->_saveall;
        $self->_savestackstate($n);
        push @{ $self->buffer }, "    if (!$top) { goto case $n; }\n";
    }

    sub lextypes {
        my ($self, %args) = @_;
        #say STDERR "lextypes: @args";
        my $body = $self->body // $self->bodies->[-1];
        if ($body) {
            for (keys %args) { $body->lexical->{$_} = 1 }
        }
        %{ $self->lex2type } = (%{ $self->lex2type }, %args);
    }

    sub rawlexget {
        my ($self, $name) = @_;
        $self->_push($self->lex2type->{$name}, "th.lex[" . qm($name) . "]");
    }

    sub rawlexput {
        my ($self, $name) = @_;
        $self->_emit("th.lex[" . qm($name) . "] = " . $self->_pop);
    }

    sub lexget {
        my ($self, $order, $name) = @_;
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

    sub lexput {
        my ($self, $order, $name) = @_;
        my $frame = 'th.';
        if ($self->auxdepths->{'protopad'}) {
            $frame = '((Frame)th.lex[' .
                qm('aux!protopad!' . ($self->auxdepths->{'protopad'} - 1)) .
                ']).';
        }
        $self->_emit($frame . ("outer." x $order) . "lex[" . qm($name) . "] = " . $self->_pop);
    }

    sub callframe {
        my ($self) = @_;
        my $frame = 'th';
        if ($self->auxdepths->{'protopad'}) {
            $frame = '((Frame)th.lex[' .
                qm('aux!protopad!' . ($self->auxdepths->{'protopad'} - 1)) .
                '])';
        }
        $self->_push("Frame", $frame);
    }

    sub dup {
        my ($self) = @_;
        my $c = $self->_peek;
        $self->_push($self->stacktype->[-1], $c);
    }

    sub drop {
        my ($self) = @_;
        $self->_pop;
    }

    # the use of scalar here is a little bit wrong; semantically it's closer
    # to the old notion of ¢foo.  doesn't matter much since it's not exposed
    # at the Perl 6 level.
    sub pos {
        my ($self, $num) = @_;
        $self->_push('Variable',
            "new Variable(false, Variable.Context.Scalar, th.pos[$num])");
    }

    sub protolget {
        my ($self, $name) = @_;
        $self->_push('Variable', "th.proto.lex[" . qm($name) . "]");
    }

    sub call_method {
        my ($self, $nv, $name, $numargs) = @_;
        my @args = reverse map { $self->_pop } (1 .. $numargs + 1);  # invocant LV
        my $inv = $self->_pop;
        $self->_cpscall(($nv ? 'Variable' : undef), "$inv.InvokeMethod(th, " . qm($name) . ", new LValue[" . scalar(@args) . "] { " . join(", ", map { "$_.lv" } @args) . " }, null)");
    }

    sub call_sub {
        my ($self, $nv, $numargs) = @_;
        my @args = reverse map { $self->_pop } (1 .. $numargs);
        my $inv = $self->_pop;
        $self->_cpscall(($nv ? 'Variable' : undef), "$inv.Invoke(th, new LValue[" . scalar(@args) . "] { " . join(", ", map { "$_.lv" } @args) . " }, null)");
    }

    sub tail_call_sub {
        my ($self, $numargs) = @_;
        my @args = reverse map { $self->_pop } (1 .. $numargs);
        my $inv = $self->_pop;
        $self->_emit("return $inv.Invoke(th.caller, new LValue[" . scalar(@args) . "] { " . join(", ", map { "$_.lv" } @args) . " }, null)");
        $self->unreach(1);
    }

    sub clr_bool {
        my ($self, $v) = @_;
        $self->_push('System.Boolean', $v ? 'true' : 'false');
    }

    sub clr_new {
        my ($self, $class, $nargs) = @_;
        my @args = reverse map { $self->_pop } 1 .. $nargs;
        $self->_push($class, "new $class(" . join(", ", @args) . ")");
    }

    sub clr_string {
        my ($self, $text) = @_;
        $self->_push('System.String', qm($text));
    }

    sub clr_int {
        my ($self, $val) = @_;
        $self->_push('System.Int32', $val);
    }

    sub clr_double {
        my ($self, $val) = @_;
        $self->_push('System.Double', "((Double)$val)");
    }

    sub clr_arith {
        my ($self, $op) = @_;
        my $ty = $self->stacktype->[-1];
        if ($ty ne $self->stacktype->[-2]) {
            die "Overloaded operations not yet supported";
        }
        my $a2 = $self->_pop;
        my $a1 = $self->_pop;
        $self->_push($ty, "$a1 $op $a2");
    }

    sub clr_compare {
        my ($self, $op) = @_;
        my $a2 = $self->_pop;
        my $a1 = $self->_pop;
        $self->_push('Boolean', "$a1 $op $a2");
    }

    sub clr_field_get {
        my ($self, $f) = @_;
        my $ty = $typedata{$self->stacktype->[-1]}{$f}[1];
        my $obj = $self->_pop;
        $self->_push($ty, "$obj.$f");
    }

    sub clr_field_set {
        my ($self, $f) = @_;
        my $val = $self->_pop;
        my $obj = $self->_pop;
        $self->_emit("$obj.$f = $val");
    }

    sub clr_sfield_get {
        my ($self, $f) = @_;
        my $ty = $typedata{$f}[1];
        $self->_push($ty, "$f");
    }

    sub clr_sfield_set {
        my ($self, $f) = @_;
        my $val = $self->_pop;
        $self->_emit("$f = $val");
    }

    sub attr_var {
        my ($self, $f) = @_;
        my $obj = $self->_pop;
        $self->_cpscall('Variable', "$obj.GetAttribute(th, " . qm($f) . ")");
    }

    sub clr_index_get {
        my ($self, $f) = @_;
        if ($f) {
            $self->clr_string($f);
        }
        my $ix  = $self->_pop;
        $self->stacktype->[-1] =~ /Dictionary<.*,(.*)>/
            or die "Type inference needs more hacks";
        my $ty = $1;
        my $obj = $self->_pop;
        $self->_push($ty, "$obj" . "[$ix]");
    }

    sub clr_index_set {
        my ($self, $f) = @_;
        my $val = $self->_pop;
        my $ix  = $self->_pop unless $f;
        my $obj = $self->_pop;
        $self->_emit("$obj" . "[" . ($f ? qm($f) : $ix) . "] = $val");
    }

    sub cast {
        my ($self, $type) = @_;
        $self->stacktype->[-1] = $type;
    }

    sub clr_call_direct {
        my ($self, $name, $nargs) = @_;
        my $rt = $typedata{$name};
        my @args = reverse map { $self->_pop } 1 .. $nargs;
        if ($rt->[0] eq 'c') {
            $self->_cpscall(($rt->[1] eq 'Void' ? undef : $rt->[1]),
                "$name(" . join(", ", "th", @args) . ")");
        } elsif ($rt->[1] ne 'Void') {
            $self->_push($rt->[1], "$name(" . join(", ", @args) . ")");
        } else {
            $self->_emit("$name(" . join(", ", @args) . ")");
        }
    }

    sub clr_call_virt {
        my ($self, $name, $nargs) = @_;
        my @args = reverse map { $self->_pop } 1 .. $nargs;
        my $rt = $typedata{$self->stacktype->[-1]}{$name};
        my $inv = $self->_pop;
        if ($rt->[0] eq 'c') {
            $self->_cpscall(($rt->[1] eq 'Void' ? undef : $rt->[1]),
                "$inv.$name(" . join(", ", "th", @args) . ")");
        } elsif ($rt->[1] ne 'Void') {
            $self->_push($rt->[1], "$inv.$name(" . join(", ", @args) . ")");
        } else {
            $self->_emit("$inv.$name(" . join(", ", @args) . ")");
        }
    }

    sub return {
        my ($self, $nv) = @_;
        return if $self->unreach;
        if ($nv) {
            $self->_emit("th.caller.resultSlot = " . $self->_pop);
        }
        $self->_emit("return th.caller");
        $self->unreach(1);
    }

    sub push_null {
        my ($self, $ty) = @_;
        $self->_push($ty, "null");
    }

    sub close_sub {
        my ($self, $bodycg) = @_;
        $self->pop_aux('protopad');
        pop @{ $self->bodies };
        $self->peek_aux('protopad');
        my $op = $self->_pop;
        my $pp = $self->_pop;
        $self->_push('IP6', "Kernel.MakeSub(new DynBlockDelegate(" .
            $bodycg->csname . "), $pp, $op)");
    }

    sub proto_var {
        my ($self, $name) = @_;
        $self->peek_aux('protopad');
        my $pp = $self->_pop;
        my $pv = $self->_pop;
        $self->_emit("$pp.lex[" . qm($name) . "] = $pv");
    }

    # These are completely derived.

    sub scopelex {
        my ($self, $name, $set) = @_;
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

    sub open_protopad {
        my ($self, $body) = @_;
        $self->peek_aux('protopad');
        $self->clr_new('Frame', 1);
        $self->push_aux('protopad');
        push @{ $self->bodies }, $body;
    }

    ###

    sub csname {
        my ($self) = @_;
        return $self->name if $self->entry;
        my @name = split /\W+/, $self->name;
        shift @name if @name && $name[0] eq '';
        join("", (map { ucfirst $_ } @name), "_", $self->uid, "C");
    }

    sub write {
        my ($self) = @_;
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

    sub BUILD {
        my $self = shift;
        #say STDERR YAML::XS::Dump($self->ops);
        $self->ops->var_cg($self);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
