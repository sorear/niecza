use strict;
use warnings;
use 5.010;

{
    package CodeGen;
    use Moose;

    # Beta will do this using reflection
    my %typedata = (
        DynObject     => { klass        => 'DynMetaObject',
                           slots        => 'Dictionary<string,Object>' },
        Console       => { WriteLine    => 'Void' },
        DynMetaObject => { how          => 'IP6',
                           methods      => 'Dictionary<String,IP6>',
                           name         => 'String' },
        'Kernel.MakeROVar'    => 'Variable',
        'Kernel.MakeRWVar'    => 'Variable',
        'Kernel.MakeROLValue' => 'LValue',
        'Kernel.MakeRWLValue' => 'LValue'
    );

    has name      => (isa => 'Str', is => 'ro');
    has uid       => (isa => 'Int', is => 'ro', default => sub { ++(state $i) });
    has depth     => (isa => 'Int', is => 'rw', default => 0);
    has maxdepth  => (isa => 'Int', is => 'rw', default => 0);
    has savedepth => (isa => 'Int', is => 'rw', default => 0);
    has numlabels => (isa => 'Int', is => 'rw', default => 1);
    has stacktype => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has labelname => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has lex2type  => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has buffer    => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has unreach   => (isa => 'Bool', is => 'rw', default => 0);

    sub qm { "\"" . $_[0] . "\"" }

    sub _emit {
        my ($self, $line) = @_;
        #push @{ $self->buffer }, sprintf "    // d=%d md=%d sd=%d nl=%d\n",
        #    $self->depth, $self->maxdepth, $self->savedepth, $self->numlabels;
        push @{ $self->buffer }, "    $line;\n";
    }

    sub _undercheck {
        my ($self, $margin) = @_;
        die "Stack underflow" if $margin > $self->depth;
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

    sub _swap {
        my ($self) = @_;
        $self->_undercheck(2);
        $self->_overcheck(1);
        my $n = $self->depth;

        $self->_emit(sprintf "s%d = s%d", $n, $n-2);
        $self->_emit(sprintf "s%d = s%d", $n-2, $n-1);
        $self->_emit(sprintf "s%d = s%d", $n-1, $n);
        @{ $self->stacktype }[-1,-2] = @{ $self->stacktype }[-2,-1];
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
        warn unless defined $ty;
        push @{ $self->stacktype }, $ty;
    }

    sub _saveall {
        my ($self) = @_;
        for my $i ($self->savedepth .. $self->depth - 1) {
            $self->_emit("th.lex[\"s$i\"] = s$i");
        }
        $self->savedepth($self->depth);
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
        push @{ $self->buffer }, "    goto case $n;\n" unless $self->unreach;
        push @{ $self->buffer }, "case $n:\n";
        $self->unreach(0);
    }

    sub goto {
        my ($self, $n) = @_;
        $n = ($self->labelname->{$n} //= $self->label) if $n < 0;
        $self->_saveall;
        push @{ $self->buffer }, "    goto case $n;\n";
        $self->unreach(1);
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

    sub lex_lv {
        my ($self, $order, $name) = @_;
        $self->_push("LValue", "((Variable)th." . ("outer." x $order) . "lex[" . qm($name) . "]).lv");
    }

    sub lextypes {
        my ($self, @args) = @_;
        %{ $self->lex2type } = (%{ $self->lex2type }, @args);
    }

    sub rawlexget {
        my ($self, $order, $name) = @_;
        $self->_push($self->lex2type->{$name}, "th." . ("outer." x $order) . "lex[" . qm($name) . "]");
    }

    sub rawlexput {
        my ($self, $order, $name) = @_;
        $self->_emit("th." . ("outer." x $order) . "lex[" . qm($name) . "] = " . $self->_pop);
    }

    sub string_lv {
        my ($self, $text) = @_;
        $self->_push("LValue", "Kernel.NewROLValue(new CLRImportObject(" . qm($text) . "))");
    }

    sub how {
        my ($self) = @_;
        my $v = $self->_pop;
        $self->_cpscall("IP6", "$v.HOW(th)");
    }

    sub fetchlv {
        my ($self) = @_;
        my $lv = $self->_pop;
        $self->_cpscall("IP6", "$lv.container.Fetch(th)");
    }

    sub dup_fetchlv {
        my ($self) = @_;
        my $lv = $self->_peek;
        $self->_cpscall('IP6', "$lv.container.Fetch(th)");
        $self->_swap;
    }

    sub pos {
        my ($self, $num) = @_;
        $self->_push('LValue', "th.pos[$num]");
    }

    sub clone_lex {
        my ($self, $name) = @_;
        $self->_push('LValue', "((Variable)th.proto.lex[" . qm($name) . "]).lv");
        $self->dup_fetchlv;
        $self->_push('LValue', "Kernel.NewROLValue(th)");
        $self->call_method(1, "clone", 1);
        $self->lextypes($name, 'Variable');
        $self->rawlexput($name);
    }

    sub copy_lex {
        my ($self, $name) = @_;
        $self->_push('LValue', "((Variable)th.proto.lex[" . qm($name) . "]).lv");
        $self->fetchlv;
        $self->_push('Variable', "Kernel.NewRWVar(" . $self->_pop . ")");
        $self->lextypes($name, 'Variable');
        $self->rawlexput($name);
    }

    sub call_method {
        my ($self, $nv, $name, $numargs) = @_;
        my @args = reverse map { $self->_pop } (1 .. $numargs + 1);  # invocant LV
        my $inv = $self->_pop;
        $self->_cpscall(($nv ? 'Variable' : undef), "$inv.InvokeMethod(th, " . qm($name) . ", new LValue[" . scalar(@args) . "] { " . join(", ", @args) . " }, null)");
    }

    sub call_sub {
        my ($self, $nv, $numargs) = @_;
        my @args = reverse map { $self->_pop } (1 .. $numargs);
        my $inv = $self->_pop;
        $self->_cpscall(($nv ? 'Variable' : undef), "$inv.Invoke(th, new LValue[" . scalar(@args) . "] { " . join(", ", @args) . " }, null)");
    }

    sub tail_call_sub {
        my ($self, $numargs) = @_;
        my @args = reverse map { $self->_pop } (1 .. $numargs);
        my $inv = $self->_pop;
        $self->_emit("return $inv.Invoke(th.caller, new LValue[" . scalar(@args) . "] { " . join(", ", @args) . " }, null)");
        $self->unreach(1);
    }

    sub clr_unwrap {
        my ($self, $ty) = @_;
        my $v = $self->_pop;
        $self->_push($ty, "((CLRImportObject)$v).val");
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

    sub clr_field_get {
        my ($self, $f) = @_;
        my $ty = $typedata{$self->stacktype->[-1]}{$f};
        my $obj = $self->_pop;
        $self->_push($ty, "$obj.$f");
    }

    sub clr_field_set {
        my ($self, $f) = @_;
        my $val = $self->_pop;
        my $obj = $self->_pop;
        $self->_emit("$obj.$f = $val");
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
        if ($f) {
            $self->clr_string($f);
        }
        my $ix  = $self->_pop;
        my $val = $self->_pop;
        my $obj = $self->_pop;
        $self->_emit("$obj" . "[$ix] = $val");
    }

    sub cast {
        my ($self, $type) = @_;
        $self->stacktype->[-1] = $type;
    }

    sub clr_call_direct {
        my ($self, $name, $nargs) = @_;
        my $rt = $typedata{$name};
        my @args = reverse map { $self->_pop } 1 .. $nargs;
        if ($rt ne 'Void') {
            $self->_push($rt, "$name(" . join(", ", @args) . ")");
        } else {
            $self->_emit("$name(" . join(", ", @args) . ")");
        }
    }

    sub return {
        my ($self, $nv) = @_;
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

    sub open_protopad {
        my ($self) = @_;
        my $p = $self->_peek;
        $self->_push('Frame', "new Frame($p)");
    }

    sub close_sub {
        my ($self, $bodycg) = @_;
        my $pp = $self->_pop;
        my $op = $self->_peek;
        $self->_push('IP6', "Kernel.MakeSub(new DynBlockDelegate(" .
            $bodycg->csname . "), $pp, $op)");
    }

    sub proto_var {
        my ($self, $name) = @_;
        my $pv = $self->_pop;
        my $pp = $self->_peek;
        $self->_emit("$pp.lex[" . qm($name) . "] = $pv");
    }

    sub csname {
        my ($self) = @_;
        my @name = split /\W+/, $self->name;
        shift @name if @name && $name[0] eq '';
        join("", (map { ucfirst $_ } @name), "_", $self->uid, "C");
    }

    sub write {
        my ($self) = @_;
        my $name = $self->csname;
        print " " x 8, "private static Frame $name(Frame th) {\n";
        print " " x 12, "Console.WriteLine(\"Entering $name @ \" + th.ip);\n";
        if ($self->maxdepth) {
            print " " x 12, "object " . join(", ", map { "s$_" }
                0 .. ($self->maxdepth - 1)) . ";\n";
        }
        print " " x 12, "switch (th.ip) {\n";
        print " " x 16, "case 0:\n";
        print " " x 16, $_ for @{ $self->buffer };
        print " " x 16, "default:\n";
        print " " x 20, "throw new Exception(\"Invalid IP\");\n";
        print " " x 12, "}\n";
        print " " x 8, "}\n";
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
