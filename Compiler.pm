use strict;
use warnings;
use 5.010;

{
    package CodeGen;
    use Moose;

    has name      => (isa => 'Str', is => 'ro');
    has uid       => (isa => 'Int', is => 'ro', default => sub { ++(state $i) });
    has depth     => (isa => 'Int', is => 'rw', default => 0);
    has maxdepth  => (isa => 'Int', is => 'rw', default => 0);
    has savedepth => (isa => 'Int', is => 'rw', default => 0);
    has numlabels => (isa => 'Int', is => 'rw', default => 1);
    has stacktype => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has labelname => (isa => 'HashRef', is => 'ro', default => sub { +{} });
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
        push @{ $self->buffer }, "    goto case $n;\n" unless $self->unreach;
        push @{ $self->buffer }, "case $n:\n";
        $self->unreach(0);
    }

    sub goto {
        my ($self, $n) = @_;
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
        $self->_push("Niecza.LValue", "((Variable)th." . ("outer." x $order) . "lex[" . qm($name) . "]).lv");
    }

    sub rawlexget {
        my ($self, $ty, $name) = @_;
        $self->_push($ty, "th.lex[" . qm($name) . "]");
    }

    sub rawlexput {
        my ($self, $ty, $name) = @_;
        $self->_emit("th.lex[" . qm($name) . "] = " . $self->_pop);
    }

    sub string_lv {
        my ($self, $text) = @_;
        $self->_push("Niecza.LValue", "Kernel.NewROLValue(new CLRImportObject(" . qm($text) . "))");
    }

    sub how {
        my ($self) = @_;
        my $v = $self->_pop;
        $self->_cpscall("Niecza.IP6", "$v.HOW(th)");
    }

    sub fetchlv {
        my ($self) = @_;
        my $lv = $self->_pop;
        $self->_cpscall("Niecza.IP6", "$lv.container.Fetch(th)");
    }

    sub dup_fetchlv {
        my ($self) = @_;
        my $lv = $self->_peek;
        $self->_cpscall('Niecza.IP6', "$lv.container.Fetch(th)");
        $self->_swap;
    }

    sub pos {
        my ($self, $num) = @_;
        $self->_push('Niecza.LValue', "th.pos[$num]");
    }

    sub clone_lex {
        my ($self, $name) = @_;
        $self->_push('Niecza.LValue', "((Variable)th.proto.lex[" . qm($name) . "]).lv");
        $self->dup_fetchlv;
        $self->_push('Niecza.LValue', "Kernel.NewROLValue(th)");
        $self->call_method(1, "clone", 1);
        $self->rawlexput('Niecza.Variable', $name);
    }

    sub copy_lex {
        my ($self, $name) = @_;
        $self->_push('Niecza.LValue', "((Variable)th.proto.lex[" . qm($name) . "]).lv");
        $self->fetchlv;
        $self->_push('Niecza.Variable', "Kernel.NewRWVar(" . $self->_pop . ")");
        $self->rawlexput('Niecza.Variable', $name);
    }

    sub call_method {
        my ($self, $nv, $name, $numargs) = @_;
        my @args = reverse map { $self->_pop } (1 .. $numargs + 1);  # invocant LV
        my $inv = $self->_pop;
        $self->_cpscall(($nv ? 'Niecza.Variable' : undef), "$inv.InvokeMethod(th, " . qm($name) . ", new LValue[" . scalar(@args) . "] { " . join(", ", @args) . " }, null)");
    }

    sub call_sub {
        my ($self, $nv, $numargs) = @_;
        my @args = reverse map { $self->_pop } (1 .. $numargs);
        my $inv = $self->_pop;
        $self->_cpscall(($nv ? 'Niecza.Variable' : undef), "$inv.Invoke(th, new LValue[" . scalar(@args) . "] { " . join(", ", @args) . " }, null)");
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
        my ($self, $ty, $f) = @_;
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
        my ($self, $ty, $f) = @_;
        if ($f) {
            $self->clr_string($f);
        }
        my $ix  = $self->_pop;
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
        my ($self, $rt, $name, $nargs) = @_;
        my @args = reverse map { $self->_pop } 1 .. $nargs;
        if (defined $rt) {
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
        $self->_push('Niecza.Frame', "new Frame($p)");
    }

    sub close_sub {
        my ($self, $bodycg) = @_;
        my $pp = $self->_pop;
        my $op = $self->_peek;
        $self->_push('Niecza.IP6', "Kernel.MakeSub(new DynBlockDelegate(" .
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

{
    package Body;
    use Moose;

    has name    => (isa => 'Str', is => 'rw', default => "anon");
    has do      => (isa => 'Expression', is => 'rw');
    has enter   => (isa => 'ArrayRef[Expression]', is => 'ro',
        default => sub { [] });
    has lexical => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has outer   => (isa => 'Body', is => 'rw', init_arg => undef);
    # various things which need PRE-INIT time initialization -
    # phasers (Expr), subblocks [str, Body], variables [str, Expr]
    has protos  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has codegen => (isa => 'CodeGen', is => 'rw');

    sub code {
        my ($self) = @_;
        if ($self->codegen) { return $self->codegen }
        $self->codegen(CodeGen->new(name => $self->name));
        my $cg = $self->codegen;
        $_->void_cg($cg, $self) for @{ $self->enter };
        $self->do->item_cg($cg, $self);
        $cg->return(1) unless $cg->unreach;
        return $cg;
    }

    sub write {
        my ($self) = @_;
        $self->code->write;
        for my $pi (@{ $self->protos }) {
            $pi->[2]->outer($self);
            $pi->[2]->name($pi->[1] // 'PREINIT');
            $pi->[2]->write;
        }
    }

    sub preinit {
        my ($self, $cg) = @_;
        for my $pi (@{ $self->protos }) {
            my ($k,$a,$b) = @$pi;
            $b->name($a // "PREINIT");
            $b->outer($self);
            $cg->open_protopad;
            $b->preinit($cg);
            $cg->close_sub($b->code);
            if ($k) {
                $cg->call_sub(($a ? 1 : 0), 0);
            } else {
                $cg->clr_call_direct('Variable', 'Kernel.NewROVar', 1);
            }
            $cg->proto_var($a) if $a;
        }
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Expression;
    use Moose;

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package NIL;
    use Moose;
    extends 'Expression';

    has code => (isa => 'ArrayRef', is => 'ro', required => 1);

    sub item_cg {
        my ($self, $cg, $body) = @_;
        for my $insn (@{ $self->code }) {
            my ($op, @args) = @$insn;
            $cg->$op(@args);
        }
    }

    sub void_cg {
        my ($self, $cg, $body) = @_;
        $self->item_cg($cg, $body);
        $cg->drop;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package StatementList;
    use Moose;
    extends 'Expression';

    has children => (isa => 'ArrayRef[Statement]', is => 'ro', default => sub { +{} });

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package CallSub;
    use Moose;
    extends 'Expression';

    has invocant    => (isa => 'Expression', is => 'ro', required => 1);
    has positionals => (isa => 'ArrayRef[Expression]', is => 'ro',
        default => sub { [] });

    sub item_cg {
        my ($self, $cg, $body) = @_;
        $self->invocant->item_cg($cg, $body);
        $cg->fetchlv;
        $_->item_cg($cg, $body) for @{ $self->positionals };
        $cg->call_sub(1, scalar(@{ $self->positionals }));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package StringLiteral;
    use Moose;
    extends 'Expression';

    has text => (isa => 'Str', is => 'ro', required => 1);

    sub item_cg {
        my ($self, $cg, $body) = @_;
        $cg->string_lv($self->text);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Lexical;
    use Moose;
    extends 'Expression';

    has name => (isa => 'Str', is => 'ro', required => 1);

    sub item_cg {
        my ($self, $cg, $body) = @_;
        my ($order, $scope) = (0, $body);
        while ($scope && !$scope->lexical->{$self->name}) {
            $scope = $scope->outer;
            $order++;
        }
        if (!$scope) {
            die "Failed to resolve lexical " . $self->name . " in " .
                $body->name;
        }
        $cg->lex_lv($order, $self->name);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package CloneSub;
    use Moose;
    extends 'Expression';

    has name => (isa => 'Str', is => 'ro', required => 1);

    sub void_cg {
        my ($self, $cg) = @_;
        $cg->clone_lex($self->name);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Unit;
    use Moose;
    has mainline => (isa => 'Body', is => 'ro', required => 1);

    has codegen => (isa => 'CodeGen', is => 'rw');

    sub code {
        my ($self) = @_;
        my $cg = $self->codegen;
        if ($cg) {
            return $cg;
        } else {
            $self->codegen($cg = CodeGen->new(name => 'boot'));
            $cg->push_null('Niecza.Frame');
            $cg->open_protopad;
            $self->mainline->preinit($cg);
            $cg->close_sub($self->mainline->code);
            $cg->call_sub(0,0);
            $cg->return;
            return $cg;
        }
    }

    sub write {
        my ($self) = @_;
        print <<EOH;
using System;
using System.Collections.Generic;
namespace Niecza {
    public class MainClass {
        public static void Main() {
            Frame root_f = new Frame(null, null,
                    new DynBlockDelegate(@{[ $self->code->csname ]}));
            Frame current = root_f;
            while (current != null) {
                current = current.Continue();
            }
        }
EOH
        $self->code->write;
        $self->mainline->write;
        print "    }\n}\n"
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

my $unit = Unit->new(
    mainline => Body->new(
        name   => 'body',
        protos => [
            # we have to do this directly due to the circularity saw.  same
            # reason forces uncontainerized .NET values
            # class ClassHOW {
            #     has Array[ClassHOW] $.parents;
            #     has Dictionary[string,Sub] $.local-methods;
            #     has DynMetaObject $.meta;
            #     has DynObject $.proto;
            #
            #     sub 
            # }
            [ 1, 'ClassHOW' => Body->new(
                    protos => [
                        [ 0, '&wrap-metaclass', Body->new(do => NIL->new(code => [
                            # Args: 0: metaclass metaobject 1: metaobject
                            # returns: protoobject
                            ['clr_new', 'DynObject', 0],
                            ['rawlexput', 'DynObject', 'mci'],
                            ['pos',1],
                            ['fetchlv'],
                            ['clr_unwrap', 'DynMetaObject'],
                            ['rawlexput', 'DynMetaObject', 'mo'],
                            ['clr_new', 'DynObject', 0],
                            ['rawlexput', 'DynObject', 'p'],

                            ['rawlexget', 'DynObject', 'p'],
                            ['rawlexget', 'DynMetaObject', 'mo'],
                            ['clr_field_set', 'klass'],

                            ['rawlexget', 'DynMetaObject', 'mo'],
                            ['rawlexget', 'DynObject', 'mci'],
                            ['clr_field_set', 'how'],

                            ['rawlexget', 'DynObject', 'mci'],
                            ['clr_field_get', 'Dictionary<string,object>', 'slots'],
                            ['rawlexget', 'DynMetaObject', 'mo'],
                            ['clr_index_set', 'meta-object'],

                            ['rawlexget', 'DynObject', 'mci'],
                            ['clr_field_get', 'Dictionary<string,object>', 'slots'],
                            ['rawlexget', 'DynObject', 'p'],
                            ['clr_index_set', 'prototype'],

                            ['rawlexget', 'DynObject', 'mci'],
                            ['pos',0],
                            ['fetchlv'],
                            ['clr_unwrap', 'DynMetaObject'],
                            ['clr_field_set', 'klass'],

                            ['rawlexget', 'DynObject', 'p'],
                            ['clr_call_direct', 'Variable', 'Kernel.NewROVar', 1]]))],
                        [ 0, '&new-metaclass', Body->new(do => NIL->new(code => [
                            # Args: 0: metaclass metaobject, 1: name
                            ['clr_new','DynMetaObject',0],
                            ['rawlexput','DynMetaObject','mo'],

                            ['rawlexget','DynMetaObject','mo'],
                            ['pos',1],
                            ['fetchlv'],
                            ['clr_unwrap','String'],
                            ['clr_field_set','name'],

                            ['lex_lv',1,'&wrap-metaclass'],
                            ['fetchlv'],
                            ['pos',0],
                            ['rawlexget','DynMetaObject','mo'],
                            ['clr_new','CLRImportObject',1],
                            ['clr_call_direct','LValue','Kernel.NewROLValue',1],
                            ['tail_call_sub', 2]]))],
                        [ 0, '&new', Body->new(do => NIL->new(code => [
                            ['lex_lv',1,'&new-metaclass'],
                            ['fetchlv'],
                            ['pos',0],
                            ['fetchlv'],
                            ['cast','DynObject'],
                            ['clr_field_get','DynMetaObject','klass'],
                            ['clr_new','CLRImportObject',1],
                            ['clr_call_direct','LValue','Kernel.NewROLValue',1],
                            ['pos',1],
                            ['tail_call_sub',2]]))],
                        [ 0, '&add-method', Body->new(do => NIL->new(code => [
                            ['push_null','Variable'],
                            ]))]],
                    do => NIL->new(code => [
                        ['copy_lex','&new-metaclass'],
                        ['lex_lv',0,'&new-metaclass'],
                        ['fetchlv'],
                        ['push_null','DynMetaObject'],
                        ['clr_new','CLRImportObject',1],
                        ['clr_call_direct','LValue','Kernel.NewROLValue',1],
                        ['string_lv','ClassHOW'],
                        ['call_sub',1,2],
                        ['rawlexput','Variable','$p'],

                        ['lex_lv',0,'$p'],
                        ['fetchlv'],
                        ['how'],
                        ['cast','DynObject'],
                        ['lex_lv',0,'$p'],
                        ['fetchlv'],
                        ['cast','DynObject'],
                        ['clr_field_get','DynMetaObject','klass'],
                        ['clr_field_set','klass'],

                        ['rawlexget','Variable','$p']]))],
            [ 0, '&say' => Body->new(
                    do => NIL->new(
                        code => [
                            ['pos', 0],
                            ['fetchlv'],
                            ['clr_unwrap', 'System.String'],
                            ['clr_call_direct', undef,
                                "System.Console.WriteLine", 1],
                            ['push_null', 'Variable']])) ]],
        enter  => [CloneSub->new(name => '&say')],
        lexical=> { '&say', 1 },
        do     => CallSub->new(
            invocant    => Lexical->new(name => '&say'),
            positionals => [StringLiteral->new(text => 'Hello, World')])));

$unit->write;
