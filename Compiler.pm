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
    has buffer    => (isa => 'ArrayRef', is => 'ro', default => sub { [] });

    sub qm { "\"" . $_[0] . "\"" }

    sub _emit {
        my ($self, $line) = @_;
        #push @{ $self->buffer }, sprintf "    // d=%d md=%d sd=%d nl=%d\n",
        #    $self->depth, $self->maxdepth, $self->savedepth, $self->numlabels;
        push @{ $self->buffer }, "    $line;\n";
    }

    sub _undercheck {
        my ($self, $margin) = @_;
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
    }

    sub _peek {
        my ($self) = @_;
        $self->_undercheck(1);
        return "s" . ($self->depth - 1);
    }

    sub _pop {
        my ($self) = @_;
        $self->_undercheck(1);
        $self->depth($self->depth - 1);
        return "s" . ($self->depth);
    }

    sub _push {
        my ($self, $expr) = @_;
        $self->_overcheck(1);
        my $n = $self->depth;
        $self->_emit("s$n = $expr");
        $self->depth($n + 1);
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
        push @{ $self->buffer }, "    goto case $n;\n";
        push @{ $self->buffer }, "case $n:\n";
    }

    sub goto {
        my ($self, $n) = @_;
        $self->_saveall;
        push @{ $self->buffer }, "    goto case $n;\n";
    }

    sub _cpscall {
        my ($self, $nv, $expr) = @_;
        $self->_saveall;
        my $n = $self->label;
        $self->_emit("th.resultSlot = null");
        $self->_emit("th.ip = $n");
        $self->_emit("return $expr");
        push @{ $self->buffer }, "case $n:\n";
        $self->_push('th.resultSlot') if $nv;
    }

    sub lex_lv {
        my ($self, $name) = @_;
        $self->_push("((Variable)th.lex[" . qm($name) . "]).lv");
    }

    sub string_lv {
        my ($self, $text) = @_;
        $self->_push("Kernel.NewROLValue(new CLRImportObject(" . qm($text) . "))");
    }

    sub fetchlv {
        my ($self) = @_;
        my $lv = $self->_pop;
        $self->_cpscall(1, "((LValue)$lv).container.Fetch(th)");
    }

    sub dup_fetchlv {
        my ($self) = @_;
        my $lv = $self->_peek;
        $self->_cpscall(1, "((LValue)$lv).container.Fetch(th)");
        $self->_swap;
    }

    sub pos {
        my ($self, $num) = @_;
        $self->_push("th.pos[$num]");
    }

    sub clone_lex {
        my ($self, $name) = @_;
        $self->_push("((Variable)th.proto.lex[" . qm($name) . "]).lv");
        $self->dup_fetchlv;
        $self->_push("Kernel.NewROLValue(th)");
        $self->call_method(0, "clone", 1);
        $self->_emit("th.lex[" . qm($name) . "] = th.resultSlot");
    }

    sub call_method {
        my ($self, $nv, $name, $numargs) = @_;
        my @args = reverse map { "((LValue)" . $self->_pop . ")" } (1 .. $numargs + 1);  # invocant LV
        my $inv = $self->_pop;
        $self->_cpscall($nv, "((IP6)$inv).InvokeMethod(th, " . qm($name) . ", new LValue[" . scalar(@args) . "] { " . join(", ", @args) . " }, null)");
    }

    sub call_sub {
        my ($self, $nv, $numargs) = @_;
        my @args = reverse map { "((LValue)" . $self->_pop . ")" } (1 .. $numargs);
        my $inv = $self->_pop;
        $self->_cpscall($nv, "((IP6)$inv).Invoke(th, new LValue[" . scalar(@args) . "] { " . join(", ", @args) . " }, null)");
    }

    sub tail_call_sub {
        my ($self, $numargs) = @_;
        my @args = reverse map { "((LValue)" . $self->_pop . ")" } (1 .. $numargs);
        my $inv = $self->_pop;
        $self->_emit("return ((IP6)$inv).Invoke(th.caller, new LValue[" . scalar(@args) . "] { " . join(", ", @args) . " }, null)");
    }

    sub clr_unwrap {
        my ($self) = @_;
        my $v = $self->_pop;
        $self->_push("((CLRImportObject)$v).val");
    }

    sub clr_call_direct {
        my ($self, $nv, $name, @argtypes) = @_;
        my @args = reverse map { "(($_)" . $self->_pop . ")" } reverse @argtypes;
        if ($nv) {
            $self->_push("$name(" . join(", ", @args) . ")");
        } else {
            push @{ $self->buffer }, "    $name(" . join(", ", @args) . ");\n";
        }
    }

    sub return {
        my ($self, $nv) = @_;
        if ($nv) {
            push @{ $self->buffer }, "    th.caller.resultSlot = " . $self->_pop . ";\n";
        }
        push @{ $self->buffer }, "    return th.caller;\n";
    }

    sub push_null {
        my ($self) = @_;
        $self->_push("null");
    }

    sub open_protopad {
        my ($self) = @_;
        my $p = $self->_peek;
        $self->_push("new Frame((Frame)$p)");
    }

    sub close_sub {
        my ($self, $bodycg) = @_;
        my $pp = $self->_pop;
        my $op = $self->_peek;
        $self->_push("Kernel.MakeSub(new DynBlockDelegate(" . $bodycg->csname .
            "), ((Frame)$pp), ((Frame)$op))");
    }

    sub proto_var {
        my ($self, $name) = @_;
        my $pv = $self->_pop;
        my $pp = $self->_peek;
        $self->_emit("((Frame)$pp).lex[" . qm($name) . "] = Kernel.NewROVar((IP6)$pv)");
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
        $_->void_cg($cg) for @{ $self->enter };
        $self->do->item_cg($cg);
        $cg->return;
        return $cg;
    }

    sub write {
        my ($self) = @_;
        $self->code->write;
        for my $pi (@{ $self->protos }) {
            if (ref($pi) eq 'ARRAY' && $pi->[1]->isa('Body')) {
                $pi->[1]->outer($self);
                $pi->[1]->name($pi->[0]);
                $pi->[1]->write;
            }
        }
    }

    sub preinit {
        my ($self, $cg) = @_;
        for my $pi (@{ $self->protos }) {
            if (ref($pi) ne 'ARRAY') {
                $pi->void_cg($cg);
            } elsif ($pi->[1]->isa('Body')) {
                $pi->[1]->name($pi->[0]);
                $pi->[1]->outer($self);
                $cg->open_protopad;
                $pi->[1]->preinit($cg);
                $cg->close_sub($pi->[1]->code);
                $cg->proto_var($pi->[0]);
            } else {
                $pi->[1]->item_cg($cg);
                $cg->proto_var($pi->[0]);
            }
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
        my ($self, $cg) = @_;
        for my $insn (@{ $self->code }) {
            my ($op, @args) = @$insn;
            $cg->$op(@args);
        }
    }

    sub void_cg {
        my ($self, $cg) = @_;
        $self->item_cg($cg);
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
            $cg->push_null;
            $cg->open_protopad;
            $self->mainline->preinit($cg);
            $cg->close_sub($self->mainline->code);
            $cg->tail_call_sub(0);
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
            [ '&say' => Body->new(
                    do => NIL->new(
                        code => [
                            ['pos', 0],
                            ['fetchlv'],
                            ['clr_unwrap'],
                            ['clr_call_direct', 0, "System.Console.WriteLine",
                                "System.String"],
                            ['push_null']])) ]],
        enter  => [CloneSub->new(name => '&say')],
        do     => NIL->new(
            code => [
                ['lex_lv', '&say'],
                ['fetchlv'],
                ['string_lv', 'Hello World'],
                ['call_sub', 1, 1]])));

$unit->write;
