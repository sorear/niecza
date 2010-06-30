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

    sub push_root_frame {
        my ($self) = @_;
        $self->_push("Kernel.KernelFrame");
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
}

{
    package Body;
    use Moose;

    has do      => (isa => 'Expression', is => 'rw');
    has lexical => (isa => 'Scope', is => 'rw');
}

{
    package Scope;
    use Moose;

    has names => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has outer => (isa => 'Scope', is => 'rw');
}

{
    package Expression;
    use Moose;
}

{
    package StatementList;
    use Moose;
    extends 'Expression';

    has children => (isa => 'ArrayRef[Statement]', is => 'ro', default => sub { +{} });
}

my $boot = CodeGen->new(name => 'boot');

$boot->push_root_frame;

$boot->open_protopad;
my $main = CodeGen->new(name => 'main');

$boot->open_protopad;
my $say = CodeGen->new(name => '&say');
$say->pos(0);
$say->fetchlv;
$say->clr_unwrap;
$say->clr_call_direct(0, "System.Console.WriteLine", "System.String");
$say->return;

$boot->close_sub($say);
$boot->proto_var('&say');

$main->clone_lex('&say');
$main->lex_lv('&say');
$main->fetchlv;
$main->string_lv('Hello World');
$main->call_sub(0, 1);
$main->return;

$boot->close_sub($main);
$boot->tail_call_sub(0);

print <<EOH;
using System;
using System.Collections.Generic;
namespace Sprixel {
    public class MainClass {
        public static void Main() {
            Frame root_f = new Frame(null, null,
                    new DynBlockDelegate(@{[ $boot->csname ]}));
            Frame current = root_f;
            while (current != null) {
                current = current.Continue();
            }
        }
EOH
$boot->write;
$main->write;
$say->write;
print "    }\n}\n"
