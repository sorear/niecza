use strict;
use warnings;
use 5.010;

{
    package CodeGen;
    use Moose;

    has depth     => (isa => 'Int', is => 'rw', default => 0);
    has maxdepth  => (isa => 'Int', is => 'rw', default => 0);
    has savedepth => (isa => 'Int', is => 'rw', default => 0);
    has numlabels => (isa => 'Int', is => 'rw', default => 1);
    has buffer    => (isa => 'ArrayRef', is => 'ro', default => sub { [] });

    sub _pop {
        my ($self) = @_;
        if ($self->depth == $self->savedepth) {
            my $n = $self->depth - 1;
            push @{ $self->buffer }, "    s$n = th.lex[\"s$n\"];\n";
            $self->savedepth($n);
        }
        my $n = $self->depth - 1;
        $self->depth($n);
        return "s$n";
    }

    sub _push {
        my ($self, $expr) = @_;
        if ($self->depth == $self->maxdepth) {
            $self->maxdepth($self->depth + 1);
        }
        my $n = $self->depth;
        push @{ $self->buffer }, "    s$n = $expr;\n";
        $self->depth($n + 1);
    }

    sub _saveall {
        my ($self) = @_;
        for my $i ($self->savedepth .. $self->depth - 1) {
            push @{ $self->buffer }, "    th.lex[\"s$i\"] = s$i;\n";
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
        push @{ $self->buffer }, "    th.resultSlot = null;\n";
        push @{ $self->buffer }, "    th.ip = $n;\n";
        push @{ $self->buffer }, "    return $expr;\n";
        push @{ $self->buffer }, "case $n:\n";
        $self->_push('th.resultSlot') if $nv;
    }

    sub fetchlv {
        my ($self) = @_;
        my $lv = $self->_pop;
        $self->_cpscall(1, "((LValue)$lv).container.Fetch(th)");
    }

    sub pos {
        my ($self, $num) = @_;
        $self->_push("th.pos[$num]");
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

    sub code {
        my ($self) = @_;
        my $code = "";
        if ($self->maxdepth) {
            $code .= "object " . join(", ", map { "s$_" }
                0 .. ($self->maxdepth - 1)) . ";\n";
        }
        $code .= "switch (th.ip) {\n";
        $code .= "case 0:\n";
        $code .= $_ for @{ $self->buffer };
        $code .= "default:\n";
        $code .= "    throw new Exception(\"Invalid IP\");\n";
        $code .= "}\n";

        $code;
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

my $cg = CodeGen->new;

$cg->pos(0);
$cg->fetchlv;
$cg->clr_unwrap;
$cg->clr_call_direct(0, "System.Console.WriteLine", "System.String");
$cg->return;

print $cg->code;
