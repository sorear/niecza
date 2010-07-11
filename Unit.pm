use strict;
use warnings;
use 5.010;

# A Unit generates a CLR class with a BOOT member
# All used Units except for the setting go into Niecza.Kernel.Units
# The main program generates a main class, which sets up Units and runs the
# setting
# BOOT subs take one argument, the outer protopad
{
    package Unit;
    use Moose;
    has mainline => (isa => 'Body', is => 'ro', required => 1);
    has name     => (isa => 'Str', is => 'ro', required => 1);
    has codegen  => (isa => 'CodeGen', is => 'rw');

    sub code {
        my ($self) = @_;
        my $cg = $self->codegen;
        if ($cg) {
            return $cg;
        } else {
            $self->codegen($cg = CodeGen->new(name => 'BOOT', entry => 1));
            $cg->new_aux('protopad', 'Frame');
            $cg->new_aux('how', 'Variable');
            $cg->pos(0);
            $cg->fetch;
            $cg->cast('Frame');
            $cg->push_aux('protopad');
            $cg->open_protopad;
            $self->mainline->do_preinit($cg);
            $cg->close_sub($self->mainline->code);
            $cg->return(1);
            return $cg;
        }
    }

    sub write {
        my ($self) = @_;
        #say STDERR (YAML::XS::Dump($self));
        print <<EOH;
using System;
using System.Collections.Generic;
using Niecza;
public class @{[ $self->name ]} {
EOH
        $self->code->write;
        $self->mainline->write;
        print "}\n"
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
