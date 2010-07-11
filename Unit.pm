use strict;
use warnings;
use 5.010;

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
            $cg->new_aux('protopad', 'Frame');
            $cg->new_aux('how', 'Variable');
            $cg->push_null('Frame');
            $cg->push_aux('protopad');
            $cg->open_protopad;
            $self->mainline->do_preinit($cg);
            $cg->close_sub($self->mainline->code);
            $cg->call_sub(0,0);
            $cg->return;
            return $cg;
        }
    }

    sub write {
        my ($self) = @_;
        #say STDERR (YAML::XS::Dump($self));
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
1;
