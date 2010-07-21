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
    has code     => (isa => 'CodeGen', is => 'ro', init_arg => undef, lazy => 1,
        builder => 'gen_code');
    has setting  => (isa => 'Body', is => 'ro');

    sub gen_code {
        my ($self) = @_;
        $self->mainline->outer($self->setting) if $self->setting;
        CodeGen->new(name => 'BOOT', entry => 1,
            ops => CgOp::letn('protopad',
                CgOp::cast('Frame', CgOp::fetch(CgOp::pos(0))),
                CgOp::return(
                    CgOp::newscalar(
                        CgOp::protosub($self->mainline)))));
    }

    sub write {
        my ($self) = @_;
        #say STDERR (YAML::XS::Dump($self));
        print ::NIECZA_OUT <<EOH;
public class @{[ $self->name ]} {
EOH
        $self->code->write;
        $self->mainline->write;
        print ::NIECZA_OUT "}\n"
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
