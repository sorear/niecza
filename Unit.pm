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
    has mainboot => (isa => 'CgOp', is => 'rw');
    has name     => (isa => 'Str', is => 'ro', required => 1);
    has setting  => (is => 'ro');

    has is_setting => (isa => 'Bool', is => 'ro');
    has setting_name => (isa => 'Str', is => 'ro');

    sub lift_decls {
        $_[0]->mainline->lift_decls;
    }

    sub to_cgop {
        $_[0]->mainboot($_[0]->mainline->to_cgop);
    }

    sub extract_scopes {
        $_[0]->mainline->extract_scopes($_[0]->setting);
    }

    sub gen_code {
        my ($self) = @_;
        CodeGen->know_module($self->csname($self->setting_name));
        CodeGen->know_module($self->csname);
        CodeGen->new(csname => 'BOOT',
            ops => CgOp::letn('pkg', CgOp::rawsget('Kernel.Global'),
                CgOp::rawscall($self->csname($self->setting_name) . '.Initialize'),
                CgOp::letn('protopad',
                    CgOp::cast('Frame', CgOp::rawsget($self->csname($self->setting_name) .
                            '.Environment')),
                    ($self->is_setting ?
                        CgOp::rawsset($self->csname . '.Installer',
                            CgOp::protosub($self->mainline)) :
                        CgOp::subcall(CgOp::rawsget($self->csname($self->setting_name) . '.Installer'),
                            CgOp::newscalar(CgOp::protosub($self->mainline)))),
                    CgOp::return())));
    }

    sub csname {
        my $x = $_[1] // $_[0]->name;
        $x =~ s/::/./g;
        $x ||= 'MAIN';
        $x;
    }

    sub write {
        my ($self) = @_;
        #say STDERR (YAML::XS::Dump($self));
        print ::NIECZA_OUT <<EOH;
public class @{[ $self->csname ]} {
EOH
        $self->gen_code->write;
        $self->mainline->write;
        if ($self->is_setting) {
            print ::NIECZA_OUT <<EOSB ;
    public static IP6 Installer;
EOSB
        }
        if (!$self->name) { # || has_MAIN
            print ::NIECZA_OUT <<EOMAIN ;
    public static void Main() {
        Initialize();
    }
EOMAIN
        }
        print ::NIECZA_OUT <<EOGB ;
    private static bool Initialized;
    public static Frame Environment;
    public static void Initialize() {
        if (!Initialized) {
            Initialized = true;
            Kernel.RunLoop(new DynBlockDelegate(BOOT));
        }
    }
}
EOGB
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
