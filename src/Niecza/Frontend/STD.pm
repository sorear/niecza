package Niecza::Frontend::STD;
use Moose;
use Niecza::Grammar ();
use Niecza::Actions ();
has lang=>(is=>'ro');
has UNITNAME=>(is=>'ro');
sub parse {
    my ($self,$source,$filename) = @_;
    local $::YOU_WERE_HERE;
    local $::UNITNAME = $self->UNITNAME // 'MAIN';
    $::UNITNAME =~ s/::/./g;
    $STD::ALL = {};

    my $ast = Niecza::Grammar->parse($source, setting => $self->lang,
        actions => 'Niecza::Actions', filename=>$filename)->{_ast},
}
1;
