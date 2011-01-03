package Niecza::Pass::Begin;
use Moose;
has lang=>(is=>'ro');
sub invoke {
    my ($self,$ast) = @_;
    local $::SETTING_UNIT;

    if ($self->lang ne 'NULL') {
        $::SETTING_UNIT = $self->lang;
    }
    $ast->begin;
}
1;
