use strict;
use warnings;
use 5.010;

{
    package Unit;
    use Moose;
    has mainline => (isa => 'Body', is => 'ro', required => 1);
    has name     => (isa => 'Str', is => 'ro', required => 1);

    has is_setting => (isa => 'Bool', is => 'ro');
    has setting_name => (isa => 'Str', is => 'ro');

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
