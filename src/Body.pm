use strict;
use warnings;
use 5.010;
use utf8;
use CgOp ();

{
    package Body;
    use Moose;

    has name      => (isa => 'Str', is => 'rw', default => "ANON");
    has uid       => (isa => 'Int', is => 'ro', default => sub { ++(state $i) });
    has do        => (isa => 'Op', is => 'rw');
    has signature => (isa => 'Maybe[Sig]', is => 'rw');
    has mainline  => (isa => 'Bool', is => 'ro', lazy => 1,
            builder => 'is_mainline');
    # '' for incorrectly contextualized {p,x,}block, blast
    has type      => (isa => 'Str', is => 'rw');
    has returnable=> (isa => 'Bool', is => 'rw');

    # my $x inside, floats out; mostly for blasts; set by context so must be rw
    has transparent => (isa => 'Bool', is => 'rw', default => 0);
    # only used for the top mainline
    has file => (isa => 'Str', is => 'ro');
    has text => (isa => 'Str', is => 'ro');

    # metadata for runtime inspection
    has class => (isa => 'Str', is => 'rw', default => 'Sub');
    has ltm   => (is => 'rw');

    sub is_mainline { $_[0]->scopetree->{'?is_mainline'} }

    sub csname {
        my ($self) = @_;
        my @name = split /\W+/, $self->name;
        shift @name if @name && $name[0] eq '';
        join("", (map { ucfirst $_ } @name), "_", $self->uid, "C");
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
