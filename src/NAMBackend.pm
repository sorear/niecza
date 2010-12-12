use 5.010;
use utf8;
use strict;
use warnings;

package NAMBackend;

our $unit;

sub run {
    local $unit = shift;

    [ header(),
      stashlog(),
      objects() ]
}

sub header {
    [ $unit->mainline->xref,
      $unit->name,
      $unit->setting,
      $unit->bottom_ref,
      $unit->tdeps,
      $unit->modtime,
      $unit->filename ]
}

sub stashlog {
    $unit->ns->log;
}

sub objects {
    map { defined($_) ? $_->nam_object : [] } @{ $unit->xref };
}

sub Metamodel::StaticSub::nam_object {
    my ($self) = @_;
    [ $self->outer,
      $self->name,
      $self->run_once,
      $self->spad_exists,
      $self->signature,
      $self->lexicals,
      $self->code->cgop($self),
      [ map { $_->xref->[1] } @{ $self->zyg } ],
      $self->gather_hack,
      $self->parametric_role_hack,
      $self->augment_hack,
      $self->is_phaser // -1,
      $self->body_of,
      $self->in_class,
      $self->cur_pkg,
      $self->returnable,
      $self->augmenting,
      $self->class,
      $self->ltm,
      $self->exports ]
}

sub Metamodel::Package::nam_object {
    my $self = shift;
    [ $self->name,
      $self->exports ]
}

sub Metamodel::Class::nam_object {
    my $self = shift;
    [ $self->name,
      $self->exports,
      $self->attributes,
      $self->methods,
      $self->superclasses,
      $self->linearized_mro ]
}

sub Metamodel::Role::nam_object {
    my $self = shift;
    [ $self->name,
      $self->exports,
      $self->attributes,
      $self->methods,
      $self->superclasses ]
}

sub Metamodel::ParametricRole::nam_object {
    my $self = shift;
    [ $self->name,
      $self->exports,
      $self->builder,
      $self->attributes,
      $self->methods,
      $self->superclasses ]
}

1;
