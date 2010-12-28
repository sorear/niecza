use 5.010;
use utf8;
use strict;
use warnings;

package NAMBackend;

our $unit;
use JSON;

sub run {
    local $unit = shift;

    $unit->visit_local_subs_postorder(\&nam_sub);
    my $nam = $unit->to_nam;

    eval {
        $nam = encode_json $nam;
    };
    if ($@) {
        say ($@);
        say (YAML::XS::Dump $nam);
    }
    return $nam;
}

sub nam_sub {
    my $s = shift;
    $s->{nam} = $s->code->cgop($s);
    if ($s->parametric_role_hack) {
        for (@{ $unit->deref($s->parametric_role_hack)->methods }) {
            if (ref $_->name) {
                $_->{name} = $_->name->cgop($s)->to_nam;
            }
        }
    }
    delete $s->{code};
}

sub Metamodel::Unit::to_nam {
    my $self = shift;
    [
        $self->mainline->xref,
        $self->name,
        $self->ns->log,
        $self->setting,
        $self->bottom_ref,
        [ map { $_ && $_->to_nam } @{ $self->xref } ],
        [ map { [$_, @{ $self->tdeps->{$_} }] } sort keys %{ $self->tdeps } ],
    ]
}

sub Metamodel::StaticSub::to_nam {
    my $self = shift;
    my $flags = 0;
    $flags |= 1 if $self->run_once;
    $flags |= 2 if $self->spad_exists;
    $flags |= 4 if $self->gather_hack;
    $flags |= 8 if $self->strong_used;
    $flags |= 16 if $self->returnable;
    $flags |= 32 if $self->augmenting;
    [
        'sub',
        $self->name,
        $self->{outer}, # get the raw xref
        $flags,
        [ map { $_->xref->[1] } @{ $self->zyg } ],
        $self->parametric_role_hack,
        $self->augment_hack,
        $self->is_phaser,
        $self->body_of,
        $self->in_class,
        $self->cur_pkg,
        $self->class,
        $self->ltm,
        $self->exports,
        ($self->signature && [ map { $_->to_nam } @{ $self->signature->params } ]),
        [ map { [ $_, @{ $self->lexicals->{$_}->to_nam } ] }
            sort keys %{ $self->lexicals } ],
        $self->{nam}->to_nam,
    ]
}

sub Metamodel::Package::to_nam {
    my $self = shift;
    [
        substr(lc(ref($self)),11),
        $self->name,
        $self->exports,
        @_
    ]
}

sub Metamodel::Class::to_nam {
    my $self = shift;
    $self->Metamodel::Package::to_nam(
        [ map { $_->to_nam } @{ $self->attributes } ],
        [ map { $_->to_nam } @{ $self->methods } ],
        $self->superclasses,
        $self->linearized_mro,
    );
}

sub Metamodel::Role::to_nam {
    my $self = shift;
    $self->Metamodel::Package::to_nam(
        [ map { $_->to_nam } @{ $self->attributes } ],
        [ map { $_->to_nam } @{ $self->methods } ],
        $self->superclasses,
    );
}

sub Metamodel::ParametricRole::to_nam {
    my $self = shift;
    $self->Metamodel::Package::to_nam(
        [ map { $_->to_nam } @{ $self->attributes } ],
        [ map { $_->to_nam } @{ $self->methods } ],
        $self->superclasses,
    );
}

sub Metamodel::Method::to_nam {
    [ $_[0]->name, $_[0]->kind, $_[0]->var, $_[0]->body ]
}

sub Metamodel::Attribute::to_nam {
    [ $_[0]->name, $_[0]->public, $_[0]->ivar, $_[0]->ibody ]
}

sub Sig::Parameter::to_nam {
    my $self = shift;
    my $flags = 0;
    $flags |= 1 if $self->slurpy;
    $flags |= 2 if $self->slurpycap;
    $flags |= 4 if $self->rwtrans;
    $flags |= 8 if $self->full_parcel;
    $flags |= 16 if $self->optional;
    $flags |= 32 if $self->positional;
    $flags |= 64 if $self->readonly;
    $flags |= 128 if $self->list;
    $flags |= 256 if $self->hash;

    [
        $self->name,
        $flags,
        $self->slot,
        $self->names,
        $self->mdefault
    ]
}


sub Metamodel::Lexical::Simple::to_nam {
    ['simple', ($_[0]->noinit ? 4 : 0) + ($_[0]->list ? 2 : 0) +
        ($_[0]->hash ? 1 : 0)]
}
sub Metamodel::Lexical::Common::to_nam { ['common', @{$_[0]->path}, $_[0]->name ] }
sub Metamodel::Lexical::Alias::to_nam {  ['alias', $_[0]->to] }
sub Metamodel::Lexical::SubDef::to_nam { ['sub', @{ $_[0]->body->xref } ] }
sub Metamodel::Lexical::Stash::to_nam {  ['stash', @{ $_[0]->path } ] }

sub CgOpNode::to_nam {
    [ map { Scalar::Util::blessed($_) ? $_->to_nam : $_ } @{ $_[0] } ]
}

1;
