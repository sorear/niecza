use 5.010;
use utf8;
use strict;
use warnings;

package NAMBackend;

our $unit;
our ($uname, $xref, $xid);
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

sub load {
    my ($text) = @_;
    return unit_from_nam(@{ decode_json($text) });
}

sub Metamodel::Unit::to_nam {
    my $self = shift;
    [
        $self->mainline->xref,
        $self->name,
        $self->ns->log,
        $self->setting,
        $self->bottom_ref,
        $self->filename,
        $self->modtime,
        [ map { $_ && $_->to_nam } @{ $self->xref } ],
        [ map { [$_, @{ $self->tdeps->{$_} }] } sort keys %{ $self->tdeps } ],
        stash_tonam($self->ns->root),
    ]
}

sub stash_tonam {
    my $hr = shift;
    my @out;
    for my $key (sort keys %$hr) {
        my $value = [ $key, @{ $hr->{$key} } ];
        if ($value->[1] eq 'var' && $value->[3]) {
            $value->[3] = stash_tonam($value->[3]);
        }
        push @out, $value;
    }
    \@out;
}

sub stash_fromnam {
    my %out;
    for my $row (@_) {
        my $key = shift @$row;
        if ($row->[0] eq 'var' && $row->[2]) {
            $row->[2] = stash_fromnam(@{ $row->[2] });
        }
        $out{$key} = $row;
    }
    \%out;
}

my %xref_funcs;

sub unit_from_nam {
    my ($mlref, $name, $log, $setting, $bottom, $filename, $modtime, $xr,
        $td, $root) = @_;
    local $xref = [];
    local $uname = $name;
    local $xid = 0;
    local $unit = bless { }, 'Metamodel::Unit';
    for (my $i = 0; $i < @$xr; $i++) {
        $xid = $i;
        if (!$xr->[$i]) {
            push @$xref, undef;
        } else {
            my ($i) = $xref_funcs{$xr->[$i][0]}->(@{ $xr->[$i] });
            push @$xref, $i;
        }
    }
    # XXX suboptimality
    for (my $i = 0; $i < @$xr; $i++) {
        if ($xr->[$i] && $xr->[$i][0] eq 'sub') {
            for my $row (@{ $xr->[$i][16] }) {
                my ($k,$v) = lex_from_nam(@$row);
                $xref->[$i]->lexicals->{$k} = $v;
            }
            for my $z (@{ $xr->[$i][4] }) {
                push @{ $xref->[$i]->{zyg} }, $xref->[$z];
            }
        }
    }

    $unit->{mainline} = $xref->[$mlref->[1]];
    $unit->{name} = $name;
    $unit->{ns} = (bless { root => stash_fromnam(@$root), log => $log },
            'Metamodel::Namespace');
    $unit->{setting} = $setting;
    $unit->{bottom_ref} = $bottom;
    $unit->{xref} = $xref;
    $unit->{filename} = $filename;
    $unit->{modtime} = $modtime;
    $unit->{tdeps} = { map { shift(@$_), $_ } @$td };
    $unit;
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
        $self->hint_hack,
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

$xref_funcs{sub} = sub {
    my ($kind, $name, $outer, $flags, $zyg, $prh, $ah, $hh, $isp, $body,
        $inc, $crp, $cls, $ltm, $exp, $sig, $rlx, $nam) = @_;
    # Most of these are used only by code-gen.  Lexicals are injected later.

    bless {
        unit => $unit,
        name => $name,
        xref => [ $uname, $xid, $name ],
        outer => $outer,
        run_once => !!($flags & 1),
        spad_exists => !!($flags & 2),
        lexicals => {},
        zyg => [],
        class => $cls,
        ltm => $ltm,
    }, 'Metamodel::StaticSub';
};

sub Metamodel::Package::to_nam {
    my $self = shift;
    [
        substr(lc(ref($self)),11),
        $self->name,
        $self->exports,
        @_
    ]
}

$xref_funcs{package} = $xref_funcs{module} = sub {
    my ($type, $name, $exports, @stuff) = @_;
    $type = 'parametricRole' if $type eq 'parametricrole';
    (bless {
        xref => [ $uname, $xid, $name ],
        name => $name,
        exports => $exports
    }, ('Metamodel::' . ucfirst $type)), @stuff;
};

sub Metamodel::Class::to_nam {
    my $self = shift;
    $self->Metamodel::Package::to_nam(
        [ map { $_->to_nam } @{ $self->attributes } ],
        [ map { $_->to_nam } @{ $self->methods } ],
        $self->superclasses,
        $self->linearized_mro,
    );
}

$xref_funcs{class} = $xref_funcs{grammar} = sub {
    my ($obj, $attr, $meth, $sup, $mro, @s) = $xref_funcs{module}->(@_);
    $obj->{attributes}     = [ map { attr_from_nam(@$_) } @$attr ];
    $obj->{methods}        = [ map { method_from_nam(@$_) } @$meth ];
    $obj->{superclasses}   = $sup;
    $obj->{linearized_mro} = $mro;
    $obj;
};

sub Metamodel::Role::to_nam {
    my $self = shift;
    $self->Metamodel::Package::to_nam(
        [ map { $_->to_nam } @{ $self->attributes } ],
        [ map { $_->to_nam } @{ $self->methods } ],
        $self->superclasses,
    );
}

$xref_funcs{role} = $xref_funcs{parametricrole} = sub {
    my ($obj, $attr, $meth, $sup, @s) = $xref_funcs{module}->(@_);
    $obj->{attributes}     = [ map { attr_from_nam(@$_) } @$attr ];
    $obj->{methods}        = [ map { method_from_nam(@$_) } @$meth ];
    $obj->{superclasses}   = $sup;
    $obj;
};

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

sub method_from_nam {
    my ($name, $kind, $var, $body) = @_;
    bless { name => $name, kind => $kind, var => $var,
        body => $body }, 'Metamodel::Method';
}

sub attr_from_nam {
    my ($name, $public, $ivar, $ibody) = @_;
    bless { name => $name, public => $public, ivar => $ivar, ibody => $ibody },
        'Metamodel::Attribute';
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

sub parm_from_nam {
    my ($name, $flags, $slot, $names, $mdefault) = @_;
    return Sig::Parameter->new(
        slurpy   => !!($flags & 1),   slurpycap   => !!($flags & 2),
        rwtrans  => !!($flags & 4),   full_parcel => !!($flags & 8),
        optional => !!($flags & 16),  positional  => !!($flags & 32),
        readonly => !!($flags & 64),  list        => !!($flags & 128),
        hash     => !!($flags & 256),
        name => $name, slot => $slot, names => $names);
}


sub Metamodel::Lexical::Simple::to_nam {
    ['simple', ($_[0]->noinit ? 4 : 0) + ($_[0]->list ? 2 : 0) +
        ($_[0]->hash ? 1 : 0)]
}
sub Metamodel::Lexical::Common::to_nam { ['common', @{$_[0]->path}, $_[0]->name ] }
sub Metamodel::Lexical::Alias::to_nam {  ['alias', $_[0]->to] }
sub Metamodel::Lexical::Hint::to_nam {  ['hint'] }
sub Metamodel::Lexical::SubDef::to_nam { ['sub', @{ $_[0]->body->xref } ] }
sub Metamodel::Lexical::Stash::to_nam {  ['stash', @{ $_[0]->path } ] }

sub lex_from_nam {
    my ($name, $type, @xtra) = @_;
    return $name, Metamodel::Lexical::Simple->new
                        if $type eq 'simple';
    return $name, Metamodel::Lexical::Common->new(name => pop(@xtra),
        path => \@xtra) if $type eq 'common';
    return $name, Metamodel::Lexical::Alias->new(to => $xtra[0])
                        if $type eq 'alias';
    return $name, Metamodel::Lexical::Hint->new
                        if $type eq 'hint';
    return $name, Metamodel::Lexical::SubDef->new(body => $xref->[$xtra[1]])
                        if $type eq 'sub';
    return $name, Metamodel::Lexical::Stash->new(path => \@xtra)
                        if $type eq 'stash';
    die "weird lex type $type";
}

sub CgOpNode::to_nam {
    [ map { Scalar::Util::blessed($_) ? $_->to_nam : $_ } @{ $_[0] } ]
}

1;
