use strict;
use warnings;
use 5.010;
use CodeGen ();
use CgOp ();

{
    package Body;
    use Moose;

    has name      => (isa => 'Str', is => 'rw', default => "anon");
    has do        => (isa => 'Op', is => 'rw');
    has enter     => (isa => 'ArrayRef[Op]', is => 'ro',
        default => sub { [] });
    has outer     => (isa => 'Body', is => 'rw', init_arg => undef);
    has decls     => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has code      => (isa => 'CodeGen', is => 'ro', init_arg => undef,
        lazy => 1, builder => 'gen_code');
    has signature => (isa => 'Maybe[Sig]', is => 'ro');
    has mainline  => (isa => 'Bool', is => 'ro', lazy => 1,
        builder => 'is_mainline');
    # currently used types are phaser, loop, cond, class, mainline, bare, sub
    # also '' for incorrectly contextualized {p,x,}block, blast
    has type      => (isa => 'Str', is => 'rw');

    has lexical   => (isa => 'HashRef[Str]', is => 'ro', lazy_build => 1);
    # my $x inside, floats out; mostly for blasts; set by context so must be rw
    has transparent => (isa => 'Bool', is => 'rw', default => 0);

    sub _build_lexical {
        my ($self) = @_;

        +{ map { $_->used_slots } @{ $self->_alldecls } };
    }

    sub is_mainline {
        my $self = shift;

        if ($self->type && $self->type eq 'mainline') {
            return 1;
        }

        if (!($self->type) || !($self->outer)) {
            die "Critical phase error";
        }

        if ($self->type eq 'bare' || $self->type eq 'class') {
            return $self->outer->mainline;
        } else {
            return 0;
        }
    }

    sub floated_decls {
        my ($self) = @_;
        if ($self->transparent) {
            $self->do->local_decls;
        } else {
            ();
        }
    }

    has _alldecls => (isa => 'ArrayRef[Decl]', is => 'ro', lazy_build => 1);
    sub _build__alldecls {
        my ($self) = @_;
        my @x = @{ $self->decls };
        unshift @x, $self->do->local_decls if !$self->transparent;
        unshift @x, map { $_->extra_decls } @x;
        \@x;
    }

    sub gen_code {
        my ($self) = @_;
        # TODO: Bind a return value here to catch non-ro sub use
        CodeGen->new(name => $self->name, body => $self,
            lex2type => +{ %{ $self->lexical } },
            ops => CgOp::prog($self->enter_code,
                CgOp::return($self->do->code($self))));
    }

    sub enter_code {
        my ($self) = @_;
        my @p;
        push @p, map { $_->enter_code($self) } @{ $self->_alldecls };
        push @p, $self->signature->binder if $self->signature;
        push @p, map { CgOp::sink($_->code($self)) } @{ $self->enter };
        CgOp::prog(@p);
    }

    sub write {
        my ($self) = @_;
        $self->code->write;
        $_->write($self) for (@{ $self->_alldecls });
    }

    sub preinit_code {
        my ($self) = @_;
        CgOp::prog(map { $_->preinit_code($self) } @{ $self->_alldecls });
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
