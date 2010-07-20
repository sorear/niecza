use 5.010;
use MooseX::Declare;
use CodeGen ();
use CgOp ();

class Body {
    has name      => (isa => 'Str', is => 'rw', default => "anon");
    has do        => (isa => 'Op', is => 'rw');
    has enter     => (isa => 'ArrayRef[Op]', is => 'ro',
        default => sub { [] });
    has lexical   => (isa => 'HashRef', is => 'ro', default => sub { +{} });
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

    method is_mainline () {
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

    method gen_code () {
        # TODO: Bind a return value here to catch non-ro sub use
        CodeGen->new(name => $self->name, body => $self,
            ops => CgOp::prog($self->enter_code,
                CgOp::return($self->do->code($self))));
    }

    method enter_code () {
        my @p;
        push @p, CgOp::lextypes(map { $_, 'Variable' }
            keys %{ $self->lexical });
        push @p, map { $_->enter_code($self) } @{ $self->decls };
        push @p, $self->signature->binder if $self->signature;
        push @p, map { CgOp::sink($_->code($self)) } @{ $self->enter };
        CgOp::prog(@p);
    }

    method write () {
        $self->code->write;
        $_->write($self) for (@{ $self->decls });
    }

    method preinit_code () {
        CgOp::prog(map { $_->preinit_code($self) } @{ $self->decls });
    }
}

1;
