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
    has lexical   => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has outer     => (isa => 'Body', is => 'rw', init_arg => undef);
    has decls     => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has codegen   => (isa => 'CodeGen', is => 'rw');
    has signature => (isa => 'Maybe[Sig]', is => 'ro');

    sub code {
        my ($self) = @_;
        if ($self->codegen) { return $self->codegen }
        $self->codegen(CodeGen->new(name => $self->name, body => $self));
        my $cg = $self->codegen;
        $self->do_enter($cg);
        $self->do->code($self)->var_cg($cg);
        # TODO: Bind a return value here to catch non-ro sub use
        $cg->return(1) unless $cg->unreach;
        return $cg;
    }

    sub do_enter {
        my ($self, $cg) = @_;
        $cg->lextypes($_, 'Variable') for keys %{ $self->lexical };
        $_->enter_code($self)->var_cg($cg) for @{ $self->decls };
        $self->signature->gen_binder($cg) if $self->signature;
        for (@{ $self->enter }) {
            CgOp::sink($_->code($self))->var_cg($cg);
        }
    }

    sub write {
        my ($self) = @_;
        $self->code->write;
        $_->write($self) for (@{ $self->decls });
    }

    sub preinit_code {
        my ($self) = @_;
        CgOp::prog(map { $_->preinit_code($self) } @{ $self->decls });
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

# Like a normal body, but creates a protoobject during preinit and run!
{
    package Body::Class;
    use Moose;
    extends 'Body';

    has 'var'        => (is => 'rw', isa => 'Str');
    has 'super'      => (is => 'ro', isa => 'ArrayRef', default => sub { [] });
    has 'augmenting' => (is => 'ro', isa => 'Bool', default => 0);

    sub makeproto {
        my ($self) = @_;
        my @p;
        push @p, CgOp::lextypes('!plist', 'List<DynMetaObject>');
        push @p, CgOp::lexput(0, '!plist',
            CgOp::rawnew('List<DynMetaObject>'));

        for my $super (@{ $self->super }) {
            push @p, CgOp::rawcall(CgOp::lexget(0, '!plist'), 'Add',
                CgOp::getfield('klass',
                    CgOp::cast('DynObject',
                        CgOp::fetch(CgOp::scopedlex($super)))));
        }
        push @p, CgOp::lexput(1, $self->var,
            CgOp::methodcall(
                CgOp::lexget(1, $self->var . '!HOW'), 'create-protoobject',
                CgOp::wrap(CgOp::callframe),
                CgOp::wrap(CgOp::lexget(0, '!plist'))));
        CgOp::prog(@p);
    }

    before do_enter => sub {
        my ($self, $cg) = @_;
        $cg->share_lex('!scopenum');
        $self->makeproto->var_cg($cg);
    };

    around preinit_code => sub {
        my ($o, $self) = @_;
        $self->lexical->{'!scopenum'} = 1;
        CgOp::prog(
            $o->($self),
            $self->makeproto);
    };

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
