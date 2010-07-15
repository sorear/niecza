use strict;
use warnings;
use 5.010;
use CodeGen ();

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
        $self->do->cg($cg, $self);
        # TODO: Bind a return value here to catch non-ro sub use
        $cg->return(1) unless $cg->unreach;
        return $cg;
    }

    sub do_enter {
        my ($self, $cg) = @_;
        $cg->lextypes($_, 'Variable') for keys %{ $self->lexical };
        $_->do_enter($cg, $self) for @{ $self->decls };
        $self->signature->gen_binder($cg) if $self->signature;
        for (@{ $self->enter }) {
            $_->cg($cg, $self);
            $cg->drop;
        }
    }

    sub write {
        my ($self) = @_;
        $self->code->write;
        $_->write($self) for (@{ $self->decls });
    }

    sub do_preinit {
        my ($self, $cg) = @_;
        $_->do_preinit($cg, $self) for @{ $self->decls };
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
        my ($self, $cg) = @_;
        $cg->lextypes('!plist', 'List<DynMetaObject>');
        $cg->clr_new('List<DynMetaObject>', 0);
        $cg->lexput(0, '!plist');

        for my $super (@{ $self->super }) {
            $cg->lexget(0, '!plist');
            $cg->scopelexget($super, $self);
            $cg->fetch;
            $cg->cast('DynObject');
            $cg->clr_field_get('klass');
            $cg->clr_call_virt('Add', 1);
        }
        $cg->lexget(1, $self->var . '!HOW');
        $cg->dup_fetch;
        $cg->callframe;
        $cg->clr_wrap;
        $cg->lexget(0, '!plist');
        $cg->clr_wrap;
        $cg->call_method(1, "create-protoobject", 2);
        $cg->lexput(1, $self->var);
    }

    before do_enter => sub {
        my ($self, $cg) = @_;
        $cg->share_lex('!scopenum');
        $self->makeproto($cg);
    };

    around do_preinit => sub {
        my ($o, $self, $cg) = @_;
        $self->lexical->{'!scopenum'} = 1;
        $o->($self, $cg);
        $self->makeproto($cg);
    };

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
