use strict;
use warnings;
use 5.010;
use CodeGen ();

{
    package Body;
    use Moose;

    has name    => (isa => 'Str', is => 'rw', default => "anon");
    has do      => (isa => 'Op', is => 'rw');
    has enter   => (isa => 'ArrayRef[Op]', is => 'ro',
        default => sub { [] });
    has lexical => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has outer   => (isa => 'Body', is => 'rw', init_arg => undef);
    has decls   => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has codegen => (isa => 'CodeGen', is => 'rw');

    sub code {
        my ($self) = @_;
        if ($self->codegen) { return $self->codegen }
        $self->codegen(CodeGen->new(name => $self->name));
        my $cg = $self->codegen;
        $self->do_enter($cg);
        $self->do->item_cg($cg, $self);
        # TODO: Bind a return value here to catch non-ro sub use
        $cg->return(1) unless $cg->unreach;
        return $cg;
    }

    sub do_enter {
        my ($self, $cg) = @_;
        $cg->lextypes($_, 'Variable') for keys %{ $self->lexical };
        $_->do_enter($cg, $self) for @{ $self->decls };
        $_->void_cg($cg, $self) for @{ $self->enter };
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

    has 'var' => (is => 'rw', isa => 'Str');

    sub makeproto {
        my ($self, $cg) = @_;
        $cg->lextypes('!plist', 'List<DynMetaObject>');
        $cg->clr_new('List<DynMetaObject>', 0);
        $cg->lexput(0, '!plist');
        # TODO handle superclasses here!
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
        $self->makeproto($cg);
    };

    after do_preinit => sub {
        my ($self, $cg) = @_;
        $self->makeproto($cg);
    };

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
