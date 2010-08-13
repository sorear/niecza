use strict;
use warnings;
use 5.010;

use CgOp;

{
    package Op;
    use Moose;

    # XXX Use raw-er StrPos stuff, and track more details
    has file => (isa => 'Str', is => 'ro');
    has line => (isa => 'Int', is => 'ro');

    sub zyg { }

    sub lift_decls {
        my ($self) = shift;
        map { $_->lift_decls } $self->zyg;
    }

    sub cgop {
        my ($self, $body) = @_;
        if (defined $self->file) {
            CgOp::ann($self->file, $self->line, $self->code($body));
        } else {
            $self->code($body);
        }
    }

    sub statement_level { shift }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::CgOp;
    use Moose;
    extends 'Op';

    has op => (is => 'ro', required => 1);

    sub code { $_[0]->op }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::StatementList;
    use Moose;
    extends 'Op';

    has children => (isa => 'ArrayRef[Op]', is => 'ro', required => 1);
    sub zyg { @{ shift()->children } }

    sub code {
        my ($self, $body) = @_;
        my @ch = map { $_->cgop($body) } @{ $self->children };
        # XXX should be Nil or something
        my $end = @ch ? pop(@ch) : CgOp::wrap(CgOp::null('object'));

        CgOp::prog((map { CgOp::sink($_) } @ch), $end);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::CallSub;
    use Moose;
    extends 'Op';

    has invocant    => (isa => 'Op', is => 'ro', required => 1);
    has positionals => (isa => 'ArrayRef[Op]', is => 'ro',
        default => sub { [] });
    sub zyg { $_[0]->invocant, @{ $_[0]->positionals } }

    sub code {
        my ($self, $body) = @_;
        CgOp::subcall(CgOp::fetch($self->invocant->cgop($body)),
            map { $_->cgop($body) } @{ $self->positionals });
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::YouAreHere;
    use Moose;
    extends 'Op';

    has unitname  => (isa => 'Str', is => 'ro', clearer => 'drop_unitname');
    has save_only => (isa => 'Bool', is => 'ro', default => 0);

    sub lift_decls {
        my $un = $_[0]->unitname; $_[0]->drop_unitname;
        Decl::SaveEnv->new(unitname => $un)
    }

    sub code {
        my ($self, $body) = @_;
        $self->save_only ? CgOp::null('Variable') :
            CgOp::subcall(CgOp::fetch(CgOp::scopedlex('!mainline')));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::CallMethod;
    use Moose;
    extends 'Op';

    has receiver    => (isa => 'Op', is => 'ro', required => 1);
    has positionals => (isa => 'ArrayRef[Op]', is => 'ro',
        default => sub { [] });
    has name        => (isa => 'Str', is => 'ro', required => 1);
    sub zyg { $_[0]->receiver, @{ $_[0]->positionals } }

    sub code {
        my ($self, $body) = @_;
        CgOp::methodcall($self->receiver->cgop($body),
            $self->name, map { $_->cgop($body) } @{ $self->positionals });
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::GetSlot;
    use Moose;
    extends 'Op';

    has object => (isa => 'Op', is => 'ro', required => 1);
    has name   => (isa => 'Str', is => 'ro', required => 1);
    sub zyg { $_[0]->object }

    sub code {
        my ($self, $body) = @_;
        CgOp::varattr($self->name, CgOp::fetch($self->object->cgop($body)));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

# or maybe we should provide Op::Let and let Actions do the desugaring?
{
    package Op::CallMetaMethod;
    use Moose;
    extends 'Op';

    has receiver    => (isa => 'Op', is => 'ro', required => 1);
    has positionals => (isa => 'ArrayRef[Op]', is => 'ro',
        default => sub { [] });
    has name        => (isa => 'Str', is => 'ro', required => 1);
    sub zyg { $_[0]->receiver, @{ $_[0]->positionals } }

    sub code {
        my ($self, $body) = @_;
        CgOp::let($self->receiver->cgop($body), sub {
            CgOp::methodcall(CgOp::newscalar(CgOp::how(CgOp::fetch($_[0]))),
                $self->name, $_[0], map { $_->cgop($body) }
                    @{ $self->positionals })});
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Paren;
    use Moose;
    extends 'Op';

    has inside => (isa => 'Op', is => 'ro', required => 1);
    sub zyg { $_[0]->inside }

    sub code {
        my ($self, $body) = @_;
        $self->inside->cgop($body);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::SimplePair;
    use Moose;
    extends 'Op';

    has key   => (isa => 'Op', is => 'ro', required => 1);
    has value => (isa => 'Op', is => 'ro', required => 1);
    sub zyg { $_[0]->key, $_[0]->value }

    sub code {
        my ($self, $body) = @_;
        CgOp::subcall(CgOp::fetch(CgOp::scopedlex('&infix:<=>>')),
            $self->key->cgop($body), $self->value->cgop($body));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::SimpleParcel;
    use Moose;
    extends 'Op';

    has items => (isa => 'ArrayRef[Op]', is => 'ro', required => 1);
    sub zyg { @{ $_[0]->items } }

    sub code {
        my ($self, $body) = @_;
        CgOp::subcall(CgOp::fetch(CgOp::scopedlex('&infix:<,>')),
            map { $_->cgop($body) } @{ $self->items });
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Interrogative;
    use Moose;
    extends 'Op';

    has receiver    => (isa => 'Op', is => 'ro', required => 1);
    has name        => (isa => 'Str', is => 'ro', required => 1);
    sub zyg { $_[0]->receiver }

    sub code {
        my ($self, $body) = @_;
        my $c = CgOp::fetch($self->receiver->cgop($body));
        given ($self->name) {
            when ("HOW") {
                $c = CgOp::how($c);
            }
            when ("WHAT") {
                $c = CgOp::getfield('typeObject',
                    CgOp::getfield('klass', CgOp::cast('DynObject', $c)));
            }
            default {
                die "Invalid interrogative $_";
            }
        }
        CgOp::newscalar($c);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::HereStub;
    use Moose;
    extends 'Op';

    has node => (is => 'ro', required => 1);

    sub zyg {
        my ($self) = @_;
        return ($self->node // {})->{doc}{_ast} //
            die("Here document used before body defined");
    }

    sub code {
        my ($self, $body) = @_;
        $self->zyg->cgop($body);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Yada;
    use Moose;
    extends 'Op';

    has kind => (isa => 'Str', is => 'ro', required => 1);

    sub code {
        my ($self, $body) = @_;

        CgOp::prog(
            CgOp::subcall(
                CgOp::fetch(CgOp::scopedlex("&warn")),
                CgOp::string_var(">>>Stub code executed<<<")
            ),
            CgOp::subcall(
                CgOp::fetch(CgOp::scopedlex("&exit")),
            ),
        );
    }
}

{
    package Op::ShortCircuit;
    use Moose;
    extends 'Op';

    has kind => (isa => 'Str', is => 'ro', required => 1);
    has args => (isa => 'ArrayRef[Op]', is => 'ro', required => 1);
    sub zyg { @{ $_[0]->args } }

    sub red2 {
        my ($self, $sym, $o2) = @_;
        given ($self->kind) {
            when ("&&") {
                return CgOp::ternary(CgOp::unbox('Boolean', CgOp::fetch(
                        CgOp::methodcall($sym, 'Bool'))), $o2, $sym);
            }
            when ("||") {
                return CgOp::ternary(CgOp::unbox('Boolean', CgOp::fetch(
                        CgOp::methodcall($sym, 'Bool'))), $sym, $o2);
            }
            when ("andthen") {
                return CgOp::ternary(CgOp::unbox('Boolean', CgOp::fetch(
                        CgOp::methodcall($sym, 'defined'))), $o2, $sym);
            }
            when ("//") {
                return CgOp::ternary(CgOp::unbox('Boolean', CgOp::fetch(
                        CgOp::methodcall($sym, 'defined'))), $sym, $o2);
            }
            default {
                die "That's not a sensible short circuit, now is it?";
            }
        }
    }

    sub code {
        my ($self, $body) = @_;

        my @r = reverse @{ $self->args };
        my $acc = (shift @r)->cgop($body);

        for (@r) {
            $acc = CgOp::let($_->cgop($body), sub { $self->red2($_[0], $acc) });
        }

        $acc;
    }
}

{
    package Op::StringLiteral;
    use Moose;
    extends 'Op';

    has text => (isa => 'Str', is => 'ro', required => 1);

    sub code {
        my ($self, $body) = @_;
        CgOp::string_var($self->text);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Conditional;
    use Moose;
    extends 'Op';

    has check => (isa => 'Op', is => 'ro', required => 1);
    has true  => (isa => 'Maybe[Op]', is => 'ro', required => 1);
    has false => (isa => 'Maybe[Op]', is => 'ro', required => 1);

    sub zyg { grep { defined } $_[0]->check, $_[0]->true, $_[0]->false }

    sub code {
        my ($self, $body) = @_;

        CgOp::ternary(
            CgOp::unbox('Boolean',
                CgOp::fetch(
                    CgOp::methodcall($self->check->cgop($body), "Bool"))),
            # XXX use Nil
            ($self->true ? $self->true->cgop($body) :
                CgOp::null('Variable')),
            ($self->false ? $self->false->cgop($body) :
                CgOp::null('Variable')));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::WhileLoop;
    use Moose;
    extends 'Op';

    has check => (isa => 'Op', is => 'ro', required => 1);
    has body  => (isa => 'Op', is => 'ro', required => 1);
    has once  => (isa => 'Bool', is => 'ro', required => 1);
    has until => (isa => 'Bool', is => 'ro', required => 1);
    sub zyg { $_[0]->check, $_[0]->body }

    sub code {
        my ($self, $body) = @_;

        CgOp::prog(
            CgOp::whileloop($self->until, $self->once,
                CgOp::unbox('Boolean',
                    CgOp::fetch(
                        CgOp::methodcall($self->check->cgop($body), "Bool"))),
                CgOp::sink($self->body->cgop($body))),
            CgOp::null('Variable'));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::ForLoop;
    use Moose;
    extends 'Op';

    has source => (isa => 'Op', is => 'ro', required => 1);
    has sink   => (isa => 'Op', is => 'ro', required => 1);
    has immed  => (isa => 'Bool', is => 'ro', default => 0);
    sub zyg { $_[0]->source, $_[0]->sink }

    sub code {
        my ($self, $body) = @_;

        CgOp::methodcall(
            CgOp::subcall(CgOp::fetch(CgOp::scopedlex('&flat')),
                $self->source->cgop($body)),
            ($self->immed ? 'for' : 'map'),
            $self->sink->cgop($body));
    }

    sub statement_level {
        my ($self) = @_;
        Op::ForLoop->new(source => $self->source, sink => $self->sink,
            immed => 1);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

# only for state $x will start and START{} in void context, yet
{
    package Op::Start;
    use Moose;
    extends 'Op';

    # possibly should use a raw boolean somehow
    has condvar => (isa => 'Str', is => 'ro', required => 1);
    has body => (isa => 'Op', is => 'ro', required => 1);
    sub zyg { $_[0]->body }

    sub lift_decls {
        my ($self) = @_;
        Decl::StateVar->new(backing => $self->condvar),
            $self->SUPER::lift_decls(@_);
    }

    sub code {
        my ($self, $body) = @_;

        CgOp::ternary(
            CgOp::unbox('Boolean',
                CgOp::fetch(
                    CgOp::methodcall(CgOp::scopedlex($self->condvar), "Bool"))),
            CgOp::wrap(CgOp::null('object')),
            CgOp::prog(
                CgOp::assign(CgOp::scopedlex($self->condvar),
                    CgOp::box('Bool', CgOp::bool(1))),
                $self->body->cgop($body)));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}


{
    package Op::Num;
    use Moose;
    extends 'Op';

    has value => (isa => 'Num', is => 'ro', required => 1);

    sub code {
        my ($self, $body) = @_;
        CgOp::box('Num', CgOp::double($self->value));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Bind;
    use Moose;
    extends 'Op';

    has lhs => (isa => 'Op', is => 'ro', required => 1);
    has rhs => (isa => 'Op', is => 'ro', required => 1);
    has readonly => (isa => 'Bool', is => 'ro', required => 1);
    sub zyg { $_[0]->lhs, $_[0]->rhs }

    sub code {
        my ($self, $body) = @_;
        CgOp::prog(
            CgOp::bind($self->readonly, $self->lhs->cgop($body),
                $self->rhs->cgop($body)),
            CgOp::null('Variable'));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::PackageDef;
    use Moose;
    extends 'Op';

    has name => (is => 'ro', isa => 'Str', predicate => 'has_name');
    has var  => (is => 'ro', isa => 'Str', required => 1);
    has bodyvar => (is => 'ro', isa => 'Str');
    has stub => (is => 'ro', isa => 'Bool', default => 0);
    has body => (is => 'ro', isa => 'Body');
    has exports => (is => 'ro', isa => 'ArrayRef[Str]', default => sub { [] });
    has ourpkg => (is => 'ro', isa => 'Maybe[ArrayRef[Str]]');

    sub decl_class { 'Decl::Package' }
    sub lift_decls {
        my ($self) = @_;
        my @r = $self->decl_class->new(stub => $self->stub, var => $self->var,
            ($self->has_name ? (name => $self->name) : ()),
            ($self->stub ? () : (body => $self->body,
                    bodyvar => $self->bodyvar)),
            ourpkg => $self->ourpkg);

        for my $tag (@{ $self->exports }) {
            for my $sym ($self->var, $self->var . '::') {
                push @r, Decl::PackageAlias->new(slot => $sym,
                    name => $sym, path => [ 'OUR', 'EXPORT', $tag ]);
            }
        }

        @r;
    }

    sub code {
        my ($self, $body) = @_;
        if ($self->stub) {
            CgOp::scopedlex($self->var);
        } else {
            CgOp::prog(
                CgOp::sink(CgOp::subcall(CgOp::fetch(
                            CgOp::scopedlex($self->bodyvar)))),
                CgOp::scopedlex($self->var));
        }
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::ModuleDef;
    use Moose;
    extends 'Op::PackageDef';

    sub decl_class { 'Decl::Module' }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::ClassDef;
    use Moose;
    extends 'Op::ModuleDef';

    sub decl_class { 'Decl::Class' }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::GrammarDef;
    use Moose;
    extends 'Op::ClassDef';

    sub decl_class { 'Decl::Grammar' }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::PreInit;
    use Moose;
    extends 'Op';

    has var    => (isa => 'Str', is => 'ro', predicate => 'has_var');
    has body   => (isa => 'Body', is => 'ro', required => 1);

    sub lift_decls {
        my ($self) = @_;
        Decl::PreInit->new(var => $self->var, code => $self->body);
    }

    sub code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->var);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Super;
    use Moose;
    extends 'Op';

    has name    => (isa => 'Str', is => 'ro');

    sub lift_decls {
        my ($self) = @_;
        Decl::Super->new(name => $self->name);
    }

    sub code {
        my ($self, $body) = @_;
        CgOp::null('Variable');
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Attribute;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro');
    has accessor => (isa => 'Bool', is => 'ro');

    sub lift_decls {
        my ($self) = @_;
        my @r;
        push @r, Decl::Attribute->new(name => $self->name);
        if ($self->accessor) {
            push @r, Decl::Sub->new(var => ($self->name . '!a'),
                code => Body->new(
                    name => $self->name,
                    signature => Sig->new(params => [])->for_method,
                    type => 'sub',
                    do => Op::GetSlot->new(
                        object => Op::Lexical->new(name => "self"),
                        name => $self->name)));
            push @r, Decl::HasMethod->new(name => $self->name,
                var => $self->name . '!a');
        }
        @r;
    }

    sub code {
        my ($self, $body) = @_;
        CgOp::null('Variable');
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Whatever;
    use Moose;
    extends 'Op';

    has slot => (isa => 'Str', is => 'ro', required => 1);

    sub code {
        my ($self, $body) = @_;
        CgOp::methodcall(CgOp::scopedlex('Whatever'), "new");
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::WhateverCode;
    use Moose;
    extends 'Op';

    has ops  => (isa => 'Op', is => 'ro', required => 1);
    has vars => (isa => 'ArrayRef[Str]', is => 'ro', required => 1);
    has slot => (isa => 'Str', is => 'ro', required => 1);

    sub lift_decls {
        my ($self) = @_;
        Decl::Sub->new(var => $self->slot, code => Body->new(
                name => "whatever-anon", type => 'sub', transparent => 1,
                do => $self->ops, signature => Sig->simple(@{ $self->vars })));
    }

    sub code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->slot);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::BareBlock;
    use Moose;
    extends 'Op';

    has var    => (isa => 'Str', is => 'ro', required => 1);
    has body   => (isa => 'Body', is => 'ro', required => 1);

    sub lift_decls {
        my ($self) = @_;
        Decl::Sub->new(var => $self->var, code => $self->body);
    }

    sub code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->var);
    }

    sub statement_level {
        Op::CallSub->new(invocant => $_[0]);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::SubDef;
    use Moose;
    extends 'Op';

    has var    => (isa => 'Str', is => 'ro', required => 1);
    has body   => (isa => 'Body', is => 'ro', required => 1);
    has class  => (isa => 'Str', is => 'ro', default => 'Sub');
    has ltm    => (isa => 'Maybe[CgOp]', is => 'ro', default => undef);
    has method_too => (isa => 'Maybe[Str]', is => 'ro', required => 0);
    has exports => (isa => 'ArrayRef[Str]', is => 'ro', default => sub { [] });

    sub lift_decls {
        my ($self) = @_;
        my @r;
        push @r, Decl::Sub->new(var => $self->var, class => $self->class,
            code => $self->body, ltm => $self->ltm);
        push @r, Decl::HasMethod->new(name => $self->method_too,
            var => $self->var) if defined($self->method_too);
        push @r, Decl::PackageAlias->new(slot => $self->var,
            name => $self->var, path => [ 'OUR', 'EXPORT', $_ ])
                for (@{ $self->exports });
        @r;
    }

    sub code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->var);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Lexical;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro', required => 1);
    has state_decl => (isa => 'Bool', is => 'ro', default => 0);

    has declaring => (isa => 'Bool', is => 'ro');
    has list => (isa => 'Bool', is => 'ro');
    has hash => (isa => 'Bool', is => 'ro');

    has state_backing => (isa => 'Str', is => 'ro');

    sub lift_decls {
        my ($self) = @_;
        return () unless $self->declaring;

        if ($self->state_backing) {
            return Decl::StateVar->new(slot => $self->name,
                    backing => $self->state_backing, list => $self->list,
                    hash => $self->hash);
        } else {
            return Decl::SimpleVar->new(slot => $self->name,
                    list => $self->list, hash => $self->hash);
        }
    }

    sub code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->name);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::ContextVar;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro', required => 1);

    sub code {
        my ($self, $body) = @_;
        CgOp::rawscall('Kernel.ContextHelper', CgOp::callframe,
            CgOp::clr_string($self->name));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::PackageVar;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro', required => 1);
    has slot => (isa => 'Str', is => 'ro', required => 1);
    has path => (isa => 'ArrayRef[Str]', is => 'ro', required => 1);

    sub looks_static {
        my ($self) = @_;
        my $v = $self->path->[0];
        if (!defined($v) || $v eq 'MY' || $v eq 'CALLER' || $v eq 'OUTER'
                || $v eq 'DYNAMIC') {
            return 0;
        } else {
            return 1;
        }
    }

    sub lift_decls {
        my ($self) = @_;
        $self->looks_static ?
            Decl::OurAlias->new(name => $self->name, slot => $self->slot,
                path => $self->path) :
            ();
    }

    sub code {
        my ($self, $body) = @_;
        $self->looks_static ? CgOp::scopedlex($self->slot) :
            ($body->lookup_var($self->name, @{ $self->path }))[1];
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Use;
    use Moose;
    extends 'Op';

    has unit => (isa => 'Str', is => 'ro', required => 1);
    has symbols => (isa => 'HashRef[ArrayRef[Str]]', is => 'ro', required => 1);

    sub lift_decls {
        my ($self) = @_;
        Decl::Use->new(unit => $self->unit, symbols => $self->symbols);
    }

    sub code { CgOp::null('Variable') }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Take;
    use Moose;
    extends 'Op';

    has value => (isa => 'Op', is => 'ro', required => 1);
    sub zyg { $_[0]->value }

    sub code {
        my ($self, $body) = @_;
        CgOp::rawsccall('Kernel.Take', $self->value->cgop($body));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Gather;
    use Moose;
    extends 'Op';

    has body => (isa => 'Body', is => 'ro', required => 1);
    has var  => (isa => 'Str',  is => 'ro', required => 1);

    sub lift_decls {
        my ($self) = @_;
        Decl::Sub->new(var => $self->var, code => $self->body);
    }

    sub code {
        my ($self, $body) = @_;

        # construct a frame for our sub ip=0
        # construct a GatherIterator with said frame
        # construct a List from the iterator

        CgOp::subcall(CgOp::fetch(CgOp::scopedlex('&_gather')),
            CgOp::newscalar(CgOp::rawsccall('Kernel.GatherHelper',
                    CgOp::fetch(CgOp::scopedlex($self->var)))));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}


1;
