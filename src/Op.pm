use strict;
use warnings;
use utf8;
use 5.010;

use CgOp;

{
    package Op;
    use Moose;

    # XXX Use raw-er StrPos stuff, and track more details
    has file => (isa => 'Str', is => 'ro');
    has line => (isa => 'Int', is => 'ro');

    sub zyg { }
    # This should be a conservative approximation of nonvoid context for
    # the optimizer; semantic contexts are very downplayed in Perl 6
    # and we can live without them for a while.
    sub ctxzyg { map { $_ => 1 } $_[0]->zyg }

    sub cgop {
        my ($self, $body) = @_;
        if (defined $self->file) {
            CgOp::ann($self->file, $self->line, $self->code($body));
        } else {
            $self->code($body);
        }
    }

    # A few words on the nature of bvalues
    # A bvalue cannot escape a sub; the return would always extract the
    # Variable.  Most ops don't return bvalues, nor expect them.  To avoid
    # the overhead of every Op::Lexical returning a bvalue, we do a very
    # primitive escape analysis here; only generate bvalues on the LHS of
    # binds.  Further, we don't need to generate the bvalues explicitly, since
    # we know they'll just be bound.
    sub code_bvalue {
        my ($self, $body, $ro, $rhscg) = @_;
        Carp::confess("Illegal use of " . ref($self) . " in bvalue context");
    }

    sub statement_level { shift }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::CgOp;
    use Moose;
    extends 'Op';

    has op => (is => 'ro');
    has optree => (is => 'ro');

    sub zyg {
        return () unless $_[0]->optree;
        our $rec; local $rec = sub {
            my ($node) = @_;
            blessed($node) ? ($node) :
                ref($node) ? (map { $rec->($_) } @$node) : ();
        };
        $rec->($_[0]->optree);
    }

    sub code {
        my ($self, $body) = @_;
        return $self->op if $self->op;
        our $rec; local $rec = sub {
            my ($node) = @_;
            return $node if !ref($node);
            return $node->code($body) if blessed($node);
            my ($cmd, @vals) = @$node;
            no strict 'refs';
            "CgOp::$cmd"->(map { $rec->($_) } @vals);
        };
        $rec->($self->optree);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::StatementList;
    use Moose;
    extends 'Op';

    has children => (isa => 'ArrayRef[Op]', is => 'ro', required => 1);
    sub zyg { @{ shift()->children } }
    sub ctxzyg {
        my ($self, $f) = @_;
        my @r = map { $_ => 0 } @{ $self->children };
        $r[-1] = $f if @r;
        @r;
    }

    sub code {
        my ($self, $body) = @_;
        my @ch = map { $_->cgop($body) } @{ $self->children };
        my $end = @ch ? pop(@ch) : CgOp::corelex('Nil');

        CgOp::prog((map { CgOp::sink($_) } @ch), $end);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::CallLike;
    use Moose;
    extends 'Op';

    has positionals => (isa => 'ArrayRef[Op]', is => 'ro',
        default => sub { [] });
    has args => (isa => 'ArrayRef[Op]', is => 'ro');
    sub zyg { @{ $_[0]->args // $_[0]->positionals } }

    sub getargs {
        $_[0]->args ? @{ $_[0]->args } :
            map { Op::Paren->new(inside => $_) } @{ $_[0]->positionals };
    }

    sub adverb {
        my ($self, $adv) = @_;
        my %h = %$self;
        delete $h{args};
        delete $h{positionals};
        blessed($self)->new(args => [ $self->getargs, $adv ], %h);
    }

    sub parsearglist {
        my ($body, @args) = @_;
        my @out;
        for my $a (@args) {
            if ($a->isa('Op::SimplePair')) {
                push @out, ":" . $a->key, $a->value->cgop($body);
            } elsif ($a->isa('Op::CallSub') && $a->invocant->isa('Op::Lexical')
                    && $a->invocant->name eq '&prefix:<|>') {
                push @out, 'flatcap', CgOp::fetch(CgOp::methodcall(
                    $a->positionals->[0]->cgop($body), 'Capture'));
            } else {
                push @out, $a->cgop($body);
            }
        }
        @out;
    }

    sub argblock {
        my ($self, $body) = @_;
        if (! $self->args) {
            return map { $_->cgop($body) } @{ $self->positionals };
        }
        parsearglist($body, @{ $self->args });
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::CallSub;
    use Moose;
    extends 'Op::CallLike';

    has invocant    => (isa => 'Op', is => 'ro', required => 1);
    sub zyg { $_[0]->invocant, $_[0]->SUPER::zyg }

    sub code {
        my ($self, $body) = @_;
        CgOp::subcall(CgOp::fetch($self->invocant->cgop($body)),
            $self->argblock($body));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::YouAreHere;
    use Moose;
    extends 'Op';

    has unitname  => (isa => 'Str', is => 'ro', clearer => 'drop_unitname');

    sub code {
        my ($self, $body) = @_;
        # this should be a little fancier so closure can work
        CgOp::subcall(CgOp::fetch(CgOp::context_get(CgOp::clr_string(
                        '*resume_' . $self->unitname), CgOp::int(0))));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::CallMethod;
    use Moose;
    extends 'Op::CallLike';

    has receiver    => (isa => 'Op', is => 'ro', required => 1);
    has name        => (isa => 'Str', is => 'ro', required => 1);
    has private     => (isa => 'Bool', is => 'ro', default => 0);
    has ppath       => (isa => 'Maybe[ArrayRef[Str]]', is => 'ro');
    has pclass      => (isa => 'ArrayRef', is => 'rw');
    sub zyg { $_[0]->receiver, $_[0]->SUPER::zyg }

    sub code {
        my ($self, $body) = @_;
        if ($self->private) {
            CgOp::subcall(CgOp::stab_privatemethod(
                    CgOp::class_ref('mo', @{ $self->pclass }),
                    CgOp::clr_string($self->name)),
                $self->receiver->cgop($body), $self->argblock($body));
        } else {
            CgOp::methodcall($self->receiver->cgop($body),
                $self->name, $self->argblock($body));
        }
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
    extends 'Op::CallLike';

    has receiver    => (isa => 'Op', is => 'ro', required => 1);
    has name        => (isa => 'Str', is => 'ro', required => 1);
    sub zyg { $_[0]->receiver, $_[0]->SUPER::zyg }

    sub code {
        my ($self, $body) = @_;
        CgOp::let($self->receiver->cgop($body), sub {
            CgOp::methodcall(CgOp::newscalar(CgOp::how(CgOp::fetch($_[0]))),
                $self->name, $_[0], $self->argblock($body))});
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
    sub ctxzyg { $_[0]->inside, $_[1] }

    sub code {
        my ($self, $body) = @_;
        $self->inside->cgop($body);
    }

    sub code_bvalue {
        my ($self, $body, $ro, $rhscg) = @_;
        $self->inside->code_bvalue($body, $ro, $rhscg);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::SimplePair;
    use Moose;
    extends 'Op';

    has key   => (isa => 'Str', is => 'ro', required => 1);
    has value => (isa => 'Op', is => 'ro', required => 1);
    sub zyg { $_[0]->value }

    sub code {
        my ($self, $body) = @_;
        CgOp::subcall(CgOp::fetch(CgOp::corelex('&infix:<=>>')),
            CgOp::string_var($self->key), $self->value->cgop($body));
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
        CgOp::subcall(CgOp::fetch(CgOp::corelex('&infix:<,>')),
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
                $c = CgOp::obj_what($c);
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
                CgOp::fetch(CgOp::corelex("&warn")),
                CgOp::string_var(">>>Stub code executed<<<")
            ),
            CgOp::subcall(
                CgOp::fetch(CgOp::corelex("&exit")),
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
                return CgOp::ternary(CgOp::unbox('bool', CgOp::fetch(
                        CgOp::methodcall($sym, 'Bool'))), $o2, $sym);
            }
            when ("||") {
                return CgOp::ternary(CgOp::unbox('bool', CgOp::fetch(
                        CgOp::methodcall($sym, 'Bool'))), $sym, $o2);
            }
            when ("andthen") {
                return CgOp::ternary(CgOp::unbox('bool', CgOp::fetch(
                        CgOp::methodcall($sym, 'defined'))), $o2, $sym);
            }
            when ("//") {
                return CgOp::ternary(CgOp::unbox('bool', CgOp::fetch(
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
    sub ctxzyg {
        $_[0]->check, 1,
        map { defined($_) ? ($_, $_[1]) : () } $_[0]->true, $_[0]->false;
    }

    sub code {
        my ($self, $body) = @_;

        CgOp::ternary(
            CgOp::unbox('bool',
                CgOp::fetch(
                    CgOp::methodcall($self->check->cgop($body), "Bool"))),
            ($self->true ? $self->true->cgop($body) :
                CgOp::corelex('Nil')),
            ($self->false ? $self->false->cgop($body) :
                CgOp::corelex('Nil')));
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
    sub ctxzyg { $_[0]->check, 1, $_[0]->body, 0 }

    sub code {
        my ($self, $body) = @_;
        my $id = Niecza::Actions->genid;

        CgOp::prog(
            CgOp::whileloop($self->until, $self->once,
                CgOp::unbox('bool',
                    CgOp::fetch(
                        CgOp::methodcall($self->check->cgop($body), "Bool"))),
                CgOp::prog(
                    CgOp::label("redo$id"),
                    CgOp::sink($self->body->cgop($body)),
                    CgOp::label("next$id"),
                    CgOp::ehspan(1, undef, 0, "redo$id", "next$id", "next$id"),
                    CgOp::ehspan(2, undef, 0, "redo$id", "next$id", "last$id"),
                    CgOp::ehspan(3, undef, 0, "redo$id", "next$id", "redo$id"))),
            CgOp::label("last$id"),
            CgOp::corelex('Nil'));
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
    sub zyg { $_[0]->source, $_[0]->sink }

    sub code {
        my ($self, $body) = @_;

        CgOp::methodcall(
            CgOp::subcall(CgOp::fetch(CgOp::corelex('&flat')),
                $self->source->cgop($body)), 'map', $self->sink->cgop($body));
    }

    sub statement_level {
        my ($self) = @_;
        my $var = Niecza::Actions->gensym;
        $self->sink->once(1);
        Op::ImmedForLoop->new(source => $self->source, var => $var,
            sink => Op::CallSub->new(invocant => $self->sink,
                positionals => [ Op::LetVar->new(name => $var) ]));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::ImmedForLoop;
    use Moose;
    extends 'Op';

    has var    => (isa => 'Str', is => 'ro', required => 1);
    has source => (isa => 'Op', is => 'ro', required => 1);
    has sink   => (isa => 'Op', is => 'ro', required => 1);
    sub zyg { $_[0]->source, $_[0]->sink }
    sub ctxzyg { $_[0]->source, 1, $_[0]->sink, 0 }

    sub code {
        my ($self, $body) = @_;

        my $id = Niecza::Actions->genid;

        CgOp::rnull(CgOp::letn(
            "!iter$id", CgOp::unbox('vvarlist', CgOp::fetch(CgOp::methodcall(
                    $self->source->cgop($body), 'iterator'))),
            $self->var, CgOp::null('var'),
            CgOp::whileloop(0, 0,
                CgOp::iter_hasflat(CgOp::letvar("!iter$id")),
                CgOp::prog(
                    CgOp::letvar($self->var,
                        CgOp::vvarlist_shift(CgOp::letvar("!iter$id"))),
                    CgOp::label("redo$id"),
                    CgOp::sink($self->sink->cgop($body)),
                    CgOp::label("next$id"),
                    CgOp::ehspan(1, undef, 0, "redo$id", "next$id", "next$id"),
                    CgOp::ehspan(2, undef, 0, "redo$id", "next$id", "last$id"),
                    CgOp::ehspan(3, undef, 0, "redo$id", "next$id", "redo$id"))),
            CgOp::label("last$id")));
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
    sub ctxzyg { $_[0]->body, $_[1] }

    sub code {
        my ($self, $body) = @_;

        CgOp::ternary(
            CgOp::unbox('bool',
                CgOp::fetch(
                    CgOp::methodcall(CgOp::scopedlex($self->condvar), "Bool"))),
            CgOp::corelex('Nil'),
            CgOp::prog(
                CgOp::assign(CgOp::scopedlex($self->condvar),
                    CgOp::box('Bool', CgOp::bool(1))),
                $self->body->cgop($body)));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Try;
    use Moose;
    extends 'Op';

    has body => (isa => 'Op', is => 'ro', required => 1);
    sub zyg { $_[0]->body }

    sub code {
        my ($self, $body) = @_;

        my $id = Niecza::Actions->genid;

        CgOp::prog(
            CgOp::ehspan(5, undef, 0, "start$id", "end$id", "end$id"),
            CgOp::span("start$id", "end$id", 1, $self->body->cgop($body)));
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
        $self->lhs->code_bvalue($body, $self->readonly, $self->rhs->cgop($body))
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Augment;
    use Moose;
    extends 'Op';

    has name => (is => 'ro', isa => 'Str');
    has bodyvar => (is => 'ro', isa => 'Str');
    has body => (is => 'ro', isa => 'Body');
    has pkg => (is => 'ro', isa => 'ArrayRef[Str]');

    sub code {
        my ($self, $body) = @_;
        CgOp::subcall(CgOp::fetch(CgOp::scopedlex($self->bodyvar)));
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

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::RoleDef;
    use Moose;
    extends 'Op::ModuleDef';

    has signature => (isa => 'Sig', is => 'ro');

    sub code {
        my ($self, $body) = @_;
        if ($self->signature) {
            CgOp::scopedlex($self->var);
        } else {
            $self->SUPER::code($body);
        }
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::ClassDef;
    use Moose;
    extends 'Op::ModuleDef';

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::GrammarDef;
    use Moose;
    extends 'Op::ClassDef';

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Super;
    use Moose;
    extends 'Op';

    has name    => (isa => 'Str', is => 'ro');
    has path    => (isa => 'Maybe[ArrayRef[Str]]', is => 'ro');

    sub code {
        my ($self, $body) = @_;
        CgOp::corelex('Nil');
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

    sub code {
        my ($self, $body) = @_;
        CgOp::corelex('Nil');
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
        CgOp::methodcall(CgOp::corelex('Whatever'), "new");
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

    sub code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->var);
    }

    sub statement_level {
        $_[0]->body->type('voidbare');
        Op::CallSub->new(invocant => Op::SubDef->new(var => $_[0]->var,
                body => $_[0]->body, once => 1));
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
    has method_too => (isa => 'Maybe[ArrayRef[Str]]', is => 'ro');
    has proto_too => (isa => 'Maybe[Str]', is => 'ro', required => 0);
    has exports => (isa => 'ArrayRef[Str]', is => 'ro', default => sub { [] });
    # Is candidate for beta-optimization.  Not compatible with method_too,
    # proto_too, exports, ltm
    has once   => (isa => 'Bool', is => 'rw', default => 0);

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

    sub code {
        my ($self, $body) = @_;
        CgOp::scopedlex($self->name);
    }

    sub code_bvalue {
        my ($self, $body, $ro, $rhscg) = @_;
        CgOp::prog(
            CgOp::scopedlex($self->name, CgOp::newboundvar($ro, $self->list || $self->hash, $rhscg)),
            CgOp::scopedlex($self->name));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::ConstantDecl;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro', required => 1);
    has init => (isa => 'Op', is => 'rw');
    has path => (isa => 'Maybe[ArrayRef]', is => 'ro');

    sub code {
        my ($self, $body) = @_;
        CgOp::rnull(CgOp::scopedlex($self->name, $self->init->cgop($body)));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::ContextVar;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro', required => 1);
    has uplevel => (isa => 'Int', is => 'ro', default => 0);

    sub code {
        my ($self, $body) = @_;
        CgOp::context_get(CgOp::clr_string($self->name), CgOp::int($self->uplevel));
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
    has list => (isa => 'Bool', is => 'ro', default => 0);
    has hash => (isa => 'Bool', is => 'ro', default => 0);

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

    sub code {
        my ($self, $body) = @_;
        $self->looks_static ? CgOp::scopedlex($self->slot) :
            CgOp::bget(($body->lookup_var($self->name, @{ $self->path }))[1]);
    }

    sub code_bvalue {
        my ($self, $body, $ro, $rhscg) = @_;
        $self->looks_static ?
            CgOp::prog(
                CgOp::scopedlex($self->slot,
                    CgOp::newboundvar($ro, $self->list || $self->hash, $rhscg)),
                CgOp::scopedlex($self->slot)) :
            CgOp::letn('!bv', ($body->lookup_var($self->name,
                        @{ $self->path }))[1],
                CgOp::bset(CgOp::letvar('!bv'), CgOp::newboundvar($ro,
                        $self->list || $self->hash, $rhscg)),
                CgOp::bget(CgOp::letvar('!bv')));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Use;
    use Moose;
    extends 'Op';

    has unit => (isa => 'Str', is => 'ro', required => 1);

    sub code { CgOp::corelex('Nil') }

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
        CgOp::take($self->value->cgop($body));
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

    sub code {
        my ($self, $body) = @_;

        # construct a frame for our sub ip=0
        # construct a GatherIterator with said frame
        # construct a List from the iterator

        CgOp::subcall(CgOp::fetch(CgOp::corelex('&_gather')),
            CgOp::newscalar(CgOp::startgather(
                    CgOp::fetch(CgOp::scopedlex($self->var)))));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

# the existance of these complicates cross-sub inlining a bit
{
    package Op::MakeCursor;
    use Moose;
    extends 'Op';

    sub code {
        my ($self, $body) = @_;

        CgOp::prog(
            CgOp::scopedlex('$*/', CgOp::newscalar(CgOp::rxcall('MakeCursor'))),
            CgOp::scopedlex('$*/'));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::LetVar;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro', required => 1);

    sub code { CgOp::letvar($_[0]->name); }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::RegexBody;
    use Moose;
    extends 'Op';

    has rxop => (isa => 'RxOp', is => 'ro', required => 1);
    has name => (isa => 'Str', is => 'ro', default => '');
    has passcap => (isa => 'Bool', is => 'ro', default => 0);
    has passcut => (isa => 'Bool', is => 'ro', default => 0);
    has pre => (isa => 'ArrayRef[Op]', is => 'ro', default => sub { [] });
    has canback => (isa => 'Bool', is => 'ro', default => 1);

    sub zyg { @{ $_[0]->pre }, $_[0]->rxop->opzyg }

    sub code {
        my ($self, $body) = @_;

        my @mcaps;
        local $::in_quant = 0;
        if (!$self->passcap) {
            my $u = $self->rxop->used_caps;
            for (keys %$u) {
                push @mcaps, $_ if $u->{$_} >= 2;
            }
        }
        my @pre = map { CgOp::sink($_->code($body)) } @{ $self->pre };

        CgOp::prog(
            @pre,
            CgOp::rxinit(CgOp::clr_string($self->name),
                    CgOp::cast('cursor', CgOp::fetch(CgOp::scopedlex('self'))),
                    $self->passcap, $self->passcut),
            ($self->passcap ? () :
                CgOp::rxpushcapture(CgOp::null('cursor'), @mcaps)),
            $self->rxop->code($body),
            ($self->canback ? CgOp::rxend() : CgOp::rxfinalend()),
            CgOp::label('backtrack'),
            CgOp::rxbacktrack(),
            CgOp::null('var'));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

### BEGIN DESUGARING OPS
# These don't appear in source code, but are used by other ops to preserve
# useful structure.

# used after β-reductions
{
    package Op::SigBind;
    use Moose;
    extends 'Op';

    has signature   => (isa => 'Sig', is => 'ro', required => 1);
    # positionals *really* should be a bunch of gensym Lexical's, or else
    # you risk shadowing hell.  this needs to be handled at a different level
    has positionals => (isa => 'ArrayRef[Op]', is => 'ro', required => 1);

    sub zyg { @{ $_[0]->positionals } }

    sub code {
        my ($self, $body) = @_;

        CgOp::prog(
            $self->signature->bind_inline($body,
                map { $_->cgop($body) } @{ $self->positionals }),
            CgOp::null('var'));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Assign;
    use Moose;
    extends 'Op';

    has lhs => (isa => 'Op', is => 'ro', required => 1);
    has rhs => (isa => 'Op', is => 'ro', required => 1);
    sub zyg { $_[0]->lhs, $_[0]->rhs }

    sub code {
        my ($self, $body) = @_;
        CgOp::rnull(CgOp::assign($self->lhs->cgop($body), $self->rhs->cgop($body)));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
{
    package Op::Let;
    use Moose;
    extends 'Op';

    has var  => (isa => 'Str', is => 'ro', required => 1);
    has type => (isa => 'Str', is => 'ro');
    has to   => (isa => 'Op',  is => 'ro');
    has in   => (isa => 'Op',  is => 'ro', required => 1);

    sub zyg { ($_[0]->to ? ($_[0]->to) : ()), $_[0]->in }

    sub code {
        my ($self, $body) = @_;

        CgOp::letn($self->var,
            ($self->to ? $self->to->cgop($body) : CgOp::null($self->type)),
            $self->in->cgop($body));
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
