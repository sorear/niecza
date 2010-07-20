use 5.010;
use MooseX::Declare;

use CgOp;

class Op {
    method paren { $self }
}

class Op::NIL extends Op {
    has ops => (isa => 'ArrayRef', is => 'ro', required => 1);

    method code ($body) {
        CgOp::nil(map { blessed $_ ? $_->code($body) : $_ } @{ $self->ops });
    }
}

class Op::CgOp extends Op {
    has op => (is => 'ro', required => 1);

    method code { $self->op }
}

class Op::StatementList extends Op {
    has children => (isa => 'ArrayRef[Op]', is => 'ro', required => 1);

    method code ($body) {
        my @ch = map { $_->code($body) } @{ $self->children };
        # XXX should be Nil or something
        my $end = @ch ? pop(@ch) : CgOp::wrap(CgOp::null('object'));

        CgOp::prog((map { CgOp::sink($_) } @ch), $end);
    }
}

class Op::CallSub extends Op {
    has invocant    => (isa => 'Op', is => 'ro', required => 1);
    has positionals => (isa => 'ArrayRef[Op]', is => 'ro',
        default => sub { [] });
    # non-parenthesized constructor
    has splittable_pair => (isa => 'Bool', is => 'rw', default => 0);
    has splittable_parcel => (isa => 'Bool', is => 'rw', default => 0);

    method paren () {
        Op::CallSub->new(invocant => $self->invocant,
            positionals => $self->positionals);
    }

    method code ($body) {
        CgOp::subcall(CgOp::fetch($self->invocant->code($body)),
            map { $_->code($body) } @{ $self->positionals });
    }
}

class Op::CallMethod extends Op {
    has receiver    => (isa => 'Op', is => 'ro', required => 1);
    has positionals => (isa => 'ArrayRef[Op]', is => 'ro',
        default => sub { [] });
    has name        => (isa => 'Str', is => 'ro', required => 1);

    method code ($body) {
        CgOp::methodcall($self->receiver->code($body),
            $self->name, map { $_->code($body) } @{ $self->positionals });
    }
}

class Op::GetSlot extends Op {
    has object => (isa => 'Op', is => 'ro', required => 1);
    has name   => (isa => 'Str', is => 'ro', required => 1);

    method code ($body) {
        CgOp::varattr($self->name, CgOp::fetch($self->object->code($body)));
    }
}

# or maybe we should provide Op::Let and let Actions do the desugaring?
class Op::CallMetaMethod extends Op {
    has receiver    => (isa => 'Op', is => 'ro', required => 1);
    has positionals => (isa => 'ArrayRef[Op]', is => 'ro',
        default => sub { [] });
    has name        => (isa => 'Str', is => 'ro', required => 1);

    method code ($body) {
        CgOp::let($self->receiver->code($body), 'Variable', sub {
            CgOp::methodcall(CgOp::newscalar(CgOp::how(CgOp::fetch($_[0]))),
                $self->name, $_[0], map { $_->code($body) }
                    @{ $self->positionals })});
    }
}

class Op::Interrogative extends Op {
    has receiver    => (isa => 'Op', is => 'ro', required => 1);
    has name        => (isa => 'Str', is => 'ro', required => 1);

    method code ($body) {
        my $c = CgOp::fetch($self->receiver->code($body));
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
}

class Op::Yada extends Op {
    has kind => (isa => 'Str', is => 'ro', required => 1);

    method code ($body) {

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

class Op::ShortCircuit extends Op {
    has kind => (isa => 'Str', is => 'ro', required => 1);
    has args => (isa => 'ArrayRef', is => 'ro', required => 1);

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

    method code ($body) {

        my @r = reverse @{ $self->args };
        my $acc = (shift @r)->code($body);

        for (@r) {
            $acc = CgOp::let($_->code($body), 'Variable',
                sub { $self->red2($_[0], $acc) });
        }

        $acc;
    }
}

class Op::StringLiteral extends Op {
    has text => (isa => 'Str', is => 'ro', required => 1);

    method code ($body) {
        CgOp::string_var($self->text);
    }
}

class Op::Conditional extends Op {
    has check => (isa => 'Op', is => 'ro', required => 1);
    has true  => (isa => 'Maybe[Op]', is => 'ro', required => 1);
    has false => (isa => 'Maybe[Op]', is => 'ro', required => 1);

    method code ($body) {

        CgOp::ternary(
            CgOp::unbox('Boolean',
                CgOp::fetch(
                    CgOp::methodcall($self->check->code($body), "Bool"))),
            # XXX use Nil
            ($self->true ? $self->true->code($body) :
                CgOp::null('Variable')),
            ($self->false ? $self->false->code($body) :
                CgOp::null('Variable')));
    }
}

class Op::WhileLoop extends Op {
    has check => (isa => 'Op', is => 'ro', required => 1);
    has body  => (isa => 'Op', is => 'ro', required => 1);
    has once  => (isa => 'Bool', is => 'ro', required => 1);
    has until => (isa => 'Bool', is => 'ro', required => 1);

    method code ($body) {
        CgOp::prog(
            CgOp::whileloop($self->until, $self->once,
                CgOp::unbox('Boolean',
                    CgOp::fetch(
                        CgOp::methodcall($self->check->code($body), "Bool"))),
                CgOp::sink($self->body->code($body))),
            CgOp::null('Variable'));
    }
}

# only for state $x will start and START{} in void context, yet
class Op::Start extends Op {
    # possibly should use a raw boolean somehow
    has condvar => (isa => 'Str', is => 'ro', required => 1);
    has body => (isa => 'Op', is => 'ro', required => 1);

    method code ($body) {

        CgOp::ternary(
            CgOp::unbox('Boolean',
                CgOp::fetch(
                    CgOp::methodcall(CgOp::scopedlex($self->condvar), "Bool"))),
            CgOp::wrap(CgOp::null('object')),
            CgOp::prog(
                CgOp::assign(CgOp::scopedlex($self->condvar),
                    CgOp::box('Bool', CgOp::bool(1))),
                $self->body->code($body)));
    }
}


class Op::Num extends Op {
    has value => (isa => 'Num', is => 'ro', required => 1);

    method code ($body) {
        CgOp::box('Num', CgOp::double($self->value));
    }
}

class Op::Bind extends Op {
    has lhs => (isa => 'Op', is => 'ro', required => 1);
    has rhs => (isa => 'Op', is => 'ro', required => 1);
    has readonly => (isa => 'Bool', is => 'ro', required => 1);

    method code ($body) {
        CgOp::prog(
            CgOp::bind($self->readonly, $self->lhs->code($body),
                $self->rhs->code($body)),
            CgOp::null('Variable'));
    }
}

class Op::Lexical extends Op {
    has name => (isa => 'Str', is => 'ro', required => 1);
    has state_decl => (isa => 'Bool', is => 'ro', default => 0);

    sub paren {
        Op::Lexical->new(name => shift()->name);
    }

    method code ($body) {
        CgOp::scopedlex($self->name);
    }
}

1;
