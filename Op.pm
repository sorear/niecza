use strict;
use warnings;
use 5.010;

{
    package Op;
    use Moose;

    sub paren { shift }

    sub void_cg {
        my ($self, $cg, $body) = @_;
        $self->item_cg($cg, $body);
        $cg->drop;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::NIL;
    use Moose;
    extends 'Op';

    has code => (isa => 'ArrayRef', is => 'ro', required => 1);

    sub item_cg {
        my ($self, $cg, $body) = @_;
        for my $insn (@{ $self->code }) {
            if (blessed $insn) {
                $insn->item_cg($cg, $body);
            } else {
                my ($op, @args) = @$insn;
                $cg->$op(@args);
            }
        }
    }

    sub void_cg {
        my ($self, $cg, $body) = @_;
        $self->item_cg($cg, $body);
        $cg->drop;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::StatementList;
    use Moose;
    extends 'Op';

    has children => (isa => 'ArrayRef[Op]', is => 'ro', required => 1);

    sub item_cg {
        my ($self, $cg, $body) = @_;
        if (!@{ $self->children }) {
            # XXX should be Nil or something
            $cg->push_null('object');
            $cg->clr_wrap;
        } else {
            my @kids = @{ $self->children };
            my $end = pop @kids;
            for (@kids) {
                $_->void_cg($cg, $body);
            }
            $end->item_cg($cg, $body);
        }
    }

    sub void_cg {
        my ($self, $cg, $body) = @_;
        for (@{ $self->children }) {
            $_->void_cg($cg, $body);
        }
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
    # non-parenthesized constructor
    has splittable_pair => (isa => 'Bool', is => 'rw', default => 0);
    has splittable_parcel => (isa => 'Bool', is => 'rw', default => 0);

    sub paren {
        my ($self) = @_;
        Op::CallSub->new(invocant => $self->invocant,
            positionals => $self->positionals);
    }

    sub item_cg {
        my ($self, $cg, $body) = @_;
        $self->invocant->item_cg($cg, $body);
        $cg->fetch;
        $_->item_cg($cg, $body) for @{ $self->positionals };
        $cg->call_sub(1, scalar(@{ $self->positionals }));
    }

    sub void_cg {
        my ($self, $cg, $body) = @_;
        $self->invocant->item_cg($cg, $body);
        $cg->fetch;
        $_->item_cg($cg, $body) for @{ $self->positionals };
        $cg->call_sub(0, scalar(@{ $self->positionals }));
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

    sub item_cg {
        my ($self, $cg, $body) = @_;
        $self->receiver->item_cg($cg, $body);
        $cg->dup_fetch;
        $_->item_cg($cg, $body) for @{ $self->positionals };
        $cg->call_method(1, $self->name, scalar(@{ $self->positionals }));
    }

    sub void_cg {
        my ($self, $cg, $body) = @_;
        $self->receiver->item_cg($cg, $body);
        $cg->dup_fetch;
        $_->item_cg($cg, $body) for @{ $self->positionals };
        $cg->call_method(0, $self->name, scalar(@{ $self->positionals }));
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

    sub item_cg {
        my ($self, $cg, $body) = @_;
        $self->receiver->item_cg($cg, $body);
        $cg->fetch;
        given ($self->name) {
            when ("HOW") {
                $cg->how;
            }
            default {
                die "Invalid interrogative $_";
            }
        }
        $cg->newvar;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Yada;
    use Moose;
    extends 'Op';

    has kind => (isa => 'Str', is => 'ro', required => 1);
}

{
    package Op::StringLiteral;
    use Moose;
    extends 'Op';

    has text => (isa => 'Str', is => 'ro', required => 1);

    sub item_cg {
        my ($self, $cg, $body) = @_;
        $cg->string_var($self->text);
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

    sub cg {
        my ($self, $nv, $cg, $body) = @_;

        $self->check->item_cg($cg, $body);
        $cg->dup_fetch;
        $cg->call_method(1, "Bool", 0);
        $cg->fetch;
        $cg->unbox('Boolean');

        my $t = $self->true;
        my $f = $self->false;

        # XXX use Nil
        $t //= Op::NIL->new(code => [[ push_null => 'Variable' ]]);
        $f //= Op::NIL->new(code => [[ push_null => 'Variable' ]]);

        my $l1 = $cg->label;
        my $l2 = $cg->label;
        $cg->ncgoto($l1);
        my $m = $nv ? 'item_cg' : 'void_cg';
        $t->$m($cg, $body);
        $cg->goto($l2);
        $cg->labelhere($l1);
        $f->$m($cg, $body);
        $cg->labelhere($l2);
    }

    sub item_cg {
        my ($self, $cg, $body) = @_;
        $self->cg(1, $cg, $body);
    }

    sub void_cg {
        my ($self, $cg, $body) = @_;
        $self->cg(0, $cg, $body);
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

    sub void_cg {
        my ($self, $cg, $body) = @_;

        my $l1 = $cg->label;
        my $l2 = $self->once ? 0 : $cg->label;

        $cg->goto($l2) unless $self->once;
        $cg->labelhere($l1);
        $self->body->void_cg($cg, $body);
        $cg->labelhere($l2) unless $self->once;
        $self->check->item_cg($cg, $body);
        $cg->dup_fetch;
        $cg->call_method(1, "Bool", 0);
        $cg->fetch;
        $cg->unbox('Boolean');
        my $m = $self->until ? 'ncgoto' : 'cgoto';
        $cg->$m($l1);
    }

    sub item_cg {
        my ($self, $cg, $body) = @_;
        $self->void_cg($cg, $body);
        $cg->push_null('Variable');
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Num;
    use Moose;
    extends 'Op';

    has value => (isa => 'Num', is => 'ro', required => 1);

    sub item_cg {
        my ($self, $cg, $body) = @_;
        $cg->clr_double($self->value);
        $cg->box('Num');
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

    sub item_cg {
        my ($self, $cg, $body) = @_;
        $self->lhs->item_cg($cg, $body);
        $cg->dup;
        $self->rhs->item_cg($cg, $body);
        $cg->clr_field_get('lv');
        $cg->clr_field_set('lv');
    }

    sub void_cg {
        my ($self, $cg, $body) = @_;
        $self->lhs->item_cg($cg, $body);
        $self->rhs->item_cg($cg, $body);
        $cg->clr_field_get('lv');
        $cg->clr_field_set('lv');
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

{
    package Op::Lexical;
    use Moose;
    extends 'Op';

    has name => (isa => 'Str', is => 'ro', required => 1);

    sub item_cg {
        my ($self, $cg, $body) = @_;
        $cg->scopelexget($self->name, $body);
    }

    sub void_cg {
        my ($self, $cg, $body) = @_;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
