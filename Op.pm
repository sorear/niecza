use strict;
use warnings;
use 5.010;

{
    package Op;
    use Moose;

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
