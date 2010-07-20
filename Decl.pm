use 5.010;
use MooseX::Declare;

use CgOp;

class Decl {
    has zyg => (is => 'ro', isa => 'ArrayRef', default => sub { [] });

    sub used_slots   () { }
    sub preinit_code { CgOp::noop }
    sub enter_code   { CgOp::noop }
    sub write        {}
}

class Decl::PreInit extends Decl {
    has var    => (isa => 'Str', is => 'ro', predicate => 'has_var');
    has code   => (isa => 'Body', is => 'ro', required => 1);
    has shared => (isa => 'Bool', is => 'ro', default => 0);

    method used_slots () {
        return $self->has_var ? ($self->var) : ();
    }

    method preinit_code ($body) {
        $self->code->outer($body);
        my $c = CgOp::subcall(CgOp::protosub($self->code));
        $self->has_var ? CgOp::proto_var($self->var, $c) : CgOp::sink($c);
    }

    method enter_code ($body) {
        !$self->has_var ? CgOp::noop :
            ($self->shared || $body->mainline) ? CgOp::share_lex($self->var) :
            CgOp::copy_lex($self->var);
    }

    method write ($body) {
        $self->code->outer($body);
        $self->code->write;
    }
}

class Decl::Sub extends Decl {
    has var    => (isa => 'Str', is => 'ro', required => 1);
    has code   => (isa => 'Body', is => 'ro', required => 1);

    method used_slots () {
        return $self->var;
    }

    method preinit_code ($body) {
        $self->code->outer($body);

        CgOp::proto_var($self->var, CgOp::newscalar(
                CgOp::protosub($self->code)));
    }

    method enter_code ($body) {
        $body->mainline ?
            CgOp::share_lex($self->var) :
            CgOp::clone_lex($self->var);
    }

    method write ($body) {
        $self->code->outer($body);
        $self->code->write;
    }
}

class Decl::SimpleVar extends Decl {
    has slot => (isa => 'Str', is => 'ro', required => 1);
    has list => (isa => 'Bool', is => 'ro', default => 0);

    method used_slots {
        return $self->slot;
    }

    method preinit_code ($body) {

        if ($self->list) {
            CgOp::proto_var($self->slot,
                CgOp::newrwlistvar(CgOp::fetch(CgOp::scopedlex('Any'))));
        } else {
            CgOp::proto_var($self->slot,
                CgOp::newrwscalar(CgOp::fetch(CgOp::scopedlex('Any'))));
        }
    }

    method enter_code ($body) {

        $body->mainline ?
            CgOp::share_lex($self->slot) :
            CgOp::copy_lex($self->slot);
    }

    method write ($body) {
    }
}

class Decl::StateVar extends Decl {
    has slot    => (isa => 'Str', is => 'ro', required => 1);
    has backing => (isa => 'Str', is => 'ro', required => 1);

    method used_slots {
        return $self->slot;
    }

    method preinit_code ($body) {
        CgOp::proto_var($self->slot, CgOp::scopedlex($self->backing));
    }

    method enter_code ($body) {
        CgOp::scopedlex($self->slot, CgOp::scopedlex($self->backing));
    }
}

class Decl::RunMainline extends Decl {
    method used_slots { '!mainline' }

    method preinit_code ($body) {

        # XXX ought not to have side effects here.
        $::SETTING_RESUME = $body;

        CgOp::proto_var('!mainline',
            CgOp::subcall(
                CgOp::rawscall('Kernel.MakeSub',
                    CgOp::rawsget('Kernel.MainlineContinuation'),
                    CgOp::null('Frame'), CgOp::null('Frame')),
                CgOp::newscalar(CgOp::aux('protopad'))));
    }

    method enter_code ($body) {
        $body->mainline ?
            CgOp::share_lex('!mainline') :
            CgOp::clone_lex('!mainline');
    }
}

class Decl::Class extends Decl {
    has name => (is => 'ro', isa => 'Str', predicate => 'has_name');
    has var  => (is => 'ro', isa => 'Str', required => 1);
    has bodyvar => (is => 'ro', isa => 'Str');
    has stub => (is => 'ro', isa => 'Bool', default => 0);
    has parents => (is => 'ro', isa => 'ArrayRef', default => sub { [] });
    has body => (is => 'ro', isa => 'Body');

    method used_slots () {
        if ($self->stub) {
            ($self->var, $self->var . '!HOW');
        } else {
            ($self->var, $self->var . '!HOW', $self->bodyvar);
        }
    }

    method preinit_code ($body) {
        if ($self->stub) {
            return CgOp::prog(
                CgOp::proto_var($self->var . '!HOW', CgOp::null('Variable')),
                CgOp::proto_var($self->var, CgOp::null('Variable')));
        }

        $self->body->outer($body);

        CgOp::with_aux("how",
            CgOp::methodcall(CgOp::scopedlex("ClassHOW"), "new",
                CgOp::wrap(CgOp::clr_string($self->name // 'ANON'))),

            CgOp::proto_var($self->var . '!HOW', CgOp::aux("how")),

            # TODO: Initialize the protoobject to a failure here so an awesome
            # error is produced if someone tries to use an incomplete class in
            # a BEGIN.
            CgOp::proto_var($self->var, CgOp::null('Variable')),

            CgOp::proto_var($self->bodyvar,
                CgOp::newscalar(
                    CgOp::protosub($self->body))),
            CgOp::scopedlex($self->var,
                CgOp::methodcall(CgOp::aux("how"), "create-protoobject")));
    }

    method enter_code ($body) {
        CgOp::prog(
            CgOp::share_lex($self->var . '!HOW'),
            CgOp::share_lex($self->var),
            ($self->stub ? () :
                ($body->mainline ?
                    CgOp::share_lex($self->bodyvar) :
                    CgOp::clone_lex($self->bodyvar))));
    }

    method write ($body) {
        return unless $self->body;
        $self->body->outer($body);
        $self->body->write;
    }
}

class Decl::HasMethod extends Decl {
    has name => (is => 'ro', isa => 'Str', required => 1);
    has var  => (is => 'ro', isa => 'Str', required => 1);

    method preinit_code ($body) {
        if ($body->type ne 'class') {
            #TODO: Make this a sorry.
            die "Tried to set a method outside a class!";
        }
        CgOp::sink(
            CgOp::methodcall(CgOp::aux("how"), "add-method",
                CgOp::wrap(CgOp::clr_string($self->name)),
                CgOp::scopedlex($self->var)));
    }
}

class Decl::Super extends Decl {
    has name => (is => 'ro', isa => 'Str', required => 1);

    method preinit_code ($body) {
        if ($body->type ne 'class') {
            #TODO: Make this a sorry.
            die "Tried to set a superclass outside an initial class!";
        }

        CgOp::sink(
            CgOp::methodcall(CgOp::aux('how'), "add-super",
                CgOp::scopedlex($self->name . "!HOW")));
    }
}

class Decl::Regex extends Decl {
    has slot => (is => 'ro', isa => 'Str', required => 1);

    method preinit_code ($body) {
    }
}

1;
