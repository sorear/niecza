use strict;
use warnings;
use 5.010;
use CodeGen ();
use CgOp ();

{
    package Body;
    use Moose;

    has name      => (isa => 'Str', is => 'rw', default => "anon");
    has uid       => (isa => 'Int', is => 'ro', default => sub { ++(state $i) });
    has do        => (isa => 'Op', is => 'rw');
    has scopetree => (is => 'rw');
    has signature => (isa => 'Maybe[Sig]', is => 'ro');
    has mainline  => (isa => 'Bool', is => 'ro', lazy => 1,
            builder => 'is_mainline');
    # '' for incorrectly contextualized {p,x,}block, blast
    has type      => (isa => 'Str', is => 'rw');

    has lexical   => (isa => 'HashRef[Str]', is => 'rw');
    # my $x inside, floats out; mostly for blasts; set by context so must be rw
    has transparent => (isa => 'Bool', is => 'rw', default => 0);
    has decls => (isa => 'ArrayRef[Decl]', is => 'rw');
    has cgoptree => (isa => 'CgOp', is => 'rw');
    # only used for the top mainline
    has file => (isa => 'Str', is => 'ro');
    has text => (isa => 'Str', is => 'ro');

    sub is_mainline { $_[0]->scopetree->{'?is_mainline'} }

    sub extract_scopes {
        my ($self, $outer) = @_;

        $self->lexical(+{ map { $_->used_slots } @{ $self->decls } });
        my %h = %{ $self->lexical };

        if ($self->type eq 'mainline') {
            $h{'?is_mainline'} = 1;
        } elsif ($self->type =~ /^(?:bare|package|module|class|grammar|role|slang|knowhow)$/) {
            $h{'?is_mainline'} = $outer->{'?is_mainline'};
        } else {
            $h{'?is_mainline'} = 0;
        }

        $h{'OUTER::'} = $outer;
        $_->extract_scopes(\%h) for (map { $_->bodies } @{ $self->decls });
        $self->scopetree(\%h);
    }

    sub lift_decls {
        my ($self) = @_;
        my (@self_q, @outer_q);

        # it is the caller's responsibility to process returned decls
        local $::process = sub {
            my (@decls) = @_;
            for my $decl (@decls) {
                push @outer_q, $decl->outer_decls;
                for my $b ($decl->bodies) {
                    my @o = $b->lift_decls;
                    if ($self->transparent) {
                        push @outer_q, @o;
                    } else {
                        for (@o) { $::process->($_); }
                    }
                }
                push @self_q, $decl;
            }
        };

        $::process->(Decl::Hint->new(name => '$?CURPKG',
            value => CgOp::letvar('pkg'))) if $self->type =~ /mainline|class|
            package|grammar|module|role|slang|knowhow/x;

        if ($self->type eq 'mainline') {
            $::process->(
                Decl::Hint->new(name => '$?GLOBAL',
                    value => CgOp::letvar('pkg')),
                Decl::Hint->new(name => '$?FILE',
                    value => CgOp::string_var($self->file)),
                Decl::Hint->new(name => '$?ORIG',
                    value => CgOp::string_var($self->text)));
        }
        $::process->($self->signature->local_decls) if $self->signature;
        if ($self->transparent) {
            push @outer_q, $self->do->lift_decls;
        } else {
            $::process->($self->do->lift_decls);
        }

        $self->decls(\@self_q);

        @outer_q;
    }

    sub to_cgop {
        my ($self) = @_;
        my @enter;
        push @enter, map { $_->enter_code($self) } @{ $self->decls };
        push @enter, $self->signature->binder($self) if $self->signature;
        # TODO: Bind a return value here to catch non-ro sub use
        if ($self->type eq 'gather') {
            $self->cgoptree(CgOp::prog(@enter,
                    CgOp::sink($self->do->cgop($self)),
                    CgOp::rawsccall('Kernel.Take',
                        CgOp::scopedlex('EMPTY'))));
        } else {
            $self->cgoptree(CgOp::prog(@enter,
                    CgOp::return($self->do->cgop($self))));
        }
        map { $_->preinit_code($self) } @{ $self->decls };
    }

    sub to_anf {
        my ($self) = @_;
        $self->cgoptree($self->cgoptree->cps_convert(1));
        $_->to_anf for (map { $_->bodies } @{ $self->decls });
    }

    sub write {
        my ($self) = @_;
        CodeGen->new(lex2types => $self->lexical, csname => $self->csname,
            body => $self, ops => $self->cgoptree)->write;
        $_->write for (map { $_->bodies } @{ $self->decls });
    }

    sub lex_level {
        my ($self, $var) = @_;

        my $i = 0;
        my $st = $self->scopetree;
        while ($st) {
            return $i if ($st->{$var});
            $i++;
            $st = $st->{'OUTER::'};
        }
        return -1;
    }

    sub csname {
        my ($self) = @_;
        my @name = split /\W+/, $self->name;
        shift @name if @name && $name[0] eq '';
        join("", (map { ucfirst $_ } @name), "_", $self->uid, "C");
    }

    # In order to support proper COMMON semantics on package variables
    # we have only one operation here - autovivifying lookup.
    #
    # This should be used for all accesses like $a::b, $::b.  $a is a simple
    # lexical.
    sub lookup_var {
        my ($self, $name, @path) = @_;

        return $self->lookup_pkg((map { $_ . "::" } @path),
            (defined $name) ? ($name) : ());
    }

    sub lookup_pkg {
        my ($self, @components) = @_;

        my $pkgcg;
        my $usedstate = 0;
        # TODO: S02 says PROCESS:: and GLOBAL:: are also accessible as lexical
        # packages in UNIT::.
        if ($components[0] eq 'PROCESS::') {
            $pkgcg = CgOp::rawsget('Kernel.Process');
            shift @components;
        } elsif ($components[0] eq 'GLOBAL::') {
            $pkgcg = CgOp::scopedlex('$?GLOBAL');
            shift @components;
        } elsif ($components[0] eq 'OUR::') {
            $pkgcg = CgOp::scopedlex('$?CURPKG');
            shift @components;
        } elsif ($self->lex_level($components[0]) >= 0) {
            $pkgcg = CgOp::scopedlex($components[0]);
            $usedstate = 1 unless $components[0] =~ /::$/;
            shift @components;
        } else {
            $pkgcg = CgOp::scopedlex('$?CURPKG');
        }

        for my $c (@components) {
            $pkgcg = CgOp::rawscall('Kernel.PackageLookup', CgOp::fetch($pkgcg),
                CgOp::clr_string($c));
        }

        $usedstate, $pkgcg;
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}

1;
