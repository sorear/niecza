class NieczaPassBegin;

use MONKEY_TYPING;
use Unit;
use Sig;
use Body;
use Op;
use Metamodel;

method invoke($ast) { $ast.begin }

### Code goes here to build up the metamodel from an Op tree
# We should eventually wire this to the parser, so that metamodel stuff can
# exist during the parse itself; will be needed for macros

augment class Unit { method begin() {
    my $*unit = ::Metamodel::Unit.new(name => $.name,
        ns => ::Metamodel::Namespace.new,
        filename => $.filename,
        modtime => $.modtime,
        setting => ($.setting_name eq 'NULL' ?? Str !! $.setting_name));
    %*units{$.name} = $*unit;
    $*unit.tdeps{$.name} = [$.filename, $.modtime];

    $*unit.need_unit($.setting_name) if $.setting_name ne 'NULL';

    $*unit.create_stash(['GLOBAL']);
    $*unit.create_stash(['PROCESS']);

    my @*opensubs;
    $*unit.mainline = $.mainline.begin(once => True,
            itop => ($.setting_name ne 'NULL' ?? $*unit.get_unit($.setting_name).bottom_ref !! Any));

    $*unit;
} }

my %type2phaser = ( init => 0, end => 1, begin => 2 );

augment class Body { method begin(:$once = False, :$itop, :$body_of, :$cur_pkg, :$augmenting = False, :$prefix = '', :$gather_hack, :$augment_hack) {
    my $top = @*opensubs ?? @*opensubs[*-1].xref !! $itop;
    my $rtop = $top && $*unit.deref($top);
    my $istop = !@*opensubs;

    my $type = $.type // '';
    my $metabody = ::Metamodel::StaticSub.new(
        unit       => $*unit,
        outerx     => $top,
        body_of    => $body_of,
        in_class   => $body_of // (@*opensubs ?? @*opensubs[*-1].in_class !!
            Any),
        cur_pkg    => $cur_pkg // (@*opensubs ?? @*opensubs[*-1].cur_pkg !!
            [ 'GLOBAL' ]), # cur_pkg does NOT propagate down from settings
        augmenting => $augmenting,
        name       => $prefix ~ $.name,
        returnable => $.returnable,
        unsafe     => ?$.unsafe,
        transparent=> $.transparent,
        gather_hack=> $gather_hack,
        augment_hack=> $augment_hack,
        is_phaser  => %type2phaser{$type},
        class      => $.class,
        ltm        => $.ltm,
        run_once   => $once && ($istop || $rtop.run_once));

    $*unit.create_stash($metabody.cur_pkg);

    push @*opensubs, $metabody; # always visible in the signature XXX

    if $.signature {
        $.signature.begin;
        $metabody.signature = $.signature;
    }

    if $type eq 'regex' {
        $metabody.add_my_name('$*/');
    }
    $metabody.add_my_name('$_') if $istop;

    pop @*opensubs if $.transparent;

    $.do.begin;
    $metabody.code = $.do;

    $metabody.close;
    pop @*opensubs unless $.transparent;

    $metabody;
} }

augment class Sig {
    method begin() { for @$.params { $_.begin } }
}

augment class Sig::Parameter { method begin() { #OK exist
    @*opensubs[*-1].add_my_name($.slot, list => $.list,
        hash => $.hash, noinit => True) if defined $.slot;
    if defined $.default {
        my $mdefault = $.default.begin;
        $.mdefault = $mdefault.xref;
        @*opensubs[*-1].add_child($mdefault);
        $!default = Any;
    }
    # XXX should use a proper undef not 'Any'
    $.tclass = ($.type ~~ Str) ?? Any !!
        $*unit.get_item(@*opensubs[*-1].find_pkg($.type));
} }

augment class Op {
    method begin() { for self.zyg { $_.begin } }
}

augment class Op::YouAreHere { #OK exist
    method begin() {
        $*unit.bottom_ref = @*opensubs[*-1].xref;
        @*opensubs[*-1].strong_used = True;
        @*opensubs[*-1].create_static_pad;
    }
}

augment class Op::Use { #OK exist
    method begin() {
        my $name = $.unit;
        my $u2 = $*unit.need_unit($.unit);

        my @can = @( $u2.mainline.find_pkg([$name.split('::')]) );
        my @exp = (@can, 'EXPORT', 'DEFAULT');

        # XXX I am not sure how need binding should work in the :: case
        if $name !~~ /"::"/ {
            @*opensubs[*-1].lexicals{$name} =
                ::Metamodel::Lexical::Stash.new(path => @can);
        }

        for $*unit.list_stash(@exp) -> $tup {
            my $uname = $tup[0];
            my $lex;
            if $tup[1] eq 'var' {
                if $tup[2] && !$tup[2][0] {
                    $lex = ::Metamodel::Lexical::Common.new(path => @exp, name => $uname);
                } elsif $tup[2] {
                    $lex = ::Metamodel::Lexical::Stash.new(path => [@exp, $uname]);
                }
            } elsif $tup[1] eq 'graft' {
                $lex = ::Metamodel::Lexical::Stash.new(path => $tup[2]);
            } else {
                die "weird return";
            }

            @*opensubs[*-1].lexicals{$uname} = $lex;
        }
    }
}

augment class Op::Lexical { #OK exist
    method begin() {
        my $typeconstraint = $.typeconstraint;
        if $typeconstraint {
            $typeconstraint = $*unit.get_item(
                @*opensubs[*-1].find_pkg($typeconstraint));
        }
        if $.state_backing {
            @*opensubs[*-1].add_state_name($.name, $.state_backing,
                list => $.list, hash => $.hash, :$typeconstraint);
        } elsif $.declaring {
            @*opensubs[*-1].add_my_name($.name, list => $.list, hash => $.hash,
                :$typeconstraint);
        }
    }
}

augment class Op::CallMethod { #OK exist
    method begin() {
        for self.zyg { $_.begin } # XXX callsame
        if $.private {
            if $.ppath {
                $.pclass = $*unit.get_item(@*opensubs[*-1].find_pkg($.ppath));
            } elsif @*opensubs[*-1].in_class {
                $.pclass = @*opensubs[*-1].in_class;
            } else {
                die "unable to resolve class of reference for method";
            }
        }
    }
}

augment class Op::Labelled { #OK exist
    method begin() {
        @*opensubs[*-1].add_label($.name);
        for self.zyg { $_.begin } # XXX callsame
    }
}

augment class Op::ConstantDecl { #OK exist
    method begin() {
        if $.path {
            @*opensubs[*-1].add_common_name($.name,
                @*opensubs[*-1].find_pkg($.path), $.name);
        } else {
            @*opensubs[*-1].add_hint($.name);
        }

        # Ordinarily this is always set, but the parser checks to ensure it
        # are slightly fragile, for instance, 4 + constant $x = 5 parses
        # as (4 + constant $x) = 5
        if !$.init {
            die "Malformed constant decl";
        }

        $.init.begin;
        my $nb = ::Metamodel::StaticSub.new(
            transparent=> True,
            unit       => $*unit,
            outerx     => @*opensubs[*-1].xref,
            name       => $.name,
            cur_pkg    => @*opensubs[*-1].cur_pkg,
            class      => 'Sub',
            run_once   => False,
            is_phaser  => 2,
            hint_hack  => [ @*opensubs[*-1].xref, $.name ],
            code       => $.init);
        @*opensubs[*-1].create_static_pad; # for protosub instance
        @*opensubs[*-1].add_child($nb);
    }
}

augment class Op::PackageVar { #OK exist
    method begin() {
        # cache the lookup here
        @*opensubs[*-1].add_common_name($.slot,
            @*opensubs[*-1].find_pkg($.path), $.name);
    }
}

augment class Op::Attribute { #OK exist
    method begin() {
        my $ns = @*opensubs[*-1].body_of //
            die "attribute $.name declared outside of any class";
        my $tc = $.typeconstraint;
        if $tc {
            $tc = $*unit.get_item(@*opensubs[*-1].find_pkg($tc));
        }
        die "attribute $.name declared in an augment"
            if @*opensubs[*-1].augmenting;
        my ($ibref, $ibvar);
        if $.initializer {
            my $ibody = $.initializer.begin;
            $ibvar = ::GLOBAL::NieczaActions.gensym;
            @*opensubs[*-1].add_my_sub($ibvar, $ibody);
            $ibref = $ibody.xref;
        }
        $ns = $*unit.deref($ns);
        $ns.add_attribute($.name, +$.accessor, $ibvar, $ibref, $tc);
        my $nb = ::Metamodel::StaticSub.new(
            transparent=> True,
            unit       => $*unit,
            outerx     => @*opensubs[*-1].xref,
            name       => $.name,
            cur_pkg    => @*opensubs[*-1].cur_pkg,
            class      => 'Sub',
            signature  => Sig.simple('self'),
            code       => ::Op::GetSlot.new(name => $.name,
                object => ::Op::Lexical.new(name => 'self')));
        $nb.add_my_name('self', noinit => True);
        @*opensubs[*-1].create_static_pad; # for protosub instance
        $nb.strong_used = True;
        @*opensubs[*-1].add_my_sub($.name ~ '!a', $nb);
        $ns.add_method('private', $.name, $.name ~ '!a', $nb.xref);
        if $.accessor {
            $ns.add_method('normal', $.name, $.name ~ '!a', $nb.xref);
        }
    }
}

augment class Op::Super { #OK exist
    method begin() {
        my $ns   = $*unit.deref(@*opensubs[*-1].body_of //
            die "superclass $.name declared outside of any class");
        die "superclass $.name declared in an augment"
            if @*opensubs[*-1].augmenting;
        $ns.add_super($*unit.get_item(@*opensubs[*-1].find_pkg([ @( $.path // ['MY'] ), $.name ])));
    }
}

augment class Op::SubDef { #OK exist
    method begin() {
        my $prefix = '';
        if defined $.bindmethod {
            $prefix = $*unit.deref(@*opensubs[*-1].body_of).name ~ ".";
        }
        $.symbol = $.bindlex ?? ('&' ~ $.body.name) !!
            ::GLOBAL::NieczaActions.gensym;
        $.bindpackages //= [];
        my $body = $.body.begin(:$prefix,
            once => ($.body.type // '') eq 'voidbare');
        @*opensubs[*-1].add_my_sub($.symbol, $body);
        my $r = $body.xref;
        if $.bindpackages || defined $.bindmethod {
            $body.strong_used = True;
        }
        @*opensubs[*-1].create_static_pad if $body.strong_used;

        if defined $.bindmethod {
            if @*opensubs[*-1].augment_hack {
                if $.bindmethod[1] ~~ Op {
                    die "Computed names are legal only in parametric roles";
                }
                push @*opensubs[*-1].augment_hack,
                    [ @$.bindmethod, $.symbol, $r ];
            } else {
                $*unit.deref(@*opensubs[*-1].body_of)\
                    .add_method(|$.bindmethod, $.symbol, $r);
            }
        }

        @*opensubs[*-1].add_exports($*unit, $.symbol, $.bindpackages);
        $body.exports = [ map { [ @($body.cur_pkg), 'EXPORT', $_, $.symbol ] },
                @$.bindpackages ];

        $!body = Body;
    }
}

augment class Op::VoidPhaser { #OK exist
    method begin() {
        @*opensubs[*-1].create_static_pad;
        @*opensubs[*-1].add_child($.body.begin);
        $!body = Body;
    }
}

augment class Op::BareBlock { #OK exist
    method begin() {
        @*opensubs[*-1].add_my_sub($.var, $.body.begin);
        $!body = Body;
    }
}

augment class Op::Gather { #OK exist
    method begin() {
        @*opensubs[*-1].add_my_sub($.var, $.body.begin(gather_hack => True));
        $!body = Body;
    }
}

augment class Op::WhateverCode { #OK exist
    method begin() {
        my $body = Body.new(transparent => True, do => $.ops,
            signature => Sig.simple(@$.vars));
        @*opensubs[*-1].add_my_sub($.slot, $body.begin);
    }
}

augment class Op::Start { #OK exist
    method begin() {
        @*opensubs[*-1].add_state_name(Str, $.condvar);
        for self.zyg { $_.begin } # XXX callsame
    }
}

# XXX symbolic class referencing
my %pclasses = (
    ::Op::PackageDef.typename => ::Metamodel::Package,
    ::Op::ModuleDef.typename => ::Metamodel::Module,
    ::Op::ClassDef.typename => ::Metamodel::Class,
    ::Op::GrammarDef.typename => ::Metamodel::Grammar,
    ::Op::RoleDef.typename => ::Metamodel::Role
);

augment class Op::PackageDef { #OK exist
    method begin() {
        my $pclass = %pclasses{self.typename}; #XXX

        if $pclass === ::Metamodel::Role && $.signature {
            $pclass = ::Metamodel::ParametricRole;
            $.body.signature = $.signature;
        }

        my @ns = $.ourpkg ??
            (@( @*opensubs[*-1].find_pkg($.ourpkg) ), $.ourvar) !!
            $*unit.anon_stash;
        my $n = pop(@ns);

        $*unit.create_stash([@ns, $n]);
        @*opensubs[*-1].add_my_stash($.var, [ @ns, $n ]);
        @*opensubs[*-1].add_pkg_exports($*unit, $.name, [ @ns, $n ], $.exports);
        if !$.stub {
            my $obj  = $pclass.new(name => $.name).xref;
            $*unit.bind_item([ @ns, $n ], $obj);
            my $body = $.body.begin(body_of => $obj, cur_pkg => [ @ns, $n ],
                once => ($pclass !=== ::Metamodel::ParametricRole));
            $*unit.deref($obj).close;
            $*unit.deref($obj).exports = [
                    [ @ns, $n ],
                    map { [ @(@*opensubs[*-1].cur_pkg), 'EXPORT', $_, $.var ]},
                        @$.exports ];

            if $pclass === ::Metamodel::ParametricRole {
                $body.parametric_role_hack = $obj;
                $body.add_my_name('*params', noinit => True);
                $body.create_static_pad;
            }
            @*opensubs[*-1].add_my_sub($.bodyvar, $body);
        }
    }
}

augment class Op::Augment { #OK exist
    method begin() {
        # XXX shouldn't we distinguish augment class Foo { } from ::Foo ?
        my $pkg = @*opensubs[*-1].find_pkg([ @$.pkg, $.name ]);
        my $so = $*unit.get_item($pkg);
        my $dso = $*unit.deref($so);
        if $dso.^isa(::Metamodel::Role) {
            die "illegal augment of a role";
        }
        my @ah = $so;
        my $body = $.body.begin(augment_hack => @ah,
            body_of => $so, augmenting => True, once => True, cur_pkg => $pkg);
        $body.augment_hack = Any;
        @*opensubs[*-1].add_my_sub($.bodyvar, $body);

        my $ph = ::Metamodel::StaticSub.new(
            unit       => $*unit,
            outerx     => $body.xref,
            cur_pkg    => [ 'GLOBAL' ],
            name       => 'ANON',
            is_phaser  => 0,
            augment_hack => @ah,
            class      => 'Sub',
            code       => ::Op::StatementList.new(children => []),
            run_once   => $body.run_once);
        $body.create_static_pad;
        $body.add_child($ph);
    }
}
