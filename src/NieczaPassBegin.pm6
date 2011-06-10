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
        modtime => $.modtime);
    if $*niecza_outer_ref {
        $*unit.setting_ref = $*niecza_outer_ref;
        $*unit.need_unit($*unit.setting_ref.[0]);
    } elsif $.setting_name ne 'NULL' {
        $*unit.need_unit($.setting_name);
        $*unit.setting_ref = $*unit.get_unit($.setting_name).bottom_ref;
    }
    %*units{$.name} = $*unit;
    $*unit.tdeps{$.name} = [$.filename, $.modtime];

    $*unit.create_stash(['GLOBAL']);
    $*unit.create_stash(['PROCESS']);

    my @*opensubs;
    $*unit.mainline = $.mainline.begin(once => True,
            itop => $*unit.setting_ref);

    $*unit;
} }

my %type2phaser = ( init => 0, end => 1, begin => 2 );

augment class Body { method begin(:$once = False, :$itop, :$body_of, :$cur_pkg, :$augmenting = False, :$prefix = '', :$gather_hack, :$augment_hack, :$method_of) {
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
        if defined($method_of) &&
                $*unit.deref($method_of) !~~ ::Metamodel::Class {
            # XXX type checks against roles do not work yet
            $method_of ::= Any;
        }
        $.signature.begin($.returnable, $method_of);
        $metabody.signature = $.signature;
    }

    if $type eq 'regex' {
        $metabody.add_my_name('$*/');
    }
    $metabody.add_my_name('$_') if !$top;
    if $istop {
        $metabody.add_hint('$?FILE');
    }

    pop @*opensubs if $.transparent;

    $.do.begin;
    $metabody.code = $.do;

    $metabody.close;
    pop @*opensubs unless $.transparent;

    $metabody;
} }

augment class Sig {
    method begin($ret, $meth) { for @$.params { $_.begin($ret, $meth) } }
}

augment class Sig::Parameter { method begin($ret, $meth) { #OK exist
    my $deftype;
    if $.invocant && defined $meth {
        $deftype = $meth;
    } else {
        my $sname = $ret ?? 'Any' !! 'Mu';
        $deftype = $ret ?? Any !!
            $*unit.get_item(@*opensubs[*-1].find_pkg(['MY', $sname]));
        $.type //= $sname;
    }
    @*opensubs[*-1].add_my_name($.slot, list => $.list,
        hash => $.hash, noinit => True) if defined $.slot;
    @*opensubs[*-1].add_my_name('self', :noinit) if $.invocant;
    if defined $.default {
        my $mdefault = $.default.begin;
        $.mdefault = $mdefault.xref;
        @*opensubs[*-1].add_child($mdefault);
        $!default = Any;
    }
    $.tclass = ($.type ~~ Str) ?? $deftype !!
        $*unit.get_item(@*opensubs[*-1].find_pkg($.type));
} }

augment class Op {
    method begin() { for self.zyg { $_.begin } }
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

augment class Op::SubsetDef { #OK exist
    method begin() {
        my @ns = $.ourname ?? @( @*opensubs[*-1].find_pkg($.ourname) ) !!
            $*unit.anon_stash;

        $*unit.create_stash([@ns]);
        @*opensubs[*-1].add_my_stash($.lexvar, [@ns]);
        @*opensubs[*-1].add_pkg_exports($*unit, $.name, [@ns], $.exports);

        my $ibody = $.body.begin;
        @*opensubs[*-1].create_static_pad;
        @*opensubs[*-1].add_child($ibody);

        my $basetype = $*unit.get_item(@*opensubs[*-1].find_pkg(
            [@$.basetype]));
        my $obj  = ::Metamodel::Subset.new(name => $.name,
            where => $ibody.xref, :$basetype).xref;
        $*unit.bind_item([@ns], $obj);
        $*unit.deref($obj).exports = [ [@ns],
            map { [ @(@*opensubs[*-1].cur_pkg), 'EXPORT', $_, $.name ]},
                @$.exports ];
    }
}
