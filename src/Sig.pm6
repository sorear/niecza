class Sig;

use CgOp;

class Parameter {
    # eek too many attributes.  It probably makes sense to use a bitfield or
    # two here.
    has $.slot; # Str
    has Bool $.slurpy = False;
    has Bool $.slurpycap = False;
    # does not vivify; rw
    has Bool $.rwtrans = False;
    has Bool $.is_copy = False;
    has Bool $.full_parcel = False;
    has Bool $.optional = False;
    has Bool $.defouter = False;
    has Bool $.positional = True;
    has Bool $.invocant = False; # is rw
    has Bool $.multi_ignored = False; # is rw
    has $.default; # Body
    has $.mdefault; # Xref; filled in by begin (rw)
    has Bool $.rw = False;
    has $.names = []; # Array of Str
    has $.name; # Str
    has $.list = False; # Bool
    has $.hash = False; # Bool
    has $.type = 'Any'; # Str
    has $.tclass; # is rw; Xref

    method !default_get($body) {
        if defined $!mdefault {
            CgOp.call_uncloned_sub(@$!mdefault);
        } elsif $!defouter {
            CgOp.outerlex($!slot);
        } elsif $!optional {
            CgOp.class_ref('typeVar', @$!tclass);
        } else {
            CgOp.die("No value in $body.name() available for parameter $!name");
        }
    }

    method single_get_inline($body, @posr) {
        if ($!positional && @posr) {
            shift @posr;
        } else {
            self!default_get($body);
        }
    }

    method do_copy($val) {
        CgOp.prog(
            CgOp.scopedlex($!slot, ($!hash ?? CgOp._cgop("newhash") !!
                $!list ?? CgOp._cgop("newarray") !!
                CgOp._cgop("newtypedscalar",
                    CgOp.class_ref("mo", @( $!tclass // 'Any' ))))),
            CgOp.sink(CgOp.assign(CgOp.scopedlex($!slot), $val)))
    }

    method bind_inline($body, @posr) {
        my $get = $!full_parcel ?? self.parcel_get_inline(@posr) !!
            $!slurpycap ?? self.slurpycap_get_inline(@posr) !!
            $!slurpy ?? self.slurpy_get_inline(@posr) !!
            self.single_get_inline($body, @posr);

        if (defined $!slot) {
            if $!is_copy {
                self.do_copy($get);
            } else {
                my $type = CgOp.class_ref('mo', @($!tclass || 'Any'));
                CgOp.scopedlex($!slot, $!rwtrans ?? $get !!
                    CgOp.newboundvar(+(!$!rw), +$!list, $type, $get));
            }
        } else {
            CgOp.sink($get);
        }
    }
    method simple($n) { self.new(name => $n, slot => $n) }
}

has $.params = die "Sig.params required"; # Array of Sig::Parameter

method for_method() {
    if $!params && $!params.[0].invocant {
        return self;
    }
    Sig.new(params => [ Sig::Parameter.new(name => 'self', :invocant),
        @$!params ]);
}

method simple(*@names) {
    Sig.new(params => [map { Sig::Parameter.simple($_) }, @names]);
}

method bind_inline($body, *@pos) {
    CgOp.prog(map { $_.bind_inline($body, @pos) }, @$!params);
}
