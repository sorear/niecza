class Sig;

use CgOp;

class Parameter {
    # eek too many attributes.  It probably makes sense to use a bitfield or
    # two here.
    has $.slot; # Str
    has $.slurpy = False; # Bool
    has $.slurpycap = False; # Bool
    # does not vivify; rw
    has $.rwtrans = False; # Bool
    has $.full_parcel = False; # Bool
    has $.optional = False; # Bool
    has $.default; # Body
    has $.mdefault; # Xref; filled in by begin (rw)
    # not 'is rw' (how meta).
    has $.readonly = False;
    has $.names = []; # Array of Str
    has $.name; # Str
    has $.list = False; # Bool
    has $.hash = False; # Bool
    has $.type = 'Any'; # Str
    has $.tclass; # is rw; Xref

    method !default_get($body) {
        if defined $!mdefault {
            CgOp.call_uncloned_sub(@$!mdefault);
        } elsif $!optional {
            CgOp.scopedlex($!type);
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

    method bind_inline($body, @posr) {
        my $get = $!full_parcel ?? self.parcel_get_inline(@posr) !!
            $!slurpycap ?? self.slurpycap_get_inline(@posr) !!
            $!slurpy ?? self.slurpy_get_inline(@posr) !!
            self.single_get_inline($body, @posr);

        if (defined $!slot) {
            CgOp.scopedlex($!slot, $!rwtrans ?? $get !!
                CgOp.newboundvar(+$!readonly, +$!list, $get));
        } else {
            CgOp.sink($get);
        }
    }

    method simple($n) { self.new(name => $n, slot => $n, readonly => True) }
}

has $.params = die "Sig.params required"; # Array of Sig::Parameter
has $.explicit_inv = False; # Bool

method for_method() {
    return self if $!explicit_inv;
    Sig.new(params => [ Sig::Parameter.simple('self'), @$!params ]);
}

method simple(*@names) {
    Sig.new(params => [map { Sig::Parameter.simple($_) }, @names]);
}

method bind_inline($body, *@pos) {
    CgOp.prog(map { $_.bind_inline($body, @pos) }, @$!params);
}
