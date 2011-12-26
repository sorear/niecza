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
    has $.mdefault; # Xref
    has Bool $.rw = False;
    has $.names = []; # Array of Str
    has $.name; # Str
    has $.list = False; # Bool
    has $.hash = False; # Bool
    has $.tclass; # is rw; Xref

    method simple($n) { self.new(name => $n, slot => $n, :rwtrans) }
}

has $.params = die "Sig.params required"; # Array of Sig::Parameter

method simple(*@names) {
    Sig.new(params => [map { Sig::Parameter.simple($_) }, @names]);
}
