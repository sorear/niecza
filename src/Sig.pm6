our ($Sig, $SigParameter);
class Sig;

# Value processing
our constant $HASTYPE    = 1; #OK not used
our constant $MULTI_IGNORED = 16384; #OK not used
our constant $ANY_DEF    =  0x40000; #OK
our constant $UNDEF_ONLY =  0x80000; #OK
our constant $DEF_ONLY   =  0xC0000; #OK
our constant $TYPE_ONLY  = 0x100000; #OK
our constant $DEF_MASK   = 0x1C0000; #OK

# Value binding
our constant $READWRITE  = 2; #OK not used
our constant $RWTRANS    = 8; #OK not used
our constant $INVOCANT   = 8192; #OK not used
our constant $IS_COPY    = 32768; #OK not used
our constant $IS_LIST    = 65536; #OK not used
our constant $IS_HASH    = 131072; #OK not used
our constant $CALLABLE   = 0x20_0000; #OK

# Value source
our constant $HASDEFAULT = 32; #OK not used
our constant $OPTIONAL   = 64; #OK not used
our constant $DEFOUTER   = 4096; #OK not used
our constant $POSITIONAL = 128; #OK not used
our constant $SLURPY_POS = 256; #OK not used
our constant $SLURPY_NAM = 512; #OK not used
our constant $SLURPY_CAP = 1024; #OK not used
our constant $SLURPY_PCL = 2048; #OK not used

class Parameter {
    has Str $.slot;
    has Int $.flags is rw = $POSITIONAL;
    has $.mdefault; # Xref
    has $.names = []; # Array of Str
    has Str $.name;
    has $.tclass is rw; # Type

    has $.where is rw; # List of (Type | StaticSub); lazily created
    has Sig $.subsig is rw;
    has $.typecap is rw; # List of Str; lazily created

    has Str $.attribute;
    has $.attribute_type;

    method simple($n) { self.new(name => $n, slot => $n, flags => $RWTRANS + $POSITIONAL) }
}

has $.params = die "Sig.params required"; # Array of Sig::Parameter

method simple(*@names) {
    Sig.new(params => [map { Sig::Parameter.simple($_) }, @names]);
}

INIT {
    $Sig = Sig;
    $SigParameter = Sig::Parameter;
}
