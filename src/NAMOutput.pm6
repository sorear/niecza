class NAMOutput;

use JSYNC;
use Metamodel;
use Sig;
use MONKEY_TYPING;

method run($*unit) {
    my @*subsnam;

    $*unit.visit_local_subs_postorder(&nam_sub);
    to-json($*unit.to_nam) ~ "\n" ~ to-json(@*subsnam);
}

sub nam_sub($s) {
    my $code = $s.code;
    if $s.topicalizer {
        $code = ::Op::TopicalHook.new(inner => $code);
    }
    my $lx = $s.lexicals;
    my @labels = grep { $lx{$_}.^isa(::Metamodel::Lexical::Label) }, $lx.keys;
    if @labels {
        $code = ::Op::LabelHook.new(labels => @labels, inner => $code);
    }
    if $s.parametric_role_hack {
        for @( $*unit.deref($s.parametric_role_hack).methods ) -> $me {
            if $me.name ~~ ::GLOBAL::Op {
                $me.name = $me.name.cgop($s);
            }
        }
    }
    @*subsnam[$s.xref[1]] = [
        $s.xref, # for documentation
        $s.parametric_role_hack,
        $s.augment_hack,
        $s.hint_hack,
        $s.is_phaser,
        $s.body_of,
        $s.in_class,
        $s.cur_pkg,
        $code.cgop($s),
    ];
}

method load($text) {
    $text := $text.substr(0, $text.index("\n"));
    unit_from_nam(from-json($text));
}

augment class Metamodel::Unit { #OK exist
    method to_nam() {
        [
            $.name,
            [ map { [$_, @( $.tdeps{$_} )] }, sort keys $.tdeps ],
            $.mainline.xref,
            $.setting_ref,
            $.bottom_ref,
            $.filename,
            $.modtime,
            $.ns.root,
            [ $.ns.log ],
            [ map { $_ && $_.to_nam }, @$.xref ],
        ]
    }
}

sub unit_from_nam(@block) {
    my ($name, $td, $mlref, $setting, $bottom, $filename, $modtime,
        $nsroot, $nslog, $xr) = @block;
    my $*uname = $name;
    my $*unit = ::Metamodel::Unit.new(
        name       => $name,
        ns         => ::Metamodel::Namespace.new(log => @$nslog,
            root => $nsroot),
        setting_ref => $setting,
        bottom_ref => $bottom,
        filename   => $filename,
        modtime    => $modtime,
        tdeps      => _hash_constructor(map { (shift($_) => $_) }, @$td));

    my $*xref = $*unit.xref;
    my $*xid = 0;
    while $*xid < @$xr {
        my $xv = $xr[$*xid];
        push $*xref, $xv && ($xv[0] eq 'sub' ?? &sub_from_nam !! &packagely)($xv);
        $*xid++;
    }
    # XXX suboptimality
    $*xid = 0;
    while $*xid < @$xr {
        if ($xr[$*xid] && $xr[$*xid][0] eq 'sub') {
            for @( $xr[$*xid][8] ) -> $row {
                my ($k,$v) = lex_from_nam($row);
                $*xref[$*xid].lexicals{$k} = $v;
            }
            for @( $xr[$*xid][4] ) -> $z {
                push $*xref[$*xid].zyg, $*xref[$z];
            }
        }
        $*xid++;
    }
    $*unit.mainline = $*unit.xref[$mlref[1]];

    $*unit;
}

augment class Metamodel::StaticSub { #OK exist
    method to_nam() {
        my $flags = 0;
        $flags +|=  1 if $.run_once;
        $flags +|=  2 if $.spad_exists;
        $flags +|=  4 if $.gather_hack;
        $flags +|=  8 if $.strong_used;
        $flags +|= 16 if $.returnable;
        $flags +|= 32 if $.augmenting;
        $flags +|= 64 if $.transparent;
        $flags +|= 128 if $.unsafe;
        [
            'sub',
            $.name,
            $.outerx,
            $flags,
            [ map { $_.xref[1] }, @$.zyg ],
            $.class,
            $.ltm,
            ($.signature && [ map { $_.to_nam }, @( $.signature.params ) ]),
            [ map { [ $_, @( $.lexicals{$_}.to_nam ) ] },
                sort keys $.lexicals ],
            $.extend,
        ]
    }
}

sub sub_from_nam(@block) {
    my ($kind, $name, $outer, $flags, $zyg, #OK
        $cls, $ltm, $sig, $rlx, $ext) = @block; #OK
    # Most of these are used only by code-gen.  Lexicals are injected later.

    ::Metamodel::StaticSub.new(
        :no_xref,
        unit => $*unit,
        name => $name,
        xref => [ $*uname, $*xid, $name ],
        outerx => $outer,
        run_once => ?($flags +& 1),
        spad_exists => ?($flags +& 2),
        transparent => ?($flags +& 64),
        extend => $ext,
        zyg => [],
        class => $cls,
        ltm => $ltm,
    )
}

my %pkgtypes = (
    package => ::Metamodel::Package,
    module  => ::Metamodel::Module,
    class   => ::Metamodel::Class,
    grammar => ::Metamodel::Grammar,
    role    => ::Metamodel::Role,
    parametricrole => ::Metamodel::ParametricRole,
    subset  => ::Metamodel::Subset,
);

my %typecodes = map { ($_.value.typename => $_.key) }, %pkgtypes;

augment class Metamodel::Package { #OK exist
    method to_nam(*@more) {
        [
            %typecodes{self.typename},
            $.name,
            $.who,
            @more
        ]
    }
}

sub packagely(@block) {
    my ($type, $name, $who, $attr, $meth, $sup, $mro) = @block;
    # these two are nonstandard
    if $type eq 'subset' {
        return ::Metamodel::Subset.new(
            :no_xref, :$name, :$who,
            xref => [ $*uname, $*xid, $name ],
            basetype => $attr,
            where => $meth,
        );
    }

    # this relies on .new ignoring unrecognized keys
    %pkgtypes{$type}.new(
        :no_xref, :$name, :$who,
        xref => [ $*uname, $*xid, $name ],
        attributes => $attr && [ map &attr_from_nam, @$attr ],
        methods => $meth && [ map &method_from_nam, @$meth ],
        superclasses => $sup,
        linearized_mro => $mro
    )
}

augment class Metamodel::Class { #OK exist
    method to_nam() {
        nextwith(self,
            [ map { $_.to_nam }, @$.attributes ],
            [ map { $_.to_nam }, @$.methods ],
            $.superclasses,
            $.linearized_mro);
    }
}

augment class Metamodel::Role { #OK exist
    method to_nam() {
        nextwith(self,
            [ map { $_.to_nam }, @$.attributes ],
            [ map { $_.to_nam }, @$.methods ],
            $.superclasses);
    }
}

augment class Metamodel::ParametricRole { #OK exist
    method to_nam() {
        nextwith(self,
            [ map { $_.to_nam }, @$.attributes ],
            [ map { $_.to_nam }, @$.methods ],
            $.superclasses);
    }
}

augment class Metamodel::Subset { #OK exist
    method to_nam() {
        nextwith(self,
            $.basetype,
            $.where);
    }
}

my %methodtypes = ( normal => 0, private => 1, sub => 2 );
my %methodmulti = ( only => 0, proto => 4, multi => 8 );
my @methodfromtype = ('normal', 'private', 'sub');
my @methodfrommulti = ('only', 'proto', 'multi');
augment class Metamodel::Method { #OK exist
    method to_nam() {
        [ $.name, %methodtypes{$.kind} + %methodmulti{$.multi}, $.var, $.body ]
    }
}

augment class Metamodel::Attribute { #OK exist
    method to_nam() {
        [ $.name, $.sigil, $.public, $.ivar, $.ibody, $.typeconstraint ]
    }
}

sub method_from_nam(@block) {
    my ($name, $kind, $var, $body) = @block;
    ::Metamodel::Method.new(:$name, :$var, :$body,
        kind => @methodfromtype[$kind +& 3],
        multi => @methodfrommulti[$kind +> 2]);
}

sub attr_from_nam(@block) {
    my ($name, $sigil, $public, $ivar, $ibody, $typeconstraint) = @block;
    ::Metamodel::Attribute.new(:$name, :$public, :$ivar, :$ibody, :$sigil,
        :$typeconstraint);
}

augment class Sig::Parameter { #OK exist
    method to_nam() {
        my $flags = 0;
        $flags = $flags +| 1   if $.slurpy;
        $flags = $flags +| 2   if $.slurpycap;
        $flags = $flags +| 4   if $.rwtrans;
        $flags = $flags +| 8   if $.full_parcel;
        $flags = $flags +| 16  if $.optional;
        $flags = $flags +| 32  if $.positional;
        $flags = $flags +| 64  if $.rw;
        $flags = $flags +| 128 if $.list;
        $flags = $flags +| 256 if $.hash;
        $flags = $flags +| 512 if $.defouter;
        $flags = $flags +| 1024 if $.invocant;
        $flags = $flags +| 2048 if $.multi_ignored;
        $flags = $flags +| 4096 if $.is_copy;

        [
            $.name,
            $flags,
            $.slot,
            $.names,
            $.mdefault,
            $.tclass
        ]
    }
}

sub parm_from_nam(@block) {
    my ($name, $flags, $slot, $names, $mdefault, $tclass) = @block; #OK
    ::Sig::Parameter.new(
        slurpy   => ?($flags +& 1),    slurpycap     => ?($flags +& 2),
        rwtrans  => ?($flags +& 4),    full_parcel   => ?($flags +& 8),
        optional => ?($flags +& 16),   positional    => ?($flags +& 32),
        rw       => ?($flags +& 64),   list          => ?($flags +& 128),
        hash     => ?($flags +& 256),  defouter      => ?($flags +& 512),
        invocant => ?($flags +& 1024), multi_ignored => ?($flags +& 2048),
        is_copy  => ?($flags +& 4096),
        name => $name, slot => $slot, names => $names, :$tclass);
}

augment class Metamodel::Lexical::Simple { #OK exist
    method to_nam() { ['simple', 4 * $!noinit + 2 * $!list + 1 * $!hash +
            8 * $!roinit + 16 * $!defouter, $!typeconstraint] }
}
augment class Metamodel::Lexical::Common { #OK exist
    method to_nam() { ['common', $.pkg, $.name ] }
}
augment class Metamodel::Lexical::Alias { #OK exist
    method to_nam() { ['alias', $.to] }
}
augment class Metamodel::Lexical::Hint { #OK exist
    method to_nam() { ['hint'] }
}
augment class Metamodel::Lexical::Label { #OK exist
    method to_nam() { ['label'] }
}
augment class Metamodel::Lexical::Dispatch { #OK exist
    method to_nam() { ['dispatch'] }
}
augment class Metamodel::Lexical::SubDef { #OK exist
    method to_nam() { ['sub', @( $.body.xref ) ] }
}
augment class Metamodel::Lexical::Stash { #OK exist
    method to_nam() { ['stash', @( $.pkg ) ] }
}

sub lex_from_nam(@block) {
    my ($name, $type, @xtra) = @block;
    return ($name, ::Metamodel::Lexical::Simple.new)
                        if $type eq 'simple';
    return ($name, ::Metamodel::Lexical::Common.new(name => @xtra[1],
        pkg => @xtra[0])) if $type eq 'common';
    return ($name, ::Metamodel::Lexical::Alias.new(to => @xtra[0]))
                        if $type eq 'alias';
    return ($name, ::Metamodel::Lexical::Hint.new)
                        if $type eq 'hint';
    return ($name, ::Metamodel::Lexical::Label.new)
                        if $type eq 'label';
    return ($name, ::Metamodel::Lexical::Dispatch.new)
                        if $type eq 'dispatch';
    return ($name, ::Metamodel::Lexical::SubDef.new(body => $*xref[@xtra[1]]))
                        if $type eq 'sub';
    return ($name, ::Metamodel::Lexical::Stash.new(pkg => @xtra))
                        if $type eq 'stash';
    die "weird lex type $type";
}
