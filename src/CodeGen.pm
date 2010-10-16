use strict;
use warnings;
use 5.010;

use CgOpToCLROp;
use CLRTypes;

{
    package CodeGen;
    use Moose;

    has ops       => (is => 'ro', required => 1);
    has name    => (isa => 'Str', is => 'ro');
    has csname    => (isa => 'Str', is => 'ro');
    has minlets   => (isa => 'Int', is => 'ro', default => 0);
    has usednamed => (isa => 'Bool', is => 'ro', default => 0);

    has numlabels => (isa => 'Int', is => 'rw', default => 1);
    has numips    => (isa => 'Int', is => 'rw', default => 1);
    has lineinfo  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has resulttype=>(isa => 'Maybe[Str]', is => 'rw', default => '');
    has labelname => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has buffer    => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has unreach   => (isa => 'Bool', is => 'rw', default => 0);
    has outcap    => (isa => 'Bool', is => 'rw', default => 0);

    has letstack  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has letused   => (isa => 'HashRef', is => 'ro', default => sub { +{} });
    has consttab  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has linestack => (isa => 'ArrayRef', is => 'ro', default => sub { [0] });
    has ehspans   => (isa => 'ArrayRef', is => 'ro', default => sub { [] });
    has ehlabels  => (isa => 'ArrayRef', is => 'ro', default => sub { [] });

    # These are the sub-primitives.  Their very inteface exposes volatile
    # details of the codegen.

    sub qm {
        my $out = $_[0];
        $out =~ s/"/""/g;
        "@\"$out\"";
    }

    sub _odds {
        my @o; for (my $i = 1; $i < @_; $i+=2 ) { push @o, $_[$i] } @o
    }

    sub _emit {
        my ($self, $line) = @_;
        push @{ $self->buffer }, "    $line;\n";
    }

    sub _cpscall {
        my ($self, $rt, $expr) = @_;
        my $n = $self->ip;
        $self->_emit("th.ip = $n");
        $self->_emit("return $expr");
        $self->lineinfo->[$n] = $self->linestack->[-1];
        push @{ $self->buffer }, "case $n:\n";
        die "Broken call $expr" if !defined($rt);
        $self->resulttype($rt);
    }

    sub cpssync {
        my ($self) = @_;
        $self->_emit("th.ip = " . ($self->numips - 1));
    }

    sub result {
        my ($self) = @_;
        $self->cast($self->resulttype, "object", "th.resultSlot");
    }

    sub set_result {
        my ($self, $ty, $it) = @_;
        $self->resulttype($ty);
        $self->_emit("th.resultSlot = $it");
    }

    sub _lexn {
        my ($which) = @_;
        ($which < 4) ? ("th.lex$which") : ("th.lexn[" . ($which - 4) . "]");
    }

    sub push_line {
        my ($self, $line) = @_;
        push @{ $self->linestack }, $line;
    }

    sub pop_line {
        my ($self) = @_;
        pop @{ $self->linestack };
    }

    sub lexnsize {
        my ($self) = @_;
        my $i = 0;
        for (keys %{ $self->letused }) { $i++ if $_ >= 6 }
        $i;
    }

    sub push_let {
        my ($self, $which, $ty, $v) = @_;

        my $ls = $self->letstack;
        my $lu = $self->letused;
        my $i;

        for ($i = 0; ; $i++) {
            last if !$lu->{$i} &&
                ($i >= 2 || $ty eq 'Int32' || $ty eq 'System.Int32');
        }

        my $ph = ($i < 2) ? "th.lexi$i" : _lexn($i-2);
        my $pht = ($i < 2) ? $ty : "object";

        push @$ls, [ $which, $ph, $pht, $ty, $i ];
        $lu->{$i} = 1;

        $self->_emit("$ph = $v");
    }

    sub drop_let {
        my ($self, $which) = @_;
        die "Let consistency error" if $which ne $self->letstack->[-1][0];
        my $r = pop @{ $self->letstack };
        $self->letused->{ $r->[4] } = 0;
    }

    sub peek_let {
        my ($self, $which) = @_;
        my $i = @{ $self->letstack } - 1;
        while ($i >= 0 && $self->letstack->[$i][0] ne $which) { $i-- }
        my $k = $self->letstack->[$i];
        ($k->[2] eq $k->[3]) ? (@$k[2,1]) : $self->cast(@$k[3,2,1]);
    }

    sub poke_let {
        my ($self, $which, $ty, $v) = @_;
        my $i = @{ $self->letstack } - 1;
        while ($i >= 0 && $self->letstack->[$i][0] ne $which) { $i-- }
        $self->_emit($self->letstack->[$i][1] . " = $v");
    }

    sub const {
        my ($self, $type, $val) = @_;
        my $knum = @{ $self->consttab };
        my $name = "K_" . $self->csname . "_$knum";
        push @{ $self->consttab }, "$type $name = $val";
        $type, $name;
    }

    sub label {
        my ($self) = @_;
        my $n = $self->numlabels;
        $self->numlabels($n + 1);
        return $n;
    }

    sub ip {
        my ($self) = @_;
        my $n = $self->numips;
        $self->numips($n + 1);
        return $n;
    }

    sub labelhere {
        my ($self, $n) = @_;
        my $ip = $self->labelname->{$n} = $self->ip;
        push @{ $self->buffer }, "    goto case $ip;\n" unless $self->unreach;
        push @{ $self->buffer }, "case $ip:\n";
        $self->lineinfo->[$ip] = $self->linestack->[-1];
        $self->unreach(0);
    }

    sub goto {
        my ($self, $n) = @_;
        push @{ $self->buffer }, "    goto case \@\@L$n;\n";
        $self->unreach(1);
    }

    sub cgoto {
        my ($self, $n, $ty, $top) = @_;
        push @{ $self->buffer }, "    if ($top) { goto case \@\@L$n; }\n";
    }

    sub ncgoto {
        my ($self, $n, $ty, $top) = @_;
        push @{ $self->buffer }, "    if (!$top) { goto case \@\@L$n; }\n";
    }

    sub ehspan {
        my ($self, $type, $lab, $lidl, $ls, $le, $lg) = @_;
        my $lid = -1;
        if (defined $lab) {
            $lid = @{ $self->ehlabels };
            $self->labelname->{$lidl} = $lid if defined $lidl;
        }
        push @{ $self->ehspans }, "\@\@L$ls", "\@\@L$le", $type,
            "\@\@L$lg", $lid;
    }

    sub hintget {
        my ($self, $type, $data, $name) = @_;
        $self->cast($type, "object", "$data.hints[" . qm($name) . "]");
    }

    sub rtpadget {
        my ($self, $type, $order, $name) = @_;
        $self->cast($type, "object", "th" . (".outer" x $order) . ".lex[" . qm($name) . "]");
    }

    sub rtpadput {
        my ($self, $order, $name, $ty, $val) = @_;
        $self->_emit("th" . (".outer" x $order) . ".lex[" . qm($name) . "] = $val");
    }

    sub rtpadgeti {
        my ($self, $type, $order, $name) = @_;
        my $tag = $name < 4 ? "lex$name" : ("lexn[" . ($name - 4) . "]");
        $self->cast($type, "object", "th" . (".outer" x $order) . ".$tag");
    }

    sub rtpadputi {
        my ($self, $order, $name, $type, $val) = @_;
        my $tag = $name < 4 ? "lex$name" : ("lexn[" . ($name - 4) . "]");
        $self->_emit("th" . (".outer" x $order) . ".$tag = $val");
    }

    sub callframe {
        my ($self) = @_;
        "Frame", "th";
    }

    sub drop {
        my ($self, $ty, $val) = @_;
        $self->_emit($val);
    }

    sub pos {
        my ($self, $vty, $val) = @_;
        'Variable', "th.pos[$val]";
    }

    sub _prepcall {
        my ($self, $sig, @args) = @_;
        my $quick = 1;
        for (@$sig) { $quick &&= ($_ eq '') }
        my ($inv, @vals) = _odds(@args);
        if (!@vals) {
            return $inv, "Variable.None", "null";
        } elsif ($quick) {
            return $inv, "new Variable[] { " . join(", ", @vals) . "}", "null";
        } else {
            # TODO: optimize harder
            $self->outcap(1);
            $self->_emit("_inv = $inv");
            $self->_emit("_pos = new List<Variable>()");
            $self->_emit("_nam = new Dictionary<string,Variable>()");
            for (my $ix = 0; $ix < @$sig; $ix++) {
                if ($sig->[$ix] eq '') {
                    $self->_emit("_pos.Add($vals[$ix])");
                } elsif (substr($sig->[$ix],0,1) eq ':') {
                    my $n = qm(substr($sig->[$ix],1));
                    $self->_emit("_nam[$n] = $vals[$ix]");
                } elsif ($sig->[$ix] eq 'flatpos') {
                    $self->_emit("_pos.AddRange($vals[$ix])");
                } elsif ($sig->[$ix] eq 'flatnam') {
                    $self->_emit("Kernel.AddMany(_nam, $vals[$ix])");
                } elsif ($sig->[$ix] eq 'flatcap') {
                    $self->_emit("Kernel.AddCap(_pos, _nam, $vals[$ix])");
                } else {
                    die "weird sig bit $sig->[$ix]";
                }
            }
            return "_inv", "_pos.ToArray()", "_nam";
        }
    }

    sub call_method {
        my ($self, $name, $sig, @args) = @_;
        my ($inv, $pos, $nam) = $self->_prepcall($sig, @args);
        $self->_cpscall('Variable', "$inv.InvokeMethod(th, ".qm($name).", $pos, $nam)");
    }

    sub call_sub {
        my ($self, $sig, @args) = @_;
        my ($inv, $pos, $nam) = $self->_prepcall($sig, @args);
        $self->_cpscall('Variable', "$inv.Invoke(th, $pos, $nam)");
    }

    sub tail_call_sub {
        my ($self, $sig, @args) = @_;
        my ($inv, $pos, $nam) = $self->_prepcall($sig, @args);
        $self->_emit("return $inv.Invoke(th.caller, $pos, $nam)");
        $self->unreach(1);
    }

    sub clr_bool {
        my ($self, $v) = @_;
        'System.Boolean', $v ? 'true' : 'false';
    }

    sub clr_new {
        my ($self, $class, @args) = @_;
        $class, "new $class(" . join(", ", _odds @args) . ")";
    }

    sub clr_new_arr {
        my ($self, $class, @args) = @_;
        if ($class eq 'Variable' && !@args) {
            return "Variable[]", "Variable.None";
        }
        $class . "[]", "new $class []{" .  join(", ", _odds @args) . "}";
    }

    sub clr_new_zarr {
        my ($self, $class, $nity, $nitems) = @_;
        if ($class eq 'Variable' && $nitems eq "0") {
            return "Variable[]", "Variable.None";
        }
        $class . "[]", "(new $class [$nitems])";
    }

    sub clr_string {
        my ($self, $text) = @_;
        'System.String', qm($text);
    }

    sub clr_char {
        my ($self, $val) = @_;
        'Char', "((char)" . ord($val) . ")";
    }

    sub clr_int {
        my ($self, $val) = @_;
        'Int32', $val;
    }

    sub clr_double {
        my ($self, $val) = @_;
        'Double', "((Double)$val)";
    }

    sub labelid {
        my ($self, $lbl) = @_;
        'Int32', "\@\@L$lbl";
    }

    sub clr_arith {
        my ($self, $op, $ty1, $a1, $ty2, $a2) = @_;
        if ($ty1 ne $ty2) {
            die "Overloaded operations not yet supported";
        }
        $ty1, "($a1 $op $a2)";
    }

    sub clr_compare {
        my ($self, $op, $ty1, $a1, $ty2, $a2) = @_;
        'Boolean', "($a1 $op $a2)";
    }

    sub clr_field_get {
        my ($self, $f, $oty, $obj) = @_;
        my ($nm, $cl, $ty) = CLRTypes->info('f', $oty, $f);
        $ty, "($obj.$nm)";
    }

    sub clr_field_set {
        my ($self, $f, $oty, $obj, $vty, $val) = @_;
        $self->_emit("$obj.$f = $val");
    }

    sub clr_sfield_get {
        my ($self, $f) = @_;
        my ($nm, $cl, $ty) = CLRTypes->info('f', $f);
        $ty, "$nm";
    }

    sub clr_sfield_set {
        my ($self, $f, $vty, $val) = @_;
        $self->_emit(CLRTypes->strip($f) . " = $val");
    }

    sub clr_index_get {
        my ($self, $ixty, $ix, $oty, $obj) = @_;
        my ($nm, $cl, $ty) = CLRTypes->info('i', $oty, 'Item');
        $ty, "($obj" . "[$ix])";
    }

    sub clr_index_set {
        my ($self, $ixty, $ix, $oty, $obj, $vty, $val) = @_;
        $self->_emit("$obj" . "[$ix] = ($val)");
    }

    sub cast {
        my ($self, $type, $vty, $val) = @_;
        $type, "(($type)$val)";
    }

    sub clr_call_direct {
        my ($self, $name, @args) = @_;
        my ($nm, $cl, $rt) = CLRTypes->info('cm', $name);
        if ($cl eq 'c') {
            $self->_cpscall($rt,
                "$nm(" . join(", ", "th", _odds @args) . ")");
        } elsif ($rt ne 'Void') {
            return ($rt, "$nm(" . join(", ", _odds @args) . ")");
        } else {
            $self->_emit("$nm(" . join(", ", _odds @args) . ")");
        }
    }

    sub clr_call_virt {
        my ($self, $name, $ity, $inv, @args) = @_;
        my ($nm, $cl, $rt) = CLRTypes->info('cm', $ity, $name);
        if ($cl eq 'c') {
            $self->_cpscall($rt,
                "$inv.$nm(" . join(", ", "th", _odds @args) . ")");
        } elsif ($rt ne 'Void') {
            return ($rt, "$inv.$nm(" . join(", ", _odds @args) . ")");
        } else {
            $self->_emit("$inv.$nm(" . join(", ", _odds @args) . ")");
        }
    }

    sub rxbprim {
        my ($self, $name, @args) = @_;
        $self->_emit("if (!th.rx.$name(" . join(", ", _odds @args) . ")) goto case \@\@Lbacktrack");
    }

    sub rxpushb {
        my ($self, $tag, $label) = @_;
        push @{ $self->buffer }, "    th.rx.PushBacktrack(\@\@L$label);\n";
    }

    sub return {
        my ($self, $nv, $rty, $rval) = @_;
        return if $self->unreach;
        if ($nv) {
            $self->_emit("th.caller.resultSlot = $rval");
        }
        $self->_emit("return th.caller");
        $self->unreach(1);
    }

    sub push_null {
        my ($self, $ty) = @_;
        # (Int32)null isn't liked much
        $ty, $ty eq 'Int32' ? "0" : "null";
    }

    ###

    sub subinfo_ctor_args {
        my ($self, $outersi, $ltm) = @_;
        for (@{ $self->buffer }, @{ $self->consttab }, @{ $self->ehspans }) {
            s/\@\@L(\w+)/$self->labelname->{$1}/eg;
        }
        (CgOp::clr_string("$::UNITNAME " . $self->name),
         CgOp::rawnewarr('int', map { CgOp::int($_//0) } @{ $self->lineinfo }),
         CgOp::rawsget($::UNITNAME . '.' . $self->csname . ':f,DynBlockDelegate'),
         $outersi, $ltm,
         CgOp::rawnewarr('int', map { CgOp::int($_) } @{ $self->ehspans }),
         (@{ $self->ehlabels } ? CgOp::rawnewarr('str',
              map { CgOp::clr_string($_) } @{ $self->ehlabels }) :
              CgOp::null('clr:string[]')));
    }

    sub csharp {
        my ($self) = @_;
        my $t = '';
        my $name = $self->csname;
        $t .= " " x 4 . "public static Frame $name(Frame th) {\n";
        if ($self->outcap) {
            $t .= " " x 8 . "IP6 _inv; List<Variable> _pos; Dictionary<string,Variable> _nam;\n";
        }
        $t .= " " x 8 . "switch (th.ip) {\n";
        $t .= " " x 12 . "case 0:\n";
        if ($self->lexnsize > 0) {
            $t .= " " x 16 . "th.lexn = new object[" . $self->lexnsize . "];\n";
        }
        if ($self->usednamed) {
            $t .= " " x 16 . "if (th.lex == null) th.lex = new Dictionary<string,object>();\n";
        }
        for (@{ $self->buffer }, @{ $self->consttab }, @{ $self->ehspans }) {
            s/\@\@L(\w+)/$self->labelname->{$1}/eg;
        }
        $t .= " " x 12 . $_ for @{ $self->buffer };
        $t .= " " x 12 . "default:\n";
        $t .= " " x 16 . "return Kernel.Die(th, \"Invalid IP\");\n";
        $t .= " " x 8 . "}\n";
        $t .= " " x 4 . "}\n";
        for (@{ $self->consttab }) {
            $t .= " " x 4 . "private static $_;\n";
        }
        $t;
    }

    sub BUILD {
        my $self = shift;

        for (my $i = 0; $i < $self->minlets; $i++) {
            $self->letused->{$i + 2} = 1;
        }

        CgOpToCLROp::codegen($self, $self->ops);
    }

    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
