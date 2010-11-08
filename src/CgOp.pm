use 5.010;
use strict;
use warnings;

{
    package CgOp;
    use Moose;

    has op  => (isa => 'ArrayRef',       is => 'ro', required => 1);
    has zyg => (isa => 'ArrayRef[CgOp]', is => 'ro', default => sub { [] });

    no Moose;
    __PACKAGE__->meta->make_immutable;
}

{
    package CgOp;
    use Scalar::Util 'blessed';

    # really primitive

    sub prog {
        CgOp->new(op => ['seq'], zyg => [ @_ ]);
    }

    sub span {
        my ($ls,$le,$sy,@r) = @_;
        CgOp->new(op => [span => $ls, $le, $sy], zyg => [prog(@r)]);
    }

    sub null {
        CgOp->new(op => [ push_null => CLRTypes->mapt($_[0]) ]);
    }

    sub ehspan { CgOp->new(op => [ ehspan => @_ ]); }

    sub sink { CgOp->new(op => ['drop'], zyg => [ $_[0] ]); }
    sub sync { CgOp->new(op => ['cpssync']) }

    sub fetch { rawcall($_[0], 'Fetch'); }

    sub how { rawcall($_[0], "HOW"); }

    sub getfield {
        CgOp->new(op => [ 'clr_field_get', $_[0] ], zyg => [ $_[1] ]);
    }

    sub setfield {
        CgOp->new(op => [ 'clr_field_set', $_[0] ],
            zyg => [ $_[1], $_[2] ]);
    }

    sub getindex {
        CgOp->new(op => [ 'clr_index_get' ],
            zyg => [ _str($_[0]), $_[1] ]);
    }

    sub setindex {
        CgOp->new(op  => [ 'clr_index_set' ],
            zyg => [ _str($_[0]), $_[1], $_[2] ]);
    }

    sub getslot { cast($_[1], rawcall($_[2], 'GetSlot', _str($_[0]))); }

    sub setslot { rawcall($_[1], 'SetSlot', _str($_[0]), $_[2]); }

    sub cast {
        CgOp->new(op => [ 'cast', CLRTypes->mapt($_[0]) ], zyg => [ $_[1] ]);
    }

    sub const {
        CgOp->new(op => [ 'const' ], zyg => [ $_[0] ]);
    }

    sub newscalar {
        rawscall('Kernel.NewROScalar', $_[0]);
    }

    sub newblankrwscalar {
        rawscall('Kernel.NewRWScalar', rawsget('Kernel.AnyMO'),
            rawsget('Kernel.AnyP'));
    }

    sub newrwscalar { rawscall('Kernel.NewRWScalar', rawsget('Kernel.AnyMO'),
            $_[0]); }

    sub newrwlistvar { rawscall('Kernel.NewRWListVar', $_[0]); }

    sub double { CgOp->new(op => [ 'clr_double', $_[0] ]); }

    sub labelid { CgOp->new(op => [ 'labelid', $_[0] ]) }

    sub int { CgOp->new(op => [ 'clr_int', $_[0] ]); }

    sub bool { CgOp->new(op => [ 'clr_bool', $_[0] ]); }

    sub unbox {
        cast($_[0], rawscall('Kernel.UnboxAny', $_[1]));
    }

    # begin smarter constructors
    sub _str { blessed($_[0]) ? $_[0] : clr_string($_[0]) }
    sub _int { blessed($_[0]) ? $_[0] : CgOp::int($_[0]) }

    sub noop { prog() }

    sub rnull {
        prog($_[0], corelex('Nil'));
    }

    sub getattr {
        fetch(varattr($_[0], $_[1]));
    }

    sub varattr { getslot($_[0], 'var', $_[1]); }

    sub newblanklist {
        newrwlistvar(ternary(
                compare('==', rawsget('Kernel.ArrayP'), null('obj')),
                null('obj'),
                fetch(methodcall(newscalar(rawsget('Kernel.ArrayP')), 'new'))));
    }

    sub newblankhash {
        newrwlistvar(
                fetch(methodcall(newscalar(rawsget('Kernel.HashP')), 'new')));
    }

    sub string_var { box('Str', clr_string($_[0])); }

    sub box {
        rawscall('Kernel.BoxAny', $_[1],
            blessed($_[0]) ? $_[0] :
            ($_[0] eq 'Str') ? rawsget('Kernel.StrP') :
                fetch(corelex($_[0])));
    }

    sub obj_is_defined { rawcall($_[0], 'IsDefined') }
    sub obj_typename { rawcall($_[0], 'GetTypeName') }
    sub obj_what { rawcall($_[0], 'GetTypeObject') }
    sub obj_llhow { getfield('mo', $_[0]) }
    sub obj_isa { rawcall($_[0], 'Isa', $_[1]) }
    sub obj_does { rawcall($_[0], 'Does', $_[1]) }
    sub obj_newblank { rawnew('clr:DynObject', $_[0]) }

    sub var_islist { getfield('islist', $_[0]) }

    sub llhow_name { getfield('name', $_[0]) }
    sub stab_privatemethod { rawcall($_[0], 'GetPrivateMethod', $_[1]) }
    sub stab_what { getfield('typeObject', $_[0]) }

    sub varhash_setindex { setindex(@_) }
    sub varhash_getindex { getindex(@_) }
    sub varhash_contains_key { rawcall($_[0], 'ContainsKey', _str($_[1])) }
    sub varhash_delete_key { rawcall($_[0], 'Remove', _str($_[1])) }
    sub varhash_clear { rawcall($_[0], 'Clear') }
    sub varhash_keys { rawscall('Kernel.KeysHelper', $_[0]) }
    sub varhash_dup { rawnew('varhash', $_[0]) }
    sub varhash_new { rawnew('varhash') }

    sub newgeneralvar { rawnew('clr:SimpleVariable', $_[0], $_[1], rawsget('Kernel.AnyMO'), $_[2], $_[3]) }
    sub poscount { getfield('Length', getfield('pos', callframe())) }

    sub num_to_string { rawcall($_[0], 'ToString') }
    sub str_length { getfield('Length', $_[0]) }
    sub str_substring { rawcall($_[0], 'Substring', $_[1], $_[2]) }
    sub str_chr { rawnew('str', cast('clr:System.Char', $_[0]), CgOp::int(1)) }
    sub strcmp { rawscall('String.CompareOrdinal', $_[0], $_[1]) }

    sub strbuf_new { rawnew('strbuf') }
    sub strbuf_append { rawcall($_[0], 'Append', $_[1]) }
    sub strbuf_seal { rawcall($_[0], 'ToString') }

    sub say { rawscall('Console.WriteLine', $_[0]) }
    sub note { rawscall('Console.Error.WriteLine', $_[0]) }
    sub exit { rawscall('System.Environment.Exit', $_[0]) }
    sub slurp { rawscall('System.IO.File.ReadAllText', $_[0]) }

    sub treader_getc    { rawcall($_[0], 'Read:m,Int32') }
    sub treader_slurp   { rawcall($_[0], 'ReadToEnd:m,String') }
    sub treader_getline { rawcall($_[0], 'ReadLine:m,String') }
    sub treader_stdin   { rawscall('Kernel.OpenStdin:m,System.IO.TextReader') }

    sub fvarlist_length { getfield('Length', $_[0]) }
    sub fvarlist_new { rawnewarr('var', @_) }
    sub fvarlist_item { getindex($_[0], $_[1]) }

    sub vvarlist_from_fvarlist { rawnew('vvarlist', $_[0]) }
    sub vvarlist_to_fvarlist { rawcall($_[0], 'CopyAsArray:m,Variable[]') }
    sub vvarlist_clone { rawnew('vvarlist', $_[0]) }
    sub vvarlist_new_empty { rawnew('vvarlist') }
    sub vvarlist_shift { rawcall($_[0], 'Shift') }
    sub vvarlist_pop { rawcall($_[0], 'Pop') }
    sub vvarlist_count { rawcall($_[0], 'Count') }
    sub vvarlist_unshift { rawcall($_[0], 'Unshift', $_[1]) }
    sub vvarlist_unshiftn { rawcall($_[0], 'UnshiftN', $_[1]) }
    sub vvarlist_push { rawcall($_[0], 'Push', $_[1]) }
    sub vvarlist_append { rawcall($_[0], 'PushD:m,Void', $_[1]) }
    sub vvarlist_item { getindex($_[0], $_[1]) }
    sub vvarlist_sort { rawscall('Kernel.SortHelper', callframe(), $_[0], $_[1]) }

    sub frame_caller { getfield('caller', $_[0]) }
    sub frame_file { rawcall($_[0], 'ExecutingFile') }
    sub frame_line { rawcall($_[0], 'ExecutingLine') }
    sub frame_hint { rawcall($_[0], 'LexicalFind', $_[1]) }

    sub cursor_start   { rawnew('cursor', $_[0], $_[1]) }
    sub cursor_pos     { getfield('pos', $_[0]) }
    sub cursor_from    { getfield('from', $_[0]) }
    sub cursor_butpos  { rawcall($_[0], 'At', $_[1]) }
    sub cursor_backing { rawcall($_[0], 'GetBacking:m,String') }
    sub cursor_dows    { rawcall($_[0], 'SimpleWS') }
    sub cursor_item    { rawcall($_[0], 'GetKey', $_[1]) }
    sub cursor_unpackcaps { rawcall($_[0], 'UnpackCaps:m,Void', $_[1]) }
    sub cursor_O       { rawcall($_[0], 'O:m,Variable', $_[1]) }
    sub cursor_synthetic { rawnew('cursor', @_[0,1,2,3]) }
    sub cursor_synthcap{ rawcall($_[0], 'SynPushCapture:m,Void', @_[1,2]) }
    sub cursor_fresh   { rawcall($_[0], 'FreshClass:m,Cursor', $_[1]) }
    sub rxstripcaps    { rawcall($_[0], 'StripCaps:m,Cursor') }

    sub fcclist_new    { rawnewarr('cc', @_) }
    sub fladlist_new   { rawnewarr('lad', @_) }

    sub bget { getfield('v', $_[0]) }
    sub bset { setfield('v', $_[0], $_[1]) }

    sub default_new    { rawscall('Kernel.DefaultNew', $_[0]) }
    sub cotake         { rawscall('Kernel.CoTake', $_[0]) }
    sub take           { rawscall('Kernel.Take', $_[0]) }
    sub control        { rawscall('Kernel.SearchForHandler', CgOp::int(shift()),
            @_) }
    sub context_get    { rawscall('Kernel.ContextHelper', callframe(), $_[0], $_[1]) }
    sub startgather    { rawscall('Kernel.GatherHelper', $_[0]) }
    sub get_first      { rawscall('Kernel.GetFirst', $_[0]) }
    sub promote_to_list{ rawscall('Kernel.PromoteToList:c,Variable', $_[0]) }
    sub instrole       { rawscall('Kernel.InstantiateRole', $_[0]) }
    sub role_apply     { rawscall('Kernel.RoleApply', $_[0], $_[1]) }
    sub iter_hasflat { rawscall('Kernel.IterHasFlat:c,Boolean', $_[0], bool(1)) }
    sub iter_hasarg { rawscall('Kernel.IterHasFlat:c,Boolean', $_[0], bool(0)) }
    sub iter_to_list { rawscall('Kernel.IterToList:m,Void', $_[0], $_[1]) }
    sub iter_flatten { rawscall('Kernel.IterFlatten:c,VarDeque', $_[0]) }
    sub iter_copy_elems { rawscall('Kernel.IterCopyElems:m,VarDeque', $_[0]) }

    sub sig_slurp_capture { rawscall('Kernel.SigSlurpCapture:m,IP6', callframe()) }

    sub newboundvar {
        rawscall('Kernel.NewBoundVar', bool($_[0] || $_[1]), bool($_[1]),
            rawsget('Kernel.AnyMO'), $_[2]);
    }

    sub assign {
        rawscall('Kernel.Assign', $_[0], $_[1]);
    }

    sub compare {
        CgOp->new(op => [ 'clr_compare', $_[0] ], zyg => [ $_[1], $_[2] ]);
    }

    sub arith {
        CgOp->new(op => [ 'clr_arith', $_[0] ], zyg => [ $_[1], $_[2] ]);
    }

    # Not a CgOp function, rewritten by the resolve_lex pass
    sub scopedlex {
        my $n = shift;
        CgOp->new(op => [ scopelex => $n ], zyg => [ @_ ]);
    }
    sub corelex {
        my $n = shift;
        CgOp->new(op => [ corelex => $n ], zyg => [ @_ ]);
    }
    sub specificlex {
        my ($ref, $n) = splice @_, 0, 2;
        CgOp->new(op => [ specificlex => @$ref, $n ], zyg => [ @_ ]);
    }

    sub class_ref { CgOp->new(op => [ class_ref => @_ ]); }

    sub _process_arglist {
        my $ar = shift;
        my @sig;
        my $j = 0;
        for (my $i = 0; $i < @$ar; ) {
            if (blessed($ar->[$i])) {
                push @sig, '';
            } else {
                push @sig, $ar->[$i++];
            }
            $ar->[$j++] = $ar->[$i++];
        }
        $#$ar = $j - 1;
        @sig;
    }

    sub subcall {
        my ($sub, @args) = @_;
        my @sig = _process_arglist(\@args);
        CgOp->new(op => [ 'call_sub', \@sig ], zyg => [ $sub, @args ]);
    }

    sub methodcall {
        my ($obj, $name, @args) = @_;
        my @sig = _process_arglist(\@args);
        let($obj, sub {
            CgOp->new(op => [ 'call_method', $name, ['', @sig] ],
                zyg => [ fetch($_[0]), $_[0], @args ])});
    }

    sub callframe { CgOp->new(op => [ 'callframe' ]); }

    sub rxframe { getfield('rx', callframe) }
    sub rxcall { rawcall(rxframe, @_) }
    sub pushcut { rxcall('PushCutGroup', clr_string($_[0])) }
    sub popcut { rxcall('PopCutGroup') }

    sub rxinit {
        setfield('rx', callframe(), rawnew('clr:RxFrame', $_[0], $_[1],
                bool($_[2]), bool($_[3])))
    }
    sub rxpushcapture {
        my $c = shift;
        rxcall('PushCapture', const(rawnewarr('str',
                    map { clr_string($_) } @_)), $c);
    }
    sub rxend         { rxcall('End') }
    sub rxfinalend    { rxcall('FinalEnd') }
    sub rxbacktrack   { rxcall('Backtrack') }
    sub rxgetquant    { rxcall('GetQuant') }
    sub rxsetquant    { rxcall('SetQuant:m,Void', $_[0]) }
    sub rxopenquant   { rxcall('OpenQuant') }
    sub rxclosequant  { rxcall('CloseQuant') }
    sub rxincquant    { rxcall('IncQuant') }
    sub rxsetclass    { rxcall('SetClass', $_[0]) }
    sub rxsetpos      { rxcall('SetPos', $_[0]) }
    sub rxsetcapsfrom { rxcall('SetCapturesFrom:m,Void', $_[0]) }
    sub rxgetpos      { rxcall('GetPos:m,Int32') }
    sub rxcommitgroup { rxcall('CommitGroup', $_[0]) }

    sub cc_expr { rawnew('cc', rawnewarr('int',
                map { CgOp::int($_) } @{ $_[0] })) }
    sub construct_lad {
        my ($l) = @_;
        my $r = ref $l;
        if (!$r) {
            return CgOp::clr_string($l);
        } elsif ($r eq 'ARRAY') {
            if (!@$l || ref ($l->[0])) {
                return fladlist_new(map { construct_lad($_) } @$l);
            } else {
                my ($h,@r) = @$l;
                return CgOp::rawnew("clr:LAD$h", map { construct_lad($_) } @r);
            }
        } else {
            return $l;
        }
    }

    sub letvar {
        $_[1] ?
            CgOp->new(op => [ 'poke_let', $_[0] ], zyg => [ $_[1] ]):
            CgOp->new(op => [ 'peek_let', $_[0] ]);
    }

    sub clr_string {
        Carp::confess "invalid undef in clr_string" unless defined $_[0];
        CgOp->new(op => [ 'clr_string', $_[0] ]); }

    sub char { CgOp->new(op => [ 'clr_char', $_[0] ]); }

    sub withtypes {
        if (blessed($_[0])) {
            prog(@_);
        } else {
            my $n = shift;
            my $t = shift;
            letn($n, null($t), withtypes(@_));
        }
    }

    sub return {
        $_[0] ?
            CgOp->new(op => [ 'return', 1 ], zyg => [ $_[0] ]) :
            CgOp->new(op => [ return => 0]);
    }

    sub rawscall {
        my ($name, @args) = @_;
        CgOp->new(op => [ 'clr_call_direct', $name ], zyg => [ @args ]);
    }

    sub rawcall {
        my ($inv, $name, @args) = @_;
        CgOp->new(op => [ 'clr_call_virt', $name ], zyg => [ $inv, @args ]);
    }

    sub label { CgOp->new(op => [ 'labelhere', $_[0] ]); }

    sub goto {
        my ($name) = @_;
        CgOp->new(op => [ 'goto', $name ]);
    }

    sub cgoto { CgOp->new(op => [ 'cgoto', $_[0] ], zyg => [ $_[1] ]); }

    sub ncgoto { CgOp->new(op => [ 'ncgoto', $_[0] ], zyg => [ $_[1] ]); }

    sub rxpushb {
        my ($tag, $lbl) = @_;
        CgOp->new(op => [ 'rxpushb', $tag, $lbl ]);
    }

    sub rxbprim {
        my ($name, @args) = @_;
        CgOp->new(op => [ 'rxbprim', $name ], zyg => [ @args ]);
    }

    sub rawsget {
        Carp::confess "Undefined name in rawsget" unless defined $_[0];
        CgOp->new(op => [ 'clr_sfield_get', $_[0] ]);
    }

    sub rawsset {
        Carp::confess "Undefined name in rawsset" unless defined $_[0];
        CgOp->new(op => [ 'clr_sfield_set', $_[0] ], zyg => [ $_[1] ]);
    }

    sub rawnew {
        my ($name, @args) = @_;
        CgOp->new(op => [ 'clr_new', CLRTypes->mapt($name) ], zyg => \@args);
    }

    sub rawnewarr {
        my ($name, @args) = @_;
        CgOp->new(op => [ 'clr_new_arr', CLRTypes->mapt($name) ],
            zyg => \@args);
    }

    sub rawnewzarr {
        my ($name, $ni) = @_;
        CgOp->new(op => [ 'clr_new_zarr', CLRTypes->mapt($name) ], zyg => [ $ni ]);
    }

    sub ann {
        my ($file, $line, $stuff) = @_;
        CgOp->new(op => [ 'ann', $line ], zyg => [$stuff]);
    }

    sub die {
        my ($msg) = @_;
        if (blessed($msg)) {
            rawscall('Kernel.SearchForHandler', &int(5), null('clr:Niecza.Frame'),
                &int(-1), null('str'), newscalar($msg));
        } else {
            rawscall('Kernel.Die', clr_string($msg));
        }
    }

    sub letn {
        my (@stuff) = @_;
        if (blessed($stuff[0])) {
            @stuff;
        } else {
            if (!@stuff) {
                Carp::confess "Invalid letn protocol";
            }
            my ($name, $value) = splice @stuff, 0, 2;
            CgOp->new(op => ['let', $name], zyg => [ $value, letn(@stuff) ]);
        }
    }

    sub pos { CgOp->new(op => [ 'pos' ], zyg => [ _int($_[0]) ]); }

    sub ternary {
        CgOp->new(op => ['ternary'], zyg => [ $_[0], $_[1], $_[2] ]);
    }

    sub whileloop {
        CgOp->new(op => ['while', $_[0], $_[1]], zyg => [ $_[2], $_[3] ]);
    }

    my $nextlet = 0;
    sub let {
        my ($head, $bodyf) = @_;
        my $v = ($nextlet++);
        letn($v, $head, $bodyf->(letvar($v)));
    }
}

1;
