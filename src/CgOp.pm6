class CgOp;

method _cgop(*@bits) {
    for @bits { $_ // die "Illegal undef in cgop @bits[0]" }
    [ @bits ];
}

for <
        ann arith assign bget bif_array_constructor bif_at_key bif_at_pos
        bif_bool bif_chars bif_chr bif_coerce_to_int bif_coerce_to_num
        bif_comma bif_cross bif_defined bif_delete_key bif_divide
        bif_exists_key bif_gettimeofday bif_grep bif_hash bif_hash_keys
        bif_hash_kv bif_hash_pairs bif_hash_values bif_item bif_list
        bif_make bif_map bif_minus bif_mod bif_mul bif_negate bif_not
        bif_now bif_num bif_numand bif_numcompl bif_numeq bif_numge
        bif_numgt bif_numle bif_numlshift bif_numlt bif_numne bif_numor
        bif_numrshift bif_numxor bif_ord bif_pair bif_plus bif_postinc
        bif_rand bif_rat_approx bif_simple_eval bif_str bif_streq
        bif_strge bif_strgt bif_strle bif_strlt bif_strne bif_substr3
        bif_zip bool box boxlist bset callframe callnext call_uncloned_sub
        cast cgoto char class_ref compare const context_get control
        corelex cotake cursor_ast cursor_backing cursor_butpos cursor_dows
        cursor_fresh cursor_from cursor_item cursor_O cursor_pos
        cursor_reduced cursor_start cursor_synthcap cursor_synthetic
        cursor_unmatch cursor_unpackcaps default_new die do_require ehspan
        exactnum exit fcclist_new fetch fladlist_new foreign_class
        frame_caller frame_file frame_hint frame_line from_json from_jsync
        fvarlist_item fvarlist_length fvarlist_new getargv getfield
        get_first getindex getslot goto how instrole int iter_copy_elems
        iter_flatten iter_hasarg iter_hasflat iter_to_list label labelid
        letn letscope letvar llhow_name ltm_push_alts mrl_count mrl_index
        ncgoto newblankrwscalar newboundvar newrwlistvar newrwscalar
        newscalar newtypedscalar newvarrayvar newvhashvar newvnewarrayvar
        newvnewhashvar newvsubvar note null num_to_string obj_asbool
        obj_asdef obj_asnum obj_asstr obj_at_key obj_at_pos obj_delete_key
        obj_does obj_exists_key obj_getbool obj_getdef obj_getnum
        obj_getstr obj_isa obj_is_defined obj_llhow obj_newblank
        obj_typename obj_vasbool obj_vasdef obj_vasnum obj_vasstr
        obj_vat_key obj_vat_pos obj_vdelete_key obj_vexists_key obj_what
        outerlex path_any_exists path_change_ext path_combine
        path_dir_exists path_file_exists path_modified path_realpath
        popcut print prog promote_to_list pushcut rawcall rawnew rawnewarr
        rawnewzarr rawscall rawsget rawsset return role_apply run_dispatch
        rxbacktrack rxbprim rxcall rxclosequant rxcommitgroup rxend
        rxfinalend rxframe rxgetpos rxgetquant rxincorpcut rxincorpshift
        rxincquant rxinit rxopenquant rxpushb rxpushcapture rxsetcapsfrom
        rxsetclass rxsetpos rxsetquant rxstripcaps say scopedlex setbox
        setfield setindex setslot set_status sig_slurp_capture sink slurp
        span specificlex spew stab_privatemethod stab_what startgather
        start_iter status_get str strbuf_append strbuf_new strbuf_seal
        str_chr strcmp str_flip str_length str_substring str_tolower
        str_tonum str_toupper take ternary to_json to_jsync treader_getc
        treader_getline treader_open treader_slurp treader_stdin unbox
        var_get_var varhash_clear varhash_contains_key varhash_delete_key
        varhash_dup varhash_getindex varhash_new varhash_setindex
        var_islist var_new_tied vvarlist_append vvarlist_clone
        vvarlist_count vvarlist_from_fvarlist vvarlist_item
        vvarlist_new_empty vvarlist_new_singleton vvarlist_pop
        vvarlist_push vvarlist_shift vvarlist_sort vvarlist_to_fvarlist
        vvarlist_unshift vvarlist_unshiftn whileloop xspan bif_times
        bif_divop obj_can bif_sqrt bif_push bif_pop bif_unshift bif_shift
        newarray newhash
        > -> $name {
    my $fnc = anon sub CgOperator (\|@parcel) {
        Q:CgOp {
            (letn arr {[@parcel]}
                  items (getslot items vvarlist (@ (l arr)))
              (sink (vvarlist_shift (l items)))
              (vvarlist_unshift (l items) {$name})
              (l arr))
        }
    };
    Q:CgOp { (rnull (_cgop _addmethod (obj_llhow (@ {CgOp})) 0
        (obj_getstr {$name}) (@ {$fnc}))) }
}
Q:CgOp { (rnull (_cgop _invalidate (obj_llhow (@ {CgOp})))) };

method double($x) {
    # Hack - prevent JSON syntax errors
    my $str = ~$x;
    self._cgop('double', ($str eq 'Infinity' || $str eq 'NaN' ||
        $str eq '-Infinity') ?? $str !! $x);
}

method newblanklist() { CgOp.newarray }
method newblankhash() { CgOp.newhash }
method string_var($x) { CgOp.box('Str', CgOp.str($x)) }
method noop() { CgOp.prog() }
method rnull($p) { CgOp.prog($p, CgOp.corelex('Nil')) }
method getattr($a,$v) { CgOp.fetch(CgOp.varattr($a,$v)) }
method varattr($a,$v) { CgOp.getslot($a, 'var', $v) }

my $nextlet = 0;
method let($head,$body) {
    my $v = "!L" ~ $nextlet++;
    CgOp.letn($v, $head, $body(CgOp.letvar($v)));
}

method cc_expr($cc) { CgOp._cgop('newcc', @( $cc.terms )) }

method _process_arglist(*@araw) {
    my @aout;
    my $sig = '';
    while @araw {
        my $o = @araw.shift;
        if $o ~~ List {
            push @aout, $o;
            $sig = $sig ~ "\0";
        } else {
            $sig = $sig ~ chr(chars $o) ~ $o;
            push @aout, @araw.shift;
        }
    }
    $sig, $( @aout );
}

method subcall($sub, *@args) {
    my ($sig, $aout) = CgOp._process_arglist(@args);
    CgOp._cgop('subcall', $sig, $sub, @$aout);
}

method methodcall($obj, $name, *@args) {
    my ($sig, $aout) = CgOp._process_arglist(@args);
    CgOp._cgop('methodcall', $name, $sig, $obj, @$aout);
}
