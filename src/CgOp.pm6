# 049d8e5b643845aa2e4409f24ebe039668211dd2

class CgOp;

sub chr($x) { Q:CgOp { (rawscall Builtins,Kernel.Chr {$x}) } }

method _cgop(*@bits) {
    for @bits { $_ // die "Illegal undef in cgop @bits[0]" }
    [ @bits ];
}

method getfield (*@_) { self._cgop("getfield", @_) }
method getindex (*@_) { self._cgop("getindex", @_) }
method rawcall (*@_) { self._cgop("rawcall", @_) }
method rawnew (*@_) { self._cgop("rawnew", @_) }
method rawnewarr (*@_) { self._cgop("rawnewarr", @_) }
method rawnewzarr (*@_) { self._cgop("rawnewzarr", @_) }
method rawscall (*@_) { self._cgop("rawscall", @_) }
method rawsget (*@_) { self._cgop("rawsget", @_) }
method rawsset (*@_) { self._cgop("rawsset", @_) }
method setfield (*@_) { self._cgop("setfield", @_) }
method setindex (*@_) { self._cgop("setindex", @_) }
method ann (*@_) { self._cgop("ann", @_) }
method arith (*@_) { self._cgop("arith", @_) }
method assign (*@_) { self._cgop("assign", @_) }
method bget (*@_) { self._cgop("bget", @_) }
method bif_at_key (*@_) { self._cgop("bif_at_key", @_) }
method bif_at_pos (*@_) { self._cgop("bif_at_pos", @_) }
method bif_bool (*@_) { self._cgop("bif_bool", @_) }
method bif_chars (*@_) { self._cgop("bif_chars", @_) }
method bif_chr ($i) { self._cgop("bif_chr", $i) }
method bif_defined (*@_) { self._cgop("bif_defined", @_) }
method bif_delete_key (*@_) { self._cgop("bif_delete_key", @_) }
method bif_divide (*@_) { self._cgop("bif_divide", @_) }
method bif_exists_key (*@_) { self._cgop("bif_exists_key", @_) }
method bif_gettimeofday () { self._cgop("bif_gettimeofday") }
method bif_hash_keys (*@_) { self._cgop("bif_hash_keys", @_) }
method bif_hash_kv (*@_) { self._cgop("bif_hash_kv", @_) }
method bif_hash_pairs (*@_) { self._cgop("bif_hash_pairs", @_) }
method bif_hash_values (*@_) { self._cgop("bif_hash_values", @_) }
method bif_minus (*@_) { self._cgop("bif_minus", @_) }
method bif_mul (*@_) { self._cgop("bif_mul", @_) }
method bif_negate (*@_) { self._cgop("bif_negate", @_) }
method bif_not (*@_) { self._cgop("bif_not", @_) }
method bif_num (*@_) { self._cgop("bif_num", @_) }
method bif_numand ($n1, $n2) { self._cgop("bif_numand", $n1, $n2) }
method bif_numcompl ($n) { self._cgop("bif_numcompl", $n) }
method bif_numeq (*@_) { self._cgop("bif_numeq", @_) }
method bif_numge (*@_) { self._cgop("bif_numge", @_) }
method bif_numgt (*@_) { self._cgop("bif_numgt", @_) }
method bif_numle (*@_) { self._cgop("bif_numle", @_) }
method bif_numlshift ($n1, $n2) { self._cgop("bif_numlshift", $n1, $n2) }
method bif_numlt (*@_) { self._cgop("bif_numlt", @_) }
method bif_numne (*@_) { self._cgop("bif_numne", @_) }
method bif_numor ($n1, $n2) { self._cgop("bif_numor", $n1, $n2) }
method bif_numxor ($n1, $n2) { self._cgop("bif_numxor", $n1, $n2) }
method bif_numrshift ($n1, $n2) { self._cgop("bif_numrshift", $n1, $n2) }
method bif_ord ($n) { self._cgop("bif_ord", $n) }
method bif_plus (*@_) { self._cgop("bif_plus", @_) }
method bif_postinc (*@_) { self._cgop("bif_postinc", @_) }
method bif_str (*@_) { self._cgop("bif_str", @_) }
method bif_streq (*@_) { self._cgop("bif_streq", @_) }
method bif_strge (*@_) { self._cgop("bif_strge", @_) }
method bif_strgt (*@_) { self._cgop("bif_strgt", @_) }
method bif_strle (*@_) { self._cgop("bif_strle", @_) }
method bif_strlt (*@_) { self._cgop("bif_strlt", @_) }
method bif_strne (*@_) { self._cgop("bif_strne", @_) }
method bif_substr3 (*@_) { self._cgop("bif_substr3", @_) }
method bool (*@_) { self._cgop("bool", @_) }
method box (*@_) { self._cgop("box", @_) }
method bset (*@_) { self._cgop("bset", @_) }
method callframe (*@_) { self._cgop("callframe", @_) }
method call_uncloned_sub (*@_) { self._cgop("call_uncloned_sub", @_) }
method cast (*@_) { self._cgop("cast", @_) }
method cgoto (*@_) { self._cgop("cgoto", @_) }
method char (*@_) { self._cgop("char", @_) }
method class_ref (*@_) { self._cgop("class_ref", @_) }
method compare (*@_) { self._cgop("compare", @_) }
method const (*@_) { self._cgop("const", @_) }
method context_get (*@_) { self._cgop("context_get", @_) }
method control (*@_) { self._cgop("control", @_) }
method corelex (*@_) { self._cgop("corelex", @_) }
method cotake (*@_) { self._cgop("cotake", @_) }
method cursor_backing (*@_) { self._cgop("cursor_backing", @_) }
method cursor_butpos (*@_) { self._cgop("cursor_butpos", @_) }
method cursor_dows (*@_) { self._cgop("cursor_dows", @_) }
method cursor_fresh (*@_) { self._cgop("cursor_fresh", @_) }
method cursor_from (*@_) { self._cgop("cursor_from", @_) }
method cursor_item (*@_) { self._cgop("cursor_item", @_) }
method cursor_O (*@_) { self._cgop("cursor_O", @_) }
method cursor_pos (*@_) { self._cgop("cursor_pos", @_) }
method cursor_start (*@_) { self._cgop("cursor_start", @_) }
method cursor_synthcap (*@_) { self._cgop("cursor_synthcap", @_) }
method cursor_synthetic (*@_) { self._cgop("cursor_synthetic", @_) }
method cursor_unpackcaps (*@_) { self._cgop("cursor_unpackcaps", @_) }
method cursor_unmatch ($c) { self._cgop("cursor_unmatch", $c) }
method cursor_reduced ($c) { self._cgop("cursor_reduced", $c) }
method default_new (*@_) { self._cgop("default_new", @_) }
method die (*@_) { self._cgop("die", @_) }
method do_require (*@_) { self._cgop("do_require", @_) }
method ehspan (*@_) { self._cgop("ehspan", @_) }
method exit (*@_) { self._cgop("exit", @_) }
method fcclist_new (*@_) { self._cgop("fcclist_new", @_) }
method fetch (*@_) { self._cgop("fetch", @_) }
method fladlist_new (*@_) { self._cgop("fladlist_new", @_) }
method foreign_class (*@_) { self._cgop("foreign_class", @_) }
method frame_caller (*@_) { self._cgop("frame_caller", @_) }
method frame_file (*@_) { self._cgop("frame_file", @_) }
method frame_hint (*@_) { self._cgop("frame_hint", @_) }
method frame_line (*@_) { self._cgop("frame_line", @_) }
method from_jsync (*@_) { self._cgop("from_jsync", @_) }
method fvarlist_item (*@_) { self._cgop("fvarlist_item", @_) }
method fvarlist_length (*@_) { self._cgop("fvarlist_length", @_) }
method fvarlist_new (*@_) { self._cgop("fvarlist_new", @_) }
method getargv (*@_) { self._cgop("getargv", @_) }
method get_first (*@_) { self._cgop("get_first", @_) }
method getslot (*@_) { self._cgop("getslot", @_) }
method goto (*@_) { self._cgop("goto", @_) }
method how (*@_) { self._cgop("how", @_) }
method instrole (*@_) { self._cgop("instrole", @_) }
method int (*@_) { self._cgop("int", @_) }
method iter_copy_elems (*@_) { self._cgop("iter_copy_elems", @_) }
method iter_flatten (*@_) { self._cgop("iter_flatten", @_) }
method iter_hasarg (*@_) { self._cgop("iter_hasarg", @_) }
method iter_hasflat (*@_) { self._cgop("iter_hasflat", @_) }
method iter_to_list (*@_) { self._cgop("iter_to_list", @_) }
method label (*@_) { self._cgop("label", @_) }
method labelid (*@_) { self._cgop("labelid", @_) }
method letn (*@_) { self._cgop("letn", @_) }
method letvar (*@_) { self._cgop("letvar", @_) }
method llhow_name (*@_) { self._cgop("llhow_name", @_) }
method ncgoto (*@_) { self._cgop("ncgoto", @_) }
method newblankrwscalar (*@_) { self._cgop("newblankrwscalar", @_) }
method newboundvar (*@_) { self._cgop("newboundvar", @_) }
method newrwlistvar (*@_) { self._cgop("newrwlistvar", @_) }
method newrwscalar (*@_) { self._cgop("newrwscalar", @_) }
method newscalar (*@_) { self._cgop("newscalar", @_) }
method newvarrayvar (*@_) { self._cgop("newvarrayvar", @_) }
method newvhashvar (*@_) { self._cgop("newvhashvar", @_) }
method newvnewarrayvar (*@_) { self._cgop("newvnewarrayvar", @_) }
method newvnewhashvar (*@_) { self._cgop("newvnewhashvar", @_) }
method newvsubvar (*@_) { self._cgop("newvsubvar", @_) }
method note (*@_) { self._cgop("note", @_) }
method null (*@_) { self._cgop("null", @_) }
method num_to_string (*@_) { self._cgop("num_to_string", @_) }
method obj_asbool (*@_) { self._cgop("obj_asbool", @_) }
method obj_asdef (*@_) { self._cgop("obj_asdef", @_) }
method obj_asnum (*@_) { self._cgop("obj_asnum", @_) }
method obj_asstr (*@_) { self._cgop("obj_asstr", @_) }
method obj_at_key (*@_) { self._cgop("obj_at_key", @_) }
method obj_at_pos (*@_) { self._cgop("obj_at_pos", @_) }
method obj_delete_key (*@_) { self._cgop("obj_delete_key", @_) }
method obj_does (*@_) { self._cgop("obj_does", @_) }
method obj_exists_key (*@_) { self._cgop("obj_exists_key", @_) }
method obj_getbool (*@_) { self._cgop("obj_getbool", @_) }
method obj_getdef (*@_) { self._cgop("obj_getdef", @_) }
method obj_getnum (*@_) { self._cgop("obj_getnum", @_) }
method obj_getstr (*@_) { self._cgop("obj_getstr", @_) }
method obj_isa (*@_) { self._cgop("obj_isa", @_) }
method obj_is_defined (*@_) { self._cgop("obj_is_defined", @_) }
method obj_llhow (*@_) { self._cgop("obj_llhow", @_) }
method obj_newblank (*@_) { self._cgop("obj_newblank", @_) }
method obj_typename (*@_) { self._cgop("obj_typename", @_) }
method obj_vasbool (*@_) { self._cgop("obj_vasbool", @_) }
method obj_vasdef (*@_) { self._cgop("obj_vasdef", @_) }
method obj_vasnum (*@_) { self._cgop("obj_vasnum", @_) }
method obj_vasstr (*@_) { self._cgop("obj_vasstr", @_) }
method obj_vat_key (*@_) { self._cgop("obj_vat_key", @_) }
method obj_vat_pos (*@_) { self._cgop("obj_vat_pos", @_) }
method obj_vdelete_key (*@_) { self._cgop("obj_vdelete_key", @_) }
method obj_vexists_key (*@_) { self._cgop("obj_vexists_key", @_) }
method obj_what (*@_) { self._cgop("obj_what", @_) }
method popcut (*@_) { self._cgop("popcut", @_) }
method print (*@_) { self._cgop("print", @_) }
method prog (*@_) { self._cgop("prog", @_) }
method promote_to_list (*@_) { self._cgop("promote_to_list", @_) }
method pushcut (*@_) { self._cgop("pushcut", @_) }
method return (*@_) { self._cgop("return", @_) }
method role_apply (*@_) { self._cgop("role_apply", @_) }
method rxbacktrack (*@_) { self._cgop("rxbacktrack", @_) }
method rxbprim (*@_) { self._cgop("rxbprim", @_) }
method rxcall (*@_) { self._cgop("rxcall", @_) }
method rxclosequant (*@_) { self._cgop("rxclosequant", @_) }
method rxcommitgroup (*@_) { self._cgop("rxcommitgroup", @_) }
method rxend (*@_) { self._cgop("rxend", @_) }
method rxfinalend (*@_) { self._cgop("rxfinalend", @_) }
method rxframe (*@_) { self._cgop("rxframe", @_) }
method rxgetpos (*@_) { self._cgop("rxgetpos", @_) }
method rxgetquant (*@_) { self._cgop("rxgetquant", @_) }
method rxincquant (*@_) { self._cgop("rxincquant", @_) }
method rxinit (*@_) { self._cgop("rxinit", @_) }
method rxopenquant (*@_) { self._cgop("rxopenquant", @_) }
method rxpushb (*@_) { self._cgop("rxpushb", @_) }
method rxpushcapture (*@_) { self._cgop("rxpushcapture", @_) }
method rxsetcapsfrom (*@_) { self._cgop("rxsetcapsfrom", @_) }
method rxsetclass (*@_) { self._cgop("rxsetclass", @_) }
method rxsetpos (*@_) { self._cgop("rxsetpos", @_) }
method rxsetquant (*@_) { self._cgop("rxsetquant", @_) }
method rxstripcaps (*@_) { self._cgop("rxstripcaps", @_) }
method say (*@_) { self._cgop("say", @_) }
method scopedlex (*@_) { self._cgop("scopedlex", @_) }
method setbox (*@_) { self._cgop("setbox", @_) }
method setslot (*@_) { self._cgop("setslot", @_) }
method set_status (*@_) { self._cgop("set_status", @_) }
method sig_slurp_capture (*@_) { self._cgop("sig_slurp_capture", @_) }
method sink (*@_) { self._cgop("sink", @_) }
method slurp (*@_) { self._cgop("slurp", @_) }
method span (*@_) { self._cgop("span", @_) }
method specificlex (*@_) { self._cgop("specificlex", @_) }
method spew (*@_) { self._cgop("spew", @_) }
method stab_privatemethod (*@_) { self._cgop("stab_privatemethod", @_) }
method stab_what (*@_) { self._cgop("stab_what", @_) }
method startgather (*@_) { self._cgop("startgather", @_) }
method status_get (*@_) { self._cgop("status_get", @_) }
method str (*@_) { self._cgop("str", @_) }
method strbuf_append (*@_) { self._cgop("strbuf_append", @_) }
method strbuf_new (*@_) { self._cgop("strbuf_new", @_) }
method strbuf_seal (*@_) { self._cgop("strbuf_seal", @_) }
method str_chr (*@_) { self._cgop("str_chr", @_) }
method strcmp (*@_) { self._cgop("strcmp", @_) }
method str_length (*@_) { self._cgop("str_length", @_) }
method str_substring (*@_) { self._cgop("str_substring", @_) }
method str_tolower (*@_) { self._cgop("str_tolower", @_) }
method str_toupper (*@_) { self._cgop("str_toupper", @_) }
method str_tonum ($s) { self._cgop("str_tonum", $s) }
method take (*@_) { self._cgop("take", @_) }
method ternary (*@_) { self._cgop("ternary", @_) }
method to_jsync (*@_) { self._cgop("to_jsync", @_) }
method treader_getc (*@_) { self._cgop("treader_getc", @_) }
method treader_getline (*@_) { self._cgop("treader_getline", @_) }
method treader_slurp (*@_) { self._cgop("treader_slurp", @_) }
method treader_stdin (*@_) { self._cgop("treader_stdin", @_) }
method unbox (*@_) { self._cgop("unbox", @_) }
method varhash_clear (*@_) { self._cgop("varhash_clear", @_) }
method varhash_contains_key (*@_) { self._cgop("varhash_contains_key", @_) }
method varhash_delete_key (*@_) { self._cgop("varhash_delete_key", @_) }
method varhash_dup (*@_) { self._cgop("varhash_dup", @_) }
method varhash_getindex (*@_) { self._cgop("varhash_getindex", @_) }
method varhash_new (*@_) { self._cgop("varhash_new", @_) }
method varhash_setindex (*@_) { self._cgop("varhash_setindex", @_) }
method var_islist (*@_) { self._cgop("var_islist", @_) }
method vvarlist_append (*@_) { self._cgop("vvarlist_append", @_) }
method vvarlist_clone (*@_) { self._cgop("vvarlist_clone", @_) }
method vvarlist_count (*@_) { self._cgop("vvarlist_count", @_) }
method vvarlist_from_fvarlist (*@_) { self._cgop("vvarlist_from_fvarlist", @_) }
method vvarlist_item (*@_) { self._cgop("vvarlist_item", @_) }
method vvarlist_new_empty (*@_) { self._cgop("vvarlist_new_empty", @_) }
method vvarlist_new_singleton (*@_) { self._cgop("vvarlist_new_singleton", @_) }
method vvarlist_pop (*@_) { self._cgop("vvarlist_pop", @_) }
method vvarlist_push (*@_) { self._cgop("vvarlist_push", @_) }
method vvarlist_shift (*@_) { self._cgop("vvarlist_shift", @_) }
method vvarlist_sort (*@_) { self._cgop("vvarlist_sort", @_) }
method vvarlist_to_fvarlist (*@_) { self._cgop("vvarlist_to_fvarlist", @_) }
method vvarlist_unshift (*@_) { self._cgop("vvarlist_unshift", @_) }
method vvarlist_unshiftn (*@_) { self._cgop("vvarlist_unshiftn", @_) }
method whileloop (*@_) { self._cgop("whileloop", @_) }
method get_lexer (*@_) { self._cgop("get_lexer", @_) }
method run_protoregex (*@_) { self._cgop("run_protoregex", @_) }
method label_table (*@_) { self._cgop("label_table", @_) }
method mrl_count (*@_) { self._cgop("mrl_count", @_) }
method mrl_index (*@_) { self._cgop("mrl_index", @_) }
method treader_open (*@_) { self._cgop("treader_open", @_) }
method bif_make (*@_) { self._cgop("bif_make", @_) }
method cursor_ast (*@_) { self._cgop("cursor_ast", @_) }
method to_json (*@_) { self._cgop("to_json", @_) }
method from_json (*@_) { self._cgop("from_json", @_) }
method path_file_exists($n) { self._cgop("path_file_exists", $n) }
method path_dir_exists($n) { self._cgop("path_dir_exists", $n) }
method path_any_exists($n) { self._cgop("path_any_exists", $n) }
method path_combine($n1, $n2) { self._cgop("path_combine", $n1, $n2) }
method path_change_ext($n, $ex) { self._cgop("path_change_ext", $n, $ex) }
method path_realpath($n) { self._cgop("path_realpath", $n) }
method path_modified($n) { self._cgop("path_modified", $n) }
method bif_item($i) { self._cgop("bif_item", $i) }
method bif_list($i) { self._cgop("bif_list", $i) }
method bif_hash($i) { self._cgop("bif_hash", $i) }
method bif_grep(*@a) { self._cgop("bif_grep", @a) }
method bif_map(*@a) { self._cgop("bif_map", @a) }
method bif_array_constructor($i) { self._cgop("bif_array_constructor", $i) }
method callnext($cap) { self._cgop("callnext",$cap) }
method bif_zip($fcn,$pcl) { self._cgop("bif_zip",$fcn,$pcl) }
method bif_cross($fcn,$pcl) { self._cgop("bif_cross",$fcn,$pcl) }
method letscope(*@items) { self._cgop('letscope', @items) }
method xspan(*@items) { self._cgop('xspan', @items) }
method bif_mod($x,$y) { self._cgop('bif_mod', $x, $y) }

method double($x) {
    # Hack - prevent JSON syntax errors
    my $str = ~$x;
    self._cgop('double', ($str eq 'Infinity' || $str eq 'NaN' ||
        $str eq '-Infinity') ?? $str !! $x);
}

sub _str($x) { ($x ~~ List) ?? $x !! CgOp.str($x) }
sub _int($x) { ($x ~~ List) ?? $x !! CgOp.int($x) }

method newblanklist() { CgOp.methodcall(CgOp.corelex('Array'), 'new') }
method newblankhash() { CgOp.methodcall(CgOp.corelex('Hash'), 'new') }
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
method construct_lad(*@trees) { CgOp._cgop('ladconstruct', [@trees]) }

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
    CgOp.let($obj, -> $of {
        CgOp._cgop('methodcall', $name, $sig, CgOp.fetch($of), $of, @$aout) });
}
