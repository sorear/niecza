use 5.010;
use strict;
use warnings;

{
    package CgOpNode;
}

package CgOp;
use Scalar::Util 'blessed';

sub _cgop {
    if (grep { !defined } @_) {
        Carp::confess "Illegal undef in cgop $_[0]";
    }
    bless [@_], 'CgOpNode'
}

BEGIN {
    no warnings 'qw';
    my @names = qw<
        getfield, getindex, rawcall, rawnew, rawnewarr, rawnewzarr,
        rawscall, rawsget, rawsset, setfield, setindex,

        ann, arith, assign, bget, bif_at_key, bif_at_pos, bif_bool,
        bif_chars, bif_defined, bif_delete_key, bif_divide,
        bif_exists_key, bif_hash_keys, bif_hash_kv, bif_hash_pairs,
        bif_hash_values, bif_minus, bif_mul, bif_negate, bif_not,
        bif_num, bif_numeq, bif_numge, bif_numgt, bif_numle,
        bif_numlt, bif_numne, bif_plus, bif_postinc, bif_str,
        bif_streq, bif_strge, bif_strgt, bif_strle, bif_strlt,
        bif_strne, bif_substr3, bool, box, bset, callframe,
        call_uncloned_sub, cast, cgoto, char, class_ref, compare,
        const, context_get, control, corelex, cotake,
        cursor_backing, cursor_butpos, cursor_dows, cursor_fresh,
        cursor_from, cursor_item, cursor_O, cursor_pos,
        cursor_start, cursor_synthcap, cursor_synthetic,
        cursor_unpackcaps, default_new, die, do_require, double,
        ehspan, exit, fcclist_new, fetch, fladlist_new,
        foreign_class, frame_caller, frame_file, frame_hint,
        frame_line, from_jsync, fvarlist_item, fvarlist_length,
        fvarlist_new, getargv, get_first, getslot, goto, how,
        instrole, int, iter_copy_elems, iter_flatten, iter_hasarg,
        iter_hasflat, iter_to_list, label, labelid, letn, letvar,
        llhow_name, ncgoto, newblankrwscalar, newboundvar,
        newrwlistvar, newrwscalar, newscalar, newvarrayvar,
        newvhashvar, newvnewarrayvar, newvnewhashvar, newvsubvar,
        note, null, num_to_string, obj_asbool, obj_asdef, obj_asnum,
        obj_asstr, obj_at_key, obj_at_pos, obj_delete_key, obj_does,
        obj_exists_key, obj_getbool, obj_getdef, obj_getnum,
        obj_getstr, obj_isa, obj_is_defined, obj_llhow,
        obj_newblank, obj_typename, obj_vasbool, obj_vasdef,
        obj_vasnum, obj_vasstr, obj_vat_key, obj_vat_pos,
        obj_vdelete_key, obj_vexists_key, obj_what, popcut, print,
        prog, promote_to_list, pushcut, return, role_apply,
        rxbacktrack, rxbprim, rxcall, rxclosequant, rxcommitgroup,
        rxend, rxfinalend, rxframe, rxgetpos, rxgetquant,
        rxincquant, rxinit, rxopenquant, rxpushb, rxpushcapture,
        rxsetcapsfrom, rxsetclass, rxsetpos, rxsetquant,
        rxstripcaps, say, scopedlex, setbox, setslot, set_status,
        sig_slurp_capture, sink, slurp, span, specificlex, spew,
        stab_privatemethod, stab_what, startgather, status_get, str,
        strbuf_append, strbuf_new, strbuf_seal, str_chr, strcmp,
        str_length, str_substring, str_tolower, str_toupper, take,
        ternary, to_jsync, treader_getc, treader_getline,
        treader_slurp, treader_stdin, unbox, varhash_clear,
        varhash_contains_key, varhash_delete_key, varhash_dup,
        varhash_getindex, varhash_new, varhash_setindex, var_islist,
        vvarlist_append, vvarlist_clone, vvarlist_count,
        vvarlist_from_fvarlist, vvarlist_item, vvarlist_new_empty,
        vvarlist_new_singleton, vvarlist_pop, vvarlist_push,
        vvarlist_shift, vvarlist_sort, vvarlist_to_fvarlist,
        vvarlist_unshift, vvarlist_unshiftn, whileloop,
        get_lexer, run_protoregex, label_table, mrl_count, mrl_index,
        treader_open, bif_make, cursor_ast, to_json, from_json,
    >;

    my $code;
    for my $n (@names) {
        chop $n;
        $code .= "sub $n { _cgop(\"$n\", \@_) }\n";
    }
    eval $code;
}

sub _str { blessed($_[0]) ? $_[0] : str($_[0]) }
sub _int { blessed($_[0]) ? $_[0] : CgOp::int($_[0]) }

sub newblanklist { methodcall(corelex('Array'), 'new') }
sub newblankhash { methodcall(corelex('Hash'), 'new') }
sub string_var { box('Str', str($_[0])); }
sub noop { prog() }
sub rnull { prog($_[0], corelex('Nil')); }
sub getattr { fetch(varattr($_[0], $_[1])); }
sub varattr { getslot($_[0], 'var', $_[1]); }

my $nextlet = 0;
sub let {
    my ($head, $bodyf) = @_;
    my $v = ($nextlet++);
    letn($v, $head, $bodyf->(letvar($v)));
}

sub cc_expr { _cgop('newcc', @{ $_[0] }) }
sub construct_lad { _cgop('ladconstruct', @_) }

sub _process_arglist {
    my $ar = shift;
    my $sig = '';
    my $j = 0;
    for (my $i = 0; $i < @$ar; ) {
        my $o = $ar->[$i];
        if (blessed($o)) {
            $sig .= "\0";
        } else {
            $sig .= chr(length($o)) . $o;
            $i++;
        }
        $ar->[$j++] = $ar->[$i++];
    }
    $#$ar = $j - 1;
    $sig;
}

sub subcall {
    my ($sub, @args) = @_;
    my $sig = _process_arglist(\@args);
    _cgop('subcall', $sig, $sub, @args);
}

sub methodcall {
    my ($obj, $name, @args) = @_;
    my $sig = _process_arglist(\@args);
    let($obj, sub {
        _cgop('methodcall', $name, $sig, fetch($_[0]), $_[0], @args)});
}

1;
