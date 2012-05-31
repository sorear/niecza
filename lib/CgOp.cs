// This class is a stopgap measure to reduce friction during the switchover.
// Eventually this will be removed and Op will generate ClrOp trees directly.

using System;
using System.Text;
using System.Collections.Generic;

namespace Niecza.Compiler {
    class CgOp {
        string   kind;
        object[] kids;

        CgOp(string kind_, object[] kids_) { kind = kind_; kids = kids_; }

        public static CgOp N(string kind, params object[] kids) {
            return new CgOp(kind, kids);
        }
        public static CgOp ann(params object[] kids)
            { return N("ann",kids); }
        public static CgOp arith(params object[] kids)
            { return N("arith",kids); }
        public static CgOp assign(params object[] kids)
            { return N("assign",kids); }
        public static CgOp bget(params object[] kids)
            { return N("bget",kids); }
        public static CgOp @bool(params object[] kids)
            { return N("bool",kids); }
        public static CgOp box(params object[] kids)
            { return N("box",kids); }
        public static CgOp boxlist(params object[] kids)
            { return N("boxlist",kids); }
        public static CgOp bset(params object[] kids)
            { return N("bset",kids); }
        public static CgOp callframe(params object[] kids)
            { return N("callframe",kids); }
        public static CgOp callnext(params object[] kids)
            { return N("callnext",kids); }
        public static CgOp call_uncloned_sub(params object[] kids)
            { return N("call_uncloned_sub",kids); }
        public static CgOp cast(params object[] kids)
            { return N("cast",kids); }
        public static CgOp cgoto(params object[] kids)
            { return N("cgoto",kids); }
        public static CgOp @char(params object[] kids)
            { return N("char",kids); }
        public static CgOp class_ref(params object[] kids)
            { return N("class_ref",kids); }
        public static CgOp compare(params object[] kids)
            { return N("compare",kids); }
        public static CgOp @const(params object[] kids)
            { return N("const",kids); }
        public static CgOp context_get(params object[] kids)
            { return N("context_get",kids); }
        public static CgOp control(params object[] kids)
            { return N("control",kids); }
        public static CgOp corelex(params object[] kids)
            { return N("corelex",kids); }
        public static CgOp cotake(params object[] kids)
            { return N("cotake",kids); }
        public static CgOp cursor_ast(params object[] kids)
            { return N("cursor_ast",kids); }
        public static CgOp cursor_backing(params object[] kids)
            { return N("cursor_backing",kids); }
        public static CgOp cursor_butpos(params object[] kids)
            { return N("cursor_butpos",kids); }
        public static CgOp cursor_dows(params object[] kids)
            { return N("cursor_dows",kids); }
        public static CgOp cursor_fresh(params object[] kids)
            { return N("cursor_fresh",kids); }
        public static CgOp cursor_from(params object[] kids)
            { return N("cursor_from",kids); }
        public static CgOp cursor_item(params object[] kids)
            { return N("cursor_item",kids); }
        public static CgOp cursor_O(params object[] kids)
            { return N("cursor_O",kids); }
        public static CgOp cursor_pos(params object[] kids)
            { return N("cursor_pos",kids); }
        public static CgOp cursor_reduced(params object[] kids)
            { return N("cursor_reduced",kids); }
        public static CgOp cursor_start(params object[] kids)
            { return N("cursor_start",kids); }
        public static CgOp cursor_synthcap(params object[] kids)
            { return N("cursor_synthcap",kids); }
        public static CgOp cursor_synthetic(params object[] kids)
            { return N("cursor_synthetic",kids); }
        public static CgOp cursor_unmatch(params object[] kids)
            { return N("cursor_unmatch",kids); }
        public static CgOp cursor_unpackcaps(params object[] kids)
            { return N("cursor_unpackcaps",kids); }
        public static CgOp default_new(params object[] kids)
            { return N("default_new",kids); }
        public static CgOp die(params object[] kids)
            { return N("die",kids); }
        public static CgOp do_require(params object[] kids)
            { return N("do_require",kids); }
        public static CgOp ehspan(params object[] kids)
            { return N("ehspan",kids); }
        public static CgOp exactnum(params object[] kids)
            { return N("exactnum",kids); }
        public static CgOp exit(params object[] kids)
            { return N("exit",kids); }
        public static CgOp fcclist_new(params object[] kids)
            { return N("fcclist_new",kids); }
        public static CgOp fetch(params object[] kids)
            { return N("fetch",kids); }
        public static CgOp fladlist_new(params object[] kids)
            { return N("fladlist_new",kids); }
        public static CgOp foreign_class(params object[] kids)
            { return N("foreign_class",kids); }
        public static CgOp frame_caller(params object[] kids)
            { return N("frame_caller",kids); }
        public static CgOp frame_file(params object[] kids)
            { return N("frame_file",kids); }
        public static CgOp frame_hint(params object[] kids)
            { return N("frame_hint",kids); }
        public static CgOp frame_line(params object[] kids)
            { return N("frame_line",kids); }
        public static CgOp from_json(params object[] kids)
            { return N("from_json",kids); }
        public static CgOp from_jsync(params object[] kids)
            { return N("from_jsync",kids); }
        public static CgOp fvarlist_item(params object[] kids)
            { return N("fvarlist_item",kids); }
        public static CgOp fvarlist_length(params object[] kids)
            { return N("fvarlist_length",kids); }
        public static CgOp fvarlist_new(params object[] kids)
            { return N("fvarlist_new",kids); }
        public static CgOp getargv(params object[] kids)
            { return N("getargv",kids); }
        public static CgOp getfield(params object[] kids)
            { return N("getfield",kids); }
        public static CgOp get_first(params object[] kids)
            { return N("get_first",kids); }
        public static CgOp getindex(params object[] kids)
            { return N("getindex",kids); }
        public static CgOp getslot(params object[] kids)
            { return N("getslot",kids); }
        public static CgOp @goto(params object[] kids)
            { return N("goto",kids); }
        public static CgOp how(params object[] kids)
            { return N("how",kids); }
        public static CgOp instrole(params object[] kids)
            { return N("instrole",kids); }
        public static CgOp @int(params object[] kids)
            { return N("int",kids); }
        public static CgOp iter_copy_elems(params object[] kids)
            { return N("iter_copy_elems",kids); }
        public static CgOp iter_flatten(params object[] kids)
            { return N("iter_flatten",kids); }
        public static CgOp iter_hasarg(params object[] kids)
            { return N("iter_hasarg",kids); }
        public static CgOp iter_hasflat(params object[] kids)
            { return N("iter_hasflat",kids); }
        public static CgOp iter_to_list(params object[] kids)
            { return N("iter_to_list",kids); }
        public static CgOp label(params object[] kids)
            { return N("label",kids); }
        public static CgOp labelid(params object[] kids)
            { return N("labelid",kids); }
        public static CgOp letn(params object[] kids)
            { return N("letn",kids); }
        public static CgOp letscope(params object[] kids)
            { return N("letscope",kids); }
        public static CgOp letvar(params object[] kids)
            { return N("letvar",kids); }
        public static CgOp llhow_name(params object[] kids)
            { return N("llhow_name",kids); }
        public static CgOp ltm_push_alts(params object[] kids)
            { return N("ltm_push_alts",kids); }
        public static CgOp mrl_count(params object[] kids)
            { return N("mrl_count",kids); }
        public static CgOp mrl_index(params object[] kids)
            { return N("mrl_index",kids); }
        public static CgOp ncgoto(params object[] kids)
            { return N("ncgoto",kids); }
        public static CgOp newblankrwscalar(params object[] kids)
            { return N("newblankrwscalar",kids); }
        public static CgOp newboundvar(params object[] kids)
            { return N("newboundvar",kids); }
        public static CgOp newrwlistvar(params object[] kids)
            { return N("newrwlistvar",kids); }
        public static CgOp newrwscalar(params object[] kids)
            { return N("newrwscalar",kids); }
        public static CgOp newtypedscalar(params object[] kids)
            { return N("newtypedscalar",kids); }
        public static CgOp newvarrayvar(params object[] kids)
            { return N("newvarrayvar",kids); }
        public static CgOp newvhashvar(params object[] kids)
            { return N("newvhashvar",kids); }
        public static CgOp newvnewarrayvar(params object[] kids)
            { return N("newvnewarrayvar",kids); }
        public static CgOp newvnewhashvar(params object[] kids)
            { return N("newvnewhashvar",kids); }
        public static CgOp newvsubvar(params object[] kids)
            { return N("newvsubvar",kids); }
        public static CgOp note(params object[] kids)
            { return N("note",kids); }
        public static CgOp @null(params object[] kids)
            { return N("null",kids); }
        public static CgOp num_to_string(params object[] kids)
            { return N("num_to_string",kids); }
        public static CgOp obj_asbool(params object[] kids)
            { return N("obj_asbool",kids); }
        public static CgOp obj_asdef(params object[] kids)
            { return N("obj_asdef",kids); }
        public static CgOp obj_asnum(params object[] kids)
            { return N("obj_asnum",kids); }
        public static CgOp obj_asstr(params object[] kids)
            { return N("obj_asstr",kids); }
        public static CgOp obj_at_key(params object[] kids)
            { return N("obj_at_key",kids); }
        public static CgOp obj_at_pos(params object[] kids)
            { return N("obj_at_pos",kids); }
        public static CgOp obj_delete_key(params object[] kids)
            { return N("obj_delete_key",kids); }
        public static CgOp obj_does(params object[] kids)
            { return N("obj_does",kids); }
        public static CgOp obj_exists_key(params object[] kids)
            { return N("obj_exists_key",kids); }
        public static CgOp obj_getbool(params object[] kids)
            { return N("obj_getbool",kids); }
        public static CgOp obj_getdef(params object[] kids)
            { return N("obj_getdef",kids); }
        public static CgOp obj_getnum(params object[] kids)
            { return N("obj_getnum",kids); }
        public static CgOp obj_getstr(params object[] kids)
            { return N("obj_getstr",kids); }
        public static CgOp obj_isa(params object[] kids)
            { return N("obj_isa",kids); }
        public static CgOp obj_is_defined(params object[] kids)
            { return N("obj_is_defined",kids); }
        public static CgOp obj_llhow(params object[] kids)
            { return N("obj_llhow",kids); }
        public static CgOp obj_newblank(params object[] kids)
            { return N("obj_newblank",kids); }
        public static CgOp obj_typename(params object[] kids)
            { return N("obj_typename",kids); }
        public static CgOp obj_vasbool(params object[] kids)
            { return N("obj_vasbool",kids); }
        public static CgOp obj_vasdef(params object[] kids)
            { return N("obj_vasdef",kids); }
        public static CgOp obj_vasnum(params object[] kids)
            { return N("obj_vasnum",kids); }
        public static CgOp obj_vasstr(params object[] kids)
            { return N("obj_vasstr",kids); }
        public static CgOp obj_vat_key(params object[] kids)
            { return N("obj_vat_key",kids); }
        public static CgOp obj_vat_pos(params object[] kids)
            { return N("obj_vat_pos",kids); }
        public static CgOp obj_vdelete_key(params object[] kids)
            { return N("obj_vdelete_key",kids); }
        public static CgOp obj_vexists_key(params object[] kids)
            { return N("obj_vexists_key",kids); }
        public static CgOp obj_what(params object[] kids)
            { return N("obj_what",kids); }
        public static CgOp outerlex(params object[] kids)
            { return N("outerlex",kids); }
        public static CgOp path_any_exists(params object[] kids)
            { return N("path_any_exists",kids); }
        public static CgOp path_change_ext(params object[] kids)
            { return N("path_change_ext",kids); }
        public static CgOp path_combine(params object[] kids)
            { return N("path_combine",kids); }
        public static CgOp path_dir_exists(params object[] kids)
            { return N("path_dir_exists",kids); }
        public static CgOp path_file_exists(params object[] kids)
            { return N("path_file_exists",kids); }
        public static CgOp path_modified(params object[] kids)
            { return N("path_modified",kids); }
        public static CgOp path_realpath(params object[] kids)
            { return N("path_realpath",kids); }
        public static CgOp popcut(params object[] kids)
            { return N("popcut",kids); }
        public static CgOp print(params object[] kids)
            { return N("print",kids); }
        public static CgOp prog(params object[] kids)
            { return N("prog",kids); }
        public static CgOp promote_to_list(params object[] kids)
            { return N("promote_to_list",kids); }
        public static CgOp pushcut(params object[] kids)
            { return N("pushcut",kids); }
        public static CgOp rawcall(params object[] kids)
            { return N("rawcall",kids); }
        public static CgOp rawnew(params object[] kids)
            { return N("rawnew",kids); }
        public static CgOp rawnewarr(params object[] kids)
            { return N("rawnewarr",kids); }
        public static CgOp rawnewzarr(params object[] kids)
            { return N("rawnewzarr",kids); }
        public static CgOp rawscall(params object[] kids)
            { return N("rawscall",kids); }
        public static CgOp rawsget(params object[] kids)
            { return N("rawsget",kids); }
        public static CgOp rawsset(params object[] kids)
            { return N("rawsset",kids); }
        public static CgOp @return(params object[] kids)
            { return N("return",kids); }
        public static CgOp role_apply(params object[] kids)
            { return N("role_apply",kids); }
        public static CgOp rxbacktrack(params object[] kids)
            { return N("rxbacktrack",kids); }
        public static CgOp rxbprim(params object[] kids)
            { return N("rxbprim",kids); }
        public static CgOp rxcall(params object[] kids)
            { return N("rxcall",kids); }
        public static CgOp rxclosequant(params object[] kids)
            { return N("rxclosequant",kids); }
        public static CgOp rxcommitgroup(params object[] kids)
            { return N("rxcommitgroup",kids); }
        public static CgOp rxend(params object[] kids)
            { return N("rxend",kids); }
        public static CgOp rxfinalend(params object[] kids)
            { return N("rxfinalend",kids); }
        public static CgOp rxframe(params object[] kids)
            { return N("rxframe",kids); }
        public static CgOp rxgetpos(params object[] kids)
            { return N("rxgetpos",kids); }
        public static CgOp rxgetquant(params object[] kids)
            { return N("rxgetquant",kids); }
        public static CgOp rxincorpcut(params object[] kids)
            { return N("rxincorpcut",kids); }
        public static CgOp rxincorpshift(params object[] kids)
            { return N("rxincorpshift",kids); }
        public static CgOp rxincquant(params object[] kids)
            { return N("rxincquant",kids); }
        public static CgOp rxinit(params object[] kids)
            { return N("rxinit",kids); }
        public static CgOp rxopenquant(params object[] kids)
            { return N("rxopenquant",kids); }
        public static CgOp rxpushb(params object[] kids)
            { return N("rxpushb",kids); }
        public static CgOp rxpushcapture(params object[] kids)
            { return N("rxpushcapture",kids); }
        public static CgOp rxsetcapsfrom(params object[] kids)
            { return N("rxsetcapsfrom",kids); }
        public static CgOp rxsetclass(params object[] kids)
            { return N("rxsetclass",kids); }
        public static CgOp rxsetpos(params object[] kids)
            { return N("rxsetpos",kids); }
        public static CgOp rxsetquant(params object[] kids)
            { return N("rxsetquant",kids); }
        public static CgOp rxstripcaps(params object[] kids)
            { return N("rxstripcaps",kids); }
        public static CgOp say(params object[] kids)
            { return N("say",kids); }
        public static CgOp scopedlex(params object[] kids)
            { return N("scopedlex",kids); }
        public static CgOp setbox(params object[] kids)
            { return N("setbox",kids); }
        public static CgOp setfield(params object[] kids)
            { return N("setfield",kids); }
        public static CgOp setindex(params object[] kids)
            { return N("setindex",kids); }
        public static CgOp setslot(params object[] kids)
            { return N("setslot",kids); }
        public static CgOp set_status(params object[] kids)
            { return N("set_status",kids); }
        public static CgOp sig_slurp_capture(params object[] kids)
            { return N("sig_slurp_capture",kids); }
        public static CgOp sink(params object[] kids)
            { return N("sink",kids); }
        public static CgOp slurp(params object[] kids)
            { return N("slurp",kids); }
        public static CgOp span(params object[] kids)
            { return N("span",kids); }
        public static CgOp specificlex(params object[] kids)
            { return N("specificlex",kids); }
        public static CgOp spew(params object[] kids)
            { return N("spew",kids); }
        public static CgOp stab_privatemethod(params object[] kids)
            { return N("stab_privatemethod",kids); }
        public static CgOp stab_what(params object[] kids)
            { return N("stab_what",kids); }
        public static CgOp startgather(params object[] kids)
            { return N("startgather",kids); }
        public static CgOp start_iter(params object[] kids)
            { return N("start_iter",kids); }
        public static CgOp statement(params object[] kids)
            { return N("statement",kids); }
        public static CgOp status_get(params object[] kids)
            { return N("status_get",kids); }
        public static CgOp str(params object[] kids)
            { return N("str",kids); }
        public static CgOp strbuf_append(params object[] kids)
            { return N("strbuf_append",kids); }
        public static CgOp strbuf_new(params object[] kids)
            { return N("strbuf_new",kids); }
        public static CgOp strbuf_seal(params object[] kids)
            { return N("strbuf_seal",kids); }
        public static CgOp str_chr(params object[] kids)
            { return N("str_chr",kids); }
        public static CgOp strcmp(params object[] kids)
            { return N("strcmp",kids); }
        public static CgOp str_flip(params object[] kids)
            { return N("str_flip",kids); }
        public static CgOp str_length(params object[] kids)
            { return N("str_length",kids); }
        public static CgOp str_substring(params object[] kids)
            { return N("str_substring",kids); }
        public static CgOp str_tolower(params object[] kids)
            { return N("str_tolower",kids); }
        public static CgOp str_tonum(params object[] kids)
            { return N("str_tonum",kids); }
        public static CgOp str_toupper(params object[] kids)
            { return N("str_toupper",kids); }
        public static CgOp take(params object[] kids)
            { return N("take",kids); }
        public static CgOp ternary(params object[] kids)
            { return N("ternary",kids); }
        public static CgOp to_json(params object[] kids)
            { return N("to_json",kids); }
        public static CgOp to_jsync(params object[] kids)
            { return N("to_jsync",kids); }
        public static CgOp treader_getc(params object[] kids)
            { return N("treader_getc",kids); }
        public static CgOp treader_getline(params object[] kids)
            { return N("treader_getline",kids); }
        public static CgOp treader_open(params object[] kids)
            { return N("treader_open",kids); }
        public static CgOp treader_slurp(params object[] kids)
            { return N("treader_slurp",kids); }
        public static CgOp treader_stdin(params object[] kids)
            { return N("treader_stdin",kids); }
        public static CgOp unbox(params object[] kids)
            { return N("unbox",kids); }
        public static CgOp var_get_var(params object[] kids)
            { return N("var_get_var",kids); }
        public static CgOp varhash_clear(params object[] kids)
            { return N("varhash_clear",kids); }
        public static CgOp varhash_contains_key(params object[] kids)
            { return N("varhash_contains_key",kids); }
        public static CgOp varhash_delete_key(params object[] kids)
            { return N("varhash_delete_key",kids); }
        public static CgOp varhash_dup(params object[] kids)
            { return N("varhash_dup",kids); }
        public static CgOp varhash_getindex(params object[] kids)
            { return N("varhash_getindex",kids); }
        public static CgOp varhash_new(params object[] kids)
            { return N("varhash_new",kids); }
        public static CgOp varhash_setindex(params object[] kids)
            { return N("varhash_setindex",kids); }
        public static CgOp var_islist(params object[] kids)
            { return N("var_islist",kids); }
        public static CgOp var_new_tied(params object[] kids)
            { return N("var_new_tied",kids); }
        public static CgOp vvarlist_append(params object[] kids)
            { return N("vvarlist_append",kids); }
        public static CgOp vvarlist_clone(params object[] kids)
            { return N("vvarlist_clone",kids); }
        public static CgOp vvarlist_count(params object[] kids)
            { return N("vvarlist_count",kids); }
        public static CgOp vvarlist_from_fvarlist(params object[] kids)
            { return N("vvarlist_from_fvarlist",kids); }
        public static CgOp vvarlist_item(params object[] kids)
            { return N("vvarlist_item",kids); }
        public static CgOp vvarlist_new_empty(params object[] kids)
            { return N("vvarlist_new_empty",kids); }
        public static CgOp vvarlist_new_singleton(params object[] kids)
            { return N("vvarlist_new_singleton",kids); }
        public static CgOp vvarlist_pop(params object[] kids)
            { return N("vvarlist_pop",kids); }
        public static CgOp vvarlist_push(params object[] kids)
            { return N("vvarlist_push",kids); }
        public static CgOp vvarlist_shift(params object[] kids)
            { return N("vvarlist_shift",kids); }
        public static CgOp vvarlist_sort(params object[] kids)
            { return N("vvarlist_sort",kids); }
        public static CgOp vvarlist_to_fvarlist(params object[] kids)
            { return N("vvarlist_to_fvarlist",kids); }
        public static CgOp vvarlist_unshift(params object[] kids)
            { return N("vvarlist_unshift",kids); }
        public static CgOp vvarlist_unshiftn(params object[] kids)
            { return N("vvarlist_unshiftn",kids); }
        public static CgOp whileloop(params object[] kids)
            { return N("whileloop",kids); }
        public static CgOp widen(params object[] kids)
            { return N("widen",kids); }
        public static CgOp xspan(params object[] kids)
            { return N("xspan",kids); }
        public static CgOp times(params object[] kids)
            { return N("times",kids); }
        public static CgOp divop(params object[] kids)
            { return N("divop",kids); }
        public static CgOp obj_can(params object[] kids)
            { return N("obj_can",kids); }
        public static CgOp sqrt(params object[] kids)
            { return N("sqrt",kids); }
        public static CgOp push(params object[] kids)
            { return N("push",kids); }
        public static CgOp pop(params object[] kids)
            { return N("pop",kids); }
        public static CgOp unshift(params object[] kids)
            { return N("unshift",kids); }
        public static CgOp shift(params object[] kids)
            { return N("shift",kids); }
        public static CgOp ind_method_call(params object[] kids)
            { return N("ind_method_call",kids); }
        public static CgOp newarray(params object[] kids)
            { return N("newarray",kids); }
        public static CgOp newhash(params object[] kids)
            { return N("newhash",kids); }
        public static CgOp you_are_here(params object[] kids)
            { return N("you_are_here",kids); }
        public static CgOp frame_outer(params object[] kids)
            { return N("frame_outer",kids); }
        public static CgOp frame_sub(params object[] kids)
            { return N("frame_sub",kids); }
        public static CgOp makejunction(params object[] kids)
            { return N("makejunction",kids); }
        public static CgOp who(params object[] kids)
            { return N("who",kids); }
        public static CgOp sc_root(params object[] kids)
            { return N("sc_root",kids); }
        public static CgOp sc_indir(params object[] kids)
            { return N("sc_indir",kids); }
        public static CgOp temporize(params object[] kids)
            { return N("temporize",kids); }
        public static CgOp _addmethod(params object[] kids)
            { return N("_addmethod",kids); }
        public static CgOp _invalidate(params object[] kids)
            { return N("_invalidate",kids); }
        public static CgOp rxlprim(params object[] kids)
            { return N("rxlprim",kids); }
        public static CgOp @double(params object[] kids)
            { return N("double",kids); }

        public static CgOp newblanklist() { return newarray(); }
        public static CgOp newblankhash() { return newhash(); }
        public static CgOp string_var(string s) { return box("Str",str(s)); }
        public static CgOp noop() { return prog(); }
        public static CgOp rnull(CgOp p) { return prog(p, corelex("Nil")); }

        static int nextlet;
        public static CgOp Let(CgOp head, Func<CgOp,CgOp> body) {
            string v = "!L" + (nextlet++);
            return letn(v, head, body(letvar(v)));
        }

        // cc_expr

        static CgOp process_arglist(string kind, object a1, object a2,
                object[] raw) {
            var bits = new List<object>();
            var sig  = new StringBuilder();
            bits.Add(a1);
            bits.Add(null); // sig space
            if (a2 != null) bits.Add(a2);
            int ix = 0;
            while (ix < raw.Length) {
                if (raw[ix] is CgOp) {
                    sig.Append((char) 0);
                } else {
                    string k = (string)raw[ix++];
                    sig.Append((char) k.Length);
                    sig.Append(k);
                }
                bits.Add(raw[ix++]);
            }
            bits[1] = sig.ToString();
            return N(kind, bits.ToArray());
        }

        public static CgOp subcall(CgOp sub, params object[] args) {
            return process_arglist("subcall", sub, null, args);
        }

        public static CgOp methodcall(CgOp self, object name, params object[] args) {
            return process_arglist("methodcall", name, self, args);
        }


        void Output(StringBuilder to) {
            to.Append("[\"");
            to.Append(kind);
            to.Append('"');
            foreach (object k in kids) {
                to.Append(',');
                CgOp zs = k as CgOp;
                if (zs != null)
                    zs.Output(to);
                else {
                    string ks = k.ToString();
                    to.Append('"');
                    foreach (char ksc in ks) {
                        if (ksc >= ' ' && ksc <= '~' && ksc != '"' && ksc != '\\')
                            to.Append(ksc);
                        else
                            to.AppendFormat("\\u{0:X4}", (int)ksc);
                    }
                    to.Append('"');
                }
            }
            to.Append("]");
        }
        public override string ToString() {
            var sb = new StringBuilder();
            Output(sb);
            return sb.ToString();
        }
    }

    public class Test {
        public static void Run() {
            var c = CgOp.subcall(CgOp.corelex("&say"), CgOp.string_var("Hello, World"));
            Console.WriteLine(c);
        }
    }
}
