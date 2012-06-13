using System;
using System.Collections.Generic;

// Public methods with the same name as a STD rule are assumed to be handlers
// for that rule.

namespace Niecza.Compiler {
    class Actions {
        internal CompJob job;
        public Actions(CompJob job) { this.job = job; }

        // Little things to make writing wads of data-extraction code nicer...
        string asstr(Variable v) { return v.Fetch().mo.mro_raw_Str.Get(v); }
        bool istrue(Variable v) { return v.Fetch().mo.mro_raw_Bool.Get(v); }
        bool isdef(Variable v) { return v.Fetch().mo.mro_raw_defined.Get(v); }
        Variable atk(Variable v, string at) {
            return v.Fetch().mo.mro_at_key.Get(v, mkstr(at));
        }
        Variable atp(Variable v, int at) {
            return v.Fetch().mo.mro_at_pos.Get(v, mkint(at));
        }

        P6any mkint(int i) { return (P6any)Builtins.MakeInt(i); }
        P6any mkstr(string s) { return (P6any)Builtins.MakeStr(s); }

        P6any plus(Variable v1,Variable v2) { return (P6any)Builtins.plus(v1,v2); }
        P6any minus(Variable v1,Variable v2) { return (P6any)Builtins.minus(v1,v2); }
        P6any mul(Variable v1,Variable v2) { return (P6any)Builtins.mul(v1,v2); }
        P6any div(Variable v1,Variable v2) { return (P6any)Builtins.divide(v1,v2); }
        P6any pow(Variable v1,Variable v2) { return (P6any)Builtins.pow(v1,v2); }

        // SSTO TODO integrate with parser
        void sorry(Cursor m, string fmt, params object[] args) {
            throw new NotImplementedException();
        }
        STable compile_get_pkg(bool auto, params string[] path) {
            throw new NotImplementedException();
        }
        void addlex(Cursor pos, SubInfo to, string name, LexInfo li) {
            throw new NotImplementedException();
        }
        void trymop(Cursor m, Action a) {
            try {
                a();
            } catch (Exception e) {
                sorry(m, e.Message);
            }
        }

        T ast<T>(Variable v) {
            return (T)Kernel.UnboxAny<object>(((Cursor)v.Fetch()).ast);
        }
        void make(Cursor m, object val) {
            m.ast = Kernel.BoxRaw(val, Kernel.AnyMO);
        }

        Variable[] flist(Variable list) {
            // we force the list to iterate here
            VarDeque iter = Builtins.start_iter(list.Fetch().mo.mro_list.Get(list));
            VarDeque into = new VarDeque();
            while (Kernel.IterHasFlat(iter, true))
                into.Push(iter.Shift());
            return into.CopyAsArray();
        }

        B[] map<A,B>(Func<A,B> fn, A[] args) {
            B[] ret = new B[args.Length];
            for (int i = 0; i < args.Length; i++)
                ret[i] = fn(args[i]);
            return ret;
        }

        // SSTO TODO: how are we handling categoricals?  FALLBACK

        // TODO Merge with the corresponding setting/runtime code?
        // using p6numbers because we need bignum support here
        // TODO Unicode
        P6any from_base(Cursor m, Variable str, int based_) {
            var acc = mkint(0);
            var based = mkint(based_);
            int punto = -1;

            foreach (char ch_ in asstr(str)) {
                var ch = char.ToLower(ch_);
                if (ch == '_') continue;
                if (ch == '.') { punto = 0; continue; }
                if (punto >= 0) punto++;
                var digit = ch >= 'a' ? ((int)ch) - 87 : ((int)ch) - 48;
                if (digit >= based_)
                    sorry(m, "Digit <{0}> too large for radix {1}", ch, based_);
                acc = plus(mkint(digit), mul(based, acc));
            }
            return punto < 0 ? acc : div(acc, pow(based, mkint(punto)));
        }

        public void decint(Cursor m) { make(m,from_base(m, m, 10)); }
        public void hexint(Cursor m) { make(m,from_base(m, m, 16)); }
        public void octint(Cursor m) { make(m,from_base(m, m, 8)); }
        public void binint(Cursor m) { make(m,from_base(m, m, 2)); }
        public void integer(Cursor m) {
            Variable v = null;
            if (isdef(v = atk(m, "decint")) || isdef(v = atk(m, "octint")) ||
                    isdef(v = atk(m, "hexint")) || isdef(v = atk(m, "binint")))
                make(m,ast<P6any>(v));
        }

        public void decints(Cursor m) { make(m, map(v => ast<P6any>(v), flist(atk(m,"decint")))); }
        public void hexints(Cursor m) { make(m, map(v => ast<P6any>(v), flist(atk(m,"hexint")))); }
        public void octints(Cursor m) { make(m, map(v => ast<P6any>(v), flist(atk(m,"octint")))); }
        public void binints(Cursor m) { make(m, map(v => ast<P6any>(v), flist(atk(m,"binint")))); }

        public void dec_number(Cursor m) {
            if (istrue(atk(m,"escale"))) {
                // SSTO use the real number parser here
                make(m, (P6any)Builtins.MakeFloat(
                            Utils.S2N(asstr(m).Replace("_",""))));
            } else make(m,from_base(m,m,10));
        }

        public void radint(Cursor m) {
            Variable var = null;
            if (isdef(var = atk(m,"rad_number")) || isdef(var = atk(m,"integer")))
                make(m,ast<P6any>(var));
        }

        public void rad_number(Cursor m) { // returns Op *or* P6any ...
            int radix = (int)Utils.S2N(asstr(atk(m,"radix")));

            var f = atk(m,"circumfix");
            if (istrue(f)) {
                // STD guarantees this can never happen from within radint;
                // only number can see this
                make(m, Op.Helpers.mkcall(m, "&unbase", new Op.Num(m,
                    new object[] { 10, radix.ToString() }), ast<Op.Op>(f)));
                return;
            }
            var value = istrue(f=atk(m,"int")) ? from_base(m,f,radix):mkint(0);

            if (istrue(f = atk(m,"frac"))) {
                var shift = asstr(f).Replace("_","").Length;
                value = plus(value, div(from_base(m, f, radix),
                            pow(mkint(radix), mkint(shift))));
            }
            if (istrue(f = atk(m,"base"))) {
                value = mul(value, pow(ast<P6any>(f), istrue(atk(m,"exp")) ?
                    ast<P6any>(atk(m,"exp")) : mkint(0)));
                value = (P6any)Builtins.coerce_to_num(value);
                // exponential notation is always imprecise here
            }
            make(m,value);
        }

        public void number(Cursor m) {
            var child = atk(m,"integer");
            if (!isdef(child)) child = atk(m,"dec_number");
            if (!isdef(child)) child = atk(m,"rad_number");
            object val;
            if (!isdef(child)) {
                val = asstr(m) == "NaN" ? double.NaN : double.PositiveInfinity;
            } else {
                val = ast<object>(child);
            }
            make(m, val is Op.Op ? val : new Op.Const(m,(P6any)val));
        }

        public void charname(Cursor m) {
            var radint = atk(m,"radint");
            string res;
            if (istrue(radint)) {
                var ast = ast<P6any>(radint);
                int val = (int)Builtins.ToNum(ast);
                if (!ast.Isa(Kernel.IntMO) || val < 0 || val > 0x10FFFF) {
                    res = " ";
                    sorry(m, "Numeric character identifiers must be integers between 0 and 0x10FFFF");
                } else {
                    res = new string((char) val, 1);
                }
            } else {
                res = " ";
                trymop(m, () => { res = Niecza.UCD.DataSet.GetCodepoint(asstr(m)); });
            }
            make(m, mkstr(res));
        }

        public void charnames(Cursor m) {
            var sb = new System.Text.StringBuilder();
            foreach (var charname in flist(atk(m,"charname")))
                sb.Append(ast<string>(charname));
            make(m,sb.ToString());
        }

        public void charspec(Cursor m) {
            if (istrue(atk(m,"charnames"))) {
                make(m,ast<string>(atk(m,"charnames")));
            } else {
                var str = asstr(m);
                if (CC.Digit.Accepts(str[0])) {
                    make(m, asstr(Builtins.chr(from_base(m,m,10))));
                } else {
                    make(m, new string((char) (str[0] & 31), 1));
                }
            }
        }

        public void value__number(Cursor m) { make(m,ast<Op.Op>(atk(m,"number"))); }
        public void value__quote(Cursor m) { make(m,ast<Op.Op>(atk(m,"quote"))); }

        public void label(Cursor m) {
            trymop(m, () => {
                addlex(m, job.curlex, asstr(atk(m,"identifier")), new LILabel());
            });
            make(m, asstr(atk(m,"identifier")));
        }

        public void morename(Cursor m) { // Either String Op.Op
            Variable v = null;
            if (istrue(v = atk(m,"identifier"))) make(m,asstr(v));
            else if (istrue(v = atk(m,"EXPR"))) make(m,ast<Op.Op>(v));
            else make(m,(string)null);
        }

        struct TypeConstraint {
            public P6any value;
            public Op.Op where;
            public int tmode; // if type != null, a SIG_ flag
            public STable type;
        }

        // P6any (value constraint) or Op.Op (where) or 
        public void type_constraint(Cursor m) {
            var tc = default(TypeConstraint);
            if (istrue(atk(m,"value"))) {
                var val = ast<Op.Op>(atk(m,"value")).const_value(job.curlex);
                if (val == null) {
                    sorry(m, "Value constraint is not constant");
                    val = Kernel.AnyP;
                }
                tc.value = val.Fetch();
            } else if (istrue(atk(m,"typename"))) {
                tc = ast<TypeConstraint>(atk(m,"typename"));
            } else {
                tc.where = ast<Op.Op>(atk(m,"EXPR"));
            }
            make(m,tc);
        }

        public void typename(Cursor m) {
            if (istrue(atk(m, "whence")))
                sorry(m, "WHENCE blocks are not allowed on declarative type names");

            var ret = default(TypeConstraint);
            if (istrue(atk(m, "ident"))) {
                sorry(m, "::?CLASS syntax NYI");
                ret.tmode = 0; ret.type = Kernel.AnyMO;
                make(m, ret);
                return;
            }

            var lname = atk(m, "longname");
            ret.type = process_name(lname, REFER).pkg;

            if (ret.type == null) {
                sorry(m, "A type must be provided");
                ret.type = Kernel.AnyMO;
            }

            if (istrue(atk(m, "typename")))
                sorry(m, "Coercive declarations NYI");

            foreach (var cp in flist(atk(lname, "colonpair"))) {
                string scp = asstr(cp);
                int mask = 0;
                if (scp == ":_") mask = Parameter.ANY_DEF;
                if (scp == ":T") mask = Parameter.UNDEF_ONLY;
                if (scp == ":U") mask = Parameter.UNDEF_ONLY;
                if (scp == ":D") mask = Parameter.DEF_ONLY;
                if (mask == 0) continue;
                if (ret.tmode != 0)
                    sorry(m, "You may only specify one of :_ :D :U :T");
                ret.tmode |= mask;
            }

            make(m, ret);
        }

        struct Name {
            public bool dc;
            public object[] names; // Either String Op
        }
        public void name(Cursor m) {
            Name r = default(Name);
            r.names = map(v => ast<object>(v), flist(atk(m,"morename")));
            if (istrue(atk(m,"identifier"))) {
                r.names = Utils.PrependArr(r.names, asstr(atk(m,"identifier")));
                r.dc = true;
            }
            make(m,r);
        }

        bool is_pseudo_pkg(string name) {
            foreach (var s in new [] { "MY", "OUR", "CORE", "DYNAMIC", "GLOBAL",
                "CALLER", "OUTER", "UNIT", "SETTING", "PROCESS", "COMPILING",
                "PARENT", "CLR" })
                if (name == s) return true;
            return false;
        }

        struct VarInfo {
            public STable pkg;
            public string name;
            public Op.Op ind;
        };

        // This is to be the one place where names are processed

        // MODES
        //
        // DECL:  pkg + name used. name == null means no name given.
        // DEFER: pkg + name *or* ind

        // OPTIONS
        // clean:     remove :sym<xyz>
        const int REFER = 0;
        const int DECL  = 1;
        const int DEFER = 2;

        const int CLEAN = 4;
        VarInfo process_name(Variable nm, int mode = 0) {
            var ret = new VarInfo();
            if (!isdef(nm)) {
                return ret;
            }
            Cursor c = (Cursor) nm.Fetch();

            var nast = ast<Name>(atk(nm, "name"));
            var names = nast.names;
            int nlen = names.Length;
            string ext = "";
            bool trail = nlen != 0 && names[nlen - 1] == null;
            if (trail) nlen--;

            if ((mode & CLEAN) == 0) {
                var eb = new System.Text.StringBuilder();
                foreach (var cp in flist(atk(c, "colonpair")))
                    eb.Append(get_cp_ext(cp));
                ext = eb.ToString();
            }

            if ((mode & DEFER) == 0) {
                for (int i = 0; i < nlen; i++) {
                    if (names[i] is Op.Op)
                        names[i] = asstr(eval_ast(c, (Op.Op)names[i]));
                }
            }

            if ((mode & DECL) != 0) {
                // class :: is ... { } is a placeholder for a lack of name
                if (nlen == 0)
                    return ret;
                if (trail)
                    sorry(c, "Illegal explicit declaration of a symbol table");
                if ((mode & DEFER) != 0) throw new NotSupportedException();

                ret.name = (string)names[nlen - 1] + ext;
                if (nlen != 1) {
                    // The remainder is assumed to name an existing or new pkg
                    trymop(c, () => {
                        var sa = new string[nlen];
                        Array.Copy(names,0, sa,0, nlen);
                        ret.pkg = compile_get_pkg(true, sa);
                    });
                }
                return ret;
            }
            else if ((mode & DEFER) != 0) {
                // The stuff returned here is processed by the variable rule,
                // and also by method call generation
                if (trail) goto dyn;
                var tail = new string[nlen-1];
                for (int i = 0; i < nlen-1; i++) {
                    tail[i] = names[i] as string;
                    if (tail[i] == null) goto dyn;
                }
                ret.name = names[nlen-1] as string;
                if (ret.name == null) goto dyn;
                ret.name = ret.name + ext;
                try {
                    ret.pkg = compile_get_pkg(true, tail);
                } catch (Exception) { }
                if (ret.pkg == null) goto dyn;
                return ret;
dyn:
                var bits = new List<Op.Op>();
                for (int i = 0; i < nlen; i++) {
                    bits.Add(names[i] is Op.Op ? (Op.Op)names[i] :
                        new Op.StringLiteral(c,(string)names[i]));
                    if (i != nlen - 1) bits.Add(new Op.StringLiteral(c,"::"));
                }
                if (trail) bits.Add(new Op.StringLiteral(c,"::"));
                bits.Add(new Op.StringLiteral(c,ext));
                ret.name = null;
                ret.ind = Op.Helpers.mkstringycat(c, bits.ToArray());
                return ret;
            }
            else {
                if (trail)
                    sorry(c,"Class required, but symbol table name used instead");
                if (nlen == 0) return ret;
                var tail = new string[nlen];
                Array.Copy(names,0, tail,0, nlen);
                tail[nlen-1] += ext;
                trymop(c, () => {
                    ret.pkg = compile_get_pkg(false, tail);
                });
                return ret;
            }
        }

        public void subshortname(Cursor m) {
            if (istrue(atk(m,"colonpair"))) {
                var n = new System.Text.StringBuilder();
                n.Append(asstr(atk(m,"category")));
                foreach (var cp in flist(atk(m,"colonpair")))
                    n.Append(get_cp_ext(cp));
                var ret = default(VarInfo);
                ret.name = n.ToString();
                make(m,ret);
            } else {
                make(m,ast<VarInfo>(atk(m,"desigilname")));
            }
        }

        public void sublongname(Cursor m) {
            if (istrue(atk(m,"sigterm"))) {
                sorry(m, "Sigterm sublongnames NYI");
                make(m, default(VarInfo));
            } else {
                make(m, ast<VarInfo>(atk(m,"subshortname")));
            }
        }

        public void desigilname(Cursor m) {
            if (istrue(atk(m,"variable"))) {
                var r = default(VarInfo);
                r.ind = do_variable_reference(m, ast<VarInfo>(atk(m,"variable")));
                make(m,r);
                check_variable(atk(m,"variable"));
            } else {
                make(m,process_name(atk(m,"longname"),DEFER));
            }
        }

        public void quote__22_20(Cursor m) { make(m,ast<Op.Op>(atk(m,"nibble"))); }
        public void quote__27_27(Cursor m) { make(m,ast<Op.Op>(atk(m,"nibble"))); }
        public void quote__qq(Cursor m) { make(m,ast<Op.Op>(atk(m,"quibble")));}
        public void quote__q(Cursor m) { make(m,ast<Op.Op>(atk(m,"quibble")));}
        public void quote__Q(Cursor m) { make(m,ast<Op.Op>(atk(m,"quibble")));}
        public void quote__s(Cursor m) { make(m,ast<Op.Op>(atk(m,"pat")));}

        public Op.Op rxembed(Cursor m, Op.Op ops) {
            return inliney_call(m, thunk_sub(ops, new[] { "$¢" }),
                new Op.MakeCursor(m));
        }

        // forward...
        Op.Op do_variable_reference(Cursor m, VarInfo ast) { throw new NotImplementedException(); }
        void check_variable(Variable v) { throw new NotImplementedException(); }
        string get_cp_ext(Variable c) { throw new NotImplementedException(); }
        Variable eval_ast(Cursor c, Op.Op o) { throw new NotImplementedException(); }

        Op.Op block_expr(Cursor m, SubInfo blk) {
            var name = job.gensym();
            addlex(m, job.curlex, name, new LISub(blk));
            return Op.Helpers.mklex(m, name);
        }

        Op.Op inliney_call(Cursor m, SubInfo block, params Op.Op[] parms) {
            var sym = job.gensym();
            addlex(m, job.curlex, sym, new LISub(block));
            return CompUtils.BetaCall(m, sym, parms);
        }

        internal SubInfo thunk_sub(Op.Op code, string[] parms,
                string name = null, STable cls = null, LAD ltm = null,
                bool nosimpl = false) {

            var n = create_sub(name ?? "ANON", job.curlex, Kernel.BlockMO,
                job.curlex.cur_pkg, job.curlex.in_class, false, null);

            n.special |= SubInfo.TRANSPARENT;
            n.ltm = ltm;
            Parameter[] ps = new Parameter[parms.Length];
            for (int i = 0; i < ps.Length; i++) {
                addlex(null, n, parms[i], new LISimple(LISimple.NOINIT, null));
                ps[i] = Parameter.TPos(parms[i], n.dylex[parms[i]].SigIndex());
            }
            n.sig = new Signature(ps);
            finish(n, code, nosimpl);
            return n;
        }

        // does NOT handle $?PERL construction
        internal SubInfo create_sub(string name, SubInfo outer, STable cls,
                STable pkg, STable icl, bool once, Frame outer_frame) {

            var n = new SubInfo(job.unit, name, outer, cls, pkg, once, outer_frame);
            n.in_class = icl;
            if (n.outer != null && n.outer.unit == job.unit)
                n.outer.children.Add(n);
            job.unit.our_subs.Add(n);
            return n;
        }

        internal void finish(SubInfo n, Op.Op code, bool done) {
            if (!done) code = code.simplify(n);
            if (code.onlystub()) n.SetExtend("onlystub", true);
            // serialize cgops
            n.nam_str = code.cgop(n).ToString(out n.nam_refs);
            n.code = RuntimeUnit.JitCompileSub;
            if (n.protopad != null)
                n.protopad.code = n.code;
        }
    }
}
