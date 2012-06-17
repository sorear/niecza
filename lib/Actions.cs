using System;
using System.Collections.Generic;
using System.Text;

// Public methods with the same name as a STD rule are assumed to be handlers
// for that rule.

namespace Niecza.Compiler {
    class Actions {
        internal CompJob job;
        RxOp.RxOp rnoop = new RxOp.Sequence(new RxOp.RxOp[0]);
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
        P6any mkstr(string s) { return s==null ? Kernel.StrMO.typeObj : (P6any)Builtins.MakeStr(s); }

        P6any plus(Variable v1,Variable v2) { return (P6any)Builtins.plus(v1,v2); }
        P6any minus(Variable v1,Variable v2) { return (P6any)Builtins.minus(v1,v2); }
        P6any mul(Variable v1,Variable v2) { return (P6any)Builtins.mul(v1,v2); }
        P6any div(Variable v1,Variable v2) { return (P6any)Builtins.divide(v1,v2); }
        P6any pow(Variable v1,Variable v2) { return (P6any)Builtins.pow(v1,v2); }
        void assign(Variable a, Variable b) { Kernel.Assign(a,b); }

        // SSTO TODO integrate with parser
        internal void sorry(Cursor m, string fmt, params object[] args) {
            throw new NotImplementedException();
        }
        internal void worry(Cursor m, string fmt, params object[] args) {
            throw new NotImplementedException();
        }
        STable compile_get_pkg(bool auto, params string[] path) {
            throw new NotImplementedException();
        }
        internal void addlex(Cursor pos, SubInfo to, string name, LexInfo li) {
            throw new NotImplementedException();
        }
        internal LexInfo lookup_lex(SubInfo from, string name, Cursor mark = null) {
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
        void from(Cursor m, string field) {
            m.ast = ((Cursor)atk(m,field).Fetch()).ast;
        }

        Variable[] flist(Variable list) {
            // we force the list to iterate here
            VarDeque iter = Builtins.start_iter(list.Fetch().mo.mro_list.Get(list));
            VarDeque into = new VarDeque();
            while (Kernel.IterHasFlat(iter, true))
                into.Push(iter.Shift());
            return into.CopyAsArray();
        }

        T[] flist_ast<T>(Variable list) {
            // we force the list to iterate here
            VarDeque iter = Builtins.start_iter(list.Fetch().mo.mro_list.Get(list));
            List<T> into = new List<T>();
            while (Kernel.IterHasFlat(iter, true))
                into.Add(ast<T>(iter.Shift()));
            return into.ToArray();
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

        public void decints(Cursor m) { make(m, flist_ast<P6any>(atk(m,"decint"))); }
        public void hexints(Cursor m) { make(m, flist_ast<P6any>(atk(m,"hexint"))); }
        public void octints(Cursor m) { make(m, flist_ast<P6any>(atk(m,"octint"))); }
        public void binints(Cursor m) { make(m, flist_ast<P6any>(atk(m,"binint"))); }

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
            foreach (var charname in flist_ast<string>(atk(m,"charname")))
                sb.Append(charname);
            make(m,sb.ToString());
        }

        public void charspec(Cursor m) {
            if (istrue(atk(m,"charnames"))) {
                from(m,"charnames");
            } else {
                var str = asstr(m);
                if (CC.Digit.Accepts(str[0])) {
                    make(m, asstr(Builtins.chr(from_base(m,m,10))));
                } else {
                    make(m, new string((char) (str[0] & 31), 1));
                }
            }
        }

        public void value__number(Cursor m) { from(m,"number"); }
        public void value__quote(Cursor m) { from(m,"quote"); }

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
            r.names = flist_ast<object>(atk(m,"morename"));
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

        class VarInfo {
            public STable pkg;
            public string name;
            // ind: op which holds the *name* of a variable
            // term: for pseudo-variable use
            public Op.Op ind, term;
            public string capid;
            public char sigil, twigil;
            public bool @checked;
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
                make(m,new VarInfo { name = n.ToString() });
            } else {
                from(m,"desigilname");
            }
        }

        public void sublongname(Cursor m) {
            if (istrue(atk(m,"sigterm"))) {
                sorry(m, "Sigterm sublongnames NYI");
                make(m, new VarInfo {});
            } else {
                from(m,"subshortname");
            }
        }

        public void desigilname(Cursor m) {
            if (istrue(atk(m,"variable"))) {
                make(m, new VarInfo { term = do_variable_reference(m,
                            ast<VarInfo>(atk(m,"variable"))) });
                check_variable(atk(m,"variable"));
            } else {
                make(m,process_name(atk(m,"longname"),DEFER));
            }
        }

        public void quote__22_20(Cursor m) { from(m,"nibble"); }
        public void quote__27_27(Cursor m) { from(m,"nibble"); }
        public void quote__qq(Cursor m) { from(m,"quibble");}
        public void quote__q(Cursor m) { from(m,"quibble");}
        public void quote__Q(Cursor m) { from(m,"quibble");}
        public void quote__s(Cursor m) { from(m,"pat");}

        public void quote__2f_2f(Cursor m) { make(m,op_for_regex(m,ast<RxOp.RxOp>(atk(m,"nibble")))); }
        public void quote__rx(Cursor m) {
            extract_rx_adverbs(false, false, atk(m,"quibble"));
            make(m,op_for_regex(m,ast<RxOp.RxOp>(atk(m,"quibble"))));
        }

        public void quote__m(Cursor m) {
            make(m,new Op.CallMethod(m, "match", Op.Helpers.mklex(m,"$/"),
                false, Utils.PrependArr(
                    extract_rx_adverbs(true,false,atk(m,"quibble")),
                    op_for_regex(m, ast<RxOp.RxOp>(atk(m,"quibble"))))));
        }

        public void quote__ms(Cursor m) { quote__m(m); }

        internal Op.Op rxembed(Cursor m, Op.Op ops) {
            return inliney_call(m, thunk_sub(ops, new[] { "$¢" }),
                new Op.MakeCursor(m));
        }

        internal Op.Op op_for_regex(Cursor m, RxOp.RxOp rxop) {
            var lift = new List<Op.Op>();
            rxop.oplift(lift);
            var ltm = RxOp.RxOp.optimize_lad(rxop.lad());
            rxop = rxop.rxsimp(false);
            var ops = new Op.RegexBody(m, "", rxop, lift.ToArray());
            var sub = thunk_sub(ops, new [] { "self" }, null, Kernel.RegexMO,
                    Niecza.CLRBackend.DowncallReceiver.BuildLad(ltm));
            addlex(m, sub, "$/", new LISimple(0, null));
            return block_expr(m, sub);
        }

        internal RxOp.RxOp encapsulate_regex(Cursor m, RxOp.RxOp rxop,
                    bool passcut = false) {
            var lift = new List<Op.Op>();
            rxop.oplift(lift);
            var lad = rxop.lad();
            var nrxop = rxop.rxsimp(false);
            var subbody = new Op.RegexBody(m, "", nrxop, lift.ToArray(),
                    passcut);
            var sub = thunk_sub(subbody, new [] { "self" }, null,
                Kernel.RegexMO);
            var subop = new Op.CallSub(m,block_expr(m,sub),true,
                new [] { new Op.MakeCursor(m) });
            return new RxOp.Subrule(subop, lad);
        }

        public void regex_block(Cursor m) {
            if (istrue(atk(m,"quotepair")))
                sorry(m, "Regex adverbs NYI");
            if (istrue(atk(m,"onlystar")))
                make(m,new RxOp.ProtoRedis());
            else
                from(m,"nibble");
        }

        public void regex_def_1(Cursor m) {
            throw new NotImplementedException(); // SSTO install_sub
        }

        public void regex_def_2(Cursor m) {
            if (flist(atk(m,"signature")).Length > 1)
                sorry(m, "Too many signatures on regex");

            foreach (var tx in flist(atk(m, "trait"))) {
                var t = ast<Prod<string,object>>(tx);
                if (t.v1 == "unary" || t.v1 == "binary" || t.v1 == "defequiv" ||
                        t.v1 == "of") {
                    // Ignored for now
                } else if (t.v1 == "endsym") {
                    job.rxinfo.endsym = (string)t.v2;
                } else {
                    sorry(m, "Unhandled regex trait {0}", t.v1);
                }
            }

            string cn = (string)job.curlex.GetExtend0("cleanname");
            if ((string)job.curlex.GetExtend0("multi") == "proto") {
                if (cn != null) job.proto_endsym[cn] = job.rxinfo.endsym;
            } else {
                if (cn != null && job.rxinfo.endsym == null)
                    job.rxinfo.endsym = job.proto_endsym.GetDefault(cn, null);
            }

            job.rxinfo.dba = (string)job.curlex.GetExtend0("name") ?? "anonymous regex";
        }

        public void regex_def(Cursor m) {
            var ast = ast<RxOp.RxOp>(atk(m,"regex_block"));

            var lift = new List<Op.Op>();
            ast.oplift(lift);
            var ltm = RxOp.RxOp.optimize_lad(ast.lad());

            job.curlex.ltm = Niecza.CLRBackend.DowncallReceiver.BuildLad(ltm);
            ast = ast.rxsimp(false);
            if (ast is RxOp.ProtoRedis) {
                finish_dispatcher(job.curlex, "regex");
            } else {
                finish(job.curlex, new Op.RegexBody(m, (string)(job.curlex.GetExtend0("name") ?? ""), ast, lift.ToArray()));
            }
            // SSTO: reexamine match pruning
            make(m, Op.Helpers.mklex(m, job.curlex.outervar));
        }

        public void regex_declarator__regex(Cursor m) {
            from(m,"regex_def");
        }
        public void regex_declarator__rule(Cursor m) { regex_declarator__regex(m); }
        public void regex_declarator__token(Cursor m) { regex_declarator__regex(m); }

        public void atom(Cursor m) {
            if (istrue(atk(m,"metachar"))) {
                from(m,"metachar");
            } else {
                make(m,new RxOp.String(asstr(m), job.rxinfo.i));
            }
        }

        struct QuantInfo {
            public Op.Op closure;
            public RxOp.RxOp sep, tilde, tilde_inner;
            public string mod;
            public int min, max;
            public bool minimal, nonlisty, opsep, space, general;
        }

        public void quantified_atom(Cursor m) {
            var atom = ast<RxOp.RxOp>(atk(m,"atom"));
            var q    = istrue(atk(m,"quantifier")) ? ast<QuantInfo>(atk(m,"quantifier")) : default(QuantInfo);

            if (job.rxinfo.r) {
                // quantifier without explicit :? / :! gets :
                if (q.mod == null) q.mod = "";
            }

            if (q.max > 0 || q.general) {
                var z = new List<RxOp.RxOp> { atom };
                if (istrue(atk(m,"separator"))) {
                    if (q.sep != null)
                        sorry(m, "Cannot use two separators in one quantifier_atom");
                    var q2 = ast<QuantInfo>(atk(m,"separator"));
                    q.opsep = q2.opsep;
                    q.sep   = q2.sep;
                    q.space = q2.space;
                }
                if (q.sep != null) z.Add(q.sep);
                // parsing quirk. x #`(1) ** #`(21) y, the 1* position is
                // counted as $<normspace> but the 2* is parsed by the
                // quantifier
                if ((q.general || q.sep != null) && job.rxinfo.s &&
                        (q.space || istrue(atk(m,"normspace")))) {
                    if (q.sep != null) {
                        z[1] = new RxOp.Sequence(new [] {
                            new RxOp.Sigspace(), z[1], new RxOp.Sigspace() });
                    } else {
                        z.Add(new RxOp.Sigspace());
                    }
                }
                atom = new RxOp.Quantifier(z.ToArray(), q.min, q.max,
                    q.closure, q.mod == "?", q.nonlisty, q.opsep);
            }

            if (q.mod == "")
                atom = new RxOp.Cut(atom);

            if (q.tilde != null) {
                var closer = q.tilde;
                if (closer is RxOp.Cut)
                    closer = closer.zyg[0];
                if (!(closer is RxOp.String)) {
                    sorry(m,"Non-literal closers for ~ NYI");
                    closer = new RxOp.String("",false);
                }
                atom = new RxOp.Sequence(new [] { atom,
                    new RxOp.Tilde(q.tilde_inner, ((RxOp.String)closer).text,
                        job.rxinfo.dba) });
            }

            make(m,atom);
        }

        public void quantifier__2a(Cursor m) {
            make(m,new QuantInfo { min = 0, max = int.MaxValue, mod = ast<string>(atk(m,"quantmod")) });
        }
        public void quantifier__2b(Cursor m) {
            make(m,new QuantInfo { min = 1, max = int.MaxValue, mod = ast<string>(atk(m,"quantmod")) });
        }
        public void quantifier__3f(Cursor m) {
            make(m,new QuantInfo { min = 0, max = 1, mod = ast<string>(atk(m,"quantmod")) });
        }
        public void quantifier__3a(Cursor m) {
            make(m,new QuantInfo { mod = "" });
        }
        public void quantifier__7e(Cursor m) {
            var tl = flist_ast<RxOp.RxOp>(atk(m,"quantified_atom"));
            make(m, new QuantInfo { tilde = tl[0], tilde_inner = tl[1] });
        }
        public void quantifier__2a2a(Cursor m) {
            var r = new QuantInfo();
            Variable v;
            if (istrue(v = atk(m,"embeddedblock")))
                r = new QuantInfo { min=0, max=int.MaxValue, closure = rxblock(m, v) };
            else if (istrue(v = atk(m, "quantified_atom")))
                r = new QuantInfo { min=1, max=int.MaxValue, sep = ast<RxOp.RxOp>(v) };
            else {
                r = new QuantInfo();
                r.min = (int)Utils.S2N(asstr(atk(m,"0")));
                r.max = istrue(atk(m,"1")) ? (int)Utils.S2N(asstr(atk(m,"1"))) :
                    asstr(m).IndexOf("..") >= 0 ? int.MaxValue : r.min;
            }
            r.mod = ast<string>(atk(m,"quantmod"));
            r.general = true;
            r.space = istrue(atk(m,"normspace"));
            make(m,r);
        }

        public void separator(Cursor m) {
            make(m, new QuantInfo { sep = ast<RxOp.RxOp>(atk(m,"quantified_atom")),
                space = istrue(atk(m,"normspace")),
                opsep = m.global.orig_s[m.from] == '%' });
        }

        public void quantmod(Cursor m) {
            var t = asstr(m);
            if (t == "") { make(m,null); return; }
            if (t[0] == ':') t = t.Substring(1);
            if (t == "+") t = "";
            make(m,t);
        }

        public void quant_atom_list(Cursor m) {
            make(m, new RxOp.Sequence(
                flist_ast<RxOp.RxOp>(atk(m,"quantified_atom"))));
        }

        void LISTrx(Cursor m) {
            var tag = asstr(atk(atk(atk(m,"delims"),"0"),"sym"));
            var zyg = flist_ast<RxOp.RxOp>(atk(m,"list"));
            var dba = job.rxinfo.dba;
            if (tag == "&" || tag == "&") make(m,new RxOp.Conj(zyg));
            if (tag == "||") make(m,new RxOp.SeqAlt(zyg,dba));
            if (tag == "|") make(m,new RxOp.Alt(zyg,dba));
        }

        public void metachar__sigwhite(Cursor m) {
            make(m, job.rxinfo.s ? (RxOp.RxOp)new RxOp.Sigspace() : rnoop);
        }

        public void metachar__unsp(Cursor m) { make(m, rnoop); }

        public void metachar__7b_7d(Cursor m) {
            make(m, new RxOp.VoidBlock(rxblock(m, atk(m,"embeddedblock"))));
        }

        public Op.Op rxblock(Cursor m, Variable blk) {
            var si = ast<SubInfo>(blk);
            trymop(m, () => {
                addlex(m, si, "$¢", new LISimple(LISimple.NOINIT, null));
                si.sig = new Signature(new [] { Parameter.TPos("$¢",
                    si.dylex["$¢"].SigIndex()) });
            });

            return inliney_call(m, si, new Op.MakeCursor(m));
        }

        public void metachar__mod(Cursor m) {
            // most of these have only parse-time effects
            if (((Cursor)atk(m,"mod_internal").Fetch()).ast == null)
                make(m,rnoop);
            else
                from(m,"mod_internal");
        }

        public void metachar__3a3a(Cursor m) { make(m,new RxOp.CutLTM()); } //::
        public void metachar__3a3a3e(Cursor m) { make(m, new RxOp.CutBrack()); } // ::>
        public void metachar__3a3a3a(Cursor m) { make(m, new RxOp.CutRule()); } // :::

        public void metachar__5b_5d(Cursor m) { // [ ]
            make(m,new RxOp.ConfineLang(ast<RxOp.RxOp>(atk(m,"nibbler"))));
        }

        public void metachar__28_29(Cursor m) { // ( )
            var pnum = job.rxinfo.paren++;
            make(m, rxcapturize(m, pnum.ToString(), encapsulate_regex(m,
                ast<RxOp.RxOp>(atk(m,"nibbler")), true)));
        }

        public void metachar__3c28(Cursor m) { // <(
            make(m, new RxOp.Endpoint("from"));
        }

        public void metachar__293e(Cursor m) { // )>
            make(m, new RxOp.Endpoint("to"));
        }

        public void metachar__3c3c (Cursor m) { // <<
            make(m, new RxOp.ZeroWidth(RxFrame.BEFORE_WORD));
        }
        public void metachar__3e3e (Cursor m) { // >>
            make(m, new RxOp.ZeroWidth(RxFrame.AFTER_WORD));
        }
        public void metachar__ab (Cursor m) { // «
            make(m, new RxOp.ZeroWidth(RxFrame.BEFORE_WORD));
        }
        public void metachar__bb (Cursor m) { // »
            make(m, new RxOp.ZeroWidth(RxFrame.AFTER_WORD));
        }

        public void metachar__7b2a7d(Cursor m) { // {*}
            make(m, new RxOp.ProtoRedis());
        }

        public void metachar__qw(Cursor m) {
            var strings = new List<RxOp.RxOp>();
            trymop(m, () => {
                var words = eval_ast(m, ast<Op.Op>(atk(m,"circumfix")));
                foreach (var w in Builtins.UnboxLoS(words))
                    strings.Add(new RxOp.String(w, job.rxinfo.i));
            });
            make(m, new RxOp.Alt(strings.ToArray(), job.rxinfo.dba));
        }

        public void metachar__3c_3e(Cursor m) { // < >
            from(m,"assertion");
        }
        public void metachar__5c(Cursor m) { // \
            var cc = ast<object>(atk(m,"backslash"));
            make(m, (cc is string) ? new RxOp.String((string)cc, job.rxinfo.i)
                    : cc_to_rxop((CCinfo)cc));
        }

        public void metachar__2e(Cursor m) { // .
            make(m, new RxOp.Any());
        }

        public void metachar__5e(Cursor m) { // ^
            make(m, new RxOp.ZeroWidth(RxFrame.BEGIN_STRING));
        }
        public void metachar__5e5e(Cursor m) { // ^^
            make(m, new RxOp.ZeroWidth(RxFrame.BEGIN_LINE));
        }
        public void metachar__24(Cursor m) { // $
            make(m, new RxOp.ZeroWidth(RxFrame.END_STRING));
        }
        public void metachar__2424(Cursor m) { // $$
            make(m, new RxOp.ZeroWidth(RxFrame.END_LINE));
        }

        public void metachar__27_27(Cursor m) { // ' '
            var qa = ast<Op.Op>(atk(m,"quote"));
            if (!(qa is Op.StringLiteral)) {
                make(m,new RxOp.VarString(rxembed(m, qa)));
            } else {
                make(m, new RxOp.String(((Op.StringLiteral)qa).text,
                    job.rxinfo.i));
            }
        }
        public void metachar__22_22(Cursor m) { // " "
            metachar__27_27(m);
        }

        bool all_digits(string s) {
            foreach (char c in s)
                if (c < '0' || c > '9') return false;
            return true;
        }

        public void metachar__var(Cursor m) {
            if (istrue(atk(m,"binding"))) {
                var a = ast<RxOp.RxOp>(atk(atk(m,"binding"),"quantified_atom")).uncut();
                var cid = ast<VarInfo>(atk(m,"variable")).capid;

                if (cid == null) {
                    sorry(m, "Non-Match bindings NYI");
                    cid = "moo";
                }

                if (a is RxOp.VoidBlock) {
                    make(m, new RxOp.SaveValue(cid, ((RxOp.VoidBlock)a).block));
                    return;
                }

                if (all_digits(cid))
                    job.rxinfo.paren = 1 + (int)Utils.S2N(cid);

                make(m,rxcapturize(m, cid, a));
                return;
            }

            var kind = "scalar_var";
            var vs = asstr(atk(m,"variable"));
            if (vs[0] == '$') kind = "scalar_var";
            else if (vs[0] == '@') kind = "list_var";
            else sorry(m, "Only $ and @ variables may be used on regexes for now");

            make(m, new RxOp.ListPrim(vs, kind, rxembed(m,
                do_variable_reference(m, ast<VarInfo>(atk(m,"variable"))))));
            check_variable(atk(m,"variable"));
        }

        RxOp.RxOp rxcapturize(Cursor m, string name, RxOp.RxOp rxop) {
            RxOp.Capturing crxop = rxop as RxOp.Capturing;
            if (crxop == null) {
                // $<foo> = [...]
                crxop = new RxOp.StringCap(rxop);
            }

            // $<foo>=(...)
            // XXX might not quite be right
            if (crxop.captures.Length == 1 && all_digits(crxop.captures[0])) {
                return crxop.withcaps(new [] { name });
            }

            return crxop.withcaps(Utils.PrependArr(crxop.captures, name));
        }

        // UTS18 specifies a rule for "pulling up" negations in character
        // classes, so we have to delay the negation, it seems.
        struct CCinfo {
            public bool neg;
            public RxOp.RxOp rxop;
        }

        CCinfo negate_cc(CCinfo a) {
            return new CCinfo { neg = !a.neg, rxop = a.rxop };
        }
        CCinfo void_cc() { return cclass_cc(CClass.Empty); }
        CCinfo cclass_cc(CClass cc) {
            return new CCinfo { rxop = new RxOp.CClassElem(cc) };
        }
        CCinfo op_cc(bool neg, RxOp.RxOp rxop) { return new CCinfo { neg = neg, rxop = rxop }; }
        CCinfo neg_cclass_cc(CClass cc) { return negate_cc(cclass_cc(cc)); }
        CCinfo string_cc(string str) {
            return Utils.Codes(str) == 1 ? cclass_cc(CClass.list(Utils.Ord(str))) :
                new CCinfo { rxop = new RxOp.String(str, false) };
        }

        // TODO: implement this more directly
        CCinfo xor_cc(CCinfo lhs, CCinfo rhs) {
            return or_cc(and_cc(lhs, negate_cc(rhs)),
                         and_cc(negate_cc(lhs), rhs));
        }
        CCinfo and_cc(CCinfo lhs, CCinfo rhs) {
            return negate_cc(or_cc(negate_cc(lhs), negate_cc(rhs)));
        }

        CCinfo or_cc(CCinfo lhs, CCinfo rhs) {
            if (Config.CCTrace) Console.WriteLine("or({0} {1})", lhs.rxop, rhs.rxop);

            var ccl = lhs.rxop as RxOp.CClassElem;
            var ccr = rhs.rxop as RxOp.CClassElem;

            if (lhs.neg) {
                if (rhs.neg) {
                    return (ccl != null && ccr != null) ?
                        neg_cclass_cc(ccl.cc.minus(ccr.cc.negate())) :
                        op_cc(true, new RxOp.Conj(new [] { lhs.rxop, rhs.rxop }));
                } else { // !L | R = !(L & !R)
                    return (ccl != null && ccr != null) ?
                        neg_cclass_cc(ccl.cc.minus(ccr.cc)) :
                        op_cc(true, new RxOp.Sequence(new [] {
                            new RxOp.NotBefore(rhs.rxop), lhs.rxop }));
                }
            } else {
                if (rhs.neg) {
                    return or_cc(rhs, lhs);
                } else {
                    return (ccl != null && ccr != null) ?
                        cclass_cc(ccl.cc.plus(ccr.cc)) :
                        op_cc(false, new RxOp.Alt(new [] { lhs.rxop, rhs.rxop },
                            "character class"));
                }
            }
        }

        RxOp.RxOp cc_to_rxop(CCinfo z) {
            if (Config.CCTrace) Console.WriteLine("do_cc {0}", z.rxop);
            if (z.neg && z.rxop is RxOp.CClassElem)
                return new RxOp.CClassElem(((RxOp.CClassElem)z.rxop).cc.negate());
            return z.neg ? new RxOp.Sequence(new RxOp.RxOp[] {
                    new RxOp.NotBefore(z.rxop), new RxOp.Any() }) : z.rxop;
        }

        public void cclass_expr(Cursor m) {
            var ops = flist(atk(m,"ops"));
            var zyg = flist_ast<CCinfo>(atk(m,"cclass_union"));
            int sh = 0;
            var a = zyg[sh++];
            foreach (var op in ops) {
                a = (asstr(op) == "^") ? xor_cc(a,zyg[sh]) : or_cc(a,zyg[sh]);
                sh++;
            }
            if (Config.CCTrace) Console.WriteLine("cclass_expr {0}", a.rxop);
            make(m,a);
        }

        public void cclass_union(Cursor m) {
            var zyg = flist_ast<CCinfo>(atk(m,"cclass_add"));
            var a = zyg[0];
            for (int sh = 1; sh < zyg.Length; sh++) a = and_cc(a, zyg[sh]);
            if (Config.CCTrace) Console.WriteLine("cclass_union {0}", a.rxop);
            make(m,a);
        }

        public void cclass_add(Cursor m) {
            var zyg = flist_ast<CCinfo>(atk(m,"cclass_elem"));
            int sh = 0;
            var a = zyg[sh++];
            if (asstr(atk(m,"sign")) == "-") a = negate_cc(a);
            foreach (var op in flist(atk(m,"op"))) {
                a = asstr(op) == "+" ? or_cc(a, zyg[sh++])
                                     : and_cc(a, negate_cc(zyg[sh++]));
            }
            if (Config.CCTrace) Console.WriteLine("cclass_add {0}", a.rxop);
            make(m,a);
        }


        public void cclass_elem__name(Cursor m) {
            var ns = asstr(atk(m,"name"));
            make(m, ns == "INTERNAL::alpha" ?
                cclass_cc(CClass.Alpha) :
                op_cc(false, new RxOp.Subrule(ns, false)));
            if (Config.CCTrace)
                Console.WriteLine(":name {0} {1}", ns, ast<CCinfo>(m).rxop);
        }

        public void cclass_elem__5b_5d(Cursor m) { // [ ]
            from(m,"nibble");
            if (Config.CCTrace)
                Console.WriteLine(":[] {0}", ast<CCinfo>(m).rxop);
        }

        public void cclass_elem__28_29(Cursor m) { // ( )
            from(m,"cclass_expr");
        }

        public void cclass_elem__property(Cursor m) {
            var body = thunk_sub(ast<Op.Op>(atk(m,"colonpair")),
                new string[0], asstr(atk(m,"colonpair")));
            body.outer.CreateProtopad(null);
            make(m,void_cc());
            trymop(m, () => {
                var oa = (object[])Niecza.UCD.DataSet.CompileCClass(body.RunBEGIN());
                var ia = new int[oa.Length];
                Array.Copy(oa,0, ia,0, ia.Length);
                make(m, cclass_cc(new CClass(ia)));
            });
        }

        public void cclass_elem__quote(Cursor m) {
            var qa = ast<Op.Op>(atk(m,"quote"));
            var sqa = qa as Op.StringLiteral;
            if (sqa == null) {
                make(m,new RxOp.VarString(rxembed(m, qa)));
            } else if (!job.rxinfo.i) {
                make(m,string_cc(sqa.text));
            } else {
                make(m,op_cc(false,new RxOp.String(sqa.text, job.rxinfo.i)));
            }
        }

        RxOp.RxOp decapturize(Cursor m) {
            var a = ast<RxOp.RxOp>(atk(m,"assertion"));
            var ac = a as RxOp.Capturing;
            return ac == null ? a : ac.withcaps(new string[0]);
        }


        public void assertion__name(Cursor m) {
            var pname = process_name(atk(m,"longname"), DEFER);
            var name  = asstr(atk(m,"longname"));

            if (pname.name == null && pname.ind == null) {
                pname.name = "alpha";
                sorry(m,"Method call requires a method name");
            }

            var lex = lookup_lex(job.curlex, "&" + name) as LISub;
            var is_lexical = m.global.orig_s[m.from-1] != '.' &&
                lex != null && lex.def.mo.HasType(Kernel.RegexMO);

            if (istrue(atk(m,"assertion"))) {
                from(m,"assertion");
            } else if (name == "sym") {
                if (job.rxinfo.sym == null)
                    sorry(m,"<sym> is only valid in multiregexes");
                make(m,new RxOp.Sym(job.rxinfo.sym ?? "", job.rxinfo.endsym,
                    job.rxinfo.i, job.rxinfo.a));
            } else if (name == "before") {
                make(m,new RxOp.Before(ast<RxOp.RxOp>(atk(m,"nibbler"))));
                return; // no capture needed
            } else if (name == "after") {
                var ll = new List<CClass>();
                ast<RxOp.RxOp>(atk(m,"nibbler")).tocclist(ll);
                foreach (var l in ll) {
                    if (l == null) {
                        sorry(m,"Unsupported elements in after list");
                        ll.Clear();
                        break;
                    }
                }
                make(m,new RxOp.ZeroWidthCCs(ll.ToArray(),true,false));
                return; // no capture needed
            } else if (!istrue(atk(m,"nibbler")) && !istrue(atk(m,"arglist")) &&
                    pname.pkg == null && pname.ind == null && !is_lexical) {
                make(m,new RxOp.Subrule(pname.name, false));
            } else {
                var args = istrue(atk(m,"nibbler")) ?
                    new [] { op_for_regex(m, ast<RxOp.RxOp>(atk(m,"nibbler")))}:
                    istrue(atk(m,"arglist")) ? ast<Op.Op[]>(atk(m,"arglist")) :
                    new Op.Op[0];

                Op.Op callop = null;
                if (is_lexical) {
                    callop = new Op.CallSub(m, Op.Helpers.mklex(m,"&"+name),
                        true, Utils.PrependArr(args,Op.Helpers.mklex(m,"$¢")));
                } else if (pname.ind != null) {
                    callop = (new Operator.Method(pname.ind, args, "::(",
                        false, null)).with_args(m, Op.Helpers.mklex(m,"$¢"));
                } else {
                    callop = (new Operator.Method(pname.name, args, "",
                        false, pname.pkg)).with_args(m, Op.Helpers.mklex(m,"$¢"));
                }

                make(m, new RxOp.Subrule(rxembed(m, callop), false));
            }

            make(m, rxcapturize(m, name, ast<RxOp.RxOp>(m)));
        }

        public void assertion__variable(Cursor m) {
            var va = ast<Op.Op>(atk(m,"variable"));
            var vacs = va as Op.CallSub;
            switch(asstr(m)[0]) {
                case '&':
                    if (vacs != null) {
                        make(m,new RxOp.Subrule(new Op.CallSub(m, vacs.invocant,
                            false, Utils.PrependArr(vacs.getargs(),
                                new Op.MakeCursor(m)))));
                    } else {
                        make(m,new RxOp.Subrule(new Op.CallSub(m, va, true,
                            new Op.MakeCursor(m))));
                    }
                    break;
                case '$':
                    make(m,new RxOp.ListPrim("", "scalar_asn", rxembed(m,va)));
                    break;
                case '@':
                    make(m,new RxOp.ListPrim("", "list_asn", rxembed(m,va)));
                    break;
                default:
                    make(m,rnoop);
                    sorry(m,"Sigil {0} is not allowed for regex assertions", asstr(m)[0]);
                    break;
            }
        }

        public void assertion__method(Cursor m) {
            if (istrue(atk(m,"dottyop"))) {
                make(m,new RxOp.Subrule(rxembed(m,
                    ast<Operator>(atk(m,"dottyop")).with_args(m,
                        new Op.MakeCursor(m)))));
            } else {
                make(m,decapturize(m));
            }
        }

        void booly_assertion(Cursor m, bool neg) {
            var asrt = atk(m,"assertion").Fetch();
            if (istrue(asrt)) {
               if (((Cursor)asrt).reduced == "assertion:sym<{ }>") {
                   make(m,new RxOp.CheckBlock(ast<RxOp.ListPrim>(asrt).ops, neg));
                } else if (neg) {
                    make(m,new RxOp.NotBefore(decapturize(m)));
                } else {
                    make(m,new RxOp.Before(decapturize(m)));
                }
            } else {
                make(m,neg ? new RxOp.None() : rnoop);
            }
        }
        public void assertion__3f(Cursor m) { booly_assertion(m, false); } //?
        public void assertion__21(Cursor m) { booly_assertion(m, true); }  //!

        public void assertion__7b_7d(Cursor m) { // { }
            make(m,new RxOp.ListPrim("", "scalar_asn",
                        rxblock(m,atk(m,"embeddedblock"))));
        }

        public void assertion__3a(Cursor m) { // :
            make(m,cc_to_rxop(ast<CCinfo>(atk(m,"cclass_expr"))));
        }
        public void assertion__5b(Cursor m) { assertion__3a(m); } // [
        public void assertion__2d(Cursor m) { assertion__3a(m); } // -
        public void assertion__2b(Cursor m) { assertion__3a(m); } // +

        public void mod_internal__3amy(Cursor m) {
            make(m,new RxOp.Statement(ast<Op.Op>(atk(m,"statement"))));
        }

        public void mod_internal__p6adv(Cursor m) {
            make(m,rnoop);
            var k = atk(atk(m,"quotepair"),"k");
            var v = atk(atk(m,"quotepair"),"v");
            if (!v.Fetch().Isa(Kernel.MatchMO)) {
                sorry(m,":{0} requires an expression argument",asstr(k));
                return;
            }
            var va = ast<Op.Op>(v);
            var ks = asstr(k);

            if (ks == "lang") {
                make(m,new RxOp.SetLang(rxembed(m, va)));
            } else if (ks == "dba") {
                job.rxinfo.dba = asstr(eval_ast(m,va));
            }
        }

        public void backslash__qq(Cursor m) { from(m,"quote"); }

        void bslash(Cursor m, string ss) {
            make(m, char.IsUpper(asstr(m)[0]) ? negate_cc(string_cc(ss)) :
                    (object)ss);
        }
        void bslash(Cursor m, CClass cc) {
            make(m, char.IsUpper(asstr(m)[0]) ? negate_cc(cclass_cc(cc)) :
                    cclass_cc(cc));
        }
        void bslash(Cursor m, CCinfo ii) {
            make(m, char.IsUpper(asstr(m)[0]) ? negate_cc(ii) : ii);
        }

        public void backslash__x(Cursor m) {
            var sb = new StringBuilder();
            if (istrue(atk(m,"hexint"))) {
                sb.Append(asstr(Builtins.chr(ast<P6any>(atk(m,"hexint")))));
            } else {
                foreach (var hi in ast<P6any[]>(atk(m,"hexints")))
                    sb.Append(asstr(Builtins.chr(hi)));
            }
            bslash(m, sb.ToString());
        }

        public void backslash__o(Cursor m) {
            var sb = new StringBuilder();
            if (istrue(atk(m,"octint"))) {
                sb.Append(asstr(Builtins.chr(ast<P6any>(atk(m,"octint")))));
            } else {
                foreach (var hi in ast<P6any[]>(atk(m,"octints")))
                    sb.Append(asstr(Builtins.chr(hi)));
            }
            bslash(m, sb.ToString());
        }

        public void backslash__5c(Cursor m) { make(m,"\\"); }
        public void backslash__stopper(Cursor m) { make(m,asstr(atk(m,"text"))); }
        public void backslash__unspace(Cursor m) { make(m,""); }
        public void backslash__misc(Cursor m) {
            make(m, isdef(atk(m,"text")) ? asstr(atk(m,"text")) :
                    asstr(atk(m,"litchar")));
        }
        public void backslash__0(Cursor m) { make(m,"\0"); }
        public void backslash__a(Cursor m) { bslash(m,"\a"); }
        public void backslash__b(Cursor m) { bslash(m,"\b"); }
        public void backslash__c(Cursor m) { bslash(m,ast<string>(atk(m,"charspec"))); }
        public void backslash__d(Cursor m) { bslash(m,CClass.Digit); }
        public void backslash__e(Cursor m) { bslash(m,"\u001B"); }
        public void backslash__f(Cursor m) { bslash(m,"\f"); }
        public void backslash__h(Cursor m) { bslash(m,CClass.HSpace); }

        public void backslash__n(Cursor m) {
            if (m.save_klass.FindMethod("backslash:d") != null) {
                // HACK - only use this form when we're looking for regexy stuff
                bslash(m, op_cc(false, new RxOp.Newline()));
            } else {
                make(m,"\n");
            }
        }

        public void backslash__r(Cursor m) { bslash(m,"\r"); }
        public void backslash__s(Cursor m) { bslash(m,CClass.Space); }
        public void backslash__t(Cursor m) { bslash(m,"\t"); }
        public void backslash__v(Cursor m) { bslash(m,"\v"); }
        public void backslash__w(Cursor m) { bslash(m,CClass.Word); }

        public void escape__5c(Cursor m) { from(m,"item"); }
        public void escape__7b_7d(Cursor m) {
            make(m, inliney_call(m, ast<SubInfo>(atk(m,"embeddedblock"))));
        }
        public void escape__24(Cursor m) { from(m,"EXPR"); } // $
        public void escape__40(Cursor m) { from(m,"EXPR"); } // @
        public void escape__25(Cursor m) { from(m,"EXPR"); } // %
        public void escape__ch(Cursor m) { make(m,asstr(atk(m,"ch"))); }
        public void escape__ws(Cursor m) { make(m,""); }
        class RangeSymbol { }
        public void escape__2e2e(Cursor m) { make(m,new RangeSymbol()); }

        static CC hsp = new CC(CClass.HSpace.terms);
        static CC sp = new CC(CClass.Space.terms);

        string[] words(string str) {
            var ret = new List<string>();
            int pos = 0;
            while (pos < str.Length) {
                int pos0 = pos;
                bool reject = sp.Accepts(str[pos++]);
                while (pos < str.Length && reject == sp.Accepts(str[pos])) pos++;
                if (!reject) ret.Add(str.Substring(pos0, pos - pos0));
            }
            return ret.ToArray();
        }

        Op.Op process_nibble(Cursor m, Variable[] bits, string prefix) {
            var acc = new List<Op.Op>();

            foreach (var n in bits) {
                var cn = (Cursor)n.Fetch();
                var ast_o = cn.ast == null ? asstr(n) : ast<object>(n);

                if (ast_o is CCinfo || ast_o is CClass) {
                    sorry(cn, "Cannot use a character class in a string");
                    ast_o = "";
                }

                if (ast_o is string && prefix != "" && cn.reduced == "Str") {
                    var sb = new StringBuilder();
                    var orig_a = cn.global.orig_a;
                    for (int i = cn.from; i < cn.pos; i++) {
                        // This is the heredoc whitespace stripper: after a
                        // literal linefeed, ignore literal whitespace up to
                        // the amount in the trailer.
                        if ((i == 0 || CC.VSpace.Accepts(orig_a[i-1])) &&
                                i != orig_a.Length && hsp.Accepts(orig_a[i])) {
                            if (orig_a.Length - i >= prefix.Length &&
                                    cn.global.orig_s.Substring(i, prefix.Length) == prefix) {
                                i += prefix.Length;
                            } else {
                                while (i != orig_a.Length && hsp.Accepts(orig_a[i]))
                                    i++;
                            }
                            if (i >= cn.pos)
                                break;
                        }
                        sb.Append(orig_a[i++]);
                    }
                    ast_o = sb.ToString();
                }

                acc.Add((ast_o as Op.Op) ?? new Op.StringLiteral(m,(string)ast_o));
            }

            var sl = Op.Helpers.mkstringycat(m, acc.ToArray());
            var post = asstr(Builtins.InvokeMethod("postprocessor", m.UnMatch()));
            if (post == "null") {
                // already OK
            } else if (post == "words" || post == "quotewords") {
                // actually quotewords is a bit trickier than this...
                if (sl is Op.StringLiteral) {
                    var text = ((Op.StringLiteral)sl).text;
                    var tok = words(text);
                    if (tok.Length == 1 && tok[0] == text && post == "words") {
                        // <1/2> special case
                        if (lookup_lex(job.curlex, "&val_nospace") != null)
                            sl = Op.Helpers.mkcall(m,"&val_nospace",sl);
                    } else {
                        var tokop = new Op.Op[tok.Length];
                        var val = lookup_lex(job.curlex, "&val") != null;
                        for (int i = 0; i < tok.Length; i++) {
                            tokop[i] = new Op.StringLiteral(m, tok[i]);
                            if (val) tokop[i] = Op.Helpers.mkcall(m, "&val", tokop[i]);
                        }
                        sl = tok.Length == 1 ? tokop[0] : new Op.Paren(m,
                            new Op.SimpleParcel(m, tokop));
                    }
                } else {
                    sl = new Op.CallMethod(m, "words-val", sl);
                }
            } else if (post == "path") {
                // TODO could stand to be a lot fancier.
                sl = new Op.CallMethod(m, "IO", sl);
            } else {
                sorry(m, "Unhandled postprocessor {0}", post);
            }

            return sl;
        }

        CCinfo process_tribble(Variable[] bits) {
            var mstack = new Cursor[bits.Length];
            var cstack = new object[bits.Length];
            int depth = 0;

            foreach (var n in bits) {
                var cn = (Cursor)n.Fetch();
                var ast = cn.ast == null ? asstr(n) : ast<object>(n);
                if (ast is string && ((string)ast) == "")
                    continue;

                mstack[depth] = cn;
                cstack[depth] = ast;

                if (depth >= 2 && cstack[depth-2] is RangeSymbol) {
                    if (depth == 2) {
                        sorry(mstack[0], ".. requires a left endpoint");
                        return void_cc();
                    }

                    for (int i = 1; i <= 3; i += 2) {
                        var cas = cstack[depth-i] as string;
                        if (cas == null || Utils.Codes(cas) != 1) {
                            sorry(mstack[depth-i], ".. endpoint must be a single character");
                            return void_cc();
                        }
                    }

                    cstack[depth-3] = cclass_cc(CClass.range(
                                Utils.Ord((string)cstack[depth-3]),
                                Utils.Ord((string)cstack[depth-1])));
                    depth -= 2;
                }
            }

            if (depth > 0 && cstack[depth-1] is RangeSymbol) {
                sorry(mstack[depth-1], ".. requires a right endpoint");
                return void_cc();
            }

            var retcc = void_cc();
            for (int i = 0; i < depth; i++)
                retcc = or_cc(retcc, cstack[i] as CCinfo? ??
                        string_cc((string) cstack[i]));
            return retcc;
        }

        public void nibbler(Cursor m) { nibbler_(m, ""); }
        void nibbler_(Cursor m, string prefix) {
            if (istrue(atk(m,"EXPR"))) {
                from(m,"EXPR");
            } else if (istrue(atk(m,"cgexp"))) {
                if (job.mgr.safe_mode) {
                    sorry(m, "Q:CgOp not allowed in safe mode");
                    make(m,new Op.StatementList(m));
                    return;
                }
                make(m, new Op.RawCgOp(m, ast<object>(atk(m,"cgexp"))));
            } else if (m.save_klass.FindMethod("ccstate") != null) {
                make(m, process_tribble(flist(atk(m,"nibbles"))));
            } else {
                make(m, process_nibble(m, flist(atk(m,"nibbles")), prefix));
            }
        }

        public void circumfix__3c_3e(Cursor m) { from(m,"nibble"); }
        public void circumfix__3c3c_3e3e(Cursor m) { from(m,"nibble"); }
        public void circumfix__ab_bb(Cursor m) { from(m,"nibble"); }

        public void circumfix__28_29(Cursor m) { // ( )
            var kids = ast<Op.Op[]>(atk(m,"semilist"));
            if (kids.Length == 1 && kids[0] is Op.WhateverCode) {
                // XXX in cases like * > (2 + *), we *don't* want the parens to
                // disable syntactic specialization, since they're required for
                // grouping
                make(m, kids[0]);
            } else if (kids.Length == 0) {
                // an empty StatementList returns Nil, but () needs to be
                // defined...
                make(m, new Op.Paren(m, new Op.SimpleParcel(m)));
            } else {
                make(m, new Op.StatementList(m, kids));
            }
        }

        public void circumfix__5b_5d(Cursor m) { // [ ]
            var kids = ast<Op.Op[]>(atk(m,"semilist"));
            make(m,Op.Helpers.mkcall(m, "&_array_constructor",
                new Op.StatementList(m, kids)));
        }

        bool check_hash(SubInfo sub, Op.Op code) {
            if (Builtins.sig_arity(sub.sig) != 0)
                return false;
            var dosl = code as Op.StatementList;
            if (dosl == null)
                return false;

            if (dosl.children.Length == 0) return true;
            if (dosl.children.Length > 1) return false;
            code = dosl.children[0];

            var bits = code is Op.SimpleParcel ? ((Op.SimpleParcel)code).items :
                new [] { code };
            if (bits.Length == 0) return false;

            if (bits[0] is Op.SimplePair) return true;
            if (bits[0] is Op.CallSub) {
                var inv = (bits[0] as Op.CallSub).invocant as Op.Lexical;
                if (inv != null && inv.name == "&infix:<=>>") return true;
            }

            if (bits[0] is Op.Const) {
                var ob = bits[0].const_value(null).Fetch();
                if (ob.Isa(Kernel.PairMO)) return true;
                if (ob.Isa(Kernel.ParcelMO) && Kernel.UnboxAny<Variable[]>(ob)[0].Fetch().Isa(Kernel.PairMO)) return true;
            }

            if (bits[0] is Op.Lexical && (bits[0] as Op.Lexical).name[0] == '%')
                return true;

            return false;
        }

        public void circumfix__7b_7d(Cursor m) { // { }
            var sym = job.gensym();
            var pbl = ast<SubInfo>(atk(m,"pblock"));
            addlex(m, job.curlex, sym, new LISub(pbl));

            make(m, pbl.GetExtend0T("hashy", false) ?
                Op.Helpers.mkcall(m, "&_hash_constructor",
                    CompUtils.BetaCall(m, sym)) :
                new Op.BareBlock(m, sym));
        }

        public void circumfix__sigil(Cursor m) {
            // XXX duplicates logic in variable
            var sigil = asstr(atk(m,"sigil"))[0];
            if (ast<Op.Op[]>(atk(m,"semilist")).Length == 0) {
                if (sigil == '$') {
                    make(m, new Op.ShortCircuit(m, Op.ShortCircuit.DOR,
                        new Op.CallMethod(m, "ast", Op.Helpers.mklex(m,"$/")),
                        new Op.CallMethod(m, "Str", Op.Helpers.mklex(m,"$/"))));
                } else if (sigil == '@' || sigil == '%') {
                    make(m, docontext(m, sigil, Op.Helpers.mklex(m,"$/")));
                } else {
                    make(m, Op.Helpers.mklex(m,"Mu"));
                    sorry(m, "Missing argument for contextualizer");
                }
                return;
            }
            circumfix__28_29(m);
            make(m,docontext(m, sigil, ast<Op.Op>(m)));
        }

        public void infix_prefix_meta_operator__21(Cursor m) { // !
            make(m, ast<Operator>(atk(m,"infixish")).meta_not());
        }

        public void infix_prefix_meta_operator__R(Cursor m) {
            make(m, ast<Operator>(atk(m,"infixish")).meta_fun(m, "&reverseop", 2));
        }
        public void infix_prefix_meta_operator__S(Cursor m) {
            make(m, ast<Operator>(atk(m,"infixish")).meta_fun(m, "&seqeop", 2));
        }
        public void infix_prefix_meta_operator__Z(Cursor m) {
            make(m, istrue(atk(m,"infixish")) ?
                    ast<Operator>(atk(m,"infixish")).meta_fun(m, "&zipop", 2) :
                    Operator.funop(m, "&infix:<Z>", 2));
        }
        public void infix_prefix_meta_operator__X(Cursor m) {
            make(m, istrue(atk(m,"infixish")) ?
                    ast<Operator>(atk(m,"infixish")).meta_fun(m, "&crossop", 2) :
                    Operator.funop(m, "&infix:<X>", 2));
        }

        void hyper(Cursor m, char flexl, char flexr) {
            make(m, ast<Operator>(atk(m,"infixish")).meta_fun(m, "&hyper", 2,
                new [] { Op.Helpers.mkbool(m, m.global.orig_a[m.from] == flexl),
                Op.Helpers.mkbool(m, m.global.orig_a[m.pos-1] == flexr) } ));
        }
        public void infix_circumfix_meta_operator__ab_bb(Cursor m) { hyper(m,'«','»'); }
        public void infix_circumfix_meta_operator__3c3c_3e3e(Cursor m) { hyper(m,'<','>'); }

        public void postfix_prefix_meta_operator__bb(Cursor m) {} // in POST
        public void prefix_postfix_meta_operator__ab(Cursor m) {} // in PRE

        public void infixish(Cursor m) {
            if (istrue(atk(m,"colonpair")) || istrue(atk(m,"regex_infix"))) {
                make(m,null);
                return; // handled elsewhere
            }

            var ast = ast<Operator>(atk(m,"infix"));
            if (istrue(atk(m, "assign_meta_operator"))) {
                // TODO: there should be at least a potential for others
                make(m, ast.meta_assign());
            } else {
                make(m, ast);
            }
        }

        public void infix__2e2e2e(Cursor m) { // ...
            // STD parses ...^ in the ... rule
            make(m, Operator.funop(m, "&infix:<" + asstr(m) + ">", 2));
        }

        public void infix__xx(Cursor m) { make(m, new Operator.Replicate()); }
        public void infix__ff(Cursor m) { make(m, new Operator.FlipFlop(false,false,false)); }
        public void infix__fff(Cursor m) { make(m, new Operator.FlipFlop(false,false,true)); }
        public void infix__ff5e(Cursor m) { make(m, new Operator.FlipFlop(false,true,false)); }
        public void infix__fff5e(Cursor m) { make(m, new Operator.FlipFlop(false,true,true)); }
        public void infix__5eff(Cursor m) { make(m, new Operator.FlipFlop(true,false,false)); }
        public void infix__5efff(Cursor m) { make(m, new Operator.FlipFlop(true,false,true)); }
        public void infix__5eff5e(Cursor m) { make(m, new Operator.FlipFlop(true,true,false)); }
        public void infix__5efff5e(Cursor m) { make(m, new Operator.FlipFlop(true,true,true)); }
        public void infix__7e7e(Cursor m) { make(m, new Operator.SmartMatch()); }
        public void infix__2c(Cursor m) { make(m, new Operator.Comma()); }
        public void infix__3a3d(Cursor m) { make(m, new Operator.Binding(false)); }
        public void infix__3a3a3d(Cursor m) { make(m, new Operator.Binding(true)); }
        public void infix__2626(Cursor m) { make(m, new Operator.ShortCircuit(Op.ShortCircuit.AND)); }
        public void infix__and(Cursor m) { make(m, new Operator.ShortCircuit(Op.ShortCircuit.AND)); }
        public void infix__7c7c(Cursor m) { make(m, new Operator.ShortCircuit(Op.ShortCircuit.OR)); }
        public void infix__or(Cursor m) { make(m, new Operator.ShortCircuit(Op.ShortCircuit.OR)); }
        public void infix__2f2f(Cursor m) { make(m, new Operator.ShortCircuit(Op.ShortCircuit.DOR)); }
        public void infix__orelse(Cursor m) { make(m, new Operator.ShortCircuit(Op.ShortCircuit.DOR)); }
        public void infix__andthen(Cursor m) { make(m, new Operator.ShortCircuit(Op.ShortCircuit.DAND)); }
        public void infix__3f3f_2121(Cursor m) { make(m, new Operator.Ternary(ast<Op.Op>(atk(m,"EXPR")))); }
        public void infix__2e3d(Cursor m) { make(m, new Operator.DotEq()); }
        public void infix__does(Cursor m) {
            make(m, new Operator.Mixin(Op.Helpers.mklex(m, "&infix:<does>")));
        }
        public void infix__but(Cursor m) {
            make(m, new Operator.Mixin(Op.Helpers.mklex(m, "&infix:<but>")));
        }

        public void prefix__temp(Cursor m) { make(m, new Operator.Temp()); }
        public void prefix__let(Cursor m) { make(m, new Operator.Let()); }

        public void statement_control__TEMP(Cursor m) {
            job.curlex.special |= SubInfo.CANNOT_INLINE;
            make(m, new Op.Temporize(m, inliney_call(m,
                            ast<SubInfo>(atk(m,"block"))), Builtins.T_BLOCK));
        }

        // Now that initializer has been split out this can be a lot smaller...
        public void INFIX(Cursor m) {
            var fn = ast<Operator>(atk(m,"infix"));
            var args = new[] { ast<Op.Op>(atk(m,"left")), ast<Op.Op>(atk(m,"right")) };
            var st = whatever_precheck(m, fn, args);
            make(m, whatever_postcheck(m, st, fn.with_args(m, args)));
        }

        public void CHAIN(Cursor m) {
            var chain = flist(atk(m,"chain"));
            var args = new Op.Op[chain.Length / 2];
            var ops  = new Operator[chain.Length / 2];

            for (int i = 0; ; i += 2) {
                args[i/2] = ast<Op.Op>(chain[i]);
                if (i+1 == chain.Length) break;
                ops[i/2] = ast<Operator>(atk(chain[i+1],"infix"));
            }

            var st = whatever_precheck(m, ops[0], args);

            int j = 0;
            Func<Op.Op> reduce = () => {
                var fa = args[j];
                var fo = ops[j];
                j++;
                if (j != ops.Length) {
                    return Op.Helpers.mklet(m, fa, (lhs) =>
                        Op.Helpers.mklet(m, args[j], (rhs) => {
                            args[j] = rhs;
                            return new Op.ShortCircuit(m, Op.ShortCircuit.AND,
                                fo.with_args(m, lhs, rhs), reduce());
                        }));
                } else {
                    return fo.with_args(m, fa, args[j]);
                }
            };

            make(m, whatever_postcheck(m, st, reduce()));
        }

        public void LIST(Cursor m) {
            if (m.save_klass.FindMethod("regex_infix") != null) {
                LISTrx(m);
                return;
            }
            // STD guarantees that all elements of delims have the same sym
            // the last item may have an ast of undef due to nulltermish
            var fn = flist_ast<Operator>(atk(m,"delims"))[0];
            var list = flist_ast<Op.Op>(atk(m,"list"));
            if (list.Length != 0 && list[list.Length - 1] == null)
                list = Utils.TrimArr(list,0,1);
            var st = whatever_precheck(m, fn, list);
            make(m, whatever_postcheck(m, st, fn.with_args(m, list)));
        }

        public void POSTFIX(Cursor m) {
            // adverbs have undef ast
            var arg = new [] { ast<Op.Op>(atk(m,"arg")) };
            var op  = ast<Operator>(atk(m,"op"));
            var st = whatever_precheck(m, op, arg);
            if (istrue(atk(atk(m,"op"),"colonpair"))) {
                var clarg = arg[0] as Op.CallLike;
                if (clarg != null) {
                    make(m, whatever_postcheck(m, st, clarg.adverb(
                        ast<Op.Op>(atk(atk(m,"op"),"colonpair")))));
                } else {
                    sorry(m, "You can't adverb that");
                    make(m, new Op.StatementList(m));
                }
            } else {
                make(m, whatever_postcheck(m, st, op.with_args(m, arg)));
            }
        }

        public void PREFIX(Cursor m) {
            var arg = new [] { ast<Op.Op>(atk(m,"arg")) };
            var op  = ast<Operator>(atk(m,"op"));
            var st = whatever_precheck(m, op, arg);
            make(m, whatever_postcheck(m, st, op.with_args(m, arg)));
        }

        public void postcircumfix__5b_5d(Cursor m) { // [ ]
            make(m, Operator.funop(m, "&postcircumfix:<[ ]>", 1,
                    ast<Op.Op[]>(atk(m,"semilist"))));
        }

        public void postcircumfix__7b_7d(Cursor m) { // { }
            make(m, Operator.funop(m, "&postcircumfix:<{ }>", 1,
                    ast<Op.Op[]>(atk(m,"semilist"))));
        }

        public void postcircumfix__3c_3e(Cursor m) { // < >
            make(m, Operator.funop(m, "&postcircumfix:<{ }>", 1,
                    ast<Op.Op>(atk(m,"nibble"))));
        }

        public void postcircumfix__28_29(Cursor m) { // ( )
            make(m, new Operator.PostCall(ast<Op.Op[][]>(atk(m,"semiarglist"))[0]));
        }

        public void postop(Cursor m) {
            from(m, istrue(atk(m,"postcircumfix")) ? "postcircumfix" : "postfix");
        }

        public void POST(Cursor m) {
            if (istrue(atk(m,"dotty" ))) from(m,"dotty" );
            if (istrue(atk(m,"privop"))) from(m,"privop");
            if (istrue(atk(m,"postop"))) from(m,"postop");

            foreach (var mo in flist(atk(m,"postfix_prefix_meta_operator")))
                make(m,ast<Operator>(m).meta_fun(m, "&hyperunary", 1));
        }

        public void PRE(Cursor m) {
            if (istrue(atk(m,"prefix"))) from(m,"prefix");
            if (istrue(atk(m,"prefix_circumfix_meta_operator")))
                from(m,"prefix_circumfix_meta_operator");

            foreach (var mo in flist(atk(m,"prefix_postfix_meta_operator")))
                make(m,ast<Operator>(m).meta_fun(m, "&hyperunary", 1));
        }

        // from now on args shall return Op.Op[]
        public void methodop(Cursor m) {
            var args = istrue(atk(m,"arglist")) ?
                ast<Op.Op[]>(atk(m,"arglist")) : istrue(atk(m,"args")) ?
                ast<Op.Op[]>(atk(m,"args")) : new Op.Op[0];

            if (istrue(atk(m,"longname"))) {
                var c = process_name(atk(m,"longname"), DEFER);
                make(m,Operator.funop(m,"&die",1));
                if (c.name == null && c.ind == null) {
                    sorry(m, "Method call requires a name");
                    return;
                }
                if (c.ind != null) {
                    make(m, new Operator.Method(c.ind, args, "::(", false, null));
                } else {
                    make(m, new Operator.Method(c.name, args, "", false, c.pkg));
                }
            } else if (istrue(atk(m,"quote"))) {
                make(m,new Operator.Method(ast<Op.Op>(atk(m,"quote")), args,
                    "", false, null));
            } else {
                var v = atk(m,"variable");
                make(m,new Operator.Function(do_variable_reference(m,
                    ast<VarInfo>(v)), 1, null, args));
                check_variable(v);
            }
        }

        public void dottyopish(Cursor m) {
            make(m, new Op.DotEqRHS(m, ast<Operator>(atk(m,"term"))));
        }

        public void dottyop(Cursor m) {
            if (istrue(atk(m,"colonpair"))) {
                sorry(m, ".:foo syntax NYI");
                make(m, Operator.funop(m,"&postfix:<++>",1));
                return;
            }

            if (istrue(atk(m,"methodop"))) from(m,"methodop");
            if (istrue(atk(m,"postop"))) from(m,"postop");
        }

        public void privop(Cursor m) {
            var ast = ast<Operator>(atk(m,"methodop")) as Operator.Method;
            if (ast == null) {
                from(m,"methodop");
                sorry(m,"! privacy marker only affects search, and as such is meaningless with a method reference.");
            } else {
                make(m, new Operator.Method(ast.name, ast.arglist, ast.meta, true, ast.package));
            }
        }

        public void dotty__2e(Cursor m) { from(m,"dottyop"); } // .
        public void dotty__2e2a(Cursor m) { // .*
            var sym = asstr(atk(m,"sym"));
            var dop = ast<Operator>(atk(m,"dottyop"));
            if (sym == ".=") {
                make(m, dop.meta_assign());
                return;
            }
            var mdop = dop as Operator.Method;
            if (mdop == null || mdop.meta != "") {
                sorry(m,"Modified method calls can only be used with actual methods");
                make(m,dop);
                return;
            }
            if (sym == ".^" || sym == ".?") {
                make(m, new Operator.Method(mdop.name, mdop.arglist, sym.Substring(1), mdop.privat, mdop.package));
            } else {
                make(m, dop);
                sorry(m, "NYI dottyop form {0}", sym);
            }
        }

        public void coloncircumfix(Cursor m) { from(m,"circumfix"); }

        Variable eval_ast(Cursor m, Op.Op ast, Variable def = null) {
            ast = ast.simplify(job.curlex);
            var kon = ast.const_value(job.curlex);
            if (kon != null) return kon;
            var sub = thunk_sub(ast, new string[0], "ANON_BEGIN", null, null, true);
            job.curlex.CreateProtopad(null);
            Variable res = def;
            trymop(m, () => {
                res = sub.RunBEGIN();
            });
            return res ?? mkstr("");
        }

        string get_cp_ext(Variable cp) {
            var m = (Cursor)cp.Fetch();
            var str = asstr(cp);
            var v = atk(m,"v");
            var k = atk(m,"k");
            if (str == ":_" || str == ":U" || str == ":D" || str == ":T") {
                return "";
            } else if (!v.Fetch().Isa(Kernel.MatchMO)) {
                return ":" + (istrue(v) ? "" : "!") + asstr(k);
            } else {
                var suf = ((Cursor)v.Fetch()).ast == null ? asstr(v) :
                    asstr(eval_ast(m, ast<Op.Op>(v)));
                return ":" + asstr(k) + "<" + suf + ">";
            }
        }

        public void colonpair_var(Cursor m) {
            if (asstr(m)[1] == '<') {
                make(m, new VarInfo { term = Op.Helpers.mkcall(m,
                    "&postcircumfix:<{ }>", new Op.Lexical(m,"$/"),
                    new Op.StringLiteral(m, asstr(atk(m,"desigilname")))) });
            } else {
                make(m, new VarInfo { sigil = asstr(atk(m,"sigil"))[0],
                    twigil = istrue(atk(m,"twigil")) ?
                        asstr(atk(m,"twigil"))[0] : '\0',
                    name = asstr(atk(m,"desigilname")) });
            }
        }

        public void colonpair(Cursor m) {
            var k = atk(m,"k"); var v = atk(m,"v");
            var tv = v.Fetch().Isa(Kernel.MatchMO) ? ast<object>(v) :
                Op.Helpers.mkbool(m, istrue(v));

            if (tv is VarInfo) {
                tv = do_variable_reference(m, (VarInfo) tv);
                check_variable(v);
            }

            make(m, new Op.SimplePair(m,asstr(k), (Op.Op)tv));
        }

        public void fatarrow(Cursor m) {
            make(m, new Op.SimplePair(m, asstr(atk(m,"key")), ast<Op.Op>(atk(m,"val"))));
        }

        string[] whatever_precheck(Cursor m, Operator op, Op.Op[] args) {
            if (op != null && !op.whatever_curry())
                return new string[0];

            var vars = new List<string>();

            for (int i = 0; i < args.Length; i++) {
                if (args[i] == null) throw new NullReferenceException();
                var wc = args[i] as Op.WhateverCode;
                if (args[i] is Op.Whatever) {
                    var slot = job.gensym();
                    vars.Add(slot);
                    args[i] = new Op.Lexical(m, slot);
                } else if (wc != null) {
                    var pass = new Op.Op[wc.vars];
                    for (int j = 0; j < wc.vars; j++) {
                        var slot = job.gensym();
                        vars.Add(slot);
                        pass[j] = new Op.Lexical(m, slot);
                    }
                    args[i] = CompUtils.BetaCall(m, wc.slot, pass);
                }
            }

            return vars.ToArray();
        }

        Op.Op whatever_postcheck(Cursor m, string[] st, Op.Op term) {
            if (st.Length == 0)
                return term;

            var slot = job.gensym();

            var body = thunk_sub(term, st, null, Kernel.WhateverCodeMO);

            addlex(m, job.curlex, slot, new LISub(body));

            return new Op.WhateverCode(m, st.Length, slot);
        }

        // term :: Op.Op
        public void term__value(Cursor m) { from(m,"value"); }

        void common_name(Cursor m, SubInfo sub, STable pkg, string slot, string pname) {
            if (!pkg.who.Isa(Kernel.StashMO)) {
                sorry(m, "NYI usage of a nonstandard package");
                return;
            }
            string who = Kernel.UnboxAny<string>(pkg.who);
            string err = sub.unit.NsBind(who, pname, null, job.filename,
                CompUtils.LineOf(m));
            if (err != null) {
                sorry(m, err);
                return;
            }
            addlex(m, sub, slot, new LICommon((char)who.Length + who + pname));
        }

        Op.Op package_var(Cursor m, string slot, string name, object path) {
            trymop(m, () => {
                check_categorical(m, slot);
                var pkg = (path as STable) ?? compile_get_pkg(true,
                    (string[])path);
                common_name(m, job.curlex, pkg, slot, name);
                CompUtils.MarkUsed(m, slot);
            });
            return new Op.Lexical(m, slot);
        }

        public void term__name(Cursor m) {
            VarInfo nast = process_name(atk(m,"longname"), DEFER);
            var name = nast.name;
            var ind  = nast.ind;

            if (istrue(atk(m,"args"))) {
                if (name != null) name = "&" + name;
                if (ind != null) ind = Op.Helpers.mkstringycat(m,
                    new Op.StringLiteral(m, "&"), ind);
            }

            Op.Op term;
            if (ind != null) {
                term = new Op.IndirectVar(m, ind);
            }
            else if (nast.pkg != null) {
                term = package_var(m, job.gensym(), name, nast.pkg);
            }
            else if (is_pseudo_pkg(name)) {
                term = new Op.IndirectVar(m, new Op.StringLiteral(m, name));
            }
            else {
                term = new Op.Lexical(m, name);
            }

            var pcs = flist(atk(m,"postcircumfix"));
            var pci = 0;
            if (pcs.Length > 0 && asstr(pcs[0])[0] == '[') {
                term = Op.Helpers.mkcall(m, "&_param_role_inst",
                        Utils.PrependArr(ast<Operator.Function>(pcs[pci++]).postargs, term));
            }
            else if (istrue(atk(m,"args"))) {
                term = new Op.CallSub(m, term, false,
                        ast<Op.Op[]>(atk(m,"args")));
            }

            if (pcs.Length > pci) {
                term = ast<Operator>(pcs[pci]).with_args(m, term);
            }
            make(m, term);
        }

        Op.Op check_type_args(Cursor m, Op.Op term) {
            var pc = atk(m,"postcircumfix");
            if (istrue(pc)) term = Op.Helpers.mkcall(m, "&_param_role_inst",
                Utils.PrependArr(ast<Operator.Function>(pc).postargs, term));
            return term;
        }

        public void term__identifier(Cursor m) {
            var id = asstr(atk(m,"identifier"));
            var sal = ast<Op.Op[]>(atk(m,"args"));

            if (is_pseudo_pkg(id)) {
                make(m, check_type_args(m, new Op.IndirectVar(m,
                    new Op.StringLiteral(m, id))));
                return;
            }

            var isname = is_name(m, id);

            if (isname && asstr(atk(m,"args")) == "") {
                make(m, check_type_args(m, new Op.Lexical(m, id)));
                return;
            }

            var lex = new Op.Lexical(m, isname ? id : "&" + id);
            make(m, new Op.CallSub(m, check_type_args(m, lex), false, sal));
        }

        public void term__self(Cursor m) { make(m, new Op.Lexical(m,"self")); }
        public void term__circumfix(Cursor m) { from(m,"circumfix"); }
        public void term__scope_declarator(Cursor m) { from(m, "scope_declarator"); }
        public void term__multi_declarator(Cursor m) { from(m, "multi_declarator"); }
        public void term__package_declarator(Cursor m) { from(m, "package_declarator"); }
        public void term__routine_declarator(Cursor m) { from(m, "routine_declarator"); }
        public void term__regex_declarator(Cursor m) { from(m, "regex_declarator"); }
        public void term__type_declarator(Cursor m) { from(m, "type_declarator"); }
        public void term__dotty(Cursor m) {
            make(m, ast<Operator>(atk(m,"dotty")).with_args(m, new Op.Lexical(m,"$_")));
        }
        public void term__capterm(Cursor m) { from(m, "capterm"); }
        public void term__sigterm(Cursor m) { from(m, "sigterm"); }
        public void term__statement_prefix(Cursor m) { from(m, "statement_prefix"); }
        public void term__variable(Cursor m) {
            make(m, do_variable_reference(m, ast<VarInfo>(atk(m,"variable"))));
        }
        public void term__2e2e2e(Cursor m) { make(m,new Op.Yada(m,"...")); }
        public void term__5f5f5f(Cursor m) { make(m,new Op.Yada(m,"???")); }
        public void term__212121(Cursor m) { make(m,new Op.Yada(m,"!!!")); }
        public void term__2a(Cursor m) { make(m,new Op.Whatever(m)); } // *
        public void term__lambda(Cursor m) {
            make(m, block_expr(m, ast<SubInfo>(atk(m,"pblock"))));
        }

        public void term__colonpair(Cursor m) {
            Op.Op[] cps = flist_ast<Op.Op>(atk(m,"colonpair"));
            if (cps.Length > 1)
                sorry(m,"Multi colonpair syntax not yet understood"); // XXX
            make(m,cps[0]);
        }

        public void term__fatarrow(Cursor m) { from(m,"fatarrow"); }

        public void term__reduce(Cursor m) {
            var assoc = asstr(atk(atk(atk(m,"op"),"O"),"assoc"));
            make(m, new Op.CallSub(m, new Op.Lexical(m,"&reduceop"), false,
                Utils.PrependArr(ast<Op.Op[]>(atk(m,"args")), 0, new [] {
                    Op.Helpers.mkbool(m, asstr(atk(m,"triangle")) != ""),
                    Op.Helpers.mkbool(m, assoc == "list"),
                    Op.Helpers.mkbool(m, assoc == "right"),
                    Op.Helpers.mkbool(m, assoc == "chain"),
                    ast<Operator>(atk(m,"op")).as_function(m) })));
        }

        bool check_strict() {
            for (var s = job.curlex; s != null; s = s.outer) {
                var l = s.GetExtend0("strict");
                if (l != null) return (bool)l;
            }
            return true;
        }

// check_variable($M)
//   $M is <variable> (desigilname, method for @$foo, .$var indir), metachar:var,
//         ...
//   $M is anon(sigil,twigil,desigilname) or anon(sigil<desigilname>) (colonpair)
//   $M is synthetic(<longname>::<postcircumfix>) (term:name)
// check_variable should handle ALL of the possible sorries resulting from
// a referential variable use.  Even term:variable is too early, since we may
// backtrack if $*QSIGIL ne '$' and no posfix.
//
// I don't like the way this is factored, since do_variable_reference has to
// redo a lot of the same scanning.
        void check_variable(Variable variable) {
            if (variable == null) return;
            VarInfo vast = ast<VarInfo>(variable);
            if (vast.term != null) return; // pseudo-var
            string name = asstr(variable);

            Cursor here = ((Cursor)variable.Fetch());
            here = here.UnMatch().At(here.from);

            if (!vast.@checked)
                sorry(here, "do_variable_reference must always precede check_variable");

            switch (vast.twigil) {
                case '\0':
                    if (job.in_decl || vast.name == null ||
                            vast.name[0] < 'A' || vast.name[0] == '¢' ||
                            is_known(here, name) || vast.pkg != null) {
                        CompUtils.MarkUsed(here, name);
                        return;
                    }
                    if (vast.sigil == '&') {
                        add_mystery(here);
                        return;
                    } else if (name == "@_" || name == "%_") {
                        add_placeholder(here, name);
                        return;
                    } else if (!check_strict()) {
                        return;
                    } else { // guaranteed fail now
                        var scope = job.get_memo(here.pos, "declend");
                        if (scope != null) {
                            sorry(here, "Variable {0} is not predeclared (declarators are tighter than comma, so maybe your '{1}' signature needs pars?)", name, asstr(scope));
                            return;
                        } else if (is_known(here, "@"+vast.name)) {
                            sorry(here,"Variable {0} is not predeclared (did you mean @{1}?)", name, vast.name);
                            return;
                        } else if (is_known(here, "%"+vast.name)) {
                            sorry(here,"Variable {0} is not predeclared (did you mean %{1}?)", name, vast.name);
                            return;
                        }
                        sorry(here,"Variable {0} is not predeclared", name);
                        return;
                    }

                case '!':
                    if (job.has_self == "") // XXX to be replaced by MOP queries
                        sorry(here, "Variable {0} used where no 'self' is available", name);
                    return;

                case '.':
                    if (job.has_self == "") // XXX to be replaced by MOP queries
                        sorry(here, "Virtual call {0} used where no 'self' is available", name);
                    if (job.has_self == "partial")
                        sorry(here, "Virtual call {0} may not be used on partially constructed object", name);
                    return;

                case '^':
                case ':':
                    add_placeholder(here, name);
                    return;

                case '~':
                    return;

                case '?':
                    if (name.IndexOf("::") >= 0) {
                        // TODO: $?CALLER::x makes sense! also CONTEXT, OUTER, MY, SETTING, CORE
                        worry(here, "Unrecognized variable: {0}", name);
                    } else {
                        if (name == "$?LINE" || name == "$?POSITION" || name == "&?BLOCK" || name == "&?ROUTINE")
                            return;
                        if (lookup_lex(job.curlex, name) != null)
                            return;
                        sorry(here, "Unrecognized variable: {0}", name);
                    }
                    return;
            }
        }

        Op.Op do_variable_reference(Cursor m, VarInfo v) {
            v.@checked = true;
            if (v.term != null)
                return v.term;

            var tw = v.twigil;
            var sl = v.sigil + (v.twigil != '\0' ? new string(v.twigil,1) : "") + v.name;

            if (v.pkg != null && "*=~?^:".IndexOf(v.twigil) >= 0) {
                sorry(m, "Twigil {0} cannot be used with qualified names", tw);
                return new Op.StatementList(m);
            }

            if (tw == '!') {
                STable pclass;
                if (v.pkg != null) {
                    pclass = v.pkg;
                } else if (lookup_lex(job.curlex, sl) != null) {
                    return new Op.Lexical(m, sl);
                } else if (job.curlex.in_class != null) {
                    pclass = job.curlex.in_class;
                } else {
                    sorry(m, "Cannot resolve class for private attribute");
                    return new Op.StatementList(m);
                }
                if (!pclass.Trusts(job.curlex.cur_pkg)) {
                    sorry(m, "Cannot call private method '{0}' on {1} because it does not trust {2}", v.name, pclass.name, job.curlex.cur_pkg.name);
                    return new Op.StatementList(m);
                }
                return new Op.GetSlot(m, new Op.Lexical(m, "self"), sl, pclass);
            }
            else if (tw == '.') {
                if (v.pkg != null)
                    sorry(m, "$.Foo::bar syntax NYI");

                return docontext(m, v.sigil, new Op.CallMethod(m, v.name,
                    new Op.Lexical(m, "self")));
            }
            // no twigil in lex name for these
            else if (tw == '^' || tw == ':') {
                return new Op.Lexical(m, v.sigil + v.name);
            }
            else if (tw == '*') {
                return new Op.ContextVar(m, sl, 0);
            }
            else if (tw == '\0' || tw == '?') {
                if (v.pkg != null) {
                    return package_var(m, job.gensym(), sl, v.pkg);
                } else if (tw == '?' && sl == "$?POSITION") {
                    return Op.Helpers.mkcall(m, "&infix:<..^>",
                        new Op.Num(m, new object[] { 10, m.from.ToString() }),
                        new Op.Num(m, new object[] { 10, m.pos.ToString() }));
                } else if (tw == '?' && sl == "$?LINE") {
                    return new Op.Num(m, new object[] { 10,
                        CompUtils.LineOf(m).ToString() });
                } else if (tw == '?' && sl == "$?FILE") {
                    return new Op.StringLiteral(m, job.filename);
                } else if (tw == '?' && sl == "$?ORIG") {
                    return new Op.StringLiteral(m, job.source);
                } else if (tw == '?' && sl == "&?BLOCK") {
                    job.curlex.special |= SubInfo.CANNOT_INLINE;
                    return new Op.GetBlock(m, false);
                } else if (tw == '?' && sl == "&?ROUTINE") {
                    job.curlex.special |= SubInfo.CANNOT_INLINE;
                    return new Op.GetBlock(m, true);
                } else {
                    return new Op.Lexical(m, sl);
                }
            }
            else {
                sorry(m, "Unhandled reference twigil {0}", tw);
                return new Op.StatementList(m);
            }
        }

        Op.Op docontext(Cursor m, char sigil, Op.Op term) {
            if ("$@%&".IndexOf(sigil) < 0)
                sorry(m, "Unhandled context character {0}", sigil);

            var method = (sigil == '$' || sigil == '&') ? "item" :
                sigil == '@' ? "list" : "hash";

            return new Op.Builtin(m, method, term);
        }

        Op.Op docontextif(Cursor m, char sigil, Op.Op op) {
            return sigil == '$' ? op : docontext(m,sigil,op);
        }

        public void variable(Cursor m) {
            char sigil = istrue(atk(m,"sigil")) ? asstr(atk(m,"sigil"))[0] :
                asstr(m)[0];
            char twigil = istrue(atk(m,"twigil")) ? asstr(atk(m,"twigil"))[0] :
                '\0';
            var twigil_s = twigil == (char)0 ? "" : new string(twigil,1);

            string name = null;
            STable pkg  = null;
            Op.Op  term = null;

            VarInfo dsosl = istrue(atk(m,"desigilname")) ?
                ast<VarInfo>(atk(m,"desigilname")) :
                istrue(atk(m,"sublongname")) ?
                    ast<VarInfo>(atk(m,"sublongname")) :
                    istrue(atk(m,"longname")) ?
                        process_name(atk(m,"longname"), DEFER) : new VarInfo();

            if (dsosl.term != null) {
                term = docontext(m, sigil, dsosl.term);
            }
            else if (dsosl.ind != null) {
                term = new Op.IndirectVar(m, Op.Helpers.mkstringycat(m,
                    new Op.StringLiteral(m, sigil + twigil_s), dsosl.ind));
            }
            else if (twigil == '.' && istrue(atk(m,"postcircumfix"))) {
                if (dsosl.pkg != null)
                    sorry(m, "$.Foo::bar syntax NYI");

                term = docontext(m, sigil, new Op.CallMethod(m, dsosl.name,
                    new Op.Lexical(m, "self"), false,
                    flist_ast<Operator.PostCall>(atk(m,"postcircumfix"))[0].arglist));
            }
            else if (dsosl.name != null) {
                name = dsosl.name;
                pkg = dsosl.pkg;
            }
            else if (istrue(atk(m,"infixish"))) {
                term = ast<Operator>(atk(m,"infixish")).as_function(m);
            }
            else if (istrue(atk(m,"special_variable"))) {
                name = asstr(atk(m, "special_variable")).Substring(1);
            }
            else if (istrue(atk(m,"index"))) {
                var ix = ast<P6any>(atk(m,"index"));
                make(m, new VarInfo { capid = asstr(ix), term = docontextif(m,
                    sigil, Op.Helpers.mkcall(m, "&postcircumfix:<[ ]>",
                        new Op.Lexical(m, "$/"), new Op.Const(m, ix))) });
                return;
            }
            else if (istrue(atk(m,"postcircumfix")) && twigil != '.') {
                var pc = (Cursor)flist(atk(m,"postcircumfix"))[0].Fetch();
                if (pc.reduced == "postcircumfix:sym<< >>") { // XXX fiddly
                    var args = ast<Operator.Function>(pc).postargs;
                    make(m, new VarInfo { capid = asstr(eval_ast(m, args[0])),
                        term = docontextif(m, sigil, Op.Helpers.mkcall(m,
                            "&postcircumfix:<{ }>", Utils.PrependArr(args,
                                new Op.Lexical(m,"$/")))) });
                    return;
                }
                else {
                    var args = ast<Operator.PostCall>(pc).arglist;
                    if (args.Length > 0) {
                        term = docontext(m, sigil, args[0]);
                    } else if (sigil == '$') {
                        term = new Op.ShortCircuit(m, Op.ShortCircuit.DOR,
                            new Op.CallMethod(m,"ast", new Op.Lexical(m,"$/")),
                            new Op.CallMethod(m,"Str", new Op.Lexical(m,"$/")));
                    } else if (sigil == '@' || sigil == '%') {
                        term = docontext(m, sigil, new Op.Lexical(m,"$/"));
                    } else {
                        term = new Op.Lexical(m,"Mu");
                        sorry(m, "Missing argument for contextualizer");
                    }
                }
            }
            else {
                name = "";
            }

            make(m, term != null ? new VarInfo { term = term } :
                new VarInfo { sigil = sigil, twigil = twigil, name = name,
                    pkg = pkg });
        }

        // forward...
        Op.Op[] extract_rx_adverbs(bool a, bool b, Variable c) { throw new NotImplementedException(); }
        void check_categorical(Cursor m, string name) { throw new NotImplementedException(); }
        bool is_name(Cursor m, string name) { throw new NotImplementedException(); }
        bool is_known(Cursor m, string name) { throw new NotImplementedException(); }
        void add_placeholder(Cursor m, string name) { throw new NotImplementedException(); }
        void add_mystery(Cursor m) { throw new NotImplementedException(); }


        internal Op.Op block_expr(Cursor m, SubInfo blk) {
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

        internal void finish(SubInfo n, Op.Op code, bool done = false) {
            if (!done) code = code.simplify(n);
            if (code.onlystub()) n.SetExtend("onlystub", true);
            // serialize cgops
            n.nam_str = code.cgop(n).ToString(out n.nam_refs);
            n.code = RuntimeUnit.JitCompileSub;
            if (n.protopad != null)
                n.protopad.code = n.code;
        }

        internal void finish_dispatcher(SubInfo s, string k) {
            if (k == "regex")
                s.code = Lexer.StandardProtoC;
            else if (k == "multi")
                s.code = Kernel.StandardTypeProtoC;
            else
                throw new Exception("Unknown dispatcher type " + k);
            if (s.protopad != null)
                s.protopad.code = s.code;
        }
    }
}
