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
        void sorry(string fmt, params object[] args) {
            throw new NotImplementedException();
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
        P6any from_base(Variable str, int based_) {
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
                    sorry("Digit <{0}> too large for radix {1}", ch, based_);
                acc = plus(mkint(digit), mul(based, acc));
            }
            return punto < 0 ? acc : div(acc, pow(based, mkint(punto)));
        }

        public void decint(Cursor m) { make(m,from_base(m, 10)); }
        public void hexint(Cursor m) { make(m,from_base(m, 16)); }
        public void octint(Cursor m) { make(m,from_base(m, 8)); }
        public void binint(Cursor m) { make(m,from_base(m, 2)); }
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
            } else make(m,from_base(m,10));
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
            var value = istrue(f=atk(m,"int")) ? from_base(f,radix) : mkint(0);

            if (istrue(f = atk(m,"frac"))) {
                var shift = asstr(f).Replace("_","").Length;
                value = plus(value, div(from_base(f, radix),
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
    }
}
