using System;
using System.Collections.Generic;

using Niecza;
using Niecza.Compiler;

namespace Niecza.Compiler.RxOp {
    abstract class RxOp {
        internal RxOp[] zyg;

        public RxOp() { zyg = new RxOp[0]; }
        public RxOp(RxOp zyg) { this.zyg = new [] { zyg }; }
        public RxOp(RxOp[] zyg) { this.zyg = zyg; }

        public virtual RxOp uncut() { return this; }
        public virtual void tocclist(List<CClass> a) { a.Add(null); }

        public virtual void VisitOps(Func<Op.Op,Op.Op> post) {
            foreach (RxOp z in zyg) z.VisitOps(post);
        }

        public abstract void code(SubInfo body, List<object> acc);
        public abstract object[] lad();

        // add your caps to the accumulator.  Only 0-1-infty matters.
        public virtual void used_caps(int quant, Dictionary<string,int> acc) {
            foreach (RxOp z in zyg) z.used_caps(quant, acc);
        }

        protected string label() {
            return "b" + (Kernel.containerRootUnit.nextid++);
        }
    }

    abstract class Capturing : RxOp {
        internal string[] captures;

        public Capturing(RxOp[] zyg) : base(zyg) { captures = new string[0]; }
        public Capturing() : this(new RxOp[0]) { }

        public abstract Capturing withcaps(string[] newcaps);

        public override void used_caps(int quant, Dictionary<string,int> acc) {
            foreach (string s in captures)
                acc[s] = acc.GetDefault(s, 0) + quant;
            base.used_caps(quant, acc);
        }
    }

    class Sym : Capturing {
        internal string text;
        internal string endsym;
        internal bool igcase;

        public Sym(string text, string endsym, bool igcase, bool igmark) {
            this.text = text; this.endsym = endsym; this.igcase = igcase;
        }

        public override Capturing withcaps(string[] newcaps) {
            var c = new Sym(text, endsym, igcase, false);
            c.captures = newcaps;
            return c;
        }

        public override void code(SubInfo body, List<object> acc) {
            // We aren't going to make a real Match unless somebody comes
            // up with a good reason...
            acc.Add(CgOp.rxpushcapture(CgOp.string_var(text), captures));
            var ic = igcase ? "NoCase" : "";
            acc.Add(text.Length == 1 ?
                CgOp.rxbprim("ExactOne"+ic, CgOp.@char(text[0])) :
                CgOp.rxbprim("Exact"+ic, CgOp.str(text)));
            if (endsym != null)
                (new Subrule(endsym, true)).code(body, acc);
        }

        public override void tocclist(List<CClass> acc) {
            for (int i = 0; i < text.Length; i++)
                acc.Add(CClass.range((int)text[i], (int)text[i]));
        }

        public override object[] lad() {
            var m = new [] { igcase ? "StrNoCase": "Str", text };
            return endsym != null ? new object[] { "Sequence",
                new [] { m, new [] { "Method", endsym } } } : m;
        }
    }

    class String : RxOp {
        string text;
        bool igcase;

        public String(string text, bool igcase=false) {
            this.text = text; this.igcase = igcase;
        }

        public override void code(SubInfo body, List<object> acc) {
            var ic = igcase ? "NoCase" : "";
            acc.Add(text.Length == 1 ?
                CgOp.rxbprim("ExactOne"+ic, CgOp.@char(text)) :
                CgOp.rxbprim("Exact"+ic, CgOp.str(text)));
        }

        public override void tocclist(List<CClass> acc) {
            for (int i = 0; i < text.Length; i++)
                acc.Add(CClass.range((int)text[i], (int)text[i]));
        }

        public override object[] lad() {
            return new [] { igcase ? "StrNoCase": "Str", text };
        }
    }

    class VarString : RxOp {
        Op.Op ops;
        public VarString(Op.Op ops) { this.ops = ops; }

        public override void VisitOps(Func<Op.Op,Op.Op> post) {
            ops = ops.VisitOps(post);
        }

        public override void code(SubInfo body, List<object> acc) {
            acc.Add(CgOp.rxbprim("Exact", CgOp.obj_getstr(ops.cgop(body))));
        }

        public override object[] lad() { return new[] { "Imp" }; }
    }

    class Quantifier : RxOp {
        int min;
        int max;
        Op.Op closure;
        bool minimal, nonlisty, opsep;

        public Quantifier(RxOp[] zyg, int min, int max, Op.Op closure,
                bool minimal, bool nonlisty, bool opsep) : base(zyg) {
            this.min = min; this.max = max; this.closure = closure;
            this.minimal = minimal; this.nonlisty = nonlisty;
            this.opsep = opsep;
        }

        public override void VisitOps(Func<Op.Op,Op.Op> post) {
            if (closure != null) closure = closure.VisitOps(post);
        }

        public override void used_caps(int quant, Dictionary<string,int> acc) {
            base.used_caps(nonlisty ? quant : 2, acc);
        }

        void exitpt(string label, CgOp cond, string exit, List<object> to) {
            if (minimal) {
                to.Add(CgOp.ternary(cond,
                    CgOp.prog(CgOp.rxpushb("QUANT", label),
                        CgOp.@goto(exit)), CgOp.prog()));
            } else {
                to.Add(CgOp.ternary(cond,
                    CgOp.rxpushb("QUANT", exit), CgOp.prog()));
            }
        }

        public override void code(SubInfo body, List<object> acc) {
            var rmin = closure != null ? CgOp.letvar("!min") : CgOp.@int(min);
            var rmax = closure != null ? CgOp.letvar("!max") : CgOp.@int(max);

            var exit   = label();
            var repeat = label();
            var sep    = label();

            List<object> to = closure != null ? new List<object>() : acc;

            if (closure != null || max < 0) to.Add(CgOp.cgoto("backtrack",
                CgOp.compare(">", CgOp.@int(0), rmax)));

            to.Add(CgOp.rxopenquant());

            // Allow 0-time exit matching null string
            exitpt(repeat, CgOp.compare("<",rmin,CgOp.@int(1)), exit,to);

            // We have to match something now
            to.Add(CgOp.label(repeat));
            to.Add(CgOp.cgoto("backtrack",
                CgOp.compare(">=", CgOp.rxgetquant(), rmax)));
            zyg[0].code(body, to);
            to.Add(CgOp.rxincquant());

            if (zyg.Length > 1) {
                exitpt(sep, CgOp.compare(">=",CgOp.rxgetquant(),rmin), exit,to);

                to.Add(CgOp.label(sep));
                zyg[1].code(body,to);

                // Allow exiting here if a trailing separator is allowed
                if (opsep) exitpt(repeat,
                        CgOp.compare(">=", CgOp.rxgetquant(), rmin), exit, to);
            } else {
                exitpt(repeat, CgOp.compare(">=",CgOp.rxgetquant(),rmin),exit,to);
            }

            to.Add(CgOp.@goto(repeat));
            to.Add(CgOp.label(exit));
            to.Add(CgOp.sink(CgOp.rxclosequant()));

            if (closure != null) {
                acc.Add(CgOp.letn(
                    "!range", closure.cgop(body),
                    "!min",   CgOp.cast("int", CgOp.obj_getnum(CgOp.methodcall(
                        CgOp.letvar("!range"), "niecza_quantifier_min"))),
                    "!max",   CgOp.cast("int", CgOp.obj_getnum(CgOp.methodcall(
                        CgOp.letvar("!range"), "niecza_quantifier_max"))),
                    CgOp.prog(to.ToArray())));
            }
        }

        public override object[] lad() {
            if (minimal || closure != null) return new [] { "Imp" };

            int mode = 0;
            if (min <= 0) mode += 1;
            if (max > 1) mode += 2;
            if (opsep) mode += 4;

            return zyg.Length > 1 ?
                new object[] { "Quant", mode, zyg[0].lad(), zyg[1].lad() } :
                new object[] { "Quant", mode, zyg[0].lad() };
        }
    }

    class Sequence : RxOp {
        public Sequence(RxOp[] z) : base(z) { }

        public override void code(SubInfo body, List<object> acc) {
            foreach (RxOp z in zyg) z.code(body, acc);
        }

        public override void tocclist(List<CClass> acc) {
            foreach (RxOp z in zyg) z.tocclist(acc);
        }

        public override object[] lad() {
            object[] ar = new object[zyg.Length];
            for (int i = 0; i < zyg.Length; i++)
                ar[i] = zyg[i].lad();
            return new object[] { "Sequence", ar };
        }
    }

    class Conj : RxOp {
        public Conj(RxOp[] z) : base(z) { }

        public override void code(SubInfo body, List<object> a) {
            if (zyg.Length == 0) return;

            for (int i = 0; i < zyg.Length; i++) {
                a.Add(CgOp.rxcall(i == 0 ? "PushConjStart" : "GotoConjStart"));
                zyg[i].code(body, a);
                a.Add(CgOp.rxcall(i == 0 ? "PushConjEnd" : "CheckConjEnd"));
            }

            a.Add(CgOp.rxcall("EndConj"));
        }

        public override object[] lad() { return new [] { "Imp" }; }
    }

    abstract class AltBase : RxOp {
        internal string dba;

        public AltBase(RxOp[] z, string d) : base(z) { dba = d; }

        public override void used_caps(int quant, Dictionary<string,int> acc) {
            var d1 = new Dictionary<string,int>();
            var d2 = new Dictionary<string,int>();

            foreach (RxOp z in zyg) {
                d2.Clear();
                z.used_caps(quant, d2);
                foreach (KeyValuePair<string,int> kv in d2)
                    if (kv.Value > d1.GetDefault(kv.Key,0))
                        d1[kv.Key] = kv.Value;
            }

            foreach (KeyValuePair<string,int> kv in d1)
                acc[kv.Key] = acc.GetDefault(kv.Key,0) + kv.Value;
        }
    }

    class SeqAlt : AltBase {
        public SeqAlt(RxOp[] z, string d) : base(z,d) { }

        public override void code(SubInfo body, List<object> acc) {
            var final = label();
            var n = zyg.Length;

            for (int i = 0; i < n; i++) {
                var end = (i == n-1) ? final : label();
                if (i != n-1) acc.Add(CgOp.rxpushb("SEQALT", end));
                zyg[i].code(body, acc);
                if (i != n-1) acc.Add(CgOp.@goto(final));
                acc.Add(CgOp.label(end));
            }
        }

        public override object[] lad() {
            return zyg.Length == 0 ? new[] {"Imp"} : zyg[0].lad();
        }
    }

    class ConfineLang : RxOp {
        // Note that BRACK automatically confines the language change
        public ConfineLang(RxOp z) : base(z) { }
        public override void code(SubInfo body, List<object> acc) {
            acc.Add(CgOp.pushcut("BRACK"));
            zyg[0].code(body, acc);
            acc.Add(CgOp.popcut());
        }
        public override object[] lad() { return zyg[0].lad(); }
    }

    class Cut : RxOp {
        public Cut(RxOp z) : base(z) { }
        public override RxOp uncut() { return zyg[0]; }
        public override void tocclist(List<CClass> to) { zyg[0].tocclist(to); }
        public override object[] lad() { return zyg[0].lad(); }

        public override void code(SubInfo body, List<object> to) {
            to.Add(CgOp.pushcut("CUTGRP"));
            zyg[0].code(body, to);
            to.Add(CgOp.rxcommitgroup(CgOp.str("CUTGRP")));
            to.Add(CgOp.popcut());
        }
    }

    class Subrule : RxOp {
        public Subrule(params object[] args) { }
        public override object[] lad() { return null; }
        public override void code(SubInfo b, List<object> to) { }
    }
}
