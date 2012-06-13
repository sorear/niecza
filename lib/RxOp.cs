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

        public virtual void oplift(List<Op.Op> post) {
            foreach (RxOp z in zyg) z.oplift(post);
        }

        public abstract void code(SubInfo body, List<object> acc);
        // synthetic node types need not implement lad()
        public virtual object[] lad() {
            throw new NotImplementedException();
        }

        // add your caps to the accumulator.  Only 0-1-infty matters.
        public virtual void used_caps(int quant, Dictionary<string,int> acc) {
            foreach (RxOp z in zyg) z.used_caps(quant, acc);
        }

        protected string label() { return "b" + CompJob.cur.genid(); }

        // placeholders...
        protected void AddMyName(params object[] args) { throw new NotImplementedException(); }

        public static object[] optimize_lad(object[] lad) {
            if ((string)lad[0] != "Sequence")
                return lad;
            var zyg = (object[][])lad[1];
            var fzyg = new List<object[]>();
            foreach(object[] z in zyg) {
                var oz = optimize_lad(z);
                if ((string)oz[0] == "Sequence") {
                    foreach (var zz in (object[][])oz[1])
                        fzyg.Add(zz);
                } else if ((string)oz[0] == "Null") {
                } else {
                    fzyg.Add(oz);
                }
            }
            var ozyg = new List<object[]>();
            foreach (var fz in fzyg) {
                if ((string)fz[0] == "None") return fz;
                if ((string)fz[0] == "Imp") {
                    ozyg.Add(fz);
                    break;
                } else if (ozyg.Count != 0 && (string)ozyg[ozyg.Count - 1][0] == "Str" && (string)fz[0] == "Str") {
                    ozyg[ozyg.Count-1] = new [] { "Str", ((string)ozyg[ozyg.Count-1][1]) + ((string)fz[1]) };
                } else {
                    ozyg.Add(fz);
                }
            }
            if (ozyg.Count == 0) {
                return new object[] { "Null" };
            } else if (ozyg.Count == 1) {
                return ozyg[0];
            } else {
                return new object[] { "Sequence", ozyg.ToArray() };
            }
        }

        public virtual bool mayback() { return true; }
        public virtual RxOp rxsimp(bool cut) {
            for (int i = 0; i < zyg.Length; i++)
                zyg[i] = zyg[i].rxsimp(false);
            return this;
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
        public override bool mayback() { return false; }
    }

    class String : RxOp {
        internal string text;
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
        public override bool mayback() { return false; }
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

        public override bool mayback() { return false; }
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

        public override RxOp rxsimp(bool cut) {
            base.rxsimp(cut);
            if (cut && zyg.Length == 1 && zyg[0] is CClassElem)
                return new QuantCClass(((CClassElem)zyg[0]).cc, min, max);
            return this;
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
        public Sequence(RxOp[] z = null) : base(z ?? new RxOp[0]) { }

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

        public override RxOp rxsimp(bool cut) {
            var kids = new List<RxOp>();
            for (int i = 0; i < zyg.Length; i++) {
                var k = zyg[i].rxsimp(cut && (i == zyg.Length - 1));
                if (k is Sequence) {
                    foreach (var kz in k.zyg) kids.Add(kz);
                } else {
                    kids.Add(k);
                }
            }
            return (kids.Count == 1) ? kids[0] : new Sequence(kids.ToArray());
        }

        public override bool mayback() {
            foreach (RxOp z in zyg) if (z.mayback()) return true;
            return false;
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
        public override bool mayback() { return zyg[0].mayback(); }
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

        public override RxOp rxsimp(bool cut) {
            var kid = zyg[0].rxsimp(true);
            return kid.mayback() ? new Cut(kid) : kid;
        }
        public override bool mayback() { return false; }
    }

    class BeforeString : RxOp {
        internal string text;
        public BeforeString(string s) { text = s; }

        public override void code(SubInfo sub, List<object> to) {
            to.Add(CgOp.rxbprim("BeforeStr", CgOp.@bool(0), CgOp.str(text)));
        }
        public override bool mayback() { return false; }
    }

    class ZeroWidthCCs : RxOp {
        CClass[] ccs;
        bool after, neg;

        public ZeroWidthCCs(CClass[] c, bool a, bool n) { ccs=c;after=a;neg=n;}

        public override object[] lad() { return new [] { "Null" }; }

        public override void code(SubInfo body, List<object> to) {
            to.Add(CgOp.rxbprim(after ? "AfterCCs" : "BeforeCCs",
                CgOp.@bool(neg?1:0), CgOp.@const(ccs)));
        }
        public ZeroWidthCCs zerowidthify(bool n) { return new ZeroWidthCCs(ccs, after, !neg); }
        public override bool mayback() { return false; }
    }

    class NotBeforeString : RxOp {
        internal string str;
        public NotBeforeString(string s) { str = s; }

        public override void code(SubInfo body, List<object> to) {
            to.Add(CgOp.rxbprim("BeforeStr", CgOp.@bool(1), CgOp.str(str)));
        }
        public override bool mayback() { return false; }
    }

    class ZeroWidth : RxOp {
        int type;
        public ZeroWidth(int type) { this.type = type; }

        public override object[] lad() { return new [] { "Null" }; }
        public override void code(SubInfo body, List<object> to) {
            to.Add(CgOp.rxbprim("ZeroWidth", CgOp.@int(type)));
        }
        public override bool mayback() { return false; }
    }

    class NotBefore : RxOp {
        public NotBefore(RxOp z) : base(z) { }
        public override object[] lad() { return new [] { "Null" }; }

        public override void code(SubInfo body, List<object> to) {
            var pass = label();
            to.Add(CgOp.pushcut("NOTBEFORE"));
            to.Add(CgOp.rxpushb("NOTBEFORE", pass));
            zyg[0].code(body, to);
            to.Add(CgOp.rxcall("CommitGroup", CgOp.str("NOTBEFORE")));
            to.Add(CgOp.@goto("backtrack"));
            to.Add(CgOp.label(pass));
            to.Add(CgOp.popcut());
        }

        public override bool mayback() { return false; }
        public override RxOp rxsimp(bool c) {
            var z = zyg[0].rxsimp(true);
            if (z is BeforeString)
                return new NotBeforeString(((BeforeString)z).text);
            if (z is Before)
                return new NotBefore(z.zyg[0]);
            if (z is String)
                return new NotBeforeString(((String)z).text);
            if (z is Subrule)
                return ((Subrule)z).zerowidthify(true);
            if (z is ZeroWidthCCs)
                return ((ZeroWidthCCs)z).zerowidthify(true);
            if (z is CClassElem)
                return new ZeroWidthCCs(new [] { ((CClassElem)z).cc }, false, true);
            return new NotBefore(z);
        }
    }

    class Before : RxOp {
        public Before(RxOp z) : base(z) { }
        public override object[] lad() {
            return new object[] { "Sequence", new [] { zyg[0].lad(), new [] { "Imp" } } };
        }
        public override void code(SubInfo body, List<object> to) {
            (new NotBefore(new NotBefore(zyg[0]))).code(body,to);
        }
        public override bool mayback() { return false; }
        // it's not unusual to write <!before> and <?before>
        public override RxOp rxsimp(bool c) {
            var z = zyg[0].rxsimp(true);
            if (z is BeforeString || z is ZeroWidthCCs || z is ZeroWidth ||
                    z is Before || z is NotBefore)
                return z;
            if (z is String)
                return new BeforeString(((String)z).text);
            if (z is Subrule)
                return ((Subrule)z).zerowidthify(false);
            if (z is CClassElem)
                return new ZeroWidthCCs(new [] { ((CClassElem)z).cc }, false, false);
            return new Before(z);
        }
    }

    class Tilde : RxOp {
        string closer, dba;

        public Tilde(RxOp z, string c, string d) : base(z) { closer=c; dba=d; }

        public override void code(SubInfo body, List<object> to) {
            var fail = label();
            var pass = label();

            if (!body.dylex.ContainsKey("$*GOAL"))
                AddMyName(body, "$*GOAL");

            to.Add(CgOp.rxcall("PushGoal", CgOp.callframe(), CgOp.str(closer)));
            zyg[0].code(body, to);
            to.Add(CgOp.rxpushb("TILDE", fail));
            to.Add(CgOp.rxbprim("Exact", CgOp.str(closer)));
            to.Add(CgOp.@goto(pass));
            to.Add(CgOp.label(fail));
            to.Add(CgOp.sink(CgOp.methodcall(CgOp.rxcall("MakeCursor"),
                "FAILGOAL", CgOp.string_var(closer), CgOp.string_var(dba),
                CgOp.box("Int", CgOp.rxgetquant()))));
            to.Add(CgOp.label(pass));
            to.Add(CgOp.rxcall("PopGoal", CgOp.callframe()));
        }

        public override object[] lad() { return new [] { "Imp" }; }
    }

    class Subrule : Capturing {
        string method;
        Op.Op regex;
        object[] ltm;
        bool selfcut, zerowidth, negative;

        Subrule(string m, Op.Op r, object[] l, bool s, bool z, bool n) {
            method=m; regex=r; ltm=l; selfcut=s; zerowidth=z; negative=n;
        }
        public Subrule(string method, bool selfcut) { this.method = method; this.selfcut = selfcut; }
        public Subrule(Op.Op regex, object[] lad) { this.regex=regex; this.ltm=lad; }

        public override RxOp rxsimp(bool cut) {
            return cut ? new Subrule(method,regex,ltm,true,zerowidth,negative) : this;
        }
        public override bool mayback() { return !selfcut; }

        public override void VisitOps(Func<Op.Op,Op.Op> post) {
            if (regex != null) regex = regex.VisitOps(post);
        }

        public override Capturing withcaps(string[] caps) {
            var n = new Subrule(method,regex,ltm,selfcut,zerowidth,negative);
            n.captures = caps;
            return n;
        }

        internal RxOp zerowidthify(bool negate) {
            return new Subrule(method,regex,ltm,selfcut,true,negate?!negative:negative);
        }

        public override void code(SubInfo body, List<object> to) {
            var callf = regex != null ? regex.cgop(body) :
                CgOp.methodcall(CgOp.rxcall("MakeCursorV"), method);

            if (selfcut) {
                to.Add(CgOp.rxincorpcut(captures, zerowidth?1:0,
                    negative?1:0, callf));
            } else {
                var bt = label();

                to.Add(CgOp.rxcall("InitCursorList", callf));
                to.Add(CgOp.label(bt));
                to.Add(CgOp.rxincorpshift(captures, bt));
            }
        }

        public override object[] lad() {
            return ltm ?? (method != null ? new [] { "Method", method } : new [] { "Imp" });
        }
    }

    class Sigspace : RxOp {
        bool selfcut;
        public Sigspace(bool c = false) { selfcut = c; }

        public override object[] lad() { return new [] { "Null" }; }

        public override void code(SubInfo body, List<object> to){
            (new Subrule("ws", selfcut)).code(body, to);
        }

        public override RxOp rxsimp(bool cut) {
            return cut ? new Sigspace(true) : this;
        }
        public override bool mayback() { return !selfcut; }
    }

    class CutLTM : RxOp {
        public override object[] lad() { return new [] { "Imp" }; } // special
        public override void code(SubInfo s, List<object> to) {
            to.Add(CgOp.rxcall("CommitGroup", CgOp.str("LTM")));
        }
        public override bool mayback() { return false; }
    }

    class CutRule : RxOp {
        public override object[] lad() { return new [] { "Null" }; }
        public override void code(SubInfo s, List<object> to) {
            to.Add(CgOp.rxcall("CommitRule"));
        }
        public override bool mayback() { return false; }
    }

    class CutBrack : RxOp {
        public override object[] lad() { return new [] { "Null" }; }
        public override void code(SubInfo s, List<object> to) {
            to.Add(CgOp.rxcall("CommitGroup", CgOp.str("BRACK")));
        }
        public override bool mayback() { return false; }
    }

    class SetLang : RxOp {
        Op.Op expr;
        public SetLang(Op.Op x) { expr = x; }
        public override void VisitOps(Func<Op.Op,Op.Op> post) {
            expr = expr.VisitOps(post);
        }

        public override void code(SubInfo body, List<object> to) {
            to.Add(CgOp.rxsetclass(CgOp.obj_llhow(CgOp.fetch(expr.cgop(body)))));
        }
        public override object[] lad() { return new [] { "Imp" }; }
        public override bool mayback() { return false; }
    }

    class Alt : AltBase {
        object[] optimized_lads;
        public Alt(RxOp[] z, string d) : base(z,d) {}

        public override void code(SubInfo body, List<object> to) {
            var ls = new string[zyg.Length];
            for (int i = 0; i < zyg.Length; i++) ls[i] = label();
            var end = label();

            to.Add(CgOp.ltm_push_alts(optimized_lads, dba, ls));
            to.Add(CgOp.@goto("backtrack"));
            for (int i = 0; i < zyg.Length; i++) {
                to.Add(CgOp.label(ls[i]));
                zyg[i].code(body, to);
                if (i != ls.Length) to.Add(CgOp.@goto(end));
            }
            to.Add(CgOp.label(end));
            to.Add(CgOp.popcut());
        }

        public override object[] lad() {
            object[] z = new object[] { zyg.Length };
            for (int i = 0; i < z.Length; i++) z[i] = zyg[i].lad();
            return new object[] { "Any", z };
        }

        public override RxOp rxsimp(bool cut) {
            var lads = new object[zyg.Length][];
            var kids = new RxOp[zyg.Length];
            for (int i = 0; i < zyg.Length; i++) {
                lads[i] = optimize_lad(zyg[i].lad());
                kids[i] = zyg[i].rxsimp(cut);
            }
            var n = new Alt(kids, dba);
            n.optimized_lads = lads;
            return n;
        }
    }

    class CheckBlock : RxOp {
        Op.Op block;
        bool negate;
        public CheckBlock(Op.Op b, bool n) { block=b; negate=n; }
        public override void VisitOps(Func<Op.Op,Op.Op> post) {
            block = block.VisitOps(post);
        }
        public override object[] lad() { return new [] { "Null" }; }
        public override void code(SubInfo body, List<object> to) {
            to.Add(CgOp.N(negate ? "cgoto" : "ncgoto", "backtrack",
                CgOp.obj_getbool(block.cgop(body))));
        }
        public override bool mayback() { return false; }
    }

    class SaveValue : RxOp {
        string capid;
        Op.Op block;
        public SaveValue(string c, Op.Op b) { block=b; capid=c; }
        public override void VisitOps(Func<Op.Op,Op.Op> post) {
            block = block.VisitOps(post);
        }
        public override object[] lad() { return new [] { "Imp" }; }
        public override void used_caps(int quant, Dictionary<string,int> acc) {
            acc[capid] = acc.GetDefault(capid, 0) + quant;
        }
        public override void code(SubInfo body, List<object> to) {
            to.Add(CgOp.rxpushcapture(block.cgop(body), new [] { capid }));
        }
        public override bool mayback() { return false; }
    }

    class VoidBlock : RxOp {
        Op.Op block;
        public VoidBlock(Op.Op b) { block=b; }
        public override void VisitOps(Func<Op.Op,Op.Op> post) {
            block = block.VisitOps(post);
        }
        public override bool mayback() { return false; }
        public override object[] lad() { return new [] { "Imp" }; }
        public override void code(SubInfo body, List<object> to) {
            to.Add(CgOp.sink(block.cgop(body)));
        }
    }

    class Statement : RxOp {
        Op.Op stmt;
        public Statement(Op.Op s) { stmt = s; }
        public override void oplift(List<Op.Op> z) { z.Add(stmt); }
        // no need to handle VisitOps here because stmt will be reparented
        // to the RegexBody node.
        public override void code(SubInfo body, List<object> to) {}
        public override object[] lad() { return new [] { "Null" }; }
        public override RxOp rxsimp(bool cut) { return new Sequence(); }
        public override bool mayback() { return false; }
    }

    class ProtoRedis : Capturing {
        public override Capturing withcaps(string[] caps) {
            var n = new ProtoRedis();
            n.captures = caps;
            return n;
        }

        public override void code(SubInfo body, List<object> to) {
            var bt = label();
            to.Add(CgOp.rxcall("InitCursorList", CgOp.rxlprim("proto_dispatch",
                CgOp.corelex("Any"))));
            to.Add(CgOp.label(bt));
            to.Add(CgOp.rxincorpshift(captures, bt));
        }

        public override object[] lad() { return new [] { "Dispatcher" }; }
    }

    class Any : RxOp {
        public override void code(SubInfo body, List<object> to) {
            to.Add(CgOp.rxbprim("AnyChar"));
        }
        public override object[] lad() { return new [] { "Dot" }; }
        public override bool mayback() { return false; }
    }

    // generated by optimizer so needs no lad; always greedy
    class QuantCClass : RxOp {
        CClass cc;
        int min, max;
        public QuantCClass(CClass c, int l, int h) { cc=c; min=l; max=h; }

        public override void code(SubInfo sub, List<object> to) {
            to.Add(CgOp.rxbprim("ScanCClass", CgOp.@int(min), CgOp.@int(max),
                CgOp.@const(cc)));
        }
        public override bool mayback() { return false; }
    }

    class CClassElem : RxOp {
        internal CClass cc;
        public CClassElem(CClass c) { cc = c; }
        public override void code(SubInfo body, List<object> to) {
            to.Add(CgOp.rxbprim("CClass", CgOp.@const(cc)));
        }
        public override object[] lad() {
            object[] terms = new object[cc.terms.Length+1];
            terms[0] = "CC";
            Array.Copy(cc.terms,0,terms,1,terms.Length-1);
            return terms;
        }
        public override bool mayback() { return false; }
    }

    class None : RxOp {
        public override void code(SubInfo s,List<object> to) {
            to.Add(CgOp.@goto("backtrack"));
        }
        public override object[] lad() { return new [] { "None" }; }
        public override bool mayback() { return false; }
    }

    class Newline : RxOp {
        public override void code(SubInfo s, List<object> to) {
            to.Add(CgOp.rxbprim("Newline"));
        }
        public override object[] lad() {
            return new object[] { "Any", new [] { new [] { "Str", "\x0D\x0A" },
                (new CClassElem(CClass.VSpace)).lad() } };
        }
    }

    class StringCap : Capturing {
        public StringCap(RxOp z) : base(new [] { z }) { }
        public override Capturing withcaps(string[] caps) {
            var n = new StringCap(zyg[0]);
            n.captures = caps;
            return n;
        }
        public override void code(SubInfo body, List<object> to) {
            to.Add(CgOp.pushcut("CAP"));
            to.Add(CgOp.rxsetquant(CgOp.rxgetpos()));
            zyg[0].code(body, to);
            to.Add(CgOp.rxpushcapture(CgOp.rxcall("StringCapture"), captures));
            to.Add(CgOp.popcut());
        }
        public override object[] lad() { return zyg[0].lad(); }
    }

    class ListPrim : Capturing {
        string name, type;
        Op.Op ops;
        public override void VisitOps(Func<Op.Op,Op.Op> post) {
            ops = ops.VisitOps(post);
        }
        public override Capturing withcaps(string[] caps) {
            var n = new ListPrim(name, type, ops);
            n.captures = caps;
            return n;
        }
        public ListPrim(string n, string t, Op.Op o) { name=n;type=t;ops=o; }

        public override void code(SubInfo body, List<object> to) {
            var bt = label();
            to.Add(CgOp.rxcall("InitCursorList", CgOp.rxlprim(type,
                ops.cgop(body))));
            to.Add(CgOp.label(bt));
            to.Add(CgOp.rxincorpshift(captures, bt));
        }

        public override object[] lad() {
            return type == "scalar_var" ? new [] { "Param", name } : new [] { "Imp" };
        }
    }

    class Endpoint : RxOp {
        string type;
        public Endpoint(string t) { type = t; }
        public override object[] lad() { return new [] { "Null" }; }
        public override void code(SubInfo body, List<object> to) {
            to.Add(CgOp.rxcall("SetEndpoint", CgOp.str(type)));
        }
    }
}
