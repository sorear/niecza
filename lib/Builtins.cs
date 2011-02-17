using Niecza;
using System;
using System.Collections.Generic;

public class Builtins {
    public static IP6 NominalCheck(string name, DynMetaObject mo, Variable v) {
        IP6 r = v.Fetch();
        if (!r.mo.HasMRO(mo))
            throw new NieczaException("Nominal type check failed for " + name +
                    " needed " + mo.name + " got " + r.mo.name);
        return r;
    }

    public static void AssignV(Variable lhs, IP6 rhs) {
        if (!lhs.islist) {
            lhs.Store(rhs);
        } else {
            Frame n = lhs.Fetch().InvokeMethod(Kernel.GetInferiorRoot(),
                    "LISTSTORE",
                    new Variable[2] { lhs, Kernel.NewROScalar(rhs) }, null);
            Kernel.RunInferior(n);
        }
    }

    public static string LaxSubstring(string str, int from) {
        if (from <= 0)
            return str;
        if (from >= str.Length)
            return "";
        return str.Substring(from);
    }

    public static string LaxSubstring2(string str, int from, int l) {
        if (from <= 0) from = 0;
        if (from >= str.Length) from = str.Length;
        if (l >= str.Length - from) l = str.Length - from;
        if (l < 0) l = 0;
        return str.Substring(from, l);
    }

    public static Variable NumericEq(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (o1.mo.mro_raw_Numeric.Get(v1) == o2.mo.mro_raw_Numeric.Get(v2)) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable NumericLt(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (o1.mo.mro_raw_Numeric.Get(v1) < o2.mo.mro_raw_Numeric.Get(v2)) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable NumericNe(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (o1.mo.mro_raw_Numeric.Get(v1) != o2.mo.mro_raw_Numeric.Get(v2)) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable NumericLe(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (o1.mo.mro_raw_Numeric.Get(v1) <= o2.mo.mro_raw_Numeric.Get(v2)) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable NumericGt(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (o1.mo.mro_raw_Numeric.Get(v1) > o2.mo.mro_raw_Numeric.Get(v2)) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable NumericGe(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (o1.mo.mro_raw_Numeric.Get(v1) >= o2.mo.mro_raw_Numeric.Get(v2)) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable StringEq(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (o1.mo.mro_raw_Str.Get(v1) == o2.mo.mro_raw_Str.Get(v2)) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable StringNe(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (o1.mo.mro_raw_Str.Get(v1) != o2.mo.mro_raw_Str.Get(v2)) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable StringLt(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (string.CompareOrdinal(o1.mo.mro_raw_Str.Get(v1),
                    o2.mo.mro_raw_Str.Get(v2)) < 0) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable StringLe(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (string.CompareOrdinal(o1.mo.mro_raw_Str.Get(v1),
                    o2.mo.mro_raw_Str.Get(v2)) <= 0) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable StringGe(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (string.CompareOrdinal(o1.mo.mro_raw_Str.Get(v1),
                    o2.mo.mro_raw_Str.Get(v2)) >= 0) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable StringGt(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (string.CompareOrdinal(o1.mo.mro_raw_Str.Get(v1),
                    o2.mo.mro_raw_Str.Get(v2)) > 0) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable Substr3(Variable v1, Variable v2, Variable v3) {
        IP6 o1 = NominalCheck("$string", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$start", Kernel.AnyMO, v2);
        IP6 o3 = NominalCheck("$chars", Kernel.AnyMO, v3);
        string r1 = o1.mo.mro_raw_Str.Get(v1);
        int r2    = (int)o2.mo.mro_raw_Numeric.Get(v2);
        int r3    = (int)o3.mo.mro_raw_Numeric.Get(v3);
        return Kernel.BoxAnyMO<string>(LaxSubstring2(r1, r2, r3), Kernel.StrMO);
    }

    public static Variable Plus(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        double r1 = o1.mo.mro_raw_Numeric.Get(v1);
        double r2 = o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO<double>(r1 + r2, Kernel.NumMO);
    }

    public static Variable Minus(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        double r1 = o1.mo.mro_raw_Numeric.Get(v1);
        double r2 = o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO<double>(r1 - r2, Kernel.NumMO);
    }

    public static Variable Mul(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        double r1 = o1.mo.mro_raw_Numeric.Get(v1);
        double r2 = o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO<double>(r1 * r2, Kernel.NumMO);
    }

    public static Variable Divide(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        double r1 = o1.mo.mro_raw_Numeric.Get(v1);
        double r2 = o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO<double>(r1 / r2, Kernel.NumMO);
    }

    public static Variable NumAnd(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO<double>((double)(r1 & r2), Kernel.NumMO);
    }

    public static Variable NumOr(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO<double>((double)(r1 | r2), Kernel.NumMO);
    }

    public static Variable NumXor(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO<double>((double)(r1 ^ r2), Kernel.NumMO);
    }

    public static Variable NumLShift(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO<double>((double)(r1 << r2), Kernel.NumMO);
    }

    public static Variable NumRShift(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO<double>((double)(r1 >> r2), Kernel.NumMO);
    }

    public static Variable NumCompl(Variable v1) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        return Kernel.BoxAnyMO<double>((double)(~r1), Kernel.NumMO);
    }

    public static Variable PostIncrement(Variable v) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v);
        AssignV(v, o1.mo.mro_succ.Get(v));
        return Kernel.NewROScalar(o1);
    }

    public static Variable Not(Variable v) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v);
        bool r = o1.mo.mro_raw_Bool.Get(v);
        return r ? Kernel.FalseV : Kernel.TrueV;
    }

    public static Variable Chars(Variable v) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v);
        string r = o1.mo.mro_raw_Str.Get(v);
        return Kernel.BoxAnyMO((double)r.Length, Kernel.NumMO);
    }

    public static Variable Negate(Variable v) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v);
        double r = o1.mo.mro_raw_Numeric.Get(v);
        return Kernel.BoxAnyMO<double>(-r, Kernel.NumMO);
    }

    public static Variable Ord(Variable v) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v);
        string r = o1.mo.mro_raw_Str.Get(v);
        // XXX Failure
        if (r.Length == 0) return Kernel.NewROScalar(Kernel.AnyP);
        return Kernel.BoxAnyMO((double)r[0], Kernel.NumMO);
    }

    public static Variable Chr(Variable v) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v);
        double r = o1.mo.mro_raw_Numeric.Get(v);
        return Kernel.BoxAnyMO(new string((char)r, 1), Kernel.StrMO);
    }

    // used in cclass.t; maybe worth exposing
    public static Variable UniCat(Variable v) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v);
        char c = (char) o1.mo.mro_raw_Numeric.Get(v);
        int ix = (int) char.GetUnicodeCategory(c);
        return Kernel.BoxAnyMO((double)ix, Kernel.NumMO);
    }

    public static Variable Make(Frame fr, Variable v) {
        if (fr.info.name == "SAFE make")
            fr = fr.caller;
        Cursor c = (Cursor) Kernel.StatusHelper(fr, "$*/", 0).Fetch();
        c.Make(v);
        return v;
    }

    public static VarDeque HashIterRaw(int mode, Variable v) {
        IP6 o = NominalCheck("$x", Kernel.AnyMO, v);
        VarHash d = Kernel.UnboxAny<VarHash>(o);

        VarDeque lv = new VarDeque();

        foreach (KeyValuePair<string,Variable> kv in d) {
            switch (mode) {
                case 0:
                    lv.Push(Kernel.BoxAnyMO<string>(kv.Key, Kernel.StrMO));
                    break;
                case 1:
                    lv.Push(kv.Value);
                    break;
                case 2:
                    lv.Push(Kernel.BoxAnyMO<string>(kv.Key, Kernel.StrMO));
                    lv.Push(kv.Value);
                    break;
                case 3:
                    DynObject p = new DynObject(Kernel.PairMO);
                    p.slots[0] = Kernel.BoxAnyMO<string>(kv.Key, Kernel.StrMO);
                    p.slots[1] = kv.Value;
                    lv.Push(Kernel.NewROScalar(p));
                    break;
            }
        }
        return lv;
    }
    public static Variable HashIter(int mode, Variable v) {
        VarDeque lv = HashIterRaw(mode, v);
        DynObject l = new DynObject(Kernel.ListMO);
        l.slots[0] = lv;
        l.slots[1] = new VarDeque();
        return Kernel.NewRWListVar(l);
    }

    public static Variable GetModTime(string path) {
        long t = System.IO.File.GetLastWriteTimeUtc(path).Ticks;
        double d = ((double)(t - 621355968000000000L)) / 10000000.0;
        return Kernel.BoxAnyMO(d, Kernel.NumMO);
    }

    public static Variable GetNow() {
        long t = System.DateTime.UtcNow.Ticks;
        double d = ((double)(t - 621355968000000000L)) / 10000000.0;
        return Kernel.BoxAnyMO(d, Kernel.NumMO);
    }

    public static bool FileOrDirExists(string path) {
        return System.IO.File.Exists(path) || System.IO.Directory.Exists(path);
    }

    // This is wrong in the long term.  The backend should be linked; it
    // should generate collectable assemblies; app-domains should be made
    // available through MetaServices.
    private static AppDomain subDomain;
    public static Variable RunCLRSubtask(Variable filename, Variable args) {
        List<string> la = new List<string>();
        VarDeque iter = new VarDeque(args);
        string sfn = filename.Fetch().mo.mro_raw_Str.Get(filename);
        //Console.WriteLine("App name {0}", sfn);
        while (Kernel.IterHasFlat(iter, true)) {
            Variable v = iter.Shift();
            la.Add(v.Fetch().mo.mro_raw_Str.Get(v));
            //Console.WriteLine("Arg {0}", la[la.Count-1]);
        }
        if (subDomain == null) {
            AppDomainSetup ads = new AppDomainSetup();
            ads.ApplicationBase = System.IO.Path.GetDirectoryName(sfn);
            subDomain = AppDomain.CreateDomain("zyg", null, ads);
        }
        int ret = subDomain.ExecuteAssembly(sfn, null, la.ToArray());
        return Kernel.BoxAnyMO((double) ret, Kernel.NumMO);
    }

    public static void RunSubtask(string file, string args) {
        System.Diagnostics.Process.Start(file, args).WaitForExit();
    }

    public static Variable ArrayConstructor(Variable bits) {
        VarDeque rest  = new VarDeque(bits);
        VarDeque items = new VarDeque();
        while (Kernel.IterHasFlat(rest, true))
            items.Push(Kernel.NewRWScalar(Kernel.AnyMO, rest.Shift().Fetch()));
        IP6 l = new DynObject(Kernel.ArrayMO);
        l.SetSlot("rest", rest);
        l.SetSlot("items", items);
        return Kernel.NewROScalar(l);
    }

    public static int GetArity(IP6 fcni) {
        if (!fcni.Isa(Kernel.SubMO))
            return 1; // can't introspect fake subs (?)
        SubInfo si = (SubInfo) fcni.GetSlot("info");
        int[] sig = si.sig_i;
        if (sig == null)
            return 1;
        int arity = 0;
        for (int i = 0; i < sig.Length; i += SubInfo.SIG_I_RECORD) {
            int fl = sig[i + SubInfo.SIG_I_FLAGS];
            if ((fl & (SubInfo.SIG_F_SLURPY_CAP | SubInfo.SIG_F_SLURPY_POS |
                    SubInfo.SIG_F_SLURPY_PCL)) != 0)
                return int.MaxValue;
            if ((fl & SubInfo.SIG_F_POSITIONAL) == 0) continue;
            arity++;
        }
        return arity;
    }

    class ItemSource {
        protected ItemSource() {}
        public static ItemSource Empty = new ItemSource();
        public virtual bool TryGet(out Variable[] r, bool block) {
            r = null;
            return true;
        }
        protected static int TryOne(VarDeque items, bool block) {
            if (block) {
                return Kernel.IterHasFlat(items, true) ? +1 : -1;
            } else {
again:
                if (items.Count() == 0) return -1;
                Variable v = items[0];
                IP6 i = v.Fetch();
                if (i.mo.HasMRO(Kernel.IterCursorMO))
                    return 0;
                if (v.islist) {
                    items.Shift();
                    items.UnshiftD(i.mo.mro_raw_iterator.Get(v));
                    goto again;
                }
                return +1;
            }
        }
    }

    class BatchSource: ItemSource {
        int arity;
        VarDeque items;

        public BatchSource(int count, VarDeque items) {
            this.arity = count;
            this.items = items;
        }

        public override bool TryGet(out Variable[] r, bool block) {
            r = null;
            List<Variable> pen = new List<Variable>();
            while (pen.Count < arity) {
                switch (TryOne(items, block)) {
                    case -1: goto nomore;
                    case 0:
                        for (int i = pen.Count - 1; i >= 0; i--)
                            items.Unshift(pen[i]);
                        return false;
                    case +1: pen.Add(items.Shift()); break;
                }
            }
nomore:
            if (pen.Count != 0)
                r = pen.ToArray();
            return true;
        }
    }

    class ZipSource : ItemSource {
        VarDeque[] sources;
        public ZipSource(Variable[] pcl) {
            sources = new VarDeque[pcl.Length];
            for (int i = 0; i < pcl.Length; i++)
                sources[i] = new VarDeque(pcl[i]);
        }

        public override bool TryGet(out Variable[] r, bool block) {
            r = null;
            for (int i = 0; i < sources.Length; i++)
                switch (TryOne(sources[i], block)) {
                    case -1: return true;
                    case  0: return false;
                }
            r = new Variable[sources.Length];
            for (int i = 0; i < sources.Length; i++)
                r[i] = sources[i].Shift();
            return true;
        }
    }

    class CrossSource: ItemSource {
        VarDeque[] basic;
        VarDeque[] iter;
        Variable[] basic_top;
        Variable[] iter_top;
        // 0=init 1=end i=advance wheel i-2
        int state;

        public CrossSource(Variable[] pcl) {
            basic = new VarDeque[pcl.Length];
            iter  = new VarDeque[pcl.Length];
            basic_top = new Variable[pcl.Length];
            iter_top  = new Variable[pcl.Length];
            for (int i = 0; i < pcl.Length; i++) {
                iter[i] = new VarDeque(pcl[i]);
            }
        }

        public override bool TryGet(out Variable[] r, bool block) {
            r = null;
            if (state == 0) {
                // Make sure all the lists are non-empty.
                for (int i = 0; i < iter.Length; i++) {
                    switch (TryOne(iter[i], block)) {
                        case -1: return true;
                        case 0:  return false;
                        case 1:  break;
                    }
                }
                for (int i = 0; i < iter.Length; i++) {
                    iter_top[i] = iter[i].Shift();
                    if (i != 0) {
                        basic[i] = new VarDeque(iter[i]);
                        basic_top[i] = iter_top[i];
                    }
                }
            }
            else if (state == 1) {
                return true;
            }
            else {
again:
                int wheel = state - 2;
                switch (TryOne(iter[wheel], block)) {
                    case 0:  return false;
                    case +1:
                        iter_top[wheel] = iter[wheel].Shift();
                        break;
                    case -1:
                        if (wheel == 0) return true;
                        iter[wheel] = new VarDeque(basic[wheel]);
                        iter_top[wheel] = basic_top[wheel];
                        state--;
                        goto again;
                }
            }
            r = new Variable[iter_top.Length];
            for (int i = 0; i < iter_top.Length; i++)
                r[i] = iter_top[i];
            state = iter_top.Length + 1;
            return true;
        }
    }

    private static SubInfo CommonMEMap_I = new SubInfo("KERNEL map", null,
            CommonMEMap_C, null, null, new int[] {
                2, 3, SubInfo.ON_NEXT, 0, 0,
                2, 3, SubInfo.ON_REDO, 1, 0,
                2, 3, SubInfo.ON_LAST, 3, 0,
            }, new string[] { "" }, 0, null, null);
    private static Frame CommonMEMap_C(Frame th) {
        ItemSource src = (ItemSource) th.lex0;
        VarDeque outq = (VarDeque) th.lex1;
        IP6 fnc = (IP6) th.lex2;
        int tailmode = th.lexi0;

        switch (th.ip) {
            case 0:
                Variable[] pen;
                if (!src.TryGet(out pen, tailmode != 0)) {
                    DynObject thunk = new DynObject(Kernel.GatherIteratorMO);
                    th.lex = new Dictionary<string,object>();
                    th.lex["!return"] = null;
                    th.MarkSharedChain();
                    thunk.slots[0] = Kernel.NewRWScalar(Kernel.AnyMO, th);
                    thunk.slots[1] = Kernel.NewRWScalar(Kernel.AnyMO, Kernel.AnyP);
                    DynObject lst = new DynObject(Kernel.ListMO);
                    lst.slots[0] = outq;
                    lst.slots[1] = new VarDeque(Kernel.NewROScalar(thunk));
                    th.caller.resultSlot = Kernel.NewRWListVar(lst);
                    th.lexi0 = 1;
                    return th.caller;
                }
                if (pen == null) {
                    if (tailmode != 0)
                        return Kernel.Take(th, Kernel.NewROScalar(Kernel.EMPTYP));
                    DynObject lst = new DynObject(Kernel.ListMO);
                    lst.slots[0] = outq;
                    lst.slots[1] = new VarDeque();
                    th.caller.resultSlot = Kernel.NewRWListVar(lst);
                    return th.caller;
                }
                th.lex3 = pen;
                th.ip = 1;
                goto case 1;
            case 1:
                th.ip = 2;
                if (fnc != null)
                    return fnc.Invoke(th, (Variable[])th.lex3, null);
                else {
                    th.resultSlot = Kernel.NewRWListVar(
                            Kernel.BoxRaw((Variable[])th.lex3,
                                Kernel.ParcelMO));
                    goto case 2;
                }
            case 2:
                if (tailmode != 0) {
                    th.ip = 0;
                    return Kernel.Take(th, (Variable)th.resultSlot);
                } else {
                    outq.Push((Variable) th.resultSlot);
                    th.ip = 0;
                    goto case 0;
                }
            case 3:
                th.lex0 = src = ItemSource.Empty;
                th.ip = 0;
                goto case 0;
            default:
                return Kernel.Die(th, "Invalid IP");
        }
    }

    public static Frame MEMap(Frame th, Variable[] lst) {
        VarDeque iter = new VarDeque(lst);
        Variable fcn = iter.Shift();
        IP6 fcni = fcn.Fetch();
        int arity = GetArity(fcni);

        Frame fr = th.MakeChild(null, CommonMEMap_I);
        fr.lexi0 = 0;
        fr.lex0 = new BatchSource(arity, iter);
        fr.lex1 = new VarDeque();
        fr.lex2 = fcni;
        return fr;
    }

    static IP6 ExtractWith(bool with, ref Variable[] pcl) {
        if (!with) return null;
        Variable[] opcl = pcl;
        pcl = new Variable[pcl.Length - 1];
        for (int j = 0; j < pcl.Length; j++)
            pcl[j] = opcl[j+1];
        return opcl[0].Fetch();
    }

    public static Frame MEZip(Frame th, bool with, Variable[] pcl) {
        Frame fr = th.MakeChild(null, CommonMEMap_I);
        fr.lexi0 = 0;
        fr.lex2 = ExtractWith(with, ref pcl);
        fr.lex0 = new ZipSource(pcl);
        fr.lex1 = new VarDeque();
        return fr;
    }

    public static Frame MECross(Frame th, bool with, Variable[] pcl) {
        Frame fr = th.MakeChild(null, CommonMEMap_I);
        fr.lexi0 = 0;
        fr.lex2 = ExtractWith(with, ref pcl);
        fr.lex0 = new CrossSource(pcl);
        fr.lex1 = new VarDeque();
        return fr;
    }

    private static SubInfo CommonGrep_I = new SubInfo("KERNEL grep", null,
            CommonGrep_C, null, null, new int[] {
                2, 3, SubInfo.ON_NEXT, 0, 0,
                2, 3, SubInfo.ON_REDO, 1, 0,
                2, 3, SubInfo.ON_LAST, 3, 0,
            }, new string[] { "" }, 0, null, null);
    private static Frame CommonGrep_C(Frame th) {
        VarDeque src = (VarDeque) th.lex0;
        VarDeque outq = (VarDeque) th.lex1;
        Variable flt = (Variable) th.lex2;
        int tailmode = th.lexi0;

        switch (th.ip) {
            case 0:
                Variable pen = null;
                while (pen == null) {
                    if (tailmode != 0) {
                        if (!Kernel.IterHasFlat(src, false)) break;
                    } else {
                        if (src.Count() == 0) break;
                        if (src[0].Fetch().mo.HasMRO(Kernel.IterCursorMO)) {
                            DynObject thunk = new DynObject(Kernel.GatherIteratorMO);
                            th.lex = new Dictionary<string,object>();
                            th.lex["!return"] = null;
                            th.MarkSharedChain();
                            thunk.slots[0] = Kernel.NewRWScalar(Kernel.AnyMO, th);
                            thunk.slots[1] = Kernel.NewRWScalar(Kernel.AnyMO, Kernel.AnyP);
                            DynObject lst = new DynObject(Kernel.ListMO);
                            lst.slots[0] = outq;
                            lst.slots[1] = new VarDeque(Kernel.NewROScalar(thunk));
                            th.caller.resultSlot = Kernel.NewRWListVar(lst);
                            th.lexi0 = 1;
                            return th.caller;
                        }
                    }
                    pen = src.Shift();
                }
                if (pen == null) {
                    if (tailmode != 0)
                        return Kernel.Take(th, Kernel.NewROScalar(Kernel.EMPTYP));
                    DynObject lst = new DynObject(Kernel.ListMO);
                    lst.slots[0] = outq;
                    lst.slots[1] = new VarDeque();
                    th.caller.resultSlot = Kernel.NewRWListVar(lst);
                    return th.caller;
                }
                th.lex3 = pen;
                th.ip = 1;
                goto case 1;
            case 1:
                th.ip = 2;
                return flt.Fetch().InvokeMethod(th, "ACCEPTS",
                        new Variable[] { flt, (Variable)th.lex3 }, null);
            case 2:
                Variable r = (Variable) th.resultSlot;
                if (!r.Fetch().mo.mro_raw_Bool.Get(r)) {
                    th.ip = 0;
                    goto case 0;
                }
                if (tailmode != 0) {
                    th.ip = 0;
                    return Kernel.Take(th, (Variable)th.lex3);
                } else {
                    outq.Push((Variable) th.lex3);
                    th.ip = 0;
                    goto case 0;
                }
            case 3:
                th.lex0 = src = new VarDeque();
                th.ip = 0;
                goto case 0;
            default:
                return Kernel.Die(th, "Invalid IP");
        }
    }
    public static Frame MEGrep(Frame th, Variable[] lst) {
        VarDeque iter = new VarDeque(lst);
        Variable fcn = iter.Shift();
        iter = Kernel.IterFlatten(iter);

        Frame fr = th.MakeChild(null, CommonGrep_I);
        fr.lexi0 = 0;
        fr.lex0 = iter;
        fr.lex1 = new VarDeque();
        fr.lex2 = fcn;
        return fr;
    }

    public static Frame CallNext(Frame th, IP6 cap) {
        Frame to = th;
        while (to != null && to.curDisp == null)
            to = to.caller;
        if (to == null || to.curDisp.next == null)
            return Kernel.Die(th, "No next function to call!");

        DispatchEnt de = to.curDisp.next;
        DynObject o = cap as DynObject;
        Frame nf = th.MakeChild(de.outer, de.info);

        Variable[] p = to.pos;
        VarHash n    = to.named;
        if (o != null) {
            p = (Variable[]) o.slots[0];
            n = o.slots[1] as VarHash;
        }
        nf = nf.info.Binder(nf, p, n);
        nf.curDisp = de;
        return nf;
    }
}
