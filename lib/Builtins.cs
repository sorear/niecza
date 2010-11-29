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
        if (lhs.whence == null && !lhs.islist) {
            if (!lhs.rw)
                throw new NieczaException("assigning to readonly value");

            lhs.Store(rhs);
        } else {
            Frame n = new Frame(null, null, Kernel.ExitRunloopSI).MakeChild(null, Kernel.AssignSI);
            n.pos = new Variable[2] { lhs, Kernel.NewROScalar(rhs) };
            Kernel.RunCore(n);
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
        return Kernel.BoxAnyMO(LaxSubstring2(r1, r2, r3), Kernel.StrMO);
    }

    public static Variable Plus(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        double r1 = o1.mo.mro_raw_Numeric.Get(v1);
        double r2 = o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO(r1 + r2, Kernel.NumMO);
    }

    public static Variable Minus(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        double r1 = o1.mo.mro_raw_Numeric.Get(v1);
        double r2 = o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO(r1 - r2, Kernel.NumMO);
    }

    public static Variable Mul(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        double r1 = o1.mo.mro_raw_Numeric.Get(v1);
        double r2 = o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO(r1 * r2, Kernel.NumMO);
    }

    public static Variable Divide(Variable v1, Variable v2) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        IP6 o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        double r1 = o1.mo.mro_raw_Numeric.Get(v1);
        double r2 = o2.mo.mro_raw_Numeric.Get(v2);
        return Kernel.BoxAnyMO(r1 / r2, Kernel.NumMO);
    }

    public static Variable PostIncrement(Variable v) {
        IP6 o1 = NominalCheck("$x", Kernel.AnyMO, v);
        double d = o1.mo.mro_raw_defined.Get(v) ?
            o1.mo.mro_raw_Numeric.Get(v) : 0;
        AssignV(v, Kernel.BoxRaw(d + 1, Kernel.NumMO));
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
        return Kernel.BoxAnyMO(-r, Kernel.NumMO);
    }

    public static VarDeque HashIterRaw(int mode, Variable v) {
        IP6 o = NominalCheck("$x", Kernel.AnyMO, v);
        Dictionary<string,Variable> d =
            (Dictionary<string,Variable>) Kernel.UnboxAny(o);

        VarDeque lv = new VarDeque();

        foreach (KeyValuePair<string,Variable> kv in d) {
            switch (mode) {
                case 0:
                    lv.Push(Kernel.BoxAnyMO(kv.Key, Kernel.StrMO));
                    break;
                case 1:
                    lv.Push(kv.Value);
                    break;
                case 2:
                    lv.Push(Kernel.BoxAnyMO(kv.Key, Kernel.StrMO));
                    lv.Push(kv.Value);
                    break;
                case 3:
                    DynObject p = new DynObject(Kernel.PairMO);
                    p.slots[0] = Kernel.BoxAnyMO(kv.Key, Kernel.StrMO);
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
}
