using Niecza;
using System;
using System.Collections.Generic;

public class Builtins {
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
        IP6 o1 = v1.Fetch();
        IP6 o2 = v2.Fetch();
        if (o1.mo.mro_raw_Numeric.Get(v1) == o2.mo.mro_raw_Numeric.Get(v2)) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable PostIncrement(Variable v) {
        IP6 o1 = v.Fetch();
        double d = o1.mo.mro_raw_defined.Get(v) ?
            o1.mo.mro_raw_Numeric.Get(v) : 0;
        v.Store(Kernel.BoxRaw(d + 1, Kernel.NumMO));
        return Kernel.NewROScalar(o1);
    }
}
