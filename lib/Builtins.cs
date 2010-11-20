using Niecza;
using System;
using System.Collections.Generic;

public class Builtins {
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
        double d = o1.mo.mro_raw_Numeric.Get(v);
        v.Store(Kernel.BoxRaw(d + 1, Kernel.NumMO));
        return Kernel.NewROScalar(o1);
    }
}
