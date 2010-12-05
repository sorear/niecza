using System;
using Niecza;
using System.Collections.Generic;

public class NieczaCLR {
    static Dictionary<Type, DynMetaObject> wrapper_cache
        = new Dictionary<Type, DynMetaObject>();

    static DynMetaObject GetWrapper(Type t) {
        lock (wrapper_cache) {
            DynMetaObject r;
            if (wrapper_cache.TryGetValue(t, out r))
                return r;
            return wrapper_cache[t] = NewWrapper(t);
        }
    }

    static DynMetaObject NewWrapper(Type t) {
        DynMetaObject m = new DynMetaObject("clr:" + t.FullName);
        m.FillClass(new string[] { }, new string[] { },
                new DynMetaObject[] { Kernel.AnyMO },
                new DynMetaObject[] { m, Kernel.AnyMO });
        // set up methods here
        m.Invalidate();
        m.typeObject = (IP6)Activator.CreateInstance(
                typeof(BoxObject<>).MakeGenericType(t), null, m);
        ((DynObject)m.typeObject).slots = null;
        return m;
    }

    public static Variable GetClass(string lang, string from) {
        lang = lang.ToLowerInvariant();
        // TODO: Microsoft.ScriptEngine interop
        if (lang != "clr" && lang != "cls" && lang != "dotnet")
            throw new NieczaException("Unknown source language " + lang);

        return Kernel.NewROScalar(GetWrapper(Type.GetType(from, true)).typeObject);
    }
}
