using System;
using System.Reflection;
using System.Reflection.Emit;
using Niecza;
using System.Collections.Generic;

class NieczaCLROpts {
    public static readonly bool Debug =
        Environment.GetEnvironmentVariable("NIECZA_CLR_TRACE") != null;
}

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

    // XXX: We don't use the standard binder because we need multiple
    // dispatch capability.
    // TODO: Use binders from the DLR once moritz et al upgrade to Mono 2.8
    static DynBlockDelegate BindAmbiguous(Type ty, string n) {
        return delegate (Frame f) {
            return Kernel.Die(f, "Ambiguous binding type");
        };
    }

    static DynBlockDelegate BindMethodGroup(Type ty, string name,
            List<MethodInfo> ms) {
        return BindAmbiguous(ty, name);
    }

    static DynBlockDelegate BindPropertyGroup(Type ty, string name,
            List<PropertyInfo> ps) {
        return BindAmbiguous(ty, name);
    }

    static void MultiAdd<T>(Dictionary<string,List<T>> d, string n, T v) {
        List<T> l;
        if (!d.TryGetValue(n, out l))
            d[n] = l = new List<T>();
        l.Add(v);
    }

    static DynMetaObject NewWrapper(Type t) {
        DynMetaObject m = new DynMetaObject("clr:" + t.FullName);
        DynMetaObject pm = t.BaseType == null ? Kernel.AnyMO :
            GetWrapper(t.BaseType);
        DynMetaObject[] mro = new DynMetaObject[pm.mro.Length + 1];
        Array.Copy(pm.mro, 0, mro, 1, pm.mro.Length);
        mro[0] = m;
        m.FillClass(new string[] { }, new string[] { },
                new DynMetaObject[] { pm }, mro);
        if (NieczaCLROpts.Debug)
            Console.WriteLine("Setting up wrapper for {0}", t.FullName);

        HashSet<string> needNewWrapper = new HashSet<string>();
        Dictionary<string,List<MethodInfo>> allMethods
            = new Dictionary<string,List<MethodInfo>>();
        Dictionary<string,List<PropertyInfo>> allProperties
            = new Dictionary<string,List<PropertyInfo>>();

        foreach (MethodInfo mi in t.GetMethods(BindingFlags.Public |
                    BindingFlags.Static | BindingFlags.Instance)) {
            if (NieczaCLROpts.Debug)
                Console.WriteLine("Checking method : {0}", mi);
            if (mi.GetBaseDefinition().DeclaringType == t && !mi.IsSpecialName)
                needNewWrapper.Add(mi.Name);
            MultiAdd(allMethods, mi.Name, mi);
        }

        foreach (PropertyInfo pi in t.GetProperties(BindingFlags.Public |
                    BindingFlags.Static | BindingFlags.Instance)) {
            if (NieczaCLROpts.Debug)
                Console.WriteLine("Checking property : {0}", pi);
            MethodInfo[] mis = pi.GetAccessors();
            if (mis.Length != 0 && mis[0].GetBaseDefinition().DeclaringType == t)
                needNewWrapper.Add(pi.Name);
            MultiAdd(allProperties, pi.Name, pi);
        }

        List<int> handlers = new List<int>();
        foreach (string n in needNewWrapper) {
            handlers.Clear();

            string siname = string.Format("wrapper {0}:{1}", t.FullName, n);

            if (allMethods.ContainsKey(n))
                handlers.Add(1);
            if (allProperties.ContainsKey(n))
                handlers.Add(2);

            DynBlockDelegate method = null;

            switch (handlers.Count == 1 ? handlers[0] : 0) {
                case 0:
                    method = BindAmbiguous(t, n);
                    break;

                case 1:
                    method = BindMethodGroup(t, n, allMethods[n]);
                    break;

                case 2:
                    method = BindPropertyGroup(t, n, allProperties[n]);
                    break;
            }

            if (NieczaCLROpts.Debug)
                Console.WriteLine("Installing {0}", siname);
            m.AddMethod(n, Kernel.MakeSub(new SubInfo(siname, method), null));
        }

        m.Invalidate();
        m.typeObject = new BoxObject<object>(null, m);
        ((DynObject)m.typeObject).slots = null;
        return m;
    }

    public static Variable BoxCLR(object o) {
        if (o == null)
            return Kernel.NewROScalar(Kernel.AnyP);
        return Kernel.NewROScalar(new BoxObject<object>(o,
                    GetWrapper(o.GetType())));
    }

    public static Variable GetClass(string lang, string from) {
        lang = lang.ToLowerInvariant();
        // TODO: Microsoft.ScriptEngine interop
        if (lang != "clr" && lang != "cls" && lang != "dotnet")
            throw new NieczaException("Unknown source language " + lang);

        string e = Environment.GetEnvironmentVariable("NIECZA_WRAP_CLASS");
        if (e != null)
            GetWrapper(Type.GetType(from, true));

        return Kernel.NewROScalar(GetWrapper(Type.GetType(from, true)).typeObject);
    }
}
