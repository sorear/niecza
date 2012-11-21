using System;
using System.Reflection;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Security.Cryptography;
using System.IO;
using Niecza;

namespace Niecza {
    abstract class CallReceiver : IDictionary {
        public bool IsFixedSize { get { return false; } }
        public bool IsReadOnly { get { return false; } }
        public bool IsSynchronized { get { return false; } }
        public int Count { get { return 0; } }
        public object SyncRoot { get { return null; } }
        public ICollection Keys { get { return null; } }
        public ICollection Values { get { return null; } }
        public void Add(object a, object b) { }
        public void Clear() { }
        public IDictionaryEnumerator GetEnumerator() { return null; }
        IEnumerator IEnumerable.GetEnumerator() { return null; }
        public bool Contains(object a) { return false; }
        public void CopyTo(Array a, int offs) { }
        public void Remove(object a) { }
        public abstract object this[object i] { get; set; }
    }

    class UpcallReceiver : CallReceiver {
        public override object this[object i] {
            set { }
            get {
                object[] ia = (object[]) i;
                Variable[] va = new Variable[ia.Length];
                var cb = Downcaller.upcall_cb.Fetch();
                for (int ix = 0; ix < ia.Length; ix++)
                    va[ix] = Downcaller.DCResult(cb.mo.setting, ia[ix]);
                try {
                    Variable vr = Builtins.InvokeSub(cb, va);
                    return Downcaller.DCArg(vr);
                } catch (Exception ex) {
                    return new Exception(ex.ToString());
                }
            }
        }
    }

    class Downcaller {
        internal static Variable upcall_cb;
        internal static IDictionary responder;
        internal static P6any UnitP, StaticSubP, TypeP, ParamP, ValueP;
        internal static string obj_dir;

        public static object RawDowncall(params object[] args) {
            return responder[args];
        }

        internal static object DCArg(Variable v) {
            P6any o = v.Fetch();
            var s = o.mo.setting;
            if (o is BoxObject<object>)
                return Kernel.UnboxAny<object>(o);
            else if (o.IsDefined()) {
                if (o.Isa(s.StrMO))
                    return (string) o.mo.mro_raw_Str.Get(v);
                else if (o.Isa(s.BoolMO))
                    return (bool) o.mo.mro_raw_Bool.Get(v);
                else if (o.Isa(s.NumMO)) {
                    double d = Kernel.UnboxAny<double>(o);
                    if ((d % 1) == 0 && d <= int.MaxValue && d >= int.MinValue)
                        return (object)(int)d;
                    return (object)d;
                } else if (o.Isa(s.ListMO)) {
                    VarDeque it = o.mo.mro_raw_iterator.Get(v);
                    var lo = new List<object>();
                    while (Kernel.IterHasFlat(it, true))
                        lo.Add(DCArg(it.Shift()));
                    return lo.ToArray();
                } else
                    return (int) o.mo.mro_raw_Numeric.Get(v);
            } else
                return null;
        }

        internal static Variable DCResult(Compartment s, object r) {
            if (r == null) return s.AnyMO.typeObj;
            else if (r is string) return s.MakeStr((string)r);
            else if (r is int) return s.MakeInt((int)r);
            else if (r is bool) return ((bool)r) ? s.TrueV : s.FalseV;
            else if (r is Exception) throw new NieczaException(((Exception)r).Message);
            else if (r is object[]) {
                object[] ra = (object[])r;
                Variable[] ba = new Variable[ra.Length];
                for (int i = 0; i < ba.Length; i++) ba[i] = DCResult(s,ra[i]);
                return s.MakeParcel(ba);
            }
            else {
                string t = (string)RawDowncall("gettype", r);
                P6any pr = (t == "type") ? TypeP :
                    (t == "sub") ? StaticSubP :
                    (t == "param") ? ParamP :
                    (t == "value") ? ValueP :
                    (t == "unit") ? UnitP : s.AnyMO.typeObj;
                return Kernel.BoxAnyMO(r, pr.mo);
            }
        }

        internal static void SerializeNam(Variable v, StringBuilder sb,
                List<object> refs) {

            P6any o = v.Fetch();
            if (o is BoxObject<int>) { /* includes bool */
                sb.Append(Kernel.UnboxAny<int>(o));
            } else if (o is BoxObject<double>) {
                sb.Append(Utils.N2S(Kernel.UnboxAny<double>(o)));
            } else if (o is BoxObject<string>) {
                string s = Kernel.UnboxAny<string>(o);
                sb.Append('"');
                foreach (char c in s) {
                    if (c >= ' ' && c <= '~' && c != '\\' && c != '"')
                        sb.Append(c);
                    else {
                        sb.Append("\\u");
                        sb.AppendFormat("{0:X4}", (int)c);
                    }
                }
                sb.Append('"');
            } else if (!o.IsDefined()) {
                sb.Append("null");
            } else if (o.Isa(o.mo.setting.ListMO)) {
                VarDeque d = o.mo.mro_raw_iterator.Get(v);
                bool comma = false;
                sb.Append('[');
                while (Kernel.IterHasFlat(d, true)) {
                    if (comma) sb.Append(',');
                    SerializeNam(d.Shift(), sb, refs);
                    comma = true;
                }
                sb.Append(']');
            } else if (o is BoxObject<object>) {
                sb.Append('!');
                sb.Append(refs.Count);
                refs.Add(Kernel.UnboxAny<object>(o));
            } else {
                throw new NieczaException("weird object in sub_finish " + o.mo.name);
            }
        }
    }
}

public partial class Builtins {
    public static string cb_get_basedir() {
        return AppDomain.CurrentDomain.BaseDirectory;
    }

    public static Variable cb_downcall(Variable list) {
        List<object> lo = new List<object>();
        VarDeque it = Builtins.start_iter(list);
        while (Kernel.IterHasFlat(it, true))
            lo.Add(Downcaller.DCArg(it.Shift()));

        return Downcaller.DCResult(list.Fetch().mo.setting, Downcaller.RawDowncall(lo.ToArray()));
    }

    // Better, but still fudgy.  Relies too much on path structure.
    public static void cb_init_slave(Variable cb, P6any cmd_obj_dir, Variable unit,
            Variable staticSub, Variable type, Variable param, Variable value) {
        if (Downcaller.responder != null) return;

        Downcaller.UnitP = unit.Fetch();
        Downcaller.StaticSubP = staticSub.Fetch();
        Downcaller.TypeP = type.Fetch();
        Downcaller.ParamP = param.Fetch();
        Downcaller.ValueP = value.Fetch();

        Downcaller.obj_dir = Path.GetFullPath(cmd_obj_dir.IsDefined() ?
                cmd_obj_dir.mo.mro_raw_Str.Get(cmd_obj_dir) :
                Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
                    "NieczaModuleCache"));

        Directory.CreateDirectory(Downcaller.obj_dir); // like mkdir -p

        Downcaller.upcall_cb = cb;
        Downcaller.responder = (IDictionary) new Niecza.CLRBackend.DowncallReceiver();
    }

    public static void cb_init_compartment(Variable c) {
        Downcaller.RawDowncall("set_binding", Downcaller.DCArg(c), Downcaller.obj_dir, new UpcallReceiver());
    }

    public static Variable cb_prune_match(Variable vr) {
        Cursor c = (Cursor)vr.Fetch();
        // remove as much as possible - don't call this if you still need
        // the match!
        if (c.feedback != null) {
            c.feedback.CommitRule();
            c.feedback.bt = null;
            c.feedback.st = new State();
            c.feedback.ast = null;
        }

        for (CapInfo it = c.captures; it != null; it = it.prev) {
            if (it.cap != null && it.cap.Fetch() is Cursor)
                cb_prune_match(it.cap);
        }

        c.captures = null;
        c.feedback = null;
        c.ast = null;
        c.xact = null;
        c.nstate = null;
        return vr;
    }

    public static Variable cb_finish(Variable si, Variable nam) {
        StringBuilder sb = new StringBuilder();
        List<object> refs = new List<object>();
        Downcaller.SerializeNam(nam, sb, refs);
        object[] args = new object[refs.Count + 3];
        args[0] = "sub_finish";
        args[1] = Kernel.UnboxAny<object>(si.Fetch());
        args[2] = sb.ToString();
        refs.CopyTo(args, 3);
        return Downcaller.DCResult(si.Fetch().mo.setting, Downcaller.RawDowncall(args));
    }

    public static string cb_do_hash(string input) {
        HashAlgorithm sha = SHA256.Create();
        byte[] ibytes = new UTF8Encoding().GetBytes(input);
        byte[] hash = sha.ComputeHash(ibytes);
        char[] buf = new char[hash.Length * 2];
        for (int i = 0; i < hash.Length; i++) {
            buf[i*2]   = "0123456789abcdef"[hash[i] >> 4];
            buf[i*2+1] = "0123456789abcdef"[hash[i] & 15];
        }
        return new string(buf);
    }

    public static string cb_exec_name() {
        return Assembly.GetEntryAssembly().Location;
    }

    static Dictionary<P6any,Dictionary<P6any,Variable>> role_cache =
        new Dictionary<P6any,Dictionary<P6any,Variable>>();
    public static Variable cb_cached_but(P6any but, Variable v1, Variable v2) {
        P6any a1 = v1.Fetch();
        P6any a2 = v2.Fetch();
        Dictionary<P6any,Variable> subcache;
        if (!role_cache.TryGetValue(a1, out subcache))
            role_cache[a1] = subcache = new Dictionary<P6any,Variable>();
        Variable var;
        if (subcache.TryGetValue(a2, out var))
            return var;

        // Mega-Hack - stop lots of internal data from being retained by
        // CALLER pointers
        Kernel.SetTopFrame(null);

        var = Builtins.InvokeSub(but, v1, v2);
        return subcache[a2] = var;
    }
}
