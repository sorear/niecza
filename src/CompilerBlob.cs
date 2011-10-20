using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Security.Cryptography;
using System.IO;

namespace Niecza {
    public abstract class CallReceiver : MarshalByRefObject, IDictionary {
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

    public class UpcallReceiver : CallReceiver {
        public override object this[object i] {
            set { }
            get {
                object[] ia = (object[]) i;
                string[] sa = new string[ia.Length];
                Array.Copy(ia, sa, ia.Length);
                Variable vr = Kernel.RunInferior(
                        Downcaller.upcall_cb.Fetch().Invoke(
                            Kernel.GetInferiorRoot(),
                            new Variable[] { Builtins.BoxLoS(sa) }, null));
                return vr.Fetch().mo.mro_raw_Str.Get(vr);
            }
        }
    }

    public class Downcaller {
        static AppDomain subDomain;
        internal static Variable upcall_cb;
        static IDictionary responder;
        static P6any UnitP, StaticSubP, TypeP;
        // Better, but still fudgy.  Relies too much on path structure.
        public static void InitSlave(Variable cb, Variable unit,
                Variable staticSub, Variable type) {
            if (subDomain != null) return;

            UnitP = unit.Fetch();
            StaticSubP = staticSub.Fetch();
            TypeP = type.Fetch();

            AppDomainSetup ads = new AppDomainSetup();
            string obj = Path.GetFullPath(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, Path.Combine("..", "obj")));
            ads.ApplicationBase = obj;
            string backend = Path.Combine(obj, "Kernel.dll");
            subDomain = AppDomain.CreateDomain("zyg", null, ads);
            upcall_cb = cb;
            responder = (IDictionary)
                subDomain.CreateInstanceFromAndUnwrap(backend,
                        "Niecza.CLRBackend.DowncallReceiver");
            RawDowncall("set_parent", AppDomain.CurrentDomain);
        }
        public static object RawDowncall(params object[] args) {
            return responder[args];
        }

        static object DCArg(Variable v) {
            P6any o = v.Fetch();
            if (o is BoxObject<object>)
                return Kernel.UnboxAny<object>(o);
            else if (o.IsDefined()) {
                if (o.Isa(Kernel.StrMO))
                    return (string) o.mo.mro_raw_Str.Get(v);
                else if (o.Isa(Kernel.BoolMO))
                    return (bool) o.mo.mro_raw_Bool.Get(v);
                else if (o.Isa(Kernel.ListMO)) {
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

        public static Variable DownCall(Variable list) {
            List<object> lo = new List<object>();
            VarDeque it = Builtins.start_iter(list);
            while (Kernel.IterHasFlat(it, true))
                lo.Add(DCArg(it.Shift()));

            return DCResult(RawDowncall(lo.ToArray()));
        }

        static Variable DCResult(object r) {
            if (r == null) return Kernel.AnyMO.typeVar;
            else if (r is string) return Kernel.BoxAnyMO((string)r, Kernel.StrMO);
            else if (r is int) return Builtins.MakeInt((int)r);
            else if (r is bool) return ((bool)r) ? Kernel.TrueV : Kernel.FalseV;
            else if (r is Exception) throw new NieczaException(((Exception)r).Message);
            else if (r is object[]) {
                object[] ra = (object[])r;
                Variable[] ba = new Variable[ra.Length];
                for (int i = 0; i < ba.Length; i++) ba[i] = DCResult(ra[i]);
                return Builtins.MakeParcel(ba);
            }
            else {
                string t = (string)RawDowncall("gettype", r);
                P6any pr = (t == "type") ? TypeP :
                    (t == "sub") ? StaticSubP :
                    (t == "unit") ? UnitP : null;
                return Kernel.BoxAnyMO(r, pr.mo);
            }
        }

        static void SerializeNam(Variable v, StringBuilder sb,
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
            } else if (o.Isa(Kernel.ListMO)) {
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

        public static Variable Finish(Variable si, Variable nam) {
            StringBuilder sb = new StringBuilder();
            List<object> refs = new List<object>();
            SerializeNam(nam, sb, refs);
            object[] args = new object[refs.Count + 3];
            args[0] = "sub_finish";
            args[1] = Kernel.UnboxAny<object>(si.Fetch());
            args[2] = sb.ToString();
            refs.CopyTo(args, 3);
            return DCResult(RawDowncall(args));
        }

        public static string DoHash(string input) {
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
    }
}
