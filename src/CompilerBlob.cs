using System;
using System.Collections;
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
                string[] sar = Builtins.UnboxLoS(Kernel.RunInferior(
                            Downcaller.upcall_cb.Fetch().Invoke(
                                Kernel.GetInferiorRoot(),
                                new Variable[] { Builtins.BoxLoS(sa) }, null)));
                object[] iar = new object[sar.Length];
                Array.Copy(sar, iar, sar.Length);
                return iar;
            }
        }
    }

    public class Downcaller {
        private static AppDomain subDomain;
        internal static Variable upcall_cb;
        private static IDictionary responder;
        // Better, but still fudgy.  Relies too much on path structure.
        public static void InitSlave(Variable cb) {
            if (subDomain != null) return;

            AppDomainSetup ads = new AppDomainSetup();
            string obj = Path.GetFullPath(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, Path.Combine("..", "obj")));
            ads.ApplicationBase = obj;
            string backend = Path.Combine(obj, "CLRBackend.exe");
            subDomain = AppDomain.CreateDomain("zyg", null, ads);
            upcall_cb = cb;
            responder = (IDictionary)
                subDomain.CreateInstanceFromAndUnwrap(backend,
                        "Niecza.CLRBackend.DowncallReceiver");
            RawDowncall("set_parent", AppDomain.CurrentDomain);
        }
        public static object[] RawDowncall(params object[] args) {
            return (object[]) responder[args];
        }
        public static Variable DownCall(Variable list) {
            string[] ps = Builtins.UnboxLoS(list);
            object[] po = new object[ps.Length];
            Array.Copy(ps, po, ps.Length);
            object[] ro = RawDowncall(po);
            string[] rs = new string[ro.Length];
            Array.Copy(ro, rs, ro.Length);
            return Builtins.BoxLoS(rs);
        }
    }
}
