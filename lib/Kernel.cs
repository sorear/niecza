using System;
using System.Collections.Generic;
using System.Threading;
using System.Runtime.InteropServices;
namespace Niecza {
    // We like to reuse continuation objects for speed - every function only
    // creates one kind of continuation, but tweaks a field for exact return
    // point.  As such, call frames and continuations are in 1:1 correspondence
    // and are unified.  Functions take a current continuation and return a new
    // continuation; we tail recurse with trampolines.

    // Only call other functions in Continue, not in the CallableDelegate or
    // equivalent!
    public delegate Frame CallableDelegate(Frame caller,
            Variable[] pos, Dictionary<string, Variable> named);
    // Used by DynFrame to plug in code
    public delegate Frame DynBlockDelegate(Frame frame);

    public abstract class IP6 {
        public abstract DynMetaObject GetMO();
        public abstract Frame GetAttribute(Frame caller, string name);

        protected Frame Fail(Frame caller, string msg) {
            return Kernel.Die(caller, msg + " in class " + GetMO().name);
        }

        // Most reprs won't have a concept of type objects
        public virtual bool IsDefined() { return true; }

        // include the invocant in the positionals!  it will not usually be
        // this, rather a container of this
        public virtual Frame InvokeMethod(Frame caller, string name,
                Variable[] pos, Dictionary<string, Variable> named) {
            IP6 m;
            //Kernel.LogNameLookup(name);
            if (GetMO().mro_methods.TryGetValue(name, out m)) {
                return m.Invoke(caller, pos, named);
            }
            return Fail(caller, "Unable to resolve method " + name);
        }

        public virtual Frame HOW(Frame caller) {
            caller.resultSlot = GetMO().how;
            return caller;
        }

        public virtual IP6 GetTypeObject() {
            return GetMO().typeObject;
        }

        public virtual string GetTypeName() {
            return GetMO().name;
        }

        public virtual bool Isa(DynMetaObject mo) {
            return GetMO().HasMRO(mo);
        }

        public virtual bool Does(DynMetaObject mo) {
            return GetMO().HasMRO(mo);
        }

        public virtual Frame Invoke(Frame c, Variable[] p,
                Dictionary<string, Variable> n) {
            DynMetaObject.InvokeHandler ih = GetMO().mro_OnInvoke;
            if (ih != null) {
                return ih(this, c, p, n);
            } else {
                Variable[] np = new Variable[p.Length + 1];
                Array.Copy(p, 0, np, 1, p.Length);
                np[0] = Kernel.NewROScalar(this);
                return InvokeMethod(c, "INVOKE", np, n);
            }
        }

        public virtual Frame Fetch(Frame c) {
            DynMetaObject.FetchHandler fh = GetMO().mro_OnFetch;
            if (fh != null) {
                return fh(this, c);
            } else {
                return InvokeMethod(c, "FETCH", new Variable[1] {
                        Kernel.NewROScalar(this) }, null);
            }
        }

        public virtual Frame Store(Frame c, IP6 o) {
            DynMetaObject.StoreHandler sh = GetMO().mro_OnStore;
            if (sh != null) {
                return sh(this, c, o);
            } else {
                return InvokeMethod(c, "STORE", new Variable[2] {
                        Kernel.NewROScalar(this), Kernel.NewROScalar(o) },
                        null);
            }
        }
    }

    // TODO: update this comment

    // Variables are things which can produce LValues, and can also bind
    // LValues.  They hold LValues and may or may not be bindable.  Variables
    // also tend to contextualize stuff put into them.
    //
    // Coercions are not used on binding unless necessary.
    //
    // Variables also have type constraints, that's how %foo and @foo differ...

    // A LValue is the meaning of function arguments, of any subexpression
    // except the targets of := and .VAR.
    //
    // They come in two flavors.  Scalary lvalues hold a container, which
    // can do FETCH and STORE.  Listy lvalues FETCH as the container itself,
    // and STORE as a method to the container.
    //
    // List->scalar context: create a simple container holding the list's
    // object, but !islist.  Read only.
    //
    // Scalar->list: bind islist, must be Iterable. Bind it same rwness.
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public class Variable {
        // If non-null, then this lv is, or at one time was, the result of
        // an autovivifying access, but has not yet committed to becoming
        // real or becoming undef.  We call these virtual containers; they
        // exist only in flight (resultSlots) and in parcel contexts.
        public IP6 whence;
        public IP6 container;
        // will be a direct ref if !rw; lists are always !rw
        public bool bvalue;
        public bool rw;
        public bool islist;

        public Variable(bool bvalue, bool rw, bool islist, IP6 whence,
                IP6 container) {
            this.bvalue = bvalue;
            this.whence = whence;
            this.container = container;
            this.rw = rw;
            this.islist = islist;
        }
    }

    // This stores all the invariant stuff about a Sub, i.e. everything
    // except the outer pointer.  Now distinct from protopads
    public class SubInfo {
        public int[] lines;
        public DynBlockDelegate code;
        public DynMetaObject mo;
        // for inheriting hints
        public SubInfo outer;
        public Dictionary<string, object> hints;
        // maybe should be a hint
        public LAD ltm;

        public void PutHint(string name, object val) {
            if (hints == null)
                hints = new Dictionary<string,object>();
            hints[name] = val;
        }

        public bool GetLocalHint<T>(string name, out T val) where T: class {
            object o;
            if (hints != null && hints.TryGetValue(name, out o)) {
                val = o as T;
                return true;
            } else {
                val = null;
                return false;
            }
        }

        public SubInfo(int[] lines, DynBlockDelegate code, SubInfo outer,
                Dictionary<string,object> hints, LAD ltm) {
            this.lines = lines;
            this.code = code;
            this.outer = outer;
            this.hints = hints;
            this.ltm = ltm;
        }

        public SubInfo(DynBlockDelegate code) :
            this(null, code, null, null, null) { }
    }

    // We need hashy frames available to properly handle BEGIN; for the time
    // being, all frames will be hashy for simplicity
    public class Frame: IP6 {
        public readonly Frame caller;
        public readonly Frame outer;
        public readonly SubInfo info;
        public object resultSlot = null;
        public int ip = 0;
        public readonly DynBlockDelegate code; // premature optimization?
        public Dictionary<string, object> lex;
        // statistically, most subs have between 1 and 4 anonymous lexicals
        public object lex0;
        public object lex1;
        public object lex2;
        public object lex3;
        public object[] lexn;

        public RxFrame rx;

        public Variable[] pos;
        public Dictionary<string, Variable> named;

        public Frame(Frame caller_, Frame outer_,
                SubInfo info_) {
            caller = caller_;
            outer = outer_;
            code = info_.code;
            info = info_;
        }

        public Frame Continue() {
            return code(this);
        }

        public override Frame GetAttribute(Frame c, string name) {
            c.resultSlot = lex[name];
            return c;
        }

        public Variable ExtractNamed(string n) {
            Variable r;
            if (named != null && named.TryGetValue(n, out r)) {
                named.Remove(n);
                return r;
            } else {
                return null;
            }
        }

        public override DynMetaObject GetMO() { return Kernel.CallFrameMO; }

        public int ExecutingLine() {
            if (info != null && info.lines != null) {
                return ip >= info.lines.Length ? 0 : info.lines[ip];
            } else {
                return 0;
            }
        }

        public string ExecutingFile() {
            string l;
            SubInfo i = info;
            while (i != null) {
                // possibly, using $?FILE and Fetch would be better
                if (i.GetLocalHint("?file", out l))
                    return l;
                i = i.outer;
            }
            return "";
        }

        public Variable LexicalFind(string name) {
            Frame csr = this;
            while (csr != null) {
                object o;
                if (csr.lex == null) {
                    csr = csr.outer;
                    continue;
                }
                if (csr.lex.TryGetValue(name, out o))
                    return (Variable)o;
                csr = csr.outer;
            }
            return Kernel.NewROScalar(Kernel.AnyP);
        }

        private static List<string> spacey = new List<string>();
        public string DepthMark() {
            Frame f = this;
            int ix = 0;
            while (f != null) { ix++; f = f.caller; }
            while (spacey.Count <= ix) { spacey.Add(new String(' ', spacey.Count * 2)); }
            return spacey[ix];
        }
    }

    // NOT IP6; these things should only be exposed through a ClassHOW-like
    // façade
    public class DynMetaObject {
        public IP6 how;
        public IP6 typeObject;
        public string name;

        public LexerCache lexcache;
        public LexerCache GetLexerCache() {
            if (lexcache == null)
                lexcache = new LexerCache();
            return lexcache;
        }

        public delegate Frame InvokeHandler(IP6 th, Frame c,
                Variable[] pos, Dictionary<string, Variable> named);
        public delegate Frame FetchHandler(IP6 th, Frame c);
        public delegate Frame StoreHandler(IP6 th, Frame c, IP6 n);

        public InvokeHandler OnInvoke;
        public FetchHandler OnFetch;
        public StoreHandler OnStore;

        public InvokeHandler mro_OnInvoke;
        public FetchHandler mro_OnFetch;
        public StoreHandler mro_OnStore;
        public Dictionary<string, IP6> mro_methods;

        public List<DynMetaObject> superclasses
            = new List<DynMetaObject>();
        public Dictionary<string, IP6> local
            = new Dictionary<string, IP6>();
        public List<string> local_attr = new List<string>();

        public Dictionary<string, int> slotMap = new Dictionary<string, int>();
        public int nslots = 0;

        private WeakReference wr_this;
        // protected by static lock
        private HashSet<WeakReference> subclasses = new HashSet<WeakReference>();
        private static object mro_cache_lock = new object();

        public int FindSlot(string name) {
            //Kernel.LogNameLookup(name);
            return slotMap[name];
        }

        public Dictionary<string, List<DynObject>> multiregex;

        public DynMetaObject[] mro;
        public HashSet<DynMetaObject> isa;

        public DynMetaObject(string name) {
            this.name = name;
            this.wr_this = new WeakReference(this);

            isa = new HashSet<DynMetaObject>();
        }

        private void Revalidate() {
            mro_OnStore = null;
            mro_OnInvoke = null;
            mro_OnFetch = null;
            mro_methods = new Dictionary<string,IP6>();

            if (mro == null)
                return;

            for (int kx = mro.Length - 1; kx >= 0; kx--) {
                DynMetaObject k = mro[kx];
                if (k.OnStore != null)
                    mro_OnStore = k.OnStore;
                if (k.OnFetch != null)
                    mro_OnFetch = k.OnFetch;
                if (k.OnInvoke != null)
                    mro_OnInvoke = k.OnInvoke;

                foreach (KeyValuePair<string,IP6> m in k.local)
                    mro_methods[m.Key] = m.Value;
            }
        }

        private void SetMRO(DynMetaObject[] arr) {
            lock(mro_cache_lock) {
                if (mro != null)
                    foreach (DynMetaObject k in mro)
                        k.subclasses.Remove(wr_this);
                foreach (DynMetaObject k in arr)
                    k.subclasses.Add(wr_this);
            }
            mro = arr;
        }

        ~DynMetaObject() {
            lock(mro_cache_lock)
                if (mro != null)
                    foreach (DynMetaObject k in mro)
                        k.subclasses.Remove(wr_this);
        }

        private void Invalidate() {
            if (mro == null)
                return;
            List<DynMetaObject> notify = new List<DynMetaObject>();
            lock(mro_cache_lock)
                foreach (WeakReference k in subclasses)
                    notify.Add(k.Target as DynMetaObject);
            foreach (DynMetaObject k in notify)
                if (k != null)
                    k.Revalidate();
        }

        public void AddMultiRegex(string name, IP6 m) {
            if (multiregex == null)
                multiregex = new Dictionary<string, List<DynObject>>();
            List<DynObject> dl;
            if (! multiregex.TryGetValue(name, out dl)) {
                dl = new List<DynObject>();
                multiregex[name] = dl;
            }
            dl.Add((DynObject)m);
        }

        public IP6 Can(string name) {
            IP6 m;
            if (mro_methods.TryGetValue(name, out m))
                return m;
            return null;
        }

        public Dictionary<string,IP6> AllMethods() {
            return mro_methods;
        }

        public HashSet<IP6> AllMethodsSet() {
            HashSet<IP6> r = new HashSet<IP6>();
            foreach (KeyValuePair<string,IP6> kv in mro_methods)
                r.Add(kv.Value);
            return r;
        }

        public bool HasMRO(DynMetaObject m) {
            return isa.Contains(m);
        }

        private static bool C3Debug =
            Environment.GetEnvironmentVariable("NIECZA_C3_TRACE") != null;

        private static string MROStr(List<DynMetaObject> chain) {
            return Kernel.JoinS(" <- ", chain, delegate(DynMetaObject o) {
                return o.name;
            });
        }

        private static void DumpC3Lists(string f, DynMetaObject[] m,
                List<List<DynMetaObject>> d) {
            Console.WriteLine(f + MROStr(new List<DynMetaObject>(m)) + " // " +
                    Kernel.JoinS(" | ", d, MROStr));
        }

        public void AddMethod(string name, IP6 code) {
            local[name] = code;
            Invalidate();
        }

        public void AddAttribute(string name) {
            local_attr.Add(name);
            Invalidate();
        }

        public void AddSuperclass(DynMetaObject other) {
            superclasses.Add(other);
        }

        // this gets called more than once for Scalar, ClassHOW, and Sub
        // just be sure that the attrib list doesn't change
        public void Complete() {
            List<List<DynMetaObject>> toMerge = new List<List<DynMetaObject>>();
            List<DynMetaObject> mro_l = new List<DynMetaObject>();
            isa = new HashSet<DynMetaObject>();
            toMerge.Add(new List<DynMetaObject>());
            toMerge[0].Add(this);

            foreach (DynMetaObject dmo in superclasses) {
                toMerge[0].Add(dmo);
                toMerge.Add(new List<DynMetaObject>(dmo.mro));
            }

            if (C3Debug)
                DumpC3Lists("C3 start: " + name + ": ", mro, toMerge);

            while (true) {
top:
                if (C3Debug)
                    DumpC3Lists("C3 iter: ", mro, toMerge);

                foreach (List<DynMetaObject> h in toMerge) {
                    if (h.Count == 0) {
                        continue; // next CANDIDATE
                    }
                    DynMetaObject cand = h[0];
                    foreach (List<DynMetaObject> bs in toMerge) {
                        if (bs.Count == 0) {
                            continue; // next BLOCKER
                        }
                        if (bs[0] == cand) {
                            continue;
                        }
                        if (bs.Contains(cand)) {
                            goto blocked;
                        }
                    }
                    // no reason not to immediately put this, and by loop
                    // order the C3 condition is kept
                    mro_l.Add(cand);
                    isa.Add(cand);
                    foreach (List<DynMetaObject> l in toMerge) {
                        l.Remove(cand);
                    }
                    goto top;
blocked:
                    ;
                }
                if (C3Debug)
                    DumpC3Lists("C3 end: ", mro, toMerge);
                foreach (List<DynMetaObject> l in toMerge) {
                    if (l.Count != 0) {
                        // should refactor this to use a real p6exception
                        throw new Exception("C3 MRO inconsistency detected");
                    }
                }
                break;
            }

            SetMRO(mro_l.ToArray());

            nslots = 0;
            foreach (DynMetaObject k in mro) {
                foreach (string an in k.local_attr) {
                    slotMap[an] = nslots++;
                }
            }

            Invalidate();
        }
    }

    // This is quite similar to DynFrame and I wonder if I can unify them.
    // These are always hashy for the same reason as Frame above
    public class DynObject: IP6 {
        // the slots have to support non-containerized values, because
        // containers are objects now
        public object[] slots;
        public DynMetaObject klass;

        public DynObject(DynMetaObject klass) {
            this.klass = klass;
            this.slots = new object[klass.nslots];
        }

        public override DynMetaObject GetMO() { return klass; }

        public override Frame GetAttribute(Frame caller, string name) {
            if (slots == null) {
                return Fail(caller, "Attempted to access slot " + name +
                        " via an object with no slots");
            }
            caller.resultSlot = GetSlot(name);
            return caller;
        }

        public void SetSlot(string name, object obj) {
            slots[klass.FindSlot(name)] = obj;
        }

        public object GetSlot(string name) {
            return slots[klass.FindSlot(name)];
        }

        public override bool IsDefined() {
            return this != klass.typeObject;
        }
    }

    // This class is slated for bloody death.  See Kernel.BoxAny for the
    // replacement.
    public class CLRImportObject : IP6 {
        public readonly object val;

        public CLRImportObject(object val_) { val = val_; }

        public override Frame GetAttribute(Frame c, string nm) {
            return Kernel.Die(c, "Attribute " + nm +
                    " not available on CLRImportObject");
        }

        public override DynMetaObject GetMO() { return null; }
    }

    // A bunch of stuff which raises big circularity issues if done in the
    // setting itself.
    public class Kernel {
        public static DynBlockDelegate MainlineContinuation;

        // Note: for classes without public .new, there's no way to get
        // "interesting" user subclasses, so direct indexing is safe

        public static object UnboxDO(DynObject o) {
            return o.slots[0];
        }

        public static object UnboxAny(IP6 o) {
            // TODO: Check for compatibility?
            return UnboxDO((DynObject)o);
        }

        private static Frame SCFetch(IP6 th, Frame caller) {
            DynObject dyo = (DynObject) th;
            caller.resultSlot = dyo.slots[0];
            return caller;
        }

        private static Frame SCStore(IP6 th, Frame caller, IP6 nv) {
            DynObject dyo = (DynObject) th;
            dyo.slots[0] = nv;
            return caller;
        }

        public static Stack<Frame> TakeReturnStack = new Stack<Frame>();

        public static Frame Take(Frame th, Variable payload) {
            Frame r = TakeReturnStack.Pop();
            r.lex["$*nextframe"] = NewROScalar(th);
            r.resultSlot = payload;
            th.resultSlot = payload;
            return r;
        }

        public static Frame CoTake(Frame th, Frame from) {
            TakeReturnStack.Push(th);
            return from;
        }

        public static Frame GatherHelper(Frame th, IP6 sub) {
            DynObject dyo = (DynObject) sub;
            Frame n = new Frame(th,
                                (Frame) dyo.slots[0],
                                (SubInfo) dyo.slots[1]);
            th.resultSlot = n;
            return th;
        }

        private static Frame SubInvoke(IP6 th, Frame caller,
                Variable[] pos, Dictionary<string,Variable> named) {
            DynObject dyo = ((DynObject) th);
            Frame outer = (Frame) dyo.slots[0];
            SubInfo info = (SubInfo) dyo.slots[1];

            Frame n = new Frame(caller, outer, info);
            n.pos = pos;
            n.named = named;

            return n;
        }
        private static SubInfo SubInvokeSubSI = new SubInfo(SubInvokeSubC);
        private static Frame SubInvokeSubC(Frame th) {
            Variable[] post;
            switch (th.ip) {
                case 0:
                    th.ip = 1;
                    return Fetch(th, th.pos[0]);
                default:
                    post = new Variable[th.pos.Length - 1];
                    Array.Copy(th.pos, 1, post, 0, th.pos.Length - 1);
                    return SubInvoke((DynObject)th.resultSlot, th.caller,
                            post, th.named);
            }
        }

        public static Frame Die(Frame caller, string msg) {
            DynObject n = new DynObject(((DynObject)StrP).klass);
            n.slots[0] = msg;
            return new FatalException(n).SearchForHandler(caller);
        }

        public static readonly DynMetaObject SubMO;
        public static readonly DynMetaObject ScalarMO;

        public static bool TraceCont;

        public static IP6 MakeSub(SubInfo info, Frame outer) {
            DynObject n = new DynObject(info.mo ?? SubMO);
            n.slots[0] = outer;
            n.slots[1] = info;
            return n;
        }

        public static DynObject MockBox(object v) {
            DynObject n = new DynObject(ScalarMO);
            n.slots[0] = v;
            return n;
        }

        public static Variable BoxAny(object v, IP6 proto) {
            if (v == null)
                return NewROScalar(proto);
            DynObject n = new DynObject(((DynObject)proto).klass);
            n.slots[0] = v;
            return NewROScalar(n);
        }

        public static IP6 MakeSC(IP6 inside) {
            DynObject n = new DynObject(ScalarMO);
            n.slots[0] = inside;
            return n;
        }

        // check whence before calling
        public static Frame Vivify(Frame th, Variable v) {
            IP6 w = v.whence;
            v.whence = null;
            return w.Invoke(th, new Variable[1] { v }, null);
        }

        private static SubInfo BindROSI = new SubInfo(BindROC);
        private static Frame BindROC(Frame th) {
            switch (th.ip) {
                case 0:
                    if (th.pos[0].whence == null)
                        goto case 1;
                    th.ip = 1;
                    return Vivify(th, th.pos[0]);
                case 1:
                    th.ip = 2;
                    return Fetch(th, th.pos[1]);
                case 2:
                    th.pos[0].container = (IP6) th.resultSlot;
                    th.pos[0].rw = false;
                    return th.caller;
                default:
                    return Kernel.Die(th, "IP invalid");
            }
        }

        private static SubInfo BindSI = new SubInfo(BindC);
        private static Frame BindC(Frame th) {
            switch (th.ip) {
                case 0:
                    // autovivify rhs if needed
                    if (th.pos[1].whence == null)
                        goto case 1;
                    th.ip = 1;
                    return Vivify(th, th.pos[1]);
                case 1:
                    if (th.pos[0].whence == null)
                        goto case 2;
                    th.ip = 2;
                    return Vivify(th, th.pos[0]);
                case 2:
                    th.pos[0].container = th.pos[1].container;
                    th.pos[0].rw = th.pos[1].rw;
                    return th.caller;
                default:
                    return Kernel.Die(th, "IP invalid");
            }
        }

        public static Frame Bind(Frame th, Variable lhs, Variable rhs,
                bool ro, bool forcerw) {
            // TODO: need exceptions for forcerw to be used
            Frame n;
            if (lhs.islist) ro = true;
            // fast path
            if (ro && !rhs.rw && lhs.whence == null) {
                lhs.rw = false;
                lhs.container = rhs.container;
                return th;
            }
            if (!ro && lhs.whence == null && rhs.whence == null) {
                lhs.rw = rhs.rw;
                lhs.container = rhs.container;
                return th;
            }

            n = new Frame(th, null, ro ? BindROSI : BindSI);
            n.pos = new Variable[2] { lhs, rhs };
            return n;
        }

        public static Frame NewBoundVar(Frame th, bool ro, bool islist,
                Variable rhs) {
            Frame n;
            // fast path
            if (ro && !rhs.rw) {
                th.resultSlot = new Variable(true, false, islist, null,
                        rhs.container);
                return th;
            }
            if (!ro && rhs.whence == null) {
                th.resultSlot = new Variable(true, rhs.rw, false, null,
                        rhs.container);
                return th;
            }

            Variable lhs = islist ? NewRWListVar(null) : NewROScalar(null);
            th.resultSlot = lhs;

            n = new Frame(th, null, ro ? BindROSI : BindSI);
            n.pos = new Variable[2] { lhs, rhs };
            return n;
        }

        // This isn't just a fetch and a store...
        private static SubInfo AssignSI = new SubInfo(AssignC);
        private static Frame AssignC(Frame th) {
            switch (th.ip) {
                case 0:
                    if (th.pos[0].whence == null)
                        goto case 1;
                    th.ip = 1;
                    return Vivify(th, th.pos[0]);
                case 1:
                    if (th.pos[0].islist) {
                        return th.pos[0].container.InvokeMethod(th.caller,
                                "LISTSTORE", th.pos, null);
                    } else if (!th.pos[0].rw) {
                        return Kernel.Die(th.caller, "assigning to readonly value");
                    } else if (!th.pos[1].rw) {
                        return th.pos[0].container.Store(th.caller,
                                th.pos[1].container);
                    } else {
                        th.ip = 2;
                        return th.pos[1].container.Fetch(th);
                    }
                case 2:
                    return th.pos[0].container.Store(th.caller,
                            (IP6)th.resultSlot);
                default:
                    return Kernel.Die(th, "Invalid IP");
            }
        }

        public static Frame Assign(Frame th, Variable lhs, Variable rhs) {
            Frame n = new Frame(th, null, AssignSI);
            n.pos = new Variable[2] { lhs, rhs };
            return n;
        }

        public static Frame Fetch(Frame th, Variable vr) {
            if (!vr.rw) {
                th.resultSlot = vr.container;
                return th;
            } else {
                return vr.container.Fetch(th);
            }
        }

        // ro, not rebindable
        public static Variable NewROScalar(IP6 obj) {
            return new Variable(false, false, false, null, obj);
        }

        public static Variable NewRWScalar(IP6 obj) {
            return new Variable(true, true, false, null, MakeSC(obj));
        }

        public static Variable NewRWListVar(IP6 container) {
            return new Variable(true, false, true, null, container);
        }

        public static VarDeque SlurpyHelper(Frame th, int from) {
            VarDeque lv = new VarDeque();
            for (int i = from; i < th.pos.Length; i++) {
                lv.Push(th.pos[i]);
            }
            return lv;
        }

        public static Variable ContextHelper(Frame th, string name) {
            object rt;
            while (th != null) {
                if (th.lex == null) {
                    th = th.caller;
                    continue;
                }
                if (th.lex.TryGetValue(name, out rt)) {
                    return (Variable)rt;
                }
                th = th.caller;
            }
            name = name.Remove(1,1);
            Dictionary<string,Variable> gstash = (Dictionary<string,Variable>)
                (((CLRImportObject)GlobalO).val);
            Variable v;

            if (gstash.TryGetValue(name, out v)) {
                return v;
            } else {
                return PackageLookup(ProcessO, name);
            }
        }

        public static Variable DefaultNew(IP6 proto) {
            DynObject n = new DynObject(((DynObject)proto).klass);
            DynMetaObject[] mro = n.klass.mro;

            for (int i = mro.Length - 1; i >= 0; i--) {
                foreach (string s in mro[i].local_attr) {
                    n.SetSlot(s, NewRWScalar(AnyP));
                }
            }

            return NewROScalar(n);
        }

        public static Frame GetFirst(Frame th, IP6 lst) {
            DynObject dyl = lst as DynObject;
            if (dyl == null) goto slow;
            if (dyl.klass != ListMO) goto slow;
            VarDeque itemsl = (VarDeque) dyl.GetSlot("items");
            if (itemsl.Count() == 0) goto slow;
            th.resultSlot = itemsl[0];
            return th;

slow:
            return lst.InvokeMethod(th, "head", new Variable[] {
                    NewROScalar(lst) }, null);
        }

        public static DynMetaObject ListMO;
        public static IP6 AnyP;
        public static IP6 ArrayP;
        public static IP6 HashP;
        public static IP6 StrP;
        public static DynMetaObject CallFrameMO;

        public static Variable PackageLookup(IP6 parent, string name) {
            Dictionary<string,Variable> stash = (Dictionary<string,Variable>)
                (((CLRImportObject)parent).val);
            Variable v;

            if (stash.TryGetValue(name, out v)) {
                return v;
            } else if (name.EndsWith("::")) {
                Dictionary<string,Variable> newstash =
                    new Dictionary<string,Variable>();
                newstash["PARENT::"] = NewROScalar(parent);
                return (stash[name] = NewROScalar(
                            new CLRImportObject(newstash)));
            } else {
                // TODO: @foo, %foo
                return (stash[name] = NewRWScalar(AnyP));
            }
        }

        public static Frame StartP6Thread(Frame th, IP6 sub) {
            Thread thr = new Thread(delegate () {
                    Frame current = sub.Invoke(th, new Variable[0], null);

                    while (current != th) {
                        try {
                            current = current.Continue();
                        } catch (Exception ex) {
                            ExceptionPacket ep = new FatalException(
                                    new CLRImportObject(ex));
                            current = ep.SearchForHandler(current);
                        }
                    }
                });
            thr.Start();
            th.resultSlot = thr;
            return th;
        }

        public static void RunLoop(SubInfo boot) {
            Kernel.TraceCont = (Environment.GetEnvironmentVariable("NIECZA_TRACE") != null);
            Frame root_f = new Frame(null, null, boot);
            Frame current = root_f;
            while (current != null) {
                try {
                    current = current.Continue();
                } catch (Exception ex) {
                    ExceptionPacket ep = new FatalException(
                            new CLRImportObject(ex));
                    current = ep.SearchForHandler(current);
                }
            }
        }

        public static void AddMany(Dictionary<string,Variable> d1,
                Dictionary<string,Variable> d2) {
            foreach (KeyValuePair<string,Variable> kv in d2) {
                d1[kv.Key] = kv.Value;
            }
        }

        // XXX should be per-unit
        public static Variable Global;
        public static IP6 GlobalO;
        public static Variable Process;
        public static IP6 ProcessO;

        static Kernel() {
            DynMetaObject pStrMO = new DynMetaObject("protoStr");
            pStrMO.AddAttribute("value");
            pStrMO.Complete();
            StrP = new DynObject(pStrMO);

            SubMO = new DynMetaObject("Sub");
            SubMO.OnInvoke = new DynMetaObject.InvokeHandler(SubInvoke);
            SubMO.AddAttribute("outer");
            SubMO.AddAttribute("info");
            SubMO.Complete();
            SubMO.AddMethod("INVOKE", MakeSub(SubInvokeSubSI, null));

            ScalarMO = new DynMetaObject("Scalar");
            ScalarMO.AddAttribute("value");
            ScalarMO.Complete();
            ScalarMO.OnFetch = new DynMetaObject.FetchHandler(SCFetch);
            ScalarMO.OnStore = new DynMetaObject.StoreHandler(SCStore);

            GlobalO = new CLRImportObject(new Dictionary<string,Variable>());
            Global = NewROScalar(GlobalO);
            ProcessO = new CLRImportObject(new Dictionary<string,Variable>());
            Process = NewROScalar(ProcessO);
        }

        public static Dictionary<string, int> usedNames = new Dictionary<string, int>();
        public static void LogNameLookup(string name) {
            int k;
            usedNames.TryGetValue(name, out k);
            usedNames[name] = k + 1;
        }

        public static void DumpNameLog() {
            foreach (KeyValuePair<string, int> kv in usedNames)
                Console.WriteLine("{0} {1}", kv.Value, kv.Key);
        }

        // This is a library function in .NET 4
        public delegate string JoinSFormatter<T>(T x);
        public static string JoinS<T>(string sep, IEnumerable<T> things) {
            return JoinS(sep, things, delegate(T y) { return y.ToString(); });
        }
        public static string JoinS<T>(string sep, IEnumerable<T> things,
                JoinSFormatter<T> fmt) {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();

            bool fst = true;
            foreach (T x in things) {
                if (!fst) sb.Append(sep);
                fst = false;
                sb.Append(fmt(x));
            }
            return sb.ToString();
        }
    }

    public abstract class ExceptionPacket {
        public Frame bot;
        public Frame top;

        public abstract bool Filter(Frame cand);
        // for setting $!
        public abstract IP6 Payload();
        // CATCH vs CONTROL
        public abstract bool IsFatal();
        public abstract Frame Process(Frame target);

        // stage 2!
        public Frame Unwind(Frame caller) {
            while (top != bot) {
                Frame pop = bot;
                bot = bot.caller;
                object o;

                if (pop.info.hints != null &&
                        pop.info.hints.TryGetValue("!unwind", out o)) {
                    Frame n = new Frame(bot, bot, (SubInfo) o);
                    n.lex = new Dictionary<string,object>();
                    n.lex["!reunwind"] = this;
                    return n;
                }
            }

            return bot;
        }

        public Frame SearchForHandler(Frame caller) {
            if (bot == null) { // first call
                bot = top = caller;
            }
            string key = IsFatal() ? "!ehcatch" : "!ehcontrol";
            while (top != null) {
                object o;
                if (top.info.hints != null &&
                        top.info.hints.TryGetValue(key, out o)) {
                    Frame fn = new Frame(bot, top, (SubInfo) o);
                    fn.lex = new Dictionary<string,object>();
                    fn.lex["!rethrow"] = this;
                    return fn;
                }

                if (top.lex != null && top.lex.ContainsKey("!rethrow")) {
                    // this is an active exception handling frame!  skip
                    // the corresponding handler
                    top = top.outer.caller;
                } else {
                    if (Filter(top)) {
                        return Process(top);
                    }
                    top = top.caller;
                }
            }

            Unhandled();
            return null;
        }

        // This *probably* ought to be handled in Perl 6 code, using .Str,
        // but we're now at a very low bootstrap level;
        private void Unhandled() {
            IP6 p = Payload();
            CLRImportObject cp = p as CLRImportObject;
            DynObject dp = p as DynObject;
            Exception e = (cp != null) ? (cp.val as Exception) : null;
            string s = (dp != null && dp.klass.slotMap.ContainsKey("value")) ?
                (dp.GetSlot("value") as string) : null;

            if (e != null) {
                // Wrapped CLR exception; has a message, and inner frames
                s = e.ToString();
            } else if (s == null) {
                s = "(no message available)";
            }
            Console.Error.WriteLine("Unhandled exception: " + s);
            Frame c;
            for (c = bot; c != null; c = c.caller) {
                Console.Error.WriteLine("  at {0} line {1}",
                        c.ExecutingFile(), c.ExecutingLine());
            }
            Environment.Exit(1);
        }
    }

    // no payload; you should have already poked the right value into a
    // result slot somewhere.  ip = 0 means immediate return (leave)
    public class LexoticControlException : ExceptionPacket {
        public Frame target;
        public int ip;

        public LexoticControlException(Frame target, int ip) {
            this.target = target;
            this.ip = ip;
        }

        public override bool Filter(Frame cand) { return cand == target; }
        public override IP6 Payload() { return Kernel.AnyP; } // XXX
        public override bool IsFatal() { return false; }
        public override Frame Process(Frame t) {
            if (ip != 0) {
                t.ip = ip;
            }
            return t;
        }
    }

    public class FatalException : ExceptionPacket {
        public IP6 payload;

        public FatalException(IP6 payload) {
            this.payload = payload;
        }

        // can only be explicitly caught
        public override bool Filter(Frame cand) { return false; }
        public override bool IsFatal() { return true; }
        public override IP6 Payload() { return payload; }
        public override Frame Process(Frame t) { return t; }
    }

    public sealed class VarDeque {
        private Variable[] data;
        private int head;
        private int count;

        public int Count() { return count; }

        public VarDeque() {
            data = new Variable[8];
        }

        public VarDeque(VarDeque tp) {
            data = (Variable[]) tp.data.Clone();
            head = tp.head;
            count = tp.count;
        }

        public VarDeque(Variable[] parcel) {
            int cap = 8;
            while (cap <= parcel.Length) cap *= 2;
            data = new Variable[cap];
            Array.Copy(parcel, 0, data, 0, parcel.Length);
            count = parcel.Length;
        }

        private int fixindex(int index) {
            int rix = index + head;
            if (rix >= data.Length) rix -= data.Length;
            return rix;
        }

        private int fixindexc(int index) {
            if (index >= count)
                throw new IndexOutOfRangeException();
            return fixindex(index);
        }

        public Variable this[int index] {
            get { return data[fixindexc(index)]; }
            set { data[fixindexc(index)] = value; }
        }

        public void Push(Variable vr) {
            checkgrow();
            data[fixindex(count++)] = vr;
        }

        public Variable Pop() {
            int index = fixindex(--count);
            Variable d = data[index];
            data[index] = null;
            return d;
        }

        public void Unshift(Variable vr) {
            checkgrow();
            head--;
            count++;
            if (head < 0) head += data.Length;
            data[head] = vr;
        }

        public void UnshiftN(Variable[] vrs) {
            for (int i = vrs.Length - 1; i >= 0; i--)
                Unshift(vrs[i]);
        }

        public Variable Shift() {
            int index = head++;
            if (head == data.Length) head = 0;
            count--;
            Variable d = data[index];
            data[index] = null;
            return d;
        }

        private void checkgrow() {
            if (count == data.Length - 1) {
                Variable[] ndata = new Variable[data.Length * 2];
                int z1 = data.Length - head;
                if (z1 >= count) {
                    Array.Copy(data, head, ndata, 0, count);
                } else {
                    Array.Copy(data, head, ndata, 0, z1);
                    int z2 = count - z1;
                    Array.Copy(data, 0, ndata, z1, z2);
                }
                data = ndata;
                head = 0;
            }
        }
    }
}

// The root setting
public class NULL {
    public static Niecza.Frame Environment = null;

    private static Niecza.SubInfo MAINSI = new Niecza.SubInfo(MAIN);
    public static Niecza.IP6 Installer = Niecza.Kernel.MakeSub(MAINSI, null);
    private static Niecza.Frame MAIN(Niecza.Frame th) {
        switch (th.ip) {
            case 0:
                th.ip = 1;
                return Niecza.Kernel.Fetch(th, th.pos[0]);
            default:
                return ((Niecza.IP6)th.resultSlot).Invoke(th.caller,
                        new Niecza.Variable[0] {}, null);
        }
    }
    public static void Initialize() {}
}
