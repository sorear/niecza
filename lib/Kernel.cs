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
        public DynMetaObject mo;

        public virtual object GetSlot(string name) {
            throw new InvalidOperationException("no slots in this repr");
        }

        public virtual void SetSlot(string name, object v) {
            throw new InvalidOperationException("no slots in this repr");
        }

        protected Frame Fail(Frame caller, string msg) {
            return Kernel.Die(caller, msg + " in class " + mo.name);
        }

        // Most reprs won't have a concept of type objects
        public virtual bool IsDefined() { return true; }

        public Frame HOW(Frame caller) {
            caller.resultSlot = mo.how;
            return caller;
        }

        // include the invocant in the positionals!  it will not usually be
        // this, rather a container of this
        public virtual Frame InvokeMethod(Frame caller, string name,
                Variable[] pos, Dictionary<string, Variable> named) {
            DispatchEnt m;
            //Kernel.LogNameLookup(name);
            if (mo.mro_methods.TryGetValue(name, out m)) {
                Frame nf = caller.MakeChild(m.outer, m.info);
                nf.pos = pos;
                nf.named = named;
                nf.curDisp = m;
                return nf;
            }
            return Fail(caller, "Unable to resolve method " + name);
        }

        public IP6 GetTypeObject() {
            return mo.typeObject;
        }

        public string GetTypeName() {
            return mo.name;
        }

        public bool Isa(DynMetaObject mo) {
            return this.mo.HasMRO(mo);
        }

        public bool Does(DynMetaObject mo) {
            return this.mo.HasMRO(mo);
        }

        public Frame Invoke(Frame c, Variable[] p,
                Dictionary<string, Variable> n) {
            DynMetaObject.InvokeHandler ih = mo.mro_OnInvoke;
            if (ih != null) {
                return ih(this, c, p, n);
            } else {
                Variable[] np = new Variable[p.Length + 1];
                Array.Copy(p, 0, np, 1, p.Length);
                np[0] = Kernel.NewROScalar(this);
                return InvokeMethod(c, "INVOKE", np, n);
            }
        }
    }

    public sealed class DispatchEnt {
        public DispatchEnt next;
        public SubInfo info;
        public Frame outer;
        public IP6 ip6;

        public DispatchEnt(DispatchEnt next, IP6 ip6) {
            this.ip6 = ip6;
            this.next = next;
            DynObject d = (DynObject)ip6;
            this.outer = (Frame) d.slots[0];
            this.info = (SubInfo) d.slots[1];
        }
    }

    // A Variable is the meaning of function arguments, of any subexpression
    // except the targets of := and ::=.

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public abstract class Variable {
        public IP6 whence;

        // these should be treated as ro for the life of the variable
        public DynMetaObject type;
        public bool rw;
        public bool islist;

        public abstract IP6  Fetch();
        public abstract void Store(IP6 v);

        public abstract IP6  GetVar();

        public static readonly Variable[] None = new Variable[0];
    }

    public sealed class SimpleVariable: Variable {
        IP6 val;

        public SimpleVariable(bool rw, bool islist, DynMetaObject type, IP6 whence, IP6 val) {
            this.val = val; this.whence = whence; this.rw = rw;
            this.islist = islist; this.type = type;
        }

        public override IP6  Fetch()       { return val; }
        public override void Store(IP6 v)  {
            if (!rw) {
                throw new NieczaException("Writing to readonly scalar");
            }
            if (!v.Isa(type)) {
                throw new NieczaException("Nominal type check failed for scalar store; got " + v.mo.name + ", needed " + type.name + " or subtype");
            }
            val = v;
        }

        public override IP6  GetVar()      {
            DynObject d = new DynObject(Kernel.ScalarMO);
            d.slots[0] = this;
            return d;
        }
    }

    // Used to make Variable sharing explicit in some cases; will eventually be
    // the only way to share a bvalue
    public sealed class BValue {
        public Variable v;
        public BValue(Variable v) { this.v = v; }
    }

    // This stores all the invariant stuff about a Sub, i.e. everything
    // except the outer pointer.  Now distinct from protopads
    public class SubInfo {
        public int[] lines;
        public DynBlockDelegate code;
        public DynMetaObject mo;
        // for inheriting hints
        public SubInfo outer;
        public string name;
        public Dictionary<string, object> hints;
        // maybe should be a hint
        public LAD ltm;

        // records: $start-ip, $end-ip, $type, $goto, $lid
        public const int ON_NEXT = 1;
        public const int ON_LAST = 2;
        public const int ON_REDO = 3;
        public const int ON_RETURN = 4;
        public const int ON_DIE = 5;
        public const int ON_SUCCEED = 6;
        public const int ON_PROCEED = 7;
        public const int ON_GOTO = 8;
        public const int ON_NEXTDISPATCH = 9;
        public int[] edata;
        public string[] label_names;

        private static string[] controls = new string[] { "unknown", "next",
            "last", "redo", "return", "die", "succeed", "proceed", "goto",
            "nextsame/nextwith" };
        public static string DescribeControl(int type, Frame tgt, int lid,
                string name) {
            string ty = (type < controls.Length) ? controls[type] : "unknown";
            if (lid >= 0) {
                return ty + "(" + tgt.info.label_names[lid] + ", lexotic)";
            } else if (name != null) {
                return ty + "(" + name + ", dynamic)";
            } else {
                return ty;
            }
        }

        public int FindControlEnt(int ip, int ty, string name, int lid) {
            for (int i = 0; i < edata.Length; i+=5) {
                if (ip < edata[i] || ip >= edata[i+1])
                    continue;
                if (ty != edata[i+2])
                    continue;
                if (lid >= 0 && lid != edata[i+4])
                    continue;
                if (name != null && !name.Equals(label_names[edata[i+4]]))
                    continue;
                return edata[i+3];
            }
            return -1;
        }

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

        public SubInfo(string name, int[] lines, DynBlockDelegate code,
                SubInfo outer, LAD ltm, int[] edata, string[] label_names) {
            this.lines = lines;
            this.code = code;
            this.outer = outer;
            this.ltm = ltm;
            this.name = name;
            this.edata = edata;
            this.label_names = label_names;
        }

        public SubInfo(string name, DynBlockDelegate code) :
            this(name, null, code, null, null, new int[0], null) { }
    }

    // We need hashy frames available to properly handle BEGIN; for the time
    // being, all frames will be hashy for simplicity
    public class Frame: IP6 {
        public Frame caller;
        public Frame outer;
        public SubInfo info;
        // a doubly-linked list of frames being used by a given coroutine
        public Frame reusable_child;
        public Frame reuser;
        public object resultSlot = null;
        public int ip = 0;
        public DynBlockDelegate code;
        public Dictionary<string, object> lex;
        // statistically, most subs have between 1 and 4 anonymous lexicals
        public object lex0;
        public object lex1;
        public object lex2;
        public object lex3;

        public int lexi0;
        public int lexi1;

        public object[] lexn;

        public DispatchEnt curDisp;
        public RxFrame rx;

        public Variable[] pos;
        public Dictionary<string, Variable> named;

        // after MakeSub, GatherHelper
        public const int SHARED = 1;
        // nextsame; on binder failure
        public const int MULTIPHASE = 2;
        public int flags;

        public Frame(Frame caller_, Frame outer_,
                SubInfo info_) {
            caller = caller_;
            outer = outer_;
            code = info_.code;
            info = info_;
            mo = Kernel.CallFrameMO;
        }

        public Frame() { mo = Kernel.CallFrameMO; }

        public Frame MakeChild(Frame outer, SubInfo info) {
            if (reusable_child == null) {
                reusable_child = new Frame();
                reusable_child.reuser = this;
            }
            reusable_child.ip = 0;
            reusable_child.resultSlot = null;
            reusable_child.lexn = null;
            reusable_child.lex = null;
            reusable_child.lex0 = null;
            reusable_child.lex1 = null;
            reusable_child.lex2 = null;
            reusable_child.lex3 = null;
            reusable_child.curDisp = null;
            reusable_child.caller = this;
            reusable_child.outer = outer;
            reusable_child.info = info;
            reusable_child.code = info.code;
            reusable_child.rx = null;
            return reusable_child;
        }

        public Frame Continue() {
            return code(this);
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

        public void MarkShared() {
            if (0 == (flags & SHARED)) {
                flags |= SHARED;
                if (reuser != null) reuser.reusable_child = reusable_child;
                if (reusable_child != null) reusable_child.reuser = reuser;
                reuser = reusable_child = null;
            }
        }

        // when control might re-enter a function
        public void MarkSharedChain() {
            for (Frame x = this; x != null; x = x.caller)
                x.MarkShared();
        }

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

    public class NieczaException: Exception {
        // hide clr stack trace for these
        public override string ToString() { return Message; }
        public NieczaException(string detail) : base(detail) {}
        public NieczaException() : base() {}
    }

    // NOT IP6; these things should only be exposed through a ClassHOW-like
    // façade
    public class DynMetaObject {
        public IP6 how;
        public IP6 typeObject;
        public string name;

        public bool isRole;
        public IP6 roleFactory;
        public Dictionary<string, IP6> instCache;
        // role type objects have an empty MRO cache so no methods can be
        // called against them; the fallback (NYI) is to pun.

        public LexerCache lexcache;
        public LexerCache GetLexerCache() {
            if (lexcache == null)
                lexcache = new LexerCache();
            return lexcache;
        }

        public delegate Frame InvokeHandler(IP6 th, Frame c,
                Variable[] pos, Dictionary<string, Variable> named);

        public InvokeHandler OnInvoke;

        public InvokeHandler mro_OnInvoke;
        public Dictionary<string, DispatchEnt> mro_methods;

        public DynMetaObject[] local_does;

        public List<DynMetaObject> superclasses
            = new List<DynMetaObject>();
        public Dictionary<string, IP6> local
            = new Dictionary<string, IP6>();
        public Dictionary<string, IP6> priv
            = new Dictionary<string, IP6>();
        public List<string> local_attr = new List<string>();

        public Dictionary<string, int> slotMap = new Dictionary<string, int>();
        public int nslots = 0;
        public string[] all_attr;

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

        public Dictionary<DynMetaObject, DynMetaObject> butCache;

        public DynMetaObject(string name) {
            this.name = name;
            this.wr_this = new WeakReference(this);

            isa = new HashSet<DynMetaObject>();
        }

        private void Revalidate() {
            mro_OnInvoke = null;
            mro_methods = new Dictionary<string,DispatchEnt>();

            if (mro == null)
                return;
            if (isRole)
                return;

            for (int kx = mro.Length - 1; kx >= 0; kx--) {
                DynMetaObject k = mro[kx];
                if (k.OnInvoke != null)
                    mro_OnInvoke = k.OnInvoke;

                foreach (KeyValuePair<string,IP6> m in k.local) {
                    DispatchEnt de;
                    mro_methods.TryGetValue(m.Key, out de);
                    mro_methods[m.Key] = new DispatchEnt(de, m.Value);
                }
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
            isa.Clear();
            foreach (DynMetaObject k in arr)
                isa.Add(k);
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
            DispatchEnt m;
            if (mro_methods.TryGetValue(name, out m))
                return m.ip6; // TODO return an iterator
            return null;
        }

        public Dictionary<string,DispatchEnt> AllMethods() {
            return mro_methods;
        }

        public HashSet<IP6> AllMethodsSet() {
            HashSet<IP6> r = new HashSet<IP6>();
            foreach (KeyValuePair<string,DispatchEnt> kv in mro_methods)
                r.Add(kv.Value.ip6);
            return r;
        }

        public bool HasMRO(DynMetaObject m) {
            return isa.Contains(m);
        }

        public void AddMethod(string name, IP6 code) {
            local[name] = code;
            Invalidate();
        }

        public void AddPrivateMethod(string name, IP6 code) {
            priv[name] = code;
        }

        public IP6 GetPrivateMethod(string name) {
            IP6 code = priv[name];
            if (code == null) { throw new NieczaException("private method lookup failed for " + name + " in class " + this.name); }
            return code;
        }


        public void FillProtoClass(string[] attr) {
            FillClass(attr, attr, new DynMetaObject[] {},
                    new DynMetaObject[] { this });
        }

        public void FillClass(string[] local_attr, string[] all_attr,
                DynMetaObject[] superclasses, DynMetaObject[] mro) {
            this.superclasses = new List<DynMetaObject>(superclasses);
            SetMRO(mro);
            this.local_attr = new List<string>(local_attr);
            this.butCache = new Dictionary<DynMetaObject, DynMetaObject>();
            this.all_attr = all_attr;
            this.local_does = new DynMetaObject[0];

            nslots = 0;
            foreach (string an in all_attr) {
                slotMap[an] = nslots++;
            }

            Invalidate();
        }

        public void FillRole(string[] attr, DynMetaObject[] superclasses,
                DynMetaObject[] cronies) {
            this.superclasses = new List<DynMetaObject>(superclasses);
            this.local_attr = new List<string>(attr);
            this.local_does = cronies;
            this.isRole = true;
            Revalidate(); // need to call directly as we aren't in any mro list
            SetMRO(Kernel.AnyMO.mro);
        }

        public void FillParametricRole(IP6 factory) {
            this.isRole = true;
            this.roleFactory = factory;
            this.instCache = new Dictionary<string, IP6>();
            Revalidate();
            SetMRO(Kernel.AnyMO.mro);
        }
    }

    // This is quite similar to DynFrame and I wonder if I can unify them.
    // These are always hashy for the same reason as Frame above
    public class DynObject: IP6 {
        // the slots have to support non-containerized values, because
        // containers are objects now
        public object[] slots;

        public DynObject(DynMetaObject klass) {
            this.mo = klass;
            this.slots = new object[klass.nslots];
        }

        public override void SetSlot(string name, object obj) {
            slots[mo.FindSlot(name)] = obj;
        }

        public override object GetSlot(string name) {
            return slots[mo.FindSlot(name)];
        }

        public override bool IsDefined() {
            return this != mo.typeObject;
        }
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
            Frame n = th.MakeChild((Frame) dyo.slots[0],
                    (SubInfo) dyo.slots[1]);
            n.MarkSharedChain();
            th.resultSlot = n;
            return th;
        }

        private static Frame SubInvoke(IP6 th, Frame caller,
                Variable[] pos, Dictionary<string,Variable> named) {
            DynObject dyo = ((DynObject) th);
            Frame outer = (Frame) dyo.slots[0];
            SubInfo info = (SubInfo) dyo.slots[1];

            Frame n = caller.MakeChild(outer, info);
            n.pos = pos;
            n.named = named;

            return n;
        }
        private static SubInfo SubInvokeSubSI = new SubInfo("Sub.INVOKE", SubInvokeSubC);
        private static Frame SubInvokeSubC(Frame th) {
            Variable[] post;
            post = new Variable[th.pos.Length - 1];
            Array.Copy(th.pos, 1, post, 0, th.pos.Length - 1);
            return SubInvoke((DynObject)th.pos[0].Fetch(), th.caller,
                    post, th.named);
        }

        public static Frame Die(Frame caller, string msg) {
            DynObject n = new DynObject(((DynObject)StrP).mo);
            n.slots[0] = msg;
            return SearchForHandler(caller, SubInfo.ON_DIE, null, -1, null,
                    NewROScalar(n));
        }

        public static Frame BindFail(Frame caller, string msg) {
            // TODO: Junctional failover goes here
            if ((caller.flags & Frame.MULTIPHASE) != 0) {
                // TODO: Figure out how .*foo fits in here.
                Variable[] p = caller.pos;
                Dictionary<string,Variable> n = caller.named;
                DispatchEnt de = caller.curDisp.next;
                if (de == null) {
                    return Die(caller.caller, "Multiple dispatch failed to find a candidate");
                }
                caller = caller.caller.MakeChild(de.outer, de.info);
                caller.pos = p;
                caller.named = n;
                caller.curDisp = de;
                caller.flags |= Frame.MULTIPHASE;
                return caller;
            }
            return Die(caller, msg);
        }

        public static Frame CheckArgEnd(Frame caller, int i, string m) {
            // TODO: checking for SigCheckOnly goes here
            if (i == caller.pos.Length &&
                    (caller.named == null || caller.named.Count == 0)) {
                caller.flags &= ~Frame.MULTIPHASE;
                return caller;
            } else {
                return BindFail(caller, m);
            }
        }

        public static readonly DynMetaObject SubMO;
        public static readonly DynMetaObject ScalarMO;
        public static readonly DynMetaObject StashMO;

        public static readonly IP6 StashP;

        public static bool TraceCont;

        public static IP6 MakeSub(SubInfo info, Frame outer) {
            DynObject n = new DynObject(info.mo ?? SubMO);
            n.slots[0] = outer;
            if (outer != null) outer.MarkShared();
            n.slots[1] = info;
            return n;
        }

        public static DynObject MockBox(object v) {
            DynObject n = new DynObject(StrP.mo);
            n.slots[0] = v;
            return n;
        }

        public static Variable BoxAny(object v, IP6 proto) {
            if (v == null)
                return NewROScalar(proto);
            DynObject n = new DynObject(((DynObject)proto).mo);
            n.slots[0] = v;
            return NewROScalar(n);
        }

        // check whence before calling
        public static Frame Vivify(Frame th, Variable v) {
            IP6 w = v.whence;
            v.whence = null;
            return w.Invoke(th, new Variable[1] { v }, null);
        }

        private static SubInfo BindSI = new SubInfo("Bind/rw-viv", BindC);
        private static Frame BindC(Frame th) {
            switch (th.ip) {
                case 0:
                    th.ip = 1;
                    return Vivify(th, th.pos[0]);
                case 1:
                    return th.caller;
                default:
                    return Kernel.Die(th, "IP invalid");
            }
        }

        public static Frame NewBoundVar(Frame th, bool ro, bool islist,
                DynMetaObject type, Variable rhs) {
            Frame n;
            if (islist) ro = true;
            if (!rhs.rw) ro = true;
            // fast path
            if (ro == !rhs.rw && islist == rhs.islist && rhs.whence == null) {
                if (!rhs.type.HasMRO(type))
                    return Kernel.Die(th, "Nominal type check failed in binding; got " + rhs.type.name + ", needed " + type.name);
                th.resultSlot = rhs;
                return th;
            }
            // ro = true and rhs.rw = true OR
            // islist != rhs.islist OR
            // whence != null (and rhs.rw = true)

            if (!rhs.rw) {
                IP6 v = rhs.Fetch();
                if (!v.Isa(type))
                    return Kernel.Die(th, "Nominal type check failed in binding; got " + v.mo.name + ", needed " + type.name);
                th.resultSlot = new SimpleVariable(false, islist, v.mo, null, v);
                return th;
            }
            // ro = true and rhw.rw = true OR
            // whence != null
            if (ro) {
                IP6 v = rhs.Fetch();
                if (!v.Isa(type))
                    return Kernel.Die(th, "Nominal type check failed in binding; got " + v.mo.name + ", needed " + type.name);
                th.resultSlot = new SimpleVariable(false, islist, v.mo, null, rhs.Fetch());
                return th;
            }

            if (!rhs.type.HasMRO(type))
                return Kernel.Die(th, "Nominal type check failed in binding; got " + rhs.type.name + ", needed " + type.name);

            th.resultSlot = rhs;

            n = th.MakeChild(null, BindSI);
            n.pos = new Variable[1] { rhs };
            return n;
        }

        // This isn't just a fetch and a store...
        private static SubInfo AssignSI = new SubInfo("Assign", AssignC);
        private static Frame AssignC(Frame th) {
            switch (th.ip) {
                case 0:
                    if (th.pos[0].whence == null)
                        goto case 1;
                    th.ip = 1;
                    return Vivify(th, th.pos[0]);
                case 1:
                    if (th.pos[0].islist) {
                        return th.pos[0].Fetch().InvokeMethod(th.caller,
                                "LISTSTORE", th.pos, null);
                    } else if (!th.pos[0].rw) {
                        return Kernel.Die(th.caller, "assigning to readonly value");
                    } else {
                        th.pos[0].Store(th.pos[1].Fetch());
                    }
                    return th.caller;
                default:
                    return Kernel.Die(th, "Invalid IP");
            }
        }

        public static Frame Assign(Frame th, Variable lhs, Variable rhs) {
            if (lhs.whence == null && !lhs.islist) {
                if (!lhs.rw) {
                    return Kernel.Die(th, "assigning to readonly value");
                }

                lhs.Store(rhs.Fetch());
                return th;
            }

            Frame n = th.MakeChild(null, AssignSI);
            n.pos = new Variable[2] { lhs, rhs };
            return n;
        }

        // ro, not rebindable
        public static Variable NewROScalar(IP6 obj) {
            return new SimpleVariable(false, false, obj.mo, null, obj);
        }

        public static Variable NewRWScalar(DynMetaObject t, IP6 obj) {
            return new SimpleVariable(true, false, t, null, obj);
        }

        public static Variable NewRWListVar(IP6 container) {
            return new SimpleVariable(false, true, container.mo, null,
                    container);
        }

        public static VarDeque SlurpyHelper(Frame th, int from) {
            VarDeque lv = new VarDeque();
            for (int i = from; i < th.pos.Length; i++) {
                lv.Push(th.pos[i]);
            }
            return lv;
        }

        // TODO: Returning keys unboxed would be better.
        public static VarDeque KeysHelper(Dictionary<string,Variable> d) {
            VarDeque lv = new VarDeque();
            foreach (string s in d.Keys) {
                lv.Push(BoxAny(s, StrP));
            }
            return lv;
        }

        public static VarDeque SortHelper(Frame th, IP6 cb, VarDeque from) {
            Variable[] tmp = from.CopyAsArray();
            Array.Sort(tmp, delegate (Variable v1, Variable v2) {
                Frame end  = th.MakeChild(null, ExitRunloopSI);
                Frame call = cb.Invoke(end, new Variable[] { v1, v2 }, null);
                RunCore(call);
                return (int)(double)UnboxAny(
                    ((Variable)end.resultSlot).Fetch());
            });
            return new VarDeque(tmp);
        }

        public static Variable ContextHelper(Frame th, string name, int up) {
            object rt;
            while (th != null) {
                if (up <= 0 && th.lex != null &&
                        th.lex.TryGetValue(name, out rt)) {
                    return (Variable)rt;
                }
                th = th.caller;
                up--;
            }
            name = name.Remove(1,1);
            Dictionary<string,BValue> gstash = (Dictionary<string,BValue>)
                UnboxAny(GlobalO);
            BValue v;

            if (gstash.TryGetValue(name, out v)) {
                return v.v;
            } else {
                return PackageLookup(ProcessO, name).v;
            }
        }

        public static Variable DefaultNew(IP6 proto) {
            DynObject n = new DynObject(((DynObject)proto).mo);
            DynMetaObject[] mro = n.mo.mro;

            for (int i = mro.Length - 1; i >= 0; i--) {
                foreach (string s in mro[i].local_attr) {
                    n.SetSlot(s, NewRWScalar(AnyMO, AnyP));
                }
            }

            return NewROScalar(n);
        }

        public static Frame PromoteToList(Frame th, Variable v) {
            if (!v.islist) {
                DynObject lst = new DynObject(RxFrame.ListMO);
                lst.slots[0 /*items*/] = new VarDeque(new Variable[] { v });
                lst.slots[1 /*rest*/ ] = new VarDeque();
                lst.slots[2 /*flat*/ ] = false;
                th.resultSlot = Kernel.NewRWListVar(lst);
                return th;
            }
            IP6 o = v.Fetch();
            if (o.Isa(RxFrame.ListMO)) {
                th.resultSlot = v;
                return th;
            }
            return o.InvokeMethod(th, "list", new Variable[] { v }, null);
        }

        public static Frame GetFirst(Frame th, Variable lst) {
            if (!lst.islist) {
                th.resultSlot = lst;
                return th;
            }
            DynObject dyl = lst.Fetch() as DynObject;
            if (dyl == null) goto slow;
            if (dyl.mo != RxFrame.ListMO) goto slow;
            VarDeque itemsl = (VarDeque) dyl.GetSlot("items");
            if (itemsl.Count() == 0) goto slow;
            th.resultSlot = itemsl[0];
            return th;

slow:
            return lst.Fetch().InvokeMethod(th, "head", new Variable[] {
                    lst }, null);
        }

        public static DynMetaObject AnyMO;
        public static IP6 AnyP;
        public static IP6 ArrayP;
        public static IP6 HashP;
        public static IP6 StrP;
        public static DynMetaObject CallFrameMO;

        public static BValue PackageLookup(IP6 parent, string name) {
            Dictionary<string,BValue> stash = (Dictionary<string,BValue>)
                UnboxAny(parent);
            BValue v;

            if (stash.TryGetValue(name, out v)) {
                return v;
            } else if (name.EndsWith("::")) {
                Dictionary<string,BValue> newstash =
                    new Dictionary<string,BValue>();
                newstash["PARENT::"] = new BValue(NewROScalar(parent));
                return (stash[name] = new BValue(BoxAny(newstash,
                                StashP)));
            } else if (name.StartsWith("@")) {
                Frame nr = new Frame(null, null, ExitRunloopSI);
                nr = ArrayP.InvokeMethod(nr, "new", new Variable[] { Kernel.NewROScalar(ArrayP) }, null);
                RunCore(nr);
                return (stash[name] = new BValue((Variable)nr.caller.resultSlot));
            } else if (name.StartsWith("%")) {
                Frame nr = new Frame(null, null, ExitRunloopSI);
                nr = HashP.InvokeMethod(nr, "new", new Variable[] { Kernel.NewROScalar(HashP) }, null);
                RunCore(nr);
                return (stash[name] = new BValue((Variable)nr.caller.resultSlot));
            } else {
                // TODO: @foo, %foo
                return (stash[name] = new BValue(NewRWScalar(AnyMO, AnyP)));
            }
        }

        private static SubInfo IRSI = new SubInfo("InstantiateRole", IRC);
        private static Frame IRC(Frame th) {
            switch (th.ip) {
                case 0:
                    {
                        string s = "";
                        th.lex0 = th.pos[0].Fetch().mo;
                        for (int i = 1; i < th.pos.Length; i++) {
                            string p = (string)UnboxAny(th.pos[i].Fetch());
                            s += new string((char)p.Length, 1);
                            s += p;
                        }
                        th.lex1 = s;
                        bool ok;
                        IP6 r;
                        lock (th.lex0)
                            ok = ((DynMetaObject) th.lex0).instCache.
                                TryGetValue((string) th.lex1, out r);
                        if (ok) {
                            th.caller.resultSlot = NewROScalar(r);
                            return th.caller;
                        }
                        th.ip = 1;
                        Variable[] to_pass = new Variable[th.pos.Length - 1];
                        Array.Copy(th.pos, 1, to_pass, 0, th.pos.Length - 1);
                        return ((DynMetaObject) th.lex0).roleFactory.
                            Invoke(th, to_pass, null);
                    }
                case 1:
                    lock (th.lex0) {
                        ((DynMetaObject) th.lex0).instCache[(string) th.lex1]
                            = ((Variable) th.resultSlot).Fetch();
                    }
                    th.caller.resultSlot = th.resultSlot;
                    return th.caller;
                default:
                    return Die(th, "Invalid IP");
            }
        }
        public static Frame InstantiateRole(Frame th, Variable[] pcl) {
            Frame n = th.MakeChild(null, IRSI);
            n.pos = pcl;
            return n;
        }

        private static DynMetaObject DoRoleApply(DynMetaObject b,
                DynMetaObject role) {
            DynMetaObject n = new DynMetaObject(b.name + " but " + role.name);
            if (role.local_attr.Count != 0)
                throw new NieczaException("RoleApply with attributes NYI");
            if (role.superclasses.Count != 0)
                throw new NieczaException("RoleApply with superclasses NYI");
            DynMetaObject[] nmro = new DynMetaObject[b.mro.Length + 1];
            Array.Copy(b.mro, 0, nmro, 1, b.mro.Length);
            nmro[0] = n;
            n.FillClass(b.local_attr.ToArray(), b.all_attr,
                    new DynMetaObject[] { b }, nmro);
            foreach (KeyValuePair<string, IP6> kv in role.priv)
                n.AddPrivateMethod(kv.Key, kv.Value);
            foreach (KeyValuePair<string, IP6> kv in role.local)
                n.AddMethod(kv.Key, kv.Value);
            if (role.multiregex != null)
                foreach (KeyValuePair<string, List<DynObject>> kv
                        in role.multiregex)
                    foreach (DynObject dx in kv.Value)
                        n.AddMultiRegex(kv.Key, dx);

            n.how = BoxAny(n, b.how).Fetch();
            n.typeObject = new DynObject(n);
            ((DynObject)n.typeObject).slots = null;

            return n;
        }

        public static DynMetaObject RoleApply(DynMetaObject b,
                DynMetaObject role) {
            lock (b) {
                DynMetaObject rs;
                if (b.butCache.TryGetValue(role, out rs))
                    return rs;
                return b.butCache[role] = DoRoleApply(b, role);
            }
        }

        public static Frame StartP6Thread(Frame th, IP6 sub) {
            th.MarkSharedChain();
            Thread thr = new Thread(delegate () {
                    Frame mark = new Frame(th, null, ExitRunloopSI);
                    Frame cur = sub.Invoke(mark, Variable.None, null);
                    RunCore(cur);
                });
            thr.Start();
            th.resultSlot = thr;
            return th;
        }

        public static void RunLoop(SubInfo boot) {
            Kernel.TraceCont = (Environment.GetEnvironmentVariable("NIECZA_TRACE") != null);
            RunCore(new Frame(new Frame(null,null, ExitRunloopSI), null, boot));
        }

        class ExitRunloopException : Exception { }
        public static SubInfo ExitRunloopSI =
            new SubInfo("ExitRunloop", ExitRunloopC);
        private static Frame ExitRunloopC(Frame th) {
            throw new ExitRunloopException();
        }

        public static void RunCore(Frame cur) {
            for(;;) {
                try {
                    if (TraceCont) {
                        for(;;) {
                            System.Console.WriteLine("{0}|{1} @ {2}",
                                    cur.DepthMark(), cur.info.name, cur.ip);
                            cur = cur.code(cur);
                        }
                    } else {
                        for(;;)
                            cur = cur.code(cur);
                    }
                } catch (ExitRunloopException) {
                    return;
                } catch (Exception ex) {
                    cur = Kernel.Die(cur, ex.ToString());
                }
            }
        }

        public static void AddCap(List<Variable> p,
                Dictionary<string,Variable> n, IP6 cap) {
            Variable[] fp = cap.GetSlot("positionals") as Variable[];
            Dictionary<string,Variable> fn = cap.GetSlot("named")
                as Dictionary<string,Variable>;
            p.AddRange(fp);
            if (fn != null) AddMany(n, fn);
        }

        public static void AddMany(Dictionary<string,Variable> d1,
                Dictionary<string,Variable> d2) {
            foreach (KeyValuePair<string,Variable> kv in d2) {
                d1[kv.Key] = kv.Value;
            }
        }

        // used as the fallbacks for $*FOO
        public static IP6 GlobalO;
        public static IP6 ProcessO;

        static Kernel() {
            DynMetaObject pStrMO = new DynMetaObject("protoStr");
            pStrMO.FillProtoClass(new string[] { "value" });
            StrP = new DynObject(pStrMO);

            StashMO = new DynMetaObject("Stash");
            StashMO.FillProtoClass(new string[] { "value" });
            StashP = new DynObject(StashMO);

            SubMO = new DynMetaObject("Sub");
            SubMO.OnInvoke = new DynMetaObject.InvokeHandler(SubInvoke);
            SubMO.FillProtoClass(new string[] { "outer", "info" });
            SubMO.AddMethod("INVOKE", MakeSub(SubInvokeSubSI, null));

            ScalarMO = new DynMetaObject("Scalar");
            ScalarMO.FillProtoClass(new string[] { "value" });
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

        private static object in_unhandled;

        // exception processing goes in two stages
        // 1. find the correct place to unwind to, calling CATCH filters
        // 2. unwind, calling LEAVE functions
        public static Frame SearchForHandler(Frame th, int type, Frame tgt,
                int lid, string name, object payload) {
            // no CONTROL/CATCH yet, so we don't need to CPS the scanloop
            Frame csr;

            Frame unf = null;
            int unip = 0;

            for (csr = th; csr != null; csr = csr.caller) {
                if (type == SubInfo.ON_NEXTDISPATCH) {
                    if (csr.curDisp != null) {
                        unf = csr;
                        break;
                    }
                    continue;
                }
                // for lexoticism
                if (tgt != null && tgt != csr)
                    continue;
                unip = csr.info.FindControlEnt(csr.ip, type, name, lid);
                if (unip >= 0) {
                    unf = csr;
                    break;
                }
            }

            if (unf == null) {
                Variable mp = (type == SubInfo.ON_DIE) ? ((Variable)payload) :
                    BoxAny("Unhandled control operator: " +
                            SubInfo.DescribeControl(type, tgt, lid, name), StrP);
                if (in_unhandled != null) {
                    Console.Error.WriteLine("Double fault {0}", in_unhandled);
                    Environment.Exit(1);
                }
                in_unhandled = true;
                DynObject dob = mp.Fetch() as DynObject;
                if (dob != null && dob.slots.Length != 0 &&
                        dob.slots[0] is string) {
                    in_unhandled = dob.slots[0];
                }
                Frame r = th.MakeChild(null, UnhandledSI);
                r.lex0 = mp;
                return r;
            } else {
                return Unwind(th, type, unf, unip, payload);
            }
        }

        private static SubInfo UnhandledSI = new SubInfo("Unhandled", UnhandledC);
        private static Frame UnhandledC(Frame th) {
            switch(th.ip) {
                case 0:
                    th.ip = 1;
                    return ((Variable)th.lex0).Fetch().InvokeMethod(th, "Str",
                            new Variable[1] { (Variable)th.lex0 }, null);
                case 1:
                    Console.Error.WriteLine("Unhandled exception: {0}",
                            (string) UnboxAny(((Variable)th.resultSlot).Fetch()));
                    th.lex0 = th.caller;
                    goto case 2;
                case 2:
                    if (th.lex0 == null)
                        Environment.Exit(1);
                    Frame f = (Frame) th.lex0;
                    Console.Error.WriteLine("  at {0} line {1} ({2} @ {3})",
                            new object[] {
                                f.ExecutingFile(), f.ExecutingLine(),
                                f.info.name, f.ip });
                    th.lex0 = ((Frame) th.lex0).caller;
                    goto case 2;
                default:
                    return Kernel.Die(th, "Invalid IP");
            }
        }

        public static Frame Unwind(Frame th, int type, Frame tf, int tip,
                object td) {
            // LEAVE handlers aren't implemented yet.
            if (type == SubInfo.ON_NEXTDISPATCH) {
                // These are a bit special because there isn't actually a
                // catching frame.
                DispatchEnt de = tf.curDisp.next;
                DynObject o = td as DynObject;
                if (de != null) {
                    Variable[] p = tf.pos;
                    Dictionary<string, Variable> n = tf.named;
                    int fl = tf.flags & Frame.MULTIPHASE;
                    tf = tf.caller.MakeChild(de.outer, de.info);
                    if (o != null) {
                        tf.pos = (Variable[]) o.slots[0];
                        tf.named = o.slots[1] as Dictionary<string,Variable>;
                    } else {
                        tf.pos = p;
                        tf.named = n;
                    }
                    tf.curDisp = de;
                    tf.flags |= fl;
                    return tf;
                } else {
                    tf.caller.resultSlot = Kernel.NewROScalar(Kernel.AnyP);
                    return tf.caller;
                }
            } else if (type == SubInfo.ON_DIE) {
                if (tf.lex == null)
                    tf.lex = new Dictionary<string,object>();
                tf.lex["$*!"] = td;
                td = Kernel.NewROScalar(Kernel.AnyP);
            }
            tf.ip = tip;
            tf.resultSlot = td;
            return tf;
        }
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

        private void CopyToArray(Variable[] tg) {
            int z1 = data.Length - head;
            if (z1 >= count) {
                Array.Copy(data, head, tg, 0, count);
            } else {
                Array.Copy(data, head, tg, 0, z1);
                int z2 = count - z1;
                Array.Copy(data, 0, tg, z1, z2);
            }
        }

        public Variable[] CopyAsArray() {
            Variable[] ret = new Variable[count];
            CopyToArray(ret);
            return ret;
        }

        private void checkgrow() {
            if (count == data.Length) {
                Variable[] ndata = new Variable[data.Length * 2];
                CopyToArray(ndata);
                data = ndata;
                head = 0;
            }
        }
    }
}
