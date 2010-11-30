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
        public ViviHook whence;

        // these should be treated as ro for the life of the variable
        public DynMetaObject type;
        public bool rw;
        public bool islist;

        public abstract IP6  Fetch();
        public abstract void Store(IP6 v);

        public abstract IP6  GetVar();

        public static readonly Variable[] None = new Variable[0];
    }

    public abstract class ViviHook {
        public abstract Frame Do(Frame th, Variable toviv);
    }

    public class SubViviHook : ViviHook {
        IP6 sub;
        public SubViviHook(IP6 sub) { this.sub = sub; }
        public override Frame Do(Frame th, Variable toviv) {
            return sub.Invoke(th, new Variable[] { toviv }, null);
        }
    }

    public class HashViviHook : ViviHook {
        IP6 hash;
        string key;
        public HashViviHook(IP6 hash, string key) { this.hash = hash; this.key = key; }
        public override Frame Do(Frame th, Variable toviv) {
            Dictionary<string,Variable> rh =
                Kernel.UnboxAny<Dictionary<string,Variable>>(hash);
            rh[key] = toviv;
            return th;
        }
    }

    public class ArrayViviHook : ViviHook {
        IP6 ary;
        int key;
        public ArrayViviHook(IP6 ary, int key) { this.ary = ary; this.key = key; }
        public override Frame Do(Frame th, Variable toviv) {
            VarDeque vd = (VarDeque) ary.GetSlot("items");
            while (vd.Count() <= key)
                vd.Push(Kernel.NewRWScalar(Kernel.AnyMO, Kernel.AnyP));
            vd[key] = toviv;
            return th;
        }
    }

    public sealed class SimpleVariable: Variable {
        IP6 val;

        public SimpleVariable(bool rw, bool islist, DynMetaObject type, ViviHook whence, IP6 val) {
            this.val = val; this.whence = whence; this.rw = rw;
            this.islist = islist; this.type = type;
        }

        public override IP6  Fetch()       { return val; }
        public override void Store(IP6 v)  {
            if (!rw) {
                throw new NieczaException("Writing to readonly scalar");
            }
            if (!v.mo.HasMRO(type)) {
                throw new NieczaException("Nominal type check failed for scalar store; got " + v.mo.name + ", needed " + type.name + " or subtype");
            }
            val = v;
        }

        public override IP6  GetVar()      {
            return new BoxObject<SimpleVariable>(this, Kernel.ScalarMO);
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
        public Dictionary<string, int> dylex;
        public uint dylex_filter; // (32,1) Bloom on hash code
        public const uint FILTER_SALT = 0x9e3779b9;

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

        public static uint FilterForName(string name) {
            uint hash = (uint)(name.GetHashCode() * FILTER_SALT);
            return 1u << (int)(hash >> 27);
        }

        public SubInfo(string name, int[] lines, DynBlockDelegate code,
                SubInfo outer, LAD ltm, int[] edata, string[] label_names,
                string[] dylexn, int[] dylexi) {
            this.lines = lines;
            this.code = code;
            this.outer = outer;
            this.ltm = ltm;
            this.name = name;
            this.edata = edata;
            this.label_names = label_names;
            if (dylexn != null) {
                dylex = new Dictionary<string, int>();
                for (int i = 0; i < dylexn.Length; i++) {
                    dylex[dylexn[i]] = dylexi[i];
                    dylex_filter |= FilterForName(dylexn[i]);
                }
            }
        }

        public SubInfo(string name, DynBlockDelegate code) :
            this(name, null, code, null, null, new int[0], null, null, null) { }
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

        public static readonly bool TraceCalls =
            Environment.GetEnvironmentVariable("NIECZA_TRACE_CALLS") != null;
        public Frame MakeChild(Frame outer, SubInfo info) {
            if (reusable_child == null) {
                reusable_child = new Frame();
                reusable_child.reuser = this;
            }
            if (TraceCalls)
                Console.WriteLine("{0}\t{1}", this.info.name, info.name);
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

        public void SetDynamic(int ix, object v) {
            switch(ix) {
                case 0: lex0 = v; break;
                case 1: lex1 = v; break;
                case 2: lex2 = v; break;
                case 3: lex3 = v; break;
                default: lexn[ix-4] = v; break;
            }
        }

        public bool TryGetDynamic(string name, uint mask, out object v) {
            v = null;
            if (lex != null && lex.TryGetValue(name, out v))
                return true;
            if ((info.dylex_filter & mask) == 0)
                return false;
            int ix;
            if (!info.dylex.TryGetValue(name, out ix))
                return false;
            switch(ix) {
                case 0: v = lex0; break;
                case 1: v = lex1; break;
                case 2: v = lex2; break;
                case 3: v = lex3; break;
                default: v = lexn[ix-4]; break;
            }
            return true;
        }

        public Variable LexicalFind(string name) {
            Frame csr = this;
            uint m = SubInfo.FilterForName(name);
            while (csr != null) {
                object o;
                if (csr.TryGetDynamic(name, m, out o)) {
                    return (Variable)o;
                }
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

    public abstract class ContextHandler<T> {
        public abstract T Get(Variable obj);

    }

    // TODO: find out if generic sharing is killing performance
    class CtxCallMethodUnbox<T> : ContextHandler<T> {
        string method;
        public CtxCallMethodUnbox(string method) { this.method = method; }

        public override T Get(Variable obj) {
            Variable v = (Variable)Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, new Variable[] { obj }, null));
            return Kernel.UnboxAny<T>(v.Fetch());
        }
    }

    class CtxCallMethod : ContextHandler<Variable> {
        string method;
        public CtxCallMethod(string method) { this.method = method; }

        public override Variable Get(Variable obj) {
            return (Variable)Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, new Variable[] { obj }, null));
        }
    }

    class CtxJustUnbox<T> : ContextHandler<T> {
        public override T Get(Variable obj) {
            return Kernel.UnboxAny<T>(obj.Fetch());
        }
    }

    class CtxReturnSelf : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            return Kernel.NewROScalar(obj.Fetch());
        }
    }

    class CtxBoxify<T> : ContextHandler<Variable> {
        ContextHandler<T> inner;
        DynMetaObject box;
        public CtxBoxify(ContextHandler<T> inner, DynMetaObject box) {
            this.inner = inner;
            this.box = box;
        }
        public override Variable Get(Variable obj) {
            return Kernel.BoxAnyMO<T>(inner.Get(obj), box);
        }
    }

    class CtxParcelIterator : ContextHandler<VarDeque> {
        public override VarDeque Get(Variable obj) {
            return new VarDeque(Kernel.UnboxAny<Variable[]>(obj.Fetch()));
        }
    }

    class CtxListIterator : ContextHandler<VarDeque> {
        public override VarDeque Get(Variable obj) {
            DynObject d = (DynObject) obj.Fetch();
            VarDeque r = new VarDeque( (VarDeque) d.slots[0] );
            r.PushD((VarDeque) d.slots[1]);
            return r;
        }
    }

    class CtxHashIterator : ContextHandler<VarDeque> {
        public override VarDeque Get(Variable obj) {
            return Builtins.HashIterRaw(3, obj);
        }
    }

    class CtxRawNativeDefined : ContextHandler<bool> {
        public override bool Get(Variable obj) {
            return obj.Fetch().IsDefined();
        }
    }

    class CtxBoolNativeDefined : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            return obj.Fetch().IsDefined() ? Kernel.TrueV : Kernel.FalseV;
        }
    }

    class CtxRawNativeNum2Str : ContextHandler<string> {
        public override string Get(Variable obj) {
            return Kernel.UnboxAny<double>(obj.Fetch()).ToString();
        }
    }

    class CtxStrNativeNum2Str : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            return Kernel.BoxAnyMO<string>(Kernel.UnboxAny<double>(obj.Fetch()).ToString(), Kernel.StrMO);
        }
    }

    // NOT IP6; these things should only be exposed through a ClassHOW-like
    // façade
    public class DynMetaObject {
        public static readonly ContextHandler<Variable> CallStr
            = new CtxCallMethod("Str");
        public static readonly ContextHandler<Variable> CallBool
            = new CtxCallMethod("Bool");
        public static readonly ContextHandler<Variable> CallNumeric
            = new CtxCallMethod("Numeric");
        public static readonly ContextHandler<Variable> CallDefined
            = new CtxCallMethod("defined");
        public static readonly ContextHandler<Variable> CallIterator
            = new CtxCallMethod("defined");
        public static readonly ContextHandler<string> RawCallStr
            = new CtxCallMethodUnbox<string>("Str");
        public static readonly ContextHandler<bool> RawCallBool
            = new CtxCallMethodUnbox<bool>("Bool");
        public static readonly ContextHandler<double> RawCallNumeric
            = new CtxCallMethodUnbox<double>("Numeric");
        public static readonly ContextHandler<bool> RawCallDefined
            = new CtxCallMethodUnbox<bool>("defined");
        public static readonly ContextHandler<VarDeque> RawCallIterator
            = new CtxCallMethodUnbox<VarDeque>("iterator");

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
                lexcache = new LexerCache(this);
            return lexcache;
        }

        public delegate Frame InvokeHandler(IP6 th, Frame c,
                Variable[] pos, Dictionary<string, Variable> named);

        public ContextHandler<Variable> mro_Str, loc_Str, mro_Numeric,
                loc_Numeric, mro_Bool, loc_Bool, mro_defined, loc_defined,
                mro_iterator, loc_iterator;
        public ContextHandler<bool> mro_raw_Bool, loc_raw_Bool, mro_raw_defined,
                loc_raw_defined;
        public ContextHandler<string> mro_raw_Str, loc_raw_Str;
        public ContextHandler<double> mro_raw_Numeric, loc_raw_Numeric;
        public ContextHandler<VarDeque> mro_raw_iterator, loc_raw_iterator;

        public InvokeHandler OnInvoke;

        public InvokeHandler mro_OnInvoke;
        public Dictionary<string, DispatchEnt> mro_methods;

        public DynMetaObject[] local_does;

        public List<DynMetaObject> superclasses
            = new List<DynMetaObject>();
        public Dictionary<string, IP6> local
            = new Dictionary<string, IP6>();
        public List<KeyValuePair<string, IP6>> ord_methods
            = new List<KeyValuePair<string, IP6>>();
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
                foreach (KeyValuePair<string,IP6> m in k.ord_methods) {
                    DispatchEnt de;
                    mro_methods.TryGetValue(m.Key, out de);
                    mro_methods[m.Key] = new DispatchEnt(de, m.Value);
                    if (m.Key == "Numeric") {
                        mro_Numeric = CallNumeric;
                        mro_raw_Numeric = RawCallNumeric;
                    }
                    if (m.Key == "Bool") {
                        mro_Bool = CallBool;
                        mro_raw_Bool = RawCallBool;
                    }
                    if (m.Key == "Str") {
                        mro_Str = CallStr;
                        mro_raw_Str = RawCallStr;
                    }
                    if (m.Key == "defined") {
                        mro_defined = CallDefined;
                        mro_raw_defined = RawCallDefined;
                    }
                    if (m.Key == "iterator") {
                        mro_iterator = CallIterator;
                        mro_raw_iterator = RawCallIterator;
                    }
                }

                if (k.OnInvoke != null)
                    mro_OnInvoke = k.OnInvoke;
                if (k.loc_Numeric != null) mro_Numeric = k.loc_Numeric;
                if (k.loc_defined != null) mro_defined = k.loc_defined;
                if (k.loc_Bool != null) mro_Bool = k.loc_Bool;
                if (k.loc_Str != null) mro_Str = k.loc_Str;
                if (k.loc_iterator != null) mro_iterator = k.loc_iterator;
                if (k.loc_raw_Numeric != null) mro_raw_Numeric = k.loc_raw_Numeric;
                if (k.loc_raw_defined != null) mro_raw_defined = k.loc_raw_defined;
                if (k.loc_raw_Bool != null) mro_raw_Bool = k.loc_raw_Bool;
                if (k.loc_raw_Str != null) mro_raw_Str = k.loc_raw_Str;
                if (k.loc_raw_iterator != null) mro_raw_iterator = k.loc_raw_iterator;
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

        public void Invalidate() {
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
            int k = mro.Length;
            if (k >= 20) {
                return isa.Contains(m);
            } else {
                while (k != 0) {
                    if (mro[--k] == m) return true;
                }
                return false;
            }
        }

        public void AddMethod(string name, IP6 code) {
            local[name] = code;
            ord_methods.Add(new KeyValuePair<string,IP6>(name, code));
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
            this.slots = (klass.nslots != 0) ? new object[klass.nslots] : null;
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

    public class BoxObject<T> : DynObject {
        public T value;
        public BoxObject(T x, DynMetaObject klass) : base(klass) { value = x; }
    }

    // A bunch of stuff which raises big circularity issues if done in the
    // setting itself.
    public class Kernel {
        public static DynBlockDelegate MainlineContinuation;

        public static T UnboxAny<T>(IP6 o) {
            return ((BoxObject<T>)o).value;
        }

        public static Stack<Frame> TakeReturnStack = new Stack<Frame>();

        public static Frame Take(Frame th, Variable payload) {
            Frame r = TakeReturnStack.Pop();
            r.SetDynamic(r.info.dylex["$*nextframe"], NewROScalar(th));
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
            return SearchForHandler(caller, SubInfo.ON_DIE, null, -1, null,
                    BoxAnyMO<string>(msg, StrMO));
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
                if (i != caller.pos.Length)
                    m += string.Format(", used {0} of {1} positionals",
                            i, caller.pos.Length);
                if (caller.named != null && caller.named.Count != 0)
                    m += ", unused named " + JoinS(", ", caller.named.Keys);
                return BindFail(caller, m);
            }
        }

        public static IP6 SigSlurpCapture(Frame caller) {
            IP6 nw = new DynObject(CaptureMO);
            nw.SetSlot("positionals", caller.pos);
            nw.SetSlot("named", caller.named);
            caller.named = null;
            return nw;
        }

        public static DynMetaObject AnyMO;
        public static DynMetaObject PairMO;
        public static DynMetaObject CallFrameMO;
        public static DynMetaObject CaptureMO;
        public static DynMetaObject IteratorMO;
        public static DynMetaObject GatherIteratorMO;
        public static DynMetaObject IterCursorMO;
        public static DynMetaObject MatchMO;
        public static IP6 AnyP;
        public static IP6 ArrayP;
        public static IP6 EMPTYP;
        public static IP6 HashP;
        public static IP6 IteratorP;
        public static readonly DynMetaObject ScalarMO;
        public static readonly DynMetaObject StashMO;
        public static readonly DynMetaObject SubMO;
        public static readonly DynMetaObject StrMO;
        public static readonly DynMetaObject NumMO;
        public static readonly DynMetaObject ArrayMO;
        public static readonly DynMetaObject ParcelMO;
        public static readonly DynMetaObject ListMO;
        public static readonly DynMetaObject HashMO;
        public static readonly DynMetaObject BoolMO;
        public static readonly DynMetaObject MuMO;
        public static readonly IP6 StashP;

        public static readonly Variable TrueV;
        public static readonly Variable FalseV;

        public static IP6 MakeSub(SubInfo info, Frame outer) {
            DynObject n = new DynObject(info.mo ?? SubMO);
            n.slots[0] = outer;
            if (outer != null) outer.MarkShared();
            n.slots[1] = info;
            return n;
        }

        public static Variable BoxAny<T>(T v, IP6 proto) {
            if (proto == BoolMO.typeObject)
                return ((bool) (object) v) ? TrueV : FalseV;
            return NewROScalar(new BoxObject<T>(v, ((DynObject)proto).mo));
        }

        public static void SetBox<T>(IP6 obj, T v) {
            ((BoxObject<T>) obj).value = v;
        }

        public static Variable BoxAnyMO<T>(T v, DynMetaObject proto) {
            if (proto == BoolMO)
                return ((bool) (object) v) ? TrueV : FalseV;
            return NewROScalar(new BoxObject<T>(v, proto));
        }

        public static IP6 BoxRaw<T>(T v, DynMetaObject proto) {
            return new BoxObject<T>(v, proto);
        }

        // check whence before calling
        public static Frame Vivify(Frame th, Variable v) {
            ViviHook w = v.whence;
            v.whence = null;
            return w.Do(th, v);
        }

        // this exists purely to hide the return value
        private static SubInfo BindSI = new SubInfo("Bind/rw-viv", BindC);
        private static Frame BindC(Frame th) {
            switch (th.ip) {
                case 0:
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
                if (!v.mo.HasMRO(type))
                    return Kernel.Die(th, "Nominal type check failed in binding; got " + v.mo.name + ", needed " + type.name);
                th.resultSlot = new SimpleVariable(false, islist, v.mo, null, v);
                return th;
            }
            // ro = true and rhw.rw = true OR
            // whence != null
            if (ro) {
                IP6 v = rhs.Fetch();
                if (!v.mo.HasMRO(type))
                    return Kernel.Die(th, "Nominal type check failed in binding; got " + v.mo.name + ", needed " + type.name);
                th.resultSlot = new SimpleVariable(false, islist, v.mo, null, rhs.Fetch());
                return th;
            }

            if (!rhs.type.HasMRO(type))
                return Kernel.Die(th, "Nominal type check failed in binding; got " + rhs.type.name + ", needed " + type.name);

            th.resultSlot = rhs;

            n = Vivify(th.MakeChild(null, BindSI), rhs);
            return n;
        }

        // This isn't just a fetch and a store...
        public static SubInfo AssignSI = new SubInfo("Assign", AssignC);
        private static Frame AssignC(Frame th) {
            switch (th.ip) {
                case 0:
                    if (th.pos[0].whence == null)
                        goto case 1;
                    th.ip = 1;
                    Frame nth = Vivify(th, th.pos[0]);
                    if (nth == th) goto case 1;
                    return nth;
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

        public static VarDeque IterCopyElems(VarDeque vals) {
            VarDeque nv = new VarDeque();
            for (int i = 0; i < vals.Count(); i++)
                nv.Push(NewRWScalar(AnyMO, vals[i].Fetch()));
            return nv;
        }

        public static string[] commandArgs;
        public static Variable[] ArgsHelper() {
            List<Variable> lv = new List<Variable>();
            foreach (string s in commandArgs) {
                lv.Add(BoxAnyMO<string>(s, StrMO));
            }
            return lv.ToArray();
        }

        public static VarDeque SortHelper(Frame th, IP6 cb, VarDeque from) {
            Variable[] tmp = from.CopyAsArray();
            Array.Sort(tmp, delegate (Variable v1, Variable v2) {
                Variable v = RunInferior(cb.Invoke(GetInferiorRoot(),
                        new Variable[] { v1, v2 }, null));
                return (int)v.Fetch().mo.mro_raw_Numeric.Get(v);
            });
            return new VarDeque(tmp);
        }

        public static Variable ContextHelper(Frame th, string name, int up) {
            object rt;
            uint m = SubInfo.FilterForName(name);
            while (th != null) {
                if (up <= 0 && th.TryGetDynamic(name, m, out rt)) {
                    return (Variable)rt;
                }
                th = th.caller;
                up--;
            }
            name = name.Remove(1,1);
            BValue v;

            if (UnboxAny<Dictionary<string,BValue>>(GlobalO)
                    .TryGetValue(name, out v)) {
                return v.v;
            } else if (UnboxAny<Dictionary<string,BValue>>(ProcessO)
                    .TryGetValue(name, out v)) {
                return v.v;
            } else {
                return NewROScalar(AnyP);
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
                DynObject lst = new DynObject(Kernel.ListMO);
                lst.slots[0 /*items*/] = new VarDeque(new Variable[] { v });
                lst.slots[1 /*rest*/ ] = new VarDeque();
                th.resultSlot = Kernel.NewRWListVar(lst);
                return th;
            }
            IP6 o = v.Fetch();
            if (o.mo.HasMRO(Kernel.ListMO)) {
                th.resultSlot = v;
                return th;
            }
            return o.InvokeMethod(th, "list", new Variable[] { v }, null);
        }

        // An Iterator is a VarDeque, where each element is either:
        //   an IterCursor, representing work to be done lazily
        //   a value with islist, representing a flattenable sublist
        //   anything else, representing that value

        // Laziness dictates that IterCursors not be reified until necessary,
        // and any infinite or I/O-bearing tasks be wrapped in them.  Calls
        // to List.iterator, however, may be assumed cheap and done eagerly.

        public static void IterToList(IP6 list, VarDeque iter) {
            VarDeque items = new VarDeque();
            IP6 item;
            while (iter.Count() != 0) {
                item = iter[0].Fetch();
                if (item.mo.HasMRO(IterCursorMO)) {
                    break;
                } else {
                    items.Push(iter.Shift());
                }
            }
            list.SetSlot("items", items);
            list.SetSlot("rest", iter);
        }

        public static Frame IterFlatten(Frame caller, VarDeque iter) {
            Frame n = caller.MakeChild(null, IF_SI);
            n.lex0 = iter;
            n.lex1 = new VarDeque();
            return n;
        }
        private static SubInfo IF_SI = new SubInfo("iter_flatten", IF_C);
        private static Frame IF_C(Frame th) {
            VarDeque inq = (VarDeque) th.lex0;
            VarDeque outq = (VarDeque) th.lex1;
            Variable inq0v;
            IP6 inq0;
            switch (th.ip) {
                case 0:
                    if (inq.Count() == 0) {
                        th.caller.resultSlot = outq;
                        return th.caller;
                    }
                    inq0v = inq[0];
                    inq0 = inq0v.Fetch();
                    if (inq0v.islist) {
                        inq.Shift();
                        inq.UnshiftD(inq0.mo.mro_raw_iterator.Get(inq0v));
                        goto case 0;
                    }
                    if (inq0.mo.HasMRO(IterCursorMO)) {
                        th.MarkSharedChain();
                        th.ip = 1;
                        DynObject thunk = new DynObject(Kernel.GatherIteratorMO);
                        thunk.slots[0] = NewRWScalar(AnyMO, th);
                        thunk.slots[1] = NewRWScalar(AnyMO, AnyP);
                        outq.Push(NewROScalar(thunk));
                        th.caller.resultSlot = outq;
                        return th.caller;
                    }
                    outq.Push(inq0v);
                    inq.Shift();
                    goto case 0;
                case 1:
                    th.ip = 2;
                    return IterHasFlat(th, inq, true);
                case 2:
                    th.ip = 1;
                    if ((Boolean) th.resultSlot) {
                        return Take(th, inq.Shift());
                    } else {
                        return Take(th, NewROScalar(Kernel.EMPTYP));
                    }
                default:
                    return Die(th, "invalid IP");
            }
        }

        public static Frame IterHasFlat(Frame caller, VarDeque iter, bool flat) {
            while (true) {
                if (iter.Count() == 0) {
                    caller.resultSlot = false;
                    return caller;
                }
                Variable i0 = iter[0];
                if (i0.islist) {
                    iter.Shift();
                    iter.UnshiftD(i0.Fetch().mo.mro_raw_iterator.Get(i0));
                    continue;
                }
                if (i0.Fetch().mo.HasMRO(IterCursorMO)) {
                    Frame n = caller.MakeChild(null, IHF_SI);
                    n.lex0 = iter;
                    n.lexi0 = flat ? 1 : 0;
                    return n;
                }

                caller.resultSlot = true;
                return caller;
            }
        }
        private static SubInfo IHF_SI = new SubInfo("iter_has_flat", IHF_C);
        private static Frame IHF_C(Frame th) {
            VarDeque iter = (VarDeque) th.lex0;
            bool flat = th.lexi0 != 0;
            Variable f;
            IP6 ff;
            switch (th.ip) {
                case 0:
                    if (iter.Count() == 0) {
                        th.caller.resultSlot = false;
                        return th.caller;
                    }

                    f = iter[0];

                    if (f.islist && flat) {
                        iter.Shift();
                        iter.UnshiftD(f.Fetch().mo.mro_raw_iterator.Get(f));
                        goto case 0;
                    }

                    if ((ff = f.Fetch()).mo.HasMRO(IterCursorMO)) {
                        th.ip = 1;
                        iter.Shift();
                        return ff.InvokeMethod(th, "reify", new Variable[] { f }, null);
                    }

                    th.caller.resultSlot = true;
                    return th.caller;

                case 1:
                    iter.UnshiftN(UnboxAny<Variable[]>(((Variable)th.resultSlot).Fetch()));
                    goto case 0;
                default:
                    return Die(th, "invalid IP");
            }
        }

        public static Frame GetFirst(Frame th, Variable lst) {
            if (!lst.islist) {
                th.resultSlot = lst;
                return th;
            }
            DynObject dyl = lst.Fetch() as DynObject;
            if (dyl == null) goto slow;
            if (dyl.mo != Kernel.ListMO) goto slow;
            VarDeque itemsl = (VarDeque) dyl.GetSlot("items");
            if (itemsl.Count() == 0) goto slow;
            th.resultSlot = itemsl[0];
            return th;

slow:
            return lst.Fetch().InvokeMethod(th, "head", new Variable[] {
                    lst }, null);
        }

        public static BValue PackageLookup(IP6 parent, string name) {
            Dictionary<string,BValue> stash =
                UnboxAny<Dictionary<string,BValue>>(parent);
            BValue v;

            if (stash.TryGetValue(name, out v)) {
                return v;
            } else if (name.EndsWith("::")) {
                Dictionary<string,BValue> newstash =
                    new Dictionary<string,BValue>();
                newstash["PARENT::"] = new BValue(NewROScalar(parent));
                return (stash[name] = new BValue(BoxAny<Dictionary<string,BValue>>(newstash,
                                StashP)));
            } else if (name.StartsWith("@")) {
                Variable n = RunInferior(ArrayP.InvokeMethod(GetInferiorRoot(),
                            "new", new Variable[] {Kernel.NewROScalar(ArrayP)},
                            null));
                return (stash[name] = new BValue(n));
            } else if (name.StartsWith("%")) {
                Variable n = RunInferior(HashP.InvokeMethod(GetInferiorRoot(),
                            "new", new Variable[] {Kernel.NewROScalar(HashP)},
                            null));
                return (stash[name] = new BValue(n));
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
                        Variable[] to_pass = new Variable[th.pos.Length - 1];
                        bool cache_ok = true;
                        for (int i = 1; i < th.pos.Length; i++) {
                            IP6 obj = th.pos[i].Fetch();
                            to_pass[i-1] = NewROScalar(obj);
                            if (obj.mo == StrMO) {
                                string p = UnboxAny<string>(obj);
                                s += new string((char)p.Length, 1);
                                s += p;
                            } else { cache_ok = false; }
                        }
                        if (!cache_ok) {
                            return ((DynMetaObject) th.lex0).roleFactory.
                                Invoke(th.caller, to_pass, null);
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
            foreach (KeyValuePair<string, IP6> kv in role.ord_methods)
                n.AddMethod(kv.Key, kv.Value);
            n.Invalidate();

            n.how = BoxAny<DynMetaObject>(n, b.how).Fetch();
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
            string trace = Environment.GetEnvironmentVariable("NIECZA_TRACE");
            if (trace != null) {
                if (trace == "all") {
                    TraceFlags = TRACE_CUR;
                    TraceFreq = 1;
                } else if (trace == "stat") {
                    TraceFlags = TRACE_ALL;
                    string p = Environment.GetEnvironmentVariable("NIECZA_TRACE_PERIOD");
                    if (!int.TryParse(p, out TraceFreq))
                        TraceFreq = 1000000;
                } else {
                    Console.Error.WriteLine("Unknown trace option {0}", trace);
                }
                TraceCount = TraceFreq;
            }
            RunCore(new Frame(new Frame(null,null, ExitRunloopSI), null, boot));
        }

        class ExitRunloopException : Exception { }
        public static SubInfo ExitRunloopSI =
            new SubInfo("ExitRunloop", ExitRunloopC);
        private static Frame ExitRunloopC(Frame th) {
            throw new ExitRunloopException();
        }

        public const int TRACE_CUR = 1;
        public const int TRACE_ALL = 2;

        public static int TraceFreq;
        public static int TraceCount;
        public static int TraceFlags;

        private static void DoTrace(Frame cur) {
            TraceCount = TraceFreq;
            if ((TraceFlags & TRACE_CUR) != 0)
                System.Console.WriteLine("{0}|{1} @ {2}",
                        cur.DepthMark(), cur.info.name, cur.ip);
            if ((TraceFlags & TRACE_ALL) != 0) {
                Console.Error.WriteLine("Context:");
                DoBacktrace(cur);
            }
        }

        [ThreadStatic] private static Frame lastFrame;
        public static void RunCore(Frame cur) {
            for(;;) {
                try {
                    if (TraceCount != 0) {
                        for(;;) {
                            if (--TraceCount == 0)
                                DoTrace(cur);
                            lastFrame = cur = cur.code(cur);
                        }
                    } else {
                        for(;;)
                            lastFrame = cur = cur.code(cur);
                    }
                } catch (ExitRunloopException) {
                    return;
                } catch (Exception ex) {
                    lastFrame = cur = Kernel.Die(cur, ex.ToString());
                }
            }
        }

        public static Frame GetInferiorRoot() {
            Frame l = lastFrame;
            return (l == null ? new Frame(null, null, ExitRunloopSI)
                    : l.MakeChild(null, ExitRunloopSI));
        }

        public static Variable RunInferior(Frame f) {
            RunCore(f);
            Frame l = lastFrame;
            object r = l.resultSlot;
            lastFrame = l.caller;
            return (Variable)r;
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
            StrMO = new DynMetaObject("Str");
            StrMO.loc_Str = new CtxReturnSelf();
            StrMO.loc_raw_Str = new CtxJustUnbox<string>();
            StrMO.FillProtoClass(new string[] { });

            BoolMO = new DynMetaObject("Bool");
            BoolMO.loc_Bool = new CtxReturnSelf();
            BoolMO.loc_raw_Bool = new CtxJustUnbox<bool>();
            BoolMO.FillProtoClass(new string[] { });
            TrueV  = NewROScalar(BoxRaw<bool>(true,  BoolMO));
            FalseV = NewROScalar(BoxRaw<bool>(false, BoolMO));

            NumMO = new DynMetaObject("Num");
            NumMO.loc_Numeric = new CtxReturnSelf();
            NumMO.loc_raw_Numeric = new CtxJustUnbox<double>();
            NumMO.loc_Str = new CtxStrNativeNum2Str();
            NumMO.loc_raw_Str = new CtxRawNativeNum2Str();
            NumMO.FillProtoClass(new string[] { });

            MuMO = new DynMetaObject("Mu");
            MuMO.loc_Bool = MuMO.loc_defined = new CtxBoolNativeDefined();
            MuMO.loc_raw_Bool = MuMO.loc_raw_defined = new CtxRawNativeDefined();
            MuMO.loc_Numeric = DynMetaObject.CallNumeric;
            MuMO.loc_raw_Numeric = DynMetaObject.RawCallNumeric;
            MuMO.loc_Str = DynMetaObject.CallStr;
            MuMO.loc_raw_Str = DynMetaObject.RawCallStr;
            MuMO.loc_iterator = DynMetaObject.CallIterator;
            MuMO.loc_raw_iterator = DynMetaObject.RawCallIterator;
            MuMO.FillProtoClass(new string[] { });

            StashMO = new DynMetaObject("Stash");
            StashMO.FillProtoClass(new string[] { });
            StashP = new DynObject(StashMO);

            ParcelMO = new DynMetaObject("Parcel");
            ParcelMO.loc_raw_iterator = new CtxParcelIterator();
            ParcelMO.FillProtoClass(new string[] { });

            ArrayMO = new DynMetaObject("Array");
            ArrayMO.FillProtoClass(new string[] { "items", "rest" });

            ListMO = new DynMetaObject("List");
            ListMO.loc_raw_iterator = new CtxListIterator();
            ListMO.FillProtoClass(new string[] { "items", "rest" });

            HashMO = new DynMetaObject("Hash");
            HashMO.loc_raw_iterator = new CtxHashIterator();
            HashMO.FillProtoClass(new string[] { });

            SubMO = new DynMetaObject("Sub");
            SubMO.OnInvoke = new DynMetaObject.InvokeHandler(SubInvoke);
            SubMO.FillProtoClass(new string[] { "outer", "info" });
            SubMO.AddMethod("INVOKE", MakeSub(SubInvokeSubSI, null));
            SubMO.Invalidate();

            ScalarMO = new DynMetaObject("Scalar");
            ScalarMO.FillProtoClass(new string[] { });
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

        public static System.IO.TextReader OpenStdin() {
            return new System.IO.StreamReader(Console.OpenStandardInput(), Console.InputEncoding);
        }

        public static System.IO.TextWriter OpenStdout() {
            return new System.IO.StreamWriter(Console.OpenStandardOutput(), Console.OutputEncoding);
        }

        public static System.IO.TextWriter OpenStderr() {
            return new System.IO.StreamWriter(Console.OpenStandardError(), Console.OutputEncoding);
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
                    BoxAnyMO<string>("Unhandled control operator: " +
                            SubInfo.DescribeControl(type, tgt, lid, name), StrMO);
                if (in_unhandled != null) {
                    Console.Error.WriteLine("Double fault {0}", in_unhandled);
                    Environment.Exit(1);
                }
                in_unhandled = true;
                try {
                    in_unhandled = UnboxAny<string>(mp.Fetch());
                } catch (Exception) {}
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
                            UnboxAny<string>(((Variable)th.resultSlot).Fetch()));
                    DoBacktrace(th.caller);
                    Environment.Exit(1);
                    return null;
                default:
                    return Kernel.Die(th, "Invalid IP");
            }
        }

        public static void DoBacktrace(Frame from) {
            while (from != null) {
                Console.Error.WriteLine("  at {0} line {1} ({2} @ {3})",
                        new object[] {
                        from.ExecutingFile(), from.ExecutingLine(),
                        from.info.name, from.ip });
                from = from.caller;
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

        public VarDeque(Variable item) {
            data = new Variable[8];
            count = 1;
            data[0] = item;
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

        public void PushN(Variable[] vrs) {
            for (int i = 0; i < vrs.Length; i++)
                Push(vrs[i]);
        }

        public void PushD(VarDeque vrs) { PushN(vrs.CopyAsArray()); }
        public void UnshiftD(VarDeque vrs) { UnshiftN(vrs.CopyAsArray()); }

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
