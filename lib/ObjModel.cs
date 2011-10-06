using System;
using System.Collections.Generic;

namespace Niecza {
    public abstract class P6any {
        public STable mo;

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
                Variable[] pos, VarHash named) {
            DispatchEnt m;
            //Kernel.LogNameLookup(name);
            if (mo.mro_methods.TryGetValue(name, out m)) {
                return m.info.Binder(caller, m.outer, m.ip6,
                        pos, named, false, m);
            }
            if (mo.mro_methods.TryGetValue("FALLBACK", out m)) {
                Variable[] npos = new Variable[pos.Length + 1];
                Array.Copy(pos, 1, npos, 2, pos.Length - 1);
                npos[0] = pos[0];
                npos[1] = Kernel.BoxAnyMO(name, Kernel.StrMO);
                return m.info.Binder(caller, m.outer, m.ip6,
                        npos, named, false, m);
            }
            return Fail(caller, "Unable to resolve method " + name);
        }

        public P6any GetTypeObject() {
            return mo.typeObject;
        }

        public string GetTypeName() {
            return mo.name;
        }

        public bool Isa(STable mo) {
            return mo.isSubset ? mo.mo.CheckSubset(this) :
                this.mo.HasMRO(mo);
        }

        public bool Does(STable mo) {
            return mo.isSubset ? mo.mo.CheckSubset(this) :
                this.mo.HasMRO(mo);
        }

        public Frame Invoke(Frame c, Variable[] p, VarHash n) {
            return mo.mro_INVOKE.Invoke(this, c, p, n);
        }
    }

    public abstract class ContextHandler<T> {
        public abstract T Get(Variable obj);
    }

    public abstract class InvokeHandler {
        public abstract Frame Invoke(P6any obj, Frame th, Variable[] pos, VarHash named);
    }
    public abstract class PushyHandler {
        public abstract Variable Invoke(Variable obj, Variable[] args);
    }

    public abstract class BindHandler {
        public abstract Variable Bind(Variable obj, Variable key, Variable to);
    }

    public abstract class IndexHandler {
        public abstract Variable Get(Variable obj, Variable key);

        public virtual P6any GetWHO(P6any obj, string key) {
            Variable r = Get(Kernel.NewROScalar(obj),
                    Kernel.BoxAnyMO(key, Kernel.StrMO));
            return r.Fetch().mo.who;
        }

        public static Variable ViviHash(Variable obj, Variable key) {
            return new SimpleVariable(true, false, Kernel.MuMO,
                    new NewHashViviHook(obj, key.Fetch().mo.mro_raw_Str.Get(key)),
                    Kernel.AnyP);
        }
        public static Variable ViviArray(Variable obj, Variable key) {
            return new SimpleVariable(true, false, Kernel.MuMO,
                    new NewArrayViviHook(obj, (int)key.Fetch().mo.mro_raw_Numeric.Get(key)),
                    Kernel.AnyP);
        }

        protected Variable Slice(Variable obj, Variable key) {
            if (key.Fetch().mo.HasMRO(Kernel.JunctionMO)) {
                return Builtins.AutoThread(key.Fetch(), delegate (Variable v) {
                    return Get(obj, v); });
            }

            VarDeque iter = Builtins.start_iter(key);
            List<Variable> items = new List<Variable>();
            while (Kernel.IterHasFlat(iter, true))
                items.Add(Get(obj, iter.Shift()));
            // TODO: 1-element slices should be deparceled.  Requires
            // LISTSTORE improvements though.
            return Kernel.NewRWListVar(Kernel.BoxRaw<Variable[]>(
                        items.ToArray(), Kernel.ParcelMO));
        }
    }

    // NOT P6any; these things should only be exposed through a ClassHOW-like
    // façade
    public class P6how {
        public STable stable;

        public bool isRole, isSubset, isPackage, isComposed, isComposing;
        public string rtype = "class"; // XXX used for compiler's inspection
        public P6any roleFactory;
        public P6any subsetWhereThunk;
        public Variable subsetFilter;
        public Dictionary<string, P6any> instCache;
        // role type objects have an empty MRO cache so no methods can be
        // called against them; the fallback (NYI) is to pun.

        public Dictionary<STable, STable> butCache;

        public Dictionary<string, DispatchEnt> inherit_methods;

        public Dictionary<string, P6how.DispatchSet> up_protos;
        public List<DispatchSet> here_protos;
        public Dictionary<DispatchSet, List<MethodInfo>> multimethods;

        public STable[] local_does;

        public List<MethodInfo> lmethods = new List<MethodInfo>();
        public List<AttrInfo> local_attr = new List<AttrInfo>();

        public List<STable> superclasses = new List<STable>();
        internal SubscriberSet subclasses = new SubscriberSet();
        Subscription[] mro_sub;

        public STable[] mro = new STable[0];
        public HashSet<STable> isa = new HashSet<STable>();

        public struct AttrInfo {
            public string name;
            public P6any init;
            public int flags;
            public STable type;
            public string file;
            public int line;
        }

        public const int A_PUBLIC = 1;
        public const int A_ARRAY  = 2;
        public const int A_HASH   = 4;

        // 0 must be public only
        public const int V_PUBLIC    = 0;
        public const int V_PRIVATE   = 1;
        public const int V_SUBMETHOD = 2;
        public const int V_MASK      = 3;

        public const int M_ONLY  = 0;
        public const int M_PROTO = 4;
        public const int M_MULTI = 8;
        public const int M_MASK  = 12;

        public struct MethodInfo {
            public string short_name;
            public string long_name;

            public string Name() {
                return ((flags & P6how.M_MASK) != 0) ? long_name : short_name;
            }

            public P6any impl;

            public int flags;

            public string file;
            public int line;
        }

        public class DispatchSet {
            public STable defining_class;
            public string name;
            public P6any proto;
        }

        void CollectMMDs() {
            // Superclass data already collected
            up_protos = new Dictionary<string,DispatchSet>();
            here_protos = new List<DispatchSet>();
            multimethods = new Dictionary<DispatchSet, List<MethodInfo>>();
            for (int i = superclasses.Count - 1; i >= 0; i--)
                foreach (KeyValuePair<string,DispatchSet> kv in
                        superclasses[i].mo.up_protos)
                    up_protos[kv.Key] = kv.Value;
            DispatchSet ds;
            foreach (MethodInfo mi in lmethods) {
                if ((mi.flags & V_MASK) != V_PUBLIC)
                    continue;
                switch (mi.flags & M_MASK) {
                    case M_PROTO:
                        ds = new DispatchSet();
                        ds.proto = mi.impl;
                        ds.name  = mi.short_name;
                        ds.defining_class = stable;
                        here_protos.Add(ds);
                        up_protos[ds.name] = ds;
                        break;
                    case P6how.M_MULTI:
                        if (up_protos.ContainsKey(mi.short_name)
                                && up_protos[mi.short_name] != null) break;
                        ds = new DispatchSet();
                        ds.name = mi.short_name;
                        ds.defining_class = stable;
                        here_protos.Add(ds);
                        up_protos[ds.name] = ds;
                        break;
                    case P6how.M_ONLY:
                        up_protos[mi.short_name] = null;
                        break;
                }
            }

            foreach (STable k in mro) {
                HashSet<List<MethodInfo>> needs_break =
                    new HashSet<List<MethodInfo>>();
                foreach (MethodInfo mi in k.mo.lmethods) {
                    if (mi.flags != (V_PUBLIC | M_MULTI))
                        continue;
                    ds = k.mo.up_protos[mi.short_name];
                    List<MethodInfo> lmi;
                    if (!multimethods.TryGetValue(ds, out lmi))
                        multimethods[ds] = lmi = new List<MethodInfo>();
                    for (int ix = 0; ix < lmi.Count; ix++)
                        if (lmi[ix].long_name == mi.long_name)
                            goto next_method;
                    lmi.Add(mi);
                    needs_break.Add(lmi);
next_method: ;
                }
                foreach (List<MethodInfo> lmi in needs_break)
                    lmi.Add(new MethodInfo());
            }
        }

        internal void Revalidate() {
            inherit_methods = stable.mro_methods =
                new Dictionary<string,DispatchEnt>();
            stable.private_mro = new Dictionary<string,P6any>();

            if (mro == null)
                return;
            if (isRole)
                return;

            CollectMMDs();
            for (int kx = mro.Length - 1; kx >= 0; kx--)
                SetupMRO(mro[kx].mo);

            // XXX really ugly, doesn't do much except allow categoricals
            // to re-use lexers
            if (superclasses.Count == 1) {
                HashSet<string> names = new HashSet<string>(superclasses[0].mo.inherit_methods.Keys);
                foreach (MethodInfo mi in lmethods)
                    if ((mi.flags & V_MASK) == V_PUBLIC)
                        names.Remove(mi.short_name);
                foreach (string n in names) {
                    //Console.WriteLine("For {0}, removing dangerous override {1}", name, n);
                    inherit_methods[n] = superclasses[0].mo.inherit_methods[n];
                }
            }

            SetupPrivates();
            SetupBuildProgram();
        }

        void SetupMRO(P6how k) {
            foreach (MethodInfo m in k.lmethods) {
                DispatchEnt de;
                if ((m.flags & V_MASK) != V_PUBLIC)
                    continue;
                string n = m.Name();
                inherit_methods.TryGetValue(n, out de);
                inherit_methods[n] = new DispatchEnt(de, m.impl);
            }

            foreach (DispatchSet ds in k.here_protos) {
                List<MethodInfo> cands;
                if (!multimethods.TryGetValue(ds, out cands))
                    cands = new List<MethodInfo>();
                P6any[] cl = new P6any[cands.Count];
                for (int ix = 0; ix < cl.Length; ix++)
                    cl[ix] = cands[ix].impl;
                P6any disp = Kernel.MakeDispatcher(ds.name, ds.proto, cl);
                DispatchEnt de;
                inherit_methods.TryGetValue(ds.name, out de);
                inherit_methods[ds.name] = new DispatchEnt(de, disp);
            }
        }

        void SetupPrivates() {
            foreach (MethodInfo m in lmethods) {
                string n = m.Name();
                if ((m.flags & V_MASK) == V_PRIVATE) {
                    stable.private_mro[n] = m.impl;
                }
                else if ((m.flags & V_MASK) == V_SUBMETHOD) {
                    DispatchEnt de;
                    if (stable.mro_methods == inherit_methods)
                        stable.mro_methods = new Dictionary<string,DispatchEnt>(
                                inherit_methods);
                    stable.mro_methods.TryGetValue(n, out de);
                    stable.mro_methods[n] = new DispatchEnt(de, m.impl);
                }
            }
        }

        void SetupBuildProgram() {
            List<P6how.AttrInfo> acc = new List<P6how.AttrInfo>();
            P6how.AttrInfo n = new P6how.AttrInfo();

            for (int i = mro.Length - 1; i >= 0; i--) {
                P6any build = null;
                foreach (MethodInfo m in mro[i].mo.lmethods) {
                    if (m.Name() == "BUILD" && m.flags == V_SUBMETHOD)
                        build = m.impl;
                }
                foreach (P6how.AttrInfo ai in mro[i].mo.local_attr) {
                    n = ai;
                    if (build != null)
                        n.flags &= ~A_PUBLIC; // BUILD will handle setting
                    acc.Add(n);
                }
                if (build != null) {
                    n.init  = build;
                    n.name  = null;
                    acc.Add(n);
                }
            }

            stable.init_program = acc.ToArray();
        }

        private void SetMRO(STable[] arr) {
            if (mro_sub != null)
                foreach (Subscription k in mro_sub)
                    k.Terminate();
            mro_sub = new Subscription[arr.Length];
            for (int i = 0; i < arr.Length; i++)
                mro_sub[i] = new Subscription(stable, arr[i].mo.subclasses);
            mro = arr;
            isa.Clear();
            foreach (STable k in arr)
                isa.Add(k);
        }

        // invariant: if a class is dirty, so are all subclasses
        // invariant: mro-graph is acyclic
        void RevalidateTree(HashSet<STable> dirty) {
            if (!dirty.Contains(stable)) return;
            foreach (STable sp in mro)
                if (sp != stable) sp.mo.RevalidateTree(dirty);
            Revalidate();
            stable.SetupVTables();
            dirty.Remove(stable);
        }

        public void Invalidate() {
            BulkRevalidate(subclasses.GetSubscribers());
        }

        public static void BulkRevalidate(IEnumerable<object> set) {
            HashSet<STable> dirty = new HashSet<STable>();
            foreach (object k in set)
                if (k is STable) dirty.Add((STable)k);
            foreach (object k in set)
                if (k is STable) ((STable)k).mo.RevalidateTree(dirty);
        }

        public void AddMethod(int flags, string name, P6any code) {
            AddMethodPos(flags, name, code, "???", 0);
        }

        public void AddMethodPos(int flags, string name, P6any code, string file, int line) {
            MethodInfo mi = new MethodInfo();
            //SubInfo si = (SubInfo) code.GetSlot("info");
            mi.impl = code;
            mi.short_name = name;
            mi.long_name = name;
            mi.file = file;
            mi.line = line;

            if ((flags & M_MASK) != 0) {
                if ((flags & M_MASK) == M_PROTO) {
                    mi.long_name = mi.long_name + ":(proto)";
                } else if (code.mo.name == "Regex") {
                    int k = mi.short_name.IndexOf(':');
                    if (k >= 0) mi.short_name = mi.short_name.Substring(0, k);
                } else {
                    // TODO this should use the real longname, but there
                    // are currently order problems with that.
                    mi.long_name += ":(" + Kernel.MMDCandidateLongname.get_unique() + ")";
                }
            }
            //Console.WriteLine("{0} {1} {2} {3}", this.name, flags, mi.short_name, mi.long_name);
            mi.flags = flags;
            lmethods.Add(mi);
        }

        public void AddAttribute(string name, int flags, P6any init,
                STable type) {
            AddAttributePos(name,flags,init,type, "???", 0);
        }
        public void AddAttributePos(string name, int flags, P6any init,
                STable type, string file, int line) {
            AttrInfo ai;
            ai.name = name;
            ai.flags = flags;
            ai.init = init;
            ai.type = type;
            ai.file = file;
            ai.line = line;
            local_attr.Add(ai);
        }

        public void FillProtoClass(STable parent, string[] slots) {
            if (parent == null) {
                FillClass(slots, new STable[] {}, new STable[] { stable });
            } else {
                STable[] mro = new STable[parent.mo.mro.Length + 1];
                Array.Copy(parent.mo.mro, 0, mro, 1, mro.Length-1);
                mro[0] = stable;
                FillClass(slots, new STable[] { parent }, mro);
            }
            Invalidate();
        }

        public void FillSubset(STable super) {
            isSubset = stable.isSubset = true;
            STable[] mro = new STable[super.mo.mro.Length + 1];
            Array.Copy(super.mo.mro, 0, mro, 1, mro.Length - 1);
            mro[0] = stable;
            FillClass(super.all_slot, new STable[] { super }, mro);
        }

        public bool CheckSubset(P6any obj) {
            if (!obj.Does(superclasses[0]))
                return false;
            if (subsetFilter == null)
                subsetFilter = Kernel.RunInferior(subsetWhereThunk.Invoke(
                    Kernel.GetInferiorRoot(), Variable.None, null));
            Variable ret = Kernel.RunInferior(subsetFilter.Fetch().
                InvokeMethod(Kernel.GetInferiorRoot(), "ACCEPTS",
                    new Variable[] { subsetFilter, Kernel.NewROScalar(obj) },
                    null));
            return ret.Fetch().mo.mro_raw_Bool.Get(ret);
        }

        public void FillClass(string[] all_slot, STable[] superclasses,
                STable[] mro) {
            this.superclasses = new List<STable>(superclasses);
            SetMRO(mro);
            butCache = new Dictionary<STable, STable>();
            stable.all_slot = all_slot;
            local_does = new STable[0];

            stable.nslots = 0;
            foreach (string an in all_slot) {
                stable.slotMap[an] = stable.nslots++;
            }
        }

        public void FillRole(STable[] superclasses, STable[] cronies) {
            this.superclasses = new List<STable>(superclasses);
            local_does = cronies;
            isRole = true;
            SetMRO(Kernel.AnyMO.mo.mro);
        }

        public void FillParametricRole(P6any factory) {
            isRole = true;
            roleFactory = factory;
            instCache = new Dictionary<string, P6any>();
            SetMRO(Kernel.AnyMO.mo.mro);
        }

        string C3State(int[] pointers, List<STable> into, STable[][] from) {
            List<string> mergees = new List<string>();

            for (int i = 0; i < pointers.Length; i++) {
                List<string> elts = new List<string>();

                for (int j = pointers[i]; j < from[i].Length; j++)
                    elts.Add(from[i][j].name);

                mergees.Add(Kernel.JoinS(" ", elts));
            }

            return Kernel.JoinS(" ", into, st => st.name) + " <- " +
                Kernel.JoinS(" | ", mergees);
        }

        string C3Merge(List<STable> into, STable[][] from) {
            int[] pointers = new int[from.Length]; // all 0s
            Dictionary<STable, int> blocked = new Dictionary<STable, int>();

            foreach (STable[] list in from) {
                for (int i = 0; i < list.Length; i++) {
                    int k;
                    // set 1 block for each non-initial value used
                    blocked.TryGetValue(list[i], out k);
                    blocked[list[i]] = k + (i == 0 ? 0 : 1);
                }
            }

            while (true) {
                if (Config.C3Trace)
                    Console.WriteLine("C3 state: " + C3State(pointers, into, from));
                STable to_shift = null;
                STable k = null;
                for (int i = 0; i < from.Length; i++) {
                    if (pointers[i] < from[i].Length &&
                            blocked[k = from[i][pointers[i]]] == 0) {
                        to_shift = k;
                        break;
                    }
                }

                if (to_shift != null) {
                    for (int i = 0; i < from.Length; i++) {
                        if (pointers[i] < from[i].Length &&
                                from[i][pointers[i]] == to_shift) {
                            pointers[i]++;
                            if (pointers[i] < from[i].Length)
                                blocked[from[i][pointers[i]]]--;
                        }
                    }
                    into.Add(to_shift);
                } else {
                    bool bad = false;
                    for (int i = 0; i < from.Length; i++)
                        if (pointers[i] < from[i].Length)
                            bad = true;
                    if (bad)
                        return C3State(pointers, into, from);
                    else
                        return null;
                }
            }
        }

        public string Compose() {
            if (isComposed || rtype == "package" || rtype == "module")
                return null;
            if (isComposing)
                return "Circularity detected while composing " + stable.name;
            isComposing = true;
            string err;
            foreach (STable su in superclasses) {
                err = su.mo.Compose();
                if (err != null) return err;
            }
            isComposed = true;

            if (rtype == "role" || rtype == "prole") {
                isRole = true;
                SetMRO(Kernel.AnyMO.mo.mro);
                return null;
            }

            if (superclasses.Count == 0 && stable != Kernel.MuMO) {
                superclasses.Add(rtype == "grammar" ? Kernel.GrammarMO :
                        Kernel.AnyMO);
            }

            STable[][] lists = new STable[superclasses.Count + 1][];
            lists[0] = new STable[superclasses.Count + 1];
            lists[0][0] = stable;
            superclasses.CopyTo(lists[0], 1);
            for (int i = 0; i < superclasses.Count; i++)
                lists[i+1] = superclasses[i].mo.mro;

            List<STable> nmro = new List<STable>();
            err = C3Merge(nmro, lists);
            if (err != null)
                return "C3 MRO generation failed for " + stable.name + ": " + err;
            SetMRO(nmro.ToArray());

            List<string> all_slot_l = new List<string>();
            foreach (STable m in mro)
                foreach (AttrInfo ai in m.mo.local_attr)
                    all_slot_l.Add(ai.name);
            butCache = new Dictionary<STable, STable>();
            stable.all_slot = all_slot_l.ToArray();
            local_does = new STable[0];

            stable.nslots = 0;
            foreach (string an in stable.all_slot) {
                stable.slotMap[an] = stable.nslots++;
            }
            return null;
        }
    }

    // The role of STable is to hold stuff that needs to exist per
    // (representation, type) pair.  Note that types are somewhat
    // epiphenominal.  STable is also in charge of cachey bits of
    // the metamodel that need to be common a lot, like vtables.
    //
    // STables currenly exist in 1:1 correspondence with types, so
    // generally a single class is limited to a single representation.
    // (Although due to quirks of the C# implementation, Cursor and
    // BoxObject can share the P6opaque STable)
    public class STable {
        public static readonly ContextHandler<Variable> CallStr
            = new CtxCallMethod("Str");
        public static readonly ContextHandler<Variable> CallBool
            = new CtxCallMethod("Bool");
        public static readonly ContextHandler<Variable> CallNumeric
            = new CtxCallMethod("Numeric");
        public static readonly ContextHandler<Variable> CallDefined
            = new CtxCallMethod("defined");
        public static readonly ContextHandler<Variable> CallIterator
            = new CtxCallMethod("iterator");
        public static readonly ContextHandler<Variable> CallItem
            = new CtxCallMethod("item");
        public static readonly ContextHandler<Variable> CallList
            = new CtxCallMethod("list");
        public static readonly ContextHandler<Variable> CallHash
            = new CtxCallMethod("hash");
        public static readonly ContextHandler<Variable> CallShift
            = new CtxCallMethod("shift");
        public static readonly ContextHandler<Variable> CallPop
            = new CtxCallMethod("pop");
        public static readonly ContextHandler<P6any> CallPred
            = new CtxCallMethodFetch("pred");
        public static readonly ContextHandler<P6any> CallSucc
            = new CtxCallMethodFetch("succ");
        public static readonly ContextHandler<string> RawCallStr
            = new CtxCallMethodUnbox<string>("Str");
        public static readonly ContextHandler<bool> RawCallBool
            = new CtxCallMethodUnboxBool("Bool");
        public static readonly ContextHandler<double> RawCallNumeric
            = new CtxCallMethodUnboxNumeric("Numeric");
        public static readonly ContextHandler<bool> RawCallDefined
            = new CtxCallMethodUnboxBool("defined");
        public static readonly ContextHandler<VarDeque> RawCallIterator
            = new CtxCallMethodUnbox<VarDeque>("iterator");
        public static readonly ContextHandler<Variable[]> RawCallReify
            = new CtxCallMethodUnbox<Variable[]>("reify");
        public static readonly IndexHandler CallAtPos
            = new IxCallMethod("postcircumfix:<[ ]>", null);
        public static readonly IndexHandler CallAtKey
            = new IxCallMethod("postcircumfix:<{ }>", null);
        public static readonly IndexHandler CallExistsKey
            = new IxCallMethod("postcircumfix:<{ }>", "exists");
        public static readonly IndexHandler CallDeleteKey
            = new IxCallMethod("postcircumfix:<{ }>", "delete");
        public static readonly IndexHandler CallLISTSTORE
            = new IxCallMethod("LISTSTORE", null);
        public static readonly InvokeHandler CallINVOKE
            = new InvokeCallMethod();
        public static readonly PushyHandler CallPush
            = new PushyCallMethod("push");
        public static readonly PushyHandler CallUnshift
            = new PushyCallMethod("unshift");

        public P6how mo;

        public P6any how, who;
        public P6any typeObject, initObject;
        public Variable typeVar, initVar;
        public int fixups_from;
        public string name;
        public bool isSubset;

        public LexerCache lexcache;

        public ContextHandler<Variable> mro_Str, mro_Numeric, mro_Bool,
                mro_defined, mro_iterator, mro_item, mro_list, mro_hash,
                mro_shift, mro_pop;
        public ContextHandler<P6any> mro_pred, mro_succ;
        public ContextHandler<bool> mro_raw_Bool, mro_raw_defined;
        public ContextHandler<string> mro_raw_Str;
        public ContextHandler<double> mro_raw_Numeric;
        public ContextHandler<VarDeque> mro_raw_iterator;
        public ContextHandler<Variable[]> mro_raw_reify;
        public ContextHandler<object> mro_to_clr;
        public IndexHandler mro_at_pos, mro_at_key, mro_exists_key,
               mro_delete_key, mro_LISTSTORE;
        public BindHandler mro_bind_pos, mro_bind_key;

        public InvokeHandler mro_INVOKE;
        public PushyHandler mro_push, mro_unshift;

        public Dictionary<string, DispatchEnt> mro_methods;
        public Dictionary<string, P6any> private_mro;

        public Dictionary<string, int> slotMap = new Dictionary<string, int>();
        public int nslots = 0;
        public string[] all_slot;
        public P6how.AttrInfo[] init_program;

        public int num_rank = -1;
        public bool is_any = false;
        public Type box_type;

        public STable(string name) {
            this.name = name;
            mo = new P6how();
            mo.stable = this;
        }

        public int FindSlot(string name) {
            //Kernel.LogNameLookup(name);
            return slotMap[name];
        }

        public LexerCache GetLexerCache() {
            if (lexcache == null)
                lexcache = new LexerCache(this);
            return lexcache;
        }

        internal void SetupVTables() {
            mro_push = _GetVT("push") as PushyHandler ?? CallPush;
            mro_unshift = _GetVT("unshift") as PushyHandler ?? CallUnshift;
            mro_shift = _GetVT("shift") as ContextHandler<Variable> ?? CallShift;
            mro_pop = _GetVT("pop") as ContextHandler<Variable> ?? CallPop;
            mro_at_key = _GetVTi("postcircumfix:<{ }>", 0) as IndexHandler ?? CallAtKey;
            mro_at_pos = _GetVTi("postcircumfix:<[ ]>", 0) as IndexHandler ?? CallAtPos;
            mro_bind_key = _GetVTi("postcircumfix:<{ }>", 3) as BindHandler;
            mro_bind_pos = _GetVTi("postcircumfix:<[ ]>", 3) as BindHandler;
            mro_LISTSTORE = _GetVT("LISTSTORE") as IndexHandler ?? CallLISTSTORE;
            mro_Bool = _GetVT("Bool") as ContextHandler<Variable> ?? CallBool;
            mro_defined = _GetVT("defined") as ContextHandler<Variable> ?? CallDefined;
            mro_delete_key = _GetVTi("postcircumfix:<{ }>", 2) as IndexHandler ?? CallDeleteKey;
            mro_exists_key = _GetVTi("postcircumfix:<{ }>", 1) as IndexHandler ?? CallExistsKey;
            mro_hash = _GetVT("hash") as ContextHandler<Variable> ?? CallHash;
            mro_INVOKE = _GetVT("postcircumfix:<( )>") as InvokeHandler ?? CallINVOKE;
            mro_item = _GetVT("item") as ContextHandler<Variable> ?? CallItem;
            mro_iterator = _GetVT("iterator") as ContextHandler<Variable> ?? CallIterator;
            mro_list = _GetVT("list") as ContextHandler<Variable> ?? CallList;
            mro_Numeric = _GetVT("Numeric") as ContextHandler<Variable> ?? CallNumeric;
            mro_pred = _GetVTU("pred") as ContextHandler<P6any> ?? CallPred;
            mro_raw_Bool = _GetVTU("Bool") as ContextHandler<bool> ?? RawCallBool;
            mro_raw_defined = _GetVTU("defined") as ContextHandler<bool> ?? RawCallDefined;
            mro_raw_iterator = _GetVTU("iterator") as ContextHandler<VarDeque> ?? RawCallIterator;
            mro_raw_Numeric = _GetVTU("Numeric") as ContextHandler<double> ?? RawCallNumeric;
            mro_raw_reify = _GetVT("reify") as ContextHandler<Variable[]> ?? RawCallReify;
            mro_raw_Str = _GetVTU("Str") as ContextHandler<string> ?? RawCallStr;
            mro_Str = _GetVT("Str") as ContextHandler<Variable> ?? CallStr;
            mro_succ = _GetVTU("succ") as ContextHandler<P6any> ?? CallSucc;
            mro_to_clr = _GetVT("to-clr") as ContextHandler<object>;

            if (Kernel.ComplexMO != null && HasMRO(Kernel.ComplexMO))
                num_rank = Builtins.NR_COMPLEX;
            else if (Kernel.NumMO != null && HasMRO(Kernel.NumMO))
                num_rank = Builtins.NR_FLOAT;
            else if (Kernel.FatRatMO != null && HasMRO(Kernel.FatRatMO))
                num_rank = Builtins.NR_FATRAT;
            else if (Kernel.RatMO != null && HasMRO(Kernel.RatMO))
                num_rank = Builtins.NR_FIXRAT;
            else if (Kernel.IntMO != null && HasMRO(Kernel.IntMO))
                num_rank = Builtins.NR_FIXINT;
            else
                num_rank = -1;

            is_any = Kernel.AnyMO != null && HasMRO(Kernel.AnyMO);
        }

        private object _GetVT(string name) {
            DispatchEnt de;
            mro_methods.TryGetValue(name, out de);
            return de == null ? null : de.info.param1;
        }

        private object _GetVTi(string name, int ix) {
            DispatchEnt de;
            mro_methods.TryGetValue(name, out de);
            object[] ri = de == null ? null : de.info.param1 as object[];
            return ri == null ? null : ri[ix];
        }

        private object _GetVTU(string name) {
            DispatchEnt de;
            mro_methods.TryGetValue(name, out de);
            return de == null ? null : de.info.param0;
        }

        public void Invalidate() { mo.Invalidate(); }

        // XXX Need jnthn to come up with a good type cache thing.
        public bool HasMRO(STable m) {
            int k = mo.mro.Length;
            if (k >= 20) {
                return mo.isa.Contains(m);
            } else {
                while (k != 0) {
                    if (mo.mro[--k] == m) return true;
                }
                return false;
            }
        }

        public P6any GetPrivateMethod(string name) {
            P6any code = private_mro[name];
            if (code == null) { throw new NieczaException("private method lookup failed for " + name + " in class " + this.name); }
            return code;
        }

        public void AddMethod(int flags, string name, P6any code) {
            mo.AddMethod(flags, name, code);
        }

        public void AddAttribute(string name, int flags, P6any init,
                STable type) {
            mo.AddAttribute(name, flags, init, type);
        }

        public void FillProtoClass(STable parent, params string[] slots) {
            mo.FillProtoClass(parent, slots);
        }

        public void FillClass(string[] all_slot, STable[] superclasses,
                STable[] mro) {
            mo.FillClass(all_slot, superclasses, mro);
        }

        public void FillRole(STable[] superclasses, STable[] cronies) {
            mo.FillRole(superclasses, cronies);
        }

        public void FillParametricRole(P6any factory) {
            mo.FillParametricRole(factory);
        }
    }

    // This is quite similar to DynFrame and I wonder if I can unify them.
    // These are always hashy for the same reason as Frame above
    public class P6opaque: P6any {
        // the slots have to support non-containerized values, because
        // containers are objects now
        public object[] slots;

        public P6opaque(STable klass) {
            this.mo = klass;
            this.slots = (klass.nslots != 0) ? new object[klass.nslots] : null;
        }

        // for thawing, if klass is uninitialized
        public P6opaque(STable klass, int na) {
            this.mo = klass;
            this.slots = (na != 0) ? new object[na] : null;
        }

        public override void SetSlot(string name, object obj) {
            if (slots == null)
                throw new NieczaException("Attempted to access slot " + name +
                        " of type object for " + mo.name);
            slots[mo.FindSlot(name)] = obj;
        }

        public override object GetSlot(string name) {
            if (slots == null)
                throw new NieczaException("Attempted to access slot " + name +
                        " of type object for " + mo.name);
            return slots[mo.FindSlot(name)];
        }

        public override bool IsDefined() {
            return this != mo.typeObject;
        }
    }

    public class BoxObject<T> : P6opaque {
        public T value;
        public BoxObject(T x, STable klass) : base(klass) { value = x; }
        public BoxObject(T x, STable klass, int na) : base(klass,na) { value = x; }
    }
}
