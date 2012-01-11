using System;
using System.Collections.Generic;
using System.Threading;
using Niecza.Serialization;

namespace Niecza {
    public abstract class P6any: IFreeze {
        public STable mo;

        public abstract void Freeze(FreezeBuffer fb);

        public virtual object GetSlot(STable type, string name) {
            throw new NieczaException("Representation " + ReprName() + " does not support attributes");
        }

        public virtual void SetSlot(STable type, string name, object v) {
            throw new NieczaException("Representation " + ReprName() + " does not support attributes");
        }

        protected Frame Fail(Frame caller, string msg) {
            return Kernel.Die(caller, msg + " in class " + mo.name);
        }

        public abstract string ReprName();

        public virtual void ChangeType(STable to) {
            throw new NieczaException("Representation " + ReprName() + " does not support mixins");
        }

        public virtual P6any ReprClone() {
            throw new NieczaException("Representation " + ReprName() + " does not support cloning");
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
            if ((m = mo.FindMethod(name)) != null) {
                return m.info.SetupCall(caller, m.outer, m.ip6,
                        pos, named, false, m);
            }
            if (mo.mro_methods.TryGetValue("FALLBACK", out m)) {
                Variable[] npos = new Variable[pos.Length + 1];
                Array.Copy(pos, 1, npos, 2, pos.Length - 1);
                npos[0] = pos[0];
                npos[1] = Kernel.BoxAnyMO(name, Kernel.StrMO);
                return m.info.SetupCall(caller, m.outer, m.ip6,
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
            return mo.useAcceptsType ? mo.mo.AcceptsType(this) :
                this.mo.HasType(mo);
        }

        public bool Does(STable mo) {
            return mo.useAcceptsType ? mo.mo.AcceptsType(this) :
                this.mo.HasType(mo);
        }

        public Frame Invoke(Frame c, Variable[] p, VarHash n) {
            return mo.mro_INVOKE.Invoke(this, c, p, n);
        }
    }

    public abstract class ContextHandler<T> : ReflectObj {
        public abstract T Get(Variable obj);
    }

    public abstract class InvokeHandler : ReflectObj {
        public abstract Frame Invoke(P6any obj, Frame th, Variable[] pos, VarHash named);
    }
    public abstract class PushyHandler : ReflectObj {
        public abstract Variable Invoke(Variable obj, Variable[] args);
    }

    public abstract class BindHandler : ReflectObj {
        public abstract Variable Bind(Variable obj, Variable key, Variable to);
    }

    public abstract class IndexHandler : ReflectObj {
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
            if (key.Fetch().mo.HasType(Kernel.JunctionMO)) {
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
    public class P6how: IFreeze, IFixup {
        // true primitive data {{{
        public STable stable;

        public const int PACKAGE = 0;
        public const int MODULE  = 1;
        public const int CLASS   = 2;
        public const int GRAMMAR = 3;
        public const int ROLE    = 4;
        public const int PARAMETRIZED_ROLE = 5;
        public const int SUBSET  = 6;
        public const int CURRIED_ROLE = 7;

        public bool isComposed, isComposing;
        public string rtype = "class"; // XXX used for compiler's inspection
        public int type = CLASS;
        public P6any roleFactory; // PARAMETRIZED_ROLE + CURRIED_ROLE only
        public P6any subsetWhereThunk; // SUBSET only
        public Variable subsetFilter; // SUBSET only
        public Variable[] curriedArgs; // CURRIED_ROLE only
        public object rolePun; // all roles

        public List<MethodInfo> lmethods = new List<MethodInfo>();
        public List<AttrInfo> local_attr = new List<AttrInfo>();

        public List<STable> superclasses = new List<STable>();
        public List<STable> local_roles = new List<STable>();
        public List<STable> role_typecheck_list = new List<STable>();
        // }}}
        // calculated at compose time {{{
        public STable[] mro = new STable[0];
        // }}}
        // strictly caches (mostly set when MRO changed) {{{
        public Dictionary<string, DispatchEnt> inherit_methods;

        public Dictionary<string, P6how.DispatchSet> up_protos;
        public List<DispatchSet> here_protos;
        public Dictionary<DispatchSet, List<MethodInfo>> multimethods;

        public STable[] type_list = new STable[0];
        public HashSet<STable> type_set = new HashSet<STable>();
        internal SubscriberSet subclasses = new SubscriberSet();
        Subscription[] mro_sub;
        // role type objects have an empty MRO cache so no methods can be
        // called against them; the fallback (NYI) is to pun.
        // }}}

        // types and constants {{{
        public struct AttrInfo {
            public STable owner;
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
        public const int A_TYPE   = 6;
        public const int A_SCALAR = 0;

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
        // }}}

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

            type_set.Clear();
            foreach (STable s in mro) {
                type_set.Add(s);
                foreach (STable s2 in s.mo.role_typecheck_list) {
                    type_set.Add(s2);
                }
            }
            foreach (STable s2 in role_typecheck_list) {
                type_set.Add(s2);
            }
            type_list = new List<STable>(type_set).ToArray();

            if (type == ROLE || type == PARAMETRIZED_ROLE || type == CURRIED_ROLE)
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
                    n.owner = mro[i];
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
                    // TODO why are we installing a long name at all?
                    mi.long_name += ":(" + Kernel.MMDCandidate.get_unique() + ")";
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
            ai.owner = stable;
            ai.name = name;
            ai.flags = flags;
            ai.init = init;
            ai.type = type;
            ai.file = file;
            ai.line = line;
            local_attr.Add(ai);
        }

        public void FillProtoClass(STable parent, string[] slots, STable[] slot_types) {
            if (parent == null) {
                FillClass(slots, slot_types, new STable[] {}, new STable[] { stable });
            } else {
                STable[] mro = new STable[parent.mo.mro.Length + 1];
                Array.Copy(parent.mo.mro, 0, mro, 1, mro.Length-1);
                mro[0] = stable;
                FillClass(slots, slot_types, new STable[] { parent }, mro);
            }
            Invalidate();
        }

        public void FillSubset(STable super) {
            stable.useAcceptsType = true;
            type = SUBSET; rtype = "subset";
            STable[] mro = new STable[super.mo.mro.Length + 1];
            Array.Copy(super.mo.mro, 0, mro, 1, mro.Length - 1);
            mro[0] = stable;
            FillClass(super.all_slot, super.type_slot, new STable[] { super }, mro);
            Revalidate();
            stable.SetupVTables();
        }

        public bool AcceptsType(P6any obj) {
            if (type == SUBSET) {
                if (!obj.Does(superclasses[0]))
                    return false;
                if (subsetFilter == null)
                    subsetFilter = Kernel.RunInferior(subsetWhereThunk.Invoke(
                        Kernel.GetInferiorRoot(), Variable.None, null));
                return Kernel.ACCEPTS(Kernel.NewROScalar(obj), subsetFilter);
            }
            if (type == CURRIED_ROLE) {
                foreach (STable cand in obj.mo.mo.type_list) {
                    if (cand.mo.type != CURRIED_ROLE)
                        continue;
                    if (cand.mo.roleFactory != roleFactory)
                        continue;
                    if (cand.mo.curriedArgs.Length != curriedArgs.Length)
                        continue;

                    bool ok = true;
                    for (int i = 0; i < curriedArgs.Length; i++) {
                        if (!Kernel.ACCEPTS(cand.mo.curriedArgs[i], curriedArgs[i])) {
                            ok = false;
                            break;
                        }
                    }
                    if (!ok) continue;
                    return true;
                }
                return false;
            }
            throw new NieczaException("accepts_type unimplemented for " + rtype);
        }

        public void FillClass(string[] all_slot, STable[] type_slot,
                STable[] superclasses, STable[] mro) {
            this.superclasses = new List<STable>(superclasses);
            SetMRO(mro);
            stable.all_slot = all_slot;
            stable.type_slot = type_slot;

            stable.nslots = 0;
            foreach (string an in all_slot) {
                stable.slotMap[an] = stable.nslots++;
            }
        }

        public void FillRole(STable[] superclasses, STable[] cronies) {
            this.superclasses = new List<STable>(superclasses);
            local_roles = new List<STable>(cronies ?? new STable[0]);
            type = ROLE; rtype = "role";
            SetMRO(Kernel.AnyMO.mo.mro);
        }

        public void FillParametricRole(P6any factory) {
            type = PARAMETRIZED_ROLE; rtype = "prole";
            roleFactory = factory;
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

        string ComputeMRO() {
            string err;
            STable[][] lists = new STable[superclasses.Count + 2][];
            lists[0] = new STable[] { stable };
            lists[superclasses.Count + 1] = superclasses.ToArray();
            for (int i = 0; i < superclasses.Count; i++)
                lists[i+1] = superclasses[i].mo.mro;

            List<STable> nmro = new List<STable>();
            err = C3Merge(nmro, lists);
            if (err != null)
                return "C3 MRO generation failed for " + stable.name + ": " + err;
            SetMRO(nmro.ToArray());
            return null;
        }

        public string Compose() {
            if (isComposed || type == PACKAGE || type == MODULE) {
                isComposed = true;
                return null;
            }
            if (isComposing)
                return "Circularity detected while composing " + stable.name;
            isComposing = true;
            string err;
            foreach (STable su in superclasses) {
                err = su.mo.Compose();
                if (err != null) return err;
            }
            isComposed = true;

            if (type == ROLE || type == PARAMETRIZED_ROLE || type == CURRIED_ROLE) {
                role_typecheck_list.Add(stable);
                foreach (STable s2 in local_roles)
                    foreach (STable s3 in s2.mo.role_typecheck_list)
                        role_typecheck_list.Add(s3);
                SetMRO(Kernel.AnyMO.mo.mro);
                Revalidate();
                stable.SetupVTables();
                return null;
            }

            if (local_roles.Count > 0) {
                if ((err = ComputeMRO()) != null) return err;
                Revalidate();
                Kernel.ApplyRoleToClass(stable, local_roles.ToArray());
            }

            if (superclasses.Count == 0 && stable != Kernel.MuMO) {
                superclasses.Add(type == GRAMMAR ? Kernel.GrammarMO :
                        Kernel.AnyMO);
            }

            if ((err = ComputeMRO()) != null) return err;

            List<string> all_slot_l = new List<string>();
            List<STable> type_slot_l = new List<STable>();
            foreach (STable m in mro)
                foreach (AttrInfo ai in m.mo.local_attr) {
                    all_slot_l.Add(ai.name);
                    type_slot_l.Add(m);
                }
            stable.all_slot = all_slot_l.ToArray();
            stable.type_slot = type_slot_l.ToArray();

            stable.nslots = 0;
            foreach (string an in stable.all_slot) {
                stable.slotMap[an] = stable.nslots++;
            }
            Revalidate();
            stable.SetupVTables();
            return null;
        }

        public STable PunRole() {
            var pun = Thread.VolatileRead(ref rolePun) as STable;
            if (pun != null) return pun;
            STable n = new STable(stable.name);

            n.how = Kernel.BoxAnyMO<STable>(n, Kernel.ClassHOWMO).Fetch();
            n.typeObject = n.initObject = new P6opaque(n);
            n.typeVar = n.initVar = Kernel.NewROScalar(n.typeObject);
            ((P6opaque)n.typeObject).slots = null;

            n.mo.local_roles.Add(stable);
            n.mo.Compose();

            Thread.VolatileWrite(ref rolePun, n);
            return n;
        }

        void IFreeze.Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.P6how);
            fb.ObjRef(stable);
            fb.Byte((byte)(isComposed ? 2 : isComposing ? 1 : 0));
            fb.String(rtype);
            fb.ObjRef(roleFactory);
            fb.ObjRef(subsetWhereThunk);
            fb.ObjRef(subsetFilter);
            fb.Refs(curriedArgs);
            fb.ObjRef(rolePun);

            // local_does not yet used
            fb.Int(lmethods.Count);
            // we do NOT save source position info here, it's only used
            // intra-unit
            foreach (MethodInfo mi in lmethods) {
                fb.String(mi.short_name);
                fb.String(mi.long_name);
                fb.ObjRef(mi.impl);
                fb.Byte(checked((byte) mi.flags));
            }

            fb.Int(local_attr.Count);
            foreach (AttrInfo ai in local_attr) {
                fb.String(ai.name);
                fb.ObjRef(ai.init);
                fb.Byte(checked((byte) ai.flags));
                fb.ObjRef(ai.type);
            }

            fb.Refs<STable>(superclasses);
            fb.Refs<STable>(local_roles);
            fb.Refs<STable>(role_typecheck_list);
            fb.Refs<STable>(mro);
        }

        internal static P6how Thaw(ThawBuffer tb) {
            P6how n = new P6how();
            tb.Register(n);
            n.stable = (STable)tb.ObjRef();
            int state = tb.Byte();
            n.isComposing = state >= 1;
            n.isComposed  = state >= 2;
            n.rtype = tb.String();
            n.type =
                n.rtype == "package" ? P6how.PACKAGE :
                n.rtype == "module" ? P6how.MODULE :
                n.rtype == "class" ? P6how.CLASS :
                n.rtype == "grammar" ? P6how.GRAMMAR :
                n.rtype == "role" ? P6how.ROLE :
                n.rtype == "prole" ? P6how.PARAMETRIZED_ROLE :
                n.rtype == "subset" ? P6how.SUBSET :
                n.rtype == "crole" ? P6how.CURRIED_ROLE :
                -1;
            n.roleFactory = (P6any)tb.ObjRef();
            n.subsetWhereThunk = (P6any)tb.ObjRef();
            n.subsetFilter = (Variable)tb.ObjRef();
            n.curriedArgs = tb.RefsA<Variable>();
            n.rolePun = tb.ObjRef();

            int mcount = tb.Int();
            while (mcount-- > 0) {
                MethodInfo mi = default(MethodInfo);
                mi.short_name = tb.String();
                mi.long_name = tb.String();
                mi.impl = (P6any)tb.ObjRef();
                mi.flags = tb.Byte();
                n.lmethods.Add(mi);
            }

            int acount = tb.Int();
            while (acount-- > 0) {
                AttrInfo ai = default(AttrInfo);
                ai.owner = n.stable;
                ai.name = tb.String();
                ai.init = (P6any)tb.ObjRef();
                ai.flags = tb.Byte();
                ai.type = (STable)tb.ObjRef();
                n.local_attr.Add(ai);
            }

            n.superclasses = tb.RefsL<STable>();
            n.local_roles = tb.RefsL<STable>();
            n.role_typecheck_list = tb.RefsL<STable>();
            n.mro = tb.RefsA<STable>();
            tb.PushFixup(n);
            return n;
        }

        void IFixup.Fixup() {
            SetMRO(mro);
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
    public class STable: IFreeze {
        /// well-known context handlers {{{
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
        /// }}}

        /// "true" state {{{
        public P6how mo;

        public P6any how, who;
        public P6any typeObject, initObject;
        public Variable typeVar, initVar;
        public string name;
        public bool useAcceptsType;

        public Type box_type;
        /// }}}

        /// compositon-created cache {{{
        public LexerCache lexcache;

        public Dictionary<string, int> slotMap = new Dictionary<string, int>();
        public int nslots = 0;
        public string[] all_slot;
        public STable[] type_slot;
        /// }}}

        /// caches set up by Revalidate {{{
        public Dictionary<string, DispatchEnt> mro_methods;
        public Dictionary<string, P6any> private_mro;
        public P6how.AttrInfo[] init_program;
        /// }}}

        /// caches set up in SetupVTables {{{
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

        public int num_rank = -1;
        public bool is_any = false;
        /// }}}

        private STable() {}
        public STable(string name) {
            this.name = name;
            mo = new P6how();
            mo.stable = this;
        }

        public int TryFindSlot(STable type, string name) {
            //Kernel.LogNameLookup(name);
            int ix;
            if (!slotMap.TryGetValue(name, out ix))
                return -1;
            if (type_slot[ix] == type)
                return ix;
            for (ix = 0; ix < all_slot.Length; ix++) {
                if (all_slot[ix] == name && type_slot[ix] == type)
                    return ix;
            }
            return -1;
        }

        public int FindSlot(STable type, string name) {
            //Kernel.LogNameLookup(name);
            int ix;
            if (!slotMap.TryGetValue(name, out ix))
                throw new NieczaException("Attribute {0} not defined in {1} or any superclass");
            if (type_slot[ix] == type)
                return ix;

            for (ix = 0; ix < all_slot.Length; ix++) {
                if (all_slot[ix] == name && type_slot[ix] == type)
                    return ix;
            }

            throw new NieczaException("Attribute {0} in {1} is defined in {2} but not {3}", name, this.name, type_slot[slotMap[name]].name, type.name);
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

            if (Kernel.ComplexMO != null && HasType(Kernel.ComplexMO))
                num_rank = Builtins.NR_COMPLEX;
            else if (Kernel.NumMO != null && HasType(Kernel.NumMO))
                num_rank = Builtins.NR_FLOAT;
            else if (Kernel.FatRatMO != null && HasType(Kernel.FatRatMO))
                num_rank = Builtins.NR_FATRAT;
            else if (Kernel.RatMO != null && HasType(Kernel.RatMO))
                num_rank = Builtins.NR_FIXRAT;
            else if (Kernel.IntMO != null && HasType(Kernel.IntMO))
                num_rank = Builtins.NR_FIXINT;
            else
                num_rank = -1;

            is_any = Kernel.AnyMO != null && HasType(Kernel.AnyMO);
        }

        private object _GetVT(string name) { return _GetVTi(name, 1); }
        private object _GetVTU(string name) { return _GetVTi(name, 0); }
        private object _GetVTi(string name, int ix) {
            DispatchEnt de;
            mro_methods.TryGetValue(name, out de);
            object[] ri = de == null ? null : de.info.param as object[];
            return ri == null ? null : ri[ix];
        }

        public void Invalidate() { mo.Invalidate(); }

        // Only call these if m.useAcceptsType is false
        public bool HasType(STable m) {
            int k = mo.type_list.Length;
            if (k >= 20) {
                return mo.type_set.Contains(m);
            } else {
                while (k != 0) {
                    if (mo.type_list[--k] == m) return true;
                }
                return false;
            }
        }

        public DispatchEnt FindMethod(string name) {
            DispatchEnt de;
            if (mro_methods.TryGetValue(name, out de))
                return de;
            if (mo.type == P6how.ROLE || mo.type == P6how.CURRIED_ROLE ||
                    mo.type == P6how.PARAMETRIZED_ROLE) {
                if (name == "ACCEPTS" || name == "defined")
                    return Kernel.MuMO.FindMethod(name);
                var pun = mo.PunRole();
                var punfunc = Kernel.GetVar("::GLOBAL::Niecza", "&autopun").v;
                var clone = Kernel.RunInferior(punfunc.Fetch().Invoke(
                    Kernel.GetInferiorRoot(), new Variable[] { pun.typeVar,
                        Builtins.MakeStr(name) }, null));
                return new DispatchEnt(null, clone.Fetch());
            }
            return null;
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

        public void FillProtoClass(STable parent) {
            mo.FillProtoClass(parent, parent.all_slot, parent.type_slot);
        }
        public void FillProtoClass(STable parent, string[] slots, STable[] stypes) {
            mo.FillProtoClass(parent, slots, stypes);
        }

        public void FillClass(string[] all_slot, STable[] type_slot,
                STable[] superclasses, STable[] mro) {
            mo.FillClass(all_slot, type_slot, superclasses, mro);
        }

        public void FillRole(STable[] superclasses, STable[] cronies) {
            mo.FillRole(superclasses, cronies);
        }

        public void FillParametricRole(P6any factory) {
            mo.FillParametricRole(factory);
        }

        void IFreeze.Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.STable);
            fb.ObjRef(mo);
            fb.ObjRef(how);
            fb.ObjRef(who);
            fb.ObjRef(typeObject);
            fb.ObjRef(initObject);
            fb.ObjRef(typeVar);
            fb.ObjRef(initVar);
            fb.String(name);
            fb.Byte((byte)(useAcceptsType ? 1 : 0));
            fb.String(box_type == null ? null : box_type.AssemblyQualifiedName);
            fb.Strings(all_slot);
            fb.Refs(type_slot);
        }

        internal static STable Thaw(ThawBuffer tb) {
            STable n = new STable();
            tb.Register(n);
            n.mo = (P6how)tb.ObjRef();
            n.how = (P6any)tb.ObjRef();
            n.who = (P6any)tb.ObjRef();
            n.typeObject = (P6any)tb.ObjRef();
            n.initObject = (P6any)tb.ObjRef();
            n.typeVar = (Variable)tb.ObjRef();
            n.initVar = (Variable)tb.ObjRef();
            n.name = tb.String();
            n.useAcceptsType = tb.Byte() != 0;
            string box_type = tb.String();
            n.box_type = box_type == null ? null : Type.GetType(box_type,true);
            n.all_slot = tb.Strings();
            n.type_slot = tb.RefsA<STable>();

            if (n.all_slot != null)
                foreach (string s in n.all_slot)
                    n.slotMap[s] = n.nslots++;

            tb.PushRevalidate(n);
            return n;
        }

        public override string ToString() { return name; }
    }

    // This is quite similar to DynFrame and I wonder if I can unify them.
    // These are always hashy for the same reason as Frame above
    public class P6opaque: P6any {
        // the slots have to support non-containerized values, because
        // containers are objects now
        public object[] slots;

        public override string ReprName() { return "P6opaque"; }

        internal P6opaque() { }
        public P6opaque(STable klass) {
            this.mo = klass;
            this.slots = (klass.nslots != 0) ? new object[klass.nslots] : null;
        }

        // for thawing, if klass is uninitialized
        public P6opaque(STable klass, int na) {
            this.mo = klass;
            this.slots = (na != 0) ? new object[na] : null;
        }

        public override void SetSlot(STable type, string name, object obj) {
            if (slots == null) {
                mo.FindSlot(type, name);
                throw new NieczaException("Attempted to access slot " + name +
                        " of type object for " + mo.name);
            }
            slots[mo.FindSlot(type, name)] = obj;
        }

        public override object GetSlot(STable type, string name) {
            if (slots == null) {
                mo.FindSlot(type, name);
                throw new NieczaException("Attempted to access slot " + name +
                        " of type object for " + mo.name);
            }
            return slots[mo.FindSlot(type, name)];
        }

        protected void CopyTo(P6opaque to) {
            if (slots == null) { to.slots = null; }
            else {
                to.slots = new object[slots.Length];
                Array.Copy(slots, to.slots, slots.Length);
            }
            to.mo = mo;
        }

        public override void ChangeType(STable to) {
            if (to.nslots == 0) { mo = to; slots = null; return; }
            object[] old_slots = slots;
            slots = new object[to.nslots];

            for (int i = 0; i < to.nslots; i++) {
                int old;
                if ((old = mo.TryFindSlot(to.type_slot[i], to.all_slot[i])) >= 0)
                    slots[i] = old_slots[old];
            }

            mo = to;
        }

        public override P6any ReprClone() {
            if (!IsDefined()) return this;
            var res = new P6opaque(mo);
            CopyTo(res);
            return res;
        }

        public override bool IsDefined() {
            return this != mo.typeObject;
        }

        public override void Freeze(FreezeBuffer fb) {
            FreezeSelf(fb, null);
        }
        protected void FreezeSelf(FreezeBuffer fb, Type t) {
            fb.Byte((byte) SerializationCode.P6opaque);
            int i;
            for (i = 0; i < FreezeBuffer.boxTypes.Length &&
                FreezeBuffer.boxTypes[i] != t; i++) { }
            if (i == FreezeBuffer.boxTypes.Length)
                throw new NotImplementedException(t.FullName);
            fb.Byte((byte)i);
            fb.ObjRef(mo);
            int l = slots == null ? 0 : slots.Length;
            fb.Int(l);
            for (i = 0; i < l; i++)
                fb.ObjRef(slots[i]);
        }
        protected virtual void SetData(object o) { }
        internal static P6opaque Create() { return new P6opaque(); }
        internal static P6opaque Thaw(ThawBuffer tb) {
            int k = tb.Byte();
            P6opaque o = FreezeBuffer.boxCreate[k]();
            tb.Register(o);
            o.mo = (STable) tb.ObjRef();
            int l = tb.Int();
            if (l > 0) {
                o.slots = new object[l];
                for (int i = 0; i < l; i++)
                    o.slots[i] = tb.ObjRef();
            }
            if (k != 0)
                o.SetData(tb.ObjRef());
            return o;
        }
    }

    public class BoxObject<T> : P6opaque {
        public T value;
        private BoxObject() { }
        public BoxObject(T x, STable klass) : base(klass) { value = x; }
        public BoxObject(T x, STable klass, int na) : base(klass,na) { value = x; }
        internal static new P6opaque Create() { return new BoxObject<T>(); }
        protected override void SetData(object o) { value = (T)o; }
        public override void Freeze(FreezeBuffer fb) {
            FreezeSelf(fb, typeof(T));
            fb.ObjRef(value);
        }

        public override string ReprName() { return "P6box[" + typeof(T).Name + "]"; }

        public override P6any ReprClone() {
            if (!IsDefined()) return this;
            var res = new BoxObject<T>(value, mo);
            CopyTo(res);
            return res;
        }
    }
}
