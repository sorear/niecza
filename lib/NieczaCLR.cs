using System;
using System.Reflection;
using System.Reflection.Emit;
using Niecza;
using Niecza.Serialization;
using System.Collections.Generic;
using System.Threading;

namespace Niecza {
    class CLROpts {
        public static readonly bool Debug =
            Environment.GetEnvironmentVariable("NIECZA_CLR_TRACE") != null;
        public static readonly bool MMDDebug =
            Environment.GetEnvironmentVariable("NIECZA_MMD_TRACE") != null;
    }

    // These classes implement the basic C# multiple dispatch algorithm:
    // Candidates form a poset.  Select the greatest element of the subset
    // filtered by admissability of the actual arguments.
    //
    // The actual algorithm starts by topologically sorting the candidates
    // $C_i$, that is, assigning $i$ values such that
    // $C_i > C_j \rightarrow i > j$.  Assume there are no nontrivial equal
    // elements in the ordering.
    //
    // Now suppose $C_n$ is the last
    // admissible candidate (if there is none, then there is trivially no
    // greatest element).
    //
    // If there is a greatest element, then it is $C_n$.  Proof.  Suppose
    // $C_m$ is the greatest element.  $C_m > C_i$ for any admissable $i$,
    // therefore $m >t i$ and $m$ is the last admissable index, therefore
    // equal to $n$.
    //
    // So all that remains is to check if $C_m$ is actually the greatest
    // element, that is to check that $C_p$ is inadmissable for all $p$ where
    // $C_p \nless C_m$.  The case $p > m$ is already known; we keep a list
    // of possible conflictors, values of $p < m$ for all $m$.

    abstract class MultiCandidate {
        public abstract bool Admissable(Frame th, Variable[] pos, VarHash named);
        public abstract int  Compare(int arity, MultiCandidate other);
        public abstract bool AdmissableArity(int arity);
        public abstract int  MinDispatchArity();
        public abstract Frame Invoke(Frame th, Variable[] pos, VarHash named);

        internal int[] conflictors;
    }

    class CandidateSet : IFreeze {
        object[] cands; // XXX no VolatileRead<T> ?
        MultiCandidate[] orig;
        string name;

        public CandidateSet(string name, MultiCandidate[] orig) {
            this.orig = orig;
            this.name = name;
            int max_arity = 0;
            foreach (MultiCandidate mc in orig) {
                int mda = mc.MinDispatchArity();
                if (mda > max_arity) max_arity = mda;
            }
            cands = new object[max_arity+1];
        }

        MultiCandidate[] GetCandidateList(int arity) {
            if (arity > cands.Length - 1)
                arity = cands.Length - 1;

            // No, really, we need a volatile read here to avoid seeing
            // stale data on systems like Alpha where "data-dependency fences"
            // are used.  See the Linux RCU docs.
            var list = (MultiCandidate[])Thread.VolatileRead(ref cands[arity]);

            if (list == null) {
                MultiCandidate[] n = SortCandidates(arity);
                // this is needed to ensure memory write ordering IIUC
                Interlocked.CompareExchange(ref cands[arity], n, null);
                return n;
            } else {
                return list;
            }
        }

        static void CheckJunctionArg(Variable v, ref int jun_pivot,
                ref string jun_pivot_n, ref int jun_rank, int num, string nam) {
            P6any obj = v.Fetch();
            if (!obj.mo.HasType(Kernel.JunctionMO))
                return;
            int jrank = Kernel.UnboxAny<int>((P6any) ((P6opaque)obj).slots[0]) / 2;
            if (jrank < jun_rank) {
                jun_rank = jrank;
                jun_pivot = num;
                jun_pivot_n = nam;
            }
        }

        static Frame CheckJunctions(Frame th, Variable[] pos, VarHash named, P6any junc_call) {
            int jun_pivot = -1;
            int jun_rank  = int.MaxValue;
            string jun_pivot_n = null;
            for (int i = 0; i < pos.Length; i++)
                CheckJunctionArg(pos[i], ref jun_pivot, ref jun_pivot_n,
                        ref jun_rank, i, null);
            if (named != null) {
                foreach (KeyValuePair<string,Variable> kv in named)
                    CheckJunctionArg(kv.Value, ref jun_pivot, ref jun_pivot_n,
                            ref jun_rank, -2, kv.Key);
            }

            if (jun_pivot == -1)
                return null;

            Variable jct = (jun_pivot == -2 ? named[jun_pivot_n] :
                    pos[jun_pivot]);
            Frame nth = th.MakeChild(null, Kernel.AutoThreadSubSI, Kernel.AnyP);

            P6opaque jo  = (P6opaque) jct.Fetch();

            nth.named = named;
            nth.pos = pos;
            nth.lex1 = Kernel.GetInfo(junc_call);
            nth.lex2 = jun_pivot_n;
            nth.lex3 = jo.slots[0];
            nth.lex4 = Kernel.UnboxAny<Variable[]>((P6any)jo.slots[1]);
            nth.lex5 = Kernel.GetOuter(junc_call);
            nth.lex6 = junc_call;
            nth.lex7 = null;
            nth.lex8 = new Variable[((Variable[])nth.lex4).Length];
            nth.lex9 = jct;

            nth.lexi0 = jun_pivot;
            nth.lexi1 = 0;

            return nth;
        }

        // throws on dispatch failure
        public Frame DoDispatch(Frame th, Variable[] pos, VarHash named, P6any junc_call) {
            MultiCandidate[] avail = GetCandidateList(pos.Length);

            int last_ix;
            for (last_ix = avail.Length - 1; last_ix >= 0; last_ix--)
                if (avail[last_ix].Admissable(th, pos, named))
                    break;

            if (last_ix < 0) {
                Frame nth = CheckJunctions(th, pos, named, junc_call);
                if (nth != null)
                    return nth;

                return Kernel.Die(th, "No candidates for dispatch to " + name +
                    "; candidates are:" + Console.Out.NewLine + "    " +
                    Kernel.JoinS(Console.Out.NewLine + "    ", orig));
            }

            foreach (int ci in avail[last_ix].conflictors) {
                if (avail[ci].Admissable(th, pos, named)) {
                    List<MultiCandidate> matched = new List<MultiCandidate>();

                    foreach (MultiCandidate mc in avail)
                        if (mc.Admissable(th, pos, named))
                            matched.Add(mc);

                    return Kernel.Die(th, "Ambiguous dispatch for " + name +
                        "; matched candidates are:" + Console.Out.NewLine + "    " +
                        Kernel.JoinS(Console.Out.NewLine + "    ", matched));
                }
            }

            if (CLROpts.MMDDebug)
                Console.WriteLine("Using {0}", avail[last_ix]);

            return avail[last_ix].Invoke(th, pos, named);
        }

        MultiCandidate[] SortCandidates(int arity) {
            List<MultiCandidate> afilt = new List<MultiCandidate>();
            foreach (MultiCandidate mc in orig)
                if (mc.AdmissableArity(arity))
                    afilt.Add(mc);
            int n = afilt.Count;
            bool[] gt = new bool[n*n];
            int[] blocks  = new int[n]; // # of unused elements less than i
            int[] reorder = new int[n];

            for (int i = 0; i < n; i++) {
                for (int j = 0; j < i; j++) {
                    int comp = afilt[i].Compare(arity, afilt[j]);
                    if (CLROpts.MMDDebug && comp != 0)
                        Console.WriteLine("{0} {1} {2}", afilt[i],
                                (comp > 0 ? '>' : '<'), afilt[j]);
                    if (comp > 0) { // $C_i > C_j$
                        gt[i*n+j] = true;
                        blocks[i]++;
                    } else if (comp < 0) {
                        gt[j*n+i] = true;
                        blocks[j]++;
                    }
                }
            }

            int assigned = 0;
            while (assigned != n) {
                int i;
                for (i = 0; i < n; i++)
                    if (blocks[i] == 0)
                        break;
                reorder[assigned++] = i;
                for (int j = 0; j < n; j++)
                    if (gt[j*n + i]) blocks[j]--;
                blocks[i] = int.MaxValue;
            }

            MultiCandidate[] ret = new MultiCandidate[n];

            for (int i = 0; i < n; i++) {
                ret[i] = afilt[reorder[i]];
                List<int> conflicts = new List<int>();
                for (int j = 0; j < i; j++) {
                    if (!gt[reorder[i]*n + reorder[j]])
                        conflicts.Add(j);
                }
                ret[i].conflictors = conflicts.ToArray();
            }

            if (CLROpts.MMDDebug) {
                Console.WriteLine("--- MMD CANDIDATE SORT ORDER ---");
                for (int i = 0; i < n; i++) {
                    Console.WriteLine("{0}: {1}", i, ret[i]);
                    Console.WriteLine("     c: {0}", Kernel.JoinS(" ", ret[i].conflictors));
                }
            }

            return ret;
        }

        void IFreeze.Freeze(FreezeBuffer fb) {
            // anyone who holds a ref to one of these needs to recreate it.
            fb.Ephemeralize();
        }
    }

    sealed class PropertyProxy : Variable {
        PropertyInfo prop;
        object       obj;
        object[]     argv;

        public PropertyProxy(PropertyInfo prop, object obj, object[] argv) {
            this.rw     = true; // make sure Fetch is called repeatedly
            this.islist = false;
            this.prop   = prop;
            this.obj    = obj;
            this.argv   = argv;
        }

        public override P6any Fetch() {
            if (!prop.CanRead)
                throw new NieczaException("Property " + prop.Name + " is write-only");
            MethodInfo mi = prop.GetGetMethod();
            object ret = mi.Invoke(obj, argv);
            return CLRWrapperProvider.BoxResult(mi.ReturnType, ret).Fetch();
        }

        public override void Store(P6any v) {
            if (!prop.CanWrite)
                throw new NieczaException("Property " + prop.Name + " is read-only");
            MethodInfo mi = prop.GetSetMethod();
            object[] argv_ = argv;
            Array.Resize(ref argv_, argv.Length + 1);
            if (!CLRWrapperProvider.CoerceArgument(out argv_[argv.Length],
                        prop.PropertyType, Kernel.NewROScalar(v)))
                throw new NieczaException("Unable to coerce value of type " + v.mo.name + " for " + prop.Name); // could also be a range problem
            mi.Invoke(obj, argv_);
        }

        public override Variable GetVar() {
            return Kernel.BoxAnyMO<Variable>(this, Kernel.ScalarMO);
        }

        public override void Freeze(Niecza.Serialization.FreezeBuffer fb) { throw new NotImplementedException(); }
    }

    sealed class FieldProxy : Variable {
        FieldInfo field;
        object    obj;

        public FieldProxy(FieldInfo field, object obj) {
            this.rw     = true; // make sure Fetch is called repeatedly
            this.islist = false;
            this.field  = field;
            this.obj    = obj;
        }

        public override P6any Fetch() {
            object ret = field.GetValue(obj);
            return CLRWrapperProvider.BoxResult(field.FieldType, ret).Fetch();
        }

        public override void Store(P6any v) {
            if (field.IsInitOnly || field.IsLiteral)
                throw new NieczaException("Field " + field.Name + " is read-only");
            object clr;
            if (!CLRWrapperProvider.CoerceArgument(out clr, field.FieldType,
                        Kernel.NewROScalar(v)))
                throw new NieczaException("Unable to coerce value of type " + v.mo.name + " for " + field.Name); // could also be a range problem
            field.SetValue(obj, clr);
        }

        public override Variable GetVar() {
            return Kernel.BoxAnyMO<Variable>(this, Kernel.ScalarMO);
        }
        public override void Freeze(Niecza.Serialization.FreezeBuffer fb) { throw new NotImplementedException(); }
    }

    class OverloadCandidate : MultiCandidate {
        MemberInfo what_call;
        Type[] args;
        bool[] refs;
        Type param_array;

        private OverloadCandidate(MemberInfo what_call, Type[] args,
                bool[] refs, Type param_array) {
            this.what_call = what_call;
            this.args = args;
            this.refs = refs;
            this.param_array = param_array;
        }

        public static void MakeCandidates(MemberInfo what, ParameterInfo[] pi,
                List<MultiCandidate> into) {
            Type[] args1 = new Type[pi.Length];
            bool[] refs = new bool[pi.Length];
            for (int i = 0; i < pi.Length; i++) {
                args1[i] = pi[i].ParameterType;
                if (args1[i].IsByRef) {
                    args1[i] = args1[i].GetElementType();
                    refs[i] = true;
                }
            }
            into.Add(new OverloadCandidate(what, args1, refs, null));

            if (pi.Length != 0 && pi[pi.Length-1].GetCustomAttributes(
                        typeof(ParamArrayAttribute), false).Length != 0) {
                Type[] args2 = new Type[args1.Length - 1];
                Array.Copy(args1, 0, args2, 0, args2.Length);
                into.Add(new OverloadCandidate(what, args2, refs,
                            args1[args1.Length - 1].GetElementType()));
            }
        }

        public override string ToString() {
            string s1 = Kernel.JoinS(", ", args);
            if (param_array != null) {
                return s1 + (s1 == "" ? "params " : ", params ") + param_array + "[]";
            } else {
                return s1;
            }
        }

        void WritebackRefs(Variable[] pos, object[] argv) {
            for (int i = 0; i < args.Length; i++)
                if (refs[i])
                    pos[i+1].Store(CLRWrapperProvider.BoxResult(args[i],
                                argv[i]).Fetch());
        }

        // pos[0] = self is not used for dispatch
        public override Frame Invoke(Frame th, Variable[] pos, VarHash named) {
            object[] argv = new object[args.Length +
                (param_array != null ? 1 : 0)];
            for (int i = 0; i < args.Length; i++)
                CLRWrapperProvider.CoerceArgument(out argv[i], args[i], pos[i+1]);
            if (param_array != null) {
                int npa = pos.Length - 1 - args.Length;
                Array pa = Array.CreateInstance(param_array, npa);
                for (int j = 0; j < npa; j++) {
                    object arg;
                    CLRWrapperProvider.CoerceArgument(out arg, param_array, pos[j + args.Length + 1]);
                    pa.SetValue(arg, j);
                }
                argv[args.Length] = pa;
            }
            object obj = Kernel.UnboxAny<object>(pos[0].Fetch());
            if (what_call is MethodInfo) {
                MethodInfo mi = (MethodInfo) what_call;
                object ret = mi.Invoke((mi.IsStatic ? null : obj), argv);
                WritebackRefs(pos, argv);
                th.resultSlot = CLRWrapperProvider.BoxResult(mi.ReturnType, ret);
            } else if (what_call is ConstructorInfo) {
                ConstructorInfo ci = (ConstructorInfo) what_call;
                object ret = ci.Invoke(argv);
                WritebackRefs(pos, argv);
                th.resultSlot = CLRWrapperProvider.BoxResult(ci.DeclaringType, ret);
            } else if (what_call is FieldInfo) {
                th.resultSlot = new FieldProxy((FieldInfo) what_call, obj);
            } else if (what_call is PropertyInfo) {
                th.resultSlot = new PropertyProxy((PropertyInfo) what_call, obj, argv);
            } else {
                throw new NieczaException("Unhandled member type " + what_call.GetType());
            }
            return th;
        }

        public override bool Admissable(Frame th, Variable[] pos, VarHash named) {
            if (named != null && named.IsNonEmpty)
                return false;
            if (!AdmissableArity(pos.Length))
                return false;

            object dummy;
            for (int i = 0; i < args.Length; i++)
                if (!CLRWrapperProvider.CoerceArgument(out dummy, args[i], pos[i+1])
                        || (refs[i] && !pos[i+1].rw))
                    return false;
            // XXX: maybe param arrays should be treated as slurpies?
            for (int i = args.Length; i < pos.Length - 1; i++)
                if (!CLRWrapperProvider.CoerceArgument(out dummy, param_array, pos[i+1]))
                    return false;

            return true;
        }

        public override int  Compare(int arity, MultiCandidate other_) {
            bool any_better = false, any_worse = false, any_diff = false;
            OverloadCandidate other = (OverloadCandidate) other_;

            for (int ix = 0; ix < arity; ix++) {
                Type t1 = ix >= args.Length ? param_array : args[ix];
                Type t2 = ix >= other.args.Length ? other.param_array : other.args[ix];
                int res = CompareType(t1, t2);
                if (t1 != t2) any_diff = true;
                if (res > 0) any_better = true;
                if (res < 0) any_worse  = true;
            }

            if (any_better && !any_worse)  return 1;
            if (any_worse  && !any_better) return -1;

            if (!any_diff) {
                if (param_array == null && other.param_array != null)
                    return 1;
                if (param_array != null && other.param_array == null)
                    return -1;
            }

            return 0;
        }

        const int NUM_NUMTYPES = 12;
        [Immutable] static Type[] num_types = new Type[] {
            typeof(sbyte), typeof(byte), typeof(short), typeof(ushort),
            typeof(int), typeof(uint), typeof(long), typeof(ulong),
            typeof(char), typeof(float), typeof(double), typeof(decimal),
        };

        // +1 if Y is a signed type shorter-or-equal to unsigned X, or
        // Y is implicitly convertable to X
        [Immutable] static sbyte[,] num_preced = new sbyte[,] {
            //sb  ub  ss  us  si  ui  sl  ul  ch  sf  df  dc
            {  0,  1,  1,  1,  1,  1,  1,  1,  0,  1,  1,  1 }, //sbyte
            {  0,  0,  1,  1,  1,  1,  1,  1,  0,  1,  1,  1 }, //byte
            {  0,  0,  0,  1,  1,  1,  1,  1,  0,  1,  1,  1 }, //short
            {  0,  0,  0,  0,  1,  1,  1,  1,  0,  1,  1,  1 }, //ushort
            {  0,  0,  0,  0,  0,  1,  1,  1,  0,  1,  1,  1 }, //int
            {  0,  0,  0,  0,  0,  0,  1,  1,  0,  1,  1,  1 }, //uint
            {  0,  0,  0,  0,  0,  0,  0,  1,  0,  1,  1,  1 }, //long
            {  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1 }, //ulong
            {  0,  0,  0,  1,  1,  1,  1,  1,  0,  1,  1,  1 }, //char
            {  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0 }, //float
            {  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 }, //double
            {  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 }, //decimal
        };

        int CompareType(Type t1, Type t2) {
            int i1, i2;
            if (t1 == t2) return 0;
            for (i1 = 0; i1 < NUM_NUMTYPES && t1 != num_types[i1]; i1++) ;
            for (i2 = 0; i2 < NUM_NUMTYPES && t2 != num_types[i2]; i2++) ;

            if (i1 != NUM_NUMTYPES && i2 != NUM_NUMTYPES)
                return num_preced[i1,i2] - num_preced[i2,i1];

            if (t1.IsAssignableFrom(t2))
                return -1;
            if (t2.IsAssignableFrom(t1))
                return 1;
            return 0;
        }

        public override bool AdmissableArity(int arity) {
            return param_array == null ? arity == args.Length + 1 :
                arity >= args.Length + 1;
        }

        public override int  MinDispatchArity() {
            return args.Length + 2;
        }
    }

    public class CLRWrapperProvider {
        [TrueGlobal] static object wrapper_cache_lock = new object();
        [CompartmentGlobal] static Dictionary<Type, STable> wrapper_cache;
        [CompartmentGlobal] static Dictionary<string, STable> named_wrapper_cache;

        public static STable GetWrapper(Type t) {
            lock (wrapper_cache_lock) {
                if (wrapper_cache == null)
                    wrapper_cache = new Dictionary<Type, STable>();
                STable r;
                if (wrapper_cache.TryGetValue(t, out r))
                    return r;
                wrapper_cache[t] = r = NewWrapper(t);
                return r;
            }
        }

        public static STable GetNamedWrapper(string nm) {
            lock (wrapper_cache_lock) {
                if (named_wrapper_cache == null)
                    named_wrapper_cache = new Dictionary<string, STable>();
                STable r;
                if (named_wrapper_cache.TryGetValue(nm, out r))
                    return r;
                Type ty = Type.GetType(nm.Substring(1));
                if (CLROpts.Debug)
                    Console.WriteLine("Loading type {0} ... {1}", nm.Substring(1), ty == null ? "failed" : "succeeded");
                if (ty != null) {
                    named_wrapper_cache[nm] = r = GetWrapper(ty);
                } else {
                    named_wrapper_cache[nm] = r = StashCursor.MakePackage(
                        "CLR" + nm.Replace(".","::"),
                        StashCursor.MakeCLR_WHO(nm)).Fetch().mo;
                }
                return r;
            }
        }

        static void MultiAdd(Dictionary<string,List<MultiCandidate>> d,
                string n, MemberInfo mi, ParameterInfo[] pi) {
            List<MultiCandidate> l;
            if (!d.TryGetValue(n, out l))
                d[n] = l = new List<MultiCandidate>();
            OverloadCandidate.MakeCandidates(mi, pi, l);
        }

        static Frame Binder(Frame th) {
            return (th.info.param[0] as CandidateSet).DoDispatch(th.caller, th.pos, th.named, th.sub);
        }

        /*
for $args (0..9) {
    my ($c1,$c2,$c3,$c4) = ("","","","");
    for $i (0..$args-1) {
        $c1 .= ", T$i a$i";
        $c2 .= "a$i,";
        $c3 .= "typeof(T$i),";
        $c4 .= ",T$i";
    }
    print " " x 8, "public static TR dnv${args}<TR$c4>(P6any f$c1) { return (TR)Callback(f, typeof(TR), new object[] { $c2 }, new Type[] { $c3 }); }\n";
    print " " x 8, "public static void dv${args}",($c4?"<".substr($c4,1).">":""),"(P6any f$c1) { Callback(f, typeof(void), new object[] { $c2 }, new Type[] { $c3 }); }\n";
}
        */
        public static TR dnv0<TR>(P6any f) { return (TR)Callback(f, typeof(TR), new object[] {  }, new Type[] {  }); }
        public static void dv0(P6any f) { Callback(f, typeof(void), new object[] {  }, new Type[] {  }); }
        public static TR dnv1<TR,T0>(P6any f, T0 a0) { return (TR)Callback(f, typeof(TR), new object[] { a0, }, new Type[] { typeof(T0), }); }
        public static void dv1<T0>(P6any f, T0 a0) { Callback(f, typeof(void), new object[] { a0, }, new Type[] { typeof(T0), }); }
        public static TR dnv2<TR,T0,T1>(P6any f, T0 a0, T1 a1) { return (TR)Callback(f, typeof(TR), new object[] { a0,a1, }, new Type[] { typeof(T0),typeof(T1), }); }
        public static void dv2<T0,T1>(P6any f, T0 a0, T1 a1) { Callback(f, typeof(void), new object[] { a0,a1, }, new Type[] { typeof(T0),typeof(T1), }); }
        public static TR dnv3<TR,T0,T1,T2>(P6any f, T0 a0, T1 a1, T2 a2) { return (TR)Callback(f, typeof(TR), new object[] { a0,a1,a2, }, new Type[] { typeof(T0),typeof(T1),typeof(T2), }); }
        public static void dv3<T0,T1,T2>(P6any f, T0 a0, T1 a1, T2 a2) { Callback(f, typeof(void), new object[] { a0,a1,a2, }, new Type[] { typeof(T0),typeof(T1),typeof(T2), }); }
        public static TR dnv4<TR,T0,T1,T2,T3>(P6any f, T0 a0, T1 a1, T2 a2, T3 a3) { return (TR)Callback(f, typeof(TR), new object[] { a0,a1,a2,a3, }, new Type[] { typeof(T0),typeof(T1),typeof(T2),typeof(T3), }); }
        public static void dv4<T0,T1,T2,T3>(P6any f, T0 a0, T1 a1, T2 a2, T3 a3) { Callback(f, typeof(void), new object[] { a0,a1,a2,a3, }, new Type[] { typeof(T0),typeof(T1),typeof(T2),typeof(T3), }); }
        public static TR dnv5<TR,T0,T1,T2,T3,T4>(P6any f, T0 a0, T1 a1, T2 a2, T3 a3, T4 a4) { return (TR)Callback(f, typeof(TR), new object[] { a0,a1,a2,a3,a4, }, new Type[] { typeof(T0),typeof(T1),typeof(T2),typeof(T3),typeof(T4), }); }
        public static void dv5<T0,T1,T2,T3,T4>(P6any f, T0 a0, T1 a1, T2 a2, T3 a3, T4 a4) { Callback(f, typeof(void), new object[] { a0,a1,a2,a3,a4, }, new Type[] { typeof(T0),typeof(T1),typeof(T2),typeof(T3),typeof(T4), }); }
        public static TR dnv6<TR,T0,T1,T2,T3,T4,T5>(P6any f, T0 a0, T1 a1, T2 a2, T3 a3, T4 a4, T5 a5) { return (TR)Callback(f, typeof(TR), new object[] { a0,a1,a2,a3,a4,a5, }, new Type[] { typeof(T0),typeof(T1),typeof(T2),typeof(T3),typeof(T4),typeof(T5), }); }
        public static void dv6<T0,T1,T2,T3,T4,T5>(P6any f, T0 a0, T1 a1, T2 a2, T3 a3, T4 a4, T5 a5) { Callback(f, typeof(void), new object[] { a0,a1,a2,a3,a4,a5, }, new Type[] { typeof(T0),typeof(T1),typeof(T2),typeof(T3),typeof(T4),typeof(T5), }); }
        public static TR dnv7<TR,T0,T1,T2,T3,T4,T5,T6>(P6any f, T0 a0, T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6) { return (TR)Callback(f, typeof(TR), new object[] { a0,a1,a2,a3,a4,a5,a6, }, new Type[] { typeof(T0),typeof(T1),typeof(T2),typeof(T3),typeof(T4),typeof(T5),typeof(T6), }); }
        public static void dv7<T0,T1,T2,T3,T4,T5,T6>(P6any f, T0 a0, T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6) { Callback(f, typeof(void), new object[] { a0,a1,a2,a3,a4,a5,a6, }, new Type[] { typeof(T0),typeof(T1),typeof(T2),typeof(T3),typeof(T4),typeof(T5),typeof(T6), }); }
        public static TR dnv8<TR,T0,T1,T2,T3,T4,T5,T6,T7>(P6any f, T0 a0, T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7) { return (TR)Callback(f, typeof(TR), new object[] { a0,a1,a2,a3,a4,a5,a6,a7, }, new Type[] { typeof(T0),typeof(T1),typeof(T2),typeof(T3),typeof(T4),typeof(T5),typeof(T6),typeof(T7), }); }
        public static void dv8<T0,T1,T2,T3,T4,T5,T6,T7>(P6any f, T0 a0, T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7) { Callback(f, typeof(void), new object[] { a0,a1,a2,a3,a4,a5,a6,a7, }, new Type[] { typeof(T0),typeof(T1),typeof(T2),typeof(T3),typeof(T4),typeof(T5),typeof(T6),typeof(T7), }); }
        public static TR dnv9<TR,T0,T1,T2,T3,T4,T5,T6,T7,T8>(P6any f, T0 a0, T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7, T8 a8) { return (TR)Callback(f, typeof(TR), new object[] { a0,a1,a2,a3,a4,a5,a6,a7,a8, }, new Type[] { typeof(T0),typeof(T1),typeof(T2),typeof(T3),typeof(T4),typeof(T5),typeof(T6),typeof(T7),typeof(T8), }); }
        public static void dv9<T0,T1,T2,T3,T4,T5,T6,T7,T8>(P6any f, T0 a0, T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7, T8 a8) { Callback(f, typeof(void), new object[] { a0,a1,a2,a3,a4,a5,a6,a7,a8, }, new Type[] { typeof(T0),typeof(T1),typeof(T2),typeof(T3),typeof(T4),typeof(T5),typeof(T6),typeof(T7),typeof(T8), }); }
        [Immutable]
        static MethodInfo[] delegate_methods;
        static CLRWrapperProvider() {
            delegate_methods = new MethodInfo[20];
            foreach (MethodInfo mi in typeof(CLRWrapperProvider).GetMethods()) {
                string nm = mi.Name;
                if (nm[0] == 'd' && nm[1] == 'v')
                    delegate_methods[2 * (nm[2] - '0')] = mi;
                if (nm[0] == 'd' && nm[1] == 'n' && nm[2] == 'v')
                    delegate_methods[2 * (nm[3] - '0') + 1] = mi;
            }
        }

        static object Callback(P6any fun, Type ret, object[] args, Type[] aty) {
            Variable[] pos = new Variable[args.Length];
            for (int i = 0; i < args.Length; i++)
                pos[i] = BoxResult(aty[i], args[i]);
            Variable retv = Kernel.RunInferior(fun.Invoke(
                Kernel.GetInferiorRoot(), pos, null));
            if (ret == typeof(void)) return null;
            object reto;
            if (!CoerceArgument(out reto, ret, retv))
                throw new Exception("Return value coercion failed, " + retv.Fetch().mo.name + " to " + ret.FullName);
            return reto;
        }

        static Frame default_handler(Frame th) {
            if (th.ip == 0) { th.ip = 1; return Frame.Binder(th); }
            // XXX there HAS to be a better way to do this.
            STable mo = ((Variable)th.lex0).Fetch().mo;
            object obj = Array.CreateInstance(mo.box_type, 1).GetValue(0);
            th.caller.resultSlot = obj == null ? mo.typeVar :
                Kernel.BoxAnyMO<object>(obj, mo);
            return th.caller;
        }

        static Frame marshal_handler(Frame th) {
            if (th.ip == 0) { th.ip = 1; return Frame.Binder(th); }
            STable mo = ((Variable)th.lex0).Fetch().mo;
            object clr;
            if (!CoerceArgument(out clr, mo.box_type, (Variable)th.lex1))
                return Kernel.Die(th, "Cannot coerce value of type " + ((Variable)th.lex1).Fetch().mo.name + " to " + mo.box_type.FullName);
            th.caller.resultSlot = clr == null ? mo.typeVar :
                Kernel.BoxAnyMO<object>(clr, mo);
            return th.caller;
        }

        static Frame unmarshal_handler(Frame th) {
            if (th.ip == 0) { th.ip = 1; return Frame.Binder(th); }
            object o = Kernel.UnboxAny<object>(((Variable)th.lex0).Fetch());
            th.caller.resultSlot = BoxResult(o == null ? typeof(void) : o.GetType(), o);
            return th.caller;
        }

        static Frame dispose_handler(Frame th) {
            if (th.ip == 0) { th.ip = 1; return Frame.Binder(th); }
            object o = Kernel.UnboxAny<object>(((Variable)th.lex0).Fetch());
            ((IDisposable)o).Dispose();
            th.caller.resultSlot = Kernel.NilP.mo.typeVar;
            return th.caller;
        }

        static Frame Str_handler(Frame th) {
            if (th.ip == 0) { th.ip = 1; return Frame.Binder(th); }
            P6any ro = ((Variable)th.lex0).Fetch();
            object o = Kernel.UnboxAny<object>(ro);
            th.caller.resultSlot = Kernel.BoxAnyMO(o == null ? ro.mo.name : o.ToString(), Kernel.StrMO);
            return th.caller;
        }

        static STable NewWrapper(Type t) {
            if (CLROpts.Debug)
                Console.WriteLine("Setting up wrapper for {0}", t.FullName);
            STable m = new STable("CLR::" + t.FullName.Replace(".","::"));
            m.who = StashCursor.MakeCLR_WHO("." + t.FullName);
            STable pm = t.BaseType == null ? Kernel.AnyMO :
                GetWrapper(t.BaseType);
            STable[] mro = new STable[pm.mo.mro.Length + 1];
            Array.Copy(pm.mo.mro, 0, mro, 1, pm.mo.mro.Length);
            mro[0] = m;
            m.FillClass(new string[] { }, new STable[] { }, new STable[] { pm }, mro);

            HashSet<string> needNewWrapper = new HashSet<string>();
            needNewWrapper.Add("new"); // don't inherit constructors
            Dictionary<string,List<MultiCandidate>> allMembers
                = new Dictionary<string,List<MultiCandidate>>();
            allMembers["new"] = new List<MultiCandidate>();

            foreach (MethodInfo mi in t.GetMethods(BindingFlags.Public |
                        BindingFlags.Static | BindingFlags.Instance)) {
                if (CLROpts.Debug)
                    Console.WriteLine("Checking method : {0}", mi);
                if (mi.IsSpecialName && (Utils.StartsWithInvariant(mi.Name, "set_") || Utils.StartsWithInvariant(mi.Name, "get_")))
                    continue; // ignore property accessors
                if (mi.GetBaseDefinition().DeclaringType == t)
                    needNewWrapper.Add(mi.Name);
                MultiAdd(allMembers, mi.Name, mi, mi.GetParameters());
            }

            foreach (ConstructorInfo mi in t.GetConstructors(BindingFlags.Public |
                        BindingFlags.Instance)) {
                if (CLROpts.Debug)
                    Console.WriteLine("Checking constructor : {0}", mi);
                needNewWrapper.Add("new");
                MultiAdd(allMembers, "new", mi, mi.GetParameters());
            }

            foreach (PropertyInfo pi in t.GetProperties(BindingFlags.Public |
                        BindingFlags.Static | BindingFlags.Instance)) {
                if (CLROpts.Debug)
                    Console.WriteLine("Checking property : {0}", pi);
                if (pi.DeclaringType == t)
                    needNewWrapper.Add(pi.Name);
                MultiAdd(allMembers, pi.Name, pi, pi.GetIndexParameters());
            }

            foreach (FieldInfo fi in t.GetFields(BindingFlags.Public |
                        BindingFlags.Static | BindingFlags.Instance)) {
                if (CLROpts.Debug)
                    Console.WriteLine("Checking fields : {0}", fi);
                if (fi.DeclaringType == t)
                    needNewWrapper.Add(fi.Name);
                MultiAdd(allMembers, fi.Name, fi, new ParameterInfo[0]);
            }

            if (typeof(IDisposable).IsAssignableFrom(t)) {
                SubInfo si;

                si = new SubInfo("KERNEL dispose-hack", dispose_handler);
                si.sig = new Signature(Parameter.TPos("self", 0));
                m.AddMethod(0, "dispose-hack", Kernel.MakeSub(si, null));
            }

            if (t == typeof(object)) {
                SubInfo si;

                si = new SubInfo("KERNEL default", default_handler);
                si.sig = new Signature(Parameter.TPos("self", 0));
                m.AddMethod(0, "default", Kernel.MakeSub(si, null));

                si = new SubInfo("KERNEL marshal", marshal_handler);
                si.sig = new Signature(Parameter.TPos("self", 0),
                        Parameter.TPos("$obj", 1));
                m.AddMethod(0, "marshal", Kernel.MakeSub(si, null));

                si = new SubInfo("KERNEL unmarshal", unmarshal_handler);
                si.sig = new Signature(Parameter.TPos("self", 0));
                m.AddMethod(0, "unmarshal", Kernel.MakeSub(si, null));

                si = new SubInfo("KERNEL Str", Str_handler);
                si.sig = new Signature(Parameter.TPos("self", 0));
                m.AddMethod(0, "Str",  Kernel.MakeSub(si, null));
                m.AddMethod(0, "gist", Kernel.MakeSub(si, null));
            }

            foreach (string n in needNewWrapper) {
                string siname = string.Format("{0}.{1}", m.name, n);

                SubInfo si = new SubInfo(siname, Binder);
                si.param = new object[] {
                    new CandidateSet(siname, allMembers[n].ToArray()) };
                if (CLROpts.Debug)
                    Console.WriteLine("Installing {0}", siname);
                P6any sub = Kernel.MakeSub(si, null);
                m.AddMethod(0, n, sub);
                if (n == "Invoke" && typeof(Delegate).IsAssignableFrom(t))
                    m.AddMethod(0, "postcircumfix:<( )>", sub);
            }

            m.Invalidate();
            m.box_type = t;
            m.typeObject = m.initObject = new BoxObject<object>(null, m);
            m.typeVar = m.initVar = Kernel.NewROScalar(m.typeObject);
            if (CLROpts.Debug) {
                Console.WriteLine("--- Created box for {0} ---", m.name);
                foreach (var o in m.mo.type_list)
                    Console.WriteLine("type {0}", o.name);
            }
            return m;
        }

        public static Variable BoxResult(Type cty, object ret) {
            if (cty == typeof(void))
                return Kernel.NewRWListVar(Kernel.NilP);
            if (cty == typeof(sbyte))
                return Builtins.MakeInt((sbyte)ret);
            if (cty == typeof(byte))
                return Builtins.MakeInt((byte)ret);
            if (cty == typeof(short))
                return Builtins.MakeInt((short)ret);
            if (cty == typeof(ushort))
                return Builtins.MakeInt((ushort)ret);
            if (cty == typeof(int))
                return Builtins.MakeInt((int)ret);
            if (cty == typeof(uint))
                return Builtins.MakeInt((uint)ret);
            if (cty == typeof(long))
                return Builtins.MakeInt((long)ret);
            if (cty == typeof(ulong))
                return Builtins.MakeInt((ulong)ret);
            if (cty == typeof(float))
                return Builtins.MakeFloat((float)ret);
            if (cty == typeof(double))
                return Builtins.MakeFloat((double)ret);
            if (cty == typeof(bool))
                return Kernel.BoxAnyMO((bool)ret, Kernel.BoolMO);
            if (cty == typeof(string))
                return Kernel.BoxAnyMO((string)ret, Kernel.StrMO);
            if (cty == typeof(Variable))
                return (Variable)ret;
            if (cty == typeof(P6any))
                return Kernel.NewROScalar((P6any)ret);

            if (ret == null)
                return Kernel.AnyMO.typeVar;
            return Kernel.BoxAnyMO<object>(ret, GetWrapper(ret.GetType()));
        }

        public static bool CoerceArgument(out object clr, Type ty, Variable var) {
            P6any obj = var.Fetch();
            clr = null;

            // type objects are typed nulls
            if (!obj.IsDefined()) {
                if (obj is BoxObject<object>) {
                    Type t = obj.mo.box_type;
                    // is this enough?
                    return (ty.IsAssignableFrom(t) && !ty.IsValueType &&
                            ty != typeof(void));
                } else if (obj.mo == Kernel.MuMO || obj.mo == Kernel.AnyMO) {
                    // untyped-ish null
                    return !ty.IsValueType && ty != typeof(void);
                } else {
                    // we'll pass this by value anyway
                    clr = obj;
                    return ty.IsAssignableFrom(obj.GetType());
                }
            }
            // in all other cases we're definitely passing a non-null value

            // Boolean values marshal to bool
            if (obj.Does(Kernel.BoolMO)) {
                clr = Kernel.UnboxAny<int>(obj) != 0;
            }
            // note, Bool ~~ Int ~~ Integral
            else if (obj.Does(Kernel.IntegralMO)) {
                // important type directed case!
                int small;
                BigInteger big;
                bool use_big = Builtins.GetAsInteger(var, out small, out big);

                if (ty == typeof(sbyte))
                    clr = (!use_big && small >= sbyte.MinValue && small <= sbyte.MaxValue) ? (object)(sbyte)small : null;
                else if (ty == typeof(byte))
                    clr = (!use_big && small >= byte.MinValue && small <= byte.MaxValue) ? (object)(byte)small : null;
                else if (ty == typeof(short))
                    clr = (!use_big && small >= short.MinValue && small <= short.MaxValue) ? (object)(short)small : null;
                else if (ty == typeof(ushort))
                    clr = (!use_big && small >= ushort.MinValue && small <= ushort.MaxValue) ? (object)(ushort)small : null;
                else {
                    big = use_big ? big : (BigInteger) small;

                    if (ty == typeof(int))
                        clr = (big >= int.MinValue && big <= int.MaxValue) ? (object)(int)big : null;
                    else if (ty == typeof(uint))
                        clr = (big >= uint.MinValue && big <= uint.MaxValue) ? (object)(uint)big : null;
                    else if (ty == typeof(long))
                        clr = (big >= long.MinValue && big <= long.MaxValue) ? (object)(long)big : null;
                    else if (ty == typeof(ulong))
                        clr = (big >= ulong.MinValue && big <= ulong.MaxValue) ? (object)(ulong)big : null;

                    else if (ty == typeof(float))
                        clr = (object)(float)big;
                    else if (ty == typeof(double))
                        clr = (object)(double)big;
                    else if (ty == typeof(decimal))
                        clr = big.GetWords().Length <= 3 ? (object)(decimal)big : null;
                    else if (ty == typeof(object))
                        clr = use_big ? null : (object)small;
                    else
                        clr = obj;
                }
            }
            else if (obj.Does(Kernel.RealMO)) {
                // fractional value

                if (ty == typeof(decimal)) {
                    // decimal is for people who care about exactness
                    int rk;
                    P6any n = Builtins.GetNumber(var, obj, out rk);
                    BigInteger num, den;
                    if (rk == Builtins.NR_FATRAT) {
                        FatRat r = Kernel.UnboxAny<FatRat>(n);
                        num = r.num; den = r.den;
                    }
                    else if (rk == Builtins.NR_FIXRAT) {
                        Rat r = Kernel.UnboxAny<Rat>(n);
                        num = r.num; den = r.den;
                    }
                    else if (rk == Builtins.NR_BIGINT) {
                        num = Kernel.UnboxAny<BigInteger>(n); den = BigInteger.One;
                    }
                    else if (rk == Builtins.NR_FIXINT) {
                        num = Kernel.UnboxAny<int>(n); den = BigInteger.One;
                    }
                    else {
                        return false;
                    }
                    BigInteger div, rem;
                    int scale = 0;
                    while (true) {
                        div = BigInteger.DivRem(den, 10, out rem);
                        if (rem.Sign != 0) break;
                        den = div;
                        scale++;
                    }
                    while (true) {
                        div = BigInteger.DivRem(den, 5, out rem);
                        if (rem.Sign != 0) break;
                        den = div;
                        num *= 2;
                        scale++;
                    }
                    while (true) {
                        div = BigInteger.DivRem(den, 2, out rem);
                        if (rem.Sign != 0) break;
                        den = div;
                        num *= 5;
                        scale++;
                    }
                    if (den != BigInteger.One)
                        return false;
                    if (scale > 28)
                        return false;
                    int[] bits = decimal.GetBits((decimal)num);
                    bits[3] = scale << 16;
                    clr = new decimal(bits);
                } else {
                    double val = obj.mo.mro_raw_Numeric.Get(var);
                    if (ty == typeof(float))
                        clr = (object)(float)val;
                    else if (ty == typeof(double) || ty == typeof(object))
                        clr = (object)val;
                }
            }
            else if (obj.Does(Kernel.StrMO)) {
                string s = Kernel.UnboxAny<string>(obj);
                if (ty == typeof(char) && s.Length == 1)
                    clr = s[0];
                else if (ty == typeof(string))
                    clr = s;
                else if (ty == typeof(object))
                    clr = s;
                else
                    clr = obj;
            }
            // "Callable"
            else if (typeof(Delegate).IsAssignableFrom(ty)) {
                MethodInfo needed = ty.GetMethod("Invoke");
                ParameterInfo[] pi = needed.GetParameters();

                if (pi.Length >= 10) {
                    clr = null;
                } else if (needed.ReturnType != typeof(void)) {
                    Type[] args = new Type[pi.Length + 1];
                    args[0] = needed.ReturnType;
                    for (int i = 0; i < pi.Length; i++)
                        args[i+1] = pi[i].ParameterType;
                    MethodInfo compat = delegate_methods[pi.Length * 2 + 1].
                        MakeGenericMethod(args);
                    clr = Delegate.CreateDelegate(ty, obj, compat);
                } else {
                    Type[] args = new Type[pi.Length];
                    for (int i = 0; i < pi.Length; i++)
                        args[i] = pi[i].ParameterType;
                    MethodInfo compat = delegate_methods[pi.Length * 2];
                    if (args.Length != 0)
                        compat = compat.MakeGenericMethod(args);
                    clr = Delegate.CreateDelegate(ty, obj, compat);
                }
            }
            else if (obj is BoxObject<object>) {
                clr = Kernel.UnboxAny<object>(obj);
            }
            else {
                clr = obj;
            }

            return clr != null && ty.IsAssignableFrom(clr.GetType());
        }

    }
}
