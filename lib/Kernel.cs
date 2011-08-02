using System;
using System.Text;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace Niecza {
    // We like to reuse continuation objects for speed - every function only
    // creates one kind of continuation, but tweaks a field for exact return
    // point.  As such, call frames and continuations are in 1:1 correspondence
    // and are unified.  Functions take a current continuation and return a new
    // continuation; we tail recurse with trampolines.

    // Used by DynFrame to plug in code
    public delegate Frame DynBlockDelegate(Frame frame);

    public sealed class DispatchEnt {
        public DispatchEnt next;
        public SubInfo info;
        public Frame outer;
        public P6any ip6;

        public DispatchEnt() {}
        public DispatchEnt(DispatchEnt next, P6any ip6) {
            this.ip6 = ip6;
            this.next = next;
            P6opaque d = (P6opaque)ip6;
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
        public STable type;
        public bool rw;
        public bool islist;

        public abstract P6any Fetch();
        public abstract void Store(P6any v);

        public abstract Variable GetVar();

        public static readonly Variable[] None = new Variable[0];
    }

    public abstract class ViviHook {
        public abstract void Do(Variable toviv);
    }

    public class SubViviHook : ViviHook {
        P6any sub;
        public SubViviHook(P6any sub) { this.sub = sub; }
        public override void Do(Variable toviv) {
            Kernel.RunInferior(sub.Invoke(Kernel.GetInferiorRoot(),
                        new Variable[] { toviv }, null));
        }
    }

    public class HashViviHook : ViviHook {
        P6any hash;
        string key;
        public HashViviHook(P6any hash, string key) { this.hash = hash; this.key = key; }
        public override void Do(Variable toviv) {
            VarHash rh = Kernel.UnboxAny<VarHash>(hash);
            rh[key] = toviv;
        }
    }

    public class NewHashViviHook : ViviHook {
        Variable hashv;
        string key;
        public NewHashViviHook(Variable hashv, string key) { this.hashv = hashv; this.key = key; }
        public override void Do(Variable toviv) {
            VarHash rh = new VarHash();
            rh[key] = toviv;
            hashv.Store(Kernel.BoxRaw(rh, Kernel.HashMO));
        }
    }

    public class ArrayViviHook : ViviHook {
        P6any ary;
        int key;
        public ArrayViviHook(P6any ary, int key) { this.ary = ary; this.key = key; }
        public override void Do(Variable toviv) {
            VarDeque vd = (VarDeque) ary.GetSlot("items");
            while (vd.Count() <= key)
                vd.Push(Kernel.NewTypedScalar(null));
            vd[key] = toviv;
        }
    }

    public class NewArrayViviHook : ViviHook {
        Variable ary;
        int key;
        public NewArrayViviHook(Variable ary, int key) { this.ary = ary; this.key = key; }
        public override void Do(Variable toviv) {
            VarDeque vd = new VarDeque();
            while (vd.Count() <= key)
                vd.Push(Kernel.NewTypedScalar(null));
            vd[key] = toviv;
            P6opaque d = new P6opaque(Kernel.ArrayMO);
            d.slots[0] = vd;
            d.slots[1] = new VarDeque();
            ary.Store(d);
        }
    }

    public sealed class SimpleVariable: Variable {
        P6any val;

        public SimpleVariable(bool rw, bool islist, STable type, ViviHook whence, P6any val) {
            this.val = val; this.whence = whence; this.rw = rw;
            this.islist = islist; this.type = type;
        }

        public override P6any  Fetch()       { return val; }
        public override void Store(P6any v)  {
            if (!rw) {
                throw new NieczaException("Writing to readonly scalar");
            }
            if (!v.Does(type)) {
                throw new NieczaException("Nominal type check failed for scalar store; got " + v.mo.name + ", needed " + type.name + " or subtype");
            }
            if (whence != null) {
                ViviHook vh = whence;
                whence = null;
                vh.Do(this);
            }
            val = v;
        }

        public override Variable GetVar() {
            return Kernel.BoxAnyMO<Variable>(this, Kernel.ScalarMO);
        }
    }

    public sealed class TiedVariable: Variable {
        P6any fetch;
        P6any store;

        public TiedVariable(STable type, P6any whsub, P6any fetch, P6any store) {
            this.fetch = fetch;
            this.store = store;
            this.whence = whsub.IsDefined() ? new SubViviHook(whsub) : null;
            this.rw = true;
            this.type = type;
        }

        public override P6any Fetch() {
            Variable vr = Kernel.RunInferior(fetch.Invoke(
                Kernel.GetInferiorRoot(), None, null));
            P6any vl = vr.Fetch();
            if (!vl.Does(type))
                throw new NieczaException("Tied variable of type " + type +
                        " returned value of type " + vl.mo.name + " instead");
            return vl;
        }

        public override void Store(P6any v) {
            if (whence != null) {
                ViviHook vh = whence;
                whence = null;
                vh.Do(this);
            }
            Kernel.RunInferior(store.Invoke(Kernel.GetInferiorRoot(),
                        new Variable[] { Kernel.NewROScalar(v) }, null));
        }

        public override Variable GetVar() {
            return Kernel.BoxAnyMO<Variable>(this, Kernel.ScalarMO);
        }
    }

    // Used to make Variable sharing explicit in some cases; will eventually be
    // the only way to share a bvalue
    public sealed class BValue {
        public Variable v;
        public BValue(Variable v) { this.v = v; }
    }

    public sealed class RuntimeUnit {
        static Dictionary<string, byte[]> heapreg;

        public string name;
        public Type type;
        public byte[] heap;
        public RuntimeUnit[] depends;
        public DynBlockDelegate[] methods; /*C*/
        public FieldInfo[] meta_fields;
        public Dictionary<string,FieldInfo> lex_fields;
        public object[] xref;
        public Frame context_pad;
        public static bool TraceLoad = Environment.GetEnvironmentVariable("NIECZA_TRACE_LOAD") != null;

        public static void HexDump(byte[] heap) {
            for (int offs = 0; offs < heap.Length; offs += 16) {
                Console.Write("{0:X6}   ", offs);
                int len = heap.Length - offs;
                if (len > 16) len = 16;
                for (int col = 0; col < 16; col++) {
                    if (col >= len)
                        Console.Write("   ");
                    else
                        Console.Write("{0:X2} ", heap[offs+col]);
                    if (col == 7)
                        Console.Write(" ");
                }
                Console.Write("   |");
                for (int col = 0; col < len; col++)
                    Console.Write(
                        (heap[offs+col] < 32 || heap[offs+col] > 126)
                            ? '.' : (char)heap[offs+col]);
                Console.WriteLine("|");
            }
        }

        public static void RegisterHeap(string name, byte[] heap) {
            if (heapreg == null)
                heapreg = new Dictionary<string, byte[]>();
            heapreg[name] = heap;
        }

        public RuntimeUnit(string name, Type type, byte[] heap,
                RuntimeUnit[] depends, int nx) {
            this.name = name;
            this.type = type;
            this.heap = (heap == null ? heapreg[name] : heap);
            this.depends = depends;
            this.xref = new object[nx];
            this.methods = new DynBlockDelegate[nx];
            this.meta_fields = new FieldInfo[nx*2];
            this.lex_fields = new Dictionary<string,FieldInfo>();

            Kernel.ModulesFinished[name] = this;

            if (TraceLoad) {
                Console.WriteLine("Setting up unit {0}", type.Name);
                HexDump(this.heap);
            }

            uint d = 0;
            foreach (MethodInfo mi in type.GetMethods()) {
                int acc = 0;
                string n = mi.Name;
                if (n.Length < 2 || n[0] != 'C') continue;
                int i = 1;
                while (i < n.Length && (d = (uint)(n[i] - '0')) < 10) {
                    acc = acc * 10 + (int)d;
                    i++;
                }
                if (n[i] != '_') continue;
                methods[acc] = (DynBlockDelegate)
                    Delegate.CreateDelegate(typeof(DynBlockDelegate), mi);
            }
            foreach (FieldInfo fi in type.GetFields()) {
                int acc = 0;
                string n = fi.Name;
                int i = 1;
                while (i < n.Length && (d = (uint)(n[i] - '0')) < 10) {
                    acc = acc * 10 + (int)d;
                    i++;
                }
                if (n[i] != '_') continue;
                int index = acc * 2;
                if (n[0] == 'I') { }
                else if (n[0] == 'P') { index++; }
                else { lex_fields[n] = fi; continue; }
                meta_fields[index] = fi;
            }
        }

        public int ReadInt(ref int from) {
            uint r = 0;
            r |= ((uint)heap[from++] <<  0);
            r |= ((uint)heap[from++] <<  8);
            r |= ((uint)heap[from++] << 16);
            r |= ((uint)heap[from++] << 24);
            return (int)r;
        }

        public object ReadXref(ref int from) {
            int unit = ReadShort(ref from);
            if (unit == 0xFFFF) return null;
            RuntimeUnit ru = depends[unit] == null ? this : depends[unit];
            return ru.xref[ReadInt(ref from)];
        }

        public void ReadSignature(SubInfo si, ref int from) {
            si.sig_i = ReadIntArray(ref from);
            if (si.sig_i == null) return;
            si.sig_r = new object[ReadInt(ref from)];
            //Console.WriteLine("{0} {1} {2}", si.name, si.sig_i.Length, si.sig_r.Length);
            for (int i = 0; i < si.sig_r.Length; i++) {
                string st = ReadStr(ref from);
                if (st == null) {
                    si.sig_r[i] = ReadXref(ref from);
                    //Console.WriteLine("{0} {1}", si.sig_r[i].GetType(),
                    //    (si.sig_r[i] is SubInfo) ? ((SubInfo)si.sig_r[i]).name :
                    //    (si.sig_r[i] is STable) ? ((STable)si.sig_r[i]).name :
                    //    "<...>");
                } else
                    si.sig_r[i] = st;
            }
        }

        public int ReadShort(ref int from) {
            uint r = 0;
            r |= ((uint)heap[from++] <<  0);
            r |= ((uint)heap[from++] <<  8);
            return (int)r;
        }

        public int[] ReadIntArray(ref int from) {
            int l = ReadInt(ref from);
            if (l == -1) return null;
            int[] r = new int[l];
            for (int i = 0; i < r.Length; i++) r[i] = ReadInt(ref from);
            return r;
        }

        public string[] LoadStrArray(int from) {
            return ReadStrArray(ref from);
        }

        public string[] ReadStrArray(ref int from) {
            int l = ReadInt(ref from);
            if (l == -1) return null;
            string[] r = new string[l];
            for (int i = 0; i < r.Length; i++) r[i] = ReadStr(ref from);
            return r;
        }

        public BigInteger ReadBigInt(ref int from) {
            sbyte sign = (sbyte) heap[from++];
            int[] swords = ReadIntArray(ref from);
            uint[] words = new uint[swords.Length];
            for (int i = 0; i < swords.Length; i++) words[i] = (uint)swords[i];
            return new BigInteger(sign, words);
        }

        public Variable[] LoadVariablePool(int from) {
            Variable[] pool = new Variable[ReadInt(ref from)];
            for (int i = 0; i < pool.Length; i++) {
                byte type = heap[from++];
                BigInteger tn;
                ulong tmp = 0;
                switch(type) {
                    case 0:
                        pool[i] = Kernel.BoxAnyMO<string>(ReadStr(ref from),
                                Kernel.StrMO);
                        break;
                    case 1:
                        tmp = (uint)ReadInt(ref from);
                        tmp |= (((ulong)(uint)ReadInt(ref from)) << 32);
                        pool[i] = Kernel.BoxAnyMO<double>(
                                BitConverter.Int64BitsToDouble((long)tmp),
                                Kernel.NumMO);
                        break;
                    case 2:
                        pool[i] = Kernel.BoxAnyMO<int>(ReadInt(ref from),
                                Kernel.IntMO);
                        break;
                    case 3:
                        pool[i] = Kernel.BoxAnyMO(ReadBigInt(ref from),
                                Kernel.IntMO);
                        break;
                    case 4:
                        tn = ReadBigInt(ref from);
                        tmp = (uint)ReadInt(ref from);
                        tmp |= (((ulong)(uint)ReadInt(ref from)) << 32);
                        pool[i] = Kernel.BoxAnyMO(new Rat(tn, tmp), Kernel.RatMO);
                        break;
                    default:
                        throw new ArgumentException();
                }
            }
            return pool;
        }

        public AltInfo[] LoadAltInfoPool(int from) {
            AltInfo[] pool = new AltInfo[ReadInt(ref from)];
            for (int i = 0; i < pool.Length; i++)
                pool[i] = new AltInfo(ReadLADArr(ref from), ReadStr(ref from),
                    ReadIntArray(ref from));
            return pool;
        }

        public CC[] LoadCCPool(int from) {
            CC[] pool = new CC[ReadInt(ref from)];
            for (int i = 0; i < pool.Length; i++)
                pool[i] = new CC(ReadIntArray(ref from));
            return pool;
        }

        public string[][] LoadStrListPool(int from) {
            string[][] pool = new string[ReadInt(ref from)][];
            for (int i = 0; i < pool.Length; i++)
                pool[i] = ReadStrArray(ref from);
            return pool;
        }

        public CC[][] LoadCCListPool(int from) {
            CC[][] pool = new CC[ReadInt(ref from)][];
            for (int i = 0; i < pool.Length; i++) {
                pool[i] = new CC[ReadInt(ref from)];
                for (int j = 0; j < pool[i].Length; j++)
                    pool[i][j] = new CC(ReadIntArray(ref from));
            }
            return pool;
        }

        public string ReadStr(ref int from) {
            int l = ReadShort(ref from);
            if ((l & 2) == 0) l |= (ReadShort(ref from) << 16);
            if (l == 0xffff) return null;

            if ((l & 1) != 0) {
                int rf = l >> 2;
                return ReadStr(ref rf);
            }

            char[] r = new char[l >> 3];
            if ((l & 4) != 0) {
                for (int i = 0; i < r.Length; i++)
                    r[i] = (char)ReadShort(ref from);
            } else {
                for (int i = 0; i < r.Length; i++)
                    r[i] = (char)heap[from++];
            }
            return new string(r);
        }

        public const int SUB_RUN_ONCE = 1;
        public const int MAKE_PROTOPAD = 2;
        public const int SUB_HAS_TYPE = 4;
        public const int SUB_IS_UNSAFE = 8;
        public const int SUB_IS_PARAM_ROLE = 16;
        public const int SUB_MAINLINE = 32;

        public void LoadStashes(int from) {
            int ct = ReadInt(ref from);
            for (int i = 0; i < ct; i++) {
                string who  = ReadStr(ref from);
                string name = ReadStr(ref from);
                object what = ReadXref(ref from);

                if (TraceLoad)
                    Console.WriteLine("Creating stash slot {0} {1}", who, name);

                BValue slot = Kernel.GetVar(who, name);
                if (what == null) continue;

                Variable item = (what is SubInfo) ?
                    Kernel.NewROScalar(((SubInfo)what).protosub) :
                    ((STable)what).typeVar;
                slot.v = Kernel.StashyMerge(slot.v, item, who, name);
            }
        }

        public void LoadAllSubs(int from) {
            int[] froms = ReadIntArray(ref from);
            foreach (int f in froms)
                LoadSubInfo(f);
        }

        public void LoadSubInfo(int from) {
            int _ifrom = from;
            int ix = ReadInt(ref from);
            byte spec = heap[from++];

            SubInfo ns = new SubInfo(
                ReadStr(ref from), /*name*/
                ReadIntArray(ref from), /*lines*/
                methods[ix],
                (SubInfo)ReadXref(ref from), /*outer*/
                ReadLAD(ref from),
                ReadIntArray(ref from), /*ehspan*/
                ReadStrArray(ref from), /*ehlabel*/
                ReadInt(ref from)); /*nspill*/

            if (TraceLoad)
                Console.WriteLine("Installing sub {0} \"{1}\" from {2:X}", ix, ns.name, _ifrom);

            int dylexc = ReadInt(ref from);
            ns.dylex = new Dictionary<string,LexInfo>();

            LexInfo li = null;
            for (int i = 0; i < dylexc; i++) {
                string name = ReadStr(ref from);
                ns.dylex_filter |= SubInfo.FilterForName(name);
                switch (heap[from++]) {
                    case 0: li = new LISlot(ReadInt(ref from)); break;
                    case 1: li = new LIField(lex_fields[ReadStr(ref from)]); break;
                    case 2: li = new LIBField(lex_fields[ReadStr(ref from)]); break;
                    case 3: li = new LIAlias(ReadStr(ref from)); break;
                    case 4: li = new LIPackage((STable)ReadXref(ref from)); break;
                    default: throw new ArgumentException("dylex typecode");
                }
                ns.dylex[name] = li;
            }

            SubInfo sc = ns.outer;
            for (ns.outer_topic_rank = 1; sc != null; sc = sc.outer) {
                if (sc.dylex != null && sc.dylex.TryGetValue("$_", out li))
                    break;
                ns.outer_topic_rank++;
            }
            ns.outer_topic_key = (li is LISlot) ? (li as LISlot).slot : -1;
            ns.self_key = -1;
            if (ns.dylex != null && ns.dylex.TryGetValue("self", out li) &&
                    li is LISlot)
                ns.self_key = (li as LISlot).slot;
            //Console.WriteLine("{0} {1} {2}", name, outer_topic_rank, outer_topic_key);
            xref[ix] = ns;
            ns.xref_no = ix;
            ns.unit = this;

            ns.special = spec;
            if ((spec & SUB_IS_UNSAFE) != 0)
                Kernel.CheckUnsafe(ns);
            if ((spec & SUB_HAS_TYPE) != 0)
                ns.mo = (STable) ReadXref(ref from);

            Frame out_pad = ns.outer == null ? null : ns.outer.protopad;

            if ((spec & SUB_MAINLINE) != 0)
                out_pad = context_pad ?? out_pad;

            if (out_pad != null || ns.outer == null)
                ns.protosub = Kernel.MakeSub(ns, out_pad);
            if ((spec & MAKE_PROTOPAD) != 0)
                ns.protopad = new Frame(null, out_pad, ns, ns.protosub);
            if ((spec & SUB_IS_PARAM_ROLE) != 0)
                ((STable) ReadXref(ref from)).FillParametricRole(ns.protosub);

            if (TraceLoad && ns.protopad != null) Console.WriteLine("created protopad");
            if (TraceLoad && out_pad != null) Console.WriteLine("has out_pad");

            ns.fixups_from = from;

            meta_fields[ix*2].SetValue(null, ns);
            if (ns.protopad != null)
                meta_fields[ix*2+1].SetValue(null, ns.protopad);
        }

        public void FixupSubs() {
            for (int i = 0; i < xref.Length; i++) {
                STable mo = xref[i] as STable;
                if (mo == null) continue;
                int from = mo.fixups_from;
                if (TraceLoad)
                    Console.WriteLine("Finishing load of package {0} \"{1}\" from {2:X}", i, mo.name, from);
                STable[] superclasses;
                STable[] mro;
                string[] slots;

                switch (heap[from++]) {
                    case 0: //role
                        superclasses = new STable[ReadInt(ref from)];
                        for (int j = 0; j < superclasses.Length; j++)
                            superclasses[j] = (STable)ReadXref(ref from);
                        mo.FillRole(superclasses, new STable[0]);
                        break;
                    case 1: //p. role
                        break;
                    case 2: //class
                        slots = ReadStrArray(ref from);
                        superclasses = new STable[ReadInt(ref from)];
                        for (int j = 0; j < superclasses.Length; j++)
                            superclasses[j] = (STable)ReadXref(ref from);
                        mro = new STable[ReadInt(ref from)];
                        for (int j = 0; j < mro.Length; j++)
                            mro[j] = (STable)ReadXref(ref from);
                        mo.FillClass(slots, superclasses, mro);
                        break;
                    case 3: //module
                        break;
                    case 4: //package
                        mo.mo.isPackage = true;
                        break;
                    case 5: //subset
                        mo.mo.FillSubset((STable)ReadXref(ref from));
                        break;
                    default:
                        throw new ArgumentException();
                }

                if (mo.isSubset) {
                    mo.initVar = mo.mo.superclasses[0].initVar;
                    mo.initObject = mo.mo.superclasses[0].initObject;
                } else {
                    mo.initVar = mo.typeVar;
                    mo.initObject = mo.typeObject;
                }

                ReadClassMembers(mo, ref from);
                if (mo.mo.isSubset)
                    mo.mo.subsetWhereThunk = ((SubInfo)ReadXref(ref from)).protosub;
                mo.how = new BoxObject<STable>(mo, ((STable)ReadXref(ref from)), 0);
            }

            // bulk inform objects that thaw is complete...
            P6how.BulkRevalidate(xref);

            for (int i = 0; i < xref.Length; i++) {
                SubInfo si = xref[i] as SubInfo;
                if (si == null) continue;
                int from = si.fixups_from;
                if (TraceLoad)
                    Console.WriteLine("Finishing load of sub {0} \"{1}\" from {2:X}", i, si.name, from);
                si.cur_pkg = ((STable)ReadXref(ref from)).who;
                ReadSignature(si, ref from);
                if (TraceLoad) Console.WriteLine("Sig loaded");
                int ph = heap[from++];
                if (ph != 0xFF) Kernel.AddPhaser(ph, si.protosub);
            }
        }

        public STable LoadPackage(int from, STable existing_mo) {
            int _ifrom = from;
            int ix = ReadInt(ref from);
            string name = ReadStr(ref from);
            string who  = ReadStr(ref from);
            if (TraceLoad)
                Console.WriteLine("Installing package {0} \"{1}\" from {2:X}", ix, name, _ifrom);
            STable mo = existing_mo != null ? existing_mo :
                new STable(name);
            xref[ix] = mo;

            mo.who = Kernel.GetStash(who);
            mo.typeObject = new P6opaque(mo, 0);
            ((P6opaque)mo.typeObject).slots = null;
            mo.typeVar = Kernel.NewROScalar(mo.typeObject);

            mo.fixups_from = from;

            return mo;
        }

        public LAD LoadLAD(int from) {
            return ReadLAD(ref from);
        }

        public LAD[] LoadLADArr(int from) {
            return ReadLADArr(ref from);
        }

        public LAD ReadLAD(ref int from) {
            switch(heap[from++]) {
                case 0: return null;
                case 1: return new LADCC(new CC(ReadIntArray(ref from)));
                case 2: return new LADStr(ReadStr(ref from));
                case 3: return new LADParam(ReadStr(ref from));
                case 4: return new LADMethod(ReadStr(ref from));
                case 5: return new LADDispatcher();
                case 6: return new LADStrNoCase(ReadStr(ref from));
                case 7: return new LADImp();
                case 8: return new LADDot();
                case 9: return new LADNone();
                case 10: return new LADNull();
                case 11: return new LADPlus(ReadLAD(ref from));
                case 12: return new LADStar(ReadLAD(ref from));
                case 13: return new LADOpt(ReadLAD(ref from));
                case 14: return new LADSequence(ReadLADArr(ref from));
                case 15: return new LADAny(ReadLADArr(ref from));
                default: throw new ArgumentException();
            }
        }

        public LAD[] ReadLADArr(ref int from) {
            LAD[] r = new LAD[ReadShort(ref from)];
            for (int i = 0; i < r.Length; i++) r[i] = ReadLAD(ref from);
            return r;
        }

        public void ReadClassMembers(STable into, ref int from) {
            int nmethods = ReadInt(ref from);
            if (nmethods < 0) return; // param role

            for (int i = 0; i < nmethods; i++) {
                into.AddMethod(ReadInt(ref from), ReadStr(ref from),
                    ((SubInfo)ReadXref(ref from)).protosub);
            }

            int nattr = ReadInt(ref from);

            for (int i = 0; i < nattr; i++) {
                string name = ReadStr(ref from);
                int flags = heap[from++];
                SubInfo init = ReadXref(ref from) as SubInfo;
                STable type = ReadXref(ref from) as STable;
                into.AddAttribute(name, flags,
                        init != null ? init.protosub : null,
                        type);
            }
        }
    }

    public abstract class LexInfo {
        public abstract object Get(Frame f);
        public virtual void Set(Frame f, object to) {
            throw new NieczaException("Variable cannot be bound");
        }
    }

    public class LISlot : LexInfo {
        public readonly int slot;
        public LISlot(int slot) { this.slot = slot; }
        public override object Get(Frame f) { return f.GetDynamic(slot); }
        public override void Set(Frame f, object to) { f.SetDynamic(slot,to); }
    }

    public class LIField : LexInfo {
        FieldInfo fi;
        public LIField(FieldInfo fi) { this.fi = fi; }
        public override object Get(Frame f) { return fi.GetValue(null); }
        public override void Set(Frame f, object to) { fi.SetValue(null, to); }
    }

    public class LIBField : LexInfo {
        FieldInfo fi;
        public LIBField(FieldInfo fi) { this.fi = fi; }
        public override object Get(Frame f) {
            return ((BValue)fi.GetValue(null)).v;
        }
        public override void Set(Frame f, object to) {
            ((BValue)fi.GetValue(null)).v = (Variable)to;
        }
    }

    public class LIAlias : LexInfo {
        string name;
        public LIAlias(string name) { this.name = name; }
        object Common(Frame f, bool set, object to) {
            Frame cr = f;
            LexInfo li = null;
            while (cr.info.dylex == null ||
                    !cr.info.dylex.TryGetValue(name, out li))
                cr = cr.outer;
            if (!set) return li.Get(cr);
            else { li.Set(cr, to); return null; }
        }
        public override object Get(Frame f) { return Common(f,false,null); }
        public override void Set(Frame f, object to) { Common(f,true,to); }
    }

    public class LIPackage : LexInfo {
        STable pkg;
        public LIPackage(STable pkg) { this.pkg = pkg; }
        public override object Get(Frame f) { return pkg.typeVar; }
    }

    // This stores all the invariant stuff about a Sub, i.e. everything
    // except the outer pointer.  Now distinct from protopads
    //
    // Actually not quite *in*variant; some of this stuff has to be
    // changed, but it's rare by construction.  We don't want to be
    // like Rakudo/Parrot where simple sub cloning requires copying
    // 100s of bytes.
    public class SubInfo {
        // Essential call functions
        public DynBlockDelegate code;
        public int nspill;
        public int[] sig_i;
        public object[] sig_r;

        // Standard metadata
        public Dictionary<string, LexInfo> dylex;
        public uint dylex_filter; // (32,1) Bloom on hash code
        public int[] lines;
        public STable mo;
        // for inheriting hints
        public SubInfo outer;
        public P6any protosub;
        public Frame protopad;
        public RuntimeUnit unit;
        public int xref_no;
        public int fixups_from;
        public string name;
        public Dictionary<string, BValue> hints;
        // maybe should be a hint
        public LAD ltm;

        public int special;
        public P6any cur_pkg;
        public int outer_topic_rank;
        public int outer_topic_key;
        public int self_key;

        // Used for closing runtime-generated SubInfo over values used
        // For vtable wrappers: 0 = unboxed, 1 = boxed
        // For dispatch routines, 0 = parameter list
        public object param0, param1;

        // No instance fields past this point
        public const int SIG_I_RECORD  = 3;
        public const int SIG_I_FLAGS   = 0;
        public const int SIG_I_SLOT    = 1;
        public const int SIG_I_NNAMES  = 2;

        // R records are variable size, but contain canonical name,
        // usable names (in order), default SubInfo (if present),
        // type STable (if present)

        // Value processing
        public const int SIG_F_HASTYPE    = 1; // else Kernel.AnyMO
        public const int SIG_F_MULTI_IGNORED = 16384;

        // Value binding
        public const int SIG_F_READWRITE  = 2;
        public const int SIG_F_RWTRANS    = 8;
        public const int SIG_F_BINDLIST   = 16;
        public const int SIG_F_INVOCANT   = 8192;
        public const int SIG_F_IS_COPY    = 32768;
        public const int SIG_F_IS_LIST    = 65536;
        public const int SIG_F_IS_HASH    = 131072;

        // Value source
        public const int SIG_F_HASDEFAULT = 32;
        public const int SIG_F_OPTIONAL   = 64;
        public const int SIG_F_DEFOUTER   = 4096;
        public const int SIG_F_POSITIONAL = 128;
        public const int SIG_F_SLURPY_POS = 256;
        public const int SIG_F_SLURPY_NAM = 512;
        public const int SIG_F_SLURPY_CAP = 1024;
        public const int SIG_F_SLURPY_PCL = 2048;

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
        public const int ON_VARLOOKUP = 10;
        // ON_VARLOOKUP is kinda special, it's not used for exceptions
        // but rather for $*FOO and the like; goto = the variable index
        public int[] edata;
        public string[] label_names;

        private static string[] controls = new string[] { "unknown", "next",
            "last", "redo", "return", "die", "succeed", "proceed", "goto",
            "nextsame/nextwith" };
        public static string DescribeControl(int type, Frame tgt,
                string name) {
            string ty = (type < controls.Length) ? controls[type] : "unknown";
            if (name != null) {
                return ty + "(" + name + (tgt != null ? ", lexotic)" : ", dynamic)");
            } else {
                return ty;
            }
        }

        public int GetInlineSlot(int ip, string name, int depth) {
            int end = 0;
            // ON_VARLOOKUP nodes are created in a set of 0 or more,
            // followed by a nameless one that marks the depth.
            // Find the correct end-node for the passed depth
            while (end < edata.Length) {
                if (edata[end+2] == ON_VARLOOKUP && edata[end+4] < 0 &&
                        edata[end+3] == depth && ip >= edata[end] &&
                        ip < edata[end+1])
                    break;
                end += 5;
            }
            if (end == edata.Length) return -1;
            while (true) {
                end -= 5;
                if (end < 0 || edata[end+2] != ON_VARLOOKUP ||
                        edata[end+4] < 0) // we've passed the end
                    return -1;
                if (name.Equals(label_names[edata[end+4]]))
                    return edata[end+3];
            }
        }

        public int FindControlEnt(int ip, int ty, string name) {
            for (int i = 0; i < edata.Length; i+=5) {
                if (ip < edata[i] || ip >= edata[i+1])
                    continue;
                if (ty != edata[i+2])
                    continue;
                if (name != null && (edata[i+4] < 0 || !name.Equals(label_names[edata[i+4]])))
                    continue;
                if (name == null && ty == ON_VARLOOKUP && edata[i+4] >= 0)
                    continue;
                return edata[i+3];
            }
            return -1;
        }

        private string PName(int rbase) {
            return ((string)sig_r[rbase]) + " in " + name;
        }
        public unsafe Frame Binder(Frame caller, Frame outer, P6any sub,
                Variable[] pos, VarHash named, bool quiet, DispatchEnt de) {
            Frame th;
            if ((special & RuntimeUnit.SUB_RUN_ONCE) != 0) {
                th = protopad;
                th.caller = caller;
                th.ip = 0;
                if (Frame.TraceCalls)
                    Console.WriteLine("{0}\t{1}", caller.info.name, name);
            } else {
                th = caller.MakeChild(outer, this, sub);
            }
            th.curDisp = de;
            th.pos = pos;
            th.named = named;
            // If we call an inferior runloop from the binder, we need for the
            // runloop to not overwrite th, which it would if the top frame
            // stayed 'caller'.
            Kernel.SetTopFrame(th);
            int[] ibuf = sig_i;
            if (ibuf == null) return th;
            int posc = 0;
            HashSet<string> namedc = null;
            int jun_pivot = -1;
            string jun_pivot_n = null;
            int jun_rank = int.MaxValue;
            if (named != null)
                namedc = new HashSet<string>(named.Keys);
            if (ibuf.Length == 0) goto noparams;
            fixed (int* ibase = ibuf) {
            int* ic = ibase;
            int* iend = ic + (ibuf.Length - 2);
            object[] rbuf = sig_r;
            int rc = 0;
            int obj_src = -1;
            string obj_src_n = null;

            while (ic < iend) {
                int flags = *(ic++);
                int slot  = *(ic++);
                int names = *(ic++);
                int rbase = rc;
                rc += (1 + names);
                if ((flags & SIG_F_HASDEFAULT) != 0) rc++;
                STable type = Kernel.AnyMO;
                obj_src = -1;
                if ((flags & SIG_F_HASTYPE) != 0)
                    type = (STable)rbuf[rc++];

                Variable src = null;
                if ((flags & SIG_F_SLURPY_PCL) != 0) {
                    src = Kernel.BoxAnyMO(pos, Kernel.ParcelMO);
                    posc  = pos.Length;
                    goto gotit;
                }
                if ((flags & SIG_F_SLURPY_CAP) != 0) {
                    P6any nw = new P6opaque(Kernel.CaptureMO);
                    nw.SetSlot("positionals", pos);
                    nw.SetSlot("named", named);
                    src = Kernel.NewROScalar(nw);
                    named = null; namedc = null; posc = pos.Length;
                    goto gotit;
                }
                if ((flags & SIG_F_SLURPY_POS) != 0) {
                    P6any l = new P6opaque(Kernel.ListMO);
                    Kernel.IterToList(l, Kernel.IterFlatten(
                                Kernel.SlurpyHelper(th, posc)));
                    src = Kernel.NewRWListVar(l);
                    posc = pos.Length;
                    goto gotit;
                }
                if ((flags & SIG_F_SLURPY_NAM) != 0) {
                    VarHash nh = new VarHash();
                    if (named != null) {
                        foreach (KeyValuePair<string,Variable> kv in named)
                            if (namedc.Contains(kv.Key))
                                nh[kv.Key] = kv.Value;
                        named = null;
                        namedc = null;
                    }
                    src = Kernel.BoxAnyMO(nh, Kernel.HashMO);
                    goto gotit;
                }
                if (names != 0 && named != null) {
                    for (int ni = 1; ni <= names; ni++) {
                        string n = (string)rbuf[rbase+ni];
                        if (namedc.Contains(n)) {
                            namedc.Remove(n);
                            src = named[n];
                            obj_src_n = n;
                            obj_src = -2;
                            goto gotit;
                        }
                    }
                }
                if ((flags & SIG_F_POSITIONAL) != 0 && posc != pos.Length) {
                    obj_src = posc;
                    src = pos[posc++];
                    goto gotit;
                }
                if ((flags & SIG_F_HASDEFAULT) != 0) {
                    Frame thn = Kernel.GetInferiorRoot()
                        .MakeChild(th, (SubInfo) rbuf[rbase + 1 + names],
                            Kernel.AnyP);
                    src = Kernel.RunInferior(thn);
                    if (src == null)
                        throw new Exception("Improper null return from sub default for " + PName(rbase));
                    goto gotit;
                }
                if ((flags & SIG_F_DEFOUTER) != 0) {
                    Frame f = th;
                    if (outer_topic_key < 0) {
                        src = Kernel.AnyMO.typeVar;
                        goto gotit;
                    }
                    for (int i = 0; i < outer_topic_rank; i++) f = f.outer;
                    src = (Variable)f.GetDynamic(outer_topic_key);
                    goto gotit;
                }
                if ((flags & SIG_F_OPTIONAL) != 0) {
                    src = type.initVar;
                    goto gotit;
                }
                if (quiet) return null;
                return Kernel.Die(th, "No value for parameter " + PName(rbase));
gotit:
                if ((flags & SIG_F_RWTRANS) != 0) {
                } else if ((flags & SIG_F_IS_COPY) != 0) {
                    if ((flags & SIG_F_IS_HASH) != 0)
                        src = Kernel.Assign(Kernel.CreateHash(),
                            Kernel.NewRWListVar(src.Fetch()));
                    else if ((flags & SIG_F_IS_LIST) != 0)
                        src = Kernel.Assign(Kernel.CreateArray(),
                            Kernel.NewRWListVar(src.Fetch()));
                    else
                        src = Kernel.Assign(Kernel.NewTypedScalar(type), src);
                } else {
                    bool islist = ((flags & SIG_F_BINDLIST) != 0);
                    bool rw     = ((flags & SIG_F_READWRITE) != 0) && !islist;
                    P6any srco  = src.Fetch();

                    if (!srco.Does(type)) {
                        if (quiet) return null;
                        if (srco.mo.HasMRO(Kernel.JunctionMO) && obj_src != -1) {
                            int jrank = Kernel.UnboxAny<int>((P6any) ((P6opaque)srco).slots[0]) / 2;
                            if (jrank < jun_rank) {
                                jun_rank = jrank;
                                jun_pivot = obj_src;
                                jun_pivot_n = obj_src_n;
                            }
                            continue;
                        }
                        return Kernel.Die(th, "Nominal type check failed in binding " + PName(rbase) + "; got " + srco.mo.name + ", needed " + type.name);
                    }

                    if (rw) {
                        if (src.rw) {
                            // this will be a functional RW binding
                            if (src.whence != null)
                                Kernel.Vivify(src);
                            goto bound;
                        } else {
                            if (quiet) return null;
                            return Kernel.Die(th, "Binding " + PName(rbase) + ", cannot bind read-only value to is rw parameter");
                        }
                    }
                    else {
                        if (!src.rw && islist == src.islist)
                            goto bound;
                        src = new SimpleVariable(false, islist, srco.mo,
                                null, srco);
                    }
bound: ;
                }
                if ((flags & SIG_F_INVOCANT) != 0 && self_key >= 0)
                    th.SetDynamic(self_key, src);
                switch (slot + 1) {
                    case 0: break;
                    case 1:  th.lex0 = src; break;
                    case 2:  th.lex1 = src; break;
                    case 3:  th.lex2 = src; break;
                    case 4:  th.lex3 = src; break;
                    case 5:  th.lex4 = src; break;
                    case 6:  th.lex5 = src; break;
                    case 7:  th.lex6 = src; break;
                    case 8:  th.lex7 = src; break;
                    case 9:  th.lex8 = src; break;
                    case 10: th.lex9 = src; break;
                    default: th.lexn[slot - 10] = src; break;
                }
            }
            }
noparams:

            if (posc != pos.Length || namedc != null && namedc.Count != 0) {
                if (quiet) return null;
                string m = "Excess arguments to " + name;
                if (posc != pos.Length)
                    m += string.Format(", used {0} of {1} positionals",
                            posc, pos.Length);
                if (namedc != null && namedc.Count != 0)
                    m += ", unused named " + Kernel.JoinS(", ", namedc);
                return Kernel.Die(th, m);
            }

            if (jun_pivot != -1 && !quiet) {
                Variable jct = (jun_pivot == -2 ? named[jun_pivot_n] :
                        pos[jun_pivot]);
                th = caller.MakeChild(null, AutoThreadSubSI, Kernel.AnyP);

                P6opaque jo  = (P6opaque) jct.Fetch();

                th.named = named;
                th.pos = pos;
                th.lex1 = this;
                th.lex2 = jun_pivot_n;
                th.lex3 = jo.slots[0];
                th.lex4 = Kernel.UnboxAny<Variable[]>((P6any)jo.slots[1]);
                th.lex5 = outer;
                th.lex6 = sub;
                th.lex7 = de;
                th.lex8 = new Variable[((Variable[])th.lex4).Length];
                th.lex9 = jct;

                th.lexi0 = jun_pivot;
                th.lexi1 = 0;

                return th;
            }

            return th;
        }

        static SubInfo AutoThreadSubSI = new SubInfo("KERNEL AutoThreadSub", AutoThreadSubC);
        static Frame AutoThreadSubC(Frame th) {
            Variable[] src = (Variable[]) th.lex4;
            Variable[] dst = (Variable[]) th.lex8;
            if (th.lexi1 > 0)
                dst[th.lexi1 - 1] = (Variable)th.resultSlot;

            if (th.lexi1 == dst.Length) {
                P6opaque nj = new P6opaque(Kernel.JunctionMO);
                nj.slots[0] = th.lex3;
                nj.slots[1] = Kernel.BoxRaw(dst, Kernel.ParcelMO);
                // restore, in case our caller is using this
                if (th.lexi0 == -2)
                    th.named[(string)th.lex2] = (Variable)th.lex9;
                else
                    th.pos[th.lexi0] = (Variable)th.lex9;
                th.caller.resultSlot = Kernel.NewROScalar(nj);
                return th.caller;
            }

            if (th.lexi0 == -2)
                th.named[(string)th.lex2] = src[th.lexi1++];
            else
                th.pos[th.lexi0] = src[th.lexi1++];

            return ((SubInfo)th.lex1).Binder(th, (Frame)th.lex5, (P6any)th.lex6,
                th.pos, th.named, false, (DispatchEnt)th.lex7);
        }

        public BValue AddHint(string name) {
            if (hints == null)
                hints = new Dictionary<string,BValue>();
            if (hints.ContainsKey(name))
                return hints[name];
            return hints[name] = new BValue(Kernel.AnyMO.typeVar);
        }

        public void SetStringHint(string name, string value) {
            AddHint(name).v = Kernel.BoxAnyMO<string>(value, Kernel.StrMO);
        }

        public bool GetLocalHint(string name, out BValue val) {
            return (hints != null && hints.TryGetValue(name, out val));
        }

        public bool GetHint(string name, out BValue val) {
            for (SubInfo s = this; s != null; s = s.outer)
                if (s.GetLocalHint(name, out val))
                    return true;
            val = null;
            return false;
        }

        public static uint FilterForName(string name) {
            if (name.Length < 2 || name[1] != '*') return 0;
            uint hash = (uint)(name.GetHashCode() * FILTER_SALT);
            return 1u << (int)(hash >> 27);
        }

        public SubInfo(string name, int[] lines, DynBlockDelegate code,
                SubInfo outer, LAD ltm, int[] edata, string[] label_names,
                int nspill) {
            this.lines = lines;
            this.code = code;
            this.outer = outer;
            this.ltm = ltm;
            this.name = name;
            this.edata = edata;
            this.label_names = label_names;
            this.nspill = nspill;
            this.dylex = new Dictionary<string,LexInfo>();
            for (int i = 0; i < edata.Length; i += 5)
                if (edata[i+2] == ON_VARLOOKUP && edata[i+4] >= 0)
                    dylex_filter |= FilterForName(label_names[edata[i+4]]);
        }

        public SubInfo(string name, DynBlockDelegate code) :
            this(name, null, code, null, null, new int[0], null, 0) { }
    }

    // We need hashy frames available to properly handle BEGIN; for the time
    // being, all frames will be hashy for simplicity
    public class Frame: P6any {
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
        // statistically, most subs have few lexicals; since Frame objects
        // are reused, bloating them doesn't hurt much
        public object lex0;
        public object lex1;
        public object lex2;
        public object lex3;
        public object lex4;
        public object lex5;
        public object lex6;
        public object lex7;
        public object lex8;
        public object lex9;

        public int lexi0;
        public int lexi1;

        public object[] lexn;

        public DispatchEnt curDisp;
        public RxFrame rx;
        public P6any sub;

        public Variable[] pos;
        public VarHash named;

        // after MakeSub, GatherHelper
        public const int SHARED = 1;
        public int flags;

        public Frame(Frame caller_, Frame outer_,
                SubInfo info_, P6any sub_) {
            caller = caller_;
            outer = outer_;
            code = info_.code;
            info = info_;
            sub = sub_;
            mo = Kernel.CallFrameMO;
            lexn = (info_.nspill > 0) ? new object[info_.nspill] : null;
        }

        public Frame() { mo = Kernel.CallFrameMO; }

        public static readonly bool TraceCalls =
            Environment.GetEnvironmentVariable("NIECZA_TRACE_CALLS") != null;
        public static readonly bool VerboseExceptions =
            Environment.GetEnvironmentVariable("NIECZA_VERBOSE_EXCEPTIONS") != null;

        public Frame MakeChild(Frame outer, SubInfo info, P6any sub) {
            if (reusable_child == null) {
                reusable_child = new Frame();
                reusable_child.reuser = this;
            }
            if (TraceCalls)
                Console.WriteLine("{0}\t{1}", this.info.name, info.name);
            reusable_child.ip = 0;
            reusable_child.resultSlot = null;
            reusable_child.lexn = (info.nspill != 0) ? new object[info.nspill] : null;
            reusable_child.lex = null;
            reusable_child.lex0 = null;
            reusable_child.lex1 = null;
            reusable_child.lex2 = null;
            reusable_child.lex3 = null;
            reusable_child.lex4 = null;
            reusable_child.lex5 = null;
            reusable_child.lex6 = null;
            reusable_child.lex7 = null;
            reusable_child.lex8 = null;
            reusable_child.lex9 = null;
            reusable_child.curDisp = null;
            reusable_child.caller = this;
            reusable_child.outer = outer;
            reusable_child.info = info;
            reusable_child.sub = sub;
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

        public Variable GetArgs() {
            P6any nw = new P6opaque(Kernel.CaptureMO);
            nw.SetSlot("positionals", pos ?? new Variable[0]);
            nw.SetSlot("named", named);
            return Kernel.NewROScalar(nw);
        }

        public string DescribeArgs() {
            string ret = null;
            try {
                Variable a = GetArgs();
                Variable sa = Kernel.RunInferior(a.Fetch().InvokeMethod(
                    Kernel.GetInferiorRoot(), "perl", new Variable[] { a },
                    null));
                ret = sa.Fetch().mo.mro_raw_Str.Get(sa);
            } catch (Exception ex) {
                ret = "[cannot display arguments: " + ex + "]";
            }
            return ret;
        }

        public string ExecutingFile() {
            BValue l;
            SubInfo i = info;
            if (i.GetHint("$?FILE", out l))
                return l.v.Fetch().mo.mro_raw_Str.Get(l.v);
            return "";
        }

        public void SetDynamic(int ix, object v) {
            switch(ix) {
                case 0: lex0 = v; break;
                case 1: lex1 = v; break;
                case 2: lex2 = v; break;
                case 3: lex3 = v; break;
                case 4: lex4 = v; break;
                case 5: lex5 = v; break;
                case 6: lex6 = v; break;
                case 7: lex7 = v; break;
                case 8: lex8 = v; break;
                case 9: lex9 = v; break;
                default: lexn[ix-10] = v; break;
            }
        }

        public bool TryBindDynamic(string name, uint mask, object to) {
            if ((info.dylex_filter & mask) != mask)
                return false;
            int ix;
            LexInfo li;
            if ((ix = info.FindControlEnt(ip, SubInfo.ON_VARLOOKUP, name)) >= 0)
                SetDynamic(ix, to);
            else if (info.dylex.TryGetValue(name, out li))
                li.Set(this, to);
            else
                return false;

            return true;
        }

        public bool TryGetDynamic(string name, uint mask, out object v) {
            v = null;
            if (lex != null && lex.TryGetValue(name, out v))
                return true;
            if ((info.dylex_filter & mask) != mask)
                return false;

            int ix;
            LexInfo li;

            if ((ix = info.FindControlEnt(ip, SubInfo.ON_VARLOOKUP, name))>=0)
                v = GetDynamic(ix);
            else if (info.dylex.TryGetValue(name, out li))
                v = li.Get(this);
            else
                return false;

            return true;
        }

        public object GetDynamic(int ix) {
            switch(ix) {
                case 0: return lex0;
                case 1: return lex1;
                case 2: return lex2;
                case 3: return lex3;
                case 4: return lex4;
                case 5: return lex5;
                case 6: return lex6;
                case 7: return lex7;
                case 8: return lex8;
                case 9: return lex9;
                default: return lexn[ix-10];
            }
        }

        public Variable LexicalFind(string name) {
            Frame csr = this;
            if (name.Length >= 2 && name[1] == '?') {
                BValue b;
                if (info.GetHint(name, out b))
                    return b.v;
                else
                    return Kernel.AnyMO.typeVar;
            }
            uint m = SubInfo.FilterForName(name);
            while (csr != null) {
                object o;
                if (csr.TryGetDynamic(name, m, out o)) {
                    return (Variable)o;
                }
                csr = csr.outer;
            }
            return Kernel.AnyMO.typeVar;
        }

        public void LexicalBind(string name, Variable to) {
            Frame csr = this;
            uint m = SubInfo.FilterForName(name);
            while (csr != null) {
                if (csr.TryBindDynamic(name, m, to))
                    return;
                csr = csr.outer;
            }
            throw new NieczaException("cannot bind " + name + " in " + info.name);
        }

        public Frame DynamicCaller() {
            if (lex == null || !lex.ContainsKey("!return"))
                return caller;
            return (Frame) lex["!return"];
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

    class InvokeSub : InvokeHandler {
        public override Frame Invoke(P6any th, Frame caller,
                Variable[] pos, VarHash named) {
            if (!th.IsDefined())
                return Kernel.Die(caller, "Cannot invoke an undef sub");
            P6opaque dyo = ((P6opaque) th);
            Frame outer = (Frame) dyo.slots[0];
            SubInfo info = (SubInfo) dyo.slots[1];

            return info.Binder(caller, outer, th, pos, named, false, null);
        }
    }

    class InvokeCallMethod : InvokeHandler {
        public override Frame Invoke(P6any th, Frame caller,
                Variable[] pos, VarHash named) {
            Variable[] np = new Variable[pos.Length + 1];
            Array.Copy(pos, 0, np, 1, pos.Length);
            np[0] = Kernel.NewROScalar(th);
            return th.InvokeMethod(caller, "postcircumfix:<( )>", np, named);
        }
    }

    class PushyCallMethod : PushyHandler {
        string method;
        public PushyCallMethod(string method) { this.method = method; }

        public override Variable Invoke(Variable obj, Variable[] args) {
            Variable[] rargs = new Variable[args.Length + 1];
            Array.Copy(args, 0, rargs, 1, args.Length);
            rargs[0] = obj;
            return Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, rargs, null));
        }
    }

    class CtxCallMethodUnbox<T> : ContextHandler<T> {
        string method;
        public CtxCallMethodUnbox(string method) { this.method = method; }

        public override T Get(Variable obj) {
            Variable v = Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, new Variable[] { obj }, null));
            return Kernel.UnboxAny<T>(v.Fetch());
        }
    }
    class CtxCallMethodUnboxBool : ContextHandler<bool> {
        string method;
        public CtxCallMethodUnboxBool(string method) { this.method = method; }

        public override bool Get(Variable obj) {
            Variable v = Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, new Variable[] { obj }, null));
            return Kernel.UnboxAny<int>(v.Fetch()) != 0;
        }
    }

    class CtxCallMethodUnboxNumeric : ContextHandler<double> {
        string method;
        public CtxCallMethodUnboxNumeric(string method) { this.method = method; }

        public override double Get(Variable obj) {
            Variable v = method == null ? obj : Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, new Variable[] { obj }, null));
            P6any o = v.Fetch();
            if (o.mo.HasMRO(Kernel.NumMO)) {
                return Kernel.UnboxAny<double>(o);
            } else if (o.mo.HasMRO(Kernel.IntMO)) {
                if (o is BoxObject<int>) {
                    return (double)Kernel.UnboxAny<int>(o);
                } else {
                    return (double)Kernel.UnboxAny<BigInteger>(o);
                }
            } else if (o.mo.HasMRO(Kernel.RatMO)) {
                Rat r = Kernel.UnboxAny<Rat>(o);
                return (double)r.num / (double)r.den;
            } else if (o.mo.HasMRO(Kernel.FatRatMO)) {
                FatRat r = Kernel.UnboxAny<FatRat>(o);
                return (double)r.num / (double)r.den;
            } else if (o.mo.HasMRO(Kernel.ComplexMO)) {
                Complex r = Kernel.UnboxAny<Complex>(o);
                if (r.im != 0)
                    throw new NieczaException("coercion would discard nonzero imaginary part");
                return r.re;
            } else {
                throw new NieczaException("Numeric failed to return core numeric type");
            }
        }
    }

    class CtxCallMethod : ContextHandler<Variable> {
        string method;
        public CtxCallMethod(string method) { this.method = method; }

        public override Variable Get(Variable obj) {
            return Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, new Variable[] { obj }, null));
        }
    }

    class CtxCallMethodFetch : ContextHandler<P6any> {
        string method;
        public CtxCallMethodFetch(string method) { this.method = method; }

        public override P6any Get(Variable obj) {
            return Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, new Variable[] { obj }, null)).Fetch();
        }
    }

    class CtxJustUnbox<T> : ContextHandler<T> {
        T dflt;
        public CtxJustUnbox(T dflt) { this.dflt = dflt; }
        public override T Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) return dflt;
            return Kernel.UnboxAny<T>(o);
        }
    }

    class CtxBoolUnbox : ContextHandler<bool> {
        public override bool Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) return false;
            return Kernel.UnboxAny<int>(o) != 0;
        }
    }

    class CtxReturnSelf : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            return Kernel.NewROScalar(obj.Fetch());
        }
    }

    class CtxReturnSelfList : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            if (obj.islist) return obj;
            return Kernel.NewRWListVar(obj.Fetch());
        }
    }

    class CtxReturnSelfItem : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            if (!obj.islist) return obj;
            return Kernel.NewROScalar(obj.Fetch());
        }
    }

    class CtxAnyList : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            VarDeque itr = new VarDeque(
                    obj.islist ? Kernel.NewROScalar(obj.Fetch()) : obj);
            P6any l = new P6opaque(Kernel.ListMO);
            Kernel.IterToList(l, itr);
            return Kernel.NewRWListVar(l);
        }
    }

    class IxParcelLISTSTORE : IndexHandler {
        public override Variable Get(Variable lhs, Variable rhs) {
            VarDeque src = Builtins.start_iter(rhs);
            P6any lhs_o = lhs.Fetch();
            if (!lhs_o.IsDefined())
                throw new NieczaException("assigning to undefined parcel");

            Variable[] dsts = Kernel.UnboxAny<Variable[]>(lhs_o);
            P6any[] srcs = new P6any[dsts.Length];

            for (int i = 0; i < dsts.Length; i++) {
                Variable d = dsts[i];
                if (d.islist) {
                    srcs[i] = new P6opaque(Kernel.ListMO);
                    Kernel.IterToList(srcs[i], src);
                    src = new VarDeque();
                } else {
                    srcs[i] = Kernel.IterHasFlat(src, true) ?
                        src.Shift().Fetch() : Kernel.AnyP;
                }
            }

            for (int i = 0; i < dsts.Length; i++) {
                Variable d = dsts[i];
                if (d.islist) {
                    d.Fetch().mo.mro_LISTSTORE.Get(d,
                            Kernel.NewRWListVar(srcs[i]));
                } else {
                    d.Store(srcs[i]);
                }
            }

            return lhs;
        }
    }
    class CtxParcelList : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            P6any o = obj.Fetch();
            VarDeque itr = o.IsDefined() ? new VarDeque(Kernel.UnboxAny<Variable[]>(o)) : new VarDeque();
            P6any l = new P6opaque(Kernel.ListMO);
            Kernel.IterToList(l, itr);
            return Kernel.NewRWListVar(l);
        }
    }

    class CtxBoxify<T> : ContextHandler<Variable> {
        ContextHandler<T> inner;
        STable box;
        public CtxBoxify(ContextHandler<T> inner, STable box) {
            this.inner = inner;
            this.box = box;
        }
        public override Variable Get(Variable obj) {
            return Kernel.BoxAnyMO<T>(inner.Get(obj), box);
        }
    }

    class CtxBoxifyInty : ContextHandler<Variable> {
        ContextHandler<double> inner;
        public CtxBoxifyInty(ContextHandler<double> inner) {
            this.inner = inner;
        }
        public override Variable Get(Variable obj) {
            return Builtins.MakeInt((long)inner.Get(obj));
        }
    }

    class CtxContainerize : ContextHandler<Variable> {
        ContextHandler<P6any> inner;
        public CtxContainerize(ContextHandler<P6any> inner) {
            this.inner = inner;
        }
        public override Variable Get(Variable obj) {
            return Kernel.NewROScalar(inner.Get(obj));
        }
    }

    class CtxParcelIterator : ContextHandler<VarDeque> {
        public override VarDeque Get(Variable obj) {
            P6any o = obj.Fetch();
            return o.IsDefined() ? new VarDeque(Kernel.UnboxAny<Variable[]>(o)) : new VarDeque();
        }
    }

    class IxArrayLISTSTORE : IndexHandler {
        public override Variable Get(Variable lhs, Variable rhs) {
            P6any lhs_o = lhs.Fetch();
            if (!lhs_o.IsDefined())
                throw new NieczaException("LISTSTORE to undefined Array");
            VarDeque iter = Builtins.start_iter(rhs);
            VarDeque items = new VarDeque();
            while (Kernel.IterHasFlat(iter, true))
                items.Push(Kernel.NewMuScalar(iter.Shift().Fetch()));
            lhs_o.SetSlot("items", items);
            lhs_o.SetSlot("rest", iter); /*now empty*/
            return lhs;
        }
    }
    class CtxListIterator : ContextHandler<VarDeque> {
        public override VarDeque Get(Variable obj) {
            P6opaque d = (P6opaque) obj.Fetch();
            if (!d.IsDefined()) return new VarDeque();
            VarDeque r = new VarDeque( (VarDeque) d.slots[0] );
            r.PushD((VarDeque) d.slots[1]);
            return r;
        }
    }

    class PopList : ContextHandler<Variable> {
        public override Variable Get(Variable v) {
            P6any o = v.Fetch();
            if (!o.IsDefined()) return Kernel.AnyMO.typeVar;
            VarDeque items = (VarDeque)o.GetSlot("items");
            VarDeque rest = (VarDeque)o.GetSlot("rest");
            while (Kernel.IterHasFlat(rest, false))
                items.Push(rest.Shift());
            return (items.Count() != 0) ? items.Pop() : Kernel.AnyMO.typeVar;
        }
    }
    class ShiftList : ContextHandler<Variable> {
        public override Variable Get(Variable v) {
            P6any o = v.Fetch();
            if (!o.IsDefined()) return Kernel.AnyMO.typeVar;
            VarDeque items = (VarDeque)o.GetSlot("items");
            VarDeque rest = (VarDeque)o.GetSlot("rest");
            if (items.Count() != 0)
                return items.Shift();
            if (Kernel.IterHasFlat(rest, false))
                return rest.Shift();
            return Kernel.AnyMO.typeVar;
        }
    }
    class UnshiftList : PushyHandler {
        public override Variable Invoke(Variable v, Variable[] args) {
            P6any o = v.Fetch();
            if (!o.IsDefined())
                throw new NieczaException("Cannot push onto type object");
            VarDeque iter = new VarDeque(args);
            VarDeque targ = (VarDeque)o.GetSlot("items");
            VarDeque st = new VarDeque();
            while (Kernel.IterHasFlat(iter, true))
                st.Push(Kernel.NewMuScalar(iter.Shift().Fetch()));
            targ.UnshiftD(st);
            return v;
        }
    }
    class PushList : PushyHandler {
        public override Variable Invoke(Variable v, Variable[] args) {
            P6any o = v.Fetch();
            if (!o.IsDefined())
                throw new NieczaException("Cannot push onto type object");
            VarDeque iter = new VarDeque(args);
            VarDeque targ = (VarDeque)o.GetSlot("rest");
            if (targ.Count() == 0) targ = (VarDeque)o.GetSlot("items");
            while (Kernel.IterHasFlat(iter, true))
                targ.Push(Kernel.NewMuScalar(iter.Shift().Fetch()));
            return v;
        }
    }

    class IxHashLISTSTORE : IndexHandler {
        public override Variable Get(Variable lhs, Variable rhs) {
            P6any lhs_o = lhs.Fetch();
            if (!lhs_o.IsDefined())
                throw new NieczaException("LISTSTORE to undefined Hash");
            VarHash into = new VarHash();
            VarDeque iter = Builtins.start_iter(rhs);
            bool first = true;
            while (Kernel.IterHasFlat(iter, true)) {
                P6any elt = iter.Shift().Fetch();
                if (first && elt.mo.HasMRO(Kernel.HashMO)) {
                    foreach(KeyValuePair<string,Variable> kv in
                            Kernel.UnboxAny<VarHash>(elt)) {
                        into[kv.Key] = kv.Value;
                    }
                } else if (elt.mo.HasMRO(Kernel.PairMO)) {
                    Variable k = (Variable) elt.GetSlot("key");
                    Variable v = (Variable) elt.GetSlot("value");
                    into[k.Fetch().mo.mro_raw_Str.Get(k)] =
                        Kernel.NewMuScalar(v.Fetch());
                } else {
                    if (!Kernel.IterHasFlat(iter, true))
                        throw new NieczaException("Unmatched key in Hash.LISTSTORE");
                    into[elt.mo.mro_raw_Str.Get(Kernel.NewROScalar(elt))] =
                        Kernel.NewMuScalar(iter.Shift().Fetch());
                }
                first = false;
            }
            Kernel.SetBox<VarHash>(lhs_o, into);
            return lhs;
        }
    }
    class CtxHashIterator : ContextHandler<VarDeque> {
        public override VarDeque Get(Variable obj) {
            return Builtins.HashIterRaw(3, obj);
        }
    }
    class CtxHashBool : ContextHandler<bool> {
        public override bool Get(Variable obj) {
            P6any o = obj.Fetch();
            return o.IsDefined() && Kernel.UnboxAny<VarHash>(o).IsNonEmpty;
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

    class CtxNumSuccish : ContextHandler<P6any> {
        double amt;
        public CtxNumSuccish(double amt) { this.amt = amt; }
        public override P6any Get(Variable obj) {
            P6any o = obj.Fetch();
            double v = o.IsDefined() ? Kernel.UnboxAny<double>(o) : 0;
            return Kernel.BoxRaw(v + amt, Kernel.NumMO);
        }
    }
    class CtxRawNativeNum2Str : ContextHandler<string> {
        public override string Get(Variable obj) {
            P6any o = obj.Fetch();
            return o.IsDefined() ? Utils.N2S(Kernel.UnboxAny<double>(o)) : "Num()";
        }
    }
    class CtxNum2Bool : ContextHandler<bool> {
        public override bool Get(Variable obj) {
            P6any o = obj.Fetch();
            return o.IsDefined() && Kernel.UnboxAny<double>(o) != 0;
        }
    }

    class CtxIntSuccish : ContextHandler<P6any> {
        int amt;
        public CtxIntSuccish(int amt) { this.amt = amt; }
        public override P6any Get(Variable obj) {
            P6any o = obj.Fetch();
            int v;
            if (o is BoxObject<BigInteger>) {
                BigInteger bn = Kernel.UnboxAny<BigInteger>(o) + amt;
                if (bn.AsInt32(out v))
                    return Kernel.BoxRaw<int>(v, Kernel.IntMO);
                else
                    return Kernel.BoxRaw<BigInteger>(bn, Kernel.IntMO);
            }
            v = o.IsDefined() ? Kernel.UnboxAny<int>(o) : 0;
            if (v == (amt > 0 ? int.MaxValue : int.MinValue))
                return Kernel.BoxRaw<BigInteger>(amt + (long)v, Kernel.IntMO);
            return Kernel.BoxRaw(v + amt, Kernel.IntMO);
        }
    }
    class CtxIntStr : ContextHandler<string> {
        public override string Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) { return o.mo.name + "()"; }
            else if (o is BoxObject<int>) { return Utils.N2S(Kernel.UnboxAny<int>(o)); }
            else { return Kernel.UnboxAny<BigInteger>(o).ToString(); }
        }
    }
    class CtxIntBool : ContextHandler<bool> {
        public override bool Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) { return false; }
            else if (o is BoxObject<int>) { return Kernel.UnboxAny<int>(o)!=0; }
            else { return Kernel.UnboxAny<BigInteger>(o) != BigInteger.Zero; }
        }
    }

    class CtxRatSuccish : ContextHandler<P6any> {
        bool up;
        public CtxRatSuccish(bool up) { this.up = up; }
        public override P6any Get(Variable obj) {
            P6any o = obj.Fetch();
            Rat rr;
            if (o.IsDefined()) {
                Rat ir = Kernel.UnboxAny<Rat>(o);
                rr = new Rat(up ? ir.num + ir.den : ir.num - ir.den, ir.den);
            } else {
                rr = new Rat(up ? BigInteger.One : BigInteger.MinusOne, 1);
            }
            return Kernel.BoxRaw<Rat>(rr, Kernel.RatMO);
        }
    }
    class CtxRatStr : ContextHandler<string> {
        public override string Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) { return o.mo.name + "()"; }
            Rat r = Kernel.UnboxAny<Rat>(o);
            return r.num.ToString() + "/" + r.den.ToString();
        }
    }
    class CtxRatBool : ContextHandler<bool> {
        public override bool Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) { return false; }
            Rat r = Kernel.UnboxAny<Rat>(o);
            return r.num != BigInteger.Zero;
        }
    }

    class CtxFatRatSuccish : ContextHandler<P6any> {
        bool up;
        public CtxFatRatSuccish(bool up) { this.up = up; }
        public override P6any Get(Variable obj) {
            P6any o = obj.Fetch();
            FatRat rr;
            if (o.IsDefined()) {
                FatRat ir = Kernel.UnboxAny<FatRat>(o);
                rr = new FatRat(up ? ir.num + ir.den : ir.num - ir.den, ir.den);
            } else {
                rr = new FatRat(up ? BigInteger.One : BigInteger.MinusOne, BigInteger.One);
            }
            return Kernel.BoxRaw<FatRat>(rr, Kernel.FatRatMO);
        }
    }
    class CtxFatRatStr : ContextHandler<string> {
        public override string Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) { return o.mo.name + "()"; }
            FatRat r = Kernel.UnboxAny<FatRat>(o);
            return r.num.ToString() + "/" + r.den.ToString();
        }
    }
    class CtxFatRatBool : ContextHandler<bool> {
        public override bool Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) { return false; }
            FatRat r = Kernel.UnboxAny<FatRat>(o);
            return r.num != BigInteger.Zero;
        }
    }

    class CtxComplexSuccish : ContextHandler<P6any> {
        double amt;
        public CtxComplexSuccish(double amt) { this.amt = amt; }
        public override P6any Get(Variable obj) {
            P6any o = obj.Fetch();
            Complex c = o.IsDefined() ? Kernel.UnboxAny<Complex>(o) : null;
            c = (c == null) ? new Complex(amt, 0) : new Complex(c.re+amt, c.im);
            return Kernel.BoxRaw(c, Kernel.ComplexMO);
        }
    }
    class CtxComplexStr : ContextHandler<string> {
        public override string Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) { return o.mo.name + "()"; }
            Complex r = Kernel.UnboxAny<Complex>(o);
            return Utils.N2S(r.re) + (r.im < 0 ? "" : "+") + Utils.N2S(r.im) + "i";
        }
    }
    class CtxComplexBool : ContextHandler<bool> {
        public override bool Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) { return false; }
            Complex r = Kernel.UnboxAny<Complex>(o);
            return r.re != 0 || r.im != 0;
        }
    }

    class CtxStrBool : ContextHandler<bool> {
        public override bool Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) return false;
            string s = Kernel.UnboxAny<string>(o);
            return !(s == "" || s == "0");
        }
    }
    class CtxStrSuccish : ContextHandler<P6any> {
        bool succ;
        public CtxStrSuccish(bool succ) { this.succ = succ; }
        // note that most of this table is katakana.  Perhaps there
        // is a better way.
        static ushort[] table = {
            48, 57, 57, 48, 65, 90, 90, 65, 97, 122, 122, 97, 913, 929,
            937, 931, 931, 937, 929, 913, 945, 961, 969, 963, 963, 969,
            961, 945, 8544, 8555, 8555, 8544, 8560, 8571, 8571, 8560,
            9312, 9331, 9331, 9312, 9332, 9351, 9351, 9332, 9372, 9397,
            9397, 9372, 9856, 9861, 9861, 9856, 12450, 12450, 12531,
            12452, 12452, 12452, 12450, 12454, 12454, 12454, 12452,
            12456, 12456, 12456, 12454, 12458, 12458, 12459, 12456,
            12461, 12461, 12461, 12459, 12463, 12463, 12463, 12461,
            12465, 12465, 12465, 12463, 12467, 12467, 12467, 12465,
            12469, 12469, 12469, 12467, 12471, 12471, 12471, 12469,
            12473, 12473, 12473, 12471, 12475, 12475, 12475, 12473,
            12477, 12477, 12477, 12475, 12479, 12479, 12479, 12477,
            12481, 12481, 12481, 12479, 12484, 12484, 12484, 12481,
            12486, 12486, 12486, 12484, 12488, 12488, 12488, 12486,
            12490, 12490, 12495, 12488, 12498, 12498, 12498, 12495,
            12501, 12501, 12501, 12498, 12504, 12504, 12504, 12501,
            12507, 12507, 12507, 12504, 12510, 12510, 12514, 12507,
            12516, 12516, 12516, 12514, 12518, 12518, 12518, 12516,
            12520, 12520, 12525, 12518, 12527, 12527, 12527, 12525,
            12530, 12530, 12531, 12527, 12450
        };
        // would perfect hashing be better?
        void TableGet(char it, out char prev, out char next) {
            int al = 0;
            int ah = table.Length / 4;
            while (true) {
                if (al >= ah) {
                    prev = next = it;
                    return;
                }
                int am = (al + ah) / 2;
                if (it < (char)table[am*4]) {
                    ah = am;
                } else if (it <= (char)table[am*4+1]) {
                    prev = (it == (char)table[am*4]) ? (char)table[am*4+2] : (char)(it-1);
                    next = (it == (char)table[am*4+1]) ? (char)table[am*4+3] : (char)(it+1);
                    return;
                } else {
                    al = am+1;
                }
            }
        }

        bool Digitish(char it) {
            char next, prev;
            TableGet(it, out prev, out next);
            return (next != it);
        }

        public override P6any Get(Variable obj) {
            P6any obj_o = obj.Fetch();
            if (!obj_o.IsDefined()) return Kernel.BoxRaw("WTF", Kernel.StrMO);
            string src = Kernel.UnboxAny<string>(obj_o);
            int right = src.Length;
tryagain:
            while (right != 0 && !Digitish(src[right-1])) right--;
            if (right == 0) return Kernel.BoxRaw("WTF", Kernel.StrMO);
            int left = right;
            while (left != 0 && Digitish(src[left-1])) left--;
            if (left != 0 && src[left-1] == '.') {
                right--;
                goto tryagain;
            }
            char[] nbuf = new char[src.Length + 1];
            for (int i = right; i < src.Length; i++) nbuf[i+1] = src[i];

            int delta = 0;
            if (succ) {
                bool carry = true;
                char zero = '\0';
                while (carry && right != left) {
                    char next, prev;
                    TableGet(src[right-1], out prev, out next);
                    carry = (next < src[right-1]);
                    zero = next;
                    nbuf[right] = next;
                    right--;
                }
                if (carry) {
                    delta++;
                    nbuf[left] = zero == '0' ? '1' : zero;
                }
            } else {
                bool borrow = true;
                while (borrow && right != left) {
                    char next, prev;
                    TableGet(src[right-1], out prev, out next);
                    borrow = (src[right-1] < prev);
                    nbuf[right] = prev;
                    right--;
                }
                if (borrow)
                    throw new NieczaException("Magical string decrement underflowed");
            }
            for (int i = 0; i < right; i++) nbuf[i+1-delta] = src[i];
            return Kernel.BoxRaw(new string(nbuf, 1-delta, src.Length+delta),
                    Kernel.StrMO);
        }
    }

    class CtxListBool : ContextHandler<bool> {
        public override bool Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) return false;
            P6opaque dob = (P6opaque) o;
            VarDeque items = (VarDeque) dob.slots[0];
            if (items.Count() != 0) return true;
            VarDeque rest = (VarDeque) dob.slots[1];
            if (rest.Count() == 0) return false;
            if (Kernel.IterHasFlat(rest, false)) {
                items.Push(rest.Shift());
                return true;
            } else {
                return false;
            }
        }
    }

    class CtxListNum : ContextHandler<double> {
        public override double Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) return 0;
            P6opaque dob = (P6opaque) o;
            VarDeque items = (VarDeque) dob.slots[0];
            VarDeque rest = (VarDeque) dob.slots[1];
            if (rest.Count() == 0) return items.Count();
            while (Kernel.IterHasFlat(rest, false)) {
                items.Push(rest.Shift());
            }
            return items.Count();
        }
    }

    class CtxJunctionBool : ContextHandler<bool> {
        public override bool Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) return false;
            P6opaque o_ = (P6opaque)o;

            int jtype = Kernel.UnboxAny<int>((P6any) o_.slots[0]);
            if (jtype == 4) return true; // XXX

            Variable[] eigen = Kernel.UnboxAny<Variable[]>((P6any) o_.slots[1]);
            int ix = 0;
            Variable v;
            // logic design taken from Rakudo here
            switch(jtype) {
                case 0: //all
                    if (ix == eigen.Length) return true;
                    v = eigen[ix++];
                    if (!v.Fetch().mo.mro_raw_Bool.Get(v)) return false;
                    goto case 0;
                case 1: //none
                    if (ix == eigen.Length) return true;
                    v = eigen[ix++];
                    if (v.Fetch().mo.mro_raw_Bool.Get(v)) return false;
                    goto case 1;
                case 2: //one, searching for first
                    if (ix == eigen.Length) return false;
                    v = eigen[ix++];
                    if (v.Fetch().mo.mro_raw_Bool.Get(v)) goto case 1;
                    goto case 2;
                case 3: //any
                    if (ix == eigen.Length) return false;
                    v = eigen[ix++];
                    if (v.Fetch().mo.mro_raw_Bool.Get(v)) return true;
                    goto case 3;
                default: throw new ArgumentException();
            }
        }
    }

    class CtxMatchStr : ContextHandler<string> {
        public override string Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) return "";
            Cursor c = (Cursor) o;
            return c.GetBacking().Substring(c.from, c.pos - c.from);
        }
    }

    class CtxStrNativeNum2Str : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            P6any o = obj.Fetch();
            return Kernel.BoxAnyMO<string>(o.IsDefined() ? Utils.N2S(Kernel.UnboxAny<double>(o)) : "Num()", Kernel.StrMO);
        }
    }

    class IxCallMethod : IndexHandler {
        string name;
        VarHash named;
        public IxCallMethod(string name, string adv) {
            this.name = name;
            if (adv != null) {
                named = new VarHash();
                named[adv] = Kernel.TrueV;
            }
        }
        public override Variable Get(Variable obj, Variable key) {
            return (Variable) Kernel.RunInferior(
                    obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), name,
                        new Variable[] { obj, key }, named));
        }
    }

    class KeySlicer : IndexHandler {
        int mode; IndexHandler bas;
        public KeySlicer(int mode, IndexHandler bas) {
            this.mode = mode; this.bas = bas;
        }

        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.islist || !ks.mo.is_any && ks.mo.HasMRO(Kernel.JunctionMO))
                return Slice(obj, key);

            switch (mode) {
                case 0:  return key;
                case 1:  return Builtins.MakeParcel(key, bas.Get(obj, key));
                default: return Builtins.pair(key, bas.Get(obj, key));
            }
        }
    }

    class IxAnyDeleteKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.islist || !ks.mo.is_any && ks.mo.HasMRO(Kernel.JunctionMO))
                return Slice(obj, key);

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return Kernel.AnyMO.typeVar;
            return Kernel.RunInferior(os.InvokeMethod(Kernel.GetInferiorRoot(),
                "delete_key", new Variable[] { obj, key }, null));
        }
    }
    class IxAnyExistsKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.islist || !ks.mo.is_any && ks.mo.HasMRO(Kernel.JunctionMO))
                return Slice(obj, key);

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return Kernel.FalseV;
            return Kernel.RunInferior(os.InvokeMethod(Kernel.GetInferiorRoot(),
                "exists_key", new Variable[] { obj, key }, null));
        }
    }
    class IxAnyBindKey : BindHandler {
        public override Variable Bind(Variable obj, Variable key, Variable to) {
            P6any os = obj.Fetch();
            if (os.IsDefined())
                return Kernel.RunInferior(os.InvokeMethod(Kernel.GetInferiorRoot(),
                    "bind_key", new Variable[] { obj, key, to }, null));
            obj.Store(Kernel.BoxRaw(new VarHash(), Kernel.HashMO));
            return Kernel.HashMO.mro_bind_key.Bind(obj, key, to);
        }
    }
    class IxAnyBindPos : BindHandler {
        public override Variable Bind(Variable obj, Variable key, Variable to) {
            P6any os = obj.Fetch();
            if (os.IsDefined())
                return Kernel.RunInferior(os.InvokeMethod(Kernel.GetInferiorRoot(),
                    "bind_pos", new Variable[] { obj, key, to }, null));
            obj.Store(Kernel.CreateArray().Fetch());
            return Kernel.ArrayMO.mro_bind_key.Bind(obj, key, to);
        }
    }
    class IxAnyAtKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.islist || !ks.mo.is_any && ks.mo.HasMRO(Kernel.JunctionMO))
                return Slice(obj, key);

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return IndexHandler.ViviHash(obj, key);
            return Kernel.RunInferior(os.InvokeMethod(Kernel.GetInferiorRoot(),
                "at_key", new Variable[] { obj, key }, null));
        }
    }
    class IxAnyAtPos : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.islist || !ks.mo.is_any && ks.mo.HasMRO(Kernel.JunctionMO))
                return Slice(obj, key);

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return IndexHandler.ViviArray(obj, key);
            if (ks.mo != Kernel.IntMO && ks.mo.HasMRO(Kernel.CodeMO)) {
                Variable elts = Kernel.RunInferior(os.InvokeMethod(
                        Kernel.GetInferiorRoot(), "elems",
                        new Variable[] { obj }, null));
                return Get(obj, Kernel.RunInferior(ks.Invoke(
                    Kernel.GetInferiorRoot(),
                    new Variable[] { elts }, null)));
            }

            return Kernel.RunInferior(os.InvokeMethod(Kernel.GetInferiorRoot(),
                "at_pos", new Variable[] { obj, key }, null));
        }
    }

    class IxCursorAtKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.islist || !ks.mo.is_any && ks.mo.HasMRO(Kernel.JunctionMO))
                return Slice(obj, key);
            P6any o = obj.Fetch();
            if (!o.IsDefined())
                return Kernel.AnyMO.typeVar;

            Cursor os = (Cursor)o;
            return os.GetKey(ks.mo.mro_raw_Str.Get(key));
        }
    }
    class IxCursorAtPos : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.islist || !ks.mo.is_any && ks.mo.HasMRO(Kernel.JunctionMO))
                return Slice(obj, key);

            P6any o = obj.Fetch();
            if (!o.IsDefined())
                return Kernel.AnyMO.typeVar;

            Cursor os = (Cursor)o;
            return os.GetKey(Utils.N2S(ks.mo.mro_raw_Numeric.Get(key)));
        }
    }

    class IxHashBindKey : BindHandler {
        public override Variable Bind(Variable obj, Variable key, Variable to) {
            P6any ks = key.Fetch();
            P6any os = obj.Fetch();
            if (!os.IsDefined())
                obj.Store(os = Kernel.BoxRaw(new VarHash(), Kernel.HashMO));
            string kss = ks.mo.mro_raw_Str.Get(key);
            VarHash h = Kernel.UnboxAny<VarHash>(os);

            return h[kss] = Kernel.NewBoundVar(Kernel.NBV_RW, Kernel.MuMO, to);
        }
    }
    class IxHashAtKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.islist || !ks.mo.is_any && ks.mo.HasMRO(Kernel.JunctionMO))
                return Slice(obj, key);

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return IndexHandler.ViviHash(obj, key);
            string kss = ks.mo.mro_raw_Str.Get(key);
            VarHash h = Kernel.UnboxAny<VarHash>(os);
            Variable r;
            if (h.TryGetValue(kss, out r))
                return r;
            return new SimpleVariable(true, false, Kernel.MuMO,
                    new HashViviHook(os, kss), Kernel.AnyP);
        }
    }
    class IxHashExistsKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.islist || !ks.mo.is_any && ks.mo.HasMRO(Kernel.JunctionMO))
                return Slice(obj, key);
            P6any os = obj.Fetch();
            if (!os.IsDefined()) return Kernel.FalseV;
            string kss = ks.mo.mro_raw_Str.Get(key);
            VarHash h = Kernel.UnboxAny<VarHash>(os);
            return h.ContainsKey(kss) ? Kernel.TrueV : Kernel.FalseV;
        }
    }
    class IxHashDeleteKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.islist || !ks.mo.is_any && ks.mo.HasMRO(Kernel.JunctionMO))
                return Slice(obj, key);
            P6any os = obj.Fetch();
            if (!os.IsDefined()) return Kernel.AnyMO.typeVar;
            string kss = ks.mo.mro_raw_Str.Get(key);
            VarHash h = Kernel.UnboxAny<VarHash>(os);
            Variable r;
            if (h.TryGetValue(kss, out r)) {
                h.Remove(kss);
                return r;
            } else {
                return Kernel.AnyMO.typeVar;
            }
        }
    }

    class IxListAtPos : IndexHandler {
        bool extend;
        public IxListAtPos(bool extend) { this.extend = extend; }

        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.islist || !ks.mo.is_any && ks.mo.HasMRO(Kernel.JunctionMO))
                return Slice(obj, key);

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return IndexHandler.ViviArray(obj, key);

            P6opaque dos = (P6opaque) os;
            VarDeque items = (VarDeque) dos.slots[0];
            VarDeque rest  = (VarDeque) dos.slots[1];

            if (ks.mo != Kernel.IntMO && ks.mo.HasMRO(Kernel.CodeMO)) {
                Variable nr = os.mo.mro_Numeric.Get(obj);
                return Get(obj, Kernel.RunInferior(ks.Invoke(
                    Kernel.GetInferiorRoot(),
                    new Variable[] { nr }, null)));
            }

            int ix = (int) key.Fetch().mo.mro_raw_Numeric.Get(key);
            while (items.Count() <= ix && Kernel.IterHasFlat(rest, false)) {
                items.Push(rest.Shift());
            }
            if (ix < 0)
                return Kernel.AnyMO.typeVar;
            if (items.Count() <= ix) {
                if (extend) {
                    return new SimpleVariable(true, false, Kernel.MuMO,
                            new ArrayViviHook(os, ix), Kernel.AnyP);
                } else {
                    return Kernel.AnyMO.typeVar;
                }
            }
            return items[ix];
        }
    }

    class IxListBindPos : BindHandler {
        public override Variable Bind(Variable obj, Variable key, Variable to) {
            P6any ks = key.Fetch();
            P6any os = obj.Fetch();
            if (!os.IsDefined())
                obj.Store(os = Kernel.CreateArray().Fetch());

            P6opaque dos = (P6opaque) os;
            VarDeque items = (VarDeque) dos.slots[0];
            VarDeque rest  = (VarDeque) dos.slots[1];

            if (ks.mo != Kernel.IntMO && ks.mo.HasMRO(Kernel.CodeMO)) {
                Variable nr = os.mo.mro_Numeric.Get(obj);
                key = Kernel.RunInferior(ks.Invoke(Kernel.GetInferiorRoot(),
                    new Variable[] { nr }, null));
            }

            int ix = (int) key.Fetch().mo.mro_raw_Numeric.Get(key);
            while (items.Count() <= ix && Kernel.IterHasFlat(rest, false)) {
                items.Push(rest.Shift());
            }
            if (ix < 0)
                throw new NieczaException("binding to out of range slot " + ix);
            while (items.Count() <= ix) {
                items.Push(Kernel.NewTypedScalar(null));
            }
            return items[ix] = Kernel.NewBoundVar(Kernel.NBV_RW, Kernel.MuMO, to);
        }
    }

    public struct StashCursor {
        public const int WHO  = 0; // use p1(P6any)
        public const int LEX  = 1; // p1(Frame) p2(int, depth)
        public const int ROOT = 2; // p1(Frame) p2(int)
        public const int DYNA = 3; // p1&p2

        int type;
        object p1;
        int p2;

        public StashCursor(Frame fr, int depth) {
            this.type = ROOT;
            this.p1 = fr;
            this.p2 = depth;
        }

        public static Variable MakePackage(string name, P6any who) {
            STable st = new STable(name);
            st.who = who;
            st.typeObject = st.initObject = new P6opaque(st, 0);
            ((P6opaque)st.typeObject).slots = null;
            st.typeVar = st.initVar = Kernel.NewROScalar(st.typeObject);
            st.mo.isPackage = true;
            // XXX should be PackageHOW
            st.how = new BoxObject<STable>(st, Kernel.AnyMO.how.mo, 0);
            st.mo.Revalidate();
            return st.typeVar;
        }

        bool HasCaller() {
            Frame f = (Frame)p1;
            if (p2 == 0) {
                f = f.caller;
                if (f != null && f.info == Kernel.ExitRunloopSI) f = f.caller;
                if (f == null) return false;
            }
            return true;
        }

        StashCursor ToCaller() {
            StashCursor r = this;
            Frame f = (Frame)p1;
            if (r.p2 > 0) r.p2--;
            else {
                f = f.caller;
                if (f != null && f.info == Kernel.ExitRunloopSI) f = f.caller;
                if (f == null)
                    throw new NieczaException("No more calling frames");
                r.p1 = f;
                r.p2 = f.info.FindControlEnt(f.ip, SubInfo.ON_VARLOOKUP, null);
                if (r.p2 < 0) r.p2 = 0;
            }
            return r;
        }

        SubInfo ToInfo() { return (p1 as Frame).info; }

        StashCursor ToOuter() {
            StashCursor r = this;
            if (r.p2 > 0) { r.p2--; }
            else {
                Frame f = ((Frame) r.p1).outer;
                r.p1 = f;
                if (f == null)
                    throw new NieczaException("No more outer frames");
                r.p2 = 0;
            }
            return r;
        }

        bool TryLexOut(string key, bool rbar_w, ref Variable o) {
            StashCursor sc = this;
            if (key.Length >= 2 && key[1] == '*') {
                return TryDynamic(key, rbar_w, ref o);
            }
            while (true) {
                if (sc.TryLex(key, rbar_w, ref o)) return true;
                if ((sc.p1 as Frame).outer == null && sc.p2 == 0)
                    return false;
                sc = sc.ToOuter();
            }
        }

        bool TryDynamic(string key, bool rbar_w, ref Variable o) {
            StashCursor sc = this;
            while (true) {
                if (sc.TryLex(key, rbar_w, ref o)) {
                    return true;
                }
                if (!sc.HasCaller())
                    break;
                sc = sc.ToCaller();
            }
            if (key.Length >= 2 && key[1] == '*') {
                key = key.Remove(1,1);
                BValue bv;

                if (Kernel.UnboxAny<Dictionary<string,BValue>>(Kernel.GlobalO).
                        TryGetValue(key, out bv) ||
                    Kernel.UnboxAny<Dictionary<string,BValue>>(Kernel.ProcessO).
                        TryGetValue(key, out bv)) {

                    if (rbar_w) { bv.v = o; } else { o = bv.v; }
                    return true;
                }
            }
            return false;
        }

        bool TryLex(string key, bool rbar_w, ref Variable o) {
            Frame f = (Frame)p1;
            if (p2 > 0) {
                int ix = f.info.GetInlineSlot(f.ip, key, p2);
                if (ix < 0) return false;

                if (rbar_w) f.SetDynamic(ix, o);
                else o = (Variable)f.GetDynamic(ix);
            }
            else {
                LexInfo li;
                if (!f.info.dylex.TryGetValue(key, out li)) return false;

                if (rbar_w) li.Set(f, o);
                else o = (Variable)li.Get(f);
            }
            return true;
        }

        void Core(string key, bool final, out StashCursor sc, out Variable v,
                Variable bind_to) {
            v = null;
            sc = this;
            if (type == DYNA) {
                // DYNAMIC::{key}, no special names used
                v = bind_to;
                if (TryDynamic(key, (bind_to != null), ref v)) {
                    if (bind_to != null) return;
                    goto have_v;
                }
                if (bind_to != null)
                    throw new NieczaException("No slot to bind");
                v = Kernel.AnyMO.typeVar;
                goto have_v;
            }
            else if (type == WHO) {
                // only special type is PARENT, maybe not even that?
                P6any who = (P6any) p1;
                Variable whov = Kernel.NewROScalar(who);
                Variable keyv = Kernel.BoxAnyMO(key, Kernel.StrMO);
                if (bind_to != null) {
                    v = who.mo.mro_bind_key.Bind(whov, keyv, bind_to);
                    return;
                }
                v = who.mo.mro_at_key.Get(whov, keyv);

                if (final) return;

                if (v.rw && !v.Fetch().IsDefined()) {
                    Variable vname = Kernel.RunInferior(who.InvokeMethod(
                        Kernel.GetInferiorRoot(), "name",
                        new Variable[] { whov }, null));
                    string name = vname.Fetch().mo.mro_raw_Str.Get(vname);
                    P6any new_who = Kernel.GetStash(name + "::" + key);
                    who.mo.mro_bind_key.Bind(whov, keyv,
                        MakePackage(key, new_who));
                    sc.p1 = new_who;
                    return;
                }
                else if (v.rw || v.Fetch().IsDefined()) {
                    throw new NieczaException(key + " does not point to a package");
                }
                else {
                    sc.p1 = v.Fetch().mo.who;
                    return;
                }
            }
            else if (type == ROOT) {
                // semantic root, handles most of the special names
                if (key == "OUR") {
                    throw new NieczaException("OUR NYI");
                } else if (key == "GLOBAL") {
                    sc.p1 = Kernel.GlobalO;
                    sc.type = WHO;
                    goto have_sc;
                } else if (key == "PROCESS") {
                    sc.p1 = Kernel.ProcessO;
                    sc.type = WHO;
                    goto have_sc;
                } else if (key == "UNIT" || key == "OUTER" ||
                        key == "SETTING" || key == "CALLER") {
                    StashCursor n = sc;
                    n.type = LEX;
                    n.Core(key, final, out sc, out v, bind_to);
                    return;
                } else if (key == "CORE") {
                    sc.type = LEX;
                    while (sc.ToInfo().unit == null ||
                            sc.ToInfo().unit.name != "CORE") sc = sc.ToOuter();
                    goto have_sc;
                } else if (key == "MY") {
                    sc.type = LEX;
                    goto have_sc;
                } else if (key == "COMPILING") {
                    throw new NieczaException("Cannot use COMPILING outside BEGIN scope");
                } else if (key == "DYNAMIC") {
                    sc.type = DYNA;
                    goto have_sc;
                } else {
                    v = bind_to;
                    if (TryLexOut(key, bind_to != null, ref v)) {
                        if (bind_to != null) return;
                        goto have_v;
                    }
                    StashCursor n = default(StashCursor);
                    n.type = WHO;
                    n.p1 = (key == "PARENT" || key.Length > 0 &&
                            "$&@%".IndexOf(key[0]) >= 0)
                        ? null // OUR NYI
                        : Kernel.GlobalO;
                    n.Core(key, final, out sc, out v, bind_to);
                    return;
                }
            }
            else if (type == LEX) {
                if (key == "OUTER") {
                    sc = sc.ToOuter();
                    goto have_sc;
                }
                else if (key == "UNIT") {
                    while (sc.p2 > 0 || (sc.ToInfo().special &
                                RuntimeUnit.SUB_MAINLINE) == 0)
                        sc = sc.ToOuter();
                    goto have_sc;
                }
                else if (key == "CALLER") {
                    sc = sc.ToCaller();
                    goto have_sc;
                }
                else if (key == "SETTING") {
                    while (sc.p2 > 0 || (sc.ToInfo().special &
                                RuntimeUnit.SUB_MAINLINE) == 0)
                        sc = sc.ToOuter();
                    sc = sc.ToOuter();
                    goto have_sc;
                }
                else {
                    v = bind_to;
                    if (TryLexOut(key, bind_to != null, ref v)) {
                        if (bind_to != null) return;
                        goto have_v;
                    }
                    if (bind_to != null)
                        throw new NieczaException("No slot to bind");
                    v = Kernel.AnyMO.typeVar;
                    goto have_v;
                }
            }
            else {
                throw new NieczaException("corrupt StashCursor");
            }

have_sc:
            if (!final) return;
            if (bind_to != null)
                throw new NieczaException("cannot bind a psuedo package");
            {
                P6any who = Kernel.BoxRaw(this, Kernel.PseudoStashMO);
                who.SetSlot("name", Kernel.BoxAnyMO(key, Kernel.StrMO));
                v = MakePackage(key, who);
            }
            return;

have_v:
            if (final) return;
            if (v.rw || v.Fetch().IsDefined())
                throw new NieczaException(key + " is not a stash");
            sc.type = WHO;
            sc.p1 = v.Fetch().mo.who;
            return;
        }

        internal Variable Raw(string key, Variable bind_to) {
            Variable r;
            StashCursor sc;
            Core(key, true, out sc, out r, bind_to);
            return r;
        }

        public Variable Indirect(string key, bool bind_ro, Variable bind_to) {
            StashCursor sc = this;
            StashCursor r;
            Variable v;
            int ix1 = 0;
            string sigil = "";
            string last = "ROOT";
            while (true) {
                int ix2 = key.IndexOf("::", ix1);
                if (ix2 < 0) {
                    key = key.Substring(ix1);
                    break;
                }
                string elt = key.Substring(ix1, ix2 - ix1);
                while (elt.Length > 0 && "$&@%?=.!*~".IndexOf(elt[0]) >= 0) {
                    sigil = sigil + elt.Substring(0,1);
                    elt = elt.Substring(1);
                }
                ix1 = ix2+2;

                if (elt != "") {
                    sc.Core(elt, false, out r, out v, null);
                    last = elt;
                    sc = r;
                }
            }
            key = sigil + key;
            if (key == "") {
                if (bind_to != null)
                    throw new NieczaException("Cannot bind to a stash");
                if (sc.type == WHO)
                    return Kernel.NewROScalar((P6any) sc.p1);
                P6any who = Kernel.BoxRaw(sc, Kernel.PseudoStashMO);
                who.SetSlot("name", Kernel.BoxAnyMO(last, Kernel.StrMO));
                return Kernel.NewROScalar(who);
            }
            if (bind_to != null) {
                bool list = key != "" && (key[0] == '@' || key[0] == '%');
                bind_to = Kernel.NewBoundVar(list ? Kernel.NBV_LIST :
                    bind_ro ? Kernel.NBV_RO : Kernel.NBV_RW, Kernel.MuMO,
                    bind_to); // XXX should use types maybe?
            }
            sc.Core(key, true, out r, out v, bind_to);
            return v;
        }
    }

    // A bunch of stuff which raises big circularity issues if done in the
    // setting itself.
    public class Kernel {
        private static VarDeque[] PhaserBanks;

        public static void AddPhaser(int i, P6any v) {
            PhaserBanks[i].Push(NewROScalar(v));
        }

        public static void FirePhasers(int i, bool lifo) {
            while (PhaserBanks[i].Count() != 0)
                RunInferior((lifo ? PhaserBanks[i].Pop() :
                            PhaserBanks[i].Shift()).Fetch().Invoke(
                            GetInferiorRoot(), Variable.None, null));
        }

        internal static HashSet<string> ModulesStarted = new HashSet<string>();
        internal static Dictionary<string,RuntimeUnit> ModulesFinished =
            new Dictionary<string,RuntimeUnit>();

        public static Variable BootModule(string name, DynBlockDelegate dgt) {
            if (ModulesFinished.ContainsKey(name))
                return AnyMO.typeVar;
            if (ModulesStarted.Contains(name))
                throw new NieczaException("Recursive module graph detected at " + name + ": " + JoinS(" ", ModulesStarted));
            ModulesStarted.Add(name);
            Variable r = Kernel.RunInferior(Kernel.GetInferiorRoot().
                    MakeChild(null, new SubInfo("boot-" + name, dgt), AnyP));
            if (!ModulesFinished.ContainsKey(name))
                ModulesFinished[name] = null;
            return r;
        }

        public static void DoRequire(string name) {
            if (ModulesFinished.ContainsKey(name))
                return;
            Assembly a = Assembly.Load(name);
            Type t = a.GetType(name);
            if (t == null) throw new NieczaException("Load module must have a type of the same name");
            MethodInfo mi = t.GetMethod("BOOT");
            if (mi == null) throw new NieczaException("Load module must have a BOOT method");
            BootModule(name, delegate (Frame fr) {
                return (Frame) mi.Invoke(null, new object[] { fr });
            });
        }

        public static T UnboxAny<T>(P6any o) {
            return ((BoxObject<T>)o).value;
        }

        public static Frame Take(Frame th, Variable payload) {
            Frame c = th;
            while (c != null && (c.lex == null || !c.lex.ContainsKey("!return")))
                c = c.caller;
            if (c == null)
                return Kernel.Die(th, "used take outside of a coroutine");

            Frame r = (Frame) c.lex["!return"];
            c.lex["!return"] = null;
            r.LexicalBind("$*nextframe", NewROScalar(th));
            r.resultSlot = payload;
            th.resultSlot = payload;
            return r;
        }

        public static Frame CoTake(Frame th, Frame from) {
            Frame c = from;
            while (c != null && (c.lex == null || !c.lex.ContainsKey("!return")))
                c = c.caller;
            if (c.lex["!return"] != null)
                return Kernel.Die(th, "Attempted to re-enter abnormally exitted or running coroutine");
            c.lex["!return"] = th;

            return from;
        }

        public static Frame GatherHelper(Frame th, P6any sub) {
            P6opaque dyo = (P6opaque) sub;
            Frame n = (dyo.slots[1] as SubInfo).Binder(th,
                    (dyo.slots[0] as Frame), dyo, Variable.None, null, false,
                    null);
            n.MarkSharedChain();
            n.lex = new Dictionary<string,object>();
            n.lex["!return"] = null;
            th.resultSlot = n;
            return th;
        }

        private static SubInfo SubInvokeSubSI = new SubInfo("Sub.postcircumfix:<( )>", SubInvokeSubC);
        private static Frame SubInvokeSubC(Frame th) {
            Variable[] post;
            post = new Variable[th.pos.Length - 1];
            Array.Copy(th.pos, 1, post, 0, th.pos.Length - 1);
            return CodeMO.mro_INVOKE.Invoke((P6opaque)th.pos[0].Fetch(),
                    th.caller, post, th.named);
        }

        private static SubInfo JunctionFallbackSI = new SubInfo("Junction.FALLBACK", JunctionFallbackC);
        private static Frame JunctionFallbackC(Frame th) {
            if (th.ip == 0) {
                if (!th.pos[0].Fetch().IsDefined())
                    return Kernel.Die(th, "Cannot autothread an undefined junction");
                P6opaque jo = (P6opaque)th.pos[0].Fetch();
                th.lex0 = Kernel.UnboxAny<Variable[]>((P6any)jo.slots[1]);
                th.lex1 = new Variable[((Variable[])th.lex0).Length];
                th.lex2 = jo.slots[0];
                th.lex3 = th.pos[1].Fetch().mo.mro_raw_Str.Get(th.pos[1]);
                th.lex4 = new Variable[th.pos.Length - 1];
                Array.Copy(th.pos, 2, (Variable[])th.lex4, 1, th.pos.Length-2);
            }

            Variable[] src = (Variable[]) th.lex0;
            Variable[] dst = (Variable[]) th.lex1;
            if (th.ip > 0)
                dst[th.ip - 1] = (Variable)th.resultSlot;

            if (th.ip == dst.Length) {
                P6opaque nj = new P6opaque(Kernel.JunctionMO);
                nj.slots[0] = th.lex2;
                nj.slots[1] = Kernel.BoxRaw(dst, Kernel.ParcelMO);
                th.caller.resultSlot = Kernel.NewROScalar(nj);
                return th.caller;
            }

            Variable[] npos = (Variable[]) th.lex4;
            npos[0] = src[th.ip++];

            return npos[0].Fetch().InvokeMethod(th, (string)th.lex3,
                    npos, th.named);
        }

        public static Frame Die(Frame caller, string msg) {
            return SearchForHandler(caller, SubInfo.ON_DIE, null, -1, null,
                    BoxAnyMO<string>(msg, StrMO));
        }

        public static P6any SigSlurpCapture(Frame caller) {
            P6any nw = new P6opaque(CaptureMO);
            nw.SetSlot("positionals", caller.pos);
            nw.SetSlot("named", caller.named);
            caller.named = null;
            return nw;
        }

        public static STable PairMO;
        public static STable EnumMapMO;
        public static STable CallFrameMO;
        public static STable CaptureMO;
        public static STable GatherIteratorMO;
        public static STable IterCursorMO;
        public static P6any AnyP;
        public static P6any ArrayP;
        public static P6any EMPTYP;
        public static P6any HashP;
        public static P6any IteratorP;
        public static readonly STable JunctionMO;
        public static readonly STable LabelMO;
        public static readonly STable AnyMO;
        public static readonly STable IteratorMO;
        public static readonly STable ScalarMO;
        public static readonly STable StashMO;
        public static STable PseudoStashMO;
        public static readonly STable CodeMO;
        public static readonly STable StrMO;
        public static readonly STable NumMO;
        public static readonly STable IntMO;
        public static readonly STable RatMO;
        public static readonly STable FatRatMO;
        public static readonly STable ComplexMO;
        public static readonly STable ArrayMO;
        public static readonly STable CursorMO;
        public static readonly STable MatchMO;
        public static readonly STable ParcelMO;
        public static readonly STable ListMO;
        public static readonly STable HashMO;
        public static readonly STable BoolMO;
        public static readonly STable MuMO;
        public static readonly P6any StashP;

        public static readonly Variable TrueV;
        public static readonly Variable FalseV;

        public static P6any MakeSub(SubInfo info, Frame outer) {
            P6opaque n = new P6opaque(info.mo ?? CodeMO, 2);
            n.slots[0] = outer;
            if (outer != null) outer.MarkShared();
            n.slots[1] = info;
            return n;
        }

        public class MMDParameter {
            public STable type;
            public bool constrained;
            public bool required;

            public static MMDParameter TOP = new MMDParameter();
            public static MMDParameter BOTTOM = new MMDParameter();

            // XXX Should requiredness be factored in?
            // 2: narrower 0: tied 1: less narrow 3: incomparable
            // by design these can be ORed
            public int IsNarrowerThan(MMDParameter other) {
                if (other == TOP) return (this == TOP) ? 0 : 2;
                if (other == BOTTOM) return (this == BOTTOM) ? 0 : 1;
                if (this == TOP) return 1;
                if (this == BOTTOM) return 2;

                bool k1 = type.HasMRO(other.type);
                bool k2 = other.type.HasMRO(type);
                if (k1 && !k2) return 2;
                if (k2 && !k1) return 1;

                if (constrained && !other.constrained) return 2;
                if (other.constrained && !constrained) return 1;

                return 0;
            }
        }

        public class MMDCandidateLongname {
            public P6any impl;
            public int tien;
            public SubInfo info;

            public List<MMDParameter> pos;
            public Dictionary<string,MMDParameter> nam;

            public bool slurpy_pos;
            public bool slurpy_nam;
            public bool extra_constraints;

            public bool IsNarrowerThan(MMDCandidateLongname other) {
                int narrower = 0;

                for (int i = 0; ; i++) {
                    MMDParameter tp = (i < pos.Count) ? pos[i] :
                        (slurpy_pos ? MMDParameter.TOP : MMDParameter.BOTTOM);
                    MMDParameter op = (i < other.pos.Count) ? other.pos[i] :
                        (other.slurpy_pos ? MMDParameter.TOP : MMDParameter.BOTTOM);
                    narrower |= tp.IsNarrowerThan(op);
                    if (i >= pos.Count && i >= other.pos.Count) break;
                }

                List<string> ns = new List<string>(nam.Keys);
                foreach (string s in other.nam.Keys)
                    if (!nam.ContainsKey(s)) ns.Add(s);
                foreach (string s in ns) {
                    MMDParameter tp = nam.ContainsKey(s) ? nam[s] :
                        (slurpy_nam ? MMDParameter.TOP : MMDParameter.BOTTOM);
                    MMDParameter op = other.nam.ContainsKey(s) ? other.nam[s] :
                        (other.slurpy_nam ? MMDParameter.TOP : MMDParameter.BOTTOM);
                    narrower |= tp.IsNarrowerThan(op);
                }

                if (slurpy_nam && !other.slurpy_nam) narrower |= 1;
                if (!slurpy_nam && other.slurpy_nam) narrower |= 2;

                if (narrower == 0 || narrower == 3)
                    return tien < other.tien;

                return (narrower == 2);
            }

            public MMDCandidateLongname(P6any impl, int tien) {
                this.impl = impl;
                this.tien = tien;
                info = (SubInfo) impl.GetSlot("info");
                int ct = info.sig_i.Length / SubInfo.SIG_I_RECORD;

                pos = new List<MMDParameter>();
                nam = new Dictionary<string,MMDParameter>();
                int rix = 0;

                for (int ix = 0; ix < ct; ix++) {
                    int flags = info.sig_i[ix*SubInfo.SIG_I_RECORD +
                        SubInfo.SIG_I_FLAGS];
                    int nnames = info.sig_i[ix*SubInfo.SIG_I_RECORD +
                        SubInfo.SIG_I_NNAMES];
                    int rbase = rix;
                    rix += (nnames + 1);
                    MMDParameter p = new MMDParameter();
                    p.required = ((flags & (SubInfo.SIG_F_OPTIONAL | SubInfo.SIG_F_HASDEFAULT)) == 0);
                    p.constrained = false;
                    p.type = AnyMO;
                    if ((flags & SubInfo.SIG_F_HASDEFAULT) != 0) rix++;
                    if ((flags & SubInfo.SIG_F_HASTYPE) != 0)
                        p.type = (STable) info.sig_r[rix++];

                    // XXX The long name model does not represent the full
                    // diversity of Perl 6 parameters, instead restricting
                    // them to 'only positional' or '1 name'
                    if (nnames > 0 && (flags & SubInfo.SIG_F_POSITIONAL) == 0) {
                        if (nnames == 1) {
                            nam[(string)info.sig_r[rbase+1]] = p;
                        } else {
                            slurpy_nam = true;
                            extra_constraints = true;
                        }
                    } else if ((flags & SubInfo.SIG_F_SLURPY_PCL) != 0) {
                        slurpy_pos = true;
                    } else if ((flags & SubInfo.SIG_F_SLURPY_CAP) != 0) {
                        slurpy_pos = true;
                        slurpy_nam = true;
                    } else if ((flags & SubInfo.SIG_F_SLURPY_POS) != 0) {
                        slurpy_pos = true;
                    } else if ((flags & SubInfo.SIG_F_SLURPY_NAM) != 0) {
                        slurpy_nam = true;
                    } else if ((flags & SubInfo.SIG_F_POSITIONAL) != 0) {
                        pos.Add(p);
                    }
                }
            }

            private static long unique;
            public static long get_unique() {
                return Interlocked.Increment(ref unique);
            }

            public string LongName() {
                List<string> bits = new List<string>();

                foreach (MMDParameter p in pos) {
                    string n = p.type.name;
                    if (!p.required) n += "?";
                    if (p.constrained)
                        n += " where { ... " + get_unique() + " }";
                    bits.Add(n);
                }

                if (slurpy_pos) bits.Add("*@_");

                List<string> names = new List<string>(nam.Keys);
                names.Sort();
                foreach (string nm in names) {
                    MMDParameter p = nam[nm];
                    string b = p.type.name + " :$" + nm;
                    if (p.required) b += "!";
                    if (p.constrained)
                        b += " where { ... " + get_unique() + " }";
                    bits.Add(b);
                }

                if (slurpy_nam) bits.Add("*%_");
                string full = JoinS(", ", bits);
                if (extra_constraints)
                    full += ";; |$c where { ... " + get_unique() + " }";
                return full;
            }
        }

        // This is a little odd.  Pending full understanding of the
        // rules it aims more for Rakudo compatibility than anything
        // else.
        private static List<P6any> SortCandidates(P6any[] raw) {
            int[] links = new int[raw.Length * raw.Length * 2];
            int ap = 0;
            int[] heads = new int[raw.Length * 2];
            int[] orig = new int[raw.Length];
            MMDCandidateLongname[] lns = new MMDCandidateLongname[raw.Length];
            int nlns = 0;
            int tien = 0;

            for (int i = 0; i < raw.Length; i++) {
                if (raw[i] == null) {
                    tien++;
                } else {
                    lns[nlns] = new MMDCandidateLongname(raw[i], tien);
                    orig[nlns] = i;
                    heads[2*nlns] = -1;
                    nlns++;
                }
            }

            for (int i = 0; i < nlns; i++) {
                for (int j = 0; j < nlns; j++) {
                    if (lns[i].IsNarrowerThan(lns[j])) {
                        //Console.WriteLine("{0} < {1}", lns[i].LongName(), lns[j].LongName());
                        heads[2*j+1]++;
                        links[ap] = heads[2*i];
                        links[ap+1] = j;
                        heads[2*i] = ap;
                        ap += 2;
                    }
                }
            }

            List<P6any> outp = new List<P6any>();

            int k = nlns;
            while (k != 0) {
                int d = 0;
                for (int i = 0; i < nlns; i++) {
                    if (heads[2*i+1] != 0) continue;
                    heads[2*i+1] = -1;
                    d++;
                }
                for (int i = 0; i < nlns; i++) {
                    if (heads[2*i+1] != -1) continue;
                    heads[2*i+1] = -2;

                    for (int j = heads[2*i]; j >= 0; j = links[j]) {
                        heads[2*links[j+1]+1]--;
                    }
                    // prevent constrained candidates from being part of a tie
                    if (lns[i].extra_constraints) outp.Add(null);
                    outp.Add(raw[orig[i]]);
                    if (lns[i].extra_constraints) outp.Add(null);
                    k--;
                }
                if (d == 0 && k != 0) {
                    throw new NieczaException("Partial order wedged");
                }
                outp.Add(null);
            }

            return outp;
        }

        public static Frame TypeDispatcher(Frame th, bool tailcall) {
            Frame dth = th;
            while ((dth.info.param0 as P6any[]) == null) dth = dth.outer;

            //Console.WriteLine("---");
            DispatchEnt root = new DispatchEnt();
            DispatchEnt ptr  = root;

            List<P6any> sp = SortCandidates(dth.info.param0 as P6any[]);

            // XXX I think this is a harmless race
            //MMDCandidate[] cs = dth.info.param1 as MMDCandidate[];
            //if (cs == null)
            //    dth.info.param1 = cs = MMDAnalyze(dth.info.param0 as P6any[]);
            int tie_state = 0;
            // 0: seen nothing  1: after first group  2: an item in first group
            foreach (P6any p in sp) {
                if (p == null) {
                    if (tie_state == 2) tie_state = 1;
                    //Console.WriteLine(".");
                    continue;
                }
                //Console.WriteLine((new MMDCandidateLongname(p,0)).LongName());
                SubInfo si = (SubInfo)p.GetSlot("info");
                Frame   o  = (Frame)p.GetSlot("outer");
                if (si.Binder(th, o, p, dth.pos, dth.named, true,
                            null) == null ) {
                    //Console.WriteLine("NOT BINDABLE");
                } else {
                    if (tie_state == 0) tie_state = 2;
                    else if (tie_state == 2)
                        return Kernel.Die(th, "Ambiguous dispatch for " + dth.info.name);
                    //Console.WriteLine("BINDABLE");
                    ptr.next = new DispatchEnt(null, p);
                    ptr = ptr.next;
                }
            }

            root = root.next;
            if (root == null)
                return Kernel.Die(th, "No matching candidates to dispatch for " + dth.info.name);

            if (tailcall) th = th.caller;
            return root.info.Binder(th, root.outer, root.ip6,
                    dth.pos, dth.named, false, root);
        }

        private static Frame StandardTypeProtoC(Frame th) {
            return TypeDispatcher(th, true);
        }

        public static P6any MakeDispatcher(string name, P6any proto, P6any[] cands) {
            //string s1 = "Dispatch";
            //foreach (P6any s in cands)
            //    s1 += ", " + ((SubInfo)s.GetSlot("info")).name;
            //Console.WriteLine("MakeDispatcher: {0}", s1);

            if (proto != null && proto.mo.name == "Regex") goto ltm;
            for (int i = 0; i < cands.Length; i++) {
                if (cands[i] == null) continue;
                if (cands[i].mo.name == "Regex") goto ltm;
                break;
            }

            SubInfo si = new SubInfo(name, StandardTypeProtoC);
            si.param0 = cands;
            return Kernel.MakeSub(si, null);
ltm:
            List<P6any> lp = new List<P6any>();
            foreach (P6any p in cands)
                if (p != null) lp.Add(p);
            return Lexer.MakeDispatcher(name, lp.ToArray());
        }

        public static bool SaferMode;

        private static Frame SaferTrap(Frame th) {
            return Die(th, th.info.name + " may not be used in safe mode");
        }

        public static void CheckUnsafe(SubInfo info) {
            if (SaferMode)
                info.code = SaferTrap;
        }
        public static Variable BoxAny<T>(T v, P6any proto) {
            if (proto == BoolMO.typeObject) {
                if (v is bool)
                    return ((bool) (object) v) ? TrueV : FalseV;
                else
                    return ((int) (object) v) != 0 ? TrueV : FalseV;
            }
            return NewROScalar(new BoxObject<T>(v, ((P6opaque)proto).mo));
        }

        public static void SetBox<T>(P6any obj, T v) {
            ((BoxObject<T>) obj).value = v;
        }

        public static Variable BoxAnyMO<T>(T v, STable proto) {
            if (proto == BoolMO) {
                if (v is bool)
                    return ((bool) (object) v) ? TrueV : FalseV;
                else
                    return ((int) (object) v) != 0 ? TrueV : FalseV;
            }
            return NewROScalar(new BoxObject<T>(v, proto));
        }

        public static P6any BoxRaw<T>(T v, STable proto) {
            return new BoxObject<T>(v, proto);
        }

        // check whence before calling
        public static void Vivify(Variable v) {
            ViviHook w = v.whence;
            v.whence = null;
            w.Do(v);
        }

        public static Variable Decontainerize(Variable rhs) {
            if (!rhs.rw) return rhs;
            P6any v = rhs.Fetch();
            return new SimpleVariable(false, rhs.islist, v.mo, null, v);
        }

        public const int NBV_RO = 0;
        public const int NBV_RW = 1;
        public const int NBV_LIST = 2;
        public static Variable NewBoundVar(int mode, STable type, Variable rhs) {
            bool rw = rhs.rw && (mode & NBV_RW) != 0;
            // we always have to fetch, because of subsets (XXX?)
            P6any rhso = rhs.Fetch();
            if (!rhso.Does(type))
                throw new NieczaException("Nominal type check failed in nonparameter binding; got " + rhso.mo.name + ", needed " + type.name);

            if (rw) {
                // working RW bind implies !rhs.islist, !islist; will return
                // rhs if successful
                if (rhs.whence != null) Vivify(rhs);
                return rhs;
            }

            bool islist = (mode & NBV_LIST) != 0;
            // if source is !rw, we may not need to fetch it
            if (!rhs.rw && islist == rhs.islist)
                return rhs;

            return new SimpleVariable(false, islist, rhso.mo, null, rhso);
        }

        public static Variable Assign(Variable lhs, Variable rhs) {
            if (!lhs.islist) {
                if (!lhs.rw)
                    throw new NieczaException("assigning to readonly value");

                lhs.Store(rhs.Fetch());
            } else {
                lhs.Fetch().mo.mro_LISTSTORE.Get(lhs, rhs);
            }
            return lhs;
        }

        // ro, not rebindable
        public static Variable NewROScalar(P6any obj) {
            return new SimpleVariable(false, false, obj.mo, null, obj);
        }

        public static Variable NewRWScalar(STable t, P6any obj) {
            return new SimpleVariable(true, false, t, null, obj);
        }

        public static Variable NewMuScalar(P6any obj) {
            return new SimpleVariable(true, false, MuMO, null, obj);
        }

        public static Variable NewTypedScalar(STable t) {
            if (t == null)
                return new SimpleVariable(true, false, MuMO, null,
                        AnyMO.typeObject);

            return new SimpleVariable(true, false, t, null, t.initObject);
        }

        public static Variable NewRWListVar(P6any container) {
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
                nv.Push(NewMuScalar(vals[i].Fetch()));
            return nv;
        }

        public static string[] commandArgs;

        public static VarDeque SortHelper(Frame th, P6any cb, VarDeque from) {
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
                return AnyMO.typeVar;
            }
        }

        public static void SetStatus(Frame th, string name, Variable v) {
            th = th.caller;
            while (true) {
                string n = th.info.name;
                // Mega-Hack: These functions wrap stuff and should
                // propagate $/
                if (n == "CORE infix:<~~>" || n == "ExitRunloop" || n == "KERNEL AutoThreadSub") {
                    th = th.caller;
                    continue;
                }
                break;
            }
            th.LexicalBind(name, v);
        }

        public static Frame DefaultNew(Frame th, P6any proto, VarHash args) {
            P6opaque n;
            P6how.AttrInfo[] prog;
            int i;
            if (th.lex9 == null) {
                th.lex9 = n = new P6opaque(((P6opaque)proto).mo);
                i = 0;
                prog = n.mo.init_program;
            } else {
                n = (P6opaque) th.lex9;
                prog = n.mo.init_program;
                i = th.lexi0;
                goto value;
            }

again:      if (i == prog.Length) {
                th.caller.resultSlot = NewROScalar(n);
                return th.caller;
            }

            Variable vx;
            if ((prog[i].flags & P6how.A_PUBLIC) != 0 &&
                    args.TryGetValue(prog[i].name, out vx)) {
                args.Remove(prog[i].name);
                th.resultSlot = vx;
            } else if (prog[i].init == null) {
                th.resultSlot = null;
            } else if (prog[i].name == null) {
                P6any init = prog[i].init;
                th.lexi0 = i;

                SubInfo si = (SubInfo) init.GetSlot("info");
                VarHash build_args = new VarHash();
                int ic = 0;
                int oc = 0;
                while (ic < si.sig_i.Length) {
                    int fl = si.sig_i[ic + SubInfo.SIG_I_FLAGS];
                    int nn = si.sig_i[ic + SubInfo.SIG_I_NNAMES];
                    ic += SubInfo.SIG_I_RECORD;

                    for (int j = 0; j < nn; j++) {
                        string name = (string) si.sig_r[oc+j+1];
                        if (args.ContainsKey(name)) {
                            build_args[name] = args[name];
                            args.Remove(name);
                        }
                    }

                    oc += 1 + nn;
                    if ((fl & SubInfo.SIG_F_HASTYPE) != 0) oc++;
                    if ((fl & SubInfo.SIG_F_HASDEFAULT) != 0) oc++;
                }

                return init.Invoke(th, new Variable[] { NewROScalar(n) },
                    build_args);
            } else {
                P6any init = prog[i].init;
                th.lexi0 = i;
                return init.Invoke(th, Variable.None, null);
            }

value:      vx = (Variable) th.resultSlot;
            if (prog[i].name == null) {
                i++;
                goto again;
            }

            if ((prog[i].flags & ~P6how.A_PUBLIC) == 0) {
                if (prog[i].type == null)
                    n.SetSlot(prog[i].name, NewMuScalar(
                        vx != null ? vx.Fetch() : AnyMO.typeObject));
                else
                    n.SetSlot(prog[i].name, NewRWScalar(prog[i].type,
                        vx != null ? vx.Fetch() : prog[i].type.initObject));
            } else {
                Variable obj = (prog[i].flags & P6how.A_HASH) != 0 ?
                    CreateHash() : CreateArray();
                if (vx != null) Assign(obj, vx);
                n.SetSlot(prog[i].name, obj);
            }

            i++;
            goto again;
        }

        public static Frame PromoteToList(Frame th, Variable v) {
            if (!v.islist) {
                P6opaque lst = new P6opaque(Kernel.ListMO);
                lst.slots[0 /*items*/] = new VarDeque(v);
                lst.slots[1 /*rest*/ ] = new VarDeque();
                th.resultSlot = Kernel.NewRWListVar(lst);
                return th;
            }
            P6any o = v.Fetch();
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

        public static void IterToList(P6any list, VarDeque iter) {
            VarDeque items = new VarDeque();
            P6any item;
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

        public static VarDeque IterFlatten(VarDeque inq) {
            VarDeque outq = new VarDeque();
            Variable inq0v;
            P6any inq0;

again:
            if (inq.Count() == 0)
                return outq;
            inq0v = inq[0];
            inq0 = inq0v.Fetch();
            if (inq0v.islist) {
                inq.Shift();
                inq.UnshiftD(inq0.mo.mro_raw_iterator.Get(inq0v));
                goto again;
            }
            if (inq0.mo.HasMRO(IterCursorMO)) {
                Frame th = new Frame(null, null, IF_SI, Kernel.AnyP);
                th.lex0 = inq;
                P6opaque thunk = new P6opaque(Kernel.GatherIteratorMO);
                th.lex = new Dictionary<string,object>();
                th.lex["!return"] = null;
                thunk.slots[0] = NewMuScalar(th);
                thunk.slots[1] = NewMuScalar(AnyP);
                outq.Push(NewROScalar(thunk));
                return outq;
            }
            outq.Push(inq0v);
            inq.Shift();
            goto again;
        }

        private static SubInfo IF_SI = new SubInfo("iter_flatten", IF_C);
        private static Frame IF_C(Frame th) {
            VarDeque inq = (VarDeque) th.lex0;
            if (IterHasFlat(inq, true)) {
                return Take(th, inq.Shift());
            } else {
                return Take(th, NewROScalar(Kernel.EMPTYP));
            }
        }

        public static bool IterHasFlat(VarDeque iter, bool flat) {
            while (true) {
                if (iter.Count() == 0)
                    return false;
                Variable i0 = iter[0];
                if (i0.islist && flat) {
                    iter.Shift();
                    iter.UnshiftD(i0.Fetch().mo.mro_raw_iterator.Get(i0));
                    continue;
                }
                P6any i0v = i0.Fetch();
                if (i0v.mo.HasMRO(IterCursorMO)) {
                    iter.Shift();
                    iter.UnshiftN(i0v.mo.mro_raw_reify.Get(i0));
                    continue;
                }

                return true;
            }
        }

        public static Variable GetFirst(Variable lst) {
            if (!lst.islist) {
                return lst;
            }
            P6opaque dyl = lst.Fetch() as P6opaque;
            if (dyl == null) { goto slow; }
            if (dyl.mo != Kernel.ListMO) { goto slow; }
            VarDeque itemsl = (VarDeque) dyl.GetSlot("items");
            if (itemsl.Count() == 0) {
                VarDeque restl = (VarDeque) dyl.GetSlot("rest");
                if (restl.Count() == 0) {
                    return AnyMO.typeVar;
                }
                goto slow;
            }
            return itemsl[0];

slow:
            return RunInferior(lst.Fetch().InvokeMethod(
                        GetInferiorRoot(), "head", new Variable[] {lst}, null));
        }

        public static Variable CreateArray() {
            P6any v = new P6opaque(ArrayMO, 2);
            v.SetSlot("items", new VarDeque());
            v.SetSlot("rest", new VarDeque());
            return NewRWListVar(v);
        }

        public static Variable CreateHash() {
            P6any v = BoxRaw(new VarHash(), HashMO);
            return NewRWListVar(v);
        }

        public static Variable StashyMerge(Variable o, Variable n, string d1, string d2) {
            if (n.rw || n.islist) return o;
            if (o.rw || o.islist) return n;

            P6any oo = o.Fetch();
            P6any nn = n.Fetch();

            if (oo == nn) return o;

            if (!oo.IsDefined() && !nn.IsDefined() && oo.mo.who == nn.mo.who) {
                if (oo.mo.mo.isPackage) return n;
                if (nn.mo.mo.isPackage) return o;
            }

            throw new NieczaException("Funny merge failure " + d1 + "::" + d2 +
                    " (" + oo.mo.name + ", " + nn.mo.name + ")");
        }

        public static BValue GetVar(string who, string name) {
            return PackageLookup(GetStash(who), name);
        }

        public static BValue PackageLookup(P6any parent, string name) {
            Dictionary<string,BValue> stash =
                UnboxAny<Dictionary<string,BValue>>(parent);
            BValue v;

            if (stash.TryGetValue(name, out v)) {
                return v;
            } else if (name.StartsWith("@")) {
                return (stash[name] = new BValue(CreateArray()));
            } else if (name.StartsWith("%")) {
                return (stash[name] = new BValue(CreateHash()));
            } else {
                return (stash[name] = new BValue(NewTypedScalar(null)));
            }
        }

        private static void Handler_Vonly(STable kl, string name,
                ContextHandler<Variable> cv, object cu) {
            WrapHandler0(kl, name, cv, cv, cu);
        }

        private static void Handler_PandBox<T>(STable kl, string name,
                ContextHandler<T> cvu, STable box) {
            ContextHandler<Variable> cv = new CtxBoxify<T>(cvu, box);
            WrapHandler0(kl, name, cv, cv, cvu);
        }

        private static void Handler_PandBoxInty(STable kl, string name,
                ContextHandler<double> cvu) {
            ContextHandler<Variable> cv = new CtxBoxifyInty(cvu);
            WrapHandler0(kl, name, cv, cv, cvu);
        }

        private static void Handler_PandCont(STable kl, string name,
                ContextHandler<P6any> cvu) {
            ContextHandler<Variable> cv = new CtxContainerize(cvu);
            WrapHandler0(kl, name, cv, cv, cvu);
        }

        private static void WrapHandler0(STable kl, string name,
                ContextHandler<Variable> cv, object cvb, object cvu) {
            DynBlockDelegate dbd = delegate (Frame th) {
                th.caller.resultSlot = cv.Get((Variable)th.lex0);
                return th.caller;
            };
            SubInfo si = new SubInfo("KERNEL " + kl.name + "." + name, dbd);
            si.sig_i = new int[3] {
                SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_POSITIONAL,
                0, 0 };
            si.sig_r = new object[1] { "self" };
            si.param1 = cvb;
            si.param0 = cvu;
            kl.AddMethod(0, name, MakeSub(si, null));
        }

        private static void WrapHandler1(STable kl, string name,
                IndexHandler cv) {
            DynBlockDelegate dbd = delegate (Frame th) {
                th.caller.resultSlot = cv.Get((Variable)th.lex0,
                        (Variable)th.lex1);
                return th.caller;
            };
            SubInfo si = new SubInfo("KERNEL " + kl.name + "." + name, dbd);
            si.sig_i = new int[6] {
                SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_POSITIONAL, 0, 0,
                SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_POSITIONAL, 1, 0
            };
            si.sig_r = new object[2] { "self", "$key" };
            si.param1 = cv;
            kl.AddMethod(0, name, MakeSub(si, null));
        }

        private static void WrapPushy(STable kl, string name,
                PushyHandler cv) {
            DynBlockDelegate dbd = delegate (Frame th) {
                Variable[] fullpc = UnboxAny<Variable[]>(
                        ((Variable)th.lex1).Fetch());
                Variable[] chop = new Variable[fullpc.Length - 1];
                Array.Copy(fullpc, 1, chop, 0, chop.Length);
                th.caller.resultSlot = cv.Invoke((Variable)th.lex0, chop);
                return th.caller;
            };
            SubInfo si = new SubInfo("KERNEL " + kl.name + "." + name, dbd);
            si.sig_i = new int[6] {
                SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_POSITIONAL, 0, 0,
                SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_SLURPY_PCL, 1, 0
            };
            si.sig_r = new object[2] { "self", "$args" };
            si.param1 = cv;
            kl.AddMethod(0, name, MakeSub(si, null));
        }

        private static Variable DispIndexy(IndexHandler at, IndexHandler exist,
                IndexHandler del, BindHandler bind, VarHash n,
                Variable self, Variable index) {

            if (n == null) goto def;
            if (del != null && n.ContainsKey("exists"))
                return exist.Get(self, index);
            if (del != null && n.ContainsKey("delete"))
                return del.Get(self, index);

            if (n.ContainsKey("k"))
                return new KeySlicer(0, at).Get(self, index);
            if (n.ContainsKey("kv"))
                return new KeySlicer(1, at).Get(self, index);
            if (n.ContainsKey("p"))
                return new KeySlicer(2, at).Get(self, index);
            if (n.ContainsKey("BIND_VALUE"))
                return bind.Bind(self, index, n["BIND_VALUE"]);

def:        return at.Get(self, index);
        }

        private static void WrapIndexy(STable kl, string name,
                IndexHandler at, IndexHandler exist, IndexHandler del,
                BindHandler bind) {
            DynBlockDelegate dbd = delegate (Frame th) {
                th.caller.resultSlot = DispIndexy(at, exist, del, bind,
                    th.named, (Variable)th.lex0, (Variable)th.lex1);
                return th.caller;
            };

            SubInfo si = new SubInfo("KERNEL " + kl.name + "." + name, dbd);
            if (del != null) {
                si.sig_i = new int[24] {
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_POSITIONAL, 0, 0,
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_POSITIONAL, 1, 0,
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_OPTIONAL, -1, 1,
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_OPTIONAL, -1, 1,
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_OPTIONAL, -1, 1,
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_OPTIONAL, -1, 1,
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_OPTIONAL, -1, 1,
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_OPTIONAL, -1, 1
                };
                si.sig_r = new object[14] { "self", "$index", "exists", "exists", "delete", "delete", "k", "k", "kv", "kv", "p", "p", "BIND_VALUE", "BIND_VALUE" };
            } else {
                si.sig_i = new int[18] {
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_POSITIONAL, 0, 0,
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_POSITIONAL, 1, 0,
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_OPTIONAL, -1, 1,
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_OPTIONAL, -1, 1,
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_OPTIONAL, -1, 1,
                    SubInfo.SIG_F_RWTRANS | SubInfo.SIG_F_OPTIONAL, -1, 1
                };
                si.sig_r = new object[10] { "self", "$index", "k", "k", "kv", "kv", "p", "p", "BIND_VALUE", "BIND_VALUE" };
            }
            si.param1 = new object[] { at, exist, del, bind };
            kl.AddMethod(0, name, MakeSub(si, null));
        }

        private static SubInfo IRSI = new SubInfo("InstantiateRole", IRC);
        private static Frame IRC(Frame th) {
            switch (th.ip) {
                case 0:
                    {
                        string s = "";
                        th.lex0 = th.pos[0].Fetch().mo;
                        bool cache_ok = true;
                        Variable[] args;
                        P6any argv = th.pos[1].Fetch();
                        if (argv.mo == Kernel.ParcelMO) {
                            args = UnboxAny<Variable[]>(argv);
                        } else {
                            args = new Variable[] { th.pos[1] };
                        }
                        Variable[] to_pass = new Variable[args.Length];
                        for (int i = 0; i < args.Length; i++) {
                            P6any obj = args[i].Fetch();
                            to_pass[i] = NewROScalar(obj);
                            if (obj.mo == StrMO) {
                                string p = UnboxAny<string>(obj);
                                s += new string((char)p.Length, 1);
                                s += p;
                            } else { cache_ok = false; }
                        }
                        if (!cache_ok) {
                            return ((STable) th.lex0).mo.roleFactory.
                                Invoke(th.caller, to_pass, null);
                        }
                        th.lex1 = s;
                        bool ok;
                        P6any r;
                        lock (th.lex0)
                            ok = ((STable) th.lex0).mo.instCache.
                                TryGetValue((string) th.lex1, out r);
                        if (ok) {
                            th.caller.resultSlot = NewROScalar(r);
                            return th.caller;
                        }
                        th.ip = 1;
                        return ((STable) th.lex0).mo.roleFactory.
                            Invoke(th, to_pass, null);
                    }
                case 1:
                    lock (th.lex0) {
                        ((STable) th.lex0).mo.instCache[(string) th.lex1]
                            = ((Variable) th.resultSlot).Fetch();
                    }
                    th.caller.resultSlot = th.resultSlot;
                    return th.caller;
                default:
                    return Die(th, "Invalid IP");
            }
        }
        public static Frame InstantiateRole(Frame th, Variable[] pcl) {
            return IRSI.Binder(th, null, null, pcl, null, false, null);
        }

        private static STable DoRoleApply(STable b,
                STable role) {
            STable n = new STable(b.name + " but " + role.name);
            if (role.mo.local_attr.Count != 0)
                throw new NieczaException("RoleApply with attributes NYI");
            if (role.mo.superclasses.Count != 0)
                throw new NieczaException("RoleApply with superclasses NYI");
            STable[] nmro = new STable[b.mo.mro.Length + 1];
            Array.Copy(b.mo.mro, 0, nmro, 1, b.mo.mro.Length);
            nmro[0] = n;
            n.FillClass(b.all_slot, new STable[] { b }, nmro);
            foreach (P6how.MethodInfo mi in role.mo.lmethods)
                n.mo.lmethods.Add(mi);
            foreach (P6how.AttrInfo ai in role.mo.local_attr)
                n.mo.local_attr.Add(ai);
            n.mo.Invalidate();

            n.how = BoxAny<STable>(n, b.how).Fetch();
            n.typeObject = n.initObject = new P6opaque(n);
            n.typeVar = n.initVar = NewROScalar(n.typeObject);
            ((P6opaque)n.typeObject).slots = null;

            return n;
        }

        public static STable RoleApply(STable b,
                STable role) {
            lock (b) {
                STable rs;
                if (b.mo.butCache.TryGetValue(role, out rs))
                    return rs;
                return b.mo.butCache[role] = DoRoleApply(b, role);
            }
        }

        public static Frame StartP6Thread(Frame th, P6any sub) {
            th.MarkSharedChain();
            Thread thr = new Thread(delegate () {
                    rlstack = new LastFrameNode();
                    rlstack.cur = th;
                    RunInferior(sub.Invoke(GetInferiorRoot(),
                            Variable.None, null));
                });
            thr.Start();
            th.resultSlot = thr;
            return th;
        }

        public static Variable RunLoop(string main_unit,
                string[] args, DynBlockDelegate boot) {
            if (args == null) {
                return BootModule(main_unit, boot);
            }
            commandArgs = args;
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
            Variable r = null;
            try {
                r = BootModule(main_unit, boot);
            } catch (NieczaException n) {
                Console.Error.WriteLine("Unhandled exception: {0}", n);
                Environment.Exit(1);
            }
            return r;
        }

        class ExitRunloopException : Exception {
            public string payload;
            public ExitRunloopException(string p) { payload = p; }
        }
        public static SubInfo ExitRunloopSI =
            new SubInfo("ExitRunloop", ExitRunloopC);
        private static Frame ExitRunloopC(Frame th) {
            throw new ExitRunloopException(th.lex0 as string);
        }

        public const int TRACE_CUR = 1;
        public const int TRACE_ALL = 2;

        public static int TraceFreq;
        public static int TraceCount;
        public static int TraceFlags;

        private static void DoTrace(Frame cur) {
            TraceCount = TraceFreq;
            if ((TraceFlags & TRACE_CUR) != 0)
                Console.WriteLine("{0}|{1} @ {2}",
                        cur.DepthMark(), cur.info.name, cur.ip);
            if ((TraceFlags & TRACE_ALL) != 0) {
                Console.WriteLine("Context:" + DescribeBacktrace(cur, null));
            }
        }

        public static void RunCore(ref Frame cur) {
            for(;;) {
                try {
                    if (TraceCount != 0) {
                        for(;;) {
                            if (--TraceCount == 0)
                                DoTrace(cur);
                            cur = cur.code(cur);
                        }
                    } else {
                        for(;;)
                            cur = cur.code(cur);
                    }
                } catch (ExitRunloopException ere) {
                    // XXX Stringifying all exceptions isn't very nice.
                    if (ere.payload != null)
                        throw new NieczaException(ere.payload);
                    return;
                } catch (Exception ex) {
                    cur = Kernel.Die(cur, ex.ToString());
                }
            }
        }

        // we like to make refs to these, so moving arrays is untenable
        class LastFrameNode {
            public LastFrameNode next, prev;
            public Frame cur, root;
        }
        [ThreadStatic] static LastFrameNode rlstack;
        public static void SetTopFrame(Frame f) {
            rlstack.cur = f;
        }

        // it is an error to throw an exception between GetInferiorRoot
        // and RunInferior
        public static Frame GetInferiorRoot() {
            LastFrameNode lfn = rlstack;
            if (lfn == null)
                lfn = rlstack = new LastFrameNode();
            if (lfn.next == null) {
                lfn.next = new LastFrameNode();
                lfn.next.prev = lfn;
            }
            Frame l = lfn.cur;
            rlstack = lfn.next;
            return lfn.next.cur = lfn.next.root = ((l == null ?
                        new Frame(null, null, ExitRunloopSI, Kernel.AnyP) :
                        l.MakeChild(null, ExitRunloopSI, AnyP)));
        }

        public static Variable RunInferior(Frame f) {
            LastFrameNode newlfn = rlstack;
            rlstack = newlfn;
            Variable result;

            try {
                Frame nroot = newlfn.root;
                newlfn.cur = f;
                RunCore(ref newlfn.cur);
                if (newlfn.cur != nroot) {
                    Console.Error.WriteLine("WRONG ExitRunloop TAKEN:" + DescribeBacktrace(newlfn.cur, null));
                    Console.Error.WriteLine("Correct:" + DescribeBacktrace(nroot, null));
                }
                result = (Variable) nroot.resultSlot;
            } finally {
                rlstack = newlfn.prev;
            }

            return result;
        }

        public static void AddCap(List<Variable> p,
                VarHash n, P6any cap) {
            Variable[] fp = cap.GetSlot("positionals") as Variable[];
            VarHash fn = cap.GetSlot("named")
                as VarHash;
            p.AddRange(fp);
            if (fn != null) AddMany(n, fn);
        }

        public static void AddMany(VarHash d1,
                VarHash d2) {
            foreach (KeyValuePair<string,Variable> kv in d2) {
                d1[kv.Key] = kv.Value;
            }
        }

        static Dictionary<string, P6any> stashes;
        public static P6any RootO;
        // used as the fallbacks for $*FOO
        public static P6any GlobalO;
        public static P6any ProcessO;

        static Kernel() {
            PhaserBanks = new VarDeque[] { new VarDeque(), new VarDeque(),
                new VarDeque() };

            CodeMO = new STable("Code");
            CodeMO.FillProtoClass(new string[] { "outer", "info" });
            SubInvokeSubSI.param1 = new InvokeSub();
            CodeMO.AddMethod(0, "postcircumfix:<( )>", MakeSub(SubInvokeSubSI, null));
            CodeMO.Invalidate();

            LabelMO = new STable("Label");
            LabelMO.FillProtoClass(new string[] { "target", "name" });
            LabelMO.Invalidate();

            BoolMO = new STable("Bool");
            Handler_Vonly(BoolMO, "Bool", new CtxReturnSelf(),
                    new CtxBoolUnbox());
            BoolMO.FillProtoClass(new string[] { "index" });
            BoolMO.Invalidate();
            TrueV  = NewROScalar(BoxRaw<int>(1, BoolMO));
            FalseV = NewROScalar(BoxRaw<int>(0, BoolMO));

            StrMO = new STable("Str");
            Handler_Vonly(StrMO, "Str", new CtxReturnSelf(),
                    new CtxJustUnbox<string>(""));
            Handler_PandBox(StrMO, "Bool", new CtxStrBool(), BoolMO);
            Handler_PandCont(StrMO, "succ", new CtxStrSuccish(true));
            Handler_PandCont(StrMO, "pred", new CtxStrSuccish(false));
            StrMO.FillProtoClass(new string[] { });
            StrMO.Invalidate();

            JunctionMO = new STable("Junction");
            Handler_PandBox(JunctionMO, "Bool", new CtxJunctionBool(), BoolMO);
            JunctionMO.AddMethod(0, "FALLBACK", MakeSub(JunctionFallbackSI, null));
            JunctionMO.FillProtoClass(new string[] { "kind_", "eigenstates_" });
            JunctionMO.Invalidate();

            IteratorMO = new STable("Iterator");
            IteratorMO.FillProtoClass(new string[] { });

            NumMO = new STable("Num");
            Handler_Vonly(NumMO, "Numeric", new CtxReturnSelf(),
                    new CtxCallMethodUnboxNumeric(null));
            Handler_Vonly(NumMO, "Str", new CtxStrNativeNum2Str(),
                    new CtxRawNativeNum2Str());
            Handler_PandBox(NumMO, "Bool", new CtxNum2Bool(), BoolMO);
            Handler_PandCont(NumMO, "succ", new CtxNumSuccish(+1));
            Handler_PandCont(NumMO, "pred", new CtxNumSuccish(-1));
            NumMO.FillProtoClass(new string[] { });
            NumMO.Invalidate();

            IntMO = new STable("Int");
            Handler_Vonly(IntMO, "Numeric", new CtxReturnSelf(),
                    new CtxCallMethodUnboxNumeric(null));
            Handler_PandBox(IntMO, "Bool", new CtxIntBool(), BoolMO);
            Handler_PandBox(IntMO, "Str", new CtxIntStr(), StrMO);
            Handler_PandCont(IntMO, "succ", new CtxIntSuccish(+1));
            Handler_PandCont(IntMO, "pred", new CtxIntSuccish(-1));
            IntMO.FillProtoClass(new string[] { });
            IntMO.Invalidate();
            FalseV.Fetch().SetSlot("index", BoxAnyMO(0, IntMO));
            TrueV.Fetch().SetSlot("index", BoxAnyMO(1, IntMO));

            RatMO = new STable("Rat");
            Handler_Vonly(RatMO, "Numeric", new CtxReturnSelf(),
                    new CtxCallMethodUnboxNumeric(null));
            Handler_PandBox(RatMO, "Bool", new CtxRatBool(), BoolMO);
            Handler_PandBox(RatMO, "Str", new CtxRatStr(), StrMO);
            Handler_PandCont(RatMO, "succ", new CtxRatSuccish(true));
            Handler_PandCont(RatMO, "pred", new CtxRatSuccish(false));
            RatMO.FillProtoClass(new string[] { });
            RatMO.Invalidate();

            FatRatMO = new STable("FatRat");
            Handler_Vonly(FatRatMO, "Numeric", new CtxReturnSelf(),
                    new CtxCallMethodUnboxNumeric(null));
            Handler_PandBox(FatRatMO, "Bool", new CtxFatRatBool(), BoolMO);
            Handler_PandBox(FatRatMO, "Str", new CtxFatRatStr(), StrMO);
            Handler_PandCont(FatRatMO, "succ", new CtxFatRatSuccish(true));
            Handler_PandCont(FatRatMO, "pred", new CtxFatRatSuccish(false));
            FatRatMO.FillProtoClass(new string[] { });
            FatRatMO.Invalidate();

            ComplexMO = new STable("Complex");
            Handler_Vonly(ComplexMO, "Numeric", new CtxReturnSelf(),
                    new CtxCallMethodUnboxNumeric(null));
            Handler_PandBox(ComplexMO, "Bool", new CtxComplexBool(), BoolMO);
            Handler_PandBox(ComplexMO, "Str", new CtxComplexStr(), StrMO);
            Handler_PandCont(ComplexMO, "succ", new CtxComplexSuccish(+1));
            Handler_PandCont(ComplexMO, "pred", new CtxComplexSuccish(-1));
            ComplexMO.FillProtoClass(new string[] { });
            ComplexMO.Invalidate();

            MuMO = new STable("Mu");
            Handler_Vonly(MuMO, "defined", new CtxBoolNativeDefined(),
                    new CtxRawNativeDefined());
            Handler_Vonly(MuMO, "Bool", new CtxBoolNativeDefined(),
                    new CtxRawNativeDefined());
            Handler_Vonly(MuMO, "item", new CtxReturnSelfItem(), null);
            MuMO.FillProtoClass(new string[] { });
            MuMO.Invalidate();

            StashMO = new STable("Stash");
            StashMO.FillProtoClass(new string[] { "name" });
            StashP = new P6opaque(StashMO);

            ParcelMO = new STable("Parcel");
            Handler_PandBox(ParcelMO, "iterator", new CtxParcelIterator(),
                    IteratorMO);
            WrapHandler1(ParcelMO, "LISTSTORE", new IxParcelLISTSTORE());
            Handler_Vonly(ParcelMO, "list", new CtxParcelList(), null);
            ParcelMO.FillProtoClass(new string[] { });
            ParcelMO.Invalidate();

            ArrayMO = new STable("Array");
            WrapHandler1(ArrayMO, "LISTSTORE", new IxArrayLISTSTORE());
            ArrayMO.FillProtoClass(new string[] { "items", "rest" });
            WrapIndexy(ArrayMO, "postcircumfix:<[ ]>", new IxListAtPos(true),
                    null, null, new IxListBindPos());
            ArrayMO.Invalidate();

            ListMO = new STable("List");
            WrapIndexy(ListMO, "postcircumfix:<[ ]>", new IxListAtPos(false),
                    null, null, new IxListBindPos());
            Handler_Vonly(ListMO, "pop", new PopList(), null);
            Handler_Vonly(ListMO, "shift", new ShiftList(), null);
            WrapPushy(ListMO, "push", new PushList());
            WrapPushy(ListMO, "unshift", new UnshiftList());
            Handler_PandBox(ListMO, "iterator", new CtxListIterator(),
                    IteratorMO);
            Handler_PandBox(ListMO, "Bool", new CtxListBool(), BoolMO);
            Handler_PandBoxInty(ListMO, "Numeric", new CtxListNum());
            Handler_Vonly(ListMO, "list", new CtxReturnSelfList(), null);
            ListMO.FillProtoClass(new string[] { "items", "rest" });
            ListMO.Invalidate();

            HashMO = new STable("Hash");
            WrapHandler1(HashMO, "LISTSTORE", new IxHashLISTSTORE());
            WrapIndexy(HashMO, "postcircumfix:<{ }>", new IxHashAtKey(),
                    new IxHashExistsKey(), new IxHashDeleteKey(),
                    new IxHashBindKey());
            Handler_PandBox(HashMO, "iterator", new CtxHashIterator(), IteratorMO);
            Handler_PandBox(HashMO, "Bool", new CtxHashBool(), BoolMO);
            Handler_Vonly(HashMO, "hash", new CtxReturnSelfList(), null);
            HashMO.FillProtoClass(new string[] { });
            HashMO.Invalidate();

            AnyMO = new STable("Any");
            Handler_Vonly(AnyMO, "list", new CtxAnyList(), null);
            WrapIndexy(AnyMO, "postcircumfix:<[ ]>", new IxAnyAtPos(),
                    null, null, new IxAnyBindPos());
            WrapIndexy(AnyMO, "postcircumfix:<{ }>", new IxAnyAtKey(),
                    new IxAnyExistsKey(), new IxAnyDeleteKey(),
                    new IxAnyBindKey());
            AnyMO.FillProtoClass(new string[] { });
            AnyMO.Invalidate();

            CursorMO = new STable("Cursor");
            WrapIndexy(CursorMO, "postcircumfix:<{ }>", new IxCursorAtKey(),
                    AnyMO.mro_exists_key, AnyMO.mro_delete_key,
                    AnyMO.mro_bind_key);
            WrapIndexy(CursorMO, "postcircumfix:<[ ]>", new IxCursorAtPos(),
                    null, null, AnyMO.mro_bind_pos);
            CursorMO.FillProtoClass(new string[] { });
            CursorMO.Invalidate();

            MatchMO = new STable("Match");
            WrapIndexy(MatchMO, "postcircumfix:<{ }>", new IxCursorAtKey(),
                    AnyMO.mro_exists_key, AnyMO.mro_delete_key,
                    AnyMO.mro_bind_key);
            WrapIndexy(MatchMO, "postcircumfix:<[ ]>", new IxCursorAtPos(),
                    null, null, AnyMO.mro_bind_pos);
            Handler_PandBox(MatchMO, "Str", new CtxMatchStr(), StrMO);
            MatchMO.FillProtoClass(new string[] { });
            MatchMO.Invalidate();

            ScalarMO = new STable("Scalar");
            ScalarMO.FillProtoClass(new string[] { });

            stashes  = new Dictionary<string,P6any>();
            RootO    = GetStash("");
            GlobalO  = GetStash("::GLOBAL");
            ProcessO = GetStash("::PROCESS");
        }

        public static P6any GetStash(string name) {
            P6any o;
            if (stashes.TryGetValue(name, out o))
                return o;
            o = BoxRaw(new Dictionary<string,BValue>(), StashMO);
            o.SetSlot("name", BoxAnyMO(name, StrMO));
            return stashes[name] = o;
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

        public static Variable NewLabelVar(Frame fr, string name) {
            P6opaque dob = new P6opaque(LabelMO);
            fr.MarkSharedChain();
            dob.slots[0] = fr;
            dob.slots[1] = name;
            return NewROScalar(dob);
        }

        private static string DescribeException(int type, Frame tgt,
                string name, object payload) {
            if (type != SubInfo.ON_DIE)
                return "Illegal control operator: " +
                    SubInfo.DescribeControl(type, tgt, name);
            try {
                Variable v = (Variable) payload;
                return v.Fetch().mo.mro_raw_Str.Get(v);
            } catch (Exception ex) {
                return "(stringificiation failed: " + ex + ")";
            }
        }

        // exception processing goes in two stages
        // 1. find the correct place to unwind to, calling CATCH filters
        // 2. unwind, calling LEAVE functions
        public static Frame SearchForHandler(Frame th, int type, Frame tgt,
                int unused, string name, object payload) {
            Frame csr;

            Frame unf = null;
            int unip = 0;

            for (csr = th; ; csr = csr.DynamicCaller()) {
                if (csr == null)
                    throw new Exception("Corrupt call chain");
                if (csr.info == ExitRunloopSI) {
                    // when this exception reaches the outer runloop,
                    // more frames will be added
                    csr.lex0 = DescribeException(type, tgt, name, payload) +
                            DescribeBacktrace(th, csr.caller);
                    return csr;
                }
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
                unip = csr.info.FindControlEnt(csr.ip, type, name);
                if (unip >= 0) {
                    unf = csr;
                    break;
                }
            }

            return Unwind(th, type, unf, unip, payload);
        }

        public static string DescribeBacktrace(Frame from, Frame upto) {
            StringBuilder sb = new StringBuilder();
            while (from != upto) {
                sb.Append(Console.Out.NewLine);
                try {
                    sb.AppendFormat("  at {0} line {1} ({2} @ {3}) {4}",
                            new object[] {
                            from.ExecutingFile(), from.ExecutingLine(),
                            from.info.name, from.ip,
                            Frame.VerboseExceptions ? from.DescribeArgs() : ""
                        });
                } catch (Exception ex) {
                    sb.AppendFormat("  (frame display failed: {0})", ex);
                }
                from = from.DynamicCaller();
            }
            return sb.ToString();
        }

        public static Frame Unwind(Frame th, int type, Frame tf, int tip,
                object td) {
            // LEAVE handlers aren't implemented yet.
            if (type == SubInfo.ON_NEXTDISPATCH) {
                // These are a bit special because there isn't actually a
                // catching frame.
                DispatchEnt de = tf.curDisp.next;
                P6opaque o = td as P6opaque;
                if (de != null) {
                    Variable[] p = tf.pos;
                    VarHash n = tf.named;
                    if (o != null) {
                        p = (Variable[]) o.slots[0];
                        n = o.slots[1] as VarHash;
                    }
                    return de.info.Binder(tf.caller, de.outer, de.ip6, p, n,
                            false, de);
                } else {
                    tf.caller.resultSlot = AnyMO.typeVar;
                    return tf.caller;
                }
            } else if (type == SubInfo.ON_DIE) {
                tf.LexicalBind("$!", (Variable)td);
                td = AnyMO.typeVar;
            }
            tf.ip = tip;
            tf.resultSlot = td;
            return tf;
        }
    }
}
