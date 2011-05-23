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
                vd.Push(Kernel.NewRWScalar(Kernel.AnyMO, Kernel.AnyP));
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
                vd.Push(Kernel.NewRWScalar(Kernel.AnyMO, Kernel.AnyP));
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
            if (!v.mo.HasMRO(type)) {
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
            if (!vl.mo.HasMRO(type))
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
        public byte[] heap;
        public RuntimeUnit[] depends;
        public object[] xref;

        public RuntimeUnit(byte[] heap, RuntimeUnit[] depends, int nx) {
            this.heap = heap;
            this.depends = depends;
            this.xref = new object[nx];
        }

        public int ReadInt(ref int from) {
            uint r = 0;
            r |= ((uint)heap[from++] <<  0);
            r |= ((uint)heap[from++] <<  8);
            r |= ((uint)heap[from++] << 16);
            r |= ((uint)heap[from++] << 24);
            return (int)r;
        }

        public int ReadShort(ref int from) {
            uint r = 0;
            r |= ((uint)heap[from++] <<  0);
            r |= ((uint)heap[from++] <<  8);
            return (int)r;
        }

        public int[] ReadIntArray(ref int from) {
            int[] r = new int[ReadInt(ref from)];
            for (int i = 0; i < r.Length; i++) r[i] = ReadInt(ref from);
            return r;
        }

        public string ReadStr(ref int from) {
            char[] r = new char[ReadShort(ref from)];
            for (int i = 0; i < r.Length; i++) r[i] = (char)ReadShort(ref from);
            return new string(r);
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
        public Dictionary<string, int> dylex;
        public uint dylex_filter; // (32,1) Bloom on hash code
        public int[] lines;
        public STable mo;
        // for inheriting hints
        public SubInfo outer;
        public string name;
        public Dictionary<string, BValue> hints;
        // maybe should be a hint
        public LAD ltm;

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
        public const int SIG_F_COPY       = 4;
        public const int SIG_F_RWTRANS    = 8;
        public const int SIG_F_BINDLIST   = 16;
        public const int SIG_F_INVOCANT   = 8192;

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

        public int FindControlEnt(int ip, int ty, string name) {
            for (int i = 0; i < edata.Length; i+=5) {
                if (ip < edata[i] || ip >= edata[i+1])
                    continue;
                if (ty != edata[i+2])
                    continue;
                if (name != null && (edata[i+4] < 0 || !name.Equals(label_names[edata[i+4]])))
                    continue;
                return edata[i+3];
            }
            return -1;
        }

        private string PName(int rbase) {
            return ((string)sig_r[rbase]) + " in " + name;
        }
        public unsafe Frame Binder(Frame th, Variable[] pos, VarHash named,
                bool quiet) {
            th.pos = pos;
            th.named = named;
            // XXX I don't fully understand how this works, but it's
            // necessary for inferior runloops from here to work.  Critical
            // section blah blah.
            Kernel.SetTopFrame(th);
            int[] ibuf = sig_i;
            if (ibuf == null) return th;
            int posc = 0;
            HashSet<string> namedc = null;
            if (named != null)
                namedc = new HashSet<string>(named.Keys);
            if (ibuf.Length == 0) goto noparams;
            fixed (int* ibase = ibuf) {
            int* ic = ibase;
            int* iend = ic + (ibuf.Length - 2);
            object[] rbuf = sig_r;
            int rc = 0;

            while (ic < iend) {
                int flags = *(ic++);
                int slot  = *(ic++);
                int names = *(ic++);
                int rbase = rc;
                rc += (1 + names);
                if ((flags & SIG_F_HASDEFAULT) != 0) rc++;
                STable type = Kernel.AnyMO;
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
                            goto gotit;
                        }
                    }
                }
                if ((flags & SIG_F_POSITIONAL) != 0 && posc != pos.Length) {
                    src = pos[posc++];
                    goto gotit;
                }
                if ((flags & SIG_F_HASDEFAULT) != 0) {
                    Frame thn = Kernel.GetInferiorRoot()
                        .MakeChild(th, (SubInfo) rbuf[rbase + 1 + names]);
                    src = Kernel.RunInferior(thn);
                    if (src == null)
                        throw new Exception("Improper null return from sub default for " + PName(rbase));
                    goto gotit;
                }
                if ((flags & SIG_F_DEFOUTER) != 0) {
                    Frame f = th;
                    if (outer_topic_key < 0) {
                        src = Kernel.NewROScalar(Kernel.AnyP);
                        goto gotit;
                    }
                    for (int i = 0; i < outer_topic_rank; i++) f = f.outer;
                    src = (Variable)f.GetDynamic(outer_topic_key);
                    goto gotit;
                }
                if ((flags & SIG_F_OPTIONAL) != 0) {
                    src = Kernel.NewROScalar(type.typeObject);
                    goto gotit;
                }
                if (quiet) return null;
                return Kernel.Die(th, "No value for parameter " + PName(rbase));
gotit:
                if ((flags & SIG_F_RWTRANS) != 0) {
                } else {
                    bool islist = ((flags & SIG_F_BINDLIST) != 0);
                    bool rw     = ((flags & SIG_F_READWRITE) != 0) && !islist;

                    // XXX $_ stupidity
                    if (rw && !src.rw)
                        return Kernel.Die(th, "Binding " + PName(rbase) + ", cannot bind read-only value to is rw parameter");
                    // fast path
                    if (rw == src.rw && islist == src.islist) {
                        if (!src.type.HasMRO(type)) {
                            if (quiet) return null;
                            return Kernel.Die(th, "Nominal type check failed in binding " + PName(rbase) + "; got " + src.type.name + ", needed " + type.name);
                        }
                        if (src.whence != null)
                            Kernel.Vivify(src);
                        goto bound;
                    }
                    // rw = false and rhs.rw = true OR
                    // rw = false and islist = false and rhs.islist = true OR
                    // rw = false and islist = true and rhs.islist = false
                    P6any srco = src.Fetch();
                    if (!srco.mo.HasMRO(type)) {
                        if (quiet) return null;
                        return Kernel.Die(th, "Nominal type check failed in binding" + PName(rbase) + "; got " + srco.mo.name + ", needed " + type.name);
                    }
                    src = new SimpleVariable(false, islist, srco.mo, null, srco);
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

            return th;
        }

        public BValue AddHint(string name) {
            if (hints == null)
                hints = new Dictionary<string,BValue>();
            return hints[name] = new BValue(Kernel.NewROScalar(Kernel.AnyP));
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
            uint hash = (uint)(name.GetHashCode() * FILTER_SALT);
            return 1u << (int)(hash >> 27);
        }

        public SubInfo(string name, int[] lines, DynBlockDelegate code,
                SubInfo outer, LAD ltm, int[] edata, string[] label_names,
                int nspill, string[] dylexn, int[] dylexi) {
            this.lines = lines;
            this.code = code;
            this.outer = outer;
            this.ltm = ltm;
            this.name = name;
            this.edata = edata;
            this.label_names = label_names;
            this.nspill = nspill;
            if (dylexn != null) {
                dylex = new Dictionary<string, int>();
                for (int i = 0; i < dylexn.Length; i++) {
                    dylex[dylexn[i]] = dylexi[i];
                    dylex_filter |= FilterForName(dylexn[i]);
                }
            }
            for (int i = 0; i < edata.Length; i += 5)
                if (edata[i+2] == ON_VARLOOKUP && edata[i+4] >= 0)
                    dylex_filter |= FilterForName(label_names[edata[i+4]]);

            SubInfo sc = outer;
            for (outer_topic_rank = 1; sc != null; sc = sc.outer) {
                if (sc.dylex != null &&
                        sc.dylex.TryGetValue("$_", out outer_topic_key))
                    break;
                outer_topic_rank++;
            }
            if (sc == null)
                outer_topic_key = -1;
            self_key = -1;
            if (dylex == null || !dylex.TryGetValue("self", out self_key))
                self_key = -1;
            //Console.WriteLine("{0} {1} {2}", name, outer_topic_rank, outer_topic_key);
        }

        public SubInfo(string name, DynBlockDelegate code) :
            this(name, null, code, null, null, new int[0], null, 0, null, null) { }
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

        public Variable[] pos;
        public VarHash named;

        // after MakeSub, GatherHelper
        public const int SHARED = 1;
        public int flags;

        public Frame(Frame caller_, Frame outer_,
                SubInfo info_) {
            caller = caller_;
            outer = outer_;
            code = info_.code;
            info = info_;
            mo = Kernel.CallFrameMO;
            lexn = (info_.nspill > 0) ? new object[info_.nspill] : null;
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

        public bool TryGetDynamic(string name, uint mask, out object v) {
            v = null;
            if (lex != null && lex.TryGetValue(name, out v))
                return true;
            if ((info.dylex_filter & mask) == 0)
                return false;
            int ix;
            if ((ix = info.FindControlEnt(ip, SubInfo.ON_VARLOOKUP, name)) < 0) {
                if (!info.dylex.TryGetValue(name, out ix))
                    return false;
            }
            v = GetDynamic(ix);
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
                    return Kernel.NewROScalar(Kernel.AnyP);
            }
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

            Frame n = caller.MakeChild(outer, info);
            n = n.info.Binder(n, pos, named, false);

            return n;
        }
    }

    class InvokeCallMethod : InvokeHandler {
        public override Frame Invoke(P6any th, Frame caller,
                Variable[] pos, VarHash named) {
            Variable[] np = new Variable[pos.Length + 1];
            Array.Copy(pos, 0, np, 1, pos.Length);
            np[0] = Kernel.NewROScalar(th);
            return th.InvokeMethod(caller, "INVOKE", np, named);
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

    class CtxListIterator : ContextHandler<VarDeque> {
        public override VarDeque Get(Variable obj) {
            P6opaque d = (P6opaque) obj.Fetch();
            if (!d.IsDefined()) return new VarDeque();
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
        public IxCallMethod(string name) { this.name = name; }
        public override Variable Get(Variable obj, Variable key) {
            return (Variable) Kernel.RunInferior(
                    obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), name,
                        new Variable[] { obj, key }, null));
        }
    }

    class IxAnyAtKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            if (key.islist) {
                return Slice(obj, key);
            }

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return IndexHandler.ViviHash(obj, key);
            throw new NieczaException("Cannot use hash access on an object of type " + os.mo.name);
        }
    }
    class IxAnyAtPos : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            if (key.islist) {
                return Slice(obj, key);
            }

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return IndexHandler.ViviArray(obj, key);
            int ix = (int) key.Fetch().mo.mro_raw_Numeric.Get(key);
            if (ix == 0) return obj;
            throw new NieczaException("Invalid index for non-array");
        }
    }

    class IxCursorAtKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            if (key.islist) {
                return Slice(obj, key);
            }
            P6any o = obj.Fetch();
            if (!o.IsDefined())
                return Kernel.NewROScalar(Kernel.AnyP);

            Cursor os = (Cursor)o;
            return os.GetKey(key.Fetch().mo.mro_raw_Str.Get(key));
        }
    }
    class IxCursorAtPos : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            if (key.islist) {
                return Slice(obj, key);
            }

            P6any o = obj.Fetch();
            if (!o.IsDefined())
                return Kernel.NewROScalar(Kernel.AnyP);

            Cursor os = (Cursor)o;
            return os.GetKey(Utils.N2S(key.Fetch().mo.mro_raw_Numeric.Get(key)));
        }
    }

    class IxHashAtKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            if (key.islist) {
                return Slice(obj, key);
            }

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return IndexHandler.ViviHash(obj, key);
            string ks = key.Fetch().mo.mro_raw_Str.Get(key);
            VarHash h = Kernel.UnboxAny<VarHash>(os);
            Variable r;
            if (h.TryGetValue(ks, out r))
                return r;
            return new SimpleVariable(true, false, Kernel.AnyMO, new HashViviHook(os, ks), Kernel.AnyP);
        }
    }
    class IxHashExistsKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any os = obj.Fetch();
            if (!os.IsDefined()) return Kernel.FalseV;
            string ks = key.Fetch().mo.mro_raw_Str.Get(key);
            VarHash h =
                Kernel.UnboxAny<VarHash>(os);
            return h.ContainsKey(ks) ? Kernel.TrueV : Kernel.FalseV;
        }
    }

    class IxListAtPos : IndexHandler {
        bool extend;
        public IxListAtPos(bool extend) { this.extend = extend; }

        public override Variable Get(Variable obj, Variable key) {
            if (key.islist) {
                return Slice(obj, key);
            }

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return IndexHandler.ViviArray(obj, key);

            P6opaque dos = (P6opaque) os;
            VarDeque items = (VarDeque) dos.slots[0];
            VarDeque rest  = (VarDeque) dos.slots[1];

            P6any ks = key.Fetch();
            if (ks.mo != Kernel.IntMO && ks.mo.HasMRO(Kernel.SubMO)) {
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
                return Kernel.NewROScalar(Kernel.AnyP);
            if (items.Count() <= ix) {
                if (extend) {
                    return new SimpleVariable(true, false, Kernel.AnyMO,
                            new ArrayViviHook(os, ix), Kernel.AnyP);
                } else {
                    return Kernel.NewROScalar(Kernel.AnyP);
                }
            }
            return items[ix];
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

        private static HashSet<string> ModulesStarted;
        private static HashSet<string> ModulesFinished;

        public static Variable BootModule(string name, DynBlockDelegate dgt) {
            if (ModulesStarted == null) ModulesStarted = new HashSet<string>();
            if (ModulesFinished == null) ModulesFinished = new HashSet<string>();
            if (ModulesFinished.Contains(name))
                return NewROScalar(AnyP);
            if (ModulesStarted.Contains(name))
                throw new NieczaException("Recursive module graph detected at " + name + ": " + JoinS(" ", ModulesStarted));
            ModulesStarted.Add(name);
            Variable r = Kernel.RunInferior(Kernel.GetInferiorRoot().
                    MakeChild(null, new SubInfo("boot-" + name, dgt)));
            ModulesFinished.Add(name);
            ModulesStarted.Remove(name);
            return r;
        }

        public static void DoRequire(string name) {
            if (ModulesFinished.Contains(name))
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
            r.SetDynamic(r.info.dylex["$*nextframe"], NewROScalar(th));
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
            Frame n = th.MakeChild((Frame) dyo.slots[0],
                    (SubInfo) dyo.slots[1]);
            n = n.info.Binder(n, Variable.None, null, false);
            n.MarkSharedChain();
            n.lex = new Dictionary<string,object>();
            n.lex["!return"] = null;
            th.resultSlot = n;
            return th;
        }

        private static SubInfo SubInvokeSubSI = new SubInfo("Sub.INVOKE", SubInvokeSubC);
        private static Frame SubInvokeSubC(Frame th) {
            Variable[] post;
            post = new Variable[th.pos.Length - 1];
            Array.Copy(th.pos, 1, post, 0, th.pos.Length - 1);
            return SubMO.mro_INVOKE.Invoke((P6opaque)th.pos[0].Fetch(),
                    th.caller, post, th.named);
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
        public static STable CallFrameMO;
        public static STable CaptureMO;
        public static STable GatherIteratorMO;
        public static STable IterCursorMO;
        public static P6any AnyP;
        public static P6any ArrayP;
        public static P6any EMPTYP;
        public static P6any HashP;
        public static P6any IteratorP;
        public static readonly STable LabelMO;
        public static readonly STable AnyMO;
        public static readonly STable IteratorMO;
        public static readonly STable ScalarMO;
        public static readonly STable StashMO;
        public static readonly STable SubMO;
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
            P6opaque n = new P6opaque(info.mo ?? SubMO);
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
                Frame nth = th.MakeChild(o, si);
                if (nth.info.Binder(nth, dth.pos, dth.named, true) == null) {
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
            Frame nf = root.info.Binder(th.MakeChild(root.outer, root.info),
                    dth.pos, dth.named, false);
            nf.curDisp = root;
            return nf;
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
            if (proto == BoolMO.typeObject)
                return ((bool) (object) v) ? TrueV : FalseV;
            return NewROScalar(new BoxObject<T>(v, ((P6opaque)proto).mo));
        }

        public static void SetBox<T>(P6any obj, T v) {
            ((BoxObject<T>) obj).value = v;
        }

        public static Variable BoxAnyMO<T>(T v, STable proto) {
            if (proto == BoolMO)
                return ((bool) (object) v) ? TrueV : FalseV;
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

        public static Frame NewBoundVar(Frame th, bool ro, bool islist,
                STable type, Variable rhs) {
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
                P6any v = rhs.Fetch();
                if (!v.mo.HasMRO(type))
                    return Kernel.Die(th, "Nominal type check failed in binding; got " + v.mo.name + ", needed " + type.name);
                th.resultSlot = new SimpleVariable(false, islist, v.mo, null, v);
                return th;
            }
            // ro = true and rhw.rw = true OR
            // whence != null
            if (ro) {
                P6any v = rhs.Fetch();
                if (!v.mo.HasMRO(type))
                    return Kernel.Die(th, "Nominal type check failed in binding; got " + v.mo.name + ", needed " + type.name);
                th.resultSlot = new SimpleVariable(false, islist, v.mo, null, rhs.Fetch());
                return th;
            }

            if (!rhs.type.HasMRO(type))
                return Kernel.Die(th, "Nominal type check failed in binding; got " + rhs.type.name + ", needed " + type.name);

            Vivify(rhs);
            th.resultSlot = rhs;
            return th;
        }

        public static Frame Assign(Frame th, Variable lhs, Variable rhs) {
            if (!lhs.islist) {
                if (!lhs.rw) {
                    return Kernel.Die(th, "assigning to readonly value");
                }

                lhs.Store(rhs.Fetch());
                return th;
            }

            return lhs.Fetch().InvokeMethod(th, "LISTSTORE", new Variable[2] { lhs, rhs }, null);

        }

        // ro, not rebindable
        public static Variable NewROScalar(P6any obj) {
            return new SimpleVariable(false, false, obj.mo, null, obj);
        }

        public static Variable NewRWScalar(STable t, P6any obj) {
            return new SimpleVariable(true, false, t, null, obj);
        }

        public static Variable NewTypedScalar(STable t) {
            return new SimpleVariable(true, false, t, null, t.typeObject);
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
                return NewROScalar(AnyP);
            }
        }

        public static void SetStatus(Frame th, string name, Variable v) {
            th = th.caller;
            while (true) {
                string n = th.info.name;
                // Mega-Hack: These functions wrap stuff and should
                // propagate $/
                if (n == "CORE infix:<~~>" || n == "ExitRunloop") {
                    th = th.caller;
                    continue;
                }
                break;
            }
            int ix;
            if (th.info.dylex != null &&
                    th.info.dylex.TryGetValue(name, out ix)) {
                th.SetDynamic(ix, v);
            }
            if (th.lex == null)
                th.lex = new Dictionary<string,object>();
            th.lex[name] = v;
        }

        public static Variable StatusHelper(Frame th, string name, int up) {
            object rt;
            uint m = SubInfo.FilterForName(name);
            while (th != null) {
                if (up <= 0 && th.TryGetDynamic(name, m, out rt)) {
                    return (Variable)rt;
                }
                th = th.outer;
                up--;
            }
            return NewROScalar(AnyP);
        }

        public static Variable DefaultNew(P6any proto, VarHash args) {
            P6opaque n = new P6opaque(((P6opaque)proto).mo);
            STable[] mro = n.mo.mo.mro;

            for (int i = mro.Length - 1; i >= 0; i--) {
                foreach (P6how.AttrInfo a in mro[i].mo.local_attr) {
                    P6any val;
                    Variable vx;
                    if (a.publ && args.TryGetValue(a.name, out vx)) {
                        val = vx.Fetch();
                    } else if (a.init == null) {
                        val = a.type.typeObject;
                    } else {
                        val = RunInferior(a.init.Invoke(GetInferiorRoot(),
                                    Variable.None, null)).Fetch();
                    }
                    n.SetSlot(a.name, NewRWScalar(a.type, val));
                }
            }

            return NewROScalar(n);
        }

        public static Frame PromoteToList(Frame th, Variable v) {
            if (!v.islist) {
                P6opaque lst = new P6opaque(Kernel.ListMO);
                lst.slots[0 /*items*/] = new VarDeque(new Variable[] { v });
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
                Frame th = new Frame(null, null, IF_SI);
                th.lex0 = inq;
                P6opaque thunk = new P6opaque(Kernel.GatherIteratorMO);
                th.lex = new Dictionary<string,object>();
                th.lex["!return"] = null;
                thunk.slots[0] = NewRWScalar(AnyMO, th);
                thunk.slots[1] = NewRWScalar(AnyMO, AnyP);
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
                    return NewROScalar(AnyP);
                }
                goto slow;
            }
            return itemsl[0];

slow:
            return RunInferior(lst.Fetch().InvokeMethod(
                        GetInferiorRoot(), "head", new Variable[] {lst}, null));
        }

        // TODO: Runtime access to grafts
        public static void CreatePath(string[] path) {
            P6any cursor = RootO;
            foreach (string n in path)
                cursor = PackageLookup(cursor, n + "::").v.Fetch();
        }

        public static BValue GetVar(string[] path) {
            P6any cursor = RootO;
            for (int i = 0; i < path.Length - 1; i++) {
                cursor = PackageLookup(cursor, path[i] + "::").v.Fetch();
            }
            return PackageLookup(cursor, path[path.Length - 1]);
        }

        public static BValue PackageLookup(P6any parent, string name) {
            Dictionary<string,BValue> stash =
                UnboxAny<Dictionary<string,BValue>>(parent);
            BValue v;

            if (stash.TryGetValue(name, out v)) {
                return v;
            } else if (name.EndsWith("::")) {
                Dictionary<string,BValue> newstash =
                    new Dictionary<string,BValue>();
                newstash["PARENT::"] = new BValue(NewROScalar(parent));
                return (stash[name] = new BValue(BoxAny<Dictionary<string,BValue>>(newstash, StashP)));
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
                return (stash[name] = new BValue(NewRWScalar(AnyMO, AnyP)));
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
            Frame n = th.MakeChild(null, IRSI);
            n = n.info.Binder(n, pcl, null, false);
            return n;
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
            n.typeObject = new P6opaque(n);
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
                        new Frame(null, null, ExitRunloopSI) :
                        l.MakeChild(null, ExitRunloopSI)));
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

        public static P6any RootO;
        // used as the fallbacks for $*FOO
        public static P6any GlobalO;
        public static P6any ProcessO;

        static Kernel() {
            PhaserBanks = new VarDeque[] { new VarDeque(), new VarDeque(),
                new VarDeque() };

            SubMO = new STable("Sub");
            SubInvokeSubSI.param1 = new InvokeSub();
            SubMO.FillProtoClass(new string[] { "outer", "info" });
            SubMO.AddMethod(0, "INVOKE", MakeSub(SubInvokeSubSI, null));
            SubMO.Invalidate();

            LabelMO = new STable("Label");
            LabelMO.FillProtoClass(new string[] { "target", "name" });
            LabelMO.Invalidate();

            BoolMO = new STable("Bool");
            Handler_Vonly(BoolMO, "Bool", new CtxReturnSelf(),
                    new CtxJustUnbox<bool>(false));
            BoolMO.FillProtoClass(new string[] { });
            BoolMO.Invalidate();
            TrueV  = NewROScalar(BoxRaw<bool>(true,  BoolMO));
            FalseV = NewROScalar(BoxRaw<bool>(false, BoolMO));

            StrMO = new STable("Str");
            Handler_Vonly(StrMO, "Str", new CtxReturnSelf(),
                    new CtxJustUnbox<string>(""));
            Handler_PandBox(StrMO, "Bool", new CtxStrBool(), BoolMO);
            StrMO.FillProtoClass(new string[] { });
            StrMO.Invalidate();

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
            StashMO.FillProtoClass(new string[] { });
            StashP = new P6opaque(StashMO);

            ParcelMO = new STable("Parcel");
            Handler_PandBox(ParcelMO, "iterator", new CtxParcelIterator(),
                    IteratorMO);
            Handler_Vonly(ParcelMO, "list", new CtxParcelList(), null);
            ParcelMO.FillProtoClass(new string[] { });
            ParcelMO.Invalidate();

            ArrayMO = new STable("Array");
            ArrayMO.FillProtoClass(new string[] { "items", "rest" });
            WrapHandler1(ArrayMO, "at-pos", new IxListAtPos(true));
            ArrayMO.Invalidate();

            ListMO = new STable("List");
            WrapHandler1(ListMO, "at-pos", new IxListAtPos(false));
            Handler_PandBox(ListMO, "iterator", new CtxListIterator(),
                    IteratorMO);
            Handler_PandBox(ListMO, "Bool", new CtxListBool(), BoolMO);
            Handler_PandBoxInty(ListMO, "Numeric", new CtxListNum());
            Handler_Vonly(ListMO, "list", new CtxReturnSelfList(), null);
            ListMO.FillProtoClass(new string[] { "items", "rest" });
            ListMO.Invalidate();

            HashMO = new STable("Hash");
            WrapHandler1(HashMO, "exists-key", new IxHashExistsKey());
            WrapHandler1(HashMO, "at-key", new IxHashAtKey());
            Handler_PandBox(HashMO, "iterator", new CtxHashIterator(), IteratorMO);
            Handler_PandBox(HashMO, "Bool", new CtxHashBool(), BoolMO);
            Handler_Vonly(HashMO, "hash", new CtxReturnSelfList(), null);
            HashMO.FillProtoClass(new string[] { });
            HashMO.Invalidate();

            AnyMO = new STable("Any");
            WrapHandler1(AnyMO, "at-key", new IxAnyAtKey());
            WrapHandler1(AnyMO, "at-pos", new IxAnyAtPos());
            Handler_Vonly(AnyMO, "list", new CtxAnyList(), null);
            AnyMO.FillProtoClass(new string[] { });
            AnyMO.Invalidate();

            CursorMO = new STable("Cursor");
            WrapHandler1(CursorMO, "at-key", new IxCursorAtKey());
            WrapHandler1(CursorMO, "at-pos", new IxCursorAtPos());
            CursorMO.FillProtoClass(new string[] { });
            CursorMO.Invalidate();

            MatchMO = new STable("Match");
            WrapHandler1(MatchMO, "at-key", new IxCursorAtKey());
            WrapHandler1(MatchMO, "at-pos", new IxCursorAtPos());
            Handler_PandBox(MatchMO, "Str", new CtxMatchStr(), StrMO);
            MatchMO.FillProtoClass(new string[] { });
            MatchMO.Invalidate();

            ScalarMO = new STable("Scalar");
            ScalarMO.FillProtoClass(new string[] { });

            RootO = BoxRaw(new Dictionary<string,BValue>(), StashMO);
            GlobalO = PackageLookup(RootO, "GLOBAL::").v.Fetch();
            ProcessO = PackageLookup(RootO, "PROCESS::").v.Fetch();
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
                    sb.AppendFormat("  at {0} line {1} ({2} @ {3})",
                            new object[] {
                            from.ExecutingFile(), from.ExecutingLine(),
                            from.info.name, from.ip });
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
                    tf = tf.caller.MakeChild(de.outer, de.info);
                    if (o != null) {
                        p = (Variable[]) o.slots[0];
                        n = o.slots[1] as VarHash;
                    }
                    tf = tf.info.Binder(tf, p, n, false);
                    tf.curDisp = de;
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
}
