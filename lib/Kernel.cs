using System;
using System.Text;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Threading;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.IO;
using Niecza.CLRBackend;
using Niecza.Serialization;

namespace Niecza {
    // We like to reuse continuation objects for speed - every function only
    // creates one kind of continuation, but tweaks a field for exact return
    // point.  As such, call frames and continuations are in 1:1 correspondence
    // and are unified.  Functions take a current continuation and return a new
    // continuation; we tail recurse with trampolines.

    // Used by DynFrame to plug in code
    public delegate Frame DynBlockDelegate(Frame frame);

    public sealed class DispatchEnt : IFreeze {
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
        public DispatchEnt(DispatchEnt next) { this.next = next; }

        void IFreeze.Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.DispatchEnt);
            fb.ObjRef(next);
            fb.ObjRef(info);
            fb.ObjRef(outer);
            fb.ObjRef(ip6);
        }
        internal static DispatchEnt Thaw(ThawBuffer tb) {
            DispatchEnt de = new DispatchEnt();
            tb.Register(de);
            de.next = (DispatchEnt) tb.ObjRef();
            de.info = (SubInfo) tb.ObjRef();
            de.outer = (Frame) tb.ObjRef();
            de.ip6 = (P6any) tb.ObjRef();
            return de;
        }
    }

    // A Variable is the meaning of function arguments, of any subexpression
    // except the targets of := and ::=.

    public abstract class Variable : IFreeze {
        public const int RO   = 0;
        public const int RW   = 1;
        public const int LIST = 2;

        public virtual int Mode { get { return (this is ListVariable) ? LIST : (this is P6any) ? RO : RW; } }
        public bool Rw { get { return !(this is P6any); } }
        public bool List { get { return this is ListVariable; } }
        public virtual void Vivify() { }
        public virtual Variable Assign(Variable inp) {
            Store(inp.Fetch());
            return this;
        }
        public virtual Variable AssignO(P6any inp, bool listishly) {
            Store(inp);
            return this;
        }

        public abstract P6any Fetch();
        public abstract void Store(P6any v);

        public virtual Variable GetVar() {
            return Kernel.BoxAnyMO<Variable>(this, Compartment.Top.ScalarMO);
        }

        public abstract void Freeze(FreezeBuffer fb);

        // note: callers need to make sure type is set up properly if null
        public Variable() { }

        [Immutable]
        public static readonly Variable[] None = new Variable[0];
    }

    public abstract class ViviHook : IFreeze {
        public abstract void Freeze(FreezeBuffer fb);
        public abstract void Do(Variable toviv);
    }

    public class SubViviHook : ViviHook {
        P6any sub;
        public SubViviHook(P6any sub) { this.sub = sub; }
        public override void Do(Variable toviv) {
            Kernel.RunInferior(sub.Invoke(Kernel.GetInferiorRoot(),
                new Variable[] { toviv }, null));
        }
        public override void Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.SubViviHook);
            fb.ObjRef(sub);
        }
        internal static object Thaw(ThawBuffer tb) {
            var n = new SubViviHook(null);
            tb.Register(n);
            n.sub = (P6any) tb.ObjRef();
            return n;
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
        public override void Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.HashViviHook);
            fb.ObjRef(hash);
            fb.String(key);
        }
        internal static IFreeze Thaw(ThawBuffer tb) {
            var n = new HashViviHook(null, null);
            tb.Register(n);
            n.hash = (P6any) tb.ObjRef();
            n.key = tb.String();
            return n;
        }
    }

    public class NewHashViviHook : ViviHook {
        Variable hashv;
        string key;
        public NewHashViviHook(Variable hashv, string key) { this.hashv = hashv; this.key = key; }
        public override void Do(Variable toviv) {
            VarHash rh = new VarHash();
            rh[key] = toviv;
            hashv.Store(Kernel.BoxRaw(rh, Compartment.Top.HashMO));
        }
        public override void Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.NewHashViviHook);
            fb.ObjRef(hashv);
            fb.String(key);
        }
        internal static IFreeze Thaw(ThawBuffer tb) {
            var n = new NewHashViviHook(null, null);
            tb.Register(n);
            n.hashv = (Variable) tb.ObjRef();
            n.key = tb.String();
            return n;
        }
    }

    public class ArrayViviHook : ViviHook {
        P6any ary;
        int key;
        public ArrayViviHook(P6any ary, int key) { this.ary = ary; this.key = key; }
        public override void Do(Variable toviv) {
            VarDeque vd = (VarDeque) ary.GetSlot(Compartment.Top.ListMO, "$!items");
            while (vd.Count() <= key)
                vd.Push(Kernel.NewTypedScalar(null));
            vd[key] = toviv;
        }
        public override void Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.ArrayViviHook);
            fb.ObjRef(ary);
            fb.Int(key);
        }
        internal static IFreeze Thaw(ThawBuffer tb) {
            var n = new ArrayViviHook(null, 0);
            tb.Register(n);
            n.ary = (P6any) tb.ObjRef();
            n.key = tb.Int();
            return n;
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
            P6opaque d = new P6opaque(Compartment.Top.ArrayMO);
            d.slots[0] = vd;
            d.slots[1] = new VarDeque();
            ary.Store(d);
        }
        public override void Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.NewArrayViviHook);
            fb.ObjRef(ary);
            fb.Int(key);
        }
        internal static IFreeze Thaw(ThawBuffer tb) {
            var n = new NewArrayViviHook(null, 0);
            tb.Register(n);
            n.ary = (Variable) tb.ObjRef();
            n.key = tb.Int();
            return n;
        }
    }

    public sealed class ListVariable: Variable {
        P6any val;

        public ListVariable(P6any val) { this.val = val; }

        public override P6any  Fetch()       { return val; }
        public override Variable Assign(Variable inp) {
            val.mo.mro_LISTSTORE.Get(this, inp);
            return this;
        }
        public override Variable AssignO(P6any inp, bool listishly) {
            val.mo.mro_LISTSTORE.Get(this, listishly ?
                    (Variable)new ListVariable(inp) : inp);
            return this;
        }
        public override void Store(P6any v)  {
            throw new NieczaException("Writing to readonly scalar");
        }
        public override void Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.ListVariable);
            fb.ObjRef(val);
        }
        internal static ListVariable Thaw(ThawBuffer tb) {
            ListVariable n = new ListVariable(null);
            tb.Register(n);
            n.val = (P6any) tb.ObjRef();
            return n;
        }
    }

    public sealed class RWVariable: Variable {
        STable type; // null for Any/Mu variables, and roish
        P6any val;
        ViviHook whence;

        public RWVariable(STable type, ViviHook whence, P6any val) {
            this.val = val; this.whence = whence;
            this.type = type;
        }

        public override P6any  Fetch()       { return val; }
        public override Variable Assign(Variable inp) {
            Store(inp.Fetch());
            return this;
        }
        public override Variable AssignO(P6any inp, bool listishly) {
            Store(inp);
            return this;
        }
        public override void Store(P6any v)  {
            if (v == Compartment.Top.NilP) {
                v = type == null ? Compartment.Top.AnyP : type.initObj;
            }
            if (type != null && !v.Does(type)) {
                throw new NieczaException("Nominal type check failed for scalar store; got " + v.mo.name + ", needed " + type.name + " or subtype");
            }
            if (whence != null) {
                ViviHook vh = whence;
                whence = null;
                vh.Do(this);
            }
            val = v;
        }

        public override void Vivify() {
            ViviHook w = whence;
            whence = null;
            if (w != null) w.Do(this);
        }

        public override void Freeze(FreezeBuffer fb) {
            int code = ((int)SerializationCode.RWVariable) +
                (whence != null ? 1 : 0);
            fb.Byte((byte)code);

            if (whence != null) fb.ObjRef(whence);
            fb.ObjRef(type);
            fb.ObjRef(val);
        }
        internal static RWVariable Thaw(ThawBuffer tb, int subcode) {
            RWVariable n = new RWVariable(null, null, null);
            tb.Register(n);
            if (subcode != 0) n.whence = (ViviHook) tb.ObjRef();
            n.type = (STable) tb.ObjRef();
            n.val = (P6any) tb.ObjRef();
            return n;
        }
    }

    public sealed class TiedVariable: Variable {
        P6any fetch;
        P6any store;
        ViviHook whence;

        private TiedVariable() { }
        public TiedVariable(P6any whsub, P6any fetch, P6any store) {
            this.fetch = fetch;
            this.store = store;
            this.whence = whsub.IsDefined() ? new SubViviHook(whsub) : null;
        }

        public override P6any Fetch() {
            Variable vr = Kernel.RunInferior(fetch.Invoke(
                Kernel.GetInferiorRoot(), new [] {Compartment.Top.AnyP}, null));
            return vr.Fetch();
        }

        public override void Store(P6any v) {
            if (whence != null) {
                ViviHook vh = whence;
                whence = null;
                vh.Do(this);
            }
            Kernel.RunInferior(store.Invoke(Kernel.GetInferiorRoot(),
                new [] { Compartment.Top.AnyP, v }, null));
        }

        public override void Vivify() {
            ViviHook w = whence;
            whence = null;
            if (w != null) w.Do(this);
        }
        public override void Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.TiedVariable);
            fb.ObjRef(fetch);
            fb.ObjRef(store);
            fb.ObjRef(whence);
        }
        internal static TiedVariable Thaw(ThawBuffer tb) {
            TiedVariable n = new TiedVariable();
            tb.Register(n);
            n.fetch  = (P6any)    tb.ObjRef();
            n.store  = (P6any)    tb.ObjRef();
            n.whence = (ViviHook) tb.ObjRef();
            return n;
        }
    }

    // Used to make Variable sharing explicit in some cases
    public class StashEnt : IFreeze {
        public Variable v;
        public string   file;
        public int      line;
        public bool     constant;

        public void Bind(Variable v) {
            if (constant)
                throw new NieczaException("Binding of constant common variable");
            this.v = v;
        }

        void IFreeze.Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.StashEnt);
            fb.Byte((byte)(constant ? 1 : 0));
            fb.ObjRef(v);
            fb.String(file);
            fb.Int(line);
        }

        internal static StashEnt Thaw(ThawBuffer tb) {
            StashEnt r = new StashEnt();
            tb.Register(r);
            r.constant = tb.Byte() != 0;
            r.v = (Variable)tb.ObjRef();
            r.file = tb.String();
            r.line = tb.Int();
            return r;
        }
    }

    // We need to isolate the compilations of different modules from
    // each other, which is accomplished by a stack of isolation containers.
    // Each global variable in niecza must be assured to be incapable of
    // compromising the isolation.  To simplify this, four restricted
    // contracts are offered:
    //
    // [Immutable] fields are never changed, so they cannot act as a side
    // channel.  This is assumed for initonly fields of primitive type or
    // some well-known immutable types.  For other cases, such as arrays,
    // that are not manifestly immutable, this attribute can be used to mark
    // them.
    //
    // [TrueGlobal] fields can be changed during execution, but are not
    // changed under the direct control of Perl 6 code.  They may leak some
    // information such as random number seeds.  They may NOT point at Perl 6
    // level objects.
    //
    // [CompartmentGlobal] fields are unrestricted in usage, but are always
    // automatically saved and restored when manipulating the container stack.
    //
    // [CORESaved] fields are the same as CompartmentGlobal but are additionally
    // saved in CORE.ser.

    [AttributeUsage(AttributeTargets.Field)]
    class CORESavedAttribute : Attribute { }
    [AttributeUsage(AttributeTargets.Field)]
    class ImmutableAttribute : Attribute { }
    [AttributeUsage(AttributeTargets.Field)]
    class CompartmentGlobalAttribute : Attribute { }
    [AttributeUsage(AttributeTargets.Field)]
    class TrueGlobalAttribute : Attribute { }

    // Boxes all per-dll state required by the code generator
    sealed class EmitUnit {
        // This is only used during GenerateCode calls, which cannot
        // call back into Perl 6 code, so it doesn't need to be
        // containerized, or even saved
        [ThreadStatic]
        internal static EmitUnit Current;

        public AssemblyBuilder asm_builder;
        public ModuleBuilder mod_builder;
        public TypeBuilder type_builder;
        public int nextid;
        public Dictionary<object, FieldInfo> constants;
        public NamProcessor np;
        Dictionary<string, CpsOp> val_constants;

        List<NamProcessor> fill = new List<NamProcessor>();
        string dll_name;
        int funique;

        public EmitUnit(string uname, string asm_name, string dll_name, bool is_mainish) {

            if (Config.CGForceSave && dll_name == null)
                dll_name = asm_name + ".exe";
            this.dll_name = dll_name;
            asm_builder = AppDomain.CurrentDomain.DefineDynamicAssembly(
                    new AssemblyName(asm_name),
                    (dll_name == null ? AssemblyBuilderAccess.Run :
                        AssemblyBuilderAccess.RunAndSave),
                    Backend.obj_dir);

            mod_builder = dll_name == null ?
                asm_builder.DefineDynamicModule(asm_name) :
                asm_builder.DefineDynamicModule(asm_name, dll_name);

            type_builder = mod_builder.DefineType(asm_name,
                    TypeAttributes.Public | TypeAttributes.Sealed |
                    TypeAttributes.Class | TypeAttributes.BeforeFieldInit);

            if (is_mainish) {
                var mainb = type_builder.DefineMethod("Main",
                        MethodAttributes.Static | MethodAttributes.Public,
                        typeof(void), new Type[] { typeof(string[]) });
                var il = mainb.GetILGenerator();

                il.Emit(OpCodes.Ldstr, uname);
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Call, typeof(Kernel).GetMethod("MainHandler"));
                il.Emit(OpCodes.Ret);

                asm_builder.SetEntryPoint(mainb);
            }

            constants = new Dictionary<object,FieldInfo>(new IdentityComparer());
            val_constants = new Dictionary<string,CpsOp>();
        }

        public void CgSub(SubInfo sub, bool erase) {
            if (sub.code != RuntimeUnit.JitCompileSub && sub.nam_str == null)
                return; // not defined by Perl 6?
            EmitUnit oc = Current;
            if (Config.CGVerbose > 0)
                Console.WriteLine("generating code for: {0} ({1:X})",
                        sub.name, sub.GetHashCode());
            Current = this;
            try {
                np = new NamProcessor(new CpsBuilder(this,
                            "C" + fill.Count + sub.name, true), sub);
                np.MakeBody(Reader.Read(sub.nam_str, sub.nam_refs));
                fill.Add(np);
            } finally {
                np = null;
                Current = oc;
            }
            if (erase) {
                sub.nam_str = null;
                sub.nam_refs = null;
            }
        }

        public Type Finish(out object constTable) {
            var type = type_builder.CreateType();
            constTable = Activator.CreateInstance(type);

            if (dll_name != null)
                asm_builder.Save(dll_name);
            foreach (NamProcessor th in fill)
                th.FillSubInfo(type, constTable);

            var fields = new Dictionary<string,FieldInfo>();
            foreach (FieldInfo fi in type.GetFields())
                fields[fi.Name] = fi;
            var rconstants = new Dictionary<object,FieldInfo>();
            foreach (var kv in constants)
                rconstants[kv.Key] = fields[kv.Value.Name];
            constants = rconstants;

            return type;
        }

        internal CpsOp TypeConstant(STable s) {
            return RefConstant(s == null ? "" : s.name, "MO", s, Tokens.STable);
        }
        internal CpsOp TypeConstantP(STable s) {
            return RefConstant(s.name, "P", s.typeObj, Tokens.P6any);
        }
        internal CpsOp SubConstant(SubInfo s) {
            return RefConstant(s == null ? "" : s.name, "S", s, Tokens.SubInfo);
        }
        internal CpsOp FrameConstant(Frame s) {
            return RefConstant(s.info.name, "F", s, Tokens.Frame);
        }

        internal CpsOp RefConstant(string n1, string n2, object val, Type nty) {
            if (val == null)
                return CpsOp.Null(nty);
            FieldInfo fi;
            if (!constants.TryGetValue(val, out fi))
                constants[val] = fi = NewField(n1, n2, nty ?? val.GetType());
            return CpsOp.IsConst(CpsOp.GetConst(fi));
        }

        internal CpsOp ValConstant(string key, object val) {
            CpsOp r;
            if (!val_constants.TryGetValue(key, out r))
                val_constants[key] = r = RefConstant(key, "", val, val is Variable ? typeof(Variable) : null);
            return r;
        }

        internal CpsOp VarConstStr(string s) {
            return ValConstant('S' + s, Builtins.MakeStr(s));
        }

        internal CpsOp VarConstNum(double n) {
            char[] c = new char[5];
            c[0] = 'N';
            long b = BitConverter.DoubleToInt64Bits(n);
            c[1] = (char)(b);
            c[2] = (char)(b >> 16);
            c[3] = (char)(b >> 32);
            c[4] = (char)(b >> 48);
            return ValConstant(new string(c), Builtins.MakeFloat(n));
        }

        public static Variable ExactNum(int numbase, string digits) {
            BigInteger num = BigInteger.Zero;
            BigInteger den = BigInteger.Zero;

            bool frac = false;
            bool neg = false;

            foreach (char d in digits) {
                if (d == '-') neg = true;
                if (d == '_') continue;
                if (d == '/') {
                    if (frac)
                        throw new Exception("two slashes in " + digits);
                    frac = true;
                    continue;
                }
                int digval;
                if (d >= '0' && d <= '9') { digval = d - '0'; }
                else if (d >= 'a' && d <= 'z') { digval = d + 10 - 'a'; }
                else if (d >= 'A' && d <= 'Z') { digval = d + 10 - 'A'; }
                else { throw new Exception("invalid digit in " + digits); }

                if (digval >= numbase) { throw new Exception("out of range digit in " + digits); }

                if (frac) {
                    den *= numbase;
                    den += digval;
                } else {
                    num *= numbase;
                    num += digval;
                }
            }

            if (neg) num = -num;

            if (num == BigInteger.Zero && den.Sign > 0)
                den = BigInteger.One;
            if (den > BigInteger.One) {
                BigInteger g = BigInteger.GreatestCommonDivisor(num, den);
                if (g != BigInteger.One) {
                    num /= g;
                    den /= g;
                }
            }

            ulong sden;
            if (!den.AsUInt64(out sden)) {
                return Builtins.MakeFloat((double)num / (double)den);
            }

            if (sden == 0)
                return Builtins.MakeInt(num);

            return Builtins.MakeFixRat(num, sden);
        }

        internal CpsOp VarConstExact(int bas, string digs) {
            return ValConstant("X" + bas + ',' + digs, ExactNum(bas, digs));
        }

        internal CpsOp CCConst(int[] cc) {
            StringBuilder code = new StringBuilder("CC");
            foreach (int x in cc) {
                code.Append((char)x);
                code.Append((char)(x>>16));
            }
            return ValConstant(code.ToString(), new CC(cc));
        }

        internal CpsOp StringListConst(string[] sl) {
            StringBuilder code = new StringBuilder("LS");
            foreach (string s in sl) {
                code.Append((char)(s.Length >> 16));
                code.Append((char)(s.Length));
                code.Append(s);
            }
            return ValConstant(code.ToString(), sl);
        }

        internal CpsOp CCListConst(int[][] ccl) {
            StringBuilder code = new StringBuilder("LC");
            foreach (int[] cc in ccl) {
                code.Append((char)(cc.Length >> 16));
                code.Append((char)(cc.Length));
                foreach (int x in cc) {
                    code.Append((char)x);
                    code.Append((char)(x>>16));
                }
            }
            CC[] buf = new CC[ccl.Length];
            for (int i = 0; i < buf.Length; i++) buf[i] = new CC(ccl[i]);
            return ValConstant(code.ToString(), buf);
        }

        internal FieldBuilder NewField(string n1, string n2, Type ty) {
            char[] buf = new char[20];
            // add unique-id
            int wp = 10;
            int ac = funique++;
            while (ac > 0) {
                int d = ac % 36;
                ac /= 36;
                buf[--wp] = d >= 10 ? (char)(d + 87) : (char)(d + 48);
            }
            int rp = wp;
            wp = 0;
            while (rp != 10) { buf[wp++] = buf[rp++]; }
            buf[wp++] = '_';
            // and names; sanitized
            foreach (char ch in n1) {
                if (wp == 20) break;
                if (ch >= ' ' && ch <= '~') // yes this is overkill
                    buf[wp++] = ch;
            }
            foreach (char ch in n2) {
                if (wp == 20) break;
                if (ch >= ' ' && ch <= '~' && wp < 20)
                    buf[wp++] = ch;
            }

            return type_builder.DefineField(new string(buf, 0, wp),
                    ty, FieldAttributes.Public);
        }

    }

    class IdentityComparer : IEqualityComparer<object> {
        public int GetHashCode(object o) {
            return RuntimeHelpers.GetHashCode(o);
        }
        public new bool Equals(object a, object b) {
            return a == b;
        }
    }

    // There are two kinds of units.  Primary units have their own hash of
    // globals, assembly (if saved), and save file; subordinate units share
    // those of the master.
    public sealed class RuntimeUnit : IFreeze {
        public string name, filename, source, asm_name, dll_name;

        public HashSet<RuntimeUnit> depended_units;
        public List<RuntimeUnit> subordinates = new List<RuntimeUnit>();
        public RuntimeUnit owner;

        public Dictionary<string, StashEnt> globals;
        public SubInfo mainline, bottom;
        public List<SubInfo> our_subs;
        public bool is_mainish;

        // note: type is only set to a non-null value if the type is a
        // member of a saved assembly; thus type being non-null implies
        // a significance to freezing and thawing the type
        public Type type;
        public object constTable;
        public Dictionary<object, FieldInfo> constants;

        public bool inited = false;

        // used during construction only
        public List<KeyValuePair<int,STable>> stubbed_stashes;
        public int nextid;

        private RuntimeUnit() { }

        public RuntimeUnit(string name, string filename, string source,
                bool main, bool runnow) {
            this.name = name;
            this.filename = filename;
            this.source = source;
            this.depended_units = new HashSet<RuntimeUnit>();
            this.stubbed_stashes = new List<KeyValuePair<int,STable>>();
            this.is_mainish = main;
            if (name == "CORE")
                Kernel.CreateBasicTypes();

            this.asm_name = Backend.prefix + name.Replace("::", ".");
            this.dll_name = asm_name + (main ? ".exe" : ".dll");
            our_subs = new List<SubInfo>();
        }

        internal void SaveSubs(EmitUnit eu, bool erase) {
            foreach (SubInfo z in our_subs)
                eu.CgSub(z, erase);
            foreach (RuntimeUnit zu in subordinates)
                foreach (SubInfo z in zu.our_subs)
                    eu.CgSub(z, erase);
        }

        internal static Frame JitCompileSub(Frame th) {
            if (th.info.code != JitCompileSub) {
                th.code = th.info.code;
                th.EnsureSpills(th.info.nspill);
                return th;
            }
            if (th.info.prototype != null &&
                    th.info.prototype.code != JitCompileSub) {
                th.info.code = th.info.prototype.code;
                th.info.nspill = th.info.prototype.nspill;

                th.code = th.info.code;
                th.EnsureSpills(th.info.nspill);
                return th;
            }

            SubInfo sub = th.info.prototype ?? th.info;

            if (Config.CGVerbose > 0)
                Console.WriteLine("Generating code for {0} now because it's about to be run", sub.name);

            EmitUnit eu = new EmitUnit(null, "Anon." + Interlocked.Increment(
                        ref anon_id), null, false);
            eu.CgSub(sub, false);
            object jit_consts;
            SetConstants(eu.Finish(out jit_consts), jit_consts, eu.constants);

            th.code = th.info.code = sub.code;
            th.info.nspill = sub.nspill;
            th.EnsureSpills(th.info.nspill);

            return th;
        }

        [CompartmentGlobal]
        internal static ObjectRegistry reg;

        // note that after Save the unit is _not_ run; the compartment
        // is immediately discarded!
        public void Save() {
            EmitUnit eu = new EmitUnit(name, asm_name, dll_name, is_mainish);

            SaveSubs(eu, !Config.KeepIL);

            type = eu.Finish(out constTable);
            constants = eu.constants;
            // all co-saved units must remember that their code is here
            foreach (RuntimeUnit zu in subordinates)
                zu.type = type;

            reg.SaveUnit(name, this);
        }

        internal void SetConstants() { SetConstants(type, constTable, constants); }
        static void SetConstants(Type ty, object ctab, Dictionary<object,FieldInfo> consts) {
            if (ty != null) {
                foreach (KeyValuePair<object, FieldInfo> kv in consts)
                    kv.Value.SetValue(ctab, kv.Key);
            }
        }

        [TrueGlobal] static int anon_id;

        // This is called when compiling a unit which will be run without
        // saving _only_
        public void PrepareEval() {
            EmitUnit eu = new EmitUnit(name, "Anon." + Interlocked.Increment(
                        ref anon_id) + "." + asm_name, null, false);
            SaveSubs(eu, false);

            object jit_consts;
            SetConstants(eu.Finish(out jit_consts), jit_consts, eu.constants);
        }

        internal void RunMainline() {
            RuntimeUnit csr = this;
            Builtins.setting_path = new Dictionary<string,SubInfo>();
            while (csr.mainline.outer != null) {
                RuntimeUnit o = csr.mainline.outer.unit;
                Builtins.setting_path[o.name] = csr.mainline;
                csr = o;
            }

            Kernel.RunInferior(Kernel.GetInferiorRoot().
                    MakeChild(null, csr.mainline, Compartment.Top.AnyP));
        }

        internal string LinkUnit(RuntimeUnit other) {
            foreach (RuntimeUnit third in other.depended_units)
                depended_units.Add(third);

            foreach (KeyValuePair<string, StashEnt> gm in other.globals) {
                StashEnt ose;
                StashEnt nse = gm.Value;
                string who = gm.Key.Substring(1, (int)gm.Key[0]);
                string name = gm.Key.Substring(1 + (int)gm.Key[0]);
                string err = null;
                if (globals.TryGetValue(gm.Key, out ose))
                    err = NsMerge(who, name, ref nse, ose);
                if (err != null) return err;
                globals[gm.Key] = nse;
            }

            Compartment.Top.check.RunNew();
            Compartment.Top.init.RunNew();
            Compartment.Top.end.RunNew();

            return null;
        }

        public static Variable MakeAppropriateVar(string name) {
            if (name.Length >= 1 && name[0] == '@')
                return Kernel.CreateArray();
            if (name.Length >= 1 && name[0] == '%')
                return Kernel.CreateHash();
            return Kernel.NewTypedScalar(null);
        }

        static bool IsEmptyAggr(Variable v) {
            if (!v.List) return false;
            P6any p = v.Fetch();
            if (p.mo == Compartment.Top.ArrayMO) {
                return ((VarDeque)p.GetSlot(Compartment.Top.ListMO, "$!items")).Count() == 0 &&
                    ((VarDeque)p.GetSlot(Compartment.Top.ListMO, "$!rest")).Count() == 0;
            }
            else if (p.mo == Compartment.Top.HashMO) {
                return Kernel.UnboxAny<VarHash>(p).Count == 0;
            }
            else {
                return false;
            }
        }

        public static string NsMerge(string who, string name,
                ref StashEnt nse, StashEnt ose) {

            P6any nseo = nse.v.Fetch();
            P6any oseo = ose.v.Fetch();
            bool  nseod = nseo.IsDefined();
            bool  oseod = oseo.IsDefined();
            int omode = ose.v.Mode;
            int nmode = nse.v.Mode;
            // lowest priority are empty common symbols
            if (nmode == Variable.RW && !nseod || !nse.constant && IsEmptyAggr(nse.v)) {
                nse = ose;
                return null;
            }
            if (omode == Variable.RW && !oseod || !ose.constant && IsEmptyAggr(ose.v)) {
                return null;
            }

            // no conflict if items are identical
            if (ose.v == nse.v || omode != Variable.RW &&
                    omode == nmode && oseo == nseo) {
                nse = ose;
                return null;
            }

            // no conflict if items are simple packages with the same who
            // note that this will effectively change a constant, but these
            // packages are observationally identical barring &infix:<does>
            // (mis)use
            if (ose.constant && !oseod && nse.constant && !nseod &&
                    (oseo.mo.mo.type == P6how.PACKAGE || nseo.mo.mo.type == P6how.PACKAGE) &&
                    oseo.mo.who.Isa(Compartment.Top.StashMO) &&
                    nseo.mo.who.Isa(Compartment.Top.StashMO) &&
                    Kernel.UnboxAny<string>(oseo.mo.who) ==
                        Kernel.UnboxAny<string>(nseo.mo.who)) {
                if (nseo.mo.mo.type == P6how.PACKAGE)
                    nse = ose;
                return null;
            }

            return "Two definitions found for symbol "+who+"::"+name+"\n\n" +
                    "  first at "+ose.file+" line "+ose.line+"\n" +
                    "  second at "+nse.file+" line "+nse.line;
        }

        public string NsBind(string who, string name, Variable var,
                string file, int line) {
            string key = (char)who.Length + who + name;
            StashEnt nse = new StashEnt();
            nse.constant = (var != null);
            if (var == null)
                var = MakeAppropriateVar(name);
            nse.v = var;
            nse.file = file;
            nse.line = line;
            StashEnt ose;
            string err = null;
            if (globals.TryGetValue(key, out ose))
                err = NsMerge(who, name, ref nse, ose);
            if (err != null) return err;
            globals[key] = nse;
            return null;
        }

        void IFreeze.Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.RuntimeUnit);

            // put this FIRST so that we can bail out in Thaw if need be
            fb.String(name);
            RuntimeUnit[] dep = new List<RuntimeUnit>(depended_units).ToArray();
            Array.Sort<RuntimeUnit>(dep, (a, b) =>
                    string.CompareOrdinal(a.name, b.name));
            string[] srcinfo = new string[dep.Length * 2];
            for (int i = 0; i < dep.Length; i++) {
                srcinfo[i*2]   = dep[i].name;
                srcinfo[i*2+1] = Utils.HashToStr(
                    ObjectRegistry.NewHash().ComputeHash(
                        new UTF8Encoding().GetBytes(dep[i].source)));
            }
            fb.Strings(srcinfo);

            fb.String(filename);
            fb.String(source);
            fb.String(asm_name);
            fb.String(dll_name);

            fb.ObjRef(owner);
            if (owner == this) {
                // master unit requires saving type and constant info
                fb.Int(constants.Count);
                foreach (KeyValuePair<object,FieldInfo> kv in constants) {
                    fb.String(kv.Value.Name);
                    fb.ObjRef(kv.Key);
                }
                fb.Int(globals.Count);
                foreach (KeyValuePair<string, StashEnt> kv in globals) {
                    fb.String(kv.Key);
                    fb.ObjRef(kv.Value);
                }
            }
            fb.Refs(dep);

            fb.Refs(subordinates);
            fb.ObjRef(mainline);
            fb.ObjRef(bottom);
            fb.Refs(our_subs);
            fb.Byte((byte)(is_mainish ? 1 : 0));

            if (name == "CORE") {
                FieldInfo[] kf = typeof(Compartment).GetFields();
                Array.Sort<FieldInfo>(kf,
                        (f1, f2) => string.CompareOrdinal(f1.Name, f2.Name));
                foreach (FieldInfo f in kf) {
                    if (f.GetCustomAttributes(typeof(CORESavedAttribute), true).Length != 0) {
                        //Console.WriteLine("Freeze {0} {1}", f, f.GetValue(Compartment.Top));
                        fb.ObjRef(f.GetValue(Compartment.Top));
                    }
                }
            }
        }

        internal static RuntimeUnit Thaw(ThawBuffer tb) {
            RuntimeUnit n = new RuntimeUnit();
            tb.Register(n);

            n.name     = tb.String();
            string[] srcinfo = tb.Strings();
            if (Builtins.upcall_receiver != null) {
                object[] args = new object[srcinfo.Length + 1];
                Array.Copy(srcinfo, 0, args, 1, srcinfo.Length);
                args[0] = "check_dated";
                object result = Builtins.UpCall(args);
                if (result is Exception)
                    throw (Exception)result;
                if ((string)result != "ok")
                    throw new ThawException("dated sources");
            }

            n.filename = tb.String();
            n.source   = tb.String();
            n.asm_name = tb.String();
            n.dll_name = tb.String();

            n.owner = (RuntimeUnit)tb.ObjRef();
            // master-case initialization has to happen before potentially
            // loading any *other* unit
            if (n.owner == n) {
                // is a master unit
                // set this before any potential of aliasing
                n.globals = new Dictionary<string,StashEnt>();
                int ncon = tb.Int();
                if (Backend.cross_level_load) {
                    // don't load a type, throw away constants
                    while (ncon-- > 0) {
                        tb.String();
                        tb.ObjRef();
                    }
                } else {
                    Assembly assembly = Assembly.Load(n.asm_name);
                    n.type = tb.type = assembly.GetType(n.asm_name, true);
                    n.constTable = Activator.CreateInstance(n.type);
                    n.constants = new Dictionary<object,FieldInfo>();
                    var fields = new Dictionary<string,FieldInfo>();
                    foreach (FieldInfo fi in n.type.GetFields())
                        fields[fi.Name] = fi;
                    var meth = new Dictionary<string,MethodInfo>();

                    foreach (MethodInfo mi in n.type.GetMethods())
                        meth[mi.Name] = mi;
                    RuntimeUnit.reg.methods[n.asm_name] = meth;
                    RuntimeUnit.reg.instances[n.asm_name] = n.constTable;
                    while (ncon-- > 0) {
                        FieldInfo fi = fields[tb.String()];
                        object val = tb.ObjRef();
                        n.constants[val] = fi;
                        fi.SetValue(n.constTable, val);
                    }
                }

                int ct = tb.Int();
                for (int i = 0; i < ct; i++) {
                    n.globals[tb.String()] = (StashEnt)tb.ObjRef();
                }
            }
            else {
                n.globals = n.owner.globals;
                if (n.globals == null) throw new Exception("load goofed");
                n.type = n.owner.type;
            }

            n.depended_units = new HashSet<RuntimeUnit>(tb.RefsA<RuntimeUnit>());
            n.subordinates = tb.RefsL<RuntimeUnit>();
            n.mainline   = (SubInfo)tb.ObjRef();
            n.bottom     = (SubInfo)tb.ObjRef();
            n.our_subs   = tb.RefsL<SubInfo>();
            n.is_mainish = tb.Byte() != 0;

            if (n.name == "CORE") {
                FieldInfo[] kf = typeof(Compartment).GetFields();
                Array.Sort<FieldInfo>(kf,
                        (f1, f2) => string.CompareOrdinal(f1.Name, f2.Name));
                foreach (FieldInfo f in kf) {
                    if (f.GetCustomAttributes(typeof(CORESavedAttribute), true).Length != 0) {
                        f.SetValue(Compartment.Top, tb.ObjRef());
                        //Console.WriteLine("Thaw {0} {1}", f, f.GetValue(Compartment.Top));
                    }
                }
            }
            return n;
        }
    }

    public sealed class LeaveHook {
        public LeaveHook next;
        public P6any     thunk;
        public int       type;

        public const int KEEP = 1;
        public const int UNDO = 2;
        public const int DIE  = 4;
        public const int POST = 7;
        public const int SENTINEL = 15;
    }

    public abstract class LexInfo {
        public SubInfo owner;
        public string name;
        public string file;
        public int line;
        public int pos;

        public abstract void Init(Frame f);
        public virtual object Get(Frame f) {
            return Get(f,null);
        }
        public virtual void Set(Frame f, object to) { Set(f,to,null); }
        public virtual object Get(Frame f, Frame orig) { return Get(f); }
        public virtual void Set(Frame f, object to, Frame orig) { Set(f,to); }

        internal virtual ClrOp SetCode(int up, ClrOp head) {
            return SetCode(up, head, null);
        }
        internal virtual ClrOp SetCode(int up, ClrOp head, SubInfo orig) {
            return SetCode(up, head);
        }
        internal virtual ClrOp GetCode(int up) { return GetCode(up, null); }
        internal virtual ClrOp GetCode(int up, SubInfo orig) {
            return GetCode(up);
        }

        // names that are forced to dynamism for quick access
        public static bool IsDynamicName(string name) {
            if (name == "$_" || name == "$/" || name == "$!") return true;
            if (name.Length < 2) return false;
            if (name[0] == '*' || name[0] == '?') return true;
            if (name[1] == '*' || name[1] == '?') return true;
            return false;
        }
        public virtual void BindFields() {}
        public virtual int SigIndex() { return -1; }

        internal abstract void DoFreeze(FreezeBuffer fb);
        internal enum LexSerCode {
            Simple, Sub, Label, Dispatch, Common, Constant, Package, Alias,
            AttrAlias
        }

        internal ClrOp DieCode(Type rty, string msg) {
            return new ClrOperator(rty, OpCodes.Throw, new [] {
                new ClrConstructorCall(typeof(NieczaException).
                    GetConstructor(new [] { typeof(string) }),
                    new [] { new ClrStringLiteral(msg) }) });
        }
    }

    public abstract class LIVarish : LexInfo {
        public int index;

        public LIVarish() { }

        public override int SigIndex() { return index; }
        public override void BindFields() {
            index = owner.num_lex_slots++;
            if (owner.protopad != null) {
                int osz = owner.protopad.lexn == null ? 0 : owner.protopad.lexn.Length;
                if (owner.num_lex_slots > 10 + osz)
                    Array.Resize(ref owner.protopad.lexn, owner.num_lex_slots * 2 - 10);
            }
        }

        public override object Get(Frame f) {
            return f.GetDynamic(index);
        }

        public override void Set(Frame f, object to) {
            f.SetDynamic(index, to);
        }

        internal override ClrOp GetCode(int up) {
            if ((owner.special & SubInfo.RUN_ONCE) != 0) {
                return new ClrUnboxAny(Tokens.Variable,
                        new ClrMethodCall(false, Tokens.Frame.GetMethod("GetDynamic"),
                    EmitUnit.Current.FrameConstant(owner.protopad).head,
                    new ClrIntLiteral(typeof(int), index)));
            }
            return new ClrPadGet(up, index);
        }

        internal override ClrOp SetCode(int up, ClrOp to) {
            if ((owner.special & SubInfo.RUN_ONCE) != 0)
                return new ClrProtoSet(index,
                    EmitUnit.Current.FrameConstant(owner.protopad).head, to);
            return new ClrPadSet(up, index, to);
        }
    }

    // TODO: Provide some way to cache a StashEnt once the currentGlobals
    // stops changing
    public class LICommon : LexInfo {
        public readonly string hkey;
        public LICommon(string hkey) { this.hkey = hkey; }

        public override void Init(Frame f) { }
        internal string Stash() { return hkey.Substring(1, (int)hkey[0]); }
        internal string VarName() { return hkey.Substring(1 + (int)hkey[0]); }

        public override object Get(Frame f) {
            return Kernel.GetGlobal(hkey);
        }

        public override void Set(Frame f, object to) {
            Kernel.BindGlobal(hkey, (Variable)to);
        }

        internal override ClrOp GetCode(int up) {
            return new ClrMethodCall(false, Tokens.Kernel_GetGlobal,
                    new ClrStringLiteral(hkey));
        }

        internal override ClrOp SetCode(int up, ClrOp to) {
            return new ClrMethodCall(false, Tokens.Kernel_BindGlobal,
                    new ClrStringLiteral(hkey), to);
        }

        internal override void DoFreeze(FreezeBuffer fb) {
            fb.Byte((byte)LexSerCode.Common);
            fb.String(hkey);
        }
    }

    public class LIConstant : LexInfo {
        public Variable value;
        public LIConstant() { this.value = Compartment.Top.AnyP; }
        internal LIConstant(Variable value) { this.value = value; }
        public override void Init(Frame f) { }
        public override void BindFields() { }

        public override object Get(Frame f) { return value; }

        internal override ClrOp GetCode(int up) {
            return EmitUnit.Current.RefConstant(name, "", value, typeof(Variable)).head;
        }

        public override void Set(Frame f, object to) {
            throw new NieczaException("Cannot bind to constant " + name);
        }
        internal override ClrOp SetCode(int up, ClrOp to) {
            return DieCode(Tokens.Void, "Cannot bind to constant " + name);
        }

        internal override void DoFreeze(FreezeBuffer fb) {
            fb.Byte((byte)LexSerCode.Constant);
            fb.ObjRef(value);
        }
    }

    public class LISub : LIVarish {
        public SubInfo def;
        public LISub(SubInfo def) { this.def = def; }
        internal LISub(int index, SubInfo def) { this.index = index; this.def = def; }
        public override void Init(Frame f) {
            Set(f, def.protosub);
        }
        internal override void DoFreeze(FreezeBuffer fb) {
            fb.Byte((byte)LexSerCode.Sub);
            fb.Int(index);
            fb.ObjRef(def);
        }
    }

    public class LISimple : LIVarish {
        public const int NOINIT = 1;
        public const int ROINIT = 2;
        public const int DEFOUTER = 4;
        public const int LIST = 8;
        public const int HASH = 16;

        public int flags;
        public STable type;
        public LISimple(int flags, STable type) {
            this.flags = flags;
            this.type  = type;
        }
        internal LISimple(int index, int flags, STable type) {
            this.index = index;
            this.flags = flags;
            this.type  = type;
        }
        internal override void DoFreeze(FreezeBuffer fb) {
            fb.Byte((byte)LexSerCode.Simple);
            fb.Int(index);
            fb.Byte((byte)flags);
            fb.ObjRef(type);
        }
        public override void Init(Frame f) {
            if ((flags & NOINIT) != 0)
                return;
            if ((flags & ROINIT) != 0)
                Set(f, Compartment.Top.AnyP);
            else if ((flags & DEFOUTER) != 0)
                Set(f, f.info.GetOuterTopic(f));
            else if ((flags & LIST) != 0)
                Set(f, Kernel.CreateArray());
            else if ((flags & HASH) != 0)
                Set(f, Kernel.CreateHash());
            else
                Set(f, Kernel.NewTypedScalar(type));
        }
    }

    public class LILabel : LIVarish {
        public LILabel() { }
        internal LILabel(int index) { this.index = index; }
        public override void Init(Frame f) {
            Set(f, Kernel.NewLabelVar(f, name));
        }
        internal override void DoFreeze(FreezeBuffer fb) {
            fb.Byte((byte)LexSerCode.Label);
            fb.Int(index);
        }
    }

    public class LIDispatch : LIVarish {
        public LIDispatch() { }
        internal LIDispatch(int index) { this.index = index; }
        public override void Init(Frame f) {
            MakeDispatch(f);
        }
        internal override void DoFreeze(FreezeBuffer fb) {
            fb.Byte((byte)LexSerCode.Dispatch);
            fb.Int(index);
        }

        // synchronize with NamProcessor.MakeDispatch
        static P6any GetProtoSub(LexInfo li, Frame f) {
            return (li is LISub) ? ((LISub)li).def.protosub :
                ((Variable)li.Get(f,null)).Fetch();
        }
        internal void MakeDispatch(Frame into) {
            HashSet<string> names = new HashSet<string>();
            List<P6any> cands = new List<P6any>();
            P6any proto = null;
            string filter = name + ":";
            string pn = name + ":(!proto)";
            //Console.WriteLine("MakeDispatch {0}", name);

            Frame f = into;
            for (SubInfo csr = into.info; ; csr = csr.outer) {
                bool brk = false;
                foreach (KeyValuePair<string,LexInfo> kp in csr.dylex) {
                    if (Utils.StartsWithInvariant(filter, kp.Key) &&
                            kp.Key != pn &&
                            !names.Contains(kp.Key)) {
                        names.Add(kp.Key);
                        brk = true;
                        //Console.WriteLine("cand {0}", kp.Key);
                        cands.Add(GetProtoSub(kp.Value,f));
                    }
                }
                if (csr.outer == null) break;
                // don't go above nearest proto
                if (csr.dylex.ContainsKey(pn)) {
                    //Console.WriteLine("proto {0}", pn);
                    proto = GetProtoSub(csr.dylex[pn], f);
                    break;
                }
                if (brk) cands.Add(null);
                f = f.outer;
            }

            Set(into, Kernel.MakeDispatcher(name, proto, cands.ToArray()));
        }
    }

    public class LIAlias : LexInfo {
        public string to;
        public LIAlias(string to) { this.to = to; }
        public override void Init(Frame f) { }
        object Common(Frame f, bool set, object bind, Frame orig) {
            Frame cr = f;
            LexInfo li = null;
            while (cr.info.dylex == null ||
                    !cr.info.dylex.TryGetValue(to, out li))
                cr = cr.outer;
            if (!set) return li.Get(cr, orig);
            else { li.Set(cr, bind, orig); return null; }
        }
        public override object Get(Frame f, Frame orig) { return Common(f,false,null,orig); }
        public override void Set(Frame f, object bind, Frame orig) { Common(f,true,bind,orig); }

        internal override ClrOp GetCode(int up, SubInfo orig) {
            LexInfo real;
            SubInfo sc = owner;
            while (!sc.dylex.TryGetValue(to, out real)) {
                sc = sc.outer;
                up++;
            }
            return real.GetCode(up, orig);
        }

        internal override ClrOp SetCode(int up, ClrOp bind, SubInfo orig) {
            LexInfo real;
            SubInfo sc = owner;
            while (!sc.dylex.TryGetValue(to, out real)) {
                sc = sc.outer;
                up++;
            }
            return real.SetCode(up, bind, orig);
        }
        internal override void DoFreeze(FreezeBuffer fb) {
            fb.Byte((byte)LexSerCode.Alias);
            fb.String(to);
        }
    }

    public class LIAttrAlias : LexInfo {
        public STable atype;
        public string aname;
        public LIAttrAlias(STable atype, string aname) {
            this.atype = atype;
            this.aname = aname;
        }
        public override void Init(Frame f) { }
        P6any GetSelf(Frame f) {
            SubInfo sc = owner;
            LexInfo self;
            while (sc != null && !sc.dylex.TryGetValue("self", out self)) {
                sc = sc.outer;
                f  = f.outer;
            }
            if (sc == null)
                throw new NieczaException("No 'self' available in this scope to resolve reference to attribute '{0}'", name);
            return ((Variable)self.Get(f)).Fetch();
        }
        ClrOp GetSelfCode(SubInfo orig) {
            SubInfo sc = orig;
            LexInfo self;
            int up = 0;
            while (sc != null && !sc.dylex.TryGetValue("self", out self)) {
                sc = sc.outer;
                up++;
            }
            if (sc == null)
                return DieCode(Tokens.P6any, string.Format("No 'self' available in this scope to resolve reference to attribute '{0}'", name));
            return new ClrMethodCall(false, Tokens.Variable_Fetch,
                    self.GetCode(up, orig));
        }
        public override object Get(Frame f, Frame orig) {
            if (orig == null) throw new NieczaException("Indirect access to attribute aliases NYI");
            return GetSelf(orig).GetSlot(atype, aname);
        }
        public override void Set(Frame f, object bind, Frame orig) {
            if (orig == null) throw new NieczaException("Indirect access to attribute aliases NYI");
            GetSelf(orig).SetSlot(atype, aname, bind);
        }

        internal override ClrOp GetCode(int up, SubInfo orig) {
            return new ClrUnboxAny(Tokens.Variable,
                new ClrMethodCall(false, Tokens.P6any_GetSlot,
                    GetSelfCode(orig),EmitUnit.Current.TypeConstant(atype).head,
                    new ClrStringLiteral(aname)));
        }

        internal override ClrOp SetCode(int up, ClrOp bind, SubInfo orig) {
            return new ClrMethodCall(false, Tokens.P6any_SetSlot,
                GetSelfCode(orig),EmitUnit.Current.TypeConstant(atype).head,
                new ClrStringLiteral(aname), bind);
        }

        internal override void DoFreeze(FreezeBuffer fb) {
            fb.Byte((byte)LexSerCode.AttrAlias);
            fb.ObjRef(atype);
            fb.String(aname);
        }
    }

    public class LIPackage : LexInfo {
        public STable pkg;
        public LIPackage(STable pkg) { this.pkg = pkg; }
        public override object Get(Frame f) {
            return pkg == Compartment.Top.NilP.mo ? (Variable)Compartment.Top.Nil : pkg.typeObj;
        }
        public override void Init(Frame f) { }
        internal override ClrOp GetCode(int up) {
            return pkg == Compartment.Top.NilP.mo ?
                EmitUnit.Current.RefConstant("Compartment.Top.Nil", "L", Compartment.Top.Nil, typeof(Variable)).head :
                EmitUnit.Current.TypeConstantP(pkg).head;
        }
        internal override void DoFreeze(FreezeBuffer fb) {
            fb.Byte((byte)LexSerCode.Package);
            fb.ObjRef(pkg);
        }

        public override void Set(Frame f, object to) {
            throw new NieczaException("Cannot bind to constant " + name);
        }
        internal override ClrOp SetCode(int up, ClrOp to) {
            return DieCode(Tokens.Void, "Cannot bind to constant " + name);
        }
    }

    public class Parameter : P6any, IFixup {
        public int      flags;
        public int      slot;
        public string   name;
        public string[] names;
        public object   def; // SubInfo or Variable
        public STable   type;

        // XXX I don't really like dangling two extra fields on every param...
        public string   attribute;
        public STable   attribute_type;

        public object[]  post_constraints; // Signature | SubInfo

        public override string ReprName() { return "P6parameter"; }

        private Parameter() { }
        public Parameter(int flags, int slot, string name,
                string[] names, object def, STable type, string attr,
                STable atype) {
            this.mo = Compartment.Top.ParameterMO;
            this.flags = flags;
            this.name = name;
            this.slot = slot;
            this.names = names;
            this.def = def;
            this.type = type;
            this.attribute = attr;
            this.attribute_type = atype;
        }

        public static Parameter TPos(string name, int slot) {
            return new Parameter(RWTRANS | POSITIONAL, slot, name,
                    null, null, Compartment.Top.AnyMO, null, null);
        }

        public static Parameter TNamedOpt(string name, int slot) {
            return new Parameter(RWTRANS | OPTIONAL, slot, name,
                    new string[] { name }, null, Compartment.Top.AnyMO, null, null);
        }

        // Value processing
        public const int HASTYPE    = 1; // else Compartment.Top.AnyMO
        public const int MULTI_IGNORED = 16384;
        public const int ANY_DEF    =  0x40000;
        public const int UNDEF_ONLY =  0x80000;
        public const int DEF_ONLY   =  0xC0000;
        public const int TYPE_ONLY  = 0x100000;
        public const int DEF_MASK   = 0x1C0000;

        public const int DEF_SHIFT = 18;

        // Value binding
        public const int READWRITE  = 2;
        public const int RWTRANS    = 8;
        public const int INVOCANT   = 8192;
        public const int IS_COPY    = 32768;
        public const int IS_LIST    = 65536;
        public const int IS_HASH    = 131072;
        public const int CALLABLE   = 0x200000;

        // Value source
        public const int HASDEFAULT = 32;
        public const int OPTIONAL   = 64;
        public const int DEFOUTER   = 4096;
        public const int POSITIONAL = 128;
        public const int SLURPY_POS = 256;
        public const int SLURPY_NAM = 512;
        public const int SLURPY_CAP = 1024;
        public const int SLURPY_PCL = 2048;

        public override void Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.Parameter);

            fb.Int(flags);
            fb.Int(slot);
            fb.String(name);
            fb.Strings(names);
            fb.ObjRef(def);
            fb.ObjRef(type);
            fb.Refs(post_constraints);
            fb.String(attribute);
            if (attribute != null)
                fb.ObjRef(attribute_type);
        }

        internal static Parameter Thaw(ThawBuffer tb) {
            Parameter n = new Parameter();
            tb.Register(n);
            tb.PushFixup(n);

            n.flags = tb.Int();
            n.slot  = tb.Int();
            n.name  = tb.String();
            n.names = tb.Strings();
            n.def   = tb.ObjRef();
            n.type  = (STable)tb.ObjRef();
            n.post_constraints = tb.RefsA<object>();
            n.attribute = tb.String();
            if (n.attribute != null)
                n.attribute_type = (STable)tb.ObjRef();

            return n;
        }

        void IFixup.Fixup() { mo = Compartment.Top.ParameterMO; }
    }

    public class Signature : P6any, IFixup {
        public Parameter[] parms;

        public override string ReprName() { return "P6sig"; }

        public Signature(params Parameter[] parms) { this.mo = Compartment.Top.SignatureMO; this.parms = parms; }
        private Signature() { }

        public override void Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.Signature);
            fb.Refs<Parameter>(parms);
        }

        internal static Signature Thaw(ThawBuffer tb) {
            Signature n = new Signature();
            tb.Register(n);
            tb.PushFixup(n);

            n.parms = tb.RefsA<Parameter>();
            return n;
        }
        void IFixup.Fixup() { mo = Compartment.Top.SignatureMO; }
    }

    // This stores all the invariant stuff about a Sub, i.e. everything
    // except the outer pointer.  Now distinct from protopads
    //
    // Actually not quite *in*variant; some of this stuff has to be
    // changed, but it's rare by construction.  We don't want to be
    // like Rakudo/Parrot where simple sub cloning requires copying
    // 100s of bytes.
    public class SubInfo : IFreeze, IFixup {
        // Essential call functions
        public DynBlockDelegate code = RuntimeUnit.JitCompileSub;
        public int nspill;
        public Signature sig;

        // Local metadata
        public int[] lines;
        public Dictionary<string, LexInfo> dylex;
        public uint dylex_filter; // (32,1) Bloom on hash code
        public string name;
        public int num_lex_slots; // for code generation
        // maybe should be in extend or a hint?
        public LAD ltm;
        public int special;
        public int phaser = -1;
        public string outervar;

        // References to related objects
        public RuntimeUnit unit;
        public SubInfo outer;
        public P6any protosub;
        public Frame protopad;
        public SubInfo prototype; // when cloning, don't jit twice
        public STable cur_pkg, methodof, body_of, in_class;
        public STable mo;

        // caches for fast $OUTER::_, invocants, exn handling
        public int outer_topic_rank;
        public int outer_topic_key;
        public int self_key;
        public SubInfo catch_, control;

        // this is used only at compile time
        public Dictionary<string,UsedInScopeInfo> used_in_scope;

        // if this is non-null, compilation is being delayed
        public string nam_str;
        public object[] nam_refs;

        // Used for closing runtime-generated SubInfo over values used
        // For vtable wrappers: 0 = unboxed, 1 = boxed
        // For dispatch routines, 0 = parameter list
        public object[] param;
        public List<SubInfo> children = new List<SubInfo>();
        public Dictionary<string,object[]> extend;

        // not saved, rarely used
        internal Dictionary<string,SubInfo> rx_compile_cache;

        public int[] edata;
        public string[] label_names;

        // No instance fields past this point
        public class UsedInScopeInfo {
            public string file;
            public int line;
            public int levels;
            public string orig_file;
            public int orig_line;
        }

        public const int RUN_ONCE = 1;
        public const int MAKE_PROTOPAD = 2;
        public const int HAS_TYPE = 4;
        public const int UNSAFE = 8;
        public const int PARAM_ROLE = 16;
        public const int MAINLINE = 32;
        public const int TRANSPARENT = 64;
        public const int INLINED = 128;
        public const int CANNOT_INLINE = 256;
        public const int RETURN_PASS = 512;

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
        public const int ON_WARNING = 11;
        // ON_VARLOOKUP is kinda special, it's not used for exceptions
        // but rather for $*FOO and the like; goto = the variable index

        [Immutable]
        private static string[] controls = new string[] { "unknown", "next",
            "last", "redo", "return", "die", "succeed", "proceed", "goto",
            "nextsame/nextwith", "varlookup", "warning" };
        public static string DescribeControl(int type, Frame tgt,
                string name) {
            string ty = (type < controls.Length) ? controls[type] : "unknown";
            if (name != null) {
                return ty + "(" + name + (tgt != null ? ", lexotic)" : ", dynamic)");
                //return ty + "(" + name + (tgt != null ? string.Format(", lexotic [{0:X}])", tgt.GetHashCode()) : ", dynamic)");
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
            if (edata == null)
                return -1;
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

        internal List<SubInfo> GetPhasers(int t1) {
            int t2 = (t1 == Kernel.PHASER_KEEP) ? Kernel.PHASER_LEAVE : t1;
            List<SubInfo> r = new List<SubInfo>();
            foreach (SubInfo z in children) {
                if (z.phaser >= t1 && z.phaser <= t2) r.Add(z);
            }
            return r;
        }

        public Frame SetupCall(Frame caller, Frame outer, P6any sub,
                Variable[] pos, VarHash named, bool quiet, DispatchEnt de) {
            Frame th;
            if ((special & RUN_ONCE) != 0) {
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
            if (quiet) th.flags |= Frame.CHECK_ONLY;
            return th;
        }

        internal Variable GetOuterTopic(Frame f) {
            for (int i = 0; i < outer_topic_rank; i++) f = f.outer;
            return (Variable)f.GetDynamic(outer_topic_key);
        }

        internal static Frame AutoThreadSubC(Frame th) {
            Variable[] src = (Variable[]) th.lex4;
            Variable[] dst = (Variable[]) th.lex8;
            if (th.lexi1 > 0)
                dst[th.lexi1 - 1] = (Variable)th.resultSlot;

            if (th.lexi1 == dst.Length) {
                P6opaque nj = new P6opaque(Compartment.Top.JunctionMO);
                nj.slots[0] = th.lex3;
                nj.slots[1] = Kernel.BoxRaw(dst, Compartment.Top.ParcelMO);
                // restore, in case our caller is using this
                if (th.lexi0 == -2)
                    th.named[(string)th.lex2] = (Variable)th.lex9;
                else
                    th.pos[th.lexi0] = (Variable)th.lex9;
                th.caller.resultSlot = nj;
                return th.Return();
            }

            if (th.lexi0 == -2)
                th.named[(string)th.lex2] = src[th.lexi1++];
            else
                th.pos[th.lexi0] = src[th.lexi1++];

            return ((SubInfo)th.lex1).SetupCall(th, (Frame)th.lex5, (P6any)th.lex6,
                th.pos, th.named, false, (DispatchEnt)th.lex7);
        }

        internal bool IsInlinable() {
            if (sig == null)
                return false;
            if ((special & CANNOT_INLINE) != 0)
                return false;
            if (children.Count != 0)
                return false;
            foreach (KeyValuePair<string,LexInfo> kv in dylex) {
                if (!(kv.Value is LISimple))
                    return false;
                if ((kv.Key.Length > 0 &&
                            (kv.Key[0] == '?' || kv.Key[0] == '*')) ||
                        (kv.Key.Length > 1 &&
                          (kv.Key[1] == '?' || kv.Key[1] == '*')))
                    return false;
            }
            foreach (Parameter p in sig.parms) {
                int fl = p.flags;
                if ((fl & Parameter.POSITIONAL) == 0)
                    return false;
                if ((fl & Parameter.HASDEFAULT) != 0)
                    return false;
            }
            return true;
        }

        internal void SetInlined() {
            special |= INLINED;
            outer.children.Remove(this);
            unit.our_subs.Remove(this);
            foreach (KeyValuePair<string, LexInfo> kv in outer.dylex)
                if (kv.Value is LISub && ((LISub)kv.Value).def == this) {
                    outer.dylex.Remove(kv.Key);
                    break;
                }
        }

        internal Variable RunBEGIN() {
            return Kernel.RunInferior(protosub.Invoke(Kernel.GetInferiorRoot(),
                Variable.None, null));
        }

        internal bool IsTopicalizer() {
            if (sig == null)
                return false;
            LexInfo topic;
            dylex.TryGetValue("$_", out topic);
            foreach (Parameter p in sig.parms) {
                if (p.slot >= 0 && topic != null && topic.SigIndex() == p.slot)
                    return true;
            }
            return false;
        }

        internal void AddLexical(string name, LexInfo li) {
            li.owner = this;
            li.name  = name;
            dylex[name] = li;
            dylex_filter |= FilterForName(name);
            li.BindFields();
            if (name == "self")
                self_key = li.SigIndex();
            if (protopad != null) {
                li.Init(protopad);

                int ix = name.LastIndexOf(':');
                LexInfo disp;
                //Console.WriteLine("AddLexical {0}", name);
                if (ix >= 0 && dylex.TryGetValue(name.Substring(0, ix),
                            out disp) && disp is LIDispatch) {
                    ((LIDispatch)disp).MakeDispatch(protopad);
                }
            }

            if (li is LISub && ((LISub)li).def.outer != this)
                Console.WriteLine("Eeep!  Adding a LISub to the wrong place");
        }

        // ofr != null is used ONLY with evals to set up a protopad pointing
        // to a specific runtime pad.  I'm not 100% sure this design is right...
        internal void CreateProtopad(Frame ofr) {
            if (protopad != null)
                return;
            if (outer != null)
                outer.CreateProtopad(null);

            // protosub is set when the parent's protopad is created,
            // or when we are
            protopad = new Frame(null, ofr != null ? ofr : outer != null ?
                    outer.protopad : null, this, protosub);
            // make sure there's room for installing the current lexicals
            // may be grown later for more lexicals, or when compiling
            // spill-slots
            protopad.EnsureSpills(num_lex_slots - 10);

            foreach (SubInfo z in children) {
                z.protosub = Kernel.MakeSub(z, protopad);
            }
            foreach (LexInfo li in dylex.Values)
                li.Init(protopad);
        }

        public static uint FilterForName(string name) {
            if (name.Length < 2 || name[1] != '*') return 0;
            uint hash = (uint)(name.GetHashCode() * FILTER_SALT);
            return 1u << (int)(hash >> 27);
        }

        private SubInfo() { }

        public override string ToString() { return name ?? "ANON"; }

        // This is a _shallow_ clone.  The children wind up shared, as do
        // a lot of immutable-ish objects that are referenced.
        internal SubInfo(SubInfo o) {
            code = o.code;
            nspill = o.nspill;
            sig = o.sig;
            lines = o.lines;
            dylex = o.dylex;
            dylex_filter = o.dylex_filter;
            name = o.name;
            num_lex_slots = o.num_lex_slots;
            ltm = o.ltm;
            special = o.special;
            phaser = -1; // not in any phaser chains
            outervar = null; // in no pads
            unit = o.unit;
            outer = o.outer;
            protosub = null;
            protopad = null; // no compile-time existance!
            cur_pkg = o.cur_pkg;
            methodof = o.methodof;
            body_of = o.body_of;
            in_class = o.in_class;
            mo = o.mo;
            outer_topic_rank = o.outer_topic_rank;
            outer_topic_key = o.outer_topic_key;
            self_key = o.self_key;
            catch_ = o.catch_;
            control = o.control;
            used_in_scope = null;
            nam_str = null;
            nam_refs = null;
            param = o.param;
            children = null;
            extend = o.extend;
            rx_compile_cache = null;
            edata = o.edata;
            label_names = o.label_names;

            prototype = o.prototype ?? o;
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

        public SubInfo(RuntimeUnit unit, string name, SubInfo outer,
                STable cls, STable pkg, bool once, Frame ofr) {
            edata = new int[0];
            this.name  = name;
            this.unit  = unit;
            this.mo    = cls;
            this.outer = outer;
            this.cur_pkg = pkg;

            SubInfo sc = outer;
            LexInfo li = null;
            for (outer_topic_rank = 1; sc != null; sc = sc.outer) {
                if (sc.dylex != null && sc.dylex.TryGetValue("$_", out li))
                    break;
                outer_topic_rank++;
            }
            outer_topic_key = (li is LIVarish) ? (li as LIVarish).index : -1;

            used_in_scope = new Dictionary<string,UsedInScopeInfo>();
            dylex = new Dictionary<string,LexInfo>();

            if (outer == null || outer.protopad != null)
                protosub = Kernel.MakeSub(this, outer == null ?
                        null : outer.protopad);
            if (once) {
                special |= RUN_ONCE;
                CreateProtopad(ofr);
            }
        }

        public SubInfo(string name, DynBlockDelegate code) :
            this(name, null, code, null, null, new int[0], null, 0) { }

        void IFreeze.Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.SubInfo);
            string mn = null;
            string tn = null;
            if (code != null) {
                Type t = code.Method.DeclaringType;
                if (t.Assembly == typeof(Kernel).Assembly) {
                    fb.Byte(1);
                } else {
                    fb.Byte(0);
                    if (t.Assembly.GetName().Name != t.FullName)
                        throw new NieczaException("violation of naming protocols? {0} != {1}", t.Assembly.GetName().Name, t.FullName);
                }
                tn = t.FullName;
                mn = code.Method.Name;
            }
            fb.String(mn);
            fb.String(tn);
            fb.Int(nspill);
            fb.ObjRef(sig);

            fb.Ints(lines);
            fb.Ints(edata);
            fb.Strings(label_names);
            fb.Int(num_lex_slots);
            fb.Int(dylex.Count);
            foreach (KeyValuePair<string, LexInfo> kv in dylex) {
                fb.String(kv.Key);
                kv.Value.DoFreeze(fb);
            }
            // not saving dylex_filter as it is a cache
            fb.String(name);
            fb.ObjRef(ltm);
            fb.Int(special);
            fb.Int(phaser);
            fb.String(outervar);
            fb.ObjRef(unit);
            fb.ObjRef(outer);
            fb.ObjRef(protosub);
            fb.ObjRef(protopad);
            fb.ObjRef(prototype);
            fb.ObjRef(cur_pkg);
            fb.ObjRef(methodof);
            fb.ObjRef(body_of);
            fb.ObjRef(in_class);
            fb.ObjRef(mo);
            // not storing caches here
            // also not storing used_in_scope, it's compiler only data
            // TODO: we want to store the compiled code not this
            fb.String(nam_str);
            if (nam_str != null)
                fb.Refs(nam_refs);
            fb.Refs(param);
            fb.Refs(children);
            if (extend == null)
                fb.Int(0);
            else {
                fb.Int(extend.Count);
                foreach(KeyValuePair<string,object[]> kv in extend) {
                    fb.String(kv.Key);
                    fb.Refs(kv.Value);
                }
            }
        }

        internal static SubInfo Thaw(ThawBuffer tb) {
            SubInfo n = new SubInfo();
            tb.Register(n);
            bool kernel = tb.Byte() != 0;
            string mn = tb.String();
            string tn = tb.String();
            if (mn != null && (kernel || !Backend.cross_level_load)) {
                MethodInfo mi;
                object obj = null;
                if (kernel) {
                    mi = typeof(Kernel).Assembly.GetType(tn, true)
                        .GetMethod(mn, BindingFlags.Public |
                            BindingFlags.NonPublic | BindingFlags.Static);
                } else {
                    Dictionary<string,MethodInfo> t1;

                    if (!RuntimeUnit.reg.methods.TryGetValue(tn, out t1) ||
                            !t1.TryGetValue(mn, out mi)) {
                        throw new Exception("Thawed sub references nonexistant method " + tn + "::" + mn);
                    }
                    obj = RuntimeUnit.reg.instances[tn];
                }
                n.code = (DynBlockDelegate) Delegate.CreateDelegate(
                    typeof(DynBlockDelegate), obj, mi);
            }

            n.nspill = tb.Int();
            n.sig = (Signature)tb.ObjRef();

            n.lines = tb.Ints();
            n.edata = tb.Ints();
            n.label_names = tb.Strings();
            for (int i = 0; i < n.edata.Length; i += 5)
                if (n.edata[i+2] == ON_VARLOOKUP && n.edata[i+4] >= 0)
                    n.dylex_filter |=FilterForName(n.label_names[n.edata[i+4]]);
            n.num_lex_slots = tb.Int();
            int dyct = tb.Int();
            n.dylex = new Dictionary<string, LexInfo>();
            for (int i = 0; i < dyct; i++) {
                string key = tb.String();
                int type = tb.Byte();
                LexInfo li = null;
                switch (type) {
                    case (int) LexInfo.LexSerCode.Simple:
                        li = new LISimple(tb.Int(), tb.Byte(), (STable)tb.ObjRef());
                        break;
                    case (int) LexInfo.LexSerCode.Sub:
                        li = new LISub(tb.Int(), (SubInfo)tb.ObjRef());
                        break;
                    case (int) LexInfo.LexSerCode.Label:
                        li = new LILabel(tb.Int());
                        break;
                    case (int) LexInfo.LexSerCode.Dispatch:
                        li = new LIDispatch(tb.Int());
                        break;
                    case (int) LexInfo.LexSerCode.Common:
                        li = new LICommon(tb.String());
                        break;
                    case (int) LexInfo.LexSerCode.Constant:
                        li = new LIConstant((Variable) tb.ObjRef());
                        break;
                    case (int) LexInfo.LexSerCode.Package:
                        li = new LIPackage((STable) tb.ObjRef());
                        break;
                    case (int) LexInfo.LexSerCode.Alias:
                        li = new LIAlias(tb.String());
                        break;
                    case (int) LexInfo.LexSerCode.AttrAlias:
                        li = new LIAttrAlias((STable)tb.ObjRef(), tb.String());
                        break;
                    default:
                        throw new ArgumentException(type.ToString());
                }
                n.dylex[key] = li;
                li.owner = n;
                li.name = key;
                n.dylex_filter |= FilterForName(key);
            }
            // not saving dylex_filter as it is a cache
            n.name = tb.String();
            n.ltm = (LAD) tb.ObjRef();
            n.special = tb.Int();
            n.phaser = tb.Int();
            n.outervar = tb.String();
            n.unit = (RuntimeUnit)tb.ObjRef();
            n.outer = (SubInfo)tb.ObjRef();
            n.protosub = (P6any)tb.ObjRef();
            n.protopad = (Frame)tb.ObjRef();
            n.prototype = (SubInfo)tb.ObjRef();
            n.cur_pkg = (STable)tb.ObjRef();
            n.methodof = (STable)tb.ObjRef();
            n.body_of = (STable)tb.ObjRef();
            n.in_class = (STable)tb.ObjRef();
            n.mo = (STable)tb.ObjRef();
            // not storing caches here
            // also not storing used_in_scope, it's compiler only data
            // TODO: we want to store the compiled code not this
            n.nam_str = tb.String();
            if (n.nam_str != null)
                n.nam_refs = tb.RefsA<object>();
            n.param = tb.RefsA<object>();
            n.children = tb.RefsL<SubInfo>();
            int nex = tb.Int();
            if (nex != 0) n.extend = new Dictionary<string,object[]>();
            for (int i = 0; i < nex; i++)
                n.extend[tb.String()] = tb.RefsA<object>();
            tb.PushFixup(n);
            return n;
        }

        void IFixup.Fixup() {
            SubInfo sc = outer;
            LexInfo li = null;
            for (outer_topic_rank = 1; sc != null; sc = sc.outer) {
                if (sc.dylex != null && sc.dylex.TryGetValue("$_", out li))
                    break;
                outer_topic_rank++;
            }
            outer_topic_key = (li is LIVarish) ? (li as LIVarish).index : -1;

            if (dylex.TryGetValue("self", out li)) {
                self_key = li.SigIndex();
            } else {
                self_key = -1;
            }

            if (children != null) {
                foreach (SubInfo z in children) {
                    if (z.phaser == Kernel.PHASER_CATCH)
                        catch_ = z;
                    if (z.phaser == Kernel.PHASER_CONTROL)
                        control = z;
                }
            }

            if (phaser == Kernel.PHASER_CHECK)
                Compartment.Top.check.Add(this, false);
            if (phaser == Kernel.PHASER_INIT)
                Compartment.Top.init.Add(this, false);
            if (phaser == Kernel.PHASER_END)
                Compartment.Top.end.Add(this, false);

            if ((special & UNSAFE) != 0)
                Kernel.CheckUnsafe(this);
        }
    }

    // This object fills the combined roles of Parrot LexPad and CallContext
    // objects.
    public class Frame: P6any, IFixup {
        // Used by trampoline to find destination
        public int ip = 0;
        public DynBlockDelegate code;
        // Storage for lexicals: most subs have a smallish number of lexicals,
        // and inlining them into the frame helps a lot.  Since Frame objects
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

        // special slot that is filled by the callee before returning
        // used in some cases as a scratch slot by the codegen
        public object resultSlot = null;

        public int lexi0;
        public int lexi1;

        public object[] lexn; // Overflow for large subs

        // Link fields for other related frames, objects
        public Frame caller;
        public Frame outer;
        public SubInfo info; // the executing sub
        // a doubly-linked list of frames being used by a given coroutine
        public Frame reusable_child;
        public Frame reuser;
        // by being non-null marks the root frame of a coroutine, also
        // stores the Frame to return to when take is called if != this
        public Frame coro_return;

        // used by nextsame to find where to go
        public DispatchEnt curDisp;
        // stores extra data needed in regex frames
        public RxFrame rx;
        // for CALLER::<&?BLOCK>
        public P6any sub;
        // linked list of LEAVE phasers to call at return time
        public LeaveHook on_leave;

        // stores original capture for callframe.args
        public Variable[] pos;
        public VarHash named;

        // after MakeSub, GatherHelper
        public const int SHARED = 1;
        public const int CHECK_ONLY = 2;
        public const int NO_JUNCTION = 4;
        public int flags;

        public Frame(Frame caller_, Frame outer_,
                SubInfo info_, P6any sub_) {
            caller = caller_;
            outer = outer_;
            code = info_.code;
            info = info_;
            sub = sub_;
            mo = Compartment.Top.CallFrameMO;
            lexn = (info_.nspill > 0) ? new object[info_.nspill] : null;
        }

        public Frame() { mo = Compartment.Top.CallFrameMO; }

        public static readonly bool TraceCalls =
            Environment.GetEnvironmentVariable("NIECZA_TRACE_CALLS") != null;
        public static readonly bool VerboseExceptions =
            Environment.GetEnvironmentVariable("NIECZA_VERBOSE_EXCEPTIONS") != null;
        public static readonly bool AllExceptions =
            Environment.GetEnvironmentVariable("NIECZA_ALL_EXCEPTIONS") != null;

        public override string ReprName() { return "P6frame"; }

        public Frame MakeChild(Frame outer, SubInfo info, P6any sub) {
            if (reusable_child == null) {
                reusable_child = new Frame();
                reusable_child.reuser = this;
            }
            if (TraceCalls)
                Console.WriteLine("{0}\t{1}", this.info.name, info.name);
            reusable_child.ip = 0;
            reusable_child.flags = 0;
            reusable_child.resultSlot = null;
            reusable_child.lexn = (info.nspill != 0) ? new object[info.nspill] : null;
            reusable_child.on_leave = null;
            reusable_child.coro_return = null;
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
            reusable_child.pos = null;
            reusable_child.named = null;
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

        internal void EnsureSpills(int nr) {
            if (nr > 0 && (lexn == null || lexn.Length < nr))
                Array.Resize(ref lexn, nr);
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
            P6any nw = new P6opaque(Compartment.Top.CaptureMO);
            var poscap = pos ?? new Variable[0];
            if (sub != null && poscap.Length > 0 && sub.Does(Compartment.Top.MethodMO)) {
                // Hide the self value so that using |callframe.args in a
                // nextwith call will DTRT
                poscap = new Variable[pos.Length - 1];
                Array.Copy(pos, 1, poscap, 0, poscap.Length);
            }
            nw.SetSlot(Compartment.Top.CaptureMO, "$!positionals", poscap);
            nw.SetSlot(Compartment.Top.CaptureMO, "$!named", named);
            return nw;
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
            if (info != null && info.unit != null && info.unit.filename != null)
                return info.unit.filename;
            else
                return "<unknown>";
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
            uint m = SubInfo.FilterForName(name);
            while (csr != null) {
                object o;
                if (csr.TryGetDynamic(name, m, out o)) {
                    return (Variable)o;
                }
                csr = csr.outer;
            }
            return Compartment.Top.AnyP;
        }

        public void LexicalBind(string name, Variable to) {
            Frame csr = this;
            uint m = SubInfo.FilterForName(name);
            while (csr != null) {
                if (csr.TryBindDynamic(name, m, to))
                    return;
                csr = csr.outer;
            }
            if (name == "$!" || name == "$/") return;
            throw new NieczaException("cannot bind " + name + " in " + info.name);
        }

        public static Frame Binder(Frame th) {
            if (th.info.sig == null || th.pos == null) return th;

            int jun_pivot = -1;
            int jun_rank = int.MaxValue;
            string jun_pivot_n = null;

            bool rawc = BindSignature(th, th.info.sig, th.flags, th.info.name,
                    th.pos, th.named, ref jun_pivot, ref jun_pivot_n,
                    ref jun_rank);

            if (rawc && jun_pivot != -1 && (th.flags & CHECK_ONLY) == 0) {
                Variable jct = (jun_pivot == -2 ? th.named[jun_pivot_n] :
                        th.pos[jun_pivot]);
                var sav_named = th.named;
                var sav_pos = th.pos;
                var sav_info = th.info;
                var sav_outer = th.outer;
                var sav_sub = th.sub;
                var sav_disp = th.curDisp;
                Frame nth = th.Return().MakeChild(null, Kernel.AutoThreadSubSI, Compartment.Top.AnyP);

                P6opaque jo  = (P6opaque) jct.Fetch();

                nth.named = sav_named;
                nth.pos = sav_pos;
                nth.lex1 = sav_info;
                nth.lex2 = jun_pivot_n;
                nth.lex3 = jo.slots[0];
                nth.lex4 = Kernel.UnboxAny<Variable[]>((P6any)jo.slots[1]);
                nth.lex5 = sav_outer;
                nth.lex6 = sav_sub;
                nth.lex7 = sav_disp;
                nth.lex8 = new Variable[((Variable[])nth.lex4).Length];
                nth.lex9 = jct;

                nth.lexi0 = jun_pivot;
                nth.lexi1 = 0;

                return nth;
            }

            if ((th.flags & CHECK_ONLY) != 0) {
                th.caller.resultSlot = rawc ? Compartment.Top.TrueV : Compartment.Top.FalseV;
                return th.Return();
            } else {
                return th;
            }
        }

        static bool RunConstraint(Frame th, string signame, Parameter param,
                bool quiet, Variable arg, object c) {
            if (c is Signature) {
                // Sub-signature
                // dummy values; no junctional operation in subsigs
                int jun_pivot = -1, jun_rank = int.MaxValue;
                string jun_pivot_n = null;
                P6any cap = Kernel.RunInferior(arg.Fetch().InvokeMethod(
                    Kernel.GetInferiorRoot(), "Capture", new Variable[] { arg },
                    null)).Fetch();
                return BindSignature(th, (Signature)c,
                        NO_JUNCTION + (quiet ? CHECK_ONLY : 0), param.name,
                        (Variable[])cap.GetSlot(Compartment.Top.CaptureMO, "$!positionals"),
                        (VarHash)cap.GetSlot(Compartment.Top.CaptureMO, "$!named"),
                        ref jun_pivot, ref jun_pivot_n, ref jun_rank);
            }
            if (c is SubInfo) {
                Frame thn = Kernel.GetInferiorRoot()
                    .MakeChild(th, (SubInfo)c, Compartment.Top.AnyP);
                var sm = Kernel.RunInferior(thn);
                bool res = Kernel.ACCEPTS(arg, sm);
                if (!res && !quiet)
                    throw new NieczaException("Constraint type check failed for parameter '" + param.name + "' in '" + signame + "'");
                return res;
            }
            if (c is Variable) {
                bool res = Kernel.ACCEPTS(arg, (Variable)c);
                if (!res && !quiet)
                    throw new NieczaException("Constraint type check failed for parameter '" + param.name + "' in '" + signame + "'");
                return res;
            }
            throw new NieczaException("funny object in post-constraint list");
        }

        public static bool BindSignature(Frame th, Signature sig, int mode,
                string signame, Variable[] pos, VarHash named,
                ref int jun_pivot, ref string jun_pivot_n, ref int jun_rank) {

            Parameter[] pbuf = sig.parms;
            int posc = 0;
            HashSet<string> namedc = null;

            bool quiet = (mode & CHECK_ONLY) != 0;

            if (named != null)
                namedc = new HashSet<string>(named.Keys);
            int pend = pbuf.Length;
            int pix = 0;
            int obj_src = -1;
            string obj_src_n = null;

            while (pix != pend) {
                Parameter param = pbuf[pix++];
                int flags = param.flags;
                int slot = param.slot;
                string[] names = param.names;
                STable type = param.type;
                obj_src = -1;

                Variable src = null;
                if ((flags & Parameter.SLURPY_PCL) != 0) {
                    src = (slot >= 0) ? Kernel.BoxAnyMO(pos, Compartment.Top.ParcelMO) :
                        Compartment.Top.AnyP;
                    posc  = pos.Length;
                    goto gotit;
                }
                if ((flags & Parameter.SLURPY_CAP) != 0) {
                    if (slot < 0) {
                        src = Compartment.Top.AnyP;
                        named = null; namedc = null; posc = pos.Length;
                        goto gotit;
                    }
                    P6any nw = new P6opaque(Compartment.Top.CaptureMO);
                    Variable[] spos = new Variable[pos.Length - posc];
                    Array.Copy(pos, posc, spos, 0, spos.Length);
                    VarHash snamed = null;
                    if (named != null) {
                        snamed = new VarHash();
                        foreach (string k in named.Keys) {
                            if (namedc.Contains(k))
                                snamed[k] = named[k];
                        }
                    }
                    nw.SetSlot(Compartment.Top.CaptureMO, "$!positionals", spos);
                    nw.SetSlot(Compartment.Top.CaptureMO, "$!named", snamed);
                    src = nw;
                    named = null; namedc = null; posc = pos.Length;
                    goto gotit;
                }
                if ((flags & Parameter.SLURPY_POS) != 0) {
                    P6any l = new P6opaque(Compartment.Top.ListMO);
                    Kernel.IterToList(l, Kernel.IterFlatten(
                                Kernel.SlurpyHelper(pos, posc)));
                    src = Kernel.NewRWListVar(l);
                    posc = pos.Length;
                    goto gotit;
                }
                if ((flags & Parameter.SLURPY_NAM) != 0) {
                    VarHash nh = new VarHash();
                    if (named != null) {
                        foreach (KeyValuePair<string,Variable> kv in named)
                            if (namedc.Contains(kv.Key))
                                nh[kv.Key] = kv.Value;
                        named = null;
                        namedc = null;
                    }
                    src = Kernel.BoxAnyMO(nh, Compartment.Top.HashMO);
                    goto gotit;
                }
                if (names != null && named != null) {
                    for (int ni = 0; ni < names.Length; ni++) {
                        string n = names[ni];
                        if (namedc.Contains(n)) {
                            namedc.Remove(n);
                            src = named[n];
                            obj_src_n = n;
                            obj_src = -2;
                            goto gotit;
                        }
                    }
                }
                if ((flags & Parameter.POSITIONAL) != 0 && posc != pos.Length) {
                    obj_src = posc;
                    src = pos[posc++];
                    goto gotit;
                }
get_default:
                if ((flags & Parameter.HASDEFAULT) != 0) {
                    if (param.def is Variable) {
                        src = (Variable)param.def;
                    } else {
                        Frame thn = Kernel.GetInferiorRoot()
                            .MakeChild(th, (SubInfo)param.def, Compartment.Top.AnyP);
                        src = Kernel.RunInferior(thn);
                        if (src == null)
                            throw new Exception("Improper null return from sub default for '" + param.name + "' in '" + signame + "'");
                    }
                    goto gotit;
                }
                if ((flags & Parameter.DEFOUTER) != 0) {
                    Frame f = th;
                    if (th.info.outer_topic_key < 0) {
                        src = Compartment.Top.AnyP;
                        goto gotit;
                    }
                    for (int i = 0; i < th.info.outer_topic_rank; i++) f = f.outer;
                    src = (Variable)f.GetDynamic(th.info.outer_topic_key);
                    goto gotit;
                }
                if ((flags & Parameter.OPTIONAL) != 0) {
                    // Array is the "default" Positional -masak
                    if ((flags & Parameter.IS_LIST) != 0)
                        src = Kernel.CreateArray();
                    else if ((flags & Parameter.IS_HASH) != 0)
                        src = Kernel.CreateHash();
                    else
                        src = type.initObj;
                    goto gotit;
                }
                if (quiet) return false;
                throw new NieczaException("No value for parameter '" + param.name + "' in '" + signame + "'");
gotit:
                switch ((flags & Parameter.DEF_MASK) >> Parameter.DEF_SHIFT) {
                    // TODO: Failure will make these cases different
                    case Parameter.TYPE_ONLY >> Parameter.DEF_SHIFT:
                    case Parameter.UNDEF_ONLY >> Parameter.DEF_SHIFT:
                        if (!src.Fetch().IsDefined())
                            break;
                        if (quiet) return false;
                        throw new NieczaException("Parameter '" + param.name + "' in '" + signame + "' requires an undefined argument");
                    case Parameter.DEF_ONLY >> Parameter.DEF_SHIFT:
                        if (src.Fetch().IsDefined())
                            break;
                        if (quiet) return false;
                        throw new NieczaException("Parameter '" + param.name + "' in '" + signame + "' requires a defined argument");
                    default:
                        break;
                }
                if ((flags & Parameter.RWTRANS) != 0) {
                } else if ((flags & Parameter.IS_COPY) != 0) {
                    if ((flags & Parameter.IS_HASH) != 0)
                        src = Kernel.Assign(Kernel.CreateHash(),
                            Kernel.NewRWListVar(src.Fetch()));
                    else if ((flags & Parameter.IS_LIST) != 0)
                        src = Kernel.Assign(Kernel.CreateArray(),
                            Kernel.NewRWListVar(src.Fetch()));
                    else
                        src = Kernel.Assign(Kernel.NewTypedScalar(type), src);
                } else {
                    bool islist = ((flags & (Parameter.IS_HASH | Parameter.IS_LIST)) != 0);
                    bool rw     = ((flags & Parameter.READWRITE) != 0) && !islist;
                    P6any srco  = src.Fetch();

                    // XXX: in order for calling methods on Compartment.Top.Nil to work,
                    // self needs to be ignored here.
                    if (srco == Compartment.Top.NilP && obj_src != -1 &&
                            (flags & Parameter.INVOCANT) == 0) {
                        obj_src = -1;
                        goto get_default;
                    }
                    if ((flags & Parameter.IS_LIST) != 0)
                        type = Compartment.Top.PositionalMO;
                    if ((flags & Parameter.IS_HASH) != 0)
                        type = Compartment.Top.AssociativeMO;
                    if ((flags & Parameter.CALLABLE) != 0)
                        type = Compartment.Top.CallableMO;
                    if (!srco.Does(type)) {
                        if (quiet) return false;
                        if (srco.mo.HasType(Compartment.Top.JunctionMO) && obj_src != -1 && (mode & NO_JUNCTION) == 0) {
                            int jrank = Kernel.UnboxAny<int>((P6any) ((P6opaque)srco).slots[0]) / 2;
                            if (jrank < jun_rank) {
                                jun_rank = jrank;
                                jun_pivot = obj_src;
                                jun_pivot_n = obj_src_n;
                            }
                            continue;
                        }
                        throw new NieczaException("Nominal type check failed in binding '" + param.name + "' in '" + signame + "'; got " + srco.mo.name + ", needed " + type.name);
                    }

                    if (rw) {
                        if (src.Rw) {
                            // this will be a functional RW binding
                            src.Vivify();
                            goto bound;
                        } else {
                            if (quiet) return false;
                            throw new NieczaException("Binding '" + param.name + "' in '" + signame + "', cannot bind read-only value to is rw parameter");
                        }
                    }
                    else {
                        if (src.Mode == (islist ? Variable.LIST : Variable.RO))
                            goto bound;
                        src = islist ? (Variable)new ListVariable(srco) : srco;
                    }
bound: ;
                }
                if (param.attribute != null) {
                    object self;
                    if (!th.TryGetDynamic("self", 0, out self))
                        throw new NieczaException("No 'self' available for attributive?");
                    Variable selfv = (Variable)self;
                    if (param.attribute[1] == '!') {
                        Kernel.Assign(((Variable)(selfv.Fetch().
                            GetSlot(param.attribute_type, param.attribute))),
                                src);
                    } else {
                        Variable dest = Kernel.RunInferior(selfv.Fetch().
                            InvokeMethod(Kernel.GetInferiorRoot(),
                                param.attribute.Substring(2),
                                new Variable[] { selfv }, null));
                        Kernel.Assign(dest, src);
                    }
                }
                if ((flags & Parameter.INVOCANT) != 0 && th.info.self_key >= 0)
                    th.SetDynamic(th.info.self_key, src);
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
                // Really, this should be earlier (next to the :D/:U checks);
                // it's not right to be binding variables before doing
                // constraint checks.  We're compromising a bit of purity here
                // for the sake of making 'Int $odd where { $odd % 2 }' DWIM.
                if (param.post_constraints != null) {
                    foreach (object c in param.post_constraints)
                        if (!RunConstraint(th, signame, param, quiet, src, c))
                            return false;
                }
            }

            if (posc != pos.Length || namedc != null && namedc.Count != 0) {
                if (quiet) return false;
                string m = "Excess arguments to " + signame;
                if (posc != pos.Length)
                    m += string.Format(", used {0} of {1} positionals",
                            posc, pos.Length);
                if (namedc != null && namedc.Count != 0)
                    m += ", unused named " + Kernel.JoinS(", ", namedc);
                throw new NieczaException(m);
            }

            return true;
        }

        public void PushLeave(int type, P6any thunk) {
            LeaveHook l = new LeaveHook();
            l.next = on_leave; on_leave = l;
            l.thunk = thunk; l.type = type;
        }

        public Frame DynamicCaller() {
            if (coro_return == null)
                return caller;
            return (Frame) coro_return;
        }

        [TrueGlobal]
        private static List<string> spacey = new List<string>();
        public string DepthMark() {
            Frame f = this;
            int ix = 0;
            while (f != null) { ix++; f = f.caller; }
            while (spacey.Count <= ix) { spacey.Add(new String(' ', spacey.Count * 2)); }
            return spacey[ix];
        }

        public override string ToString() {
            return "frame:" + info + "/" + GetHashCode();
        }

        public Frame Return() {
            if (on_leave != null) {
                Variable ret = (Variable) caller.resultSlot;
                bool ok = ret.Fetch().IsDefined();
                LeaveHook c = on_leave;
                // if exception is thrown, do not rerun LEAVEs
                on_leave = null;
                for (; c != null; c = c.next) {
                    if (0 == ((ok ? LeaveHook.KEEP : LeaveHook.UNDO) & c.type))
                        continue;
                    Variable r = Kernel.RunInferior(c.thunk.Invoke(
                        Kernel.GetInferiorRoot(), new Variable[] {ret}, null));
                    if ((c.type & LeaveHook.DIE) != 0 &&
                            !r.Fetch().mo.mro_raw_Bool.Get(r))
                        throw new NieczaException("Post-constraint failed for " + info.name);
                }
            }
            return caller;
        }
        public override void Freeze(FreezeBuffer fb) {
            fb.Byte((byte) SerializationCode.Frame);
            fb.Int(ip);
            // code will be reloaded from info.code
            fb.ObjRef(lex0);
            fb.ObjRef(lex1);
            fb.ObjRef(lex2);
            fb.ObjRef(lex3);
            fb.ObjRef(lex4);
            fb.ObjRef(lex5);
            fb.ObjRef(lex6);
            fb.ObjRef(lex7);
            fb.ObjRef(lex8);
            fb.ObjRef(lex9);
            fb.ObjRef(resultSlot); // probably not necessary
            fb.Int(lexi0);
            fb.Int(lexi1);
            fb.Refs(lexn);
            fb.ObjRef(caller); // XXX this might serialize too much
            fb.ObjRef(outer);
            fb.ObjRef(info);
            // we won't store reuse info :)
            fb.ObjRef(coro_return);
            fb.ObjRef(curDisp);
            fb.ObjRef(rx);
            fb.ObjRef(sub);
            for (LeaveHook l = on_leave; l != null; l = l.next) {
                fb.Byte(checked((byte)l.type));
                fb.ObjRef(l.thunk);
            }
            fb.Byte(LeaveHook.SENTINEL);
            fb.Refs(pos);
            fb.ObjRef(named);
            fb.Byte(checked((byte)flags));
        }
        internal static Frame Thaw(ThawBuffer tb) {
            Frame n = new Frame();
            tb.Register(n);
            n.ip = tb.Int();
            // code will be reloaded from info.code
            n.lex0 = tb.ObjRef();
            n.lex1 = tb.ObjRef();
            n.lex2 = tb.ObjRef();
            n.lex3 = tb.ObjRef();
            n.lex4 = tb.ObjRef();
            n.lex5 = tb.ObjRef();
            n.lex6 = tb.ObjRef();
            n.lex7 = tb.ObjRef();
            n.lex8 = tb.ObjRef();
            n.lex9 = tb.ObjRef();
            n.resultSlot = tb.ObjRef(); // probably not necessary
            n.lexi0 = tb.Int();
            n.lexi1 = tb.Int();
            n.lexn = tb.RefsA<object>();
            n.caller = (Frame)tb.ObjRef(); // XXX this might serialize too much
            n.outer = (Frame)tb.ObjRef();
            n.info = (SubInfo)tb.ObjRef();
            // we won't store reuse info :)
            n.coro_return = (Frame)tb.ObjRef();
            n.curDisp = (DispatchEnt)tb.ObjRef();
            n.rx = (RxFrame)tb.ObjRef();
            n.sub = (P6any)tb.ObjRef();

            LeaveHook fin = null;
            int type = tb.Byte();
            while (type != LeaveHook.SENTINEL) {
                if (fin == null) {
                    fin = n.on_leave = new LeaveHook();
                } else {
                    fin.next = new LeaveHook();
                    fin = fin.next;
                }
                fin.type = type;
                fin.thunk = (P6any) tb.ObjRef();
                type = tb.Byte();
            }

            n.pos = tb.RefsA<Variable>();
            n.named = (VarHash)tb.ObjRef();
            n.flags = tb.Byte();
            tb.PushFixup(n);
            return n;
        }
        void IFixup.Fixup() {
            code = info.code;
        }
    }

    public class NieczaException: Exception {
        // hide clr stack trace for these
        public override string ToString() { return Message; }
        public NieczaException(string detail) : base(detail) {}
        public NieczaException(string fmt, params object[] args) : base(string.Format(fmt, args)) {}
        public NieczaException() : base() {}
    }

    public class ResumeUnwindException: Exception {
        public readonly int type, to_ip;
        public readonly Frame to_frame;
        public readonly object to_data;
        public readonly string p6backtrace;

        public override string ToString() { return p6backtrace; }

        public ResumeUnwindException(int type, Frame tf, int tip, object td,
                string bt) : base(bt) {
            p6backtrace = bt;
            to_data = td;
            to_ip = tip;
            to_frame = tf;
            this.type = type;
        }
    }

    class InvokeSub : InvokeHandler {
        public override Frame Invoke(P6any th, Frame caller,
                Variable[] pos, VarHash named) {
            if (!th.IsDefined())
                return Kernel.Die(caller, "Cannot invoke an undef sub");
            P6opaque dyo = ((P6opaque) th);
            Frame outer = (Frame) dyo.slots[0];
            SubInfo info = (SubInfo) dyo.slots[1];

            return info.SetupCall(caller, outer, th, pos, named, false, null);
        }
    }

    class InvokeCallMethod : InvokeHandler {
        public override Frame Invoke(P6any th, Frame caller,
                Variable[] pos, VarHash named) {
            Variable[] np = new Variable[pos.Length + 1];
            Array.Copy(pos, 0, np, 1, pos.Length);
            np[0] = th;
            return th.InvokeMethod(caller, "postcircumfix:<( )>", np, named);
        }
    }

    class PushyCallMethod : PushyHandler {
        string method;
        public PushyCallMethod(string method) { this.method = method; }
        public PushyCallMethod() { }
        protected override object[] GetData() { return new object[]{method};}
        protected override void SetData(object[]o) { method = (string)o[0]; }

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
        public CtxCallMethodUnbox() { }
        protected override object[] GetData() { return new object[]{method};}
        protected override void SetData(object[]o) { method = (string)o[0]; }

        public override T Get(Variable obj) {
            Variable v = Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, new Variable[] { obj }, null));
            return Kernel.UnboxAny<T>(v.Fetch());
        }
    }
    class CtxCallMethodUnboxBool : ContextHandler<bool> {
        string method;
        public CtxCallMethodUnboxBool(string method) { this.method = method; }
        public CtxCallMethodUnboxBool() { }
        protected override object[] GetData() { return new object[]{method};}
        protected override void SetData(object[]o) { method = (string)o[0]; }

        public override bool Get(Variable obj) {
            Variable v = Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, new Variable[] { obj }, null));
            return Kernel.UnboxAny<int>(v.Fetch()) != 0;
        }
    }

    class CtxCallMethodUnboxNumeric : ContextHandler<double> {
        string method;
        public CtxCallMethodUnboxNumeric(string method) { this.method = method; }
        public CtxCallMethodUnboxNumeric() { }
        protected override object[] GetData() { return new object[]{method};}
        protected override void SetData(object[]o) { method = (string)o[0]; }

        public override double Get(Variable obj) {
            Variable v = method == null ? obj : Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, new Variable[] { obj }, null));
            P6any o = v.Fetch();
            if (o.mo.HasType(Compartment.Top.NumMO)) {
                return Kernel.UnboxAny<double>(o);
            } else if (o.mo.HasType(Compartment.Top.IntMO)) {
                if (o is BoxObject<int>) {
                    return (double)Kernel.UnboxAny<int>(o);
                } else {
                    return (double)Kernel.UnboxAny<BigInteger>(o);
                }
            } else if (o.mo.HasType(Compartment.Top.RatMO)) {
                Rat r = Kernel.UnboxAny<Rat>(o);
                return (double)r.num / (double)r.den;
            } else if (o.mo.HasType(Compartment.Top.FatRatMO)) {
                FatRat r = Kernel.UnboxAny<FatRat>(o);
                return (double)r.num / (double)r.den;
            } else if (o.mo.HasType(Compartment.Top.ComplexMO)) {
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
        public CtxCallMethod() { }
        protected override object[] GetData() { return new object[]{method};}
        protected override void SetData(object[]o) { method = (string)o[0]; }

        public override Variable Get(Variable obj) {
            return Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, new Variable[] { obj }, null));
        }
    }

    class CtxCallMethodFetch : ContextHandler<P6any> {
        string method;
        public CtxCallMethodFetch(string method) { this.method = method; }
        public CtxCallMethodFetch() { }
        protected override object[] GetData() { return new object[]{method};}
        protected override void SetData(object[]o) { method = (string)o[0]; }

        public override P6any Get(Variable obj) {
            return Kernel.RunInferior(obj.Fetch().InvokeMethod(Kernel.GetInferiorRoot(), method, new Variable[] { obj }, null)).Fetch();
        }
    }

    class CtxJustUnbox<T> : ContextHandler<T> {
        T dflt;
        public CtxJustUnbox(T dflt) { this.dflt = dflt; }
        public CtxJustUnbox() { }
        protected override object[] GetData() { return new object[]{dflt};}
        protected override void SetData(object[]o) { dflt = (T)o[0]; }
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
            return obj.Fetch();
        }
    }

    class CtxReturnSelfList : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            if (obj.List) return obj;
            return Kernel.NewRWListVar(obj.Fetch());
        }
    }

    class CtxReturnSelfItem : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            if (!obj.List) return obj;
            return obj.Fetch();
        }
    }

    class CtxAnyList : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            VarDeque itr = new VarDeque(obj.List ? obj.Fetch() : obj);
            P6any l = new P6opaque(Compartment.Top.ListMO);
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
                if (d.List) {
                    srcs[i] = new P6opaque(Compartment.Top.ListMO);
                    Kernel.IterToList(srcs[i], src);
                    src = new VarDeque();
                } else {
                    srcs[i] = Kernel.IterHasFlat(src, true) ?
                        src.Shift().Fetch() : Compartment.Top.AnyP;
                }
            }

            for (int i = 0; i < dsts.Length; i++) {
                dsts[i].AssignO(srcs[i], true);
            }

            return lhs;
        }
    }
    class CtxParcelList : ContextHandler<Variable> {
        public override Variable Get(Variable obj) {
            P6any o = obj.Fetch();
            VarDeque itr = o.IsDefined() ? new VarDeque(Kernel.UnboxAny<Variable[]>(o)) : new VarDeque();
            P6any l = new P6opaque(Compartment.Top.ListMO);
            Kernel.IterToList(l, itr);
            return Kernel.NewRWListVar(l);
        }
    }
    // used for both
    class CtxParcelListStr : ContextHandler<string> {
        public override string Get(Variable obj) {
            P6any o = obj.Fetch();
            VarDeque itr = o.mo.mro_raw_iterator.Get(obj);
            StringBuilder sb = new StringBuilder();
            while (Kernel.IterHasFlat(itr, true)) {
                var n = itr.Shift();
                sb.Append(n.Fetch().mo.mro_raw_Str.Get(n)).Append(' ');
            }
            if (sb.Length != 0) sb.Length--;
            return sb.ToString();
        }
    }

    class CtxBoxify<T> : ContextHandler<Variable> {
        ContextHandler<T> inner;
        STable box;
        public CtxBoxify() { }
        protected override object[] GetData() {
            return new object[] { inner, box };
        }
        protected override void SetData(object[] o) {
            inner = (ContextHandler<T>)o[0];
            box = (STable)o[1];
        }
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
        public CtxBoxifyInty() { }
        protected override object[] GetData() {
            return new object[] { inner };
        }
        protected override void SetData(object[] o) {
            inner = (ContextHandler<double>)o[0];
        }
        public CtxBoxifyInty(ContextHandler<double> inner) {
            this.inner = inner;
        }
        public override Variable Get(Variable obj) {
            return Builtins.MakeInt((long)inner.Get(obj));
        }
    }

    class CtxContainerize : ContextHandler<Variable> {
        ContextHandler<P6any> inner;
        public CtxContainerize() { }
        protected override object[] GetData() {
            return new object[] { inner };
        }
        protected override void SetData(object[] o) {
            inner = (ContextHandler<P6any>)o[0];
        }
        public CtxContainerize(ContextHandler<P6any> inner) {
            this.inner = inner;
        }
        public override Variable Get(Variable obj) {
            return inner.Get(obj);
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
            lhs_o.SetSlot(Compartment.Top.ListMO, "$!items", items);
            lhs_o.SetSlot(Compartment.Top.ListMO, "$!rest", iter); /*now empty*/
            return lhs;
        }
    }
    class CtxListIterator : ContextHandler<VarDeque> {
        public override VarDeque Get(Variable obj) {
            var d = obj.Fetch();
            if (!d.IsDefined()) return new VarDeque();
            VarDeque r = new VarDeque( (VarDeque) d.GetSlot(Compartment.Top.ListMO, "$!items" ));
            r.PushD((VarDeque) d.GetSlot(Compartment.Top.ListMO, "$!rest"));
            return r;
        }
    }

    class PopList : ContextHandler<Variable> {
        public override Variable Get(Variable v) {
            P6any o = v.Fetch();
            if (!o.IsDefined()) return Compartment.Top.AnyP;
            VarDeque items = (VarDeque)o.GetSlot(Compartment.Top.ListMO, "$!items");
            VarDeque rest = (VarDeque)o.GetSlot(Compartment.Top.ListMO, "$!rest");
            while (Kernel.IterHasFlat(rest, false))
                items.Push(rest.Shift());
            return (items.Count() != 0) ? items.Pop() : Compartment.Top.AnyP;
        }
    }
    class ShiftList : ContextHandler<Variable> {
        public override Variable Get(Variable v) {
            P6any o = v.Fetch();
            if (!o.IsDefined()) return Compartment.Top.AnyP;
            VarDeque items = (VarDeque)o.GetSlot(Compartment.Top.ListMO, "$!items");
            VarDeque rest = (VarDeque)o.GetSlot(Compartment.Top.ListMO, "$!rest");
            if (items.Count() != 0)
                return items.Shift();
            if (Kernel.IterHasFlat(rest, false))
                return rest.Shift();
            return Compartment.Top.AnyP;
        }
    }
    class UnshiftList : PushyHandler {
        public override Variable Invoke(Variable v, Variable[] args) {
            P6any o = v.Fetch();
            if (!o.IsDefined())
                throw new NieczaException("Cannot push onto type object");
            VarDeque iter = new VarDeque(args);
            VarDeque targ = (VarDeque)o.GetSlot(Compartment.Top.ListMO, "$!items");
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
            VarDeque targ = (VarDeque)o.GetSlot(Compartment.Top.ListMO, "$!rest");
            if (targ.Count() == 0) targ = (VarDeque)o.GetSlot(Compartment.Top.ListMO, "$!items");
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
                if (first && elt.mo.HasType(Compartment.Top.HashMO)) {
                    foreach(KeyValuePair<string,Variable> kv in
                            Kernel.UnboxAny<VarHash>(elt)) {
                        into[kv.Key] = kv.Value;
                    }
                } else if (elt.mo.HasType(Compartment.Top.PairMO)) {
                    Variable k = (Variable) elt.GetSlot(Compartment.Top.EnumMO, "$!key");
                    Variable v = (Variable) elt.GetSlot(Compartment.Top.EnumMO, "$!value");
                    into[k.Fetch().mo.mro_raw_Str.Get(k)] =
                        Kernel.NewMuScalar(v.Fetch());
                } else {
                    if (!Kernel.IterHasFlat(iter, true))
                        throw new NieczaException("Unmatched key in Hash.LISTSTORE");
                    into[elt.mo.mro_raw_Str.Get(elt)] =
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
            return obj.Fetch().IsDefined() ? Compartment.Top.TrueV : Compartment.Top.FalseV;
        }
    }

    class CtxNumSuccish : ContextHandler<P6any> {
        double amt;
        public CtxNumSuccish() { }
        protected override object[] GetData() { return new object[] { amt }; }
        protected override void SetData(object[] o) { amt = (double) o[0]; }
        public CtxNumSuccish(double amt) { this.amt = amt; }
        public override P6any Get(Variable obj) {
            P6any o = obj.Fetch();
            double v = o.IsDefined() ? Kernel.UnboxAny<double>(o) : 0;
            return Kernel.BoxRaw(v + amt, Compartment.Top.NumMO);
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
        public CtxIntSuccish() { }
        protected override object[] GetData() { return new object[] { amt }; }
        protected override void SetData(object[] o) { amt = (int) o[0]; }
        public CtxIntSuccish(int amt) { this.amt = amt; }
        public override P6any Get(Variable obj) {
            P6any o = obj.Fetch();
            int v;
            if (o is BoxObject<BigInteger>) {
                BigInteger bn = Kernel.UnboxAny<BigInteger>(o) + amt;
                if (bn.AsInt32(out v))
                    return Kernel.BoxRaw<int>(v, Compartment.Top.IntMO);
                else
                    return Kernel.BoxRaw<BigInteger>(bn, Compartment.Top.IntMO);
            }
            v = o.IsDefined() ? Kernel.UnboxAny<int>(o) : 0;
            if (v == (amt > 0 ? int.MaxValue : int.MinValue))
                return Kernel.BoxRaw<BigInteger>(amt + (long)v, Compartment.Top.IntMO);
            return Kernel.BoxRaw(v + amt, Compartment.Top.IntMO);
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
        public CtxRatSuccish() { }
        protected override object[] GetData() { return new object[] { up }; }
        protected override void SetData(object[] o) { up = (bool) o[0]; }
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
            return Kernel.BoxRaw<Rat>(rr, Compartment.Top.RatMO);
        }
    }
    class CtxRatStr : ContextHandler<string> {
        public override string Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) { return o.mo.name + "()"; }
            Rat r = Kernel.UnboxAny<Rat>(o);
            return Utils.N2S(((double)r.num) / ((double)r.den));
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
        public CtxFatRatSuccish() { }
        protected override object[] GetData() { return new object[] { up }; }
        protected override void SetData(object[] o) { up = (bool) o[0]; }
        public override P6any Get(Variable obj) {
            P6any o = obj.Fetch();
            FatRat rr;
            if (o.IsDefined()) {
                FatRat ir = Kernel.UnboxAny<FatRat>(o);
                rr = new FatRat(up ? ir.num + ir.den : ir.num - ir.den, ir.den);
            } else {
                rr = new FatRat(up ? BigInteger.One : BigInteger.MinusOne, BigInteger.One);
            }
            return Kernel.BoxRaw<FatRat>(rr, Compartment.Top.FatRatMO);
        }
    }
    class CtxFatRatStr : ContextHandler<string> {
        public override string Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) { return o.mo.name + "()"; }
            FatRat r = Kernel.UnboxAny<FatRat>(o);
            return Utils.N2S(((double)r.num) / ((double)r.den));
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
        public CtxComplexSuccish() { }
        protected override object[] GetData() { return new object[] { amt }; }
        protected override void SetData(object[] o) { amt = (double) o[0]; }
        public override P6any Get(Variable obj) {
            P6any o = obj.Fetch();
            Complex c = o.IsDefined() ? Kernel.UnboxAny<Complex>(o) : null;
            c = (c == null) ? new Complex(amt, 0) : new Complex(c.re+amt, c.im);
            return Kernel.BoxRaw(c, Compartment.Top.ComplexMO);
        }
    }
    class CtxComplexStr : ContextHandler<string> {
        public override string Get(Variable obj) {
            P6any o = obj.Fetch();
            if (!o.IsDefined()) { return o.mo.name + "()"; }
            Complex r = Kernel.UnboxAny<Complex>(o);
            var sb = Utils.N2S(r.re) + (r.im < 0 ? "-" : "+") + Utils.N2S(Math.Abs(r.im));
            return char.IsLetter(sb[sb.Length-1]) ? sb + "\\i" : sb + "i";
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
        public CtxStrSuccish() { }
        protected override object[] GetData() { return new object[] { succ }; }
        protected override void SetData(object[] o) { succ = (bool) o[0]; }
        // note that most of this table is katakana.  Perhaps there
        // is a better way.
        [Immutable]
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
            if (!obj_o.IsDefined()) return Kernel.BoxRaw("WTF", Compartment.Top.StrMO);
            string src = Kernel.UnboxAny<string>(obj_o);
            int right = src.Length;
tryagain:
            while (right != 0 && !Digitish(src[right-1])) right--;
            if (right == 0) return Kernel.BoxRaw("WTF", Compartment.Top.StrMO);
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
                    Compartment.Top.StrMO);
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
            return Kernel.BoxAnyMO<string>(o.IsDefined() ? Utils.N2S(Kernel.UnboxAny<double>(o)) : "Num()", Compartment.Top.StrMO);
        }
    }

    class IxCallMethod : IndexHandler {
        string name;
        VarHash named;
        public IxCallMethod() { }
        protected override object[] GetData() {
            return new object[] { name, named };
        }
        protected override void SetData(object[] o) {
            name = (string) o[0];
            named = (VarHash) o[1];
        }
        public IxCallMethod(string name, string adv) {
            this.name = name;
            if (adv != null) {
                named = new VarHash();
                named[adv] = Compartment.Top.TrueV;
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
        public KeySlicer() { }
        protected override object[] GetData() {
            return new object[] { mode, bas };
        }
        protected override void SetData(object[] o) {
            mode = (int) o[0];
            bas  = (IndexHandler) o[1];
        }
        public KeySlicer(int mode, object bas) {
            this.mode = mode; this.bas = (IndexHandler)bas;
        }

        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.List || !ks.mo.is_any && ks.mo.HasType(Compartment.Top.JunctionMO))
                return Slice(obj, key);
            else if (ks.mo.HasType(Compartment.Top.WhateverMO))
                return GetAll(obj);

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
            if (key.List || !ks.mo.is_any && ks.mo.HasType(Compartment.Top.JunctionMO))
                return Slice(obj, key);

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return Compartment.Top.AnyP;
            return Kernel.RunInferior(os.InvokeMethod(Kernel.GetInferiorRoot(),
                "delete_key", new Variable[] { obj, key }, null));
        }
    }
    class IxAnyExistsKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.List || !ks.mo.is_any && ks.mo.HasType(Compartment.Top.JunctionMO))
                return Slice(obj, key);

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return Compartment.Top.FalseV;
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
            obj.Store(Kernel.BoxRaw(new VarHash(), Compartment.Top.HashMO));
            return Compartment.Top.HashMO.mro_bind_key.Bind(obj, key, to);
        }
    }
    class IxAnyBindPos : BindHandler {
        public override Variable Bind(Variable obj, Variable key, Variable to) {
            P6any os = obj.Fetch();
            if (os.IsDefined())
                return Kernel.RunInferior(os.InvokeMethod(Kernel.GetInferiorRoot(),
                    "bind_pos", new Variable[] { obj, key, to }, null));
            obj.Store(Kernel.CreateArray().Fetch());
            return Compartment.Top.ArrayMO.mro_bind_key.Bind(obj, key, to);
        }
    }
    class IxAnyAtKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.List || !ks.mo.is_any && ks.mo.HasType(Compartment.Top.JunctionMO))
                return Slice(obj, key);
            else if (ks.mo.HasType(Compartment.Top.WhateverMO))
                return GetAll(obj);

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
            if (key.List || !ks.mo.is_any && ks.mo.HasType(Compartment.Top.JunctionMO))
                return Slice(obj, key);
            else if (ks.mo.HasType(Compartment.Top.WhateverMO))
                return GetAll(obj);

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return IndexHandler.ViviArray(obj, key);
            if (ks.mo != Compartment.Top.IntMO && ks.mo.HasType(Compartment.Top.CodeMO)) {
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
            if (key.List || !ks.mo.is_any && ks.mo.HasType(Compartment.Top.JunctionMO))
                return Slice(obj, key);
            else if (ks.mo.HasType(Compartment.Top.WhateverMO))
                return GetAll(obj);
            P6any o = obj.Fetch();
            if (!o.IsDefined())
                return Compartment.Top.AnyP;

            Cursor os = (Cursor)o;
            return os.GetKey(ks.mo.mro_raw_Str.Get(key));
        }
    }
    class IxCursorAtPos : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.List || !ks.mo.is_any && ks.mo.HasType(Compartment.Top.JunctionMO))
                return Slice(obj, key);
            else if (ks.mo.HasType(Compartment.Top.WhateverMO))
                return GetAll(obj);

            P6any o = obj.Fetch();
            if (!o.IsDefined())
                return Compartment.Top.AnyP;

            Cursor os = (Cursor)o;
            return os.GetKey(Utils.N2S(ks.mo.mro_raw_Numeric.Get(key)));
        }
    }

    class IxHashBindKey : BindHandler {
        public override Variable Bind(Variable obj, Variable key, Variable to) {
            if (key.List)
                throw new NieczaException("Cannot bind to a hash slice");
            P6any ks = key.Fetch();
            P6any os = obj.Fetch();
            if (!os.IsDefined())
                obj.Store(os = Kernel.BoxRaw(new VarHash(), Compartment.Top.HashMO));
            string kss = ks.mo.mro_raw_Str.Get(key);
            VarHash h = Kernel.UnboxAny<VarHash>(os);

            return h[kss] = to;
        }
    }
    class IxHashAtKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.List || !ks.mo.is_any && ks.mo.HasType(Compartment.Top.JunctionMO))
                return Slice(obj, key);
            else if (ks.mo.HasType(Compartment.Top.WhateverMO))
                return GetAll(obj);

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return IndexHandler.ViviHash(obj, key);
            string kss = ks.mo.mro_raw_Str.Get(key);
            VarHash h = Kernel.UnboxAny<VarHash>(os);
            Variable r;
            if (h.TryGetValue(kss, out r))
                return r;
            return new RWVariable(null, new HashViviHook(os, kss),
                    Compartment.Top.AnyP);
        }
    }
    class IxHashExistsKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.List || !ks.mo.is_any && ks.mo.HasType(Compartment.Top.JunctionMO))
                return Slice(obj, key);
            P6any os = obj.Fetch();
            if (!os.IsDefined()) return Compartment.Top.FalseV;
            string kss = ks.mo.mro_raw_Str.Get(key);
            VarHash h = Kernel.UnboxAny<VarHash>(os);
            return h.ContainsKey(kss) ? Compartment.Top.TrueV : Compartment.Top.FalseV;
        }
    }
    class IxHashDeleteKey : IndexHandler {
        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.List || !ks.mo.is_any && ks.mo.HasType(Compartment.Top.JunctionMO))
                return Slice(obj, key);
            P6any os = obj.Fetch();
            if (!os.IsDefined()) return Compartment.Top.AnyP;
            string kss = ks.mo.mro_raw_Str.Get(key);
            VarHash h = Kernel.UnboxAny<VarHash>(os);
            Variable r;
            if (h.TryGetValue(kss, out r)) {
                h.Remove(kss);
                return r;
            } else {
                return Compartment.Top.AnyP;
            }
        }
    }

    class IxListAtPos : IndexHandler {
        bool extend;
        public IxListAtPos(bool extend) { this.extend = extend; }
        public IxListAtPos() { }
        protected override object[] GetData() { return new object[] { extend};}
        protected override void SetData(object[] o) { extend = (bool)o[0]; }

        public override Variable Get(Variable obj, Variable key) {
            P6any ks = key.Fetch();
            if (key.List || !ks.mo.is_any && ks.mo.HasType(Compartment.Top.JunctionMO))
                return Slice(obj, key);
            else if (ks.mo.HasType(Compartment.Top.WhateverMO))
                return GetAll(obj);

            P6any os = obj.Fetch();
            if (!os.IsDefined())
                return IndexHandler.ViviArray(obj, key);

            P6opaque dos = (P6opaque) os;
            VarDeque items = (VarDeque) dos.slots[0];
            VarDeque rest  = (VarDeque) dos.slots[1];

            if (ks.mo != Compartment.Top.IntMO && ks.mo.HasType(Compartment.Top.CodeMO)) {
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
                return Compartment.Top.AnyP;
            if (items.Count() <= ix) {
                if (extend) {
                    return new RWVariable(null, new ArrayViviHook(os, ix),
                            Compartment.Top.AnyP);
                } else {
                    return Compartment.Top.AnyP;
                }
            }
            return items[ix];
        }
    }

    class IxListBindPos : BindHandler {
        public override Variable Bind(Variable obj, Variable key, Variable to) {
            if (key.List)
                throw new NieczaException("Cannot bind to a list slice");
            P6any ks = key.Fetch();
            P6any os = obj.Fetch();
            if (!os.IsDefined())
                obj.Store(os = Kernel.CreateArray().Fetch());

            P6opaque dos = (P6opaque) os;
            VarDeque items = (VarDeque) dos.slots[0];
            VarDeque rest  = (VarDeque) dos.slots[1];

            if (ks.mo != Compartment.Top.IntMO && ks.mo.HasType(Compartment.Top.CodeMO)) {
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
            return items[ix] = to;
        }
    }

    public struct StashCursor {
        public const int WHO  = 0; // use p1(P6any)
        public const int LEX  = 1; // p1(Frame) p2(int, depth)
        public const int ROOT = 2; // p1(Frame) p2(int)
        public const int DYNA = 3; // p1&p2
        public const int CLR  = 4; // p1(string)

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
            st.typeObj = st.initObj = new P6opaque(st, 0);
            ((P6opaque)st.typeObj).slots = null;
            st.mo.type  = P6how.PACKAGE;
            st.mo.rtype = "package";
            // XXX should be PackageHOW
            st.how = new BoxObject<STable>(st, Compartment.Top.ClassHOWMO, 0);
            st.mo.Revalidate();
            st.SetupVTables();
            return st.typeObj;
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

        // It seems more useful to me to have *.foo($CALLER::x) interpret
        // CALLER as foo($CALLER::x) would; so, this always uses OUTER
        // semantics.
        // TODO: account for the possibility of transparent inline frames
        void SkipTransparent() {
            if (p2 != 0) return;
            if (p1 == null) return;
            while (p1 != null) {
                Frame f = ((Frame) p1);
                if ((f.info.special & SubInfo.TRANSPARENT) == 0)
                    return;
                p1 = f.outer;
            }
        }

        StashCursor ToCaller() {
            StashCursor r = this;
            Frame f = (Frame)p1;
            if (r.p2 > 0) r.p2--;
            else {
                f = f.caller;
                if (f == null)
                    throw new NieczaException("No more calling frames");
                r.p1 = f;
                r.p2 = f.info.FindControlEnt(f.ip, SubInfo.ON_VARLOOKUP, null);
                if (r.p2 < 0) r.p2 = 0;
            }
            r.SkipTransparent();
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
            r.SkipTransparent();
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
                StashEnt bv;

                if (Kernel.currentGlobals.TryGetValue("\x8::GLOBAL" + key, out bv) ||
                        Kernel.currentGlobals.TryGetValue("\x9::PROCESS" + key, out bv)) {
                    if (rbar_w) { bv.Bind(o); } else { o = bv.v; }
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

        public static P6any MakeCLR_WHO(string name) {
            StashCursor sc = default(StashCursor);
            sc.type = CLR;
            sc.p1 = name;
            P6any who = Kernel.BoxRaw(sc, Compartment.Top.PseudoStashMO);
            who.SetSlot(Compartment.Top.PseudoStashMO, "$!name", Kernel.BoxAnyMO(name, Compartment.Top.StrMO));
            return who;
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
                v = Compartment.Top.AnyP;
                goto have_v;
            }
            else if (type == CLR) {
                if (Kernel.SaferMode)
                    throw new NieczaException("CLR objects may not be used directly in safe mode");
                if (bind_to != null)
                    throw new NieczaException("Cannot bind interop namespaces");
                v = CLRWrapperProvider.GetNamedWrapper((string)p1 + "." + key).typeObj;
                goto have_v;
            }
            else if (type == WHO) {
                // only special type is PARENT, maybe not even that?
                P6any who = (P6any) p1;
                Variable keyv = Kernel.BoxAnyMO(key, Compartment.Top.StrMO);
                if (bind_to != null) {
                    v = who.mo.mro_bind_key.Bind(who, keyv, bind_to);
                    return;
                }
                v = who.mo.mro_at_key.Get(who, keyv);

                if (final) return;

                if (v.Rw && !v.Fetch().IsDefined()) {
                    if (!who.Isa(Compartment.Top.StashMO))
                        throw new NieczaException("Autovivification only implemented for normal-type stashes");
                    string name = Kernel.UnboxAny<string>(who);
                    P6any new_who = Kernel.BoxRaw(name + "::" + key, Compartment.Top.StashMO);
                    who.mo.mro_bind_key.Bind(who, keyv,
                        MakePackage(key, new_who));
                    sc.p1 = new_who;
                    return;
                }
                else if (v.Rw || v.Fetch().IsDefined()) {
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
                    v = ToInfo().cur_pkg.typeObj;
                    goto have_v;
                } else if (key == "GLOBAL") {
                    sc.p1 = Kernel.GetVar("", "GLOBAL").v.Fetch().mo.who;
                    sc.type = WHO;
                    goto have_sc;
                } else if (key == "PROCESS") {
                    sc.p1 = Kernel.GetVar("", "PROCESS").v.Fetch().mo.who;
                    sc.type = WHO;
                    goto have_sc;
                } else if (key == "UNIT" || key == "OUTER" ||
                        key == "SETTING" || key == "CALLER") {
                    StashCursor n = sc;
                    n.type = LEX;
                    n.SkipTransparent();
                    n.Core(key, final, out sc, out v, bind_to);
                    return;
                } else if (key == "CORE") {
                    sc.type = LEX;
                    while (sc.ToInfo().unit == null ||
                            sc.ToInfo().unit.name != "CORE") sc = sc.ToOuter();
                    goto have_sc;
                } else if (key == "MY") {
                    sc.type = LEX;
                    sc.SkipTransparent();
                    goto have_sc;
                } else if (key == "COMPILING") {
                    throw new NieczaException("COMPILING NYI");
                } else if (key == "DYNAMIC") {
                    sc.type = DYNA;
                    goto have_sc;
                } else if (key == "CLR") {
                    sc.type = CLR;
                    sc.p1 = "";
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
                        ? ToInfo().cur_pkg.who
                        : Kernel.GetVar("", "GLOBAL").v.Fetch().mo.who;
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
                                SubInfo.MAINLINE) == 0)
                        sc = sc.ToOuter();
                    goto have_sc;
                }
                else if (key == "CALLER") {
                    sc = sc.ToCaller();
                    goto have_sc;
                }
                else if (key == "SETTING") {
                    while (sc.p2 > 0 || (sc.ToInfo().special &
                                SubInfo.MAINLINE) == 0)
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
                    v = Compartment.Top.AnyP;
                    goto have_v;
                }
            }
            else {
                throw new NieczaException("corrupt StashCursor");
            }

have_sc:
            if (!final) return;
            if (bind_to != null)
                throw new NieczaException("cannot bind a pseudo package");
            {
                P6any who = Kernel.BoxRaw(this, Compartment.Top.PseudoStashMO);
                who.SetSlot(Compartment.Top.PseudoStashMO, "$!name", Kernel.BoxAnyMO(key, Compartment.Top.StrMO));
                v = MakePackage(key, who);
            }
            return;

have_v:
            if (final) return;
            if (v.Rw || v.Fetch().IsDefined())
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
                    return (P6any) sc.p1;
                P6any who = Kernel.BoxRaw(sc, Compartment.Top.PseudoStashMO);
                who.SetSlot(Compartment.Top.PseudoStashMO, "$!name", Kernel.BoxAnyMO(last, Compartment.Top.StrMO));
                return who;
            }
            if (bind_to != null) {
                bool list = key != "" && (key[0] == '@' || key[0] == '%');
                bind_to = Kernel.NewBoundVar(list ? Kernel.NBV_LIST :
                    bind_ro ? Kernel.NBV_RO : Kernel.NBV_RW, Compartment.Top.MuMO,
                    bind_to); // XXX should use types maybe?
            }
            sc.Core(key, true, out r, out v, bind_to);
            return v;
        }
    }

    class Compartment {
        [Immutable] static readonly FieldInfo[] fieldsToSave;

        [CORESaved] public STable PairMO;
        [CORESaved] public STable EnumMO;
        [CORESaved] public STable EnumMapMO;
        [CORESaved] public STable CallFrameMO;
        [CORESaved] public STable CaptureMO;
        [CORESaved] public STable GatherIteratorMO;
        [CORESaved] public STable IterCursorMO;
        [CORESaved] public P6any NilP;
        [CORESaved] public ListVariable Nil;
        [CORESaved] public P6any AnyP;
        [CORESaved] public P6any ArrayP;
        [CORESaved] public P6any EMPTYP;
        [CORESaved] public P6any HashP;
        [CORESaved] public P6any IteratorP;
        [CORESaved] public STable RealMO;
        [CORESaved] public STable IntegralMO;
        [CORESaved] public STable JunctionMO;
        [CORESaved] public STable LabelMO;
        [CORESaved] public STable AnyMO;
        [CORESaved] public STable IteratorMO;
        [CORESaved] public STable ScalarMO;
        [CORESaved] public STable StashMO;
        [CORESaved] public STable ClassHOWMO;
        [CORESaved] public STable PseudoStashMO;
        [CORESaved] public STable GrammarMO;
        [CORESaved] public STable PositionalMO;
        [CORESaved] public STable AssociativeMO;
        [CORESaved] public STable CallableMO;
        [CORESaved] public STable CodeMO;
        [CORESaved] public STable WhateverMO;
        [CORESaved] public STable WhateverCodeMO;
        [CORESaved] public STable RoutineMO;
        [CORESaved] public STable SubMO;
        [CORESaved] public STable SubmethodMO;
        [CORESaved] public STable MethodMO;
        [CORESaved] public STable BlockMO;
        [CORESaved] public STable RegexMO;
        [CORESaved] public STable StrMO;
        [CORESaved] public STable PseudoStrMO;
        [CORESaved] public STable NumMO;
        [CORESaved] public STable IntMO;
        [CORESaved] public STable RatMO;
        [CORESaved] public STable FatRatMO;
        [CORESaved] public STable ComplexMO;
        [CORESaved] public STable ArrayMO;
        [CORESaved] public STable CursorMO;
        [CORESaved] public STable MatchMO;
        [CORESaved] public STable ParcelMO;
        [CORESaved] public STable ListMO;
        [CORESaved] public STable HashMO;
        [CORESaved] public STable BoolMO;
        [CORESaved] public STable MuMO;
        [CORESaved] public STable ParameterMO;
        [CORESaved] public STable SignatureMO;
        [CORESaved] public P6any StashP;

        [CORESaved] public BoxObject<int> TrueV;
        [CORESaved] public BoxObject<int> FalseV;

        static Compartment() {
            var typesToCheck = new Type[] {
                typeof(Kernel), typeof(RuntimeUnit), typeof(CLRWrapperProvider),
                typeof(RxFrame), typeof(Builtins)
            };

            List<FieldInfo> fields = new List<FieldInfo>();
            foreach (Type ty in typesToCheck) {
                foreach (FieldInfo fi in ty.GetFields(BindingFlags.NonPublic |
                            BindingFlags.Public | BindingFlags.Static)) {
                    foreach (object o in fi.GetCustomAttributes(true)) {
                        if (o is CORESavedAttribute) goto saveme;
                        if (o is CompartmentGlobalAttribute) goto saveme;
                    }
                    continue;
saveme:
                    fields.Add(fi);
                }
            }

            fieldsToSave = fields.ToArray();

            AppDomain.CurrentDomain.ProcessExit += BeforeExit;
        }

        [TrueGlobal] internal static bool disable_end;
        internal static void BeforeExit(object sender, EventArgs args) {
            if (disable_end) return;
            disable_end = true;
            while (Top.prev != null) { Pop(); }
            Top.end.Run();
            // XXX is this really the best place?
            Builtins.stdout.Flush();
            Builtins.stderr.Flush();
        }

        Compartment prev;
        object[] saved_values;
        private Compartment() { }

        [TrueGlobal]
        public static Compartment Top = new Compartment();

        public PhaserList check = new PhaserList(true);
        public PhaserList init  = new PhaserList(false);
        public PhaserList end   = new PhaserList(true);

        internal static void Push() {
            Compartment n = new Compartment();
            n.prev = Top;
            n.saved_values = new object[fieldsToSave.Length];

            for (int i = 0; i < fieldsToSave.Length; i++) {
                n.saved_values[i] = fieldsToSave[i].GetValue(null);
                fieldsToSave[i].SetValue(null, null);
            }

            Kernel.InitCompartment();
            Top = n;
        }

        internal static void Pop() {
            Top.end.Run();

            for (int i = 0; i < fieldsToSave.Length; i++) {
                fieldsToSave[i].SetValue(null, Top.saved_values[i]);
            }

            if (Kernel.containerRootUnit != null) {
                foreach (RuntimeUnit ru in Kernel.containerRootUnit.depended_units)
                    ru.SetConstants();
            }
            Top = Top.prev;
        }
    }

    class PhaserList {
        bool lifo;
        bool state;
        VarDeque to_run = new VarDeque();

        public PhaserList(bool lifo) { this.lifo = lifo; }

        public void Run() {
            state = true;
            while (to_run.Count() != 0) {
                Variable next = lifo ? to_run.Pop() : to_run.Shift();
                if (next == null) continue;
                Kernel.RunInferior(next.Fetch().Invoke(
                    Kernel.GetInferiorRoot(), Variable.None, null));
            }
        }

        public void RunNew() { if (state) Run(); }

        public void Add(SubInfo what, bool immed) {
            to_run.Push(what.protosub);
            if (immed && state)
                Run();
        }
    }

    // A bunch of stuff which raises big circularity issues if done in the
    // setting itself.
    public class Kernel {
        // not listed: BEGIN (cannot be implemented in this model),
        // START (desugared using state), FIRST, NEXT, LAST (use masak code)
        public const int PHASER_INIT = 0;
        public const int PHASER_END = 1;
        public const int PHASER_UNIT_INIT = 2;
        public const int PHASER_KEEP = 3;
        public const int PHASER_UNDO = 4;
        public const int PHASER_LEAVE = 5;
        public const int PHASER_ENTER = 6;
        public const int PHASER_PRE = 7;
        public const int PHASER_POST = 8;
        public const int PHASER_CATCH = 9;
        public const int PHASER_CONTROL = 10;
        public const int PHASER_CHECK = 11;
        public const int PHASER_TYPES = 12;

        public static void DoRequire(string name) {
            throw new NotImplementedException(); // TODO reimplement
        }

        public static T UnboxAny<T>(P6any o) {
            var co = o as BoxObject<T>;
            if (co != null) {
                return co.value;
            } else {
                if (typeof(T) == typeof(string) && o.Does(Compartment.Top.PseudoStrMO)) {
                    // Truly vile hack to make dualvars work.
                    return (T)o.GetSlot(Compartment.Top.PseudoStrMO, "$!value");
                } else {
                    throw new NieczaException("Cannot unbox a {0} from an object of repr {1}", typeof(T).Name, o.ReprName());
                }
            }
        }

        public static Frame Take(Frame th, Variable payload) {
            Frame c = th;
            while (c != null && c.coro_return == null)
                c = c.caller;
            if (c == null)
                return Kernel.Die(th, "used take outside of a coroutine");

            Frame r = c.coro_return;
            c.coro_return = c;
            r.LexicalBind("$*nextframe", th);
            r.resultSlot = payload;
            th.resultSlot = payload;
            return r;
        }

        public static Frame CoTake(Frame th, Frame from) {
            Frame c = from;
            while (c != null && c.coro_return == null)
                c = c.caller;
            if (c.coro_return != c)
                return Kernel.Die(th, "Attempted to re-enter abnormally exited or running coroutine");
            c.coro_return = th;

            return from;
        }

        static Frame dogather(Frame th) {
            if (th.ip == 0) {
                th.ip = 1;
                return ((P6any)th.lex0).Invoke(th, Variable.None, null);
            } else {
                return Take(th, Compartment.Top.EMPTYP);
            }
        }

        public static Frame GatherHelper(Frame th, P6any sub) {
            Frame n = th.MakeChild(null, dogather_SI, Compartment.Top.AnyP);
            n.lex0 = sub;
            n.MarkSharedChain();
            n.coro_return = n;
            th.resultSlot = n;
            return th;
        }

        private static Frame SubInvokeSubC(Frame th) {
            Variable[] post;
            post = new Variable[th.pos.Length - 1];
            Array.Copy(th.pos, 1, post, 0, th.pos.Length - 1);
            return Compartment.Top.CodeMO.mro_INVOKE.Invoke((P6opaque)th.pos[0].Fetch(),
                    th.Return(), post, th.named);
        }

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
                P6opaque nj = new P6opaque(Compartment.Top.JunctionMO);
                nj.slots[0] = th.lex2;
                nj.slots[1] = Kernel.BoxRaw(dst, Compartment.Top.ParcelMO);
                th.caller.resultSlot = nj;
                return th.Return();
            }

            Variable[] npos = (Variable[]) th.lex4;
            npos[0] = src[th.ip++];

            return npos[0].Fetch().InvokeMethod(th, (string)th.lex3,
                    npos, th.named);
        }

        public static Frame Die(Frame caller, string msg) {
            if (Frame.AllExceptions) {
                Console.WriteLine("Die: {0}", msg);
                Console.WriteLine(DescribeBacktrace(caller, null));
            }
            return SearchForHandler(caller, SubInfo.ON_DIE, null, -1, null,
                    BoxAnyMO<string>(msg, Compartment.Top.StrMO));
        }

        public static P6any SigSlurpCapture(Frame caller) {
            P6any nw = new P6opaque(Compartment.Top.CaptureMO);
            nw.SetSlot(Compartment.Top.CaptureMO, "$!positionals", caller.pos);
            nw.SetSlot(Compartment.Top.CaptureMO, "$!named", caller.named);
            caller.named = null;
            return nw;
        }

        // SubInfo objects can be mutated at runtime by .wrap so they
        // must be containerized
        [CompartmentGlobal] internal static SubInfo dogather_SI;
        [CompartmentGlobal] internal static SubInfo AutoThreadSubSI;
        [CompartmentGlobal] internal static SubInfo IF_SI;
        [CompartmentGlobal] internal static SubInfo ExitRunloopSI;
        [CompartmentGlobal] internal static SubInfo JunctionFallbackSI;
        [CompartmentGlobal] internal static SubInfo SubInvokeSubSI;
        [CompartmentGlobal] internal static SubInfo RunCATCH_I;
        [CompartmentGlobal] internal static SubInfo CommonMEMap_I;
        [CompartmentGlobal] internal static SubInfo CommonGrep_I;
        [CompartmentGlobal] internal static SubInfo TEMP_SI;

        internal static void InitGlobal() {
            Random r = new Random();
            VarHash.string_hash_argument =
                (uint)r.Next(VarHash.HASH_ARG_MAX + 1);
            VarHash.hash_automorphism =
                ((uint)r.Next(int.MaxValue)) * 2 + 1;
            SetTrace();
        }

        internal static void InitCompartment() {
            RuntimeUnit.reg = new ObjectRegistry();

            AutoThreadSubSI = new SubInfo("KERNEL AutoThreadSub",
                    SubInfo.AutoThreadSubC);
            dogather_SI   = new SubInfo("KERNEL dogather", dogather);
            ExitRunloopSI = new SubInfo("ExitRunloop", ExitRunloopC);
            ExitRunloopSI.special = SubInfo.TRANSPARENT;
            IF_SI         = new SubInfo("iter_flatten", IF_C);
            JunctionFallbackSI = new SubInfo("Junction.FALLBACK",
                    JunctionFallbackC);
            SubInvokeSubSI = new SubInfo("Sub.postcircumfix:<( )>",
                    SubInvokeSubC);
            TEMP_SI       = new SubInfo("KERNEL Scalar.TEMP", Builtins.TEMP_C);

            RunCATCH_I = new SubInfo("KERNEL run_CATCH", null,
                Builtins.RunCATCH_C, null, null, new int[] {
                    0, 5, SubInfo.ON_NEXT, 1, 0,
                    0, 5, SubInfo.ON_REDO, 2, 0,
                    0, 5, SubInfo.ON_LAST, 4, 0,
                    0, 5, SubInfo.ON_DIE,  1, 0,
                }, new string[] { "" }, 0);

            CommonMEMap_I = new SubInfo("KERNEL map", null,
                Builtins.CommonMEMap_C, null, null, new int[] {
                    2, 3, SubInfo.ON_NEXT, 0, 0,
                    2, 3, SubInfo.ON_REDO, 1, 0,
                    2, 3, SubInfo.ON_LAST, 3, 0,
                }, new string[] { "" }, 0);

            CommonGrep_I = new SubInfo("KERNEL grep", null,
                Builtins.CommonGrep_C, null, null, new int[] {
                    2, 3, SubInfo.ON_NEXT, 0, 0,
                    2, 3, SubInfo.ON_REDO, 1, 0,
                    2, 3, SubInfo.ON_LAST, 3, 0,
                }, new string[] { "" }, 0);
        }

        public static P6any MakeSub(SubInfo info, Frame outer) {
            P6opaque n = new P6opaque(info.mo ?? Compartment.Top.CodeMO, 2);
            n.slots[0] = outer;
            if (outer != null) outer.MarkShared();
            n.slots[1] = info;
            return n;
        }

        public static SubInfo GetInfo(P6any sub) {
            return (SubInfo)sub.GetSlot(Compartment.Top.CodeMO, "$!info");
        }
        public static Frame GetOuter(P6any sub) {
            return (Frame)sub.GetSlot(Compartment.Top.CodeMO, "$!outer");
        }

        public class MMDParameter {
            public STable type;
            public bool constrained;
            public bool required;

            [Immutable] public static MMDParameter TOP = new MMDParameter();
            [Immutable] public static MMDParameter BOTTOM = new MMDParameter();

            // XXX Should requiredness be factored in?
            // 2: narrower 0: tied 1: less narrow 3: incomparable
            // by design these can be ORed
            public int IsNarrowerThan(MMDParameter other) {
                if (other == TOP) return (this == TOP) ? 0 : 2;
                if (other == BOTTOM) return (this == BOTTOM) ? 0 : 1;
                if (this == TOP) return 1;
                if (this == BOTTOM) return 2;

                bool k1 = type.HasType(other.type);
                bool k2 = other.type.HasType(type);
                if (k1 && !k2) return 2;
                if (k2 && !k1) return 1;

                if (constrained && !other.constrained) return 2;
                if (other.constrained && !constrained) return 1;

                return 0;
            }
        }

        internal class MMDCandidate : MultiCandidate {
            public P6any impl;
            public SubInfo info;
            // group_n provides the ordering between groups of candidates,
            // such as in a lexical nesting or override situation
            // filter_n provides ordering within a group, for constrained
            // candidates
            public int group_n, filter_n;

            public List<MMDParameter> pos;
            public Dictionary<string,MMDParameter> nam;

            public bool slurpy_pos;
            public bool slurpy_nam;
            public int min_arity;
            public bool extra_constraints;

            public override int Compare(int arity, MultiCandidate other) {
                var o = other as MMDCandidate;
                if (o.IsNarrowerThan(this))
                    return -1;
                if (this.IsNarrowerThan(o))
                    return +1;
                return 0;
            }

            public override int MinDispatchArity() { return min_arity; }

            public override bool AdmissableArity(int arity) {
                return arity >= min_arity && (slurpy_pos || arity <= pos.Count);
            }

            public override bool Admissable(Frame th, Variable[] pos, VarHash named) {
                Frame   o = (Frame)impl.GetSlot(Compartment.Top.CodeMO, "$!outer");
                // XXX sucks a bit to have an inf runloop here
                var res = Kernel.RunInferior(info.SetupCall(
                    Kernel.GetInferiorRoot(), o, impl, pos, named, true, null));
                return (res == Compartment.Top.TrueV);
            }

            public override Frame Invoke(Frame th, Variable[] pos, VarHash named) {
                return impl.Invoke(th, pos, named);
            }

            public bool IsNarrowerThan(MMDCandidate other) {
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

                if (narrower == 0 || narrower == 3) {
                    // are they the same except for constraints?
                    if (narrower == 0 && extra_constraints)
                        return filter_n < other.filter_n;

                    return group_n < other.group_n;
                }

                return (narrower == 2);
            }

            public MMDCandidate(P6any impl, int group_n, int filter_n) {
                this.impl = impl;
                this.group_n  = group_n;
                this.filter_n = filter_n;
                info = (SubInfo) impl.GetSlot(Compartment.Top.CodeMO, "$!info");

                pos = new List<MMDParameter>();
                nam = new Dictionary<string,MMDParameter>();

                foreach (Parameter pa in info.sig.parms) {
                    int flags = pa.flags;
                    MMDParameter p = new MMDParameter();
                    p.required = ((flags & (Parameter.OPTIONAL | Parameter.HASDEFAULT)) == 0);
                    p.constrained = false;
                    p.type = pa.type;

                    if (((flags & Parameter.DEF_MASK) != Parameter.ANY_DEF &&
                            (flags & Parameter.DEF_MASK) != 0) ||
                            pa.post_constraints != null) {
                        p.constrained = true;
                        extra_constraints = true;
                    }

                    // XXX The long name model does not represent the full
                    // diversity of Perl 6 parameters, instead restricting
                    // them to 'only positional' or '1 name'
                    if (pa.names != null && (flags & Parameter.POSITIONAL) == 0) {
                        if (pa.names.Length == 1) {
                            nam[pa.names[0]] = p;
                        } else {
                            slurpy_nam = true;
                            extra_constraints = true;
                        }
                    } else if ((flags & Parameter.SLURPY_PCL) != 0) {
                        slurpy_pos = true;
                    } else if ((flags & Parameter.SLURPY_CAP) != 0) {
                        slurpy_pos = true;
                        slurpy_nam = true;
                    } else if ((flags & Parameter.SLURPY_POS) != 0) {
                        slurpy_pos = true;
                    } else if ((flags & Parameter.SLURPY_NAM) != 0) {
                        slurpy_nam = true;
                    } else if ((flags & Parameter.POSITIONAL) != 0) {
                        pos.Add(p);
                    }
                }
                foreach (MMDParameter p in pos)
                    if (p.required) min_arity++;
            }

            [TrueGlobal]
            private static long unique;
            public static long get_unique() {
                return Interlocked.Increment(ref unique);
            }

            public override string ToString() {
                List<string> bits = new List<string>();

                foreach (MMDParameter p in pos) {
                    string n = p.type.name;
                    if (!p.required) n += "?";
                    if (p.constrained)
                        n += " where { ... }";
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
                        b += " where { ... }";
                    bits.Add(b);
                }

                if (slurpy_nam) bits.Add("*%_");
                string full = JoinS(", ", bits);
                if (extra_constraints)
                    full += ";; |$c where { ... }";
                return full;
            }
        }

        public static Frame TypeDispatcher(Frame th, bool tailcall) {
            Frame dth = th;
            while (dth.info.param == null ||
                    (dth.info.param[0] as P6any[]) == null) dth = dth.outer;

            var cs = Thread.VolatileRead(ref dth.info.param[1]) as CandidateSet;
            if (cs == null) {
                List<MultiCandidate> mc = new List<MultiCandidate>();
                int filter_n = 0;
                int group_n = 0;
                foreach (P6any craw in dth.info.param[0] as P6any[]) {
                    if (craw == null) { group_n++; continue; }
                    mc.Add(new MMDCandidate(craw, group_n, filter_n++));
                }
                cs = new CandidateSet(dth.info.name, mc.ToArray());
                Interlocked.CompareExchange(ref dth.info.param[1], cs, null);
            }
            if (tailcall) th = th.Return();
            return cs.DoDispatch(th, dth.pos, dth.named, dth.sub);
        }

        internal static Frame StandardTypeProtoC(Frame th) {
            return TypeDispatcher(th, true);
        }

        public static P6any MakeDispatcher(string name, P6any proto, P6any[] cands) {
            if (proto != null && proto.mo.name == "Regex") goto ltm;
            for (int i = 0; i < cands.Length; i++) {
                if (cands[i] == null) continue;
                if (cands[i].mo.name == "Regex") goto ltm;
                break;
            }

            SubInfo si;

            si = (proto != null) ? new SubInfo(GetInfo(proto)) :
                new SubInfo(name, StandardTypeProtoC);

            si.param = new object[] { cands, null };
            return Kernel.MakeSub(si, proto == null ? null : GetOuter(proto));
ltm:
            List<P6any> lp = new List<P6any>();
            foreach (P6any p in cands)
                if (p != null) lp.Add(p);
            return Lexer.MakeDispatcher(name, proto, lp.ToArray());
        }

        [TrueGlobal]
        public static bool SaferMode;

        private static Frame SaferTrap(Frame th) {
            return Die(th, th.info.name + " may not be used in safe mode");
        }

        public static void CheckUnsafe(SubInfo info) {
            if (SaferMode)
                info.code = SaferTrap;
        }
        public static Variable BoxAny<T>(T v, P6any proto) {
            if (proto == Compartment.Top.BoolMO.typeObj) {
                if (v is bool)
                    return ((bool) (object) v) ? Compartment.Top.TrueV : Compartment.Top.FalseV;
                else
                    return ((int) (object) v) != 0 ? Compartment.Top.TrueV : Compartment.Top.FalseV;
            }
            return new BoxObject<T>(v, ((P6opaque)proto).mo);
        }

        public static void SetBox<T>(P6any obj, T v) {
            ((BoxObject<T>) obj).value = v;
        }

        public static Variable BoxAnyMO<T>(T v, STable proto) {
            if (proto == Compartment.Top.BoolMO) {
                if (v is bool)
                    return ((bool) (object) v) ? Compartment.Top.TrueV : Compartment.Top.FalseV;
                else
                    return ((int) (object) v) != 0 ? Compartment.Top.TrueV : Compartment.Top.FalseV;
            }
            return new BoxObject<T>(v, proto);
        }

        public static BoxObject<T> BoxRaw<T>(T v, STable proto) {
            return new BoxObject<T>(v, proto);
        }

        public static Variable Decontainerize(Variable rhs) {
            if (!rhs.Rw) return rhs;
            return rhs.Fetch();
        }

        public const int NBV_RO = 0;
        public const int NBV_RW = 1;
        public const int NBV_LIST = 2;
        public static Variable NewBoundVar(int mode, STable type, Variable rhs) {
            int omode = rhs.Mode;
            bool rw = omode == Variable.RW && (mode & NBV_RW) != 0;
            // we always have to fetch, because of subsets (XXX?)
            P6any rhso = rhs.Fetch();
            if (!rhso.Does(type))
                throw new NieczaException("Nominal type check failed in nonparameter binding; got " + rhso.mo.name + ", needed " + type.name);

            if (rw) {
                // working RW bind implies !rhs.islist, !islist; will return
                // rhs if successful
                rhs.Vivify();
                return rhs;
            }

            bool islist = (mode & NBV_LIST) != 0;
            // if source is !rw, we may not need to fetch it
            if (omode == (islist ? Variable.LIST : Variable.RO))
                return rhs;

            return islist ? (Variable)new ListVariable(rhso) : rhso;
        }

        public static Variable Assign(Variable lhs, Variable rhs) {
            return lhs.Assign(rhs);
        }

        public static Variable NewRWScalar(STable t, P6any obj) {
            return new RWVariable(t, null, obj);
        }

        public static Variable NewMuScalar(P6any obj) {
            return new RWVariable(null, null, obj);
        }

        public static Variable NewTypedScalar(STable t) {
            if (t == null)
                return new RWVariable(null, null, Compartment.Top.AnyMO.typeObj);

            return new RWVariable(t, null, t.initObj);
        }

        public static ListVariable NewRWListVar(P6any container) {
            return new ListVariable(container);
        }

        public static VarDeque SlurpyHelper(Variable[] pos, int from) {
            VarDeque lv = new VarDeque();
            for (int i = from; i < pos.Length; i++) {
                lv.Push(pos[i]);
            }
            return lv;
        }

        public static VarDeque IterCopyElems(VarDeque vals) {
            VarDeque nv = new VarDeque();
            for (int i = 0; i < vals.Count(); i++)
                nv.Push(NewMuScalar(vals[i].Fetch()));
            return nv;
        }

        [TrueGlobal]
        public static string[] commandArgs;

        public static VarDeque SortHelper(Frame th, P6any cb, VarDeque from) {
            Variable[] tmp = from.CopyAsArray();
            Array.Sort(tmp, delegate (Variable v1, Variable v2) {
                Variable v = RunInferior(cb.Invoke(GetInferiorRoot(),
                        new Variable[] { v1, v2 }, null));
                double rv = v.Fetch().mo.mro_raw_Numeric.Get(v);
                return (rv > 0 ? +1 : rv < 0 ? -1 : 0);
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
            StashEnt v;

            if (currentGlobals.TryGetValue("\x8::GLOBAL" + name, out v)) {
                return v.v;
            } else if (currentGlobals.TryGetValue("\x9::PROCESS" + name, out v)) {
                return v.v;
            } else {
                return Compartment.Top.AnyP;
            }
        }

        public static void SetStatus(Frame th, string name, Variable v) {
            th = th.caller;
            while (true) {
                if (th == null)
                    return;
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
                th.caller.resultSlot = n;
                return th.Return();
            }

            Variable vx;
            if ((prog[i].flags & P6how.A_PUBLIC) != 0 &&
                    args.TryGetValue(prog[i].name.Substring(2), out vx)) {
                args.Remove(prog[i].name.Substring(2));
                th.resultSlot = vx;
            } else if (prog[i].init == null) {
                th.resultSlot = null;
            } else if (prog[i].name == null) {
                P6any init = prog[i].init;
                th.lexi0 = i;

                SubInfo si = GetInfo(init);
                VarHash build_args = new VarHash();
                foreach (Parameter pa in si.sig.parms) {
                    foreach (string name in pa.names ?? new string[0]) {
                        if (args.ContainsKey(name)) {
                            build_args[name] = args[name];
                            args.Remove(name);
                        }
                    }
                }

                return init.Invoke(th, new Variable[] { n }, build_args);
            } else {
                P6any init = prog[i].init;
                th.lexi0 = i;
                return init.Invoke(th, new [] { n }, null);
            }

value:      vx = (Variable) th.resultSlot;
            if (prog[i].name == null) {
                i++;
                goto again;
            }

            Builtins.EstablishSlot(n, prog[i], vx);

            i++;
            goto again;
        }

        public static Frame PromoteToList(Frame th, Variable v) {
            if (!v.List) {
                P6opaque lst = new P6opaque(Compartment.Top.ListMO);
                lst.slots[0 /*items*/] = new VarDeque(v);
                lst.slots[1 /*rest*/ ] = new VarDeque();
                th.resultSlot = Kernel.NewRWListVar(lst);
                return th;
            }
            P6any o = v.Fetch();
            if (o.mo.HasType(Compartment.Top.ListMO)) {
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
                if (item.mo.HasType(Compartment.Top.IterCursorMO)) {
                    break;
                } else {
                    items.Push(iter.Shift());
                }
            }
            list.SetSlot(Compartment.Top.ListMO, "$!items", items);
            list.SetSlot(Compartment.Top.ListMO, "$!rest", iter);
        }

        public static VarDeque IterFlatten(VarDeque inq) {
            VarDeque outq = new VarDeque();
            Variable inq0v;
            P6any inq0;

again:
            if (inq.Count() == 0)
                return outq;
            inq0v = inq[0];
            if (inq0v == null) {
                throw new NieczaException("Circular data dependency in list iteration, or last fetch threw exception");
            }
            inq0 = inq0v.Fetch();
            if (inq0v.List) {
                inq.Shift();
                inq.UnshiftD(inq0.mo.mro_raw_iterator.Get(inq0v));
                goto again;
            }
            if (inq0.mo.HasType(Compartment.Top.IterCursorMO)) {
                Frame th = new Frame(null, null, IF_SI, Compartment.Top.AnyP);
                th.lex0 = inq;
                P6opaque thunk = new P6opaque(Compartment.Top.GatherIteratorMO);
                th.coro_return = th;
                thunk.slots[0] = NewMuScalar(th);
                thunk.slots[1] = NewMuScalar(Compartment.Top.AnyP);
                outq.Push(thunk);
                return outq;
            }
            outq.Push(inq0v);
            inq.Shift();
            goto again;
        }

        private static Frame IF_C(Frame th) {
            VarDeque inq = (VarDeque) th.lex0;
            if (IterHasFlat(inq, true)) {
                return Take(th, inq.Shift());
            } else {
                return Take(th, Compartment.Top.EMPTYP);
            }
        }

        public static bool IterHasFlat(VarDeque iter, bool flat) {
            while (true) {
                if (iter.Count() == 0)
                    return false;
                Variable i0 = iter[0];
                if (i0 == null) {
                    throw new NieczaException("Circular data dependency in list iteration, or last fetch threw exception");
                }
                if (i0.List && flat) {
                    iter[0] = null;
                    iter.Shift_UnshiftD(i0.Fetch().mo.mro_raw_iterator.Get(i0));
                    continue;
                }
                P6any i0v = i0.Fetch();
                if (i0v.mo.HasType(Compartment.Top.IterCursorMO)) {
                    iter[0] = null;
                    iter.Shift_UnshiftN(i0v.mo.mro_raw_reify.Get(i0));
                    continue;
                }

                return true;
            }
        }

        public static Variable GetFirst(Variable lst) {
            if (!lst.List) {
                return lst;
            }
            P6opaque dyl = lst.Fetch() as P6opaque;
            if (dyl == null) { goto slow; }
            if (dyl.mo != Compartment.Top.ListMO) { goto slow; }
            VarDeque itemsl = (VarDeque) dyl.GetSlot(Compartment.Top.ListMO, "$!items");
            if (itemsl.Count() == 0) {
                VarDeque restl = (VarDeque) dyl.GetSlot(Compartment.Top.ListMO, "$!rest");
                if (restl.Count() == 0) {
                    return Compartment.Top.AnyP;
                }
                goto slow;
            }
            return itemsl[0];

slow:
            return RunInferior(lst.Fetch().InvokeMethod(
                        GetInferiorRoot(), "head", new Variable[] {lst}, null));
        }

        public static Variable CreateArray() {
            P6any v = new P6opaque(Compartment.Top.ArrayMO, 2);
            v.SetSlot(Compartment.Top.ListMO, "$!items", new VarDeque());
            v.SetSlot(Compartment.Top.ListMO, "$!rest", new VarDeque());
            return NewRWListVar(v);
        }

        public static Variable CreateHash() {
            P6any v = BoxRaw(new VarHash(), Compartment.Top.HashMO);
            return NewRWListVar(v);
        }

        public static Variable StashyMerge(Variable o, Variable n, string d1, string d2) {
            P6any oo = o as P6any;
            P6any nn = n as P6any;

            if (oo == null) return n;
            if (nn == null) return o;
            if (oo == nn) return o;

            if (!oo.IsDefined() && !nn.IsDefined() && oo.mo.who == nn.mo.who) {
                if (oo.mo.mo.type == P6how.PACKAGE) return n;
                if (nn.mo.mo.type == P6how.PACKAGE) return o;
            }

            throw new NieczaException("Funny merge failure " + d1 + "::" + d2 +
                    " (" + oo.mo.name + ", " + nn.mo.name + ")");
        }

        public static StashEnt GetVar(string who, string name) {
            StashEnt v;
            string key = (char)who.Length + who + name;

            if (currentGlobals.TryGetValue(key, out v))
                return v;
            else
                v = currentGlobals[key] = new StashEnt();

            if (name.StartsWith("@")) {
                v.v = CreateArray();
            } else if (name.StartsWith("%")) {
                v.v = CreateHash();
            } else {
                v.v = NewTypedScalar(null);
            }

            return v;
        }

        private static void Handler_Vonly(STable kl, string name,
                ContextHandler<Variable> cv, object cvu) {
            WrapHandler0(kl, name, cv, cvu);
        }

        private static void Handler_PandBox<T>(STable kl, string name,
                ContextHandler<T> cvu, STable box) {
            ContextHandler<Variable> cv = new CtxBoxify<T>(cvu, box);
            WrapHandler0(kl, name, cv, cvu);
        }

        private static void Handler_PandBoxInty(STable kl, string name,
                ContextHandler<double> cvu) {
            ContextHandler<Variable> cv = new CtxBoxifyInty(cvu);
            WrapHandler0(kl, name, cv, cvu);
        }

        private static void Handler_PandCont(STable kl, string name,
                ContextHandler<P6any> cvu) {
            ContextHandler<Variable> cv = new CtxContainerize(cvu);
            WrapHandler0(kl, name, cv, cvu);
        }

        static Frame WrapHandler0cb(Frame th) {
            if (th.ip == 0) { th.ip = 1; return Frame.Binder(th); }
            th.caller.resultSlot = ((ContextHandler<Variable>)
                    th.info.param[1]).Get((Variable)th.lex0);
            return th.Return();
        }

        private static void WrapHandler0(STable kl, string name,
                ContextHandler<Variable> cvb, object cvu) {
            SubInfo si = new SubInfo("KERNEL " + kl.name + "." + name,
                    WrapHandler0cb);
            si.sig = new Signature(Parameter.TPos("self", 0));
            si.param = new object[] { cvu, cvb };
            kl.AddMethod(0, name, MakeSub(si, null));
        }

        static Frame WrapHandler1cb(Frame th) {
            if (th.ip == 0) { th.ip = 1; return Frame.Binder(th); }
            IndexHandler fcv = (IndexHandler) th.info.param[1];
            th.caller.resultSlot = fcv.Get((Variable)th.lex0,
                    (Variable)th.lex1);
            return th.Return();
        }
        private static void WrapHandler1(STable kl, string name,
                IndexHandler cv) {
            SubInfo si = new SubInfo("KERNEL " + kl.name + "." + name,
                WrapHandler1cb);
            si.sig = new Signature(Parameter.TPos("self", 0),
                    Parameter.TPos("$key", 1));
            si.param = new object[] { null, cv };
            kl.AddMethod(0, name, MakeSub(si, null));
        }

        static Frame WrapPushycb(Frame th) {
            if (th.ip == 0) { th.ip = 1; return Frame.Binder(th); }
            PushyHandler fcv = (PushyHandler) th.info.param[1];
            Variable[] fullpc = UnboxAny<Variable[]>(
                    ((Variable)th.lex1).Fetch());
            Variable[] chop = new Variable[fullpc.Length - 1];
            Array.Copy(fullpc, 1, chop, 0, chop.Length);
            th.caller.resultSlot = fcv.Invoke((Variable)th.lex0, chop);
            return th.Return();
        }
        private static void WrapPushy(STable kl, string name,
                PushyHandler cv) {
            SubInfo si = new SubInfo("KERNEL " + kl.name + "." + name,
                    WrapPushycb);
            si.sig = new Signature(Parameter.TPos("self", 0),
                new Parameter(Parameter.RWTRANS | Parameter.SLURPY_PCL, 1, "$args", null, null, null, null, null)
            );
            si.param = new object[] { null, cv };
            kl.AddMethod(0, name, MakeSub(si, null));
        }

        private static Frame DispIndexy(Frame th) {
            if (th.ip == 0) { th.ip = 1; return Frame.Binder(th); }
            object[] p     = th.info.param;
            VarHash  n     = th.named;
            Variable self  = (Variable)th.lex0;
            Variable[] pos = th.pos;

            Variable res = null;
            Variable index = pos.Length >= 2 ? pos[1] : null;

            // Until shaped arrays exist, 'deep' array accesses are turned
            // into nested accesses

            if (pos.Length > 2) {
                Variable inter = ((IndexHandler)p[0]).Get(self, index);
                Variable[] npos = new Variable[pos.Length - 1];
                Array.Copy(pos, 2, npos, 1, pos.Length - 2);
                npos[0] = inter;
                return inter.Fetch().InvokeMethod(th.Return(), (string)p[4],
                        npos, n);
            } else if (index == null) {
                // TODO: handle adverbs on Zen slices (XXX does this make sense?)
                res = self.List ? self : Kernel.NewRWListVar(self.Fetch());
            } else if (n == null) {
                res = ((IndexHandler)p[0]).Get(self, index);
            } else if (p[1] != null && n.ContainsKey("exists")) {
                res = ((IndexHandler)p[1]).Get(self, index);
            } else if (p[2] != null && n.ContainsKey("delete")) {
                res = ((IndexHandler)p[2]).Get(self, index);
            } else if (n.ContainsKey("k")) {
                res =  new KeySlicer(0, p[0]).Get(self, index);
            } else if (n.ContainsKey("kv")) {
                res = new KeySlicer(1, p[0]).Get(self, index);
            } else if (n.ContainsKey("p")) {
                res = new KeySlicer(2, p[0]).Get(self, index);
            } else if (n.ContainsKey("BIND_VALUE")) {
                res = ((BindHandler)p[3]).Bind(self, index, n["BIND_VALUE"]);
            } else {
                res = ((IndexHandler)p[0]).Get(self, index);
            }

            th.caller.resultSlot = res;
            return th.Return();
        }

        private static void WrapIndexy(STable kl, string name,
                IndexHandler at, IndexHandler exist, IndexHandler del,
                BindHandler bind) {
            SubInfo si = new SubInfo("KERNEL " + kl.name + "." + name,
                    DispIndexy);
            List<Parameter> lp = new List<Parameter>();
            lp.Add(Parameter.TPos("self", 0));
            lp.Add(new Parameter(Parameter.RWTRANS | Parameter.SLURPY_PCL, -1,
                        "args", null, null, null, null, null));
            if (del != null) {
                lp.Add(Parameter.TNamedOpt("exists", -1));
                lp.Add(Parameter.TNamedOpt("delete", -1));
            }
            lp.Add(Parameter.TNamedOpt("k", -1));
            lp.Add(Parameter.TNamedOpt("kv", -1));
            lp.Add(Parameter.TNamedOpt("p", -1));
            lp.Add(Parameter.TNamedOpt("BIND_VALUE", -1));
            si.sig = new Signature(lp.ToArray());
            si.param = new object[] { at, exist, del, bind, name };
            kl.AddMethod(0, name, MakeSub(si, null));
        }

        public static Frame InstantiateRole(Frame th, Variable[] pcl) {
            th.resultSlot = DoInstantiateRole(pcl[0].Fetch().mo,pcl[1]).typeObj;
            return th;
        }

        public static STable DoInstantiateRole(STable prole, Variable alist) {
            // get argument list - TODO make this saner
            P6any argv = alist.Fetch();
            Variable[] args = argv.mo == Compartment.Top.ParcelMO ?
                UnboxAny<Variable[]>(argv) : new Variable[] { alist };

            STable r = new STable(prole.name + "[curried]");
            r.mo.roleFactory = prole.mo.roleFactory;
            r.mo.curriedArgs = args;
            r.mo.FillRole(prole.mo.superclasses.ToArray(), null);
            r.how = new BoxObject<STable>(r, Compartment.Top.ClassHOWMO, 0);
            r.mo.type  = P6how.CURRIED_ROLE;
            r.mo.rtype = "crole";
            r.useAcceptsType = true;
            r.mo.role_typecheck_list = new List<STable>(prole.mo.role_typecheck_list);
            r.mo.role_typecheck_list.Add(r);
            r.mo.local_roles = prole.mo.local_roles;
            r.typeObj = r.initObj = new P6opaque(r);
            foreach (var mi in prole.mo.lmethods)
                r.mo.lmethods.Add(mi);
            foreach (var ai in prole.mo.local_attr)
                r.mo.local_attr.Add(ai);
            r.mo.Revalidate();
            r.SetupVTables();
            return r;
        }

        internal static STable ToInheritable(STable arg) {
            if (arg.mo.type == P6how.ROLE || arg.mo.type == P6how.CURRIED_ROLE || arg.mo.type == P6how.PARAMETRIZED_ROLE) {
                return arg.mo.PunRole();
            }
            if (arg.mo.type == P6how.CLASS || arg.mo.type == P6how.GRAMMAR) {
                return arg;
            }
            throw new NieczaException("Cannot inherit from " + arg.name + " (" + arg.mo.rtype + ")");
        }

        static STable ToComposable(STable arg, STable cls) {
            if (arg.mo.type == P6how.CURRIED_ROLE) {
                STable r;
                Variable[] pass = new Variable[arg.mo.curriedArgs.Length+1];
                pass[0] = cls.typeObj;
                Array.Copy(arg.mo.curriedArgs, 0, pass, 1, pass.Length-1);
                Frame ifr = (Frame)RunInferior(arg.mo.roleFactory.
                    Invoke(GetInferiorRoot(), pass, null)).Fetch();

                r = new STable(arg.name + "[...]");
                r.mo.FillRole(arg.mo.superclasses.ToArray(), null);
                r.how = new BoxObject<STable>(r, Compartment.Top.ClassHOWMO, 0);
                r.mo.type  = P6how.ROLE;
                r.mo.rtype = "role";
                r.mo.role_typecheck_list = arg.mo.role_typecheck_list;
                r.typeObj = r.initObj = new P6opaque(r);
                // Hack - reseat role to this closure-clone of methods
                foreach (var mi in arg.mo.lmethods) {
                    var nmi = mi;
                    SubInfo orig = GetInfo(nmi.impl);
                    nmi.impl = ((Variable)ifr.info.dylex[orig.outervar].
                            Get(ifr)).Fetch();
                    r.mo.lmethods.Add(nmi);
                }
                foreach (var ai in arg.mo.local_attr) {
                    var nai = ai;
                    if (nai.init != null) {
                        SubInfo orig = GetInfo(nai.init);
                        nai.init = ((Variable)ifr.info.
                            dylex[orig.outervar].Get(ifr)).Fetch();
                    }
                    r.mo.local_attr.Add(nai);
                }
                foreach (var r2 in arg.mo.local_roles)
                    ApplyRoleToRole(r, ToComposable(r2, cls));
                r.mo.Revalidate();
                r.SetupVTables();
                return r;
            } else if (arg.mo.type == P6how.PARAMETRIZED_ROLE) {
                return ToComposable(DoInstantiateRole(arg, Builtins.MakeParcel()), cls);
            } else if (arg.mo.type == P6how.ROLE) {
                return arg;
            } else {
                throw new NieczaException("Cannot compose a type of category " + arg.mo.rtype + " (" + arg.name + ")");
            }
        }

        public static void ApplyRoleToRole(STable into, STable role) {
            if (into.mo.type != P6how.ROLE)
                throw new NieczaException("Composing into a non-concrete role should have been deferred");
            if (role.mo.type != P6how.ROLE)
                throw new NieczaException("Trying to compose a non-concrete-role into a role");
            foreach (P6how.MethodInfo mi in role.mo.lmethods)
                into.mo.lmethods.Add(mi);
            foreach (P6how.AttrInfo ai in role.mo.local_attr)
                into.mo.local_attr.Add(ai);
            foreach (STable su in role.mo.superclasses)
                into.mo.superclasses.Add(su);
            // This is handled earlier, at parametric role compose time
            //foreach (STable su in role.mo.role_typecheck_list)
            //    into.mo.role_typecheck_list.Add(su);
        }

        static string MethodSlot(Prod<int,string> arg) {
            return (arg.v1 == P6how.V_SUBMETHOD ? "Submethod '" :
                    arg.v1 == P6how.V_PRIVATE   ? "Private method '" :
                    "Method '") + arg.v2 + "'";
        }

        public static void ApplyRoleToClass(STable cls, params STable[] roles) {
            if (cls.mo.type != P6how.CLASS && cls.mo.type != P6how.GRAMMAR)
                throw new NieczaException("Cannot compose a role into a " + cls.mo.rtype);
            for (int i = 0; i < roles.Length; i++)
                roles[i] = ToComposable(roles[i], cls);

            // check attribute merging
            var attrs   = new Dictionary<string,string>();
            foreach (P6how.AttrInfo ai in cls.mo.local_attr)
                attrs[ai.name] = null;
            foreach (STable r in roles) {
                foreach (P6how.AttrInfo ai in r.mo.local_attr) {
                    if (attrs.ContainsKey(ai.name)) {
                        string o = attrs[ai.name];
                        if (o == null) {
                            throw new NieczaException("Attribute '" + ai.name + "' already exists in the class '" + cls.name + "', but a role also wishes to compose it");
                        } else {
                            throw new NieczaException("Attribute '" + ai.name + "' is provided by both '" + attrs[ai.name] + "' and '" + r.name + "'");
                        }
                    }
                    var nai = ai;
                    nai.owner = cls;
                    cls.mo.local_attr.Add(nai);
                    attrs[nai.name] = r.name;
                }
            }

            // now methods, these are a lot harder
            var class_methods = new HashSet<Prod<int,string>>();
            var role_methods  = new Dictionary<Prod<int,string>,Prod<string,SubInfo>>();
            var requirements  = new Dictionary<Prod<int,string>,string>();

            foreach (P6how.MethodInfo mi in cls.mo.lmethods) {
                class_methods.Add(Prod.C(mi.flags & P6how.V_MASK, mi.short_name));
            }

            foreach (STable r in roles) {
                foreach (P6how.MethodInfo mi in r.mo.lmethods) {
                    var name = Prod.C(mi.flags & P6how.V_MASK, mi.short_name);
                    SubInfo info = mi.impl.Isa(Compartment.Top.CodeMO) ?
                        GetInfo(mi.impl) : null;
                    if ((mi.flags & P6how.M_MASK) != P6how.M_ONLY) {
                        // multi methods from roles are not suppressed and
                        // never used as requirements
                    } else {
                        if (class_methods.Contains(name)) continue;
                        if (info != null && info.extend != null &&
                                info.extend.ContainsKey("onlystub")) {
                            requirements[name] = r.name;
                            continue;
                        }
                        if (role_methods.ContainsKey(name) && role_methods[name].v2 != GetInfo(mi.impl)) {
                            throw new NieczaException(MethodSlot(name) + " must be resolved by class '" + cls.name + "' because it exists in roles '" + role_methods[name].v1 + "' and '" + r.name + "'");
                        }
                        role_methods[name] = Prod.C(r.name, GetInfo(mi.impl));
                    }
                    cls.mo.lmethods.Add(mi);
                }
            }

            foreach (var name in requirements) {
                if (role_methods.ContainsKey(name.Key) || class_methods.Contains(name.Key) || (name.Key.v1 == 0 && cls.FindMethod(name.Key.v2) != null)) continue;
                throw new NieczaException(MethodSlot(name.Key) + " must be implemented by '" + cls.name + "' because it is required by role '" + name.Value + "'");
            }

            foreach (STable role in roles) {
                foreach (STable su in role.mo.superclasses)
                    cls.mo.superclasses.Add(su);
                foreach (STable su in role.mo.role_typecheck_list)
                    cls.mo.role_typecheck_list.Add(su);
            }
        }

        public static STable RoleApply(STable b, STable role) {
            STable n = new STable(b.name + " but " + role.name);

            n.how = BoxAny<STable>(n, b.how).Fetch();
            n.typeObj = n.initObj = new P6opaque(n);
            ((P6opaque)n.typeObj).slots = null;

            n.mo.superclasses.Add(b);
            n.mo.local_roles.Add(role);
            n.mo.Compose();

            return n;
        }

        internal static void SetTrace() {
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
        }

        public static void RunMain(RuntimeUnit main_unit) {
            try {
                Compartment.Top.init.Run();
                main_unit.RunMainline();
            } catch (Exception n) {
                Console.Error.WriteLine("Unhandled exception: {0}", n);
                Builtins.exit(1);
            }
        }

        class ExitRunloopException : Exception {
            public ResumeUnwindException payload;
            public ExitRunloopException(ResumeUnwindException p) { payload = p; }
        }
        private static Frame ExitRunloopC(Frame th) {
            throw new ExitRunloopException(th.lex0 as ResumeUnwindException);
        }

        public const int TRACE_CUR = 1;
        public const int TRACE_ALL = 2;

        [TrueGlobal] public static int TraceFreq;
        [TrueGlobal] public static int TraceCount;
        [TrueGlobal] public static int TraceFlags;

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
            // TimToady says that any exception thrown (but not internally
            // caught) during CATCH or LEAVE processing becomes the new
            // exception
            Exception handle = null;
            for(;;) {
                try {
                    if (handle is ResumeUnwindException) {
                        ResumeUnwindException rue =
                            (ResumeUnwindException) handle;
                        cur = Kernel.Unwind(cur, rue.type, rue.to_frame,
                                rue.to_ip, rue.to_data, null, null,
                                rue.p6backtrace);
                    }
                    else if (handle != null) {
                        if (Frame.AllExceptions)
                            Console.Error.WriteLine(handle.ToString());
                        cur = Kernel.Die(cur, handle.ToString());
                    }
                    handle = null;

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
                    if (ere.payload != null)
                        throw ere.payload;
                    return;
                } catch (Exception ex) {
                    handle = ex;
                }
            }
        }

        // we like to make refs to these, so moving arrays is untenable
        class LastFrameNode {
            public LastFrameNode next, prev;
            public Frame cur, root;
            public Compartment compartment = Compartment.Top;
        }
        [ThreadStatic] static LastFrameNode rlstack;
        public static void SetTopFrame(Frame f) {
            rlstack.cur = f;
        }
        internal static void SetupThreadParent(Frame f) {
            rlstack = new LastFrameNode();
            rlstack.cur = f;
        }
        public static Frame GetTopFrame() {
            return rlstack.cur;
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
            if (lfn.compartment != Compartment.Top)
                l = null; // hide other objects
            rlstack = lfn.next;
            lfn.next.compartment = Compartment.Top;
            return lfn.next.cur = lfn.next.root = ((l == null ?
                        new Frame(null, null, ExitRunloopSI, Compartment.Top.AnyP) :
                        l.MakeChild(null, ExitRunloopSI, Compartment.Top.AnyP)));
            //lfn.next.cur = lfn.next.root = ((l == null ?
            //            new Frame(null, null, ExitRunloopSI, Compartment.Top.AnyP) :
            //            l.MakeChild(null, ExitRunloopSI, Compartment.Top.AnyP)));
            //Console.WriteLine("Created exit-runloop {0:X} caller={1:X}",
            //    lfn.next.cur.GetHashCode(), lfn.next.cur.caller == null ? 0 :
            //    lfn.next.cur.caller.GetHashCode());
            //return lfn.next.cur;
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
            Variable[] fp = cap.GetSlot(Compartment.Top.CaptureMO, "$!positionals") as Variable[];
            VarHash fn = cap.GetSlot(Compartment.Top.CaptureMO, "$!named")
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

        [CompartmentGlobal]
        public static Dictionary<string, StashEnt> currentGlobals;
        // The root unit of this isolation container; will not point to
        // an eval or such.
        [CompartmentGlobal]
        internal static RuntimeUnit containerRootUnit;

        public static Variable GetGlobal(string key) {
            return currentGlobals[key].v;
        }

        public static void BindGlobal(string key, Variable to) {
            currentGlobals[key].Bind(to);
        }

        internal static void CreateBasicTypes() {
            Compartment.Top.CodeMO = new STable("Code"); // forward decl
            Compartment.Top.MuMO = new STable("Mu");
            Compartment.Top.AnyMO = new STable("Any");
            Compartment.Top.ParameterMO = new STable("Parameter");
            Compartment.Top.SignatureMO = new STable("Signature");
            Handler_Vonly(Compartment.Top.MuMO, "defined", new CtxBoolNativeDefined(),
                    new CtxRawNativeDefined());
            Handler_Vonly(Compartment.Top.MuMO, "Bool", new CtxBoolNativeDefined(),
                    new CtxRawNativeDefined());
            Handler_Vonly(Compartment.Top.MuMO, "item", new CtxReturnSelfItem(), null);

            Compartment.Top.MuMO.FillProtoClass(null, new string[0], new STable[0]);
            Compartment.Top.MuMO.Invalidate();

            // Compartment.Top.AnyMO.typeObj is needed very early, while setting up the
            // root $_
            Compartment.Top.AnyMO.typeObj = new P6opaque(Compartment.Top.AnyMO, 0);
            Handler_Vonly(Compartment.Top.AnyMO, "list", new CtxAnyList(), null);
            WrapIndexy(Compartment.Top.AnyMO, "postcircumfix:<[ ]>", new IxAnyAtPos(),
                    null, null, new IxAnyBindPos());
            WrapIndexy(Compartment.Top.AnyMO, "postcircumfix:<{ }>", new IxAnyAtKey(),
                    new IxAnyExistsKey(), new IxAnyDeleteKey(),
                    new IxAnyBindKey());
            Compartment.Top.AnyMO.FillProtoClass(Compartment.Top.MuMO);
            Compartment.Top.AnyMO.Invalidate();

            Compartment.Top.CodeMO.FillProtoClass(Compartment.Top.AnyMO, new string[] { "$!outer", "$!info" },
                new STable[] { Compartment.Top.CodeMO, Compartment.Top.CodeMO });
            SubInvokeSubSI.param = new object[] { null, new InvokeSub() };
            Compartment.Top.CodeMO.AddMethod(0, "postcircumfix:<( )>", MakeSub(SubInvokeSubSI, null));
            Compartment.Top.CodeMO.Invalidate();

            Compartment.Top.BlockMO = new STable("Block");
            Compartment.Top.RoutineMO = new STable("Routine");
            Compartment.Top.WhateverCodeMO = new STable("WhateverCode");
            Compartment.Top.SubMO = new STable("Sub");
            Compartment.Top.SubmethodMO = new STable("Submethod");
            Compartment.Top.MethodMO = new STable("Method");
            Compartment.Top.RegexMO = new STable("Regex");

            Compartment.Top.BlockMO.FillProtoClass(Compartment.Top.CodeMO);
            Compartment.Top.RoutineMO.FillProtoClass(Compartment.Top.BlockMO);
            Compartment.Top.WhateverCodeMO.FillProtoClass(Compartment.Top.BlockMO);
            Compartment.Top.SubMO.FillProtoClass(Compartment.Top.RoutineMO);
            Compartment.Top.MethodMO.FillProtoClass(Compartment.Top.RoutineMO);
            Compartment.Top.SubmethodMO.FillProtoClass(Compartment.Top.RoutineMO);
            Compartment.Top.RegexMO.FillProtoClass(Compartment.Top.MethodMO);

            Compartment.Top.LabelMO = new STable("Label");
            Compartment.Top.LabelMO.FillProtoClass(Compartment.Top.AnyMO, new string[] { "$!target", "$!name" },
                    new STable[] { Compartment.Top.LabelMO, Compartment.Top.LabelMO });

            Compartment.Top.EnumMO = new STable("Enum");
            Compartment.Top.EnumMO.FillProtoClass(Compartment.Top.AnyMO, new string[] { "$!key", "$!value" },
                    new STable[] { Compartment.Top.EnumMO, Compartment.Top.EnumMO });
            Compartment.Top.PairMO = new STable("Pair");
            Compartment.Top.PairMO.FillProtoClass(Compartment.Top.EnumMO);

            // forward reference
            Compartment.Top.StrMO = new STable("Str");
            Compartment.Top.BoolMO = new STable("Bool");

            Compartment.Top.IntMO = new STable("Int");
            Handler_Vonly(Compartment.Top.IntMO, "Numeric", new CtxReturnSelf(),
                    new CtxCallMethodUnboxNumeric(null));
            Handler_PandBox(Compartment.Top.IntMO, "Bool", new CtxIntBool(), Compartment.Top.BoolMO);
            Handler_PandBox(Compartment.Top.IntMO, "Str", new CtxIntStr(), Compartment.Top.StrMO);
            Handler_PandCont(Compartment.Top.IntMO, "succ", new CtxIntSuccish(+1));
            Handler_PandCont(Compartment.Top.IntMO, "pred", new CtxIntSuccish(-1));
            Compartment.Top.IntMO.FillProtoClass(Compartment.Top.AnyMO);

            Handler_Vonly(Compartment.Top.BoolMO, "Bool", new CtxReturnSelf(),
                    new CtxBoolUnbox());
            Compartment.Top.BoolMO.FillProtoClass(Compartment.Top.IntMO, new string[] { "$!index" },
                new STable[] { Compartment.Top.BoolMO });
            Compartment.Top.TrueV  = BoxRaw<int>(1, Compartment.Top.BoolMO);
            Compartment.Top.FalseV = BoxRaw<int>(0, Compartment.Top.BoolMO);
            Compartment.Top.FalseV.SetSlot(Compartment.Top.BoolMO, "$!index", BoxAnyMO(0, Compartment.Top.IntMO));
            Compartment.Top.TrueV.SetSlot(Compartment.Top.BoolMO, "$!index", BoxAnyMO(1, Compartment.Top.IntMO));

            Handler_Vonly(Compartment.Top.StrMO, "Str", new CtxReturnSelf(),
                    new CtxJustUnbox<string>(""));
            Handler_PandBox(Compartment.Top.StrMO, "Bool", new CtxStrBool(), Compartment.Top.BoolMO);
            Handler_PandCont(Compartment.Top.StrMO, "succ", new CtxStrSuccish(true));
            Handler_PandCont(Compartment.Top.StrMO, "pred", new CtxStrSuccish(false));
            Compartment.Top.StrMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.JunctionMO = new STable("Junction");
            Handler_PandBox(Compartment.Top.JunctionMO, "Bool", new CtxJunctionBool(), Compartment.Top.BoolMO);
            Compartment.Top.JunctionMO.AddMethod(0, "FALLBACK", MakeSub(JunctionFallbackSI, null));
            Compartment.Top.JunctionMO.FillProtoClass(Compartment.Top.MuMO,
                    new string[] { "$!kind_", "$!eigenstates_" },
                    new STable[] { Compartment.Top.JunctionMO, Compartment.Top.JunctionMO });

            Compartment.Top.IteratorMO = new STable("Iterator");
            Compartment.Top.IteratorMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.NumMO = new STable("Num");
            Handler_Vonly(Compartment.Top.NumMO, "Numeric", new CtxReturnSelf(),
                    new CtxCallMethodUnboxNumeric(null));
            Handler_Vonly(Compartment.Top.NumMO, "Str", new CtxStrNativeNum2Str(),
                    new CtxRawNativeNum2Str());
            Handler_PandBox(Compartment.Top.NumMO, "Bool", new CtxNum2Bool(), Compartment.Top.BoolMO);
            Handler_PandCont(Compartment.Top.NumMO, "succ", new CtxNumSuccish(+1));
            Handler_PandCont(Compartment.Top.NumMO, "pred", new CtxNumSuccish(-1));
            Compartment.Top.NumMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.RatMO = new STable("Rat");
            Handler_Vonly(Compartment.Top.RatMO, "Numeric", new CtxReturnSelf(),
                    new CtxCallMethodUnboxNumeric(null));
            Handler_PandBox(Compartment.Top.RatMO, "Bool", new CtxRatBool(), Compartment.Top.BoolMO);
            Handler_PandBox(Compartment.Top.RatMO, "Str", new CtxRatStr(), Compartment.Top.StrMO);
            Handler_PandCont(Compartment.Top.RatMO, "succ", new CtxRatSuccish(true));
            Handler_PandCont(Compartment.Top.RatMO, "pred", new CtxRatSuccish(false));
            Compartment.Top.RatMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.FatRatMO = new STable("FatRat");
            Handler_Vonly(Compartment.Top.FatRatMO, "Numeric", new CtxReturnSelf(),
                    new CtxCallMethodUnboxNumeric(null));
            Handler_PandBox(Compartment.Top.FatRatMO, "Bool", new CtxFatRatBool(), Compartment.Top.BoolMO);
            Handler_PandBox(Compartment.Top.FatRatMO, "Str", new CtxFatRatStr(), Compartment.Top.StrMO);
            Handler_PandCont(Compartment.Top.FatRatMO, "succ", new CtxFatRatSuccish(true));
            Handler_PandCont(Compartment.Top.FatRatMO, "pred", new CtxFatRatSuccish(false));
            Compartment.Top.FatRatMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.ComplexMO = new STable("Complex");
            Handler_Vonly(Compartment.Top.ComplexMO, "Numeric", new CtxReturnSelf(),
                    new CtxCallMethodUnboxNumeric(null));
            Handler_PandBox(Compartment.Top.ComplexMO, "Bool", new CtxComplexBool(), Compartment.Top.BoolMO);
            Handler_PandBox(Compartment.Top.ComplexMO, "Str", new CtxComplexStr(), Compartment.Top.StrMO);
            Handler_PandCont(Compartment.Top.ComplexMO, "succ", new CtxComplexSuccish(+1));
            Handler_PandCont(Compartment.Top.ComplexMO, "pred", new CtxComplexSuccish(-1));
            Compartment.Top.ComplexMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.StashMO = new STable("Stash");
            Compartment.Top.StashMO.FillProtoClass(Compartment.Top.AnyMO);
            Compartment.Top.StashP = new P6opaque(Compartment.Top.StashMO);

            Compartment.Top.ParameterMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.SignatureMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.ClassHOWMO = new STable("ClassHOW");
            Compartment.Top.ClassHOWMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.ParcelMO = new STable("Parcel");
            Handler_PandBox(Compartment.Top.ParcelMO, "iterator", new CtxParcelIterator(),
                    Compartment.Top.IteratorMO);
            Handler_PandBox(Compartment.Top.ParcelMO, "Str", new CtxParcelListStr(), Compartment.Top.StrMO);
            WrapHandler1(Compartment.Top.ParcelMO, "LISTSTORE", new IxParcelLISTSTORE());
            Handler_Vonly(Compartment.Top.ParcelMO, "list", new CtxParcelList(), null);
            Compartment.Top.ParcelMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.ListMO = new STable("List");
            WrapIndexy(Compartment.Top.ListMO, "postcircumfix:<[ ]>", new IxListAtPos(false),
                    null, null, new IxListBindPos());
            Handler_PandBox(Compartment.Top.ListMO, "Str", new CtxParcelListStr(), Compartment.Top.StrMO);
            Handler_Vonly(Compartment.Top.ListMO, "pop", new PopList(), null);
            Handler_Vonly(Compartment.Top.ListMO, "shift", new ShiftList(), null);
            WrapPushy(Compartment.Top.ListMO, "push", new PushList());
            WrapPushy(Compartment.Top.ListMO, "unshift", new UnshiftList());
            Handler_PandBox(Compartment.Top.ListMO, "iterator", new CtxListIterator(),
                    Compartment.Top.IteratorMO);
            Handler_PandBox(Compartment.Top.ListMO, "Bool", new CtxListBool(), Compartment.Top.BoolMO);
            Handler_PandBoxInty(Compartment.Top.ListMO, "Numeric", new CtxListNum());
            Handler_Vonly(Compartment.Top.ListMO, "list", new CtxReturnSelfList(), null);
            Compartment.Top.ListMO.FillProtoClass(Compartment.Top.AnyMO, new string[] { "$!items", "$!rest" },
                new STable[] { Compartment.Top.ListMO, Compartment.Top.ListMO });

            Compartment.Top.ArrayMO = new STable("Array");
            WrapHandler1(Compartment.Top.ArrayMO, "LISTSTORE", new IxArrayLISTSTORE());
            WrapIndexy(Compartment.Top.ArrayMO, "postcircumfix:<[ ]>", new IxListAtPos(true),
                    null, null, new IxListBindPos());
            Compartment.Top.ArrayMO.FillProtoClass(Compartment.Top.ListMO);

            Compartment.Top.HashMO = new STable("Hash");
            WrapHandler1(Compartment.Top.HashMO, "LISTSTORE", new IxHashLISTSTORE());
            WrapIndexy(Compartment.Top.HashMO, "postcircumfix:<{ }>", new IxHashAtKey(),
                    new IxHashExistsKey(), new IxHashDeleteKey(),
                    new IxHashBindKey());
            Handler_PandBox(Compartment.Top.HashMO, "iterator", new CtxHashIterator(), Compartment.Top.IteratorMO);
            Handler_PandBox(Compartment.Top.HashMO, "Bool", new CtxHashBool(), Compartment.Top.BoolMO);
            Handler_Vonly(Compartment.Top.HashMO, "hash", new CtxReturnSelfList(), null);
            Compartment.Top.HashMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.CursorMO = new STable("Cursor");
            WrapIndexy(Compartment.Top.CursorMO, "postcircumfix:<{ }>", new IxCursorAtKey(),
                    Compartment.Top.AnyMO.mro_exists_key, Compartment.Top.AnyMO.mro_delete_key,
                    Compartment.Top.AnyMO.mro_bind_key);
            WrapIndexy(Compartment.Top.CursorMO, "postcircumfix:<[ ]>", new IxCursorAtPos(),
                    null, null, Compartment.Top.AnyMO.mro_bind_pos);
            Compartment.Top.CursorMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.MatchMO = new STable("Match");
            WrapIndexy(Compartment.Top.MatchMO, "postcircumfix:<{ }>", new IxCursorAtKey(),
                    Compartment.Top.AnyMO.mro_exists_key, Compartment.Top.AnyMO.mro_delete_key,
                    Compartment.Top.AnyMO.mro_bind_key);
            WrapIndexy(Compartment.Top.MatchMO, "postcircumfix:<[ ]>", new IxCursorAtPos(),
                    null, null, Compartment.Top.AnyMO.mro_bind_pos);
            Handler_PandBox(Compartment.Top.MatchMO, "Str", new CtxMatchStr(), Compartment.Top.StrMO);
            Compartment.Top.MatchMO.FillProtoClass(Compartment.Top.AnyMO);

            Compartment.Top.ScalarMO = new STable("Scalar");
            Compartment.Top.ScalarMO.FillProtoClass(Compartment.Top.AnyMO);
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

        public static Variable NewLabelVar(Frame fr, string name) {
            P6opaque dob = new P6opaque(Compartment.Top.LabelMO);
            fr.MarkSharedChain();
            dob.slots[0] = fr;
            dob.slots[1] = name;
            return dob;
        }

        private static string DescribeException(int type, Frame tgt,
                string name, object payload) {
            if (type != SubInfo.ON_DIE)
                return "Illegal control operator: " +
                    SubInfo.DescribeControl(type, tgt, name);
            try {
                Variable v1 = (Variable) payload;
                if (v1.Fetch().mo.FindMethod("gist") == null)
                    return "(no .gist on " + v1.Fetch().mo + " yet)";
                Variable v2 = RunInferior(v1.Fetch().InvokeMethod(
                    GetInferiorRoot(), "gist", new Variable[] { v1 }, null));
                return v2.Fetch().mo.mro_raw_Str.Get(v2);
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

            //Console.WriteLine("Entering SearchForHandler(th={0:X}, type={1}, tgt={2:X}, name={3}", th.GetHashCode(), type, tgt == null ? 0 : tgt.GetHashCode(), name);

            // make sure we have a list for RunCATCH
            // TODO: possibly should make X::AdHoc?
            if (type == SubInfo.ON_DIE) {
                payload = Kernel.NewRWListVar(Builtins.array_constructor(
                    (Variable)payload).Fetch());
            }

            HashSet<Frame> visited = new HashSet<Frame>();
            for (csr = th; ; csr = csr.DynamicCaller()) {
                //Console.WriteLine("looking at {0:X} (caller: {1:X}) ...", csr == null ? 0 : csr.GetHashCode(), (csr == null || csr.caller == null) ? 0 : csr.caller.GetHashCode());
                if (csr == null)
                    break; // unhandled exception, Unwind handles
                if (visited.Contains(csr))
                    Panic("Frame tree is looooopy");
                visited.Add(csr);
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
                    if (csr.info == Kernel.RunCATCH_I && type == SubInfo.ON_DIE) {
                        // Fudgy implementation of SIMPLECATCH
                        // A non-control exception has been thrown from handler
                        // Unwind will DTRT with control flow, but we need
                        // to save the exception(s)
                        // avoids "trytles all the way down"
                        Variable unh = (Variable)csr.lex2;
                        Variable cur = (Variable)csr.lex3;
                        unh.Fetch().mo.mro_push.Invoke(unh, new Variable[] {
                                cur, (Variable) payload });
                    }
                    unf = csr;
                    break;
                }

                if (type == SubInfo.ON_DIE && csr.info.catch_ != null) {
                    Frame nfr = Kernel.RunCATCH_I.SetupCall(
                        Kernel.GetInferiorRoot(), null, null, null, null,
                        false, null);
                    nfr.lex0 = Kernel.MakeSub(csr.info.catch_, csr);
                    nfr.lex1 = payload;
                    Variable np = Kernel.RunInferior(nfr);
                    if (np.Fetch().mo.mro_raw_Bool.Get(np)) {
                        payload = np;
                    } else {
                        // leave old payload so Unwind can install it
                        unf = csr;
                        unip = -1;
                        break;
                    }
                }

                if (type != SubInfo.ON_DIE && csr.info.control != null) {
                    P6any sub = Kernel.MakeSub(csr.info.control, csr);
                    Frame nfr = csr.info.control.SetupCall(
                        Kernel.GetInferiorRoot(), csr, sub,
                        new Variable[] {
                            Builtins.MakeParcel(Builtins.MakeInt(type),
                                Kernel.BoxAnyMO(name, Compartment.Top.StrMO),
                                tgt == null ? Compartment.Top.AnyP : tgt)
                        }, null, false, null);
                    Variable np = Kernel.RunInferior(nfr);
                    if (np.Fetch().mo.mro_raw_Bool.Get(np)) {
                        unf = csr;
                        unip = -1;
                        break;
                    }
                }
            }
            //Console.WriteLine("done[{0:X}]", unf == null ? 0 : unf.GetHashCode());

            if (unf == null && type == SubInfo.ON_WARNING) {
                Console.Error.WriteLine(name + DescribeBacktrace(th, null));
                return th;
            }

            try {
                return Unwind(th, type, unf, unip, payload, tgt, name, null);
            } catch (Exception e) {
                Panic(e.ToString());
                return null;
            }
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
                //sb.AppendFormat(" [{0:X}]", from.GetHashCode());
                from = from.DynamicCaller();
            }
            return sb.ToString();
        }

        // let's try to avoid looping failure
        static void Panic(string str) {
            Console.Error.WriteLine("Internal error in exception dispatch: " + str);
            Compartment.disable_end = true;
            Environment.Exit(1);
        }

        public static Frame Unwind(Frame th, int type, Frame tf, int tip,
                object td, Frame tgt, string name, string bt) {
            Frame csr = th;
            if (tf != null && tf.info == null)
                Panic("Catching frame has no info?");
            if (tf != null && tf.caller == null)
                Panic("Catching frame has no caller?" + tf.info.name);
            while (csr != tf) {
                if (csr == null) Panic("Unwinding into null frame");
                if (csr.info == null) Panic("Null SubInfo?");
                if (csr.info == ExitRunloopSI) {
                    // when this exception reaches the outer runloop,
                    // more frames will be added
                    if (bt == null)
                        bt = DescribeException(type, tgt, name, td);
                    csr.lex0 = new ResumeUnwindException(type, tf, tip,
                            td, bt + DescribeBacktrace(th, csr.caller));
                    return csr;
                }
                if (csr.DynamicCaller() != csr.caller) {
                    // Return can't be used in this case, and the other
                    // stuff is just to make LEAVE/UNDO/KEEP work right
                    csr = csr.DynamicCaller();
                } else {
                    if (csr.caller == null) Panic(csr.info.name + " has no caller?");
                    // TODO: catch generated exceptions and add to @!
                    csr.caller.resultSlot = Compartment.Top.Nil;
                    Kernel.SetTopFrame(csr);
                    csr = csr.Return();
                }
            }
            if (type == SubInfo.ON_NEXTDISPATCH) {
                if (tf.curDisp == null)
                    Panic("ON_NEXTDISPATCH caught by nondispatch frame?? " + csr.info.name);
                // These are a bit special because there isn't actually a
                // catching frame.
                DispatchEnt de = tf.curDisp.next;
                P6opaque o = td as P6opaque;
                if (de != null) {
                    Variable[] p = tf.pos;
                    VarHash n = tf.named;
                    if (o != null) {
                        var tp = (Variable[]) o.slots[0];
                        n = o.slots[1] as VarHash;
                        // nextdispatch copies over the old self
                        p = new Variable[tp.Length + 1];
                        Array.Copy(tp, 0, p, 1, tp.Length);
                        p[0] = tf.pos[0];
                    }
                    return de.info.SetupCall(tf.caller, de.outer, de.ip6, p, n,
                            false, de);
                } else {
                    tf.caller.resultSlot = Compartment.Top.AnyP;
                    return tf.caller;
                }
            } else if (type == SubInfo.ON_DIE) {
                Variable exn = (Variable)td; // will be an Array
                if (exn.Fetch().mo.mro_raw_Numeric.Get(exn) == 1)
                    exn = exn.Fetch().mo.mro_at_pos.Get(exn, Builtins.MakeInt(0));
                tf.LexicalBind("$!", (Variable)exn);
                td = Compartment.Top.AnyP;
            }
            tf.ip = tip;
            tf.resultSlot = td;
            if (tip < 0) {
                // catch IP of -1 means to force an immediate return, as
                // when a CATCH phaser is triggered.
                tf.caller.resultSlot = Compartment.Top.Nil;
                return tf.Return();
            } else {
                return tf;
            }
        }

        public static void MainHandler(string uname, string[] args) {
            InitCompartment();
            InitGlobal();
            commandArgs = args;

            RuntimeUnit ru = (RuntimeUnit)
                RuntimeUnit.reg.LoadUnit(uname).root;

            Kernel.containerRootUnit = ru;
            Kernel.currentGlobals = ru.globals;

            RunMain(ru);
        }

        // Turns all the units of the app into subordinates of the
        // main unit
        static void RewriteUnits(RuntimeUnit from, RuntimeUnit root,
                HashSet<RuntimeUnit> done) {
            if (done.Contains(from))
                return;
            done.Add(from);

            foreach (RuntimeUnit z in from.depended_units)
                RewriteUnits(z, root, done);
            foreach (RuntimeUnit z in from.subordinates)
                RewriteUnits(z, root, done);

            from.owner = root;
            from.asm_name = from.name.Replace("::", ".");
            if (from != root) {
                root.subordinates.Add(from);
                from.subordinates.Clear();
            }
        }

        public static void Main(string[] args) {
            string cmd = args.Length > 0 ? args[0] : "-help";

            InitCompartment();
            InitGlobal();

            if (cmd == "-field-inventory") {
                foreach (Type ty in typeof(Kernel).Assembly.GetTypes()) {
                    if (ty.GetCustomAttributes(typeof(CompilerGeneratedAttribute), true).Length != 0)
                        continue;

                    foreach (FieldInfo fi in ty.GetFields(BindingFlags.Static |
                            BindingFlags.Public | BindingFlags.NonPublic)) {
                        if (fi.GetCustomAttributes(typeof(CompilerGeneratedAttribute), true).Length != 0)
                            continue;
                        // already classified
                        if (fi.GetCustomAttributes(typeof(ImmutableAttribute), true).Length != 0)
                            continue;
                        if (fi.GetCustomAttributes(typeof(CORESavedAttribute), true).Length != 0)
                            continue;
                        if (fi.GetCustomAttributes(typeof(CompartmentGlobalAttribute), true).Length != 0)
                            continue;
                        if (fi.GetCustomAttributes(typeof(TrueGlobalAttribute), true).Length != 0)
                            continue;
                        // ignore effectively constant fields
                        if (fi.IsLiteral)
                            continue;
                        Type ft = fi.FieldType;
                        // delegates can be stateful, but niecza
                        // doesn't assign stateful delegates to initonly fields
                        // niecza also does not use any stateful callhandlers
                        if (fi.IsInitOnly && (ft.IsPrimitive ||
                                ft == typeof(BigInteger) ||
                                ft == typeof(Type) ||
                                ft == typeof(Assembly) ||
                                ft == typeof(FieldInfo) ||
                                ft == typeof(ConstructorInfo) ||
                                ft == typeof(MethodInfo) ||
                                typeof(Delegate).IsAssignableFrom(ft) ||
                                //ft.IsSealed && ft.GetFields(
                                //    BindingFlags.Instance |
                                //    BindingFlags.Public |
                                //    BindingFlags.NonPublic).Length == 0 ||
                                typeof(ReflectObj).IsAssignableFrom(ft) ||
                                ft == typeof(string)))
                            continue;

                        Console.WriteLine(ty.FullName + "." + fi.Name);
                    }
                }
            }
            else if (cmd == "-gen-app" && args.Length == 3) {
                // Code regeneration: this is required for the bootstrap
                // procedure, because initial code generation makes Run.CORE
                // and we need to turn it into CORE for the compiler proper.

                if (Backend.prefix != "") {
                    Console.Error.WriteLine("-gen-app may only be used with Kernel.dll");
                    Environment.Exit(1);
                }

                string exename = args[1];
                string fromdir = args[2];

                // allow loading of Run. files, but don't try to load the
                // assemblies, since we're not Run.Kernel
                Backend.obj_dir = fromdir;
                Backend.prefix  = "Run.";
                Backend.cross_level_load = true;

                RuntimeUnit root = (RuntimeUnit)
                    RuntimeUnit.reg.LoadUnit("MAIN").root;

                // reset for writing
                Backend.obj_dir = AppDomain.CurrentDomain.BaseDirectory;
                Backend.prefix  = "";

                RewriteUnits(root, root, new HashSet<RuntimeUnit>());

                // reset for writability
                RuntimeUnit.reg = new ObjectRegistry();

                root.dll_name = exename + ".exe";
                root.asm_name = exename;
                root.name = exename;
                root.Save();
            }
            else if (cmd == "-run" && args.Length == 2) {
                MainHandler(args[1], new string[0]);
            } else {
                Console.WriteLine("usage: Kernel.dll -run Unit.Name");
                Console.WriteLine("usage: Kernel.dll -gen-app App.exe build/dir");
                Console.WriteLine("usage: Kernel.dll -run Unit.Name");
            }
        }

        public static bool ACCEPTS(Variable obj, Variable filter) {
            Variable ret = Kernel.RunInferior(filter.Fetch().
                InvokeMethod(Kernel.GetInferiorRoot(), "ACCEPTS",
                    new Variable[] { filter, obj }, null));
            return ret.Fetch().mo.mro_raw_Bool.Get(ret);
        }
    }

    public class Config {
        public static readonly int CGVerbose =
            int.Parse(Environment.GetEnvironmentVariable("NIECZA_CODEGEN_TRACE") ?? "0");
        public static readonly bool CGVerifiable =
            Environment.GetEnvironmentVariable("NIECZA_CODEGEN_UNVERIFIABLE") != null ? false : true;
        public static readonly bool CGForceSave =
            Environment.GetEnvironmentVariable("NIECZA_FORCE_SAVE") != null;
        public static readonly bool C3Trace =
            Environment.GetEnvironmentVariable("NIECZA_C3_TRACE") != null;
        public static readonly bool SerTrace =
            Environment.GetEnvironmentVariable("NIECZA_SER_TRACE") != null;
        public static readonly bool SerFailInfo =
            Environment.GetEnvironmentVariable("NIECZA_SER_FAIL_INFO") != null;
        public static readonly bool KeepIL =
            Environment.GetEnvironmentVariable("NIECZA_KEEP_IL") != null;
    }
}
