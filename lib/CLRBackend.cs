// This is the new CLR backend.  The old one generated C# from Perl, which
// was slow and gave us the limitations of C#; this one aims to be faster.
// Also, by making the Perl code emit a portable format, it makes future
// portability work easier.

using System;
using System.Reflection;
using System.Reflection.Emit;
using System.Collections.Generic;
using System.Text;
using System.IO;

using Niecza;

namespace Niecza.CLRBackend {
    // The portable format is a subset of JSON, and is current read
    // into a matching internal form.
    sealed class JScalar {
        string text;
        double val;
        bool has_val;

        public JScalar(string txt) { text = txt; }
        public string str { get { return text; } }
        public double num {
            get {
                if (has_val) return val;
                val = double.Parse(text);
                has_val = true;
                return val;
            }
        }
        public static string[] SA(int cut, object x) {
            object[] arr = (object[]) x;
            string[] r = new string[ arr.Length - cut ];
            for (int i = 0; i < r.Length; i++)
                r[i] = ((JScalar)arr[i+cut]).str;
            return r;
        }
        public static int[] IA(int cut, object x) {
            object[] arr = (object[]) x;
            int[] r = new int[ arr.Length - cut ];
            for (int i = 0; i < r.Length; i++)
                r[i] = (int)((JScalar)arr[i+cut]).num;
            return r;
        }
        public static bool B(object x) {
            string s = S(x);
            if (s == "1")  return true;
            if (s == "0" || s == "") return false;
            throw new ArgumentException(s);
        }
        public static int I(object x) { return (int)((JScalar)x).num; }
        public static string S(object x) { return x == null ? null : ((JScalar)x).str; }
        public override string ToString() { return text; }
    }

    class Reader {
        static char GetHexQuad(char[] s, int ix) {
            int acc = 0;
            for (int i = 0; i < 4; i++) {
                acc <<= 4;
                int ch = (int)s[ix+i];
                acc += (ch>=(int)'a'&&ch<=(int)'f') ? (ch + 10 - (int)'a') :
                       (ch>=(int)'A'&&ch<=(int)'F') ? (ch + 10 - (int)'A') :
                       (ch - (int)'0');
            }
            return (char)acc;
        }

        public static object Read(string inputx) {
            char[] input = inputx.ToCharArray();
            int ilen = input.Length;
            int start, write;
            int ix = 0;
            List<List<object>> containers = new List<List<object>>();
            char i;
            while (true) {
                i = input[ix];
                if (i == '\t' || i == ' ' || i == '\r' || i == '\n' ||
                        i == ',') {
                    ix++;
                    continue;
                }
                if (i == '[') {
                    containers.Add(new List<object>());
                    ix++;
                    continue;
                }
                if (i == ']') {
                    object[] r = containers[containers.Count - 1].ToArray();
                    containers.RemoveAt(containers.Count - 1);
                    if (containers.Count == 0) return r;
                    containers[containers.Count - 1].Add(r);
                    ix++;
                    continue;
                }
                if (i == 'n' && ilen >= ix + 4 &&
                        input[ix+1] == 'u' && input[ix+2] == 'l' &&
                        input[ix+3] == 'l') {
                    containers[containers.Count - 1].Add(null);
                    ix += 4;
                    continue;
                }
                if (i == '"') {
                    ix++;
                    start = ix;
                    write = ix;
                    while (true) {
                        i = input[ix];
                        if (i == '\\') {
                            switch (input[ix+1]) {
                                case '/': i = '/'; break;
                                case '\\': i = '\\'; break;
                                case '"': i = '"'; break;
                                case 't': i = '\t'; break;
                                case 'r': i = '\r'; break;
                                case 'n': i = '\n'; break;
                                case 'f': i = '\f'; break;
                                case 'b': i = '\b'; break;
                                case 'u': i = GetHexQuad(input, ix+2); ix += 4; break;
                            }
                            ix += 2;
                            input[write++] = i;
                        } else if (i == '"') {
                            break;
                        } else {
                            input[write++] = i;
                            ix++;
                        }
                    }
                    ix++;
                    containers[containers.Count - 1].Add(new JScalar(new string(input, start, write - start)));
                    continue;
                }
                start = ix;
                while (true) {
                    i = input[ix];
                    if (i == ',' || i == '\r' || i == '\t' || i == '\n' ||
                            i == ' ' || i == ']')
                        break;
                    ix++;
                }
                containers[containers.Count - 1].Add(new JScalar(new string(input, start, ix - start)));
            }
        }
    }

    // Because this is the *CLR* backend's Unit object, it's always
    // associated with either an Assembly or an AssemblyBuilder.
    class Unit {
        public readonly Xref mainline_ref;
        public readonly string name;
        public readonly object[] log;
        public readonly string setting;
        public readonly Xref bottom_ref;
        public readonly object[] xref;
        public readonly object[] tdeps;

        public readonly Dictionary<string, Package> exp_pkg;

        public Assembly clrAssembly;
        public Type clrType;

        public Unit(object[] from) {
            mainline_ref = new Xref(from[0] as object[]);
            name = ((JScalar) from[1]).str;
            log = from[2] as object[];
            setting = from[3] == null ? null : ((JScalar) from[3]).str;
            bottom_ref = Xref.from(from[4] as object[]);
            xref = from[5] as object[];
            exp_pkg = new Dictionary<string,Package>();
            for (int i = 0; i < xref.Length; i++) {
                if (xref[i] == null) continue;
                object[] xr = (object[]) xref[i];
                if (xr.Length > 7) {
                    xref[i] = new StaticSub(this, xr);
                } else {
                    xref[i] = Package.From(xr);
                    (xref[i] as Package).NoteExports(exp_pkg);
                }
            }
            tdeps = from[6] as object[];
        }

        public void BindDepends() {
            foreach (object x in tdeps) {
                string n = ((JScalar)(((object[]) x)[0])).str;
                if (n == name) continue;
                Unit o = CLRBackend.GetUnit(n);
                foreach (KeyValuePair<string,Package> kv in o.exp_pkg)
                    exp_pkg[kv.Key] = kv.Value;
            }
        }

        public void VisitSubsPostorder(Action<int,StaticSub> cb) {
            DoVisitSubsPostorder(mainline_ref.index, cb);
        }

        public void VisitSubsPreorder(Action<int,StaticSub> cb) {
            DoVisitSubsPreorder(mainline_ref.index, cb);
        }

        public void VisitPackages(Action<int,Package> cb) {
            for (int i = 0; i < xref.Length; i++) {
                Package p = xref[i] as Package;
                if (p != null)
                    cb(i, p);
            }
        }

        private void DoVisitSubsPostorder(int ix, Action<int,StaticSub> cb) {
            StaticSub s = xref[ix] as StaticSub;
            foreach (int z in s.zyg)
                DoVisitSubsPostorder(z, cb);
            cb(ix,s);
        }

        private void DoVisitSubsPreorder(int ix, Action<int,StaticSub> cb) {
            StaticSub s = xref[ix] as StaticSub;
            cb(ix,s);
            foreach (int z in s.zyg)
                DoVisitSubsPreorder(z, cb);
        }

        public Package GetPackage(string[] strs, int f, int l) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < l; i++) {
                string rxes = strs[i+f];
                sb.Append((char)(rxes.Length >> 16));
                sb.Append((char)(rxes.Length & 0xFFFF));
                sb.Append(rxes);
            }
            Package p;
            exp_pkg.TryGetValue(sb.ToString(), out p);
            return p;
        }

        public Package GetCorePackage(string name) {
            StaticSub r = (StaticSub) (bottom_ref ?? mainline_ref).Resolve();
            return r.GetCorePackage(name);
        }

        public static string SharedName(char type, int ix, string name) {
            StringBuilder sb = new StringBuilder();
            sb.Append(type);
            sb.Append(ix);
            sb.Append('_');
            foreach (char c in name) {
                if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z')
                        || (c >= 'A' && c <= 'Z')) {
                    sb.Append(c);
                } else if (c == '_') {
                    sb.Append("__");
                } else if (c <= (char)255) {
                    sb.AppendFormat("_{0:X2}", (int)c);
                } else {
                    sb.AppendFormat("_U{0:X4}", (int)c);
                }
            }
            return sb.ToString();
        }

        public void BindFields(Func<string,Type,FieldInfo> binder) {
            VisitSubsPostorder(delegate(int ix, StaticSub sub) {
                sub.BindFields(ix, binder);
            });
            VisitPackages(delegate(int ix, Package pkg) {
                pkg.BindFields(ix, binder);
            });
        }
    }

    class Xref {
        public readonly string unit;
        public readonly int index;
        public readonly string name;

        public static Xref from(object[] x) {
            return (x == null) ? null : new Xref(x);
        }
        public Xref(object[] from) : this(from, 0) {}
        public Xref(object[] from, int ofs) {
            unit  = ((JScalar)from[ofs+0]).str;
            index = (int)((JScalar)from[ofs+1]).num;
            name  = ((JScalar)from[ofs+2]).str;
        }
        public object Resolve() { return CLRBackend.Resolve(this); }
    }

    class Package {
        public readonly string name;
        public readonly string type;
        public readonly object[] exports;

        public Package(object[] p) {
            type = ((JScalar)p[0]).str;
            name = ((JScalar)p[1]).str;
            exports = (object[]) p[2];
        }

        internal void NoteExports(Dictionary<string,Package> dp) {
            foreach (object x in exports) {
                object[] rx = (object[]) x;
                StringBuilder sb = new StringBuilder();
                foreach (object rxe in rx) {
                    string rxes = ((JScalar) rxe).str;
                    sb.Append((char)(rxes.Length >> 16));
                    sb.Append((char)(rxes.Length & 0xFFFF));
                    sb.Append(rxes);
                }
                dp[sb.ToString()] = this;
            }
        }

        public virtual void BindFields(int ix,
                Func<string,Type,FieldInfo> binder) { }

        public static Package From(object[] p) {
            string type = ((JScalar)p[0]).str;
            if (type == "parametricrole")
                return new ParametricRole(p);
            else if (type == "role")
                return new Role(p);
            else if (type == "class")
                return new Class(p);
            else if (type == "grammar")
                return new Grammar(p);
            else if (type == "module")
                return new Module(p);
            else if (type == "package")
                return new Package(p);
            else
                throw new Exception("unknown package type " + p);
        }
    }

    class Module: Package {
        public Module(object[] p) : base(p) { }
    }

    class ModuleWithTypeObject: Module {
        public FieldInfo typeObject;
        public FieldInfo typeVar;
        public FieldInfo metaObject;

        public override void BindFields(int ix,
                Func<string,Type,FieldInfo> binder) {
            typeObject = binder(Unit.SharedName('T', ix, name), Tokens.IP6);
            typeVar    = binder(Unit.SharedName('V', ix, name), Tokens.Variable);
            metaObject = binder(Unit.SharedName('M', ix, name), Tokens.DynMetaObject);
        }

        public ModuleWithTypeObject(object[] p) : base(p) { }
    }

    class Class: ModuleWithTypeObject {
        public readonly string[] attributes;
        public readonly object[] methods;
        public readonly object[] superclasses;
        public readonly object[] linearized_mro;
        public Class(object[] p) : base(p) {
            attributes = JScalar.SA(0, p[3]);
            methods = p[4] as object[];
            superclasses = p[5] as object[];
            linearized_mro = p[6] as object[];
        }
    }

    class Grammar: Class {
        public Grammar(object[] p) : base(p) { }
    }

    class Role: ModuleWithTypeObject {
        public readonly string[] attributes;
        public readonly object[] methods;
        public readonly object[] superclasses;
        public Role(object[] p) : base(p) {
            attributes = JScalar.SA(0,p[3]);
            methods = p[4] as object[];
            superclasses = p[5] as object[];
        }
    }

    class ParametricRole: ModuleWithTypeObject {
        public readonly object[] attributes;
        public readonly object[] methods;
        public readonly object[] superclasses;
        public ParametricRole(object[] p) : base(p) {
            attributes = p[3] as object[];
            methods = p[4] as object[];
            superclasses = p[5] as object[];
        }
    }

    class StaticSub {
        public const int RUN_ONCE = 1;
        public const int SPAD_EXISTS = 2;
        public const int GATHER_HACK = 4;
        public const int STRONG_USED = 8;
        public const int RETURNABLE = 16;
        public const int AUGMENTING = 32;
        public readonly string name;
        public readonly Unit unit;
        public readonly Xref outer;
        public readonly int flags;
        public readonly int[] zyg;
        public readonly object parametric_role_hack;
        public readonly object augment_hack;
        public readonly int is_phaser;
        public readonly Xref body_of;
        public readonly Xref in_class;
        public readonly string[] cur_pkg;
        public readonly string sclass;
        public readonly object ltm;
        public readonly object[] exports;
        public readonly object sig;
        public readonly List<KeyValuePair<string,Lexical>> lexicals;
        public readonly Dictionary<string,Lexical> l_lexicals;
        public readonly object body;

        public FieldInfo protosub;
        public FieldInfo subinfo;
        public FieldInfo protopad;
        public int nlexn;

        public StaticSub(Unit unit, object[] s) {
            this.unit = unit;
            name = ((JScalar)s[0]).str;
            outer = Xref.from(s[1] as object[]);
            flags = (int) ((JScalar)s[2]).num;
            object[] r_zyg = s[3] as object[];
            parametric_role_hack = s[4];
            augment_hack = s[5];
            is_phaser = s[6] == null ? -1 : (int) ((JScalar) s[6]).num;
            body_of = Xref.from(s[7] as object[]);
            in_class = Xref.from(s[8] as object[]);
            object[] r_cur_pkg = s[9] as object[];
            sclass = ((JScalar)s[10]).str;
            ltm = s[11];
            exports = (object[]) s[12];
            sig = s[13];

            zyg = new int[ r_zyg.Length ];
            for (int i = 0; i < r_zyg.Length; i++)
                zyg[i] = (int) ((JScalar) r_zyg[i]).num;

            cur_pkg = new string[ r_cur_pkg.Length ];
            for (int i = 0; i < r_cur_pkg.Length; i++)
                cur_pkg[i] = ((JScalar) r_cur_pkg[i]).str;

            object[] r_lexicals = s[14] as object[];
            body = s[15];
            lexicals = new List<KeyValuePair<string,Lexical>>();
            l_lexicals = new Dictionary<string,Lexical>();
            for (int i = 0; i < r_lexicals.Length; i++) {
                object[] bl = r_lexicals[i] as object[];
                string lname = ((JScalar)bl[0]).str;
                string type = ((JScalar)bl[1]).str;
                Lexical obj = null;

                if (type == "simple") {
                    obj = new LexSimple(bl);
                } else if (type == "common") {
                    obj = new LexCommon(bl);
                } else if (type == "sub") {
                    obj = new LexSub(bl);
                } else if (type == "alias") {
                    obj = new LexAlias(bl);
                } else if (type == "stash") {
                    obj = new LexStash(unit, bl);
                } else {
                    throw new Exception("unknown lex type " + type);
                }

                lexicals.Add(new KeyValuePair<string,Lexical>(lname, obj));
                l_lexicals[lname] = obj;
            }
        }

        public bool IsCore() {
            string s = unit.name;
            return (s == "SAFE" || s == "CORE");
        }

        public void BindFields(int ix, Func<string,Type,FieldInfo> binder) {
            subinfo  = binder(Unit.SharedName('I', ix, name), Tokens.SubInfo);
            if ((flags & SPAD_EXISTS) != 0)
                protopad = binder(Unit.SharedName('P', ix, name), Tokens.Frame);
            if (outer == null || (((StaticSub)outer.Resolve()).flags
                        & SPAD_EXISTS) != 0)
                protosub = binder(Unit.SharedName('S', ix, name), Tokens.IP6);

            nlexn = 0;
            for (int i = 0; i < lexicals.Count; i++)
                lexicals[i].Value.BindFields(ix, i, this,
                        lexicals[i].Key, binder);
        }

        public Package GetCorePackage(string name) {
            StaticSub csr = this;
            while (csr.unit.name != "SAFE" && csr.unit.name != "CORE")
                csr = (StaticSub) csr.outer.Resolve();
            while (!csr.l_lexicals.ContainsKey(name))
                csr = (StaticSub) csr.outer.Resolve();
            LexStash lx = (LexStash)csr.l_lexicals[name];
            return unit.GetPackage(lx.path, 0, lx.path.Length);
        }
    }

    abstract class Lexical {
        public virtual void BindFields(int six, int lix, StaticSub sub,
                string name, Func<string,Type,FieldInfo> binder) { }
        public virtual ClrOp SetCode(int up, ClrOp head) {
            throw new Exception("Lexicals of type " + this + " cannot be bound");
        }
        public abstract ClrOp GetCode(int up);
        public static bool IsDynamicName(string name) {
            if (name == "$_") return true;
            if (name.Length < 2) return false;
            if (name[0] == '*' || name[0] == '?') return true;
            if (name[1] == '*' || name[1] == '?') return true;
            return false;
        }
    }

    class LexVarish : Lexical {
        public int index;
        public FieldInfo stg;

        public override ClrOp GetCode(int up) {
            return (index >= 0) ? (ClrOp)new ClrPadGet(up, index)
                                : new ClrGetSField(stg);
        }

        public override ClrOp SetCode(int up, ClrOp to) {
            return (index >= 0) ? (ClrOp)new ClrPadSet(up, index, to)
                                : new ClrSetSField(stg, to);
        }

        public override void BindFields(int six, int lix, StaticSub sub,
                string name, Func<string,Type,FieldInfo> binder) {
            if (IsDynamicName(name) || (sub.flags & StaticSub.RUN_ONCE) == 0) {
                index = sub.nlexn++;
            } else {
                index = -1;
                stg = binder(Unit.SharedName('L', six, name), Tokens.Variable);
                sub.nlexn++;
            }
        }
    }

    class LexSimple : LexVarish {
        public const int NOINIT = 4;
        public const int LIST = 2;
        public const int HASH = 1;
        public readonly int flags;

        public LexSimple(object[] l) {
            flags = (int)((JScalar)l[2]).num;
        }
    }

    class LexCommon : Lexical {
        public readonly string[] path;
        public FieldInfo stg;
        public LexCommon(object[] l) {
            path = new string[l.Length - 2];
            for (int i = 2; i < l.Length; i++)
                path[i-2] = ((JScalar)l[i]).str;
        }
        public override ClrOp GetCode(int up) {
            return new ClrGetField(Tokens.BValue_v,
                    new ClrGetSField(stg));
        }
        public override ClrOp SetCode(int up, ClrOp to) {
            return new ClrSetField(Tokens.BValue_v,
                    new ClrGetSField(stg), to);
        }
        public override void BindFields(int six, int lix, StaticSub sub,
                string name, Func<string,Type,FieldInfo> binder) {
            stg = binder(Unit.SharedName('B', six, name), Tokens.BValue);
        }
    }

    class LexSub : LexVarish {
        public readonly Xref def;
        public LexSub(object[] l) {
            def = new Xref(l, 2);
        }
    }

    class LexAlias : Lexical {
        public readonly string to;
        public LexAlias(object[] l) {
            to = ((JScalar)l[2]).str;
        }
        public override ClrOp GetCode(int up) { throw new NotImplementedException(); }
    }

    class LexStash : Lexical {
        public readonly Unit unit;
        public readonly string[] path;
        public Package GetPackage() {
            return unit.GetPackage(path, 0, path.Length);
        }
        public override ClrOp GetCode(int up) {
            ModuleWithTypeObject p = GetPackage() as ModuleWithTypeObject;
            if (p == null) {
                return new ClrMethodCall(false, Tokens.Kernel_NewROScalar,
                        new ClrOp[] { new ClrGetSField(Tokens.Kernel_AnyP) });
            }
            return new ClrMethodCall(false, Tokens.Kernel_NewROScalar,
                    new ClrOp[] { new ClrGetSField(p.typeObject) });
        }
        public LexStash(Unit u, object[] l) {
            unit = u;
            path = new string[l.Length - 2];
            for (int i = 0; i < path.Length; i++)
                path[i] = ((JScalar)l[i+2]).str;
        }
    }

    // Extra info needed beyond what ILGenerator alone provides.  Note
    // that switch generation is done in another pass.
    class CgContext {
        public ILGenerator il;
        public TypeBuilder tb;
        public int next_case;
        public Label[] cases;
        public int num_cases;
        public Dictionary<string,int> named_cases
            = new Dictionary<string,int>();
        public Dictionary<string,Label> named_labels
            = new Dictionary<string,Label>();
        public string[] let_names = new string[0];
        public Type[] let_types = new Type[0];
        public LocalBuilder ospill, pspill, nspill;
        public List<int> lineStack = new List<int>();
        public List<int> lineBuffer = new List<int>();
        public List<int> ehspanBuffer = new List<int>();
        public List<string> ehlabelBuffer = new List<string>();

        public void make_ospill() {
            if (ospill == null)
                ospill = il.DeclareLocal(Tokens.Variable);
        }

        public void save_line() {
            lineBuffer.Add(lineStack.Count == 0 ? 0 : lineStack[lineStack.Count - 1]);
        }

        [ThreadStatic] private static int next_array;
        public void EmitIntArray(int[] vec) {
            EmitInt(vec.Length);
            // the mono JIT checks for this exact sequence
            il.Emit(OpCodes.Newarr, Tokens.Int32);
            if (vec.Length != 0) {
                byte[] buf = new byte[vec.Length * 4];
                int r = 0;
                for (int i = 0; i < vec.Length; i++) {
                    uint d = (uint) vec[i];
                    buf[r++] = (byte)((d >>  0) & 0xFF);
                    buf[r++] = (byte)((d >>  8) & 0xFF);
                    buf[r++] = (byte)((d >> 16) & 0xFF);
                    buf[r++] = (byte)((d >> 24) & 0xFF);
                }
                FieldBuilder fb = tb.DefineInitializedData(
                        "A" + (next_array++), buf, 0);
                il.Emit(OpCodes.Dup);
                il.Emit(OpCodes.Ldtoken, fb);
                il.Emit(OpCodes.Call, typeof(System.Runtime.CompilerServices.RuntimeHelpers).GetMethod("InitializeArray"));
            }
        }

        // logic stolen from mcs
        public void EmitInt(int i) {
            switch (i) {
                case -1: il.Emit(OpCodes.Ldc_I4_M1); break;
                case 0: il.Emit(OpCodes.Ldc_I4_0); break;
                case 1: il.Emit(OpCodes.Ldc_I4_1); break;
                case 2: il.Emit(OpCodes.Ldc_I4_2); break;
                case 3: il.Emit(OpCodes.Ldc_I4_3); break;
                case 4: il.Emit(OpCodes.Ldc_I4_4); break;
                case 5: il.Emit(OpCodes.Ldc_I4_5); break;
                case 6: il.Emit(OpCodes.Ldc_I4_6); break;
                case 7: il.Emit(OpCodes.Ldc_I4_7); break;
                case 8: il.Emit(OpCodes.Ldc_I4_8); break;
                default:
                    if (i >= -128 && i < 127) {
                        il.Emit(OpCodes.Ldc_I4_S, (sbyte) i);
                    } else {
                        il.Emit(OpCodes.Ldc_I4, i);
                    }
                    break;
            }
        }

        public void EmitPreSetlex(int ix) {
            if (ix >= (Tokens.NumInt32 + Tokens.NumInline)) {
                il.Emit(OpCodes.Ldfld, Tokens.Frame_lexn);
                EmitInt(ix - (Tokens.NumInt32 + Tokens.NumInline));
            }
        }

        public void EmitSetlex(int ix, Type t) {
            if (ix >= Tokens.NumInt32 && t.IsValueType)
                il.Emit(OpCodes.Box, t);

            if (ix >= (Tokens.NumInt32 + Tokens.NumInline)) {
                il.Emit(OpCodes.Stelem_Ref);
            } else if (ix >= Tokens.NumInt32) {
                il.Emit(OpCodes.Stfld,
                        Tokens.Frame_lexobj[ix - Tokens.NumInt32]);
            } else {
                il.Emit(OpCodes.Stfld, Tokens.Frame_lexi32[ix]);
            }
        }

        public void EmitGetlex(int ix, Type t) {
            if (ix >= (Tokens.NumInt32 + Tokens.NumInline)) {
                il.Emit(OpCodes.Ldfld, Tokens.Frame_lexn);
                EmitInt(ix - (Tokens.NumInt32 + Tokens.NumInline));
                il.Emit(OpCodes.Ldelem_Ref);
            } else if (ix >= Tokens.NumInt32) {
                il.Emit(OpCodes.Ldfld,
                        Tokens.Frame_lexobj[ix - Tokens.NumInt32]);
            } else {
                il.Emit(OpCodes.Ldfld, Tokens.Frame_lexi32[ix]);
            }

            if (ix >= Tokens.NumInt32 &&
                    (CLRBackend.Verifiable || t.IsValueType)) {
                il.Emit(OpCodes.Unbox_Any, t);
            }
        }
    }

    sealed class Tokens {
        public static readonly Type Void = typeof(void);
        public static readonly Type String = typeof(string);
        public static readonly Type Boolean = typeof(bool);
        public static readonly Type Int32 = typeof(int);
        public static readonly Type IntPtr = typeof(IntPtr);
        public static readonly Type Double = typeof(double);
        public static readonly Type Frame = typeof(Frame);
        public static readonly Type Kernel = typeof(Kernel);
        public static readonly Type Builtins = typeof(Builtins);
        public static readonly Type SubInfo = typeof(SubInfo);
        public static readonly Type IP6 = typeof(IP6);
        public static readonly Type Variable = typeof(Variable);
        public static readonly Type BValue = typeof(BValue);
        public static readonly Type DynObject = typeof(DynObject);
        public static readonly Type DynBlockDelegate = typeof(DynBlockDelegate);
        public static readonly Type DynMetaObject = typeof(DynMetaObject);
        public static readonly Type VarHash = typeof(VarHash);
        public static readonly Type VVarList = typeof(VarDeque);
        public static readonly Type FVarList = typeof(Variable[]);
        public static readonly Type Cursor = typeof(Cursor);
        public static readonly Type RxFrame = typeof(RxFrame);
        public static readonly Type CC = typeof(CC);
        public static readonly Type LAD = typeof(LAD);

        public static readonly ConstructorInfo SubInfo_ctor =
            SubInfo.GetConstructor(new Type[] {
                    String, typeof(int[]), typeof(DynBlockDelegate),
                    SubInfo, LAD, typeof(int[]), typeof(string[]),
                    Int32, typeof(string[]), typeof(int[]) });
        public static readonly ConstructorInfo DynBlockDelegate_ctor =
            typeof(DynBlockDelegate).GetConstructor(new Type[] {
                    typeof(object), typeof(IntPtr) });
        public static readonly ConstructorInfo DynObject_ctor =
            typeof(DynObject).GetConstructor(new Type[] {
                    DynMetaObject });
        public static readonly ConstructorInfo DMO_ctor =
            DynMetaObject.GetConstructor(new Type[] { String });
        public static readonly ConstructorInfo RxFrame_ctor =
            RxFrame.GetConstructor(new Type[] { String, Cursor, Boolean, Boolean });
        public static readonly ConstructorInfo Frame_ctor =
            Frame.GetConstructor(new Type[] { Frame, Frame, SubInfo });
        public static readonly ConstructorInfo SV_ctor =
            typeof(SimpleVariable).GetConstructor(new Type[] {
                    Boolean, Boolean, DynMetaObject, typeof(ViviHook), IP6 });
        public static readonly ConstructorInfo SubViviHook_ctor =
            typeof(SubViviHook).GetConstructor(new Type[] { IP6 });
        public static readonly ConstructorInfo HashViviHook_ctor =
            typeof(HashViviHook).GetConstructor(new Type[] { IP6, String });
        public static readonly ConstructorInfo ArrayViviHook_ctor =
            typeof(ArrayViviHook).GetConstructor(new Type[] { IP6, Int32 });
        public static readonly ConstructorInfo NewHashViviHook_ctor =
            typeof(NewHashViviHook).GetConstructor(new Type[] { Variable, String });
        public static readonly ConstructorInfo NewArrayViviHook_ctor =
            typeof(NewArrayViviHook).GetConstructor(new Type[] { Variable, Int32 });
        public static readonly ConstructorInfo CC_ctor =
            CC.GetConstructor(new Type[] { typeof(int[]) });
        public static readonly Dictionary<string,ConstructorInfo> LADctors
            = _LADctors();
        private static Dictionary<string,ConstructorInfo> _LADctors() {
            Dictionary<string,ConstructorInfo> n =
                new Dictionary<string,ConstructorInfo>();
            n["CC"] = typeof(LADCC).GetConstructor(new Type[] { CC });
            n["Str"] = typeof(LADStr).GetConstructor(new Type[] { String });
            n["Param"] = typeof(LADParam).GetConstructor(new Type[] { String });
            n["Method"] = typeof(LADMethod).GetConstructor(new Type[] { String });
            n["ProtoRegex"] = typeof(LADProtoRegex).GetConstructor(new Type[] { String });
            n["StrNoCase"] = typeof(LADStrNoCase).GetConstructor(new Type[] { String });
            n["Imp"] = typeof(LADImp).GetConstructor(new Type[] { });
            n["Dot"] = typeof(LADDot).GetConstructor(new Type[] { });
            n["None"] = typeof(LADNone).GetConstructor(new Type[] { });
            n["Null"] = typeof(LADNull).GetConstructor(new Type[] { });
            n["Plus"] = typeof(LADPlus).GetConstructor(new Type[] { LAD });
            n["Star"] = typeof(LADStar).GetConstructor(new Type[] { LAD });
            n["Opt"] = typeof(LADOpt).GetConstructor(new Type[] { LAD });
            n["Sequence"] = typeof(LADSequence).GetConstructor(new Type[] { typeof(LAD[]) });
            n["Any"] = typeof(LADAny).GetConstructor(new Type[] { typeof(LAD[]) });
            return n;
        }

        public static readonly MethodInfo IP6_InvokeMethod =
            IP6.GetMethod("InvokeMethod");
        public static readonly MethodInfo IP6_Invoke =
            IP6.GetMethod("Invoke");
        public static readonly MethodInfo IP6_SetSlot =
            IP6.GetMethod("SetSlot");
        public static readonly MethodInfo IP6_GetSlot =
            IP6.GetMethod("GetSlot");
        public static readonly MethodInfo Variable_Fetch =
            Variable.GetMethod("Fetch");
        public static readonly MethodInfo VVarList_Item =
            VVarList.GetMethod("get_Item");
        public static readonly MethodInfo VarHash_Remove =
            VarHash.GetMethod("Remove");
        public static readonly MethodInfo VarHash_get_Item =
            VarHash.GetMethod("get_Item");
        public static readonly MethodInfo VarHash_set_Item =
            VarHash.GetMethod("set_Item");
        public static readonly MethodInfo Kernel_MakeSub =
            typeof(Kernel).GetMethod("MakeSub");
        public static readonly MethodInfo Kernel_Die =
            typeof(Kernel).GetMethod("Die");
        public static readonly MethodInfo Kernel_SFH =
            typeof(Kernel).GetMethod("SearchForHandler");
        public static readonly MethodInfo Kernel_BootModule =
            typeof(Kernel).GetMethod("BootModule");
        public static readonly MethodInfo Kernel_RunLoop =
            typeof(Kernel).GetMethod("RunLoop");
        public static readonly MethodInfo Kernel_NewROScalar =
            typeof(Kernel).GetMethod("NewROScalar");
        public static readonly MethodInfo Kernel_NewRWListVar =
            typeof(Kernel).GetMethod("NewRWListVar");
        public static readonly MethodInfo Kernel_NewRWScalar =
            typeof(Kernel).GetMethod("NewRWScalar");
        public static readonly MethodInfo Kernel_NewBoundVar =
            typeof(Kernel).GetMethod("NewBoundVar");
        public static readonly MethodInfo Kernel_IterHasFlat =
            typeof(Kernel).GetMethod("IterHasFlat");
        public static readonly MethodInfo Kernel_ContextHelper =
            typeof(Kernel).GetMethod("ContextHelper");
        public static readonly MethodInfo Kernel_StatusHelper =
            typeof(Kernel).GetMethod("StatusHelper");
        public static readonly MethodInfo Kernel_SetStatus =
            typeof(Kernel).GetMethod("SetStatus");
        public static readonly MethodInfo Kernel_SortHelper =
            typeof(Kernel).GetMethod("SortHelper");
        public static readonly MethodInfo Kernel_GetVar =
            typeof(Kernel).GetMethod("GetVar");
        public static readonly MethodInfo Kernel_CreatePath =
            typeof(Kernel).GetMethod("CreatePath");
        public static readonly MethodInfo Kernel_AddPhaser =
            typeof(Kernel).GetMethod("AddPhaser");
        public static readonly MethodInfo Kernel_FirePhasers =
            typeof(Kernel).GetMethod("FirePhasers");
        public static readonly MethodInfo DMO_AddPrivateMethod =
            typeof(DynMetaObject).GetMethod("AddPrivateMethod");
        public static readonly MethodInfo DMO_AddMethod =
            typeof(DynMetaObject).GetMethod("AddMethod");
        public static readonly MethodInfo DMO_Invalidate =
            typeof(DynMetaObject).GetMethod("Invalidate");
        public static readonly MethodInfo DMO_FillParametricRole =
            typeof(DynMetaObject).GetMethod("FillParametricRole");
        public static readonly MethodInfo DMO_FillRole =
            typeof(DynMetaObject).GetMethod("FillRole");
        public static readonly MethodInfo DMO_FillClass =
            typeof(DynMetaObject).GetMethod("FillClass");
        public static readonly MethodInfo RxFrame_PushBacktrack =
            typeof(RxFrame).GetMethod("PushBacktrack");
        public static readonly MethodInfo RxFrame_PushCapture =
            typeof(RxFrame).GetMethod("PushCapture");
        public static readonly MethodInfo Console_WriteLine =
            typeof(Console).GetMethod("WriteLine", new Type[] { typeof(string) });
        public static readonly MethodInfo Console_Write =
            typeof(Console).GetMethod("Write", new Type[] { typeof(string) });
        public static readonly MethodInfo Environment_Exit =
            typeof(Environment).GetMethod("Exit");
        public static readonly MethodInfo StringBuilder_Append_String =
            typeof(StringBuilder).GetMethod("Append", new Type[] { String });
        public static readonly MethodInfo TW_WriteLine =
            typeof(TextWriter).GetMethod("WriteLine", new Type[] { String });
        public static readonly MethodInfo Console_get_Error =
            typeof(Console).GetMethod("get_Error");
        public static readonly MethodInfo Object_ToString =
            typeof(object).GetMethod("ToString", new Type[0]);

        public static readonly FieldInfo IP6_mo =
            IP6.GetField("mo");
        public static readonly FieldInfo BValue_v =
            BValue.GetField("v");
        public static readonly FieldInfo SubInfo_mo =
            SubInfo.GetField("mo");
        public static readonly FieldInfo SubInfo_sig_i =
            SubInfo.GetField("sig_i");
        public static readonly FieldInfo SubInfo_sig_r =
            SubInfo.GetField("sig_r");
        public static readonly FieldInfo DynObject_slots =
            DynObject.GetField("slots");
        public static readonly FieldInfo DMO_typeObject =
            DynMetaObject.GetField("typeObject");
        public static readonly FieldInfo DMO_how =
            DynMetaObject.GetField("how");
        public static readonly FieldInfo Kernel_NumMO =
            Kernel.GetField("NumMO");
        public static readonly FieldInfo Kernel_StrMO =
            Kernel.GetField("StrMO");
        public static readonly FieldInfo Kernel_AnyMO =
            Kernel.GetField("AnyMO");
        public static readonly FieldInfo Kernel_AnyP =
            Kernel.GetField("AnyP");
        public static readonly FieldInfo Frame_rx =
            typeof(Frame).GetField("rx");
        public static readonly FieldInfo Frame_ip =
            typeof(Frame).GetField("ip");
        public static readonly FieldInfo Frame_caller =
            typeof(Frame).GetField("caller");
        public static readonly FieldInfo Frame_outer =
            typeof(Frame).GetField("outer");
        public static readonly FieldInfo Frame_resultSlot =
            typeof(Frame).GetField("resultSlot");
        public static readonly FieldInfo Frame_lexn =
            typeof(Frame).GetField("lexn");
        public static readonly FieldInfo[] Frame_lexi32 = new FieldInfo[] {
            typeof(Frame).GetField("lexi0"),
            typeof(Frame).GetField("lexi1")
        };
        public static readonly FieldInfo[] Frame_lexobj = new FieldInfo[] {
            typeof(Frame).GetField("lex0"),
            typeof(Frame).GetField("lex1"),
            typeof(Frame).GetField("lex2"),
            typeof(Frame).GetField("lex3"),
            typeof(Frame).GetField("lex4"),
            typeof(Frame).GetField("lex5"),
            typeof(Frame).GetField("lex6"),
            typeof(Frame).GetField("lex7"),
            typeof(Frame).GetField("lex8"),
            typeof(Frame).GetField("lex9")
        };

        public const int NumInt32 = 2;
        public const int NumInline = 10;
    }

    // This are expressional CLR operators.  This is lower level than the
    // CPS stuff; if HasCases is true, Returns must be void.  Thus,
    // there is no need to handle argument spills.
    abstract class ClrOp {
        public bool HasCases;
        public bool Constant; // if this returns a value, can it be reordered?
        public Type Returns;
        public abstract void CodeGen(CgContext cx);
        public virtual void ListCases(CgContext cx) { }

        public virtual ClrOp Sink() {
            throw (Returns == Tokens.Void)
                ? (Exception)new ArgumentException()
                : new NotImplementedException();
        }

        protected static void TypeCheck(Type sub, Type super) {
            if (!super.IsAssignableFrom(sub))
                throw new Exception(sub + " not subtype of " + super);
        }
        protected static void TypeCheck(Type sub, Type super, object c) {
            if (!super.IsAssignableFrom(sub))
                throw new Exception(sub + " not subtype of " + super + ":" + c);
        }
    }

    // NOT FOR GENERAL USE: only in implementing Sink for Clr*** with
    // irreducable operations
    class ClrSink : ClrOp {
        public readonly ClrOp zyg;
        public override void ListCases(CgContext cx) {
            zyg.ListCases(cx);
        }
        public override void CodeGen(CgContext cx) {
            zyg.CodeGen(cx);
            cx.il.Emit(OpCodes.Pop);
        }
        public ClrSink(ClrOp zyg) {
            if (zyg.Returns == Tokens.Void)
                throw new ArgumentException();
            this.zyg = zyg;
            Returns = Tokens.Void;
        }
    }

    class ClrMethodCall : ClrOp {
        public readonly MethodInfo Method;
        public readonly ClrOp[] Zyg;

        public override ClrOp Sink() {
            return new ClrSink(this);
        }
        public override void CodeGen(CgContext cx) {
            if (HasCases) {
                cx.il.Emit(OpCodes.Ldarg_0);
                cx.EmitInt(cx.next_case);
                cx.il.Emit(OpCodes.Stfld, Tokens.Frame_ip);
            }
            int i = 0;
            if (!Method.IsStatic) {
                ClrOp o = Zyg[i++];
                o.CodeGen(cx);
                // XXX this doesn't work quite right if the method is
                // defined on the value type itself
                if (o.Returns.IsValueType && !Method.IsStatic)
                    cx.il.Emit(OpCodes.Box, o.Returns);
            }
            // this needs to come AFTER the invocant
            if (HasCases)
                cx.il.Emit(OpCodes.Ldarg_0);

            for (; i < Zyg.Length; i++) {
                Zyg[i].CodeGen(cx);
            }
            cx.il.Emit(((Method.IsStatic || !Method.IsVirtual) ?
                        OpCodes.Call : OpCodes.Callvirt), Method);
            if (HasCases) {
                cx.il.Emit(OpCodes.Ret);
                cx.il.MarkLabel(cx.cases[cx.next_case++]);
                cx.save_line();
            }
        }

        public override void ListCases(CgContext cx) {
            // it is not legal for any of out children to have cases to list
            if (HasCases)
                cx.num_cases++;
        }

        public ClrMethodCall(bool cps, MethodInfo mi, ClrOp[] zyg) {
            Method = mi;
            Zyg = zyg;
            Returns = cps ? Tokens.Void : mi.ReturnType;
            HasCases = cps;

            List<Type> ts = new List<Type>();

            if (!mi.IsStatic)
                ts.Add(mi.DeclaringType);
            if (cps && mi.GetParameters()[0].ParameterType != Tokens.Frame)
                throw new ArgumentException("CPS method not taking a frame");
            if (cps && mi.ReturnType != Tokens.Frame)
                throw new ArgumentException("CPS method not returning a frame");

            bool skip = cps;
            foreach (ParameterInfo pi in mi.GetParameters()) {
                if (skip) { skip = false; continue; }
                ts.Add(pi.ParameterType);
            }

            if (zyg.Length != ts.Count)
                throw new Exception("argument list length mismatch for " + mi +
                        " got " + zyg.Length + " need " + ts.Count);

            for (int i = 0; i < ts.Count; i++) {
                TypeCheck(zyg[i].Returns, ts[i], mi);
            }
        }
    }

    class ClrConstructorCall : ClrOp {
        public readonly ConstructorInfo Method;
        public readonly ClrOp[] Zyg;

        public override ClrOp Sink() {
            return new ClrSink(this);
        }
        public override void CodeGen(CgContext cx) {
            foreach (ClrOp o in Zyg) {
                o.CodeGen(cx);
            }
            cx.il.Emit(OpCodes.Newobj, Method);
        }

        public ClrConstructorCall(ConstructorInfo mi, ClrOp[] zyg) {
            Method = mi;
            Zyg = zyg;
            Returns = mi.DeclaringType;
            List<Type> ts = new List<Type>();

            foreach (ParameterInfo pi in mi.GetParameters()) {
                ts.Add(pi.ParameterType);
            }

            if (zyg.Length != ts.Count)
                throw new Exception("argument list length mismatch");

            for (int i = 0; i < ts.Count; i++) {
                TypeCheck(zyg[i].Returns, ts[i]);
            }
        }
    }

    class ClrContexty : ClrOp {
        public readonly ClrOp[] zyg;
        public readonly MethodInfo inv;
        public readonly FieldInfo thing;

        // This could be avoided in some cases, but probably +$customobj;
        // shouldn't be optimized out
        public override ClrOp Sink() {
            return new ClrSink(this);
        }
        public override void CodeGen(CgContext cx) {
            zyg[0].CodeGen(cx);
            cx.make_ospill();
            cx.il.Emit(OpCodes.Stloc, cx.ospill);
            cx.il.Emit(OpCodes.Ldloc, cx.ospill);
            cx.il.Emit(OpCodes.Callvirt, Tokens.Variable_Fetch);
            cx.il.Emit(OpCodes.Ldfld, Tokens.IP6_mo);
            cx.il.Emit(OpCodes.Ldfld, thing);
            cx.il.Emit(OpCodes.Ldfld, cx.ospill);
            for (int i = 1; i < zyg.Length; i++)
                zyg[i].CodeGen(cx);
            cx.il.Emit(OpCodes.Callvirt, inv);
        }

        public ClrContexty(FieldInfo thing, MethodInfo inv, ClrOp[] zyg) {
            this.thing = thing;
            this.inv = inv;
            this.zyg = zyg;
            Returns = inv.ReturnType;
        }
    }

    class ClrOperator : ClrOp {
        public readonly OpCode op;
        public readonly ClrOp[] zyg;

        public override ClrOp Sink() {
            ClrOp[] szyg = new ClrOp[zyg.Length];
            for (int i = 0; i < szyg.Length; i++)
                szyg[i] = zyg[i].Sink();
            return new ClrSeq(szyg);
        }

        public override void CodeGen(CgContext cx) {
            foreach (ClrOp c in zyg)
                c.CodeGen(cx);
            cx.il.Emit(op);
        }

        public ClrOperator(Type ret, OpCode op, ClrOp[] zyg) {
            Returns = ret;
            this.op = op;
            this.zyg = zyg;
        }
    }

    class ClrCompare : ClrOp {
        public readonly string op;
        public readonly ClrOp[] zyg;

        public override ClrOp Sink() {
            ClrOp[] szyg = new ClrOp[zyg.Length];
            for (int i = 0; i < szyg.Length; i++)
                szyg[i] = zyg[i].Sink();
            return new ClrSeq(szyg);
        }

        public override void CodeGen(CgContext cx) {
            foreach (ClrOp c in zyg)
                c.CodeGen(cx);
            bool flt = zyg[0].Returns == Tokens.Double;
            OpCode ilop;
            bool not = false;
            if (op == "<") { ilop = OpCodes.Clt; }
            else if (op == ">") { ilop = OpCodes.Cgt; }
            else if (op == ">=") {
                ilop = flt ? OpCodes.Clt_Un : OpCodes.Clt;
                not = true;
            }
            else if (op == "<=") {
                ilop = flt ? OpCodes.Cgt_Un : OpCodes.Cgt;
                not = true;
            }
            else if (op == "==") { ilop = OpCodes.Ceq; }
            else if (op == "!=") { ilop = OpCodes.Ceq; not = true; }
            else throw new ArgumentException(op + " as polyop");
            cx.il.Emit(ilop);
            if (not) {
                cx.il.Emit(OpCodes.Ldc_I4_0);
                cx.il.Emit(OpCodes.Ceq);
            }
        }

        public ClrCompare(string op, ClrOp[] zyg) {
            Returns = Tokens.Boolean;
            this.op = op;
            this.zyg = zyg;
        }
    }

    class ClrGetField : ClrOp {
        public readonly FieldInfo f;
        public readonly ClrOp zyg;

        // Not strictly right, but Perl 6 code never sees CLR nulls, and
        // this is a major win for some cases
        public override ClrOp Sink() {
            return zyg.Sink();
        }
        public override void CodeGen(CgContext cx) {
            zyg.CodeGen(cx);
            cx.il.Emit(OpCodes.Ldfld, f);
        }

        public ClrGetField(FieldInfo f, ClrOp zyg) {
            TypeCheck(zyg.Returns, f.DeclaringType);
            Returns = f.FieldType;
            this.f = f;
            this.zyg = zyg;
        }
    }

    class ClrSetField : ClrOp {
        public readonly FieldInfo f;
        public readonly ClrOp zyg1;
        public readonly ClrOp zyg2;

        public override void CodeGen(CgContext cx) {
            zyg1.CodeGen(cx);
            zyg2.CodeGen(cx);
            cx.il.Emit(OpCodes.Stfld, f);
        }

        public ClrSetField(FieldInfo f, ClrOp zyg1, ClrOp zyg2) {
            TypeCheck(zyg1.Returns, f.DeclaringType);
            TypeCheck(zyg2.Returns, f.FieldType);
            Returns = Tokens.Void;
            this.f = f;
            this.zyg1 = zyg1;
            this.zyg2 = zyg2;
        }
    }

    class ClrGetSField : ClrOp {
        public readonly FieldInfo f;

        public override ClrOp Sink() { return ClrNoop.Instance; }
        public override void CodeGen(CgContext cx) {
            cx.il.Emit(OpCodes.Ldsfld, f);
        }

        public ClrGetSField(FieldInfo f) {
            Returns = f.FieldType;
            this.f = f;
        }
    }

    class ClrSetSField : ClrOp {
        public readonly FieldInfo f;
        public readonly ClrOp zyg;

        public override void CodeGen(CgContext cx) {
            zyg.CodeGen(cx);
            cx.il.Emit(OpCodes.Stsfld, f);
        }

        public ClrSetSField(FieldInfo f, ClrOp zyg) {
            TypeCheck(zyg.Returns, f.FieldType);
            Returns = Tokens.Void;
            this.f = f;
            this.zyg = zyg;
        }
    }

    class ClrPadGet : ClrOp {
        public readonly int up;
        public readonly int index;

        public override ClrOp Sink() { return ClrNoop.Instance; }
        public override void CodeGen(CgContext cx) {
            cx.il.Emit(OpCodes.Ldarg_0);
            for (int i = 0; i < up; i++)
                cx.il.Emit(OpCodes.Ldfld, Tokens.Frame_outer);
            cx.EmitGetlex(index + Tokens.NumInt32, Tokens.Variable);
        }

        public ClrPadGet(int up, int index) {
            Returns = Tokens.Variable;
            this.up = up;
            this.index = index;
        }
    }

    class ClrPadSet : ClrOp {
        public readonly int up;
        public readonly int index;
        public readonly ClrOp zyg;

        public override void CodeGen(CgContext cx) {
            cx.il.Emit(OpCodes.Ldarg_0);
            for (int i = 0; i < up; i++)
                cx.il.Emit(OpCodes.Ldfld, Tokens.Frame_outer);
            cx.EmitPreSetlex(index + Tokens.NumInt32);
            zyg.CodeGen(cx);
            cx.EmitSetlex(index + Tokens.NumInt32, Tokens.Variable);
        }

        public ClrPadSet(int up, int index, ClrOp zyg) {
            Returns = Tokens.Void;
            this.zyg = zyg;
            this.up = up;
            this.index = index;
        }
    }

    class ClrProtoSet : ClrOp {
        public readonly int ix;
        public readonly ClrOp zyg1;
        public readonly ClrOp zyg2;

        public override void CodeGen(CgContext cx) {
            zyg1.CodeGen(cx);
            cx.EmitPreSetlex(ix + Tokens.NumInt32);
            zyg2.CodeGen(cx);
            cx.EmitSetlex(ix + Tokens.NumInt32, Tokens.Variable);
        }

        public ClrProtoSet(int ix, ClrOp zyg1, ClrOp zyg2) {
            TypeCheck(zyg1.Returns, Tokens.Frame);
            Returns = Tokens.Void;
            this.ix = ix;
            this.zyg1 = zyg1;
            this.zyg2 = zyg2;
        }
    }

    class ClrNoop : ClrOp {
        private ClrNoop() {
            Returns = Tokens.Void;
            HasCases = false;
        }
        public override void CodeGen(CgContext cx) { }
        public static ClrNoop Instance = new ClrNoop();
    }

    class ClrEhSpan : ClrOp {
        public readonly int kls;
        public readonly string tag;
        public readonly int lid;
        public readonly string ls;
        public readonly string le;
        public readonly string lg;

        public ClrEhSpan(int kls, string tag, int lid, string ls, string le,
                string lg) {
            Returns = Tokens.Void;
            HasCases = false;
            this.kls = kls; this.tag = tag; this.ls = ls; this.le = le;
            this.lg = lg; this.lid = lid;
        }
        public override void CodeGen(CgContext cx) {
            int lidn = -1;
            if (tag != "") {
                lidn = cx.ehlabelBuffer.Count;
                cx.ehlabelBuffer.Add(tag);
            }
            cx.ehspanBuffer.Add(cx.named_cases[ls]);
            cx.ehspanBuffer.Add(cx.named_cases[le]);
            cx.ehspanBuffer.Add(kls);
            cx.ehspanBuffer.Add(cx.named_cases[lg]);
            cx.ehspanBuffer.Add(lidn);
        }
    }

    class ClrPushLine : ClrOp {
        public readonly int line;

        public ClrPushLine(int line) {
            this.line = line;
            Returns = Tokens.Void;
            HasCases = false;
        }
        public override void CodeGen(CgContext cx) {
            cx.lineStack.Add(line);
        }
    }

    class ClrPopLine : ClrOp {
        public ClrPopLine() {
            Returns = Tokens.Void;
            HasCases = false;
        }
        public override void CodeGen(CgContext cx) {
            cx.lineStack.RemoveAt(cx.lineStack.Count - 1);
        }
    }

    class ClrSync : ClrOp {
        private ClrSync() {
            Returns = Tokens.Void;
            HasCases = true;
        }
        public override void ListCases(CgContext cx) { cx.num_cases++; }
        public override void CodeGen(CgContext cx) {
            cx.il.Emit(OpCodes.Ldarg_0);
            cx.EmitInt(cx.next_case);
            cx.il.Emit(OpCodes.Stfld, Tokens.Frame_ip);
            cx.il.MarkLabel(cx.cases[cx.next_case++]);
            cx.save_line();
        }
        public static ClrSync Instance = new ClrSync();
    }

    // only used in ClrOperator.Sink, and assumes it in the HasCases=false
    class ClrSeq : ClrOp {
        readonly ClrOp[] zyg;
        public ClrSeq(ClrOp[] zyg) {
            Returns = Tokens.Void;
            HasCases = false;
            this.zyg = zyg;
        }
        public override void CodeGen(CgContext cx) {
            foreach(ClrOp z in zyg)
                z.CodeGen(cx);
        }
    }

    class ClrSubyCall : ClrOp {
        public readonly string mname;
        public readonly string sig;
        public readonly ClrOp[] zyg;

        // generates the argument list, from the all-but-1st element of zyg
        void GenArgList(CgContext cx) {
            bool general = false;
            for (int i = 1; i < zyg.Length; i++)
                if (sig[i - 1] != '\0')
                    general = true;
            if (!general) {
                cx.EmitInt(zyg.Length - 1);
                cx.il.Emit(OpCodes.Newarr, Tokens.Variable);
                for (int i = 1; i < zyg.Length; i++) {
                    cx.il.Emit(OpCodes.Dup);
                    cx.EmitInt(i - 1);
                    zyg[i].CodeGen(cx);
                    cx.il.Emit(OpCodes.Stelem_Ref);
                }
                cx.il.Emit(OpCodes.Ldnull);
            } else {
                if (cx.pspill == null) cx.pspill = cx.il.DeclareLocal(typeof(List<Variable>));
                if (cx.nspill == null) cx.nspill = cx.il.DeclareLocal(Tokens.VarHash);
                cx.il.Emit(OpCodes.Newobj, typeof(List<Variable>).GetConstructor(new Type[0]));
                cx.il.Emit(OpCodes.Stloc, cx.pspill);
                cx.il.Emit(OpCodes.Newobj, Tokens.VarHash.GetConstructor(new Type[0]));
                cx.il.Emit(OpCodes.Stloc, cx.nspill);

                int csr = 0;
                int ix  = 1;

                while (csr != sig.Length) {
                    int len = (int)sig[csr];
                    string tok = sig.Substring(csr+1, len);
                    csr += (len + 1);

                    if (tok == "") {
                        cx.il.Emit(OpCodes.Ldloc, cx.pspill);
                        zyg[ix++].CodeGen(cx);
                        cx.il.Emit(OpCodes.Call, typeof(List<Variable>).GetMethod("Add"));
                    } else if (tok == "flatcap") {
                        cx.il.Emit(OpCodes.Ldloc, cx.pspill);
                        cx.il.Emit(OpCodes.Ldloc, cx.nspill);
                        zyg[ix++].CodeGen(cx);
                        cx.il.Emit(OpCodes.Call, Tokens.Kernel.GetMethod("AddCap"));
                    } else if (tok[0] == ':') {
                        cx.il.Emit(OpCodes.Ldloc, cx.nspill);
                        cx.il.Emit(OpCodes.Ldstr, tok.Substring(1));
                        zyg[ix++].CodeGen(cx);
                        cx.il.Emit(OpCodes.Call, Tokens.VarHash_set_Item);
                    } else {
                        throw new ArgumentException(tok);
                    }
                }

                cx.il.Emit(OpCodes.Ldloc, cx.pspill);
                cx.il.Emit(OpCodes.Call, typeof(List<Variable>).GetMethod("ToArray"));
                cx.il.Emit(OpCodes.Ldloc, cx.nspill);
            }
        }

        public override void CodeGen(CgContext cx) {
            cx.il.Emit(OpCodes.Ldarg_0);
            cx.EmitInt(cx.next_case);
            cx.il.Emit(OpCodes.Stfld, Tokens.Frame_ip);

            zyg[0].CodeGen(cx);
            cx.il.Emit(OpCodes.Ldarg_0);
            if (mname != null)
                cx.il.Emit(OpCodes.Ldstr, mname);

            GenArgList(cx);

            cx.il.Emit(OpCodes.Callvirt, (mname != null) ?
                    Tokens.IP6_InvokeMethod : Tokens.IP6_Invoke);
            cx.il.Emit(OpCodes.Ret);
            cx.il.MarkLabel(cx.cases[cx.next_case++]);
            cx.save_line();
        }

        public override void ListCases(CgContext cx) {
            cx.num_cases++;
        }

        public ClrSubyCall(string mname, string sig, ClrOp[] zyg) {
            if (mname != null) sig = "\0" + sig;
            TypeCheck(zyg[0].Returns, Tokens.IP6);
            int i = 1;
            int j = 0;
            while (j < sig.Length) {
                string s = sig.Substring(j+1, sig[j]);
                j += (1 + s.Length);
                TypeCheck(zyg[i++].Returns, (s == "flatcap") ? Tokens.IP6 : Tokens.Variable);
            }
            this.mname = mname;
            this.sig = sig;
            this.zyg = zyg;
            this.Returns = Tokens.Void;
            this.HasCases = true;
        }
    }

    class ClrPushLet : ClrOp {
        string Name;
        // Initial must not have a net let-stack effect (how to enforce?)
        ClrOp Initial;
        public ClrPushLet(string name, ClrOp initial) {
            Initial = initial;
            Name = name;
            Returns = Tokens.Void;
        }
        public override void CodeGen(CgContext cx) {
            // indexes 0-1 can only be used by ints
            int ix = (Initial.Returns == typeof(int)) ? 0 : Tokens.NumInt32;
            while (ix < cx.let_types.Length && cx.let_types[ix] != null)
                ix++;

            cx.il.Emit(OpCodes.Ldarg_0);
            cx.EmitPreSetlex(ix);

            // Initial must not have a net effect on cx.let_types
            Initial.CodeGen(cx);

            // let_types.Length tracks the highest index used.
            if (ix >= cx.let_types.Length) {
                Array.Resize(ref cx.let_types, ix+1);
                Array.Resize(ref cx.let_names, ix+1);
            }

            cx.let_types[ix] = Initial.Returns;
            cx.let_names[ix] = Name;

            cx.EmitSetlex(ix, Initial.Returns);
        }
    }

    class ClrPokeLet : ClrOp {
        string Name;
        ClrOp Value;
        public ClrPokeLet(string name, ClrOp value) {
            Value = value;
            Name = name;
            Returns = Tokens.Void;
        }
        public override void CodeGen(CgContext cx) {
            int ix = cx.let_names.Length - 1;
            while (ix >= 0 && cx.let_names[ix] != Name)
                ix--;

            if (ix == cx.let_names.Length)
                throw new Exception("let " + Name + " not found");

            cx.il.Emit(OpCodes.Ldarg_0);
            cx.EmitPreSetlex(ix);

            // Initial must not have a net effect on cx.let_types
            Value.CodeGen(cx);

            cx.EmitSetlex(ix, Value.Returns);
        }
    }

    class ClrPeekLet : ClrOp {
        string Name;
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public ClrPeekLet(string name, Type letType) {
            Name = name;
            Returns = letType;
        }
        public override void CodeGen(CgContext cx) {
            int ix = cx.let_names.Length - 1;
            while (ix >= 0 && cx.let_names[ix] != Name)
                ix--;

            if (ix == cx.let_names.Length)
                throw new Exception("let " + Name + " not found");

            cx.il.Emit(OpCodes.Ldarg_0);
            cx.EmitGetlex(ix, Returns);
        }
    }

    class ClrSetResult : ClrOp {
        ClrOp zyg;
        public ClrSetResult(ClrOp zyg) {
            Returns = Tokens.Void;
            this.zyg = zyg;
        }
        public override void CodeGen(CgContext cx) {
            cx.il.Emit(OpCodes.Ldarg_0);
            zyg.CodeGen(cx);
            if (zyg.Returns.IsValueType)
                cx.il.Emit(OpCodes.Box, zyg.Returns);
            cx.il.Emit(OpCodes.Stfld, Tokens.Frame_resultSlot);
        }
    }

    class ClrResult : ClrOp {
        public ClrResult(Type letType) {
            Returns = letType;
        }
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public override void CodeGen(CgContext cx) {
            if (Returns == Tokens.Void)
                return;
            cx.il.Emit(OpCodes.Ldarg_0);
            cx.il.Emit(OpCodes.Ldfld, Tokens.Frame_resultSlot);
            if (CLRBackend.Verifiable || Returns.IsValueType)
                cx.il.Emit(OpCodes.Unbox_Any, Returns);
        }
    }

    class ClrDropLet : ClrOp {
        public string Name;
        public ClrOp Inner;
        public override ClrOp Sink() {
            return new ClrDropLet(Name, Inner.Sink());
        }
        public ClrDropLet(string name, ClrOp inner) {
            Name = name;
            Inner = inner;
            Returns = inner.Returns;
            HasCases = inner.HasCases;
        }
        public override void ListCases(CgContext cx) {
            Inner.ListCases(cx);
        }
        public override void CodeGen(CgContext cx) {
            Inner.CodeGen(cx);

            int ix = cx.let_names.Length - 1;
            while (ix >= 0 && cx.let_names[ix] != Name)
                ix--;

            if (ix == cx.let_names.Length)
                throw new Exception("let " + Name + " not found");

            cx.let_names[ix] = null;
            cx.let_types[ix] = null;
            // XXX We probably should null reference-valued lets here
        }
    }

    // TODO Investigate DLR-style labels with arguments
    class ClrLabel : ClrOp {
        string name;
        bool case_too;
        public ClrLabel(string name, bool case_too) {
            this.name = name;
            this.case_too = case_too;
            Returns = Tokens.Void;
            HasCases = true;
        }
        public override void ListCases(CgContext cx) {
            cx.named_labels[name] = cx.il.DefineLabel();
            if (case_too)
                cx.named_cases[name] = cx.num_cases++;
        }
        public override void CodeGen(CgContext cx) {
            cx.il.MarkLabel(cx.named_labels[name]);
            if (case_too) {
                cx.il.MarkLabel(cx.cases[cx.named_cases[name]]);
                cx.next_case++;
                cx.save_line();
            }
        }
    }

    class ClrGoto : ClrOp {
        string name;
        bool iffalse;
        ClrOp inner;
        public ClrGoto(string name, bool iffalse, ClrOp inner) {
            this.name = name;
            this.iffalse = iffalse;
            this.inner = inner;
            Returns = Tokens.Void;
        }
        public override void CodeGen(CgContext cx) {
            // TODO: peephole optimize ceq/brtrue and similar forms
            Label l = cx.named_labels[name];
            if (inner != null) {
                inner.CodeGen(cx);
                cx.il.Emit(iffalse ? OpCodes.Brfalse : OpCodes.Brtrue, l);
            } else {
                cx.il.Emit(OpCodes.Br, l);
            }
        }
    }

    class ClrCpsReturn : ClrOp {
        ClrOp child;
        public ClrCpsReturn(ClrOp child) {
            this.child = child;
            this.Returns = Tokens.Void;
        }
        public override void CodeGen(CgContext cx) {
            if (child != null) {
                cx.il.Emit(OpCodes.Ldarg_0);
                cx.il.Emit(OpCodes.Ldfld, Tokens.Frame_caller);
                child.CodeGen(cx);
                cx.il.Emit(OpCodes.Stfld, Tokens.Frame_resultSlot);
            }
            cx.il.Emit(OpCodes.Ldarg_0);
            cx.il.Emit(OpCodes.Ldfld, Tokens.Frame_caller);
            cx.il.Emit(OpCodes.Ret);
        }
    }

    class ClrStringLiteral : ClrOp {
        string data;
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public ClrStringLiteral(string data) {
            this.data = data;
            Returns = Tokens.String;
            Constant = true;
        }
        public override void CodeGen(CgContext cx) {
            cx.il.Emit(OpCodes.Ldstr, data);
        }
    }

    class ClrCpsFrame : ClrOp {
        public override ClrOp Sink() { return ClrNoop.Instance; }
        private ClrCpsFrame() {
            Returns = Tokens.Frame;
            Constant = true;
        }
        public override void CodeGen(CgContext cx) {
            cx.il.Emit(OpCodes.Ldarg_0);
        }
        public static ClrCpsFrame Instance = new ClrCpsFrame();
    }

    class ClrNullLiteral : ClrOp {
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public ClrNullLiteral(Type ty) {
            Returns = ty;
            Constant = true;
        }
        public override void CodeGen(CgContext cx) {
            cx.il.Emit(OpCodes.Ldnull);
        }
    }

    class ClrUnboxAny : ClrOp {
        public readonly ClrOp zyg;
        public override ClrOp Sink() { return zyg.Sink(); }
        public ClrUnboxAny(Type ty, ClrOp zyg) {
            Returns = ty;
            this.zyg = zyg;
        }
        public override void CodeGen(CgContext cx) {
            zyg.CodeGen(cx);
            cx.il.Emit(OpCodes.Unbox_Any, Returns);
        }
    }

    class ClrDBDLiteral : ClrOp {
        readonly MethodInfo body;
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public ClrDBDLiteral(MethodInfo body) {
            this.body = body;
            Returns = Tokens.DynBlockDelegate;
            Constant = true;
        }
        public override void CodeGen(CgContext cx) {
            cx.il.Emit(OpCodes.Ldnull);
            cx.il.Emit(OpCodes.Ldftn, body);
            cx.il.Emit(OpCodes.Newobj, Tokens.DynBlockDelegate_ctor);
        }
    }

    // Because the CLR has no evaluation stack types narrower than int32, this
    // node does duty both for int and bool.  When sized types are added, it
    // will also handle int8, int16, and unsigned versions thereof.
    class ClrIntLiteral : ClrOp {
        int data;
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public ClrIntLiteral(Type ty, int data) {
            this.data = data;
            Returns = ty;
            Constant = true;
        }
        public override void CodeGen(CgContext cx) {
            cx.EmitInt(data);
        }
    }

    class ClrLabelLiteral : ClrOp {
        CgContext tcx;
        string name;
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public ClrLabelLiteral(CgContext tcx, string name) {
            this.name = name;
            this.tcx = tcx;
            Returns = Tokens.Int32;
            Constant = true;
        }
        public override void CodeGen(CgContext cx) {
            cx.EmitInt(tcx.named_cases[name]);
        }
    }

    class ClrLabelArray : ClrOp {
        CgContext tcx;
        string[] names;
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public ClrLabelArray(CgContext tcx, string[] names) {
            this.names = names;
            this.tcx = tcx;
            Returns = typeof(int[]);
            Constant = true;
        }
        public override void CodeGen(CgContext cx) {
            int[] vec = new int[names.Length];
            for (int i = 0; i < vec.Length; i++)
                vec[i] = tcx.named_cases[names[i]];
            cx.EmitIntArray(vec);
        }
    }

    class ClrNumLiteral : ClrOp {
        double data;
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public ClrNumLiteral(double data) {
            this.data = data;
            Returns = Tokens.Double;
            Constant = true;
        }
        public override void CodeGen(CgContext cx) {
            cx.il.Emit(OpCodes.Ldc_R8, data);
        }
    }

    class ClrNewArray : ClrOp {
        readonly ClrOp[] zyg;
        public ClrNewArray(Type r, ClrOp[] zyg) {
            Returns = r.MakeArrayType();
            if (r.IsValueType)
                throw new ArgumentException();
            foreach(ClrOp c in zyg)
                TypeCheck(c.Returns, r);
            HasCases = false;
            this.zyg = zyg;
        }
        public override ClrOp Sink() {
            ClrOp[] szyg = new ClrOp[zyg.Length];
            for (int i = 0; i < szyg.Length; i++)
                szyg[i] = zyg[i].Sink();
            return new ClrSeq(szyg);
        }
        public override void CodeGen(CgContext cx) {
            cx.EmitInt(zyg.Length);
            cx.il.Emit(OpCodes.Newarr, Returns.GetElementType());
            for (int i = 0; i < zyg.Length; i++) {
                cx.il.Emit(OpCodes.Dup);
                cx.EmitInt(i);
                zyg[i].CodeGen(cx);
                cx.il.Emit(OpCodes.Stelem_Ref);
            }
        }
    }

    class ClrNewIntArray : ClrOp {
        readonly int[] vec;
        public ClrNewIntArray(int[] vec) {
            if (vec.Length >= 0xfc000)
                throw new ArgumentException();
            Returns = typeof(int[]);
            this.vec = vec;
            Constant = true;
        }
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public override void CodeGen(CgContext cx) {
            cx.EmitIntArray(vec);
        }
    }


    // CpsOps are rather higher level, and can support operations that
    // both return to the trampoline and return a value.
    class CpsOp {
        // each statement MUST return void
        public ClrOp[] stmts;
        // the head MUST NOT have cases
        public ClrOp head;

        public CpsOp(ClrOp head) : this(new ClrOp[0], head) { }
        public CpsOp(ClrOp[] stmts, ClrOp head) {
            if (head.HasCases)
                throw new Exception("head must not have cases");
            foreach (ClrOp s in stmts)
                if (s.Returns != Tokens.Void)
                    throw new Exception("stmts must return void");
            this.head = head;
            this.stmts = stmts;
        }

        public static CpsOp Cps(ClrOp nothead, Type ty) {
            return new CpsOp(new ClrOp[] { nothead }, new ClrResult(ty));
        }

        // XXX only needs to be unique per sub
        [ThreadStatic] private static int nextunique = 0;
        // this particular use of a delegate feels wrong
        private static CpsOp Primitive(CpsOp[] zyg, Func<ClrOp[],CpsOp> raw) {
            List<ClrOp> stmts = new List<ClrOp>();
            List<ClrOp> args = new List<ClrOp>();
            List<string> pop = new List<string>();

            for (int i = 0; i < zyg.Length; i++) {
                foreach (ClrOp s in zyg[i].stmts)
                    stmts.Add(s);

                bool more_stmts = false;
                for (int j = i + 1; j < zyg.Length; j++)
                    if (zyg[j].stmts.Length != 0)
                        more_stmts = true;

                if (!more_stmts || zyg[i].head.Constant) {
                    args.Add(zyg[i].head);
                } else {
                    string ln = "!spill" + (nextunique++);
                    args.Add(new ClrPeekLet(ln, zyg[i].head.Returns));
                    stmts.Add(new ClrPushLet(ln, zyg[i].head));
                    pop.Add(ln);
                }
            }

            CpsOp rval = raw(args.ToArray());
            foreach (ClrOp c in rval.stmts)
                stmts.Add(c);
            ClrOp head = rval.head;
            for (int i = pop.Count - 1; i >= 0; i--)
                head = new ClrDropLet(pop[i], head);
            return new CpsOp(stmts.ToArray(), head);
        }

        public static CpsOp Sequence(CpsOp[] terms) {
            if (terms.Length == 0) return new CpsOp(ClrNoop.Instance);

            List<ClrOp> stmts = new List<ClrOp>();
            for (int i = 0; i < terms.Length - 1; i++) {
                if (terms[i].head.Returns != Tokens.Void)
                    throw new Exception("Non-void expression used in nonfinal sequence position" + terms[i].head.Returns);
                foreach (ClrOp s in terms[i].stmts)
                    stmts.Add(s);
                stmts.Add(terms[i].head);
            }

            foreach (ClrOp s in terms[terms.Length - 1].stmts)
                stmts.Add(s);
            return new CpsOp(stmts.ToArray(), terms[terms.Length - 1].head);
        }

        static ClrOp StripResult(ClrOp it) {
            if (it is ClrResult)
                return ClrNoop.Instance;
            if (it is ClrDropLet) {
                ClrDropLet cit = (ClrDropLet) it;
                string n = cit.Name;
                it = StripResult(cit.Inner);
                if (it != null)
                    return new ClrDropLet(n, it);
            }
            return null;
        }

        static void Resultify(ref ClrOp[] stmts, ref ClrOp head) {
            if (head.Returns == Tokens.Void) return;
            ClrOp head_s = StripResult(head);
            if (head_s == null) {
                head_s = ClrNoop.Instance;
                Array.Resize(ref stmts, stmts.Length + 1);
                stmts[stmts.Length - 1] = new ClrSetResult(head);
            }
            head = head_s;
        }

        public static CpsOp Span(string l1, string l2, bool sync, CpsOp body) {
            ClrOp body_h = body.head;
            ClrOp[] body_s = body.stmts;
            Resultify(ref body_s, ref body_h);
            List<ClrOp> stmts = new List<ClrOp>();

            stmts.Add(new ClrLabel(l1, true));
            if (sync) stmts.Add(ClrSync.Instance);
            foreach (ClrOp c in body_s) stmts.Add(c);
            stmts.Add(body_h);
            if (sync) stmts.Add(ClrSync.Instance);
            stmts.Add(new ClrLabel(l2, true));

            return new CpsOp(stmts.ToArray(), new ClrResult(body.head.Returns));
        }

        public static CpsOp Ternary(CpsOp cond, CpsOp iftrue, CpsOp iffalse) {
            ClrOp iftrue_h = iftrue.head;
            ClrOp iffalse_h = iffalse.head;
            ClrOp[] iftrue_s = iftrue.stmts;
            ClrOp[] iffalse_s = iffalse.stmts;

            Resultify(ref iftrue_s, ref iftrue_h);
            Resultify(ref iffalse_s, ref iffalse_h);

            string l1 = "!else" + (nextunique++);
            string l2 = "!endif" + (nextunique++);

            List<ClrOp> stmts = new List<ClrOp>();
            foreach (ClrOp c in cond.stmts)
                stmts.Add(c);
            stmts.Add(new ClrGoto(l1, true, cond.head));
            foreach (ClrOp c in iftrue_s)
                stmts.Add(c);
            stmts.Add(iftrue_h);
            stmts.Add(new ClrGoto(l2, false, null));
            stmts.Add(new ClrLabel(l1, false));
            foreach (ClrOp c in iffalse_s)
                stmts.Add(c);
            stmts.Add(iffalse_h);
            stmts.Add(new ClrLabel(l2, false));

            Type ty = iffalse.head.Returns;
            return new CpsOp(stmts.ToArray(),
                    (ty == Tokens.Void) ? (ClrOp)ClrNoop.Instance :
                    new ClrResult(ty));
        }

        // this is simplified a bit since body is always void
        public static CpsOp While(bool until, bool once, CpsOp cond, CpsOp body) {
            string l1 = "!again" + (nextunique++);
            string l2 = "!check" + (nextunique++);

            List<ClrOp> stmts = new List<ClrOp>();

            if (!once)
                stmts.Add(new ClrGoto(l2, false, null));
            stmts.Add(new ClrLabel(l1, false));
            foreach(ClrOp c in body.stmts)
                stmts.Add(c);
            stmts.Add(body.head);
            if (!once)
                stmts.Add(new ClrLabel(l2, false));
            foreach(ClrOp c in cond.stmts)
                stmts.Add(c);
            stmts.Add(new ClrGoto(l1, until, cond.head));

            return new CpsOp(stmts.ToArray(), ClrNoop.Instance);
        }

        public static CpsOp MethodCall(Type cps, MethodInfo tk, CpsOp[] zyg) {
            return Primitive(zyg, delegate (ClrOp[] heads) {
                return (cps != null) ?
                    Cps(new ClrMethodCall(true, tk, heads), cps) :
                    new CpsOp(new ClrMethodCall(false, tk, heads));
            });
        }

        public static CpsOp ConstructorCall(ConstructorInfo tk, CpsOp[] zyg) {
            return Primitive(zyg, delegate (ClrOp[] heads) {
                return new CpsOp(new ClrConstructorCall(tk, heads));
            });
        }

        public static CpsOp CpsReturn(CpsOp[] zyg) {
            return Primitive(zyg, delegate (ClrOp[] heads) {
                return new CpsOp(new ClrCpsReturn(heads.Length > 0 ? heads[0] : null));
            });
        }

        public static CpsOp EhSpan(int kls, string tag, int lid,
                string ls, string le, string lg) {
            return new CpsOp(new ClrEhSpan(kls, tag, lid, ls, le, lg));
        }

        public static CpsOp LabelId(CgContext tcx, string label) {
            return new CpsOp(new ClrLabelLiteral(tcx, label));
        }

        public static CpsOp LabelTable(CgContext tcx, string[] labels) {
            return new CpsOp(new ClrLabelArray(tcx, labels));
        }

        public static CpsOp Goto(string label, bool iffalse, CpsOp[] zyg) {
            return Primitive(zyg, delegate (ClrOp[] heads) {
                return new CpsOp(new ClrGoto(label, iffalse,
                    heads.Length > 0 ? heads[0] : null));
            });
        }

        public static CpsOp StringLiteral(string s) {
            return new CpsOp(new ClrStringLiteral(s));
        }

        public static CpsOp DoubleLiteral(double d) {
            return new CpsOp(new ClrNumLiteral(d));
        }

        public static CpsOp CharLiteral(char x) {
            return new CpsOp(new ClrIntLiteral(typeof(char), x));
        }

        public static CpsOp IntLiteral(int x) {
            return new CpsOp(new ClrIntLiteral(Tokens.Int32, x));
        }

        public static CpsOp BoolLiteral(bool x) {
            return new CpsOp(new ClrIntLiteral(Tokens.Boolean, x ? 1 : 0));
        }

        public static CpsOp DBDLiteral(MethodInfo x) {
            return new CpsOp(new ClrDBDLiteral(x));
        }

        public static CpsOp Label(string name, bool case_too) {
            return CpsOp.Cps(new ClrLabel(name, case_too), Tokens.Void);
        }

        // A previous niecza backend had this stuff tie into the
        // lifter, such that var->obj predictably happened as the
        // beginning.  But TimToady says that's not needed.
        public static CpsOp Contexty(FieldInfo thing, MethodInfo inv,
                CpsOp[] zyg) {
            return Primitive(zyg, delegate(ClrOp[] heads) {
                return new CpsOp(new ClrContexty(thing, inv, heads));
            });
        }

        public static CpsOp Operator(Type rt, OpCode op, CpsOp[] zyg) {
            return Primitive(zyg, delegate(ClrOp[] heads) {
                return new CpsOp(new ClrOperator(rt, op, heads));
            });
        }

        // this is a stupid interface.
        public static CpsOp PolyOp(string txt, CpsOp a, CpsOp b) {
            return Primitive(new CpsOp[] { a, b }, delegate(ClrOp[] heads) {
                if (heads[0].Returns != heads[1].Returns)
                    throw new ArgumentException("Arguments to " + txt + " must have same type");
                if (heads[0].Returns == Tokens.Int32 ||
                        heads[0].Returns == Tokens.Double) {
                } else if (!heads[0].Returns.IsValueType &&
                    (txt == "==" || txt == "!=")) {
                } else
                    throw new NotImplementedException();
                OpCode op;
                if (txt == "+") { op = OpCodes.Add; }
                else if (txt == "-") { op = OpCodes.Sub; }
                else if (txt == "*") { op = OpCodes.Mul; }
                else if (txt == "/") { op = OpCodes.Div; }
                else return new CpsOp(new ClrCompare(txt, heads));

                return new CpsOp(new ClrOperator(heads[0].Returns, op, heads));
            });
        }

        public static CpsOp PokeLet(string name, CpsOp[] zyg) {
            return Primitive(zyg, delegate(ClrOp[] heads) {
                return new CpsOp(new ClrPokeLet(name, heads[0]));
            });
        }

        public static CpsOp PeekLet(string name, Type rt) {
            return new CpsOp(new ClrPeekLet(name, rt));
        }

        public static CpsOp Let(string name, CpsOp head, CpsOp tail) {
            List<ClrOp> stmts = new List<ClrOp>();
            foreach (ClrOp c in head.stmts)
                stmts.Add(c);
            stmts.Add(new ClrPushLet(name, head.head));
            foreach (ClrOp c in tail.stmts)
                stmts.Add(c);
            return new CpsOp(stmts.ToArray(), new ClrDropLet(name, tail.head));
        }

        public static CpsOp Annotate(int line, CpsOp body) {
            if (body.stmts.Length == 0) return body;
            List<ClrOp> stmts = new List<ClrOp>();
            stmts.Add(new ClrPushLine(line));
            foreach (ClrOp c in body.stmts) stmts.Add(c);
            stmts.Add(new ClrPopLine());
            return new CpsOp(stmts.ToArray(), body.head);
        }

        public static CpsOp LexAccess(Lexical l, int up, CpsOp[] zyg) {
            return Primitive(zyg, delegate(ClrOp[] heads) {
                return new CpsOp((heads.Length >= 1) ? l.SetCode(up, heads[0]) :
                    l.GetCode(up));
            });
        }

        public static CpsOp SubyCall(string mname, string sig, CpsOp[] zyg) {
            return Primitive(zyg, delegate(ClrOp[] heads) {
                return CpsOp.Cps(new ClrSubyCall(mname, sig, heads), Tokens.Variable);
            });
        }

        public static CpsOp GetField(FieldInfo fi, CpsOp zyg) {
            return Primitive(new CpsOp[1] { zyg }, delegate(ClrOp[] heads) {
                return new CpsOp(new ClrGetField(fi, heads[0]));
            });
        }

        public static CpsOp GetSField(FieldInfo fi) {
            return new CpsOp(new ClrGetSField(fi));
        }

        public static CpsOp SetField(FieldInfo fi, CpsOp za, CpsOp zb) {
            return Primitive(new CpsOp[2] { za, zb }, delegate(ClrOp[] heads) {
                return new CpsOp(new ClrSetField(fi, heads[0], heads[1]));
            });
        }

        public static CpsOp SetSField(FieldInfo fi, CpsOp zyg) {
            return Primitive(new CpsOp[1] { zyg }, delegate(ClrOp[] heads) {
                return new CpsOp(new ClrSetSField(fi, heads[0]));
            });
        }

        public static CpsOp SetProtoPad(int ix, CpsOp za, CpsOp zb) {
            return Primitive(new CpsOp[2] { za, zb }, delegate(ClrOp[] heads) {
                return new CpsOp(new ClrProtoSet(ix, heads[0], heads[1]));
            });
        }

        public static CpsOp Sink(CpsOp zyg) {
            return new CpsOp(zyg.stmts, zyg.head.Sink());
        }

        public static CpsOp Null(Type ty) {
            return new CpsOp(new ClrNullLiteral(ty));
        }

        public static CpsOp UnboxAny(Type ty, CpsOp inside) {
            return Primitive(new CpsOp[] { inside }, delegate (ClrOp[] h) {
                return new CpsOp(new ClrUnboxAny(ty, h[0]));
            });
        }

        public static CpsOp CallFrame() {
            return new CpsOp(ClrCpsFrame.Instance);
        }

        // only use this for reference types
        public static CpsOp NewArray(Type ty, CpsOp[] zyg) {
            return Primitive(zyg, delegate (ClrOp[] h) {
                return new CpsOp(new ClrNewArray(ty, h));
            });
        }

        public static CpsOp NewIntArray(int[] vec) {
            return new CpsOp(new ClrNewIntArray(vec));
        }

        public static CpsOp StringArray(bool omit, string[] vec) {
            if (vec.Length == 0 && omit) {
                return Null(typeof(string[]));
            } else {
                CpsOp[] tmp = new CpsOp[vec.Length];
                for (int i = 0; i < vec.Length; i++)
                    tmp[i] = CpsOp.StringLiteral(vec[i]);
                return NewArray(Tokens.String, tmp);
            }
        }
    }

    class CpsBuilder {
        public readonly TypeBuilder tb;
        public readonly MethodBuilder mb;
        public readonly CgContext cx;
        public readonly CLRBackend module;

        public CpsBuilder(CLRBackend module, string clrname, bool pub) {
            this.module = module;
            this.tb = module.tb;
            mb = tb.DefineMethod(clrname, MethodAttributes.Static |
                    (pub ? MethodAttributes.Public : 0),
                    typeof(Frame), new Type[] { typeof(Frame) });
            cx = new CgContext();
            cx.tb = tb;
        }

        public void ReserveLex(int ct) {
            Array.Resize(ref cx.let_types, ct + Tokens.NumInt32);
            Array.Resize(ref cx.let_names, ct + Tokens.NumInt32);
            for (int i = 0; i < ct; i++) {
                cx.let_types[i+ Tokens.NumInt32] = Tokens.Variable;
            }
        }

        public int Spills() {
            int ix = cx.let_types.Length - Tokens.NumInt32 - Tokens.NumInline;
            return (ix > 0) ? ix : 0;
        }

        public void Build(CpsOp body) {
            // ListCases may want to define labels, so this needs to come
            // early
            cx.il = mb.GetILGenerator();
            cx.num_cases = 1;

            foreach (ClrOp s in body.stmts)
                s.ListCases(cx);
            body.head.ListCases(cx);

            cx.cases = new Label[cx.num_cases];
            for (int i = 0; i < cx.num_cases; i++)
                cx.cases[i] = cx.il.DefineLabel();

            cx.il.Emit(OpCodes.Ldarg_0);
            cx.il.Emit(OpCodes.Ldfld, Tokens.Frame_ip);
            cx.il.Emit(OpCodes.Switch, cx.cases);

            cx.il.Emit(OpCodes.Ldarg_0);
            cx.il.Emit(OpCodes.Ldstr, "Invalid IP");
            cx.il.Emit(OpCodes.Call, Tokens.Kernel_Die);
            cx.il.Emit(OpCodes.Ret);

            cx.il.MarkLabel(cx.cases[cx.next_case++]);
            cx.save_line();
            foreach (ClrOp s in body.stmts)
                s.CodeGen(cx);
            body.head.CodeGen(cx);
        }
    }

    class NamProcessor {
        StaticSub sub;
        public readonly CpsBuilder cpb;
        Dictionary<string, Type> let_types = new Dictionary<string, Type>();

        public NamProcessor(CpsBuilder cpb, StaticSub sub) {
            this.sub = sub;
            this.cpb = cpb;
        }

        CpsOp AccessLex(object[] zyg) {
            return RawAccessLex( JScalar.S(zyg[0]), JScalar.S(zyg[1]),
                (zyg.Length > 2 ? Scan(zyg[2]) : null) );
        }

        CpsOp RawAccessLex(string type, string name, CpsOp set_to) {
            bool core = (type == "corelex");
            bool letonly = (type == "letvar");

            Type t;
            if (!core && let_types.TryGetValue(name, out t)) {
                return (set_to == null) ? CpsOp.PeekLet(name, t) :
                    CpsOp.PokeLet(name, new CpsOp[1] { set_to });
            }
            if (letonly)
                throw new Exception("No such let " + name);

            int uplevel;
            Lexical lex = ResolveLex(name, out uplevel, core);

            return CpsOp.LexAccess(lex, uplevel,
                set_to == null ? new CpsOp[0] : new CpsOp[] { set_to });
        }

        Lexical ResolveLex(string name, out int uplevel, bool core) {
            uplevel = 0;
            StaticSub csr = sub;

            while (true) {
                Lexical r;
                if ((!core || csr.IsCore()) &&
                        csr.l_lexicals.TryGetValue(name, out r)) {
                    if (r is LexAlias) {
                        name = (r as LexAlias).to;
                    } else {
                        return r;
                    }
                }
                if (csr.outer == null)
                    throw new Exception("Unable to find lexical " + name + " in " + sub.name);
                csr = (StaticSub) csr.outer.Resolve();
                uplevel++;
            }
        }

        CpsOp Constant(CpsOp val) {
            if (val.stmts.Length != 0 || val.head.HasCases)
                throw new ArgumentException();
            FieldBuilder fb =
                cpb.tb.DefineField("K" + cpb.module.constants++,
                        val.head.Returns, FieldAttributes.Static);
            cpb.module.thaw.Add(CpsOp.SetSField(fb, val));
            return CpsOp.GetSField(fb);
        }

        CpsOp SubyCall(bool ismethod, object[] zyg) {
            string mname = ismethod ? (((JScalar)zyg[1]).str) : null;
            int sh = ismethod ? 3 : 2;
            string sig = ((JScalar) zyg[sh-1]).str;
            CpsOp[] args = new CpsOp[zyg.Length - sh];
            for (int i = 0; i < args.Length; i++)
                args[i] = Scan(zyg[i+sh]);
            return CpsOp.SubyCall(mname, sig, args);
        }

        static string FixStr(object z) { return ((JScalar)z).str; }
        static double FixNum(object z) { return ((JScalar)z).num; }
        static int FixInt(object z) { return (int)FixNum(z); }
        static bool FixBool(object z) { return FixNum(z) != 0; }
        CpsOp AnyStr(object z) {
            return (z is JScalar) ? CpsOp.StringLiteral(FixStr(z))
                : Scan(z);
        }

        static Dictionary<string, Func<NamProcessor, object[], CpsOp>> handlers;
        static Dictionary<string, Type> namtypes;

        static NamProcessor() {
            namtypes = new Dictionary<string, Type>();
            namtypes["str"] = Tokens.String;
            namtypes["num"] = Tokens.Double;
            namtypes["int"] = Tokens.Int32;
            namtypes["var"] = Tokens.Variable;
            namtypes["obj"] = Tokens.IP6;
            namtypes["fvarlist"] = Tokens.FVarList;
            namtypes["vvarlist"] = Tokens.VVarList;
            namtypes["varhash"] = Tokens.VarHash;
            namtypes["frame"] = Tokens.Frame;
            namtypes["cursor"] = Tokens.Cursor;
            namtypes["strbuf"] = typeof(StringBuilder);
            namtypes["treader"] = typeof(TextReader);

            handlers = new Dictionary<string, Func<NamProcessor,object[],CpsOp>>();
            Dictionary<string, Func<CpsOp[], CpsOp>>
                thandlers = new Dictionary<string, Func<CpsOp[], CpsOp>>();
            Dictionary<string, Func<object[], object>> mhandlers =
                new Dictionary<string, Func<object[], object>>();


            handlers["null"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.Null(namtypes[((JScalar)zyg[1]).str]); };
            handlers["str"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.StringLiteral(((JScalar)zyg[1]).str); };
            handlers["int"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.IntLiteral((int) ((JScalar)zyg[1]).num); };
            handlers["char"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.CharLiteral(JScalar.S(zyg[1])[0]); };
            handlers["double"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.DoubleLiteral(((JScalar)zyg[1]).num); };
            handlers["bool"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.BoolLiteral(((JScalar)zyg[1]).num != 0); };
            handlers["ann"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.Annotate((int) ((JScalar)zyg[2]).num, th.Scan(zyg[3])); };
            handlers["ehspan"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.EhSpan(FixInt(z[1]), FixStr(z[2]), FixInt(z[3]),
                    FixStr(z[4]), FixStr(z[5]), FixStr(z[6]));
            };
            handlers["label"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.Label(FixStr(z[1]), true);
            };
            handlers["cgoto"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.Goto(FixStr(z[1]), false, new CpsOp[] { th.Scan(z[2]) });
            };
            handlers["ncgoto"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.Goto(FixStr(z[1]), true, new CpsOp[] { th.Scan(z[2]) });
            };
            handlers["goto"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.Goto(FixStr(z[1]), false, new CpsOp[] {});
            };
            handlers["span"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.Span(FixStr(z[1]), FixStr(z[2]), FixBool(z[3]),
                    th.Scan(z[4]));
            };
            handlers["compare"] = handlers["arith"] =
                delegate(NamProcessor th, object[] zyg) {
                    return CpsOp.PolyOp(FixStr(zyg[1]),
                            th.Scan(zyg[2]), th.Scan(zyg[3])); };
            handlers["setslot"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.MethodCall(null, Tokens.IP6_SetSlot, new CpsOp[] {
                    th.Scan(zyg[2]), th.AnyStr(zyg[1]), th.Scan(zyg[3]) }); };
            handlers["getslot"] = delegate(NamProcessor th, object[] zyg) {
                Type ty = namtypes[FixStr(zyg[2])];
                return CpsOp.UnboxAny(ty, CpsOp.MethodCall(null,
                    Tokens.IP6_GetSlot, new CpsOp[] { th.Scan(zyg[3]),
                        th.AnyStr(zyg[1]) })); };
            handlers["cast"] = delegate(NamProcessor th, object[] zyg) {
                Type tty = namtypes[FixStr(zyg[1])];
                CpsOp z = th.Scan(zyg[2]);
                Type fty = z.head.Returns;

                if (tty == Tokens.Frame && fty == Tokens.IP6
                        || tty == Tokens.Cursor && fty == Tokens.IP6) {
                    return CpsOp.UnboxAny(tty, z);
                } else if (tty == Tokens.Double && fty == Tokens.Int32) {
                    return CpsOp.Operator(tty, OpCodes.Conv_R8, new CpsOp[]{z});
                } else if (tty == Tokens.Int32 && fty == Tokens.Double) {
                    return CpsOp.Operator(tty, OpCodes.Conv_I4, new CpsOp[]{z});
                } else {
                    throw new NotImplementedException("cast " + fty + " -> " + tty);
                }
            };
            handlers["die"] = delegate(NamProcessor th, object[] zyg) {
                if (zyg[1] is JScalar) {
                    return CpsOp.MethodCall(Tokens.Variable, Tokens.Kernel_Die,
                        new CpsOp[] { CpsOp.StringLiteral(FixStr(zyg[1])) });
                } else {
                    return CpsOp.MethodCall(Tokens.Variable, Tokens.Kernel_SFH,
                        new CpsOp[] { CpsOp.IntLiteral(SubInfo.ON_DIE),
                        CpsOp.Null(Tokens.Frame), CpsOp.IntLiteral(-1),
                        CpsOp.Null(Tokens.String), CpsOp.MethodCall(null,
                            Tokens.Kernel_NewROScalar, new CpsOp[] { th.Scan(zyg[1]) }) });
                }
            };
            handlers["control"] = delegate(NamProcessor th, object[] zyg) {
                CpsOp[] z = new CpsOp[5];
                for (int i = 1; i < 5; i++)
                    z[i] = th.Scan(zyg[i+1]);
                z[0] = CpsOp.IntLiteral(FixInt(zyg[1]));
                return CpsOp.MethodCall(Tokens.Variable, Tokens.Kernel_SFH, z);
            };
            handlers["box"] = delegate(NamProcessor th, object[] zyg) {
                CpsOp mo;
                if (zyg[1] is JScalar) {
                    string name = ((JScalar)zyg[1]).str;
                    // these might need to happen *early*, before user classes
                    // are set up
                    if (name == "Str") {
                        mo = CpsOp.GetSField(Tokens.Kernel_StrMO);
                    } else if (name == "Num") {
                        mo = CpsOp.GetSField(Tokens.Kernel_NumMO);
                    } else {
                        Class p = (Class)th.sub.GetCorePackage(name);
                        mo = new CpsOp(new ClrGetSField(p.metaObject));
                    }
                } else {
                    mo = CpsOp.GetField(Tokens.IP6_mo, th.Scan(zyg[1]));
                }
                CpsOp boxee = th.Scan(zyg[2]);
                return CpsOp.MethodCall(null, Tokens.Kernel.GetMethod("BoxAnyMO").MakeGenericMethod(boxee.head.Returns), new CpsOp[2] { boxee, mo });
            };
            handlers["unbox"] = delegate(NamProcessor th, object[] zyg) {
                Type t = namtypes[((JScalar)zyg[1]).str];
                CpsOp unboxee = th.Scan(zyg[2]);
                return CpsOp.MethodCall(null, Tokens.Kernel.GetMethod("UnboxAny").MakeGenericMethod(t), new CpsOp[1] { unboxee });
            };
            handlers["newboundvar"] = delegate(NamProcessor th, object[] zyg) {
                CpsOp rhs = th.Scan(zyg[3]);
                bool ro   = JScalar.B(zyg[1]);
                bool list = JScalar.B(zyg[2]);
                return CpsOp.MethodCall(Tokens.Variable,
                        Tokens.Kernel_NewBoundVar, new CpsOp[] {
                            CpsOp.BoolLiteral(ro),
                            CpsOp.BoolLiteral(list),
                            CpsOp.GetSField(Tokens.Kernel_AnyMO),
                            rhs });
            };
            handlers["whileloop"] = delegate(NamProcessor th, object[] z) {
                bool until = ((JScalar)z[1]).num != 0;
                bool once  = ((JScalar)z[2]).num != 0;
                return CpsOp.While(until, once, th.Scan(z[3]), th.Scan(z[4])); };
            handlers["_makesub"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(null, Tokens.Kernel_MakeSub, new CpsOp[]{
                    CpsOp.GetSField(((StaticSub)z[1]).subinfo),
                    CpsOp.CallFrame() }); };
            handlers["class_ref"] = delegate(NamProcessor th, object[] z) {
                string kind = FixStr(z[1]);
                ModuleWithTypeObject m;
                if (z.Length == 3) {
                    m = (ModuleWithTypeObject)th.sub.GetCorePackage(FixStr(z[2]));
                } else {
                    m = (ModuleWithTypeObject) (new Xref(z, 2)).Resolve();
                }
                if (kind != "mo")
                    throw new NotImplementedException();
                return CpsOp.GetSField(m.metaObject);
            };
            handlers["scopedlex"] =
            handlers["letvar"] =
            handlers["corelex"] = delegate(NamProcessor th, object[] zyg) {
                return th.AccessLex(zyg); };
            handlers["methodcall"] = delegate (NamProcessor th, object[] zyg) {
                return th.SubyCall(true, zyg); };
            handlers["subcall"] = delegate (NamProcessor th, object[] zyg) {
                return th.SubyCall(false, zyg); };
            handlers["letn"] = delegate(NamProcessor th, object[] zyg) {
                int i = 1;
                Dictionary<string,Type> old =
                    new Dictionary<string,Type>(th.let_types);
                List<KeyValuePair<string,CpsOp>> lvec =
                    new List<KeyValuePair<string,CpsOp>>();
                while (zyg.Length - i >= 3 && zyg[i] is JScalar) {
                    string name = ((JScalar)zyg[i]).str;
                    CpsOp  init = th.Scan(zyg[i+1]);
                    th.let_types[name] = init.head.Returns;
                    lvec.Add(new KeyValuePair<string,CpsOp>(name, init));
                    i += 2;
                }
                List<CpsOp> rest = new List<CpsOp>();
                while (i < zyg.Length)
                    rest.Add(th.Scan(zyg[i++]));
                CpsOp bit = CpsOp.Sequence(rest.ToArray());
                for (int j = lvec.Count - 1; j >= 0; j--)
                    bit = CpsOp.Let(lvec[j].Key, lvec[j].Value, bit);
                th.let_types = old;
                return bit;
            };
            handlers["newcc"] = delegate(NamProcessor th, object[] z) {
                int[] vec = new int[z.Length - 1];
                for (int i = 0; i < vec.Length; i++)
                    vec[i] = FixInt(z[i+1]);
                return CpsOp.ConstructorCall(Tokens.CC_ctor, new CpsOp[] {
                    CpsOp.NewIntArray(vec) });
            };
            handlers["rxpushcapture"] = delegate(NamProcessor th, object[] z) {
                CpsOp[] strs = new CpsOp[z.Length - 2];
                for(int i = 0; i < strs.Length; i++)
                    strs[i] = CpsOp.StringLiteral(FixStr(z[i+2]));
                CpsOp vec = th.Constant(CpsOp.NewArray(Tokens.String, strs));
                return CpsOp.MethodCall(null, Tokens.RxFrame_PushCapture, new CpsOp[] {
                    CpsOp.GetField(Tokens.Frame_rx, CpsOp.CallFrame()), vec,
                    th.Scan(z[1]) });
            };
            handlers["rxbprim"] = delegate(NamProcessor th, object[] z) {
                CpsOp[] args = new CpsOp[z.Length - 1];
                for(int i = 0; i < z.Length - 2; i++)
                    args[i+1] = th.Scan(z[i+2]);
                args[0] = CpsOp.GetField(Tokens.Frame_rx, CpsOp.CallFrame());
                CpsOp call = CpsOp.MethodCall(null,
                        Tokens.RxFrame.GetMethod(FixStr(z[1])), args);
                return CpsOp.Goto("backtrack", true, new CpsOp[] { call });
            };
            handlers["const"] = delegate(NamProcessor th, object[] z) {
                return th.Constant(th.Scan(z[1])); };

            thandlers["return"] = CpsOp.CpsReturn;
            thandlers["ternary"] = delegate(CpsOp[] z) {
                return CpsOp.Ternary(z[0], z[1], z[2]); };
            thandlers["sink"] = delegate(CpsOp[] z) {
                return CpsOp.Sink(z[0]); };
            thandlers["callframe"] = delegate(CpsOp[] z) { return CpsOp.CallFrame(); };
            thandlers["fvarlist_new"] = delegate(CpsOp[] z) {
                return CpsOp.NewArray(Tokens.Variable, z); };
            thandlers["fcclist_new"] = delegate(CpsOp[] z) {
                return CpsOp.NewArray(Tokens.CC, z); };
            thandlers["setbox"] = delegate(CpsOp[] z) {
                MethodInfo mi = typeof(Kernel).GetMethod("SetBox").MakeGenericMethod(z[1].head.Returns);
                return CpsOp.MethodCall(null, mi, z); };
            // yuck.
            thandlers["mrl_count"] = thandlers["fvarlist_length"] = delegate(CpsOp[] z) {
                return CpsOp.Operator(Tokens.Int32, OpCodes.Conv_I4,
                    new CpsOp[] { CpsOp.Operator(Tokens.IntPtr, OpCodes.Ldlen,
                        z) });
            };
            thandlers["newblankrwscalar"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, Tokens.Kernel_NewRWScalar,
                    new CpsOp[] { CpsOp.GetSField(Tokens.Kernel_AnyMO),
                        CpsOp.GetSField(Tokens.Kernel_AnyP) }); };
            // XXX - wrong order - problem?
            thandlers["fvarlist_item"] = delegate(CpsOp[] z) {
                return CpsOp.Operator(Tokens.Variable, OpCodes.Ldelem_Ref,
                    new CpsOp[] { z[1], z[0] }); };
            thandlers["mrl_index"] = delegate(CpsOp[] z) {
                return CpsOp.Operator(Tokens.IP6, OpCodes.Ldelem_Ref,
                    new CpsOp[] { z[1], z[0] }); };
            thandlers["vvarlist_item"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, Tokens.VVarList_Item, new CpsOp[]{
                    z[1], z[0] }); };
            thandlers["varhash_getindex"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, Tokens.VarHash_get_Item, new CpsOp[]{
                    z[1], z[0] }); };
            thandlers["varhash_setindex"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, Tokens.VarHash_set_Item, new CpsOp[]{
                    z[1], z[0], z[2] }); };
            thandlers["vvarlist_sort"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, Tokens.Kernel_SortHelper,
                    new CpsOp[] { CpsOp.CallFrame(), z[0], z[1] }); };
            thandlers["context_get"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, Tokens.Kernel_ContextHelper,
                    new CpsOp[] { CpsOp.CallFrame(), z[0], z[1] }); };
            thandlers["status_get"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, Tokens.Kernel_StatusHelper,
                    new CpsOp[] { CpsOp.CallFrame(), z[0], z[1] }); };
            thandlers["set_status"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, Tokens.Kernel_SetStatus,
                    new CpsOp[] { CpsOp.CallFrame(), z[0], z[1] }); };
            thandlers["newscalar"] = Methody(null, Tokens.Kernel_NewROScalar);
            thandlers["newrwlistvar"] = Methody(null, Tokens.Kernel_NewRWListVar);
            thandlers["iter_hasflat"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, Tokens.Kernel_IterHasFlat,
                    new CpsOp[] { z[0], CpsOp.BoolLiteral(true) }); };
            thandlers["iter_hasarg"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, Tokens.Kernel_IterHasFlat,
                    new CpsOp[] { z[0], CpsOp.BoolLiteral(false) }); };
            thandlers["newrwscalar"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, Tokens.Kernel_NewRWScalar, new CpsOp[]{
                    CpsOp.GetSField(Tokens.Kernel_AnyMO), z[0] }); };
            thandlers["newvsubvar"] = delegate(CpsOp[] z) {
                return CpsOp.ConstructorCall(Tokens.SV_ctor, new CpsOp[] {
                    CpsOp.BoolLiteral(true), CpsOp.BoolLiteral(false), z[0],
                    CpsOp.ConstructorCall(Tokens.SubViviHook_ctor, new CpsOp[] {
                        z[1] }), z[2] }); };
            thandlers["newvhashvar"] = delegate(CpsOp[] z) {
                return CpsOp.ConstructorCall(Tokens.SV_ctor, new CpsOp[] {
                    CpsOp.BoolLiteral(true), CpsOp.BoolLiteral(false), z[0],
                    CpsOp.ConstructorCall(Tokens.HashViviHook_ctor, new CpsOp[] {
                        z[1], z[2] }), z[3] }); };
            thandlers["newvarrayvar"] = delegate(CpsOp[] z) {
                return CpsOp.ConstructorCall(Tokens.SV_ctor, new CpsOp[] {
                    CpsOp.BoolLiteral(true), CpsOp.BoolLiteral(false), z[0],
                    CpsOp.ConstructorCall(Tokens.ArrayViviHook_ctor, new CpsOp[] {
                        z[1], z[2] }), z[3] }); };
            thandlers["newvnewhashvar"] = delegate(CpsOp[] z) {
                return CpsOp.ConstructorCall(Tokens.SV_ctor, new CpsOp[] {
                    CpsOp.BoolLiteral(true), CpsOp.BoolLiteral(false), z[0],
                    CpsOp.ConstructorCall(Tokens.NewHashViviHook_ctor, new CpsOp[] {
                        z[1], z[2] }), z[3] }); };
            thandlers["newvnewarrayvar"] = delegate(CpsOp[] z) {
                return CpsOp.ConstructorCall(Tokens.SV_ctor, new CpsOp[] {
                    CpsOp.BoolLiteral(true), CpsOp.BoolLiteral(false), z[0],
                    CpsOp.ConstructorCall(Tokens.NewArrayViviHook_ctor, new CpsOp[] {
                        z[1], z[2] }), z[3] }); };
            thandlers["strbuf_append"] = delegate(CpsOp[] z) {
                return CpsOp.Sink(CpsOp.MethodCall(null, Tokens.StringBuilder_Append_String, z)); };
            thandlers["varhash_delete_key"] = delegate(CpsOp[] z) {
                return CpsOp.Sink(CpsOp.MethodCall(null, Tokens.VarHash_Remove, z)); };
            thandlers["note"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, Tokens.TW_WriteLine, new CpsOp[]{
                    CpsOp.MethodCall(null, Tokens.Console_get_Error, new CpsOp[0]),
                    z[0] }); };
            ConstructorInfo string_ctor = Tokens.String.GetConstructor(new Type[] {
                    typeof(char), Tokens.Int32 });
            thandlers["str_chr"] = delegate(CpsOp[] z) {
                return CpsOp.ConstructorCall(string_ctor, new CpsOp[] {
                    CpsOp.Operator(typeof(char), OpCodes.Conv_U2, z),
                    CpsOp.IntLiteral(1) });
            };
            MethodInfo itcommon = Tokens.Builtins.GetMethod("HashIter");
            thandlers["bif_hash_keys"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, itcommon, new CpsOp[] {
                    CpsOp.IntLiteral(0), z[0] }); };
            thandlers["bif_hash_values"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, itcommon, new CpsOp[] {
                    CpsOp.IntLiteral(1), z[0] }); };
            thandlers["bif_hash_kv"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, itcommon, new CpsOp[] {
                    CpsOp.IntLiteral(2), z[0] }); };
            thandlers["bif_hash_pairs"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(null, itcommon, new CpsOp[] {
                    CpsOp.IntLiteral(3), z[0] }); };
            Func<CpsOp[], CpsOp> real_pushcut = RxCall(null, "PushCutGroup");
            handlers["pushcut"] = delegate(NamProcessor th, object[] z) {
                return real_pushcut(new CpsOp[] { CpsOp.StringLiteral(FixStr(z[1])) }); };
            thandlers["rxframe"] = delegate(CpsOp[] z) {
                return CpsOp.GetField(Tokens.Frame_rx, CpsOp.CallFrame()); };
            handlers["rxcall"] = delegate(NamProcessor th, object[] z) {
                CpsOp[] x = new CpsOp[z.Length - 1];
                for (int i = 2; i < z.Length; i++)
                    x[i-1] = th.Scan(z[i]);
                x[0] = CpsOp.GetField(Tokens.Frame_rx, CpsOp.CallFrame());
                string name = JScalar.S(z[1]);
                return CpsOp.MethodCall((name == "EndWith" || name == "End") ? Tokens.Void : null, Tokens.RxFrame.GetMethod(name), x); };
            handlers["rxinit"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.SetField(Tokens.Frame_rx, CpsOp.CallFrame(),
                    CpsOp.ConstructorCall(Tokens.RxFrame_ctor, new CpsOp[] {
                        th.Scan(z[1]), th.Scan(z[2]),
                        CpsOp.BoolLiteral(FixBool(z[3])),
                        CpsOp.BoolLiteral(FixBool(z[4])) })); };
            handlers["rxpushb"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(null, Tokens.RxFrame_PushBacktrack, new CpsOp[] {
                    CpsOp.GetField(Tokens.Frame_rx, CpsOp.CallFrame()),
                    CpsOp.LabelId(th.cpb.cx, JScalar.S(z[2])) }); };
            handlers["label_table"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.LabelTable(th.cpb.cx, JScalar.SA(1,z)); };
            thandlers["popcut"] = RxCall(null, "PopCutGroup");
            thandlers["rxend"] = RxCall(Tokens.Void, "End");
            thandlers["rxfinalend"] = RxCall(Tokens.Void, "FinalEnd");
            thandlers["rxbacktrack"] = RxCall(Tokens.Void, "Backtrack");
            thandlers["rxgetquant"] = RxCall(null, "GetQuant");
            thandlers["rxsetquant"] = RxCall(null, "SetQuant");
            thandlers["rxopenquant"] = RxCall(null, "OpenQuant");
            thandlers["rxclosequant"] = RxCall(null, "CloseQuant");
            thandlers["rxincquant"] = RxCall(null, "IncQuant");
            thandlers["rxsetclass"] = RxCall(null, "SetClass");
            thandlers["rxsetpos"] = RxCall(null, "SetPos");
            thandlers["rxsetcapsfrom"] = RxCall(null, "SetCapturesFrom");
            thandlers["rxgetpos"] = RxCall(null, "GetPos");
            thandlers["rxcommitgroup"] = RxCall(null, "CommitGroup");
            thandlers["get_lexer"] = Methody(null, typeof(Lexer).GetMethod("GetLexer"));
            thandlers["run_protoregex"] = Methody(null, typeof(Lexer).GetMethod("RunProtoregex"));
            handlers["ladconstruct"] = delegate(NamProcessor th, object[] z) {
                return th.ProcessLADArr(z[1]); };

            thandlers["var_islist"] = FieldGet(Tokens.Variable, "islist");
            thandlers["llhow_name"] = FieldGet(Tokens.DynMetaObject, "name");
            thandlers["stab_what"] = FieldGet(Tokens.DynMetaObject, "typeObject");
            thandlers["obj_llhow"] = FieldGet(Tokens.IP6, "mo");
            thandlers["varhash_clear"] = Methody(null, Tokens.VarHash.GetMethod("Clear"));
            thandlers["varhash_new"] = Constructy(Tokens.VarHash.GetConstructor(new Type[0]));
            thandlers["varhash_dup"] = Constructy(Tokens.VarHash.GetConstructor(new Type[]{ Tokens.VarHash }));
            thandlers["varhash_contains_key"] = Methody(null, Tokens.VarHash.GetMethod("ContainsKey"));
            thandlers["num_to_string"] = Methody(null, Tokens.Object_ToString);
            thandlers["str_length"] = Methody(null, Tokens.String.GetMethod("get_Length"));
            thandlers["str_substring"] = Methody(null, Tokens.Builtins.GetMethod("LaxSubstring2"));
            thandlers["str_tolower"] = Methody(null, Tokens.String.GetMethod("ToLowerInvariant"));
            thandlers["str_toupper"] = Methody(null, Tokens.String.GetMethod("ToUpperInvariant"));
            thandlers["strcmp"] = Methody(null, Tokens.String.GetMethod("CompareOrdinal", new Type[] { Tokens.String, Tokens.String }));
            thandlers["strbuf_new"] = Constructy(typeof(StringBuilder).GetConstructor(new Type[0]));
            thandlers["strbuf_seal"] = Methody(null, Tokens.Object_ToString);
            thandlers["say"] = Methody(null, Tokens.Console_WriteLine);
            thandlers["print"] = Methody(null, Tokens.Console_Write);
            thandlers["exit"] = Methody(null, Tokens.Environment_Exit);
            thandlers["slurp"] = Methody(null, typeof(File).GetMethod("ReadAllText", new Type[] { Tokens.String }));
            thandlers["spew"] = Methody(null, typeof(File).GetMethod("WriteAllText", new Type[] { Tokens.String, Tokens.String }));
            thandlers["getargv"] = Methody(null, Tokens.Kernel.GetMethod("ArgsHelper"));
            thandlers["vvarlist_to_fvarlist"] = Methody(null, Tokens.VVarList.GetMethod("CopyAsArray"));
            thandlers["vvarlist_shift"] = Methody(null, Tokens.VVarList.GetMethod("Shift"));
            thandlers["vvarlist_pop"] = Methody(null, Tokens.VVarList.GetMethod("Pop"));
            thandlers["vvarlist_count"] = Methody(null, Tokens.VVarList.GetMethod("Count"));
            thandlers["vvarlist_unshift"] = Methody(null, Tokens.VVarList.GetMethod("Unshift"));
            thandlers["vvarlist_unshiftn"] = Methody(null, Tokens.VVarList.GetMethod("UnshiftN"));
            thandlers["vvarlist_append"] = Methody(null, Tokens.VVarList.GetMethod("PushD"));
            thandlers["vvarlist_push"] = Methody(null, Tokens.VVarList.GetMethod("Push"));
            thandlers["vvarlist_new_empty"] = Constructy(Tokens.VVarList.GetConstructor(new Type[] { }));
            thandlers["vvarlist_new_singleton"] = Constructy(Tokens.VVarList.GetConstructor(new Type[] { Tokens.Variable }));
            thandlers["vvarlist_from_fvarlist"] = Constructy(Tokens.VVarList.GetConstructor(new Type[] { Tokens.FVarList }));
            thandlers["vvarlist_clone"] = Constructy(Tokens.VVarList.GetConstructor(new Type[] { Tokens.VVarList }));
            thandlers["stab_privatemethod"] = Methody(null, Tokens.DynMetaObject.GetMethod("GetPrivateMethod"));
            thandlers["_addprivatemethod"] = Methody(null, Tokens.DMO_AddPrivateMethod);
            handlers["_parametricrole"] = delegate(NamProcessor th, object[] z) { return th.FillParamRole(); };
            thandlers["_addmethod"] = Methody(null, Tokens.DMO_AddMethod);
            thandlers["_invalidate"] = Methody(null, Tokens.DynMetaObject.GetMethod("Invalidate"));
            thandlers["obj_is_defined"] = Methody(null, Tokens.IP6.GetMethod("IsDefined"));
            thandlers["how"] = Methody(Tokens.IP6, Tokens.IP6.GetMethod("HOW"));
            thandlers["obj_what"] = Methody(null, Tokens.IP6.GetMethod("GetTypeObject"));
            thandlers["obj_isa"] = Methody(null, Tokens.IP6.GetMethod("Isa"));
            thandlers["obj_does"] = Methody(null, Tokens.IP6.GetMethod("Does"));
            thandlers["obj_newblank"] = Constructy(Tokens.DynObject_ctor);
            thandlers["cursor_start"] = Constructy(Tokens.Cursor.GetConstructor(new Type[] { Tokens.IP6, Tokens.String }));
            thandlers["cursor_pos"] = FieldGet(Tokens.Cursor, "pos");
            thandlers["cursor_from"] = FieldGet(Tokens.Cursor, "from");
            thandlers["cursor_butpos"] = Methody(null, Tokens.Cursor.GetMethod("At"));
            thandlers["cursor_backing"] = Methody(null, Tokens.Cursor.GetMethod("GetBacking"));
            thandlers["cursor_dows"] = Methody(null, Tokens.Cursor.GetMethod("SimpleWS"));
            thandlers["cursor_item"] = Methody(null, Tokens.Cursor.GetMethod("GetKey"));
            thandlers["cursor_unpackcaps"] = Methody(null, Tokens.Cursor.GetMethod("UnpackCaps"));
            thandlers["cursor_O"] = Methody(null, Tokens.Cursor.GetMethod("O"));
            thandlers["cursor_synthetic"] = Constructy(Tokens.Cursor.GetConstructor(new Type[] { Tokens.Cursor, Tokens.String, Tokens.Int32, Tokens.Int32 }));
            thandlers["cursor_synthcap"] = Methody(null, Tokens.Cursor.GetMethod("SynPushCapture"));
            thandlers["cursor_fresh"] = Methody(null, Tokens.Cursor.GetMethod("FreshClass"));
            thandlers["rxstripcaps"] = Methody(null, Tokens.Cursor.GetMethod("StripCaps"));

            thandlers["prog"] = CpsOp.Sequence;

            thandlers["bif_postinc"] = SimpleB("PostIncrement");
            thandlers["bif_numeq"] = SimpleB("NumericEq");
            thandlers["bif_numne"] = SimpleB("NumericNe");
            thandlers["bif_numle"] = SimpleB("NumericLe");
            thandlers["bif_numlt"] = SimpleB("NumericLt");
            thandlers["bif_numge"] = SimpleB("NumericGe");
            thandlers["bif_numgt"] = SimpleB("NumericGt");
            thandlers["bif_streq"] = SimpleB("StringEq");
            thandlers["bif_strne"] = SimpleB("StringNe");
            thandlers["bif_strle"] = SimpleB("StringLe");
            thandlers["bif_strlt"] = SimpleB("StringLt");
            thandlers["bif_strge"] = SimpleB("StringGe");
            thandlers["bif_strgt"] = SimpleB("StringGt");
            thandlers["bif_plus"] = SimpleB("Plus");
            thandlers["bif_minus"] = SimpleB("Minus");
            thandlers["bif_mul"] = SimpleB("Mul");
            thandlers["bif_divide"] = SimpleB("Divide");
            thandlers["bif_not"] = SimpleB("Not");
            thandlers["bif_negate"] = SimpleB("Negate");
            thandlers["bif_chars"] = SimpleB("Chars");
            thandlers["bif_substr3"] = SimpleB("Substr3");

            thandlers["bif_defined"] = Contexty("mro_defined");
            thandlers["bif_bool"] = Contexty("mro_Bool");
            thandlers["bif_num"] = Contexty("mro_Numeric");
            thandlers["bif_str"] = Contexty("mro_Str");
            thandlers["obj_asdef"] = Contexty("mro_defined");
            thandlers["obj_asbool"] = Contexty("mro_Bool");
            thandlers["obj_asnum"] = Contexty("mro_Numeric");
            thandlers["obj_asstr"] = Contexty("mro_Str");
            thandlers["obj_getbool"] = Contexty("mro_raw_Bool");
            thandlers["obj_getdef"] = Contexty("mro_raw_defined");
            thandlers["obj_getnum"] = Contexty("mro_raw_Numeric");
            thandlers["obj_getstr"] = Contexty("mro_raw_Str");
            thandlers["bif_at_key"] = thandlers["obj_at_key"] = Contexty("mro_at_key");
            thandlers["bif_at_pos"] = thandlers["obj_at_pos"] = Contexty("mro_at_pos");
            thandlers["bif_exists_key"] = thandlers["obj_exists_key"] = Contexty("mro_exists_key");
            thandlers["bif_delete_key"] = thandlers["obj_delete_key"] = Contexty("mro_delete_key");
            thandlers["obj_typename"] = Methody(null, Tokens.IP6.GetMethod("GetTypeName"));
            thandlers["fetch"] = Methody(null, Tokens.Variable_Fetch);
            thandlers["bget"] = FieldGet(Tokens.BValue, "v");
            thandlers["default_new"] = Methody(null, Tokens.Kernel.GetMethod("DefaultNew"));
            thandlers["assign"] = Methody(Tokens.Void, Tokens.Kernel.GetMethod("Assign"));
            thandlers["cotake"] = Methody(Tokens.Variable, Tokens.Kernel.GetMethod("CoTake"));
            thandlers["take"] = Methody(Tokens.Variable, Tokens.Kernel.GetMethod("Take"));
            thandlers["startgather"] = Methody(Tokens.Frame, Tokens.Kernel.GetMethod("GatherHelper"));
            thandlers["get_first"] = Methody(null, Tokens.Kernel.GetMethod("GetFirst"));
            thandlers["promote_to_list"] = Methody(null, Tokens.Kernel.GetMethod("PromoteToList"));
            thandlers["instrole"] = Methody(Tokens.Variable, Tokens.Kernel.GetMethod("InstantiateRole"));
            thandlers["role_apply"] = Methody(null, Tokens.Kernel.GetMethod("RoleApply"));
            thandlers["iter_to_list"] = Methody(null, Tokens.Kernel.GetMethod("IterToList"));
            thandlers["iter_flatten"] = Methody(null, Tokens.Kernel.GetMethod("IterFlatten"));
            thandlers["iter_copy_elems"] = Methody(null, Tokens.Kernel.GetMethod("IterCopyElems"));
            thandlers["to_jsync"] = Methody(null, typeof(JsyncWriter).GetMethod("ToJsync"));
            thandlers["from_jsync"] = Methody(null, typeof(JsyncReader).GetMethod("FromJsync"));
            thandlers["do_require"] = Methody(null, Tokens.Kernel.GetMethod("DoRequire"));
            thandlers["frame_caller"] = FieldGet(Tokens.Frame, "caller");
            thandlers["frame_file"] = Methody(null, Tokens.Frame.GetMethod("ExecutingFile"));
            thandlers["frame_line"] = Methody(null, Tokens.Frame.GetMethod("ExecutingLine"));
            thandlers["frame_hint"] = Methody(null, Tokens.Frame.GetMethod("LexicalFind"));
            thandlers["treader_getc"] = Methody(null, typeof(TextReader).GetMethod("Read", new Type[0]));
            thandlers["treader_slurp"] = Methody(null, typeof(TextReader).GetMethod("ReadToEnd"));
            thandlers["treader_getline"] = Methody(null, typeof(TextReader).GetMethod("ReadLine"));
            thandlers["treader_stdin"] = Methody(null, typeof(Kernel).GetMethod("OpenStdin"));

            foreach (KeyValuePair<string, Func<CpsOp[], CpsOp>> kv
                    in thandlers) {
                handlers[kv.Key] = MakeTotalHandler(kv.Value);
            }
            foreach (KeyValuePair<string, Func<object[], object>> kv
                    in mhandlers) {
                handlers[kv.Key] = MakeMacroHandler(kv.Value);
            }
        }

        static Func<CpsOp[], CpsOp> SimpleB(string name) {
            return Methody(null, Tokens.Builtins.GetMethod(name));
        }

        static Func<CpsOp[], CpsOp> Methody(Type cps, MethodInfo mi) {
            if (mi == null) throw new ArgumentException();
            return delegate(CpsOp[] cpses) {
                return CpsOp.MethodCall(cps, mi, cpses); };
        }

        static Func<CpsOp[], CpsOp> RxCall(Type cps, string name) {
            MethodInfo mi = Tokens.RxFrame.GetMethod(name);
            return delegate(CpsOp[] cpses) {
                CpsOp[] n = new CpsOp[cpses.Length + 1];
                Array.Copy(cpses, 0, n, 1, cpses.Length);
                n[0] = CpsOp.GetField(Tokens.Frame_rx, CpsOp.CallFrame());
                return CpsOp.MethodCall(cps, mi, n); };
        }

        static Func<CpsOp[], CpsOp> Constructy(ConstructorInfo mi) {
            return delegate(CpsOp[] cpses) {
                return CpsOp.ConstructorCall(mi, cpses); };
        }

        static Func<CpsOp[], CpsOp> FieldGet(Type t, string name) {
            FieldInfo f = t.GetField(name);
            return delegate(CpsOp[] cpses) {
                return CpsOp.GetField(f, cpses[0]); };
        }

        static Func<CpsOp[], CpsOp> Contexty(string name) {
            FieldInfo f = Tokens.DynMetaObject.GetField(name);
            MethodInfo g = f.FieldType.GetMethod("Get");
            return delegate(CpsOp[] cpses) {
                return CpsOp.Contexty(f, g, cpses);
            };
        }

        static Func<NamProcessor, object[], CpsOp> MakeTotalHandler(
                Func<CpsOp[], CpsOp> real) {
            return delegate (NamProcessor th, object[] zyg) {
                CpsOp[] zco = new CpsOp[zyg.Length - 1];
                for (int i = 0; i < zco.Length; i++)
                    zco[i] = th.Scan(zyg[i+1]);
                return real(zco);
            };
        }

        static Func<NamProcessor, object[], CpsOp> MakeMacroHandler(
                Func<object[], object> real) {
            return delegate (NamProcessor th, object[] zyg) {
                return th.Scan(real(zyg)); };
        }

        public void MakeBody() {
            cpb.ReserveLex(sub.nlexn);
            cpb.Build(Scan(WrapBody()));
        }

        CpsOp ProcessLADArr(object lad) {
            object[] lada = (object[]) lad;
            CpsOp[] z = new CpsOp[lada.Length];
            for (int i = 0; i < z.Length; i++)
                z[i] = ProcessLAD(lada[i]);
            return CpsOp.NewArray(Tokens.LAD, z);
        }

        CpsOp ProcessLAD(object lad) {
            object[] body = (object[]) lad;
            string head = FixStr(body[0]);

            if (!Tokens.LADctors.ContainsKey(head))
                throw new ArgumentException(head);
            ConstructorInfo ci = Tokens.LADctors[head];

            if (head == "CC") {
                int[] ccs = JScalar.IA(1, body);
                return CpsOp.ConstructorCall(ci, new CpsOp[] {
                    CpsOp.ConstructorCall(Tokens.CC_ctor, new CpsOp[] {
                        CpsOp.NewIntArray(ccs) }) });
            } else if (head == "Imp" || head == "Dot" || head == "Null" || head == "None") {
                return CpsOp.ConstructorCall(ci, new CpsOp[0]);
            } else if (head == "Str" || head == "StrNoCase" || head == "Param" || head == "ProtoRegex" || head == "Method") {
                return CpsOp.ConstructorCall(ci, new CpsOp[]{
                    CpsOp.StringLiteral(JScalar.S(body[1])) });
            } else if (head == "Opt" || head == "Star" || head == "Plus") {
                return CpsOp.ConstructorCall(ci, new CpsOp[] {
                        ProcessLAD(body[1]) });
            } else if (head == "Sequence" || head == "Any") {
                return CpsOp.ConstructorCall(ci, new CpsOp[] {
                    ProcessLADArr(body[1]) });
            }

            throw new NotImplementedException("ProcessLAD " + head);
        }

        public CpsOp SubInfoCtor() {
            CpsOp[] args = new CpsOp[10];
            args[0] = CpsOp.StringLiteral(sub.unit.name + " " + sub.name);
            args[1] = CpsOp.NewIntArray(cpb.cx.lineBuffer.ToArray());
            args[2] = CpsOp.DBDLiteral(cpb.mb);
            args[3] = (sub.outer != null) ?
                CpsOp.GetSField(((StaticSub)sub.outer.Resolve()).subinfo) :
                CpsOp.Null(Tokens.SubInfo);
            args[4] = (sub.ltm != null) ? ProcessLAD(sub.ltm) :
                CpsOp.Null(Tokens.LAD);
            args[5] = CpsOp.NewIntArray( cpb.cx.ehspanBuffer.ToArray() );
            args[6] = CpsOp.StringArray( true, cpb.cx.ehlabelBuffer.ToArray() );
            args[7] = CpsOp.IntLiteral( cpb.Spills() );
            List<string> dylexn = new List<string>();
            List<int> dylexi = new List<int>();
            foreach (KeyValuePair<string, Lexical> kv in sub.lexicals) {
                if (Lexical.IsDynamicName(kv.Key)) {
                    int index = (kv.Value is LexVarish) ?
                        ((LexVarish)kv.Value).index : -1;
                    if (index >= 0) {
                        dylexn.Add(kv.Key);
                        dylexi.Add(index);
                    }
                }
            }
            if (dylexn.Count > 0) {
                args[8] = CpsOp.StringArray(true, dylexn.ToArray());
                args[9] = CpsOp.NewIntArray(dylexi.ToArray());
            } else {
                args[8] = CpsOp.Null(typeof(string[]));
                args[9] = CpsOp.Null(typeof(int[]));
            }

            return CpsOp.ConstructorCall(Tokens.SubInfo_ctor, args);
        }

        void EnterCode(List<object> frags) {
            foreach (KeyValuePair<string,Lexical> kv in sub.lexicals) {
                if ((sub.flags & StaticSub.RUN_ONCE) != 0 &&
                        (sub.flags & StaticSub.SPAD_EXISTS) != 0 &&
                        !Lexical.IsDynamicName(kv.Key))
                    continue;

                if (kv.Value is LexSub) {
                    LexSub ls = (LexSub) kv.Value;
                    frags.Add(new object[] { new JScalar("scopedlex"),
                        new JScalar(kv.Key),
                        new object[] { new JScalar("newscalar"),
                            new object[] { new JScalar("_makesub"),
                                ls.def.Resolve() } } });
                } else if (kv.Value is LexSimple) {
                    int f = ((LexSimple) kv.Value).flags;
                    if ((f & LexSimple.NOINIT) != 0) continue;

                    object bit;
                    if ((f & (LexSimple.HASH | LexSimple.LIST)) != 0) {
                        string s = ((f & LexSimple.HASH) != 0) ? "Hash" : "Array";
                        bit = new object[] { new JScalar("methodcall"),
                            new JScalar("new"), new JScalar(""),
                            new object[] { new JScalar("fetch"), new object[] { new JScalar("corelex"), new JScalar(s) } },
                            new object[] { new JScalar("corelex"), new JScalar(s) } };
                    } else {
                        bit = new object[] { new JScalar("newblankrwscalar") };
                    }
                    frags.Add(new object[] { new JScalar("scopedlex"),
                        new JScalar(kv.Key), bit });
                }
            }
        }

        CpsOp FillParamRole() {
            ParametricRole pr = (ParametricRole)
                Xref.from((object[])sub.parametric_role_hack).Resolve();
            CpsOp mo = CpsOp.PeekLet("!mo", Tokens.DynMetaObject);
            CpsOp to = CpsOp.PeekLet("!to", Tokens.DynObject);
            CpsOp pa = CpsOp.PeekLet("!pa", Tokens.VarHash);

            List<CpsOp> build = new List<CpsOp>();

            CpsOp[] supers = new CpsOp[pr.superclasses.Length];
            for (int i = 0; i < supers.Length; i++)
                supers[i] = CpsOp.GetSField(((Class)Xref.from((object[])pr.superclasses[i]).Resolve()).metaObject);

            build.Add( CpsOp.MethodCall(null, Tokens.DMO_FillRole, new CpsOp[] {
                mo, CpsOp.StringArray(false, JScalar.SA(0, pr.attributes)),
                CpsOp.NewArray(Tokens.DynMetaObject, supers),
                CpsOp.NewArray(Tokens.DynMetaObject, new CpsOp[0]) }) );

            foreach (object m in pr.methods) {
                object[] mx = (object[]) m;
                CpsOp name = (mx[0] is JScalar) ? CpsOp.StringLiteral(
                    JScalar.S(mx[0])) : Scan(new object[] { new JScalar("obj_getstr"), mx[0] });
                CpsOp var  = RawAccessLex("scopedlex", JScalar.S(mx[1]), null);
                MethodInfo a = JScalar.B(mx[2]) ?
                    Tokens.DMO_AddPrivateMethod : Tokens.DMO_AddMethod;

                build.Add(CpsOp.MethodCall(null, a, new CpsOp[] { mo, name,
                    CpsOp.MethodCall(null, Tokens.Variable_Fetch, new CpsOp[] { var }) }));
            }

            build.Add(CpsOp.MethodCall(null, Tokens.DMO_Invalidate, new CpsOp[] { mo }));
            if (sub.sig != null) {
                object[] rsig = (object[]) sub.sig;
                foreach (object se in rsig) {
                    string slot = JScalar.S( ((object[])se)[2] );
                    if (slot != null)
                        build.Add(CpsOp.MethodCall(null, Tokens.VarHash_set_Item, new CpsOp[] { pa, CpsOp.StringLiteral(slot), RawAccessLex("scopedlex", slot, null) }));
                }
            }

            build.Add(RawAccessLex("scopedlex", "*params", pa));
            build.Add(CpsOp.SetField(Tokens.DynObject_slots, to,
                        CpsOp.Null(typeof(object[]))));
            build.Add(CpsOp.SetField(Tokens.DMO_typeObject, mo, to));
            build.Add(CpsOp.CpsReturn(new CpsOp[] { CpsOp.MethodCall(null, Tokens.Kernel_NewROScalar, new CpsOp[] { to })}));

            return CpsOp.Let("!mo", CpsOp.ConstructorCall(Tokens.DMO_ctor,
                        new CpsOp[] { CpsOp.StringLiteral(pr.name) }),
                    CpsOp.Let("!to", CpsOp.ConstructorCall(Tokens.DynObject_ctor,
                            new CpsOp[] { mo }),
                        CpsOp.Let("!pa", CpsOp.ConstructorCall(Tokens.VarHash.GetConstructor(new Type[0]), new CpsOp[0]),
                            CpsOp.Sequence(build.ToArray()))));
        }

        object WrapBody() {
            object b = sub.body;
            List<object> enter = new List<object>();
            EnterCode(enter);

            // TODO: bind a ro container around return values
            if ((sub.flags & StaticSub.GATHER_HACK) != 0) {
                enter.Insert(0, new JScalar("prog"));
                enter.Add(new object[] { new JScalar("sink"), b });
                enter.Add(new object[] { new JScalar("sink"),
                        new object[] { new JScalar("take"),
                    new object[] { new JScalar("corelex"), new JScalar("EMPTY") } } });
                enter.Add(new object[] { new JScalar("return") });
                b = enter.ToArray();
            } else if (sub.augment_hack != null) {
                enter = new List<object>();
                object[] tuples = (object[]) sub.augment_hack;
                for (int i = 1; i < tuples.Length; i++) {
                    object[] t = (object[]) tuples[i];
                    enter.Add(new object[] {
                        new JScalar( ((JScalar)t[0]).str != "" ? "_addprivatemethod" : "_addmethod" ),
                        new object[] { new JScalar("letvar"), new JScalar("!mo") },
                        new object[] { new JScalar("str"), t[1] },
                        new object[] { new JScalar("fetch"), new object[] { new JScalar("scopedlex"), t[2] } } });
                }
                enter.Add(new object[] { new JScalar("_invalidate"), new object[] { new JScalar("letvar"), new JScalar("!mo") } });
                enter.Add(new object[] { new JScalar("return") });
                enter.Insert(0, new JScalar("prog"));
                object[] t0 = (object[]) tuples[0];
                b = new object[] { new JScalar("letn"), new JScalar("!mo"),
                    new object[] { new JScalar("class_ref"), new JScalar("mo"),
                        t0[0], t0[1], t0[2] },
                    enter.ToArray() };
            } else if (sub.parametric_role_hack != null) {
                enter.Insert(0, new JScalar("prog"));
                enter.Add(new object[] { new JScalar("sink"), b });
                enter.Add(new object[] { new JScalar("_parametricrole") });
                b = enter.ToArray();
            } else if ((sub.flags & StaticSub.RETURNABLE) != 0) {
                enter.Add(new object[] { new JScalar("return"),
                    new object[] { new JScalar("span"),
                        new JScalar("rstart"), new JScalar("rend"),
                        new JScalar("0"), b } });
                enter.Add(new object[] { new JScalar("ehspan"),
                    new JScalar("4"), new JScalar(""), new JScalar("0"),
                    new JScalar("rstart"), new JScalar("rend"), new JScalar("rend") });
                enter.Insert(0, new JScalar("prog"));
                b = enter.ToArray();
            } else {
                enter.Insert(0, new JScalar("prog"));
                enter.Add(new object[] { new JScalar("return"), b });
                b = enter.ToArray();
            }

            return b;
        }

        CpsOp Scan(object node) {
            object[] rnode = (object[]) node;
            string tag = ((JScalar)rnode[0]).str;
            Func<NamProcessor, object[], CpsOp> handler;
            if (!handlers.TryGetValue(tag, out handler))
                throw new Exception("Unhandled nam operator " + tag);
            return handler(this, rnode);
        }
    }

    public class CLRBackend {
        internal AssemblyBuilder ab;
        internal ModuleBuilder mob;
        internal TypeBuilder tb;

        internal Unit unit;

        internal int constants;
        internal List<CpsOp> thaw = new List<CpsOp>();

        public static int Verbose =
            Environment.GetEnvironmentVariable("NIECZA_CODEGEN_TRACE") != null ? 1 : 0;
        public static bool Verifiable =
            Environment.GetEnvironmentVariable("NIECZA_CODEGEN_VERIFIABLE") != null ? true : false;

        CLRBackend(string dir, string mobname, string filename) {
            AssemblyName an = new AssemblyName(mobname);
            ab = AppDomain.CurrentDomain.DefineDynamicAssembly(an,
                    AssemblyBuilderAccess.Save, dir);
            mob = ab.DefineDynamicModule(mobname, filename);

            tb = mob.DefineType(mobname, TypeAttributes.Public |
                    TypeAttributes.Sealed | TypeAttributes.Abstract |
                    TypeAttributes.Class | TypeAttributes.BeforeFieldInit);
        }

        void EncodeSignature(List<CpsOp> thaw, StaticSub obj) {
            if (obj.sig == null) return;
            List<int> sig_i   = new List<int>();
            List<CpsOp> sig_r = new List<CpsOp>();
            object[] rsig = (object[]) obj.sig;
            foreach (object p in rsig) {
                object[] param = (object[]) p;
                string   name  = JScalar.S(param[0]);
                int      flags = JScalar.I(param[1]);
                string   slot  = JScalar.S(param[2]);
                string[] names = JScalar.SA(0, param[3]);
                Xref     deflt = Xref.from(param[4] as object[]);

                sig_r.Add(CpsOp.StringLiteral(name));
                foreach (string n in names)
                    sig_r.Add(CpsOp.StringLiteral(n));
                int ufl = 0;
                if ((flags & 4) != 0) ufl |= SubInfo.SIG_F_RWTRANS;
                else if ((flags & 64) == 0) ufl |= SubInfo.SIG_F_READWRITE;

                if ((flags & 384) != 0) ufl |= SubInfo.SIG_F_BINDLIST;
                if (deflt != null) {
                    ufl |= SubInfo.SIG_F_HASDEFAULT;
                    sig_r.Add(CpsOp.GetSField(((StaticSub)deflt.Resolve()).subinfo));
                }
                if ((flags & 16) != 0) ufl |= SubInfo.SIG_F_OPTIONAL;
                if ((flags & 32) != 0) ufl |= SubInfo.SIG_F_POSITIONAL;
                if ((flags & 1) != 0 && (flags & 256) != 0)
                    ufl |= SubInfo.SIG_F_SLURPY_NAM;
                if ((flags & 1) != 0 && (flags & 256) == 0)
                    ufl |= SubInfo.SIG_F_SLURPY_POS;
                if ((flags & 2) != 0) ufl |= SubInfo.SIG_F_SLURPY_CAP;
                if ((flags & 8) != 0) ufl |= SubInfo.SIG_F_SLURPY_PCL;
                sig_i.Add(ufl);
                sig_i.Add(slot == null ? -1 : ((LexVarish)obj.l_lexicals[slot]).index);
                sig_i.Add(names.Length);
            }
            thaw.Add(CpsOp.SetField(Tokens.SubInfo_sig_i,
                CpsOp.GetSField(obj.subinfo), CpsOp.NewIntArray(sig_i.ToArray())));
            thaw.Add(CpsOp.SetField(Tokens.SubInfo_sig_r,
                CpsOp.GetSField(obj.subinfo), CpsOp.NewArray(typeof(object), sig_r.ToArray())));
        }

        void SetProtolex(StaticSub obj, string n, LexVarish v, CpsOp init) {
            if ((obj.flags & StaticSub.RUN_ONCE) != 0 && !Lexical.IsDynamicName(n))
                thaw.Add(CpsOp.SetSField(v.stg, init));
            else
                thaw.Add(CpsOp.SetProtoPad(v.index,
                    CpsOp.GetSField(obj.protopad), init));
        }

        void Process(Unit unit, bool asmain) {
            this.unit = unit;

            unit.BindFields(delegate(string name, Type type) {
                return tb.DefineField(name, type, FieldAttributes.Public |
                    FieldAttributes.Static);
            });

            foreach (object o in unit.tdeps) {
                string dp = JScalar.S(((object[])o)[0]);
                if (dp == unit.name) continue;
                thaw.Add(CpsOp.MethodCall(null, Tokens.Kernel_BootModule, new CpsOp[] {
                    CpsOp.StringLiteral(dp), CpsOp.DBDLiteral(CLRBackend.GetUnit(dp).clrType.GetMethod("BOOT")) }));
            }

            NamProcessor[] aux = new NamProcessor[unit.xref.Length];
            unit.VisitSubsPreorder(delegate(int ix, StaticSub obj) {
                if (Verbose > 0) Console.WriteLine("sub1 {0}", obj.name);
                CpsBuilder cpb = new CpsBuilder(this,
                    Unit.SharedName('C', ix, obj.name), false);
                NamProcessor np = aux[ix] = new NamProcessor(cpb, obj);
                np.MakeBody();
            });

            foreach (object le in unit.log) {
                object[] lea = (object[]) le;
                string t = ((JScalar)lea[0]).str;
                if (t == "pkg" || t == "var") {
                    CpsOp[] sa = new CpsOp[] { CpsOp.StringArray(false, JScalar.SA(0, lea[1])) };
                    if (t == "pkg") {
                        thaw.Add(CpsOp.MethodCall(null, Tokens.Kernel_CreatePath, sa));
                    } else {
                        thaw.Add(CpsOp.Sink(CpsOp.MethodCall(null, Tokens.Kernel_GetVar, sa)));
                    }
                }
            }

            unit.VisitPackages(delegate(int ix, Package pkg) {
                if (Verbose > 0) Console.WriteLine("pkg2 {0}", pkg.name);
                if (!(pkg is ModuleWithTypeObject))
                    return;
                ModuleWithTypeObject m = (ModuleWithTypeObject) pkg;
                FieldInfo km = null;
                FieldInfo kp = null;
                bool existing_mo = false;
                if (unit.name == "SAFE" || unit.name == "CORE") {
                    km = Tokens.Kernel.GetField(m.name + "MO");
                    kp = Tokens.Kernel.GetField(m.name + "P");
                    existing_mo = km != null && km.IsInitOnly;
                }
                thaw.Add(CpsOp.SetSField(m.metaObject, existing_mo ?
                        CpsOp.GetSField(km) :
                        CpsOp.ConstructorCall(Tokens.DMO_ctor,
                            new CpsOp[] { CpsOp.StringLiteral(m.name) })));

                if (m is Role) {
                    Role r = (Role) m;
                    CpsOp[] super = new CpsOp[ r.superclasses.Length ];
                    for (int i = 0; i < super.Length; i++)
                        super[i] = CpsOp.GetSField( ((Class)Xref.from((object[])r.superclasses[i]).Resolve()).metaObject );

                    thaw.Add(CpsOp.MethodCall(null, Tokens.DMO_FillRole, new CpsOp[] {
                        CpsOp.GetSField(r.metaObject),
                        CpsOp.StringArray(false, r.attributes),
                        CpsOp.NewArray(Tokens.DynMetaObject, super),
                        CpsOp.NewArray(Tokens.DynMetaObject, new CpsOp[0]) }));
                } else if (m is ParametricRole) {
                    // The heavy lifting is done in WrapBody
                } else if (m is Class) {
                    Class r = (Class) m;
                    List<string> all_attr = new List<string>();
                    CpsOp[] super = new CpsOp[ r.superclasses.Length ];
                    CpsOp[] mro   = new CpsOp[ r.linearized_mro.Length ];
                    for (int i = 0; i < super.Length; i++)
                        super[i] = CpsOp.GetSField( ((Class)Xref.from((object[])r.superclasses[i]).Resolve()).metaObject );
                    for (int i = 0; i < mro.Length; i++) {
                        Class p = (Class) Xref.from((object[]) r.linearized_mro[i]).Resolve();
                        mro[i] = CpsOp.GetSField(p.metaObject);
                        foreach (string a in p.attributes)
                            all_attr.Add(a);
                    }

                    thaw.Add(CpsOp.MethodCall(null, Tokens.DMO_FillClass, new CpsOp[] {
                        CpsOp.GetSField(r.metaObject),
                        CpsOp.StringArray(false, r.attributes),
                        CpsOp.StringArray(false, all_attr.ToArray()),
                        CpsOp.NewArray(Tokens.DynMetaObject, super),
                        CpsOp.NewArray(Tokens.DynMetaObject, mro) }));
                }

                thaw.Add(CpsOp.SetSField(m.typeObject,
                    CpsOp.ConstructorCall(Tokens.DynObject_ctor, new CpsOp[] {
                        CpsOp.GetSField(m.metaObject) })));
                thaw.Add(CpsOp.SetField(Tokens.DynObject_slots,
                    CpsOp.UnboxAny(Tokens.DynObject, CpsOp.GetSField(m.typeObject)),
                        CpsOp.Null(typeof(object[]))));
                thaw.Add(CpsOp.SetField(Tokens.DMO_typeObject,
                    CpsOp.GetSField(m.metaObject), CpsOp.GetSField(m.typeObject)));
                thaw.Add(CpsOp.SetSField(m.typeVar, CpsOp.MethodCall(null,
                    Tokens.Kernel_NewROScalar, new CpsOp[] {
                        CpsOp.GetSField(m.typeObject) })));

                if (kp != null)
                    thaw.Add(CpsOp.SetSField(kp, CpsOp.GetSField(m.typeObject)));
                if (km != null && !km.IsInitOnly)
                    thaw.Add(CpsOp.SetSField(km, CpsOp.GetSField(m.metaObject)));
            });

            unit.VisitSubsPreorder(delegate(int ix, StaticSub obj) {
                if (Verbose > 0) Console.WriteLine("sub2 {0}", obj.name);
                thaw.Add(CpsOp.SetSField(obj.subinfo, aux[ix].SubInfoCtor()));
                if (obj.sclass != "Sub") {
                    Class c = (Class) obj.GetCorePackage(obj.sclass);
                    thaw.Add(CpsOp.SetField(Tokens.SubInfo_mo,
                        CpsOp.GetSField(obj.subinfo),
                        CpsOp.GetSField(c.metaObject)));
                }

                if (obj.protopad != null) {
                    thaw.Add(CpsOp.SetSField(obj.protopad,
                        CpsOp.ConstructorCall(Tokens.Frame_ctor, new CpsOp[] {
                            CpsOp.Null(Tokens.Frame),
                            (obj.outer == null ? CpsOp.Null(Tokens.Frame) :
                                CpsOp.GetSField(((StaticSub)obj.outer.Resolve()).protopad)),
                            CpsOp.GetSField(obj.subinfo) })));
                }

                if (obj.protosub != null) {
                    thaw.Add(CpsOp.SetSField(obj.protosub,
                        CpsOp.MethodCall(null, Tokens.Kernel_MakeSub, new CpsOp[] {
                            CpsOp.GetSField(obj.subinfo),
                            (obj.outer == null ? CpsOp.Null(Tokens.Frame) :
                                CpsOp.GetSField(((StaticSub)obj.outer.Resolve()).protopad))
                            })));

                    if (obj.parametric_role_hack != null) {
                        ParametricRole pr = (ParametricRole)
                            (new Xref((object[])obj.parametric_role_hack)).Resolve();
                        thaw.Add(CpsOp.MethodCall(null,Tokens.DMO_FillParametricRole,
                            new CpsOp[] { CpsOp.GetSField(pr.metaObject),
                                CpsOp.GetSField(obj.protosub) }));
                    }
                }
            });

            unit.VisitPackages(delegate(int ix, Package p) {
                if (Verbose > 0) Console.WriteLine("pkg3 {0}", p.name);
                ModuleWithTypeObject m = p as ModuleWithTypeObject;
                if (m == null) return;
                foreach (object o in m.exports) {
                    thaw.Add(CpsOp.SetField(Tokens.BValue_v,
                        CpsOp.MethodCall(null, Tokens.Kernel_GetVar,
                            new CpsOp[] { CpsOp.StringArray(false, JScalar.SA(0,o)) }),
                        CpsOp.GetSField(m.typeVar)));
                }
                if (m is ParametricRole) return;
                object[] methods = (m is Class) ? ((Class)m).methods :
                    ((Role)m).methods;
                foreach (object me in methods) {
                    object[] mes = (object[]) me;
                    MethodInfo mi = ((JScalar)mes[1]).num != 0 ?
                        Tokens.DMO_AddPrivateMethod : Tokens.DMO_AddMethod;
                    thaw.Add(CpsOp.MethodCall(null, mi, new CpsOp[] {
                        CpsOp.GetSField(m.metaObject),
                        CpsOp.StringLiteral(((JScalar)mes[0]).str),
                        CpsOp.GetSField(((StaticSub)Xref.from((object[])mes[2]).Resolve()).protosub) }));
                }
                thaw.Add(CpsOp.MethodCall(null, Tokens.DMO_Invalidate,
                    new CpsOp [] { CpsOp.GetSField(m.metaObject) }));
                thaw.Add(CpsOp.SetField(Tokens.DMO_how, CpsOp.GetSField(m.metaObject),
                    CpsOp.MethodCall(null, Tokens.Kernel.GetMethod("BoxRaw").MakeGenericMethod(Tokens.DynMetaObject), new CpsOp[] { CpsOp.GetSField(m.metaObject), CpsOp.GetSField( ((Class) unit.GetCorePackage("ClassHOW")).metaObject ) })));
            });

            unit.VisitSubsPostorder(delegate(int ix, StaticSub obj) {
                if (Verbose > 0) Console.WriteLine("sub3 {0}", obj.name);
                EncodeSignature(thaw, obj);

                if (obj.is_phaser >= 0)
                    thaw.Add(CpsOp.MethodCall(null, Tokens.Kernel_AddPhaser,
                        new CpsOp[] { CpsOp.IntLiteral(obj.is_phaser),
                        CpsOp.GetSField(obj.protosub) }));

                if (obj.exports != null) {
                    foreach (object o in obj.exports) {
                        thaw.Add(CpsOp.SetField(Tokens.BValue_v,
                            CpsOp.MethodCall(null, Tokens.Kernel_GetVar,
                                new CpsOp[] { CpsOp.StringArray(false, JScalar.SA(0,o)) }),
                            CpsOp.MethodCall(null, Tokens.Kernel_NewROScalar,
                                new CpsOp[] { CpsOp.GetSField(obj.protosub) })));
                    }
                }

                foreach (KeyValuePair<string,Lexical> l in obj.lexicals) {
                    if (l.Value is LexCommon) {
                        LexCommon lx = (LexCommon)l.Value; /* XXX cname */
                        thaw.Add(CpsOp.SetSField(lx.stg,
                            CpsOp.MethodCall(null, Tokens.Kernel_GetVar, new CpsOp[] {
                                CpsOp.StringArray(false, lx.path) })));
                    } else if (l.Value is LexSub) {
                        LexSub lx = (LexSub)l.Value;
                        if ((obj.flags & StaticSub.SPAD_EXISTS) == 0) continue;
                        SetProtolex(obj, l.Key, lx, CpsOp.MethodCall(null,
                            Tokens.Kernel_NewROScalar, new CpsOp[] {
                                CpsOp.GetSField(((StaticSub)lx.def.Resolve()).protosub) }));
                    } else if (l.Value is LexSimple) {
                        LexSimple lx = (LexSimple)l.Value;
                        if ((obj.flags & StaticSub.SPAD_EXISTS) == 0) continue;
                        string type = ((lx.flags & LexSimple.HASH) != 0) ? "Hash" :
                            ((lx.flags & LexSimple.LIST) != 0) ? "Array" : null;
                        if (type != null) {
                            Class c = (Class) obj.GetCorePackage(type);
                            SetProtolex(obj, l.Key, lx, CpsOp.SubyCall("new", "",
                                new CpsOp[] {
                                    CpsOp.GetSField(c.typeObject),
                                    CpsOp.GetSField(c.typeVar) }));
                        } else {
                            SetProtolex(obj, l.Key, lx, CpsOp.MethodCall(null,
                                Tokens.Kernel_NewRWScalar, new CpsOp[] {
                                    CpsOp.GetSField(Tokens.Kernel_AnyMO),
                                    CpsOp.GetSField(Tokens.Kernel_AnyP) }));
                        }
                    }
                }
            });

            if (asmain)
                thaw.Add(CpsOp.MethodCall(null, Tokens.Kernel_FirePhasers,
                    new CpsOp[] { CpsOp.IntLiteral(0), CpsOp.BoolLiteral(false) }));
            // settings are incomplete modules and have no mainline to run
            if (unit.bottom_ref == null) {
                Type dty = typeof(Dictionary<string,Object>);
                FieldInfo lex = Tokens.Frame.GetField("lex");
                MethodInfo set = dty.GetMethod("set_Item");
                thaw.Add(CpsOp.SetField(lex, CpsOp.CallFrame(),
                    CpsOp.ConstructorCall(dty.GetConstructor(new Type[0]), new CpsOp[0])));
                string s = unit.setting;
                StaticSub m = (StaticSub) unit.mainline_ref.Resolve();
                while (s != null) {
                    thaw.Add(CpsOp.MethodCall(null, set, new CpsOp[] {
                        CpsOp.GetField(lex, CpsOp.CallFrame()),
                        CpsOp.StringLiteral("*resume_" + s),
                        CpsOp.MethodCall(null, Tokens.Kernel_NewROScalar, new CpsOp[] {
                            CpsOp.GetSField(m.protosub) }) }));
                    Unit su = CLRBackend.GetUnit(s);
                    s = su.setting;
                    m = (StaticSub) su.mainline_ref.Resolve();
                }
                thaw.Add(CpsOp.Sink(CpsOp.SubyCall(null, "",
                    new CpsOp[] { CpsOp.GetSField(m.protosub) })));
            }

            CpsBuilder boot = new CpsBuilder(this, "BOOT", true);
            thaw.Add(CpsOp.CpsReturn(new CpsOp[0]));
            boot.Build(CpsOp.Sequence(thaw.ToArray()));

            if (asmain)
                DefineMainMethod(boot.mb);
        }

        void Finish(string filename) {
            tb.CreateType();

            ab.Save(filename);
        }

        void DefineMainMethod(MethodInfo boot) {
            MethodBuilder mb = tb.DefineMethod("Main", MethodAttributes.Static |
                    MethodAttributes.Public, typeof(void),
                    new Type[] { typeof(string[]) });
            ILGenerator il = mb.GetILGenerator();

            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Ldnull);
            il.Emit(OpCodes.Ldftn, boot);
            il.Emit(OpCodes.Newobj, Tokens.DynBlockDelegate_ctor);
            il.Emit(OpCodes.Call, Tokens.Kernel_RunLoop);
            il.Emit(OpCodes.Ret);

            ab.SetEntryPoint(mb);
        }

        [ThreadStatic] static Dictionary<string, Unit> used_units;
        internal static object Resolve(Xref x) {
            return used_units[x.unit].xref[x.index];
        }
        internal static Unit GetUnit(string name) { return used_units[name]; }

        public static void Main(string[] args) {
            if (args.Length != 4) {
                Console.Error.WriteLine("usage : CLRBackend DIR UNITFILE OUTFILE ISMAIN");
                return;
            }
            string dir      = args[0];
            string unitfile = args[1];
            string outfile  = args[2];
            bool   ismain   = args[3] == "1";
            Directory.SetCurrentDirectory(dir);
            string tx = File.ReadAllText(unitfile);
            Unit root = new Unit((object[])Reader.Read(tx));
            CLRBackend c = new CLRBackend(null, root.name, outfile);

            used_units = new Dictionary<string, Unit>();
            used_units[root.name] = root;

            foreach (object x in root.tdeps) {
                object[] dn = (object[]) x;
                string name = JScalar.S(dn[0]);
                if (name == root.name) continue;
                string dtx = File.ReadAllText(name + ".nam");
                used_units[name] = new Unit((object[])Reader.Read(dtx));
            }

            foreach (Unit u in used_units.Values) {
                u.BindDepends();
                if (u != root) {
                    u.clrAssembly = Assembly.Load(u.name);
                    u.clrType = u.clrAssembly.GetType(u.name);
                    Dictionary<string,FieldInfo> df =
                        new Dictionary<string,FieldInfo>();
                    foreach (FieldInfo fi in u.clrType.GetFields())
                        df[fi.Name] = fi;
                    u.BindFields(delegate(string fn, Type t) {
                        return df[fn];
                    });
                }
            }

            c.Process(root, ismain);

            c.Finish(outfile);
        }
    }
}
