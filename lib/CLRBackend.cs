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
        public override string ToString() { return text; }
    }

    class Reader {
        static char GetHexQuad(string s, int ix) {
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

        public static object Read(string input) {
            int ix = 0;
            List<List<object>> containers = new List<List<object>>();
            char i;
            StringBuilder sb;
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
                if (i == 'n' && input.Length >= ix + 4 &&
                        input[ix+1] == 'u' && input[ix+2] == 'l' &&
                        input[ix+3] == 'l') {
                    containers[containers.Count - 1].Add(null);
                    ix += 4;
                    continue;
                }
                if (i == '"') {
                    sb = new StringBuilder();
                    ix++;
                    while (true) {
                        i = input[ix];
                        if (i == '\\') {
                            switch (input[ix+1]) {
                                case '/': i = '/'; break;
                                case '\\': i = '\\'; break;
                                case 't': i = '\t'; break;
                                case 'r': i = '\r'; break;
                                case 'n': i = '\n'; break;
                                case 'f': i = '\f'; break;
                                case 'b': i = '\b'; break;
                                case 'u': i = GetHexQuad(input, ix+2); ix += 4; break;
                            }
                            ix += 2;
                            sb.Append(i);
                        } else if (i == '"') {
                            break;
                        } else {
                            sb.Append(i);
                            ix++;
                        }
                    }
                    ix++;
                    containers[containers.Count - 1].Add(new JScalar(sb.ToString()));
                    continue;
                }
                sb = new StringBuilder();
                while (true) {
                    i = input[ix];
                    if (i == ',' || i == '\r' || i == '\t' || i == '\n' ||
                            i == ' ' || i == ']')
                        break;
                    sb.Append(i);
                    ix++;
                }
                containers[containers.Count - 1].Add(new JScalar(sb.ToString()));
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
        public readonly object attributes;
        public readonly object methods;
        public readonly object superclasses;
        public readonly object linearized_mro;
        public Class(object[] p) : base(p) {
            attributes = p[3];
            methods = p[4];
            superclasses = p[5];
            linearized_mro = p[6];
        }
    }

    class Grammar: Class {
        public Grammar(object[] p) : base(p) { }
    }

    class Role: ModuleWithTypeObject {
        public readonly object attributes;
        public readonly object methods;
        public readonly object superclasses;
        public Role(object[] p) : base(p) {
            attributes = p[3];
            methods = p[4];
            superclasses = p[5];
        }
    }

    class ParametricRole: ModuleWithTypeObject {
        public readonly object attributes;
        public readonly object methods;
        public readonly object superclasses;
        public ParametricRole(object[] p) : base(p) {
            attributes = p[3];
            methods = p[4];
            superclasses = p[5];
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
                if (lexicals[i].Value != null) // XXX
                    lexicals[i].Value.BindFields(ix, i, this,
                            lexicals[i].Key, binder);
        }
    }

    abstract class Lexical {
        public virtual void BindFields(int six, int lix, StaticSub sub,
                string name, Func<string,Type,FieldInfo> binder) { }
        public virtual ClrOp SetCode(int up, ClrOp head) {
            throw new Exception("Lexicals of type " + this + " cannot be bound");
        }
        public abstract ClrOp GetCode(int up);
        protected static bool IsDynamicName(string name) {
            if (name == "$_") return true;
            if (name.Length < 2) return false;
            if (name[0] == '*' || name[0] == '?') return true;
            if (name[1] == '*' || name[1] == '?') return true;
            return false;
        }
    }

    class LexSimple : Lexical {
        public const int NOINIT = 4;
        public const int LIST = 2;
        public const int HASH = 1;
        public readonly int flags;

        public int index;
        public FieldInfo stg;

        public LexSimple(object[] l) {
            flags = (int)((JScalar)l[2]).num;
        }

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

    class LexSub : Lexical {
        public readonly Xref def;
        public LexSub(object[] l) {
            def = new Xref(l, 2);
        }

        public int index;
        public FieldInfo stg;

        public override ClrOp GetCode(int up) {
            return (index >= 0) ? (ClrOp)new ClrPadGet(up, index)
                                : new ClrGetSField(stg);
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
            ModuleWithTypeObject p = (ModuleWithTypeObject) GetPackage();
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
        public int next_case;
        public Label[] cases;
        public int num_cases;
        public Dictionary<string,int> named_cases
            = new Dictionary<string,int>();
        public Dictionary<string,Label> named_labels
            = new Dictionary<string,Label>();
        public string[] let_names = new string[0];
        public Type[] let_types = new Type[0];
        public LocalBuilder ospill;

        public void make_ospill() {
            if (ospill == null)
                ospill = il.DeclareLocal(Tokens.Variable);
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

            if (ix >= Tokens.NumInt32) {
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
        public static readonly Type DynMetaObject = typeof(DynMetaObject);
        public static readonly Type VarHash = typeof(Dictionary<string,Variable>);
        public static readonly Type VVarList = typeof(VarDeque);
        public static readonly Type FVarList = typeof(Variable[]);
        public static readonly Type Cursor = typeof(Cursor);
        public static readonly Type CC = typeof(CC);
        public static readonly Type LAD = typeof(LAD);

        public static readonly ConstructorInfo DynBlockDelegate_ctor =
            typeof(DynBlockDelegate).GetConstructor(new Type[] {
                    typeof(object), typeof(IntPtr) });
        public static readonly ConstructorInfo DynObject_ctor =
            typeof(DynObject).GetConstructor(new Type[] {
                    DynMetaObject });
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
        public static readonly MethodInfo Kernel_Die =
            typeof(Kernel).GetMethod("Die");
        public static readonly MethodInfo Kernel_RunLoop =
            typeof(Kernel).GetMethod("RunLoop");
        public static readonly MethodInfo Kernel_NewROScalar =
            typeof(Kernel).GetMethod("NewROScalar");
        public static readonly MethodInfo Kernel_NewRWScalar =
            typeof(Kernel).GetMethod("NewRWScalar");
        public static readonly MethodInfo Kernel_NewBoundVar =
            typeof(Kernel).GetMethod("NewBoundVar");
        public static readonly MethodInfo Kernel_IterHasFlat =
            typeof(Kernel).GetMethod("IterHasFlat");
        public static readonly MethodInfo Console_WriteLine =
            typeof(Console).GetMethod("WriteLine", new Type[] { typeof(string) });
        public static readonly MethodInfo Console_Write =
            typeof(Console).GetMethod("Write", new Type[] { typeof(string) });
        public static readonly MethodInfo Environment_Exit =
            typeof(Console).GetMethod("Exit");
        public static readonly MethodInfo Object_ToString =
            typeof(object).GetMethod("ToString", new Type[0]);

        public static readonly FieldInfo IP6_mo =
            IP6.GetField("mo");
        public static readonly FieldInfo BValue_v =
            BValue.GetField("v");
        public static readonly FieldInfo Kernel_AnyMO =
            Kernel.GetField("AnyMO");
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

        protected static void TypeCheck(Type sub, Type super, string msg) {
            if (!super.IsAssignableFrom(sub))
                throw new Exception(msg + " " + sub + " not subtype of " + super);
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
            foreach (ClrOp o in Zyg) {
                if (HasCases && i == (Method.IsStatic ? 0 : 1)) {
                    // this needs to come AFTER the invocant
                    cx.il.Emit(OpCodes.Ldarg_0);
                }
                o.CodeGen(cx);

                // XXX this doesn't work quite right if the method is
                // defined on the value type itself
                if (i == 0 && o.Returns.IsValueType && !Method.IsStatic)
                    cx.il.Emit(OpCodes.Box, o.Returns);
            }
            cx.il.Emit((Method.IsStatic ? OpCodes.Call : OpCodes.Callvirt),
                    Method); // XXX C#
            if (HasCases) {
                cx.il.Emit(OpCodes.Ret);
                cx.il.MarkLabel(cx.cases[cx.next_case++]);
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

            bool skip = cps;
            foreach (ParameterInfo pi in mi.GetParameters()) {
                if (skip) { skip = false; continue; }
                ts.Add(pi.ParameterType);
            }

            if (zyg.Length != ts.Count)
                throw new Exception("argument list length mismatch for " + mi +
                        " got " + zyg.Length + " need " + ts.Count);

            for (int i = 0; i < ts.Count; i++) {
                TypeCheck(zyg[i].Returns, ts[i], "arg" + i + " of " + mi);
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
                TypeCheck(zyg[i].Returns, ts[i], "arg" + i + " of " + mi);
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
            cx.EmitGetlex(index, Tokens.Variable);
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
            cx.EmitPreSetlex(index);
            zyg.CodeGen(cx);
            cx.EmitSetlex(index, Tokens.Variable);
        }

        public ClrPadSet(int up, int index, ClrOp zyg) {
            Returns = Tokens.Void;
            this.zyg = zyg;
            this.up = up;
            this.index = index;
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
            int sp = 0;
            int ct = 0;
            for (int i = 1; i < zyg.Length; i++) {
                if (sig[sp++] != '\0')
                    throw new NotImplementedException();
                ct++;
            }
            cx.EmitInt(ct);
            cx.il.Emit(OpCodes.Newarr, Tokens.Variable);
            for (int i = 1; i < zyg.Length; i++) {
                cx.il.Emit(OpCodes.Dup);
                cx.EmitInt(i - 1);
                zyg[i].CodeGen(cx);
                cx.il.Emit(OpCodes.Stelem_Ref);
            }
            cx.il.Emit(OpCodes.Ldnull);
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
        }

        public override void ListCases(CgContext cx) {
            cx.num_cases++;
        }

        public ClrSubyCall(string mname, string sig, ClrOp[] zyg) {
            if (mname != null) sig = "\0" + sig;
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
            if (case_too)
                cx.il.MarkLabel(cx.cases[cx.named_cases[name]]);
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
                    throw new Exception("Non-void expression used in nonfinal sequence position");
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

        public static CpsOp IntLiteral(int x) {
            return new CpsOp(new ClrIntLiteral(Tokens.Int32, x));
        }

        public static CpsOp BoolLiteral(bool x) {
            return new CpsOp(new ClrIntLiteral(Tokens.Boolean, x ? 1 : 0));
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
                if (heads[0].Returns != Tokens.Int32 &&
                        heads[0].Returns != Tokens.Double)
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
            return body; // TODO: implement me
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

        public static CpsOp Sink(CpsOp zyg) {
            return new CpsOp(zyg.stmts, zyg.head.Sink());
        }

        public static CpsOp Null(Type ty) {
            return new CpsOp(new ClrNullLiteral(ty));
        }
    }

    class NamProcessor {
        StaticSub sub;
        Dictionary<string, Type> let_types = new Dictionary<string, Type>();

        public NamProcessor(StaticSub sub) {
            this.sub = sub;
        }

        CpsOp AccessLex(object[] zyg) {
            string type = ((JScalar)zyg[0]).str;
            string name = ((JScalar)zyg[1]).str;
            bool core = (type == "corelex");
            bool letonly = (type == "letvar");
            CpsOp set_to = (zyg.Length > 2) ? Scan(zyg[2]) : null;

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
            namtypes["fvarlist"] = typeof(Variable[]);
            namtypes["vvarlist"] = typeof(VarDeque);
            namtypes["varhash"] = typeof(Dictionary<string,Variable>);
            namtypes["frame"] = Tokens.Frame;
            namtypes["cursor"] = typeof(Cursor);

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
            handlers["double"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.DoubleLiteral(((JScalar)zyg[1]).num); };
            handlers["bool"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.BoolLiteral(((JScalar)zyg[1]).num != 0); };
            handlers["ann"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.Annotate((int) ((JScalar)zyg[2]).num, th.Scan(zyg[3])); };
            handlers["compare"] = handlers["arith"] =
                delegate(NamProcessor th, object[] zyg) {
                    return CpsOp.PolyOp(FixStr(zyg[1]),
                            th.Scan(zyg[2]), th.Scan(zyg[3])); };
            handlers["setslot"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.MethodCall(null, Tokens.IP6_SetSlot, new CpsOp[] {
                    th.Scan(zyg[2]), th.AnyStr(zyg[1]), th.Scan(zyg[3]) }); };
            handlers["box"] = delegate(NamProcessor th, object[] zyg) {
                CpsOp mo;
                if (zyg[1] is JScalar) {
                    string name = ((JScalar)zyg[1]).str;
                    int uplevel;
                    LexStash lx = (LexStash)th.ResolveLex(name, out uplevel, true);
                    Class p = (Class)th.sub.unit.GetPackage(lx.path, 0, lx.path.Length);
                    mo = new CpsOp(new ClrGetSField(p.metaObject));
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
                bool ro   = ((JScalar)zyg[1]).num != 0;
                bool list = ((JScalar)zyg[2]).num != 0;
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
            handlers["class_ref"] = delegate(NamProcessor th, object[] z) {
                string kind = FixStr(z[1]);
                ModuleWithTypeObject m;
                if (z.Length == 3) {
                    int uplevel;
                    LexStash ls = (LexStash)th.ResolveLex(FixStr(z[2]),
                            out uplevel, true);
                    m = (ModuleWithTypeObject)ls.GetPackage();
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

            // TODO: implement
            thandlers["const"] = delegate(CpsOp[] z) { return z[0]; };
            thandlers["ternary"] = delegate(CpsOp[] z) {
                return CpsOp.Ternary(z[0], z[1], z[2]); };
            thandlers["sink"] = delegate(CpsOp[] z) {
                return CpsOp.Sink(z[0]); };
            // yuck.
            thandlers["fvarlist_length"] = delegate(CpsOp[] z) {
                return CpsOp.Operator(Tokens.Int32, OpCodes.Conv_I4,
                    new CpsOp[] { CpsOp.Operator(Tokens.IntPtr, OpCodes.Ldlen,
                        z) });
            };
            // XXX - wrong order - problem?
            thandlers["fvarlist_item"] = delegate(CpsOp[] z) {
                return CpsOp.Operator(Tokens.Variable, OpCodes.Ldelem_Ref,
                    new CpsOp[] { z[1], z[0] }); };
            thandlers["newscalar"] = Methody(null, Tokens.Kernel_NewROScalar);
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

            thandlers["var_islist"] = FieldGet(Tokens.Variable, "islist");
            thandlers["llhow_name"] = FieldGet(Tokens.DynMetaObject, "name");
            thandlers["stab_what"] = FieldGet(Tokens.DynMetaObject, "typeObject");
            thandlers["obj_llhow"] = FieldGet(Tokens.IP6, "mo");
            thandlers["varhash_clear"] = Methody(null, Tokens.VarHash.GetMethod("Clear"));
            thandlers["num_to_string"] = Methody(null, Tokens.Object_ToString);
            thandlers["str_length"] = Methody(null, Tokens.String.GetMethod("get_Length"));
            thandlers["str_substring"] = Methody(null, Tokens.Builtins.GetMethod("LaxSubstring2"));
            thandlers["str_tolower"] = Methody(null, Tokens.String.GetMethod("ToLowerInvariant"));
            thandlers["str_toupper"] = Methody(null, Tokens.String.GetMethod("ToUpperInvariant"));
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
            thandlers["obj_is_defined"] = Methody(null, Tokens.IP6.GetMethod("IsDefined"));
            thandlers["obj_what"] = Methody(null, Tokens.IP6.GetMethod("GetTypeObject"));
            thandlers["obj_isa"] = Methody(null, Tokens.IP6.GetMethod("Isa"));
            thandlers["obj_does"] = Methody(null, Tokens.IP6.GetMethod("Does"));
            thandlers["obj_newblank"] = Constructy(Tokens.DynObject_ctor);

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
            return delegate(CpsOp[] cpses) {
                return CpsOp.MethodCall(cps, mi, cpses); };
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

        public CpsOp MakeBody() { return Scan(WrapBody()); }

        object WrapBody() {
            // XXX returnable, enter code, etc etc
            return sub.body;
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
        AssemblyBuilder ab;
        ModuleBuilder mob;
        TypeBuilder tb;

        Unit unit;

        CLRBackend(string dir, string mobname, string filename) {
            AssemblyName an = new AssemblyName(mobname);
            ab = AppDomain.CurrentDomain.DefineDynamicAssembly(an,
                    AssemblyBuilderAccess.Save, dir);
            mob = ab.DefineDynamicModule(mobname, filename);

            tb = mob.DefineType(mobname, TypeAttributes.Public |
                    TypeAttributes.Sealed | TypeAttributes.Abstract |
                    TypeAttributes.Class | TypeAttributes.BeforeFieldInit);
        }

        void Process(Unit unit) {
            this.unit = unit;

            unit.BindFields(delegate(string name, Type type) {
                return tb.DefineField(name, type, FieldAttributes.Public |
                    FieldAttributes.Static);
            });

            unit.VisitSubsPostorder(delegate(int ix, StaticSub obj) {
                Console.WriteLine(obj.name);
                NamProcessor np = new NamProcessor(obj);
                DefineCpsMethod(
                    Unit.SharedName('C', ix, obj.name), false,
                    np.MakeBody());
            });

            unit.VisitSubsPostorder(delegate(int ix, StaticSub obj) {
                // TODO append chunks to Thaw here for sub2 stuff
            });

            unit.VisitSubsPostorder(delegate(int ix, StaticSub obj) {
                // TODO append chunks to Thaw here for sub3 stuff
            });

            // TODO generate BOOT method here
        }

        void Finish(string filename) {
            tb.CreateType();

            ab.Save(filename);
        }

        MethodInfo DefineCpsMethod(string name, bool pub, CpsOp body) {
            MethodBuilder mb = tb.DefineMethod(name, MethodAttributes.Static |
                    (pub ? MethodAttributes.Public : 0),
                    typeof(Frame), new Type[] { typeof(Frame) });
            CgContext cx = new CgContext();
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
            foreach (ClrOp s in body.stmts)
                s.CodeGen(cx);
            body.head.CodeGen(cx);

            return mb;
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

        public static void Main() {
            Directory.SetCurrentDirectory("obj");
            CLRBackend c = new CLRBackend(null, "SAFE", "SAFE.dll");

            string tx = File.ReadAllText("SAFE.nam");
            Unit root = new Unit((object[])Reader.Read(tx));
            used_units = new Dictionary<string, Unit>();
            used_units["SAFE"] = root;

            c.Process(root);

            c.Finish("SAFE.dll");
        }
    }
}
