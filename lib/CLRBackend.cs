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
    // The portable format is a subset of JSON, and is currently read
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
                val = Utils.S2N(text);
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
        public static T[] A<T>(int cut, object x, Func<object, T> rdr) {
            object[] arr = (object[]) x;
            T[] r = new T[ arr.Length - cut ];
            for (int i = 0; i < r.Length; i++)
                r[i] = rdr(arr[i+cut]);
            return r;
        }
        public static bool B(object x) {
            string s = S(x);
            if (s == "1")  return true;
            if (s == "0" || s == "") return false;
            throw new ArgumentException(s);
        }
        public static int I(object x) { return (int)((JScalar)x).num; }
        public static double N(object x) { return ((JScalar)x).num; }
        public static int IN(object x) { return x == null ? -1 : (int)((JScalar)x).num; }
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
        public readonly string filename;
        public readonly double modtime;
        public readonly object[] xref;
        public readonly object[] tdeps;

        public readonly Dictionary<string, Package> exp_pkg;
        public readonly Dictionary<string, int> tdep_to_id;
        public readonly List<Unit> id_to_tdep;
        public readonly Dictionary<string, CpsOp> const_pool;

        public Assembly clrAssembly;
        public Type clrType;
        bool depsBound;
        public FieldInfo rtunit;
        public List<byte> thaw_heap;
        public Dictionary<string,int> existing_strings;

        FieldInfo cc_pool;
        Dictionary<string,int> cc_constant_cache = new Dictionary<string,int>();
        List<int[]> cc_constants = new List<int[]>();

        public Unit(object[] from, object[] code) {
            mainline_ref = Xref.from(from[0]);
            name = JScalar.S(from[1]);
            log = from[2] as object[];
            setting = JScalar.S(from[3]);
            bottom_ref = Xref.from(from[4]);
            filename = JScalar.S(from[5]);
            modtime = from[6] == null ? 0 : JScalar.N(from[6]);
            xref = from[7] as object[];
            exp_pkg = new Dictionary<string,Package>();
            tdep_to_id = new Dictionary<string,int>();
            id_to_tdep = new List<Unit>();
            const_pool = new Dictionary<string,CpsOp>();
            thaw_heap = new List<byte>();
            existing_strings = new Dictionary<string,int>();
            for (int i = 0; i < xref.Length; i++) {
                if (xref[i] == null) continue;
                object[] xr = (object[]) xref[i];
                if (JScalar.S(xr[0]) == "sub") {
                    xref[i] = new StaticSub(this, xr, (code != null &&
                            i < code.Length) ? (object[])code[i] : null);
                } else {
                    Package p = Package.From(xr);
                    xref[i] = p;
                    p.own_xref = new Xref(name, i, p.name);
                    p.NoteExports(exp_pkg);
                }
            }
            tdeps = from[8] as object[];
        }

        public void BindDepends(bool ismain) {
            if (depsBound) return;
            depsBound = true;

            int i = 0;
            foreach (object x in tdeps) {
                string n = JScalar.S(((object[]) x)[0]);
                tdep_to_id[n] = i++;
                if (n == name) {
                    id_to_tdep.Add(this);
                    continue;
                }
                Unit o = CLRBackend.GetUnit(n);
                id_to_tdep.Add(o);
                o.BindDepends(false);
                foreach (KeyValuePair<string,Package> kv in o.exp_pkg)
                    exp_pkg[kv.Key] = kv.Value;
            }

            if (ismain) return;
            string dname = name.Replace("::",".");

            clrAssembly = Assembly.LoadFile(Path.Combine(
                        CLRBackend.Current.dir, dname + ".dll"));
            clrType = clrAssembly.GetType(dname);
            Dictionary<string,FieldInfo> df =
                new Dictionary<string,FieldInfo>();
            foreach (FieldInfo fi in clrType.GetFields())
                df[fi.Name] = fi;
            BindFields(delegate(string fn, Type t) {
                return df[fn];
            });
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
            StaticSub r = (bottom_ref ?? mainline_ref).Resolve<StaticSub>();
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
            rtunit = binder("UNIT", Tokens.RuntimeUnit);
            cc_pool = binder("CC", Tokens.CC.MakeArrayType());
            VisitSubsPostorder(delegate(int ix, StaticSub sub) {
                sub.BindFields(ix, binder);
            });
            VisitPackages(delegate(int ix, Package pkg) {
                pkg.BindFields(ix, binder);
            });
        }

        public void EmitInt(int x) {
            uint xp = (uint)x;
            thaw_heap.Add((byte)xp); xp >>= 8;
            thaw_heap.Add((byte)xp); xp >>= 8;
            thaw_heap.Add((byte)xp); xp >>= 8;
            thaw_heap.Add((byte)xp); xp >>= 8;
        }

        public void EmitXref(Xref x) {
            if (x == null) {
                EmitUShort(0xFFFF);
                return;
            }
            EmitUShort(tdep_to_id[x.unit]);
            EmitInt(x.index);
        }

        public void EmitByte(int x) {
            if (x < 0 || x >= 256)
                throw new ArgumentException();
            thaw_heap.Add((byte)x);
        }

        public void EmitUShort(int x) {
            if (x < 0 || x >= 65536)
                throw new ArgumentException();
            uint xp = (uint)x;
            thaw_heap.Add((byte)xp); xp >>= 8;
            thaw_heap.Add((byte)xp); xp >>= 8;
        }

        public void EmitStr(string s) {
            if (s == null) { EmitUShort(0xFFFF); return; }
            int l;
            if (existing_strings.TryGetValue(s, out l)) {
                if (l >= 0x3FFF) {
                    EmitInt(l * 4 + 1);
                } else {
                    EmitUShort(l * 4 + 3);
                }
            } else {
                existing_strings[s] = thaw_heap.Count;
                bool wide = false;
                foreach (char c in s) if (c > 0xFF) wide = true;
                l = s.Length * 8;
                if (wide) l+= 4;
                if (l >= 0xFFF0) {
                    EmitInt(l);
                } else {
                    EmitUShort(l + 2);
                }
                foreach(char c in s) {
                    if (wide) EmitUShort((int)c);
                    else EmitByte((int)c);
                }
            }
        }

        public void EmitIntArray(int[] s) {
            if (s == null) { EmitInt(-1); return; }
            EmitInt(s.Length);
            foreach(int c in s) EmitInt(c);
        }

        public void EmitStrArray(string[] s) {
            if (s == null) { EmitInt(-1); return; }
            EmitInt(s.Length);
            foreach(string c in s) EmitStr(c);
        }

        public void EmitLADArr(object lad) {
            object[] lada = (object[]) lad;
            EmitUShort(lada.Length);
            foreach(object o in lada) EmitLAD(o);
        }

        public void EmitLAD(object lad) {
            if (lad == null) {
                EmitByte(0);
                return;
            }
            object[] body = (object[]) lad;
            string head = JScalar.S(body[0]);

            if (!Tokens.LADcodes.ContainsKey(head))
                throw new ArgumentException(head);
            EmitByte(Tokens.LADcodes[head]);

            if (head == "CC") { EmitIntArray(JScalar.IA(1, body)); }
            else if (head == "Imp" || head == "Dot" || head == "Null" || head == "None" || head == "Dispatcher") { }
            else if (head == "Str" || head == "StrNoCase" || head == "Param" || head == "Method") { EmitStr(JScalar.S(body[1])); }
            else if (head == "Opt" || head == "Star" || head == "Plus") { EmitLAD(body[1]); }
            else if (head == "Sequence" || head == "Any") { EmitLADArr(body[1]); }
            else throw new NotImplementedException("ProcessLAD " + head);
        }

        public CpsOp CCConst(int[] cc) {
            StringBuilder code = new StringBuilder();
            foreach (int x in cc) {
                code.Append((char)x);
                code.Append((char)(x>>16));
            }
            string fcode = code.ToString();
            int ix;
            if (!cc_constant_cache.TryGetValue(fcode, out ix)) {
                cc_constant_cache[fcode] = ix = cc_constants.Count;
                cc_constants.Add(cc);
            }
            return CpsOp.Operator(Tokens.CC, OpCodes.Ldelem_Ref,
                CpsOp.GetSField(cc_pool), CpsOp.IntLiteral(ix));
        }

        public CpsOp EmitCCConsts() {
            int b = thaw_heap.Count;
            EmitInt(cc_constants.Count);
            foreach (int[] cc in cc_constants)
                EmitIntArray(cc);
            return CpsOp.SetSField(cc_pool,
                    CpsOp.MethodCall(Tokens.RuntimeUnit.GetMethod("LoadCCPool"),
                        CpsOp.GetSField(rtunit), CpsOp.IntLiteral(b)));
        }
    }

    class Xref {
        public readonly string unit;
        public readonly int index;
        public readonly string name;

        public static Xref from(object x) {
            return (x == null) ? null : new Xref(x as object[]);
        }
        public Xref(string unit, int index, string name) {
            this.unit = unit; this.index = index; this.name = name;
        }
        public Xref(object[] from) : this(from, 0) {}
        public Xref(object[] from, int ofs) {
            unit  = ((JScalar)from[ofs+0]).str;
            index = (int)((JScalar)from[ofs+1]).num;
            name  = ((JScalar)from[ofs+2]).str;
        }
        public T Resolve<T>() { return (T) CLRBackend.Resolve(this); }
    }

    class Package {
        public Xref own_xref;
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
            typeObject = binder(Unit.SharedName('T', ix, name), Tokens.P6any);
            typeVar    = binder(Unit.SharedName('V', ix, name), Tokens.Variable);
            metaObject = binder(Unit.SharedName('M', ix, name), Tokens.STable);
        }

        public ModuleWithTypeObject(object[] p) : base(p) { }
    }

    class Method {
        public readonly string name;
        public readonly object cname;
        public readonly int kind;
        public readonly string var;
        public readonly Xref body;

        public Method(object[] x) {
            if (x[0] is JScalar) {
                name = JScalar.S(x[0]);
            } else {
                cname = x[0];
            }
            kind = JScalar.I(x[1]);
            var  = JScalar.S(x[2]);
            body = Xref.from(x[3]);
        }

        public static Method[] fromArray(object x) {
            return JScalar.A<Method>(0, x, delegate (object o) {
                return new Method(o as object[]); });
        }
    }

    class Attribute {
        public readonly string name;
        public readonly bool   publ;
        public readonly string ivar;
        public readonly Xref   ibody;
        public readonly Xref   type;

        public Attribute(object[] x) {
            name  = JScalar.S(x[0]);
            publ  = JScalar.B(x[1]);
            ivar  = JScalar.S(x[2]);
            ibody = Xref.from(x[3]);
            type  = Xref.from(x[4]);
        }

        public static Attribute[] fromArray(object x) {
            return JScalar.A<Attribute>(0, x, delegate (object o) {
                return new Attribute(o as object[]); });
        }
    }

    class Class: ModuleWithTypeObject {
        public readonly Attribute[] attributes;
        public readonly Method[] methods;
        public readonly Xref[] superclasses;
        public readonly Xref[] linearized_mro;
        public Class(object[] p) : base(p) {
            attributes     = Attribute.fromArray(p[3]);
            methods        = Method.fromArray(p[4]);
            superclasses   = JScalar.A<Xref>(0, p[5], Xref.from);
            linearized_mro = JScalar.A<Xref>(0, p[6], Xref.from);
        }
    }

    class Grammar: Class {
        public Grammar(object[] p) : base(p) { }
    }

    class Role: ModuleWithTypeObject {
        public readonly Attribute[] attributes;
        public readonly Method[] methods;
        public readonly Xref[] superclasses;
        public Role(object[] p) : base(p) {
            attributes = Attribute.fromArray(p[3]);
            methods    = Method.fromArray(p[4]);
            superclasses = JScalar.A<Xref>(0, p[5], Xref.from);
        }
    }

    class ParametricRole: ModuleWithTypeObject {
        public readonly Attribute[] attributes;
        public readonly Method[] methods;
        public readonly Xref[] superclasses;
        public ParametricRole(object[] p) : base(p) {
            attributes = Attribute.fromArray(p[3]);
            methods    = Method.fromArray(p[4]);
            superclasses = JScalar.A<Xref>(0, p[5], Xref.from);
        }
    }

    class StaticSub {
        public const int RUN_ONCE = 1;
        public const int SPAD_EXISTS = 2;
        public const int GATHER_HACK = 4;
        public const int STRONG_USED = 8;
        public const int RETURNABLE = 16;
        public const int AUGMENTING = 32;
        public const int UNSAFE = 128;
        public readonly string name;
        public readonly Unit unit;
        public readonly Xref outer;
        public readonly int flags;
        public readonly int[] zyg;
        public readonly Xref parametric_role_hack;
        public readonly object augment_hack;
        public readonly object[] hint_hack;
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

        public FieldInfo subinfo;
        public FieldInfo protopad;
        public int nlexn;

        public StaticSub(Unit unit, object[] s, object[] c) {
            this.unit = unit;
            name = JScalar.S(s[1]);
            outer = Xref.from(s[2]);
            flags = JScalar.I(s[3]);
            zyg = JScalar.IA(0, s[4]);
            sclass = JScalar.S(s[5]);
            ltm = s[6];
            exports = (object[]) s[7];
            sig = s[8];

            object[] r_lexicals = s[9] as object[];

            if (c != null) {
                parametric_role_hack = Xref.from(c[1]);
                augment_hack = c[2];
                hint_hack = c[3] as object[];
                is_phaser = JScalar.IN(c[4]);
                body_of = Xref.from(c[5]);
                in_class = Xref.from(c[6]);
                cur_pkg = JScalar.SA(0, c[7]);
                if (c[8] != null) r_lexicals = c[8] as object[];
                body = c[9];
            }

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
                } else if (type == "hint") {
                    obj = new LexHint(bl);
                } else if (type == "sub") {
                    obj = new LexSub(bl);
                } else if (type == "alias") {
                    obj = new LexAlias(bl);
                } else if (type == "stash") {
                    obj = new LexStash(unit, bl);
                } else if (type == "label") {
                    obj = new LexLabel(bl);
                } else if (type == "dispatch") {
                    obj = new LexDispatch(bl);
                } else {
                    throw new Exception("unknown lex type " + type);
                }

                lexicals.Add(new KeyValuePair<string,Lexical>(lname, obj));
                l_lexicals[lname] = obj;
            }
        }

        public bool IsCore() {
            string s = unit.name;
            return s == "CORE";
        }

        public void BindFields(int ix, Func<string,Type,FieldInfo> binder) {
            subinfo  = binder(Unit.SharedName('I', ix, name), Tokens.SubInfo);
            if ((flags & SPAD_EXISTS) != 0)
                protopad = binder(Unit.SharedName('P', ix, name), Tokens.Frame);

            nlexn = 0;
            for (int i = 0; i < lexicals.Count; i++)
                lexicals[i].Value.BindFields(ix, i, this,
                        lexicals[i].Key, binder);
        }

        public Package GetCorePackage(string name) {
            StaticSub csr = this;
            while (csr.unit.name != "CORE")
                csr = csr.outer.Resolve<StaticSub>();
            while (!csr.l_lexicals.ContainsKey(name))
                csr = csr.outer.Resolve<StaticSub>();
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
        public readonly Xref type;

        public LexSimple(object[] l) {
            flags = JScalar.I(l[2]);
            type = Xref.from(l[3]);
        }
    }

    class LexLabel : LexVarish {
        public LexLabel(object[] l) { }
    }
    class LexDispatch : LexVarish {
        public LexDispatch(object[] l) { }
    }

    class LexHint : Lexical {
        public FieldInfo stg;
        public LexHint(object[] l) {}
        public override ClrOp GetCode(int up) {
            return new ClrGetField(Tokens.BValue_v,
                    new ClrGetSField(stg));
        }
        public override void BindFields(int six, int lix, StaticSub sub,
                string name, Func<string,Type,FieldInfo> binder) {
            stg = binder(Unit.SharedName('B', six, name), Tokens.BValue);
        }
    }

    class LexCommon : Lexical {
        public readonly string[] path;
        public FieldInfo stg;
        public LexCommon(object[] l) {
            path = JScalar.SA(2, l);
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
            to = JScalar.S(l[2]);
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
            path = JScalar.SA(2, l);
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

        public void EmitDataArray(Type ty, int ct, byte[] vec) {
            EmitInt(ct);
            // the mono JIT checks for this exact sequence
            il.Emit(OpCodes.Newarr, ty);
            if (vec.Length != 0) {
                FieldBuilder fb = tb.DefineInitializedData(
                        "A" + (CLRBackend.Current.nextarray++), vec, 0);
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

        /* this too */
        public void EmitLong(long l) {
            if (l >= int.MinValue && l <= int.MaxValue) {
                EmitInt((int)l);
                il.Emit(OpCodes.Conv_I8);
            } else if (l >= 0 && l <= uint.MaxValue) {
                EmitInt((int)l);
                il.Emit(OpCodes.Conv_U8);
            } else {
                il.Emit(OpCodes.Ldc_I8, l);
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
        public static readonly Type Int16 = typeof(short);
        public static readonly Type Int32 = typeof(int);
        public static readonly Type Int64 = typeof(long);
        public static readonly Type UInt32 = typeof(uint);
        public static readonly Type UInt64 = typeof(ulong);
        public static readonly Type IntPtr = typeof(IntPtr);
        public static readonly Type Double = typeof(double);
        public static readonly Type Frame = typeof(Frame);
        public static readonly Type Kernel = typeof(Kernel);
        public static readonly Type Builtins = typeof(Builtins);
        public static readonly Type SubInfo = typeof(SubInfo);
        public static readonly Type P6any = typeof(P6any);
        public static readonly Type Variable = typeof(Variable);
        public static readonly Type BValue = typeof(BValue);
        public static readonly Type P6opaque = typeof(P6opaque);
        public static readonly Type DynBlockDelegate = typeof(DynBlockDelegate);
        public static readonly Type STable = typeof(STable);
        public static readonly Type VarHash = typeof(VarHash);
        public static readonly Type VVarList = typeof(VarDeque);
        public static readonly Type FVarList = typeof(Variable[]);
        public static readonly Type Cursor = typeof(Cursor);
        public static readonly Type RxFrame = typeof(RxFrame);
        public static readonly Type CC = typeof(CC);
        public static readonly Type LAD = typeof(LAD);
        public static readonly Type RuntimeUnit = typeof(RuntimeUnit);

        public static readonly ConstructorInfo SubInfo_ctor =
            SubInfo.GetConstructor(new Type[] {
                    String, typeof(int[]), typeof(DynBlockDelegate),
                    SubInfo, LAD, typeof(int[]), typeof(string[]),
                    Int32, typeof(string[]), typeof(int[]) });
        public static readonly ConstructorInfo DynBlockDelegate_ctor =
            typeof(DynBlockDelegate).GetConstructor(new Type[] {
                    typeof(object), typeof(IntPtr) });
        public static readonly ConstructorInfo P6opaque_ctor =
            typeof(P6opaque).GetConstructor(new Type[] {
                    STable });
        public static readonly ConstructorInfo DMO_ctor =
            STable.GetConstructor(new Type[] { String });
        public static readonly ConstructorInfo RxFrame_ctor =
            RxFrame.GetConstructor(new Type[] { String, Cursor, Boolean, Boolean });
        public static readonly ConstructorInfo Frame_ctor =
            Frame.GetConstructor(new Type[] { Frame, Frame, SubInfo });
        public static readonly ConstructorInfo SV_ctor =
            typeof(SimpleVariable).GetConstructor(new Type[] {
                    Boolean, Boolean, STable, typeof(ViviHook), P6any });
        public static readonly ConstructorInfo SubViviHook_ctor =
            typeof(SubViviHook).GetConstructor(new Type[] { P6any });
        public static readonly ConstructorInfo HashViviHook_ctor =
            typeof(HashViviHook).GetConstructor(new Type[] { P6any, String });
        public static readonly ConstructorInfo ArrayViviHook_ctor =
            typeof(ArrayViviHook).GetConstructor(new Type[] { P6any, Int32 });
        public static readonly ConstructorInfo NewHashViviHook_ctor =
            typeof(NewHashViviHook).GetConstructor(new Type[] { Variable, String });
        public static readonly ConstructorInfo NewArrayViviHook_ctor =
            typeof(NewArrayViviHook).GetConstructor(new Type[] { Variable, Int32 });
        public static readonly ConstructorInfo Rat_ctor =
            typeof(Rat).GetConstructor(new Type[] { typeof(BigInteger), typeof(ulong) });
        public static readonly ConstructorInfo BigInteger_ctor =
            typeof(BigInteger).GetConstructor(new Type[] { typeof(short), typeof(uint[]) });
        public static readonly ConstructorInfo CC_ctor =
            CC.GetConstructor(new Type[] { typeof(int[]) });
        public static readonly Dictionary<string,int> LADcodes
            = _LADcodes();
        private static Dictionary<string,int> _LADcodes() {
            Dictionary<string,int> n =
                new Dictionary<string,int>();
            n["CC"]  = 1;
            n["Str"] = 2;
            n["Param"] = 3;
            n["Method"] = 4;
            n["Dispatcher"] = 5;
            n["StrNoCase"] = 6;
            n["Imp"] = 7;
            n["Dot"] = 8;
            n["None"] = 9;
            n["Null"] = 10;
            n["Plus"] = 11;
            n["Star"] = 12;
            n["Opt"] = 13;
            n["Sequence"] = 14;
            n["Any"] = 15;
            return n;
        }

        public static readonly MethodInfo P6any_InvokeMethod =
            P6any.GetMethod("InvokeMethod");
        public static readonly MethodInfo P6any_Invoke =
            P6any.GetMethod("Invoke");
        public static readonly MethodInfo P6any_SetSlot =
            P6any.GetMethod("SetSlot");
        public static readonly MethodInfo P6any_GetSlot =
            P6any.GetMethod("GetSlot");
        public static readonly MethodInfo SubInfo_AddHint =
            SubInfo.GetMethod("AddHint");
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
        public static readonly MethodInfo Kernel_CheckUnsafe =
            typeof(Kernel).GetMethod("CheckUnsafe");
        public static readonly MethodInfo Kernel_NewLabelVar =
            typeof(Kernel).GetMethod("NewLabelVar");
        public static readonly MethodInfo Kernel_MakeDispatcher =
            typeof(Kernel).GetMethod("MakeDispatcher");
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
        public static readonly MethodInfo Kernel_NewTypedScalar =
            typeof(Kernel).GetMethod("NewTypedScalar");
        public static readonly MethodInfo Kernel_Decontainerize =
            typeof(Kernel).GetMethod("Decontainerize");
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
        public static readonly MethodInfo Kernel_BoxAnyMO_Int32 =
            typeof(Kernel).GetMethod("BoxAnyMO").MakeGenericMethod(typeof(int));
        public static readonly MethodInfo Kernel_BoxAnyMO_Double =
            typeof(Kernel).GetMethod("BoxAnyMO").MakeGenericMethod(typeof(double));
        public static readonly MethodInfo Kernel_BoxAnyMO_Rat =
            typeof(Kernel).GetMethod("BoxAnyMO").MakeGenericMethod(typeof(Rat));
        public static readonly MethodInfo Kernel_BoxAnyMO_BigInteger =
            typeof(Kernel).GetMethod("BoxAnyMO").MakeGenericMethod(typeof(BigInteger));
        public static readonly MethodInfo Builtins_Make =
            typeof(Builtins).GetMethod("Make");
        public static readonly MethodInfo Builtins_MEMap =
            typeof(Builtins).GetMethod("MEMap");
        public static readonly MethodInfo Builtins_MEGrep =
            typeof(Builtins).GetMethod("MEGrep");
        public static readonly MethodInfo DMO_AddMethod =
            typeof(STable).GetMethod("AddMethod");
        public static readonly MethodInfo DMO_AddAttribute =
            typeof(STable).GetMethod("AddAttribute");
        public static readonly MethodInfo DMO_Invalidate =
            typeof(STable).GetMethod("Invalidate");
        public static readonly MethodInfo DMO_FillParametricRole =
            typeof(STable).GetMethod("FillParametricRole");
        public static readonly MethodInfo DMO_FillRole =
            typeof(STable).GetMethod("FillRole");
        public static readonly MethodInfo DMO_FillClass =
            typeof(STable).GetMethod("FillClass");
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
        public static readonly MethodInfo RU_LoadStrArray =
            RuntimeUnit.GetMethod("LoadStrArray");
        public static readonly MethodInfo RU_LoadClassMembers =
            RuntimeUnit.GetMethod("LoadClassMembers");
        public static readonly MethodInfo RU_LoadSubInfo =
            RuntimeUnit.GetMethod("LoadSubInfo");
        public static readonly MethodInfo RU_LoadSignature =
            RuntimeUnit.GetMethod("LoadSignature");
        public static readonly MethodInfo RU_LoadLAD =
            RuntimeUnit.GetMethod("LoadLAD");
        public static readonly MethodInfo RU_LoadLADArr =
            RuntimeUnit.GetMethod("LoadLADArr");

        public static readonly FieldInfo P6any_mo =
            P6any.GetField("mo");
        public static readonly FieldInfo BValue_v =
            BValue.GetField("v");
        public static readonly FieldInfo SubInfo_protosub =
            SubInfo.GetField("protosub");
        public static readonly FieldInfo SubInfo_protopad =
            SubInfo.GetField("protopad");
        public static readonly FieldInfo SubInfo_mo =
            SubInfo.GetField("mo");
        public static readonly FieldInfo SubInfo_sig_i =
            SubInfo.GetField("sig_i");
        public static readonly FieldInfo SubInfo_sig_r =
            SubInfo.GetField("sig_r");
        public static readonly FieldInfo P6opaque_slots =
            P6opaque.GetField("slots");
        public static readonly FieldInfo DMO_typeObject =
            STable.GetField("typeObject");
        public static readonly FieldInfo DMO_how =
            STable.GetField("how");
        public static readonly FieldInfo Kernel_NumMO =
            Kernel.GetField("NumMO");
        public static readonly FieldInfo Kernel_IntMO =
            Kernel.GetField("IntMO");
        public static readonly FieldInfo Kernel_RatMO =
            Kernel.GetField("RatMO");
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
        public static readonly FieldInfo RU_xref = RuntimeUnit.GetField("xref");

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
            cx.il.Emit(OpCodes.Ldfld, Tokens.P6any_mo);
            cx.il.Emit(OpCodes.Ldfld, thing);
            cx.il.Emit(OpCodes.Ldloc, cx.ospill);
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
        public readonly string ls;
        public readonly string le;
        public readonly int ng;
        public readonly string lg;

        public ClrEhSpan(int kls, string tag, string ls, string le, string lg) {
            Returns = Tokens.Void;
            HasCases = false;
            this.kls = kls; this.tag = tag; this.ls = ls; this.le = le;
            this.lg = lg;
        }
        public ClrEhSpan(int kls, string tag, string ls, string le, int ng) {
            Returns = Tokens.Void;
            HasCases = false;
            this.kls = kls; this.tag = tag; this.ls = ls; this.le = le;
            this.ng = ng;
        }

        public override void CodeGen(CgContext cx) {
            int lidn = -1;
            if (tag != "") {
                for (lidn = 0; lidn < cx.ehlabelBuffer.Count &&
                        cx.ehlabelBuffer[lidn] != tag; lidn++);
                if (lidn == cx.ehlabelBuffer.Count)
                    cx.ehlabelBuffer.Add(tag);
            }
            cx.ehspanBuffer.Add(cx.named_cases[ls]);
            cx.ehspanBuffer.Add(cx.named_cases[le]);
            cx.ehspanBuffer.Add(kls);
            if (lg == null) {
                cx.ehspanBuffer.Add(ng);
            } else if (kls == SubInfo.ON_VARLOOKUP) {
                int ix = cx.let_names.Length - 1;
                while (ix >= 0 && cx.let_names[ix] != lg)
                    ix--;
                if (ix < Tokens.NumInt32)
                    throw new Exception("variable in index area??");
                cx.ehspanBuffer.Add(ix - Tokens.NumInt32);
            } else {
                cx.ehspanBuffer.Add(cx.named_cases[lg]);
            }
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
        public readonly bool ismethod;
        public readonly string sig;
        public readonly ClrOp[] zyg;

        // generates the argument list, from the elements of zyg
        void GenArgList(int min, CgContext cx) {
            bool general = false;
            for (int i = min; i < zyg.Length; i++)
                if (sig[i - min] != '\0')
                    general = true;
            if (!general) {
                cx.EmitInt(zyg.Length - min);
                cx.il.Emit(OpCodes.Newarr, Tokens.Variable);
                for (int i = min; i < zyg.Length; i++) {
                    cx.il.Emit(OpCodes.Dup);
                    cx.EmitInt(i - min);
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
                int ix  = min;

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

            zyg[ismethod ? 1 : 0].CodeGen(cx);
            cx.il.Emit(OpCodes.Ldarg_0);
            if (ismethod)
                zyg[0].CodeGen(cx);

            GenArgList(ismethod ? 2 : 1, cx);

            cx.il.Emit(OpCodes.Callvirt, ismethod ?
                    Tokens.P6any_InvokeMethod : Tokens.P6any_Invoke);
            cx.il.Emit(OpCodes.Ret);
            cx.il.MarkLabel(cx.cases[cx.next_case++]);
            cx.save_line();
        }

        public override void ListCases(CgContext cx) {
            cx.num_cases++;
        }

        public ClrSubyCall(bool ismethod, string sig, ClrOp[] zyg) {
            if (ismethod) sig = "\0" + sig;
            int i = 0;
            if (ismethod) TypeCheck(zyg[i++].Returns, Tokens.String);
            TypeCheck(zyg[i++].Returns, Tokens.P6any);
            int j = 0;
            while (j < sig.Length) {
                string s = sig.Substring(j+1, sig[j]);
                j += (1 + s.Length);
                TypeCheck(zyg[i++].Returns, (s == "flatcap") ? Tokens.P6any : Tokens.Variable);
            }
            this.ismethod = ismethod;
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

    class ClrLongLiteral : ClrOp {
        long data;
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public ClrLongLiteral(Type ty, long data) {
            this.data = data;
            Returns = ty;
            Constant = true;
        }
        public override void CodeGen(CgContext cx) {
            cx.EmitLong(data);
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
            byte[] vec = new byte[names.Length*4];
            int j = 0;
            for (int i = 0; i < names.Length; i++) {
                uint k = (uint)tcx.named_cases[names[i]];
                vec[j++] = (byte)k; k >>= 8;
                vec[j++] = (byte)k; k >>= 8;
                vec[j++] = (byte)k; k >>= 8;
                vec[j++] = (byte)k; k >>= 8;
            }
            cx.EmitDataArray(Tokens.Int32, vec.Length / 4, vec);
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

    class ClrNewDataArray : ClrOp {
        readonly byte[] vec;
        readonly int ct;
        readonly Type ty;
        public ClrNewDataArray(Type ty, int ct, byte[] vec) {
            // TODO: automatically cut array into segments
            if (vec.Length >= 0x3f0000)
                throw new ArgumentException();
            Returns = ty.MakeArrayType();
            this.ty = ty;
            this.ct = ct;
            this.vec = vec;
            Constant = true;
        }
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public override void CodeGen(CgContext cx) {
            cx.EmitDataArray(ty, ct, vec);
        }
    }

    class ClrWiden : ClrOp {
        readonly ClrOp z;
        public ClrWiden(Type to, ClrOp z) {
            TypeCheck(z.Returns, to);
            Returns = to;
            this.z = z;
            this.Constant = z.Constant;
        }
        public override void ListCases(CgContext cx) { z.ListCases(cx); }
        public override ClrOp Sink() { return z.Sink(); }
        public override void CodeGen(CgContext cx) {
            z.CodeGen(cx);
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
                    string ln = "!spill" + (CLRBackend.Current.nextspill++);
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

        public static CpsOp Sequence(params CpsOp[] terms) {
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

        public static CpsOp Span(string l1, string l2, bool sync,
                List<ClrEhSpan> co, CpsOp body) {
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
            foreach (ClrEhSpan cl in co) stmts.Add(cl);

            return new CpsOp(stmts.ToArray(), new ClrResult(body.head.Returns));
        }

        public static CpsOp Ternary(CpsOp cond, CpsOp iftrue, CpsOp iffalse) {
            ClrOp iftrue_h = iftrue.head;
            ClrOp iffalse_h = iffalse.head;
            ClrOp[] iftrue_s = iftrue.stmts;
            ClrOp[] iffalse_s = iffalse.stmts;

            Resultify(ref iftrue_s, ref iftrue_h);
            Resultify(ref iffalse_s, ref iffalse_h);

            CLRBackend cb = CLRBackend.Current;
            string l1 = "!else"  + (cb.nextlabel++);
            string l2 = "!endif" + (cb.nextlabel++);

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
            CLRBackend cb = CLRBackend.Current;
            string l1 = "!again" + (cb.nextlabel++);
            string l2 = "!check" + (cb.nextlabel++);

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

        public static CpsOp MethodCall(MethodInfo tk, params CpsOp[] zyg) {
            return CpsCall(null, tk, zyg);
        }

        public static CpsOp CpsCall(Type cps, MethodInfo tk, params CpsOp[] zyg) {
            return Primitive(zyg, delegate (ClrOp[] heads) {
                return (cps == null) ?
                    new CpsOp(new ClrMethodCall(false, tk, heads)) :
                    Cps(new ClrMethodCall(true, tk, heads), cps);
            });
        }

        public static CpsOp ConstructorCall(ConstructorInfo tk, params CpsOp[] zyg) {
            return Primitive(zyg, delegate (ClrOp[] heads) {
                return new CpsOp(new ClrConstructorCall(tk, heads));
            });
        }

        public static CpsOp CpsReturn(params CpsOp[] zyg) {
            return Primitive(zyg, delegate (ClrOp[] heads) {
                return new CpsOp(new ClrCpsReturn(heads.Length > 0 ? heads[0] : null));
            });
        }

        public static CpsOp LabelId(CgContext tcx, string label) {
            return new CpsOp(new ClrLabelLiteral(tcx, label));
        }

        public static CpsOp LabelTable(CgContext tcx, string[] labels) {
            return new CpsOp(new ClrLabelArray(tcx, labels));
        }

        public static CpsOp Goto(string label, bool iffalse, params CpsOp[] zyg) {
            return Primitive(zyg, delegate (ClrOp[] heads) {
                return new CpsOp(new ClrGoto(label, iffalse,
                    heads.Length > 0 ? heads[0] : null));
            });
        }

        public static CpsOp GotoReturn(string label, CpsOp body) {
            ClrOp body_h = body.head;
            ClrOp[] body_s = body.stmts;
            Resultify(ref body_s, ref body_h);
            List<ClrOp> stmts = new List<ClrOp>();

            foreach (ClrOp c in body_s) stmts.Add(c);
            stmts.Add(body_h);
            stmts.Add(new ClrGoto(label, false, null));

            return new CpsOp(stmts.ToArray(), new ClrNullLiteral(Tokens.Variable));
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

        public static CpsOp ShortLiteral(int x) {
            return new CpsOp(new ClrIntLiteral(Tokens.Int16, x));
        }

        public static CpsOp IntLiteral(int x) {
            return new CpsOp(new ClrIntLiteral(Tokens.Int32, x));
        }

        public static CpsOp BoolLiteral(bool x) {
            return new CpsOp(new ClrIntLiteral(Tokens.Boolean, x ? 1 : 0));
        }

        public static CpsOp LongLiteral(long x) {
            return new CpsOp(new ClrLongLiteral(Tokens.Int64, x));
        }

        public static CpsOp ULongLiteral(ulong x) {
            return new CpsOp(new ClrLongLiteral(Tokens.UInt64, (long)x));
        }

        public static CpsOp BigIntegerLiteral(BigInteger x) {
            uint[] w = x.GetWords();
            int[] ws = new int[w.Length];
            for (int i = 0; i < w.Length; i++) ws[i] = (int)w[i];
            return CpsOp.ConstructorCall(Tokens.BigInteger_ctor,
                CpsOp.ShortLiteral(x.Sign), CpsOp.NewIntArray(typeof(uint),ws));
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

        public static CpsOp Operator(Type rt, OpCode op, params CpsOp[] zyg) {
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

        public static CpsOp PokeLet(string name, CpsOp zyg) {
            return Primitive(new CpsOp[] { zyg }, delegate(ClrOp[] heads) {
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

        public static CpsOp SubyCall(bool method, string sig, params CpsOp[] zyg) {
            return Primitive(zyg, delegate(ClrOp[] heads) {
                return CpsOp.Cps(new ClrSubyCall(method, sig, heads), Tokens.Variable);
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
        public static CpsOp NewArray(Type ty, params CpsOp[] zyg) {
            return Primitive(zyg, delegate (ClrOp[] h) {
                return new CpsOp(new ClrNewArray(ty, h));
            });
        }

        public static CpsOp NewByteArray(Type ty, byte[] vec) {
            return new CpsOp(new ClrNewDataArray(ty, vec.Length, vec));
        }

        public static CpsOp NewIntArray(Type ty, int[] vec) {
            byte[] buf = new byte[vec.Length * 4];
            int r = 0;
            for (int i = 0; i < vec.Length; i++) {
                uint d = (uint) vec[i];
                buf[r++] = (byte)((d >>  0) & 0xFF);
                buf[r++] = (byte)((d >>  8) & 0xFF);
                buf[r++] = (byte)((d >> 16) & 0xFF);
                buf[r++] = (byte)((d >> 24) & 0xFF);
            }
            return new CpsOp(new ClrNewDataArray(ty, vec.Length, buf));
        }

        public static CpsOp StringArray(bool omit, string[] vec) {
            if (vec.Length == 0 && omit) {
                return Null(typeof(string[]));
            } else {
                Unit u = CLRBackend.Current.unit;
                int b = u.thaw_heap.Count;
                u.EmitStrArray(vec);
                return MethodCall(Tokens.RU_LoadStrArray,
                        GetSField(u.rtunit), IntLiteral(b));
            }
        }

        public static CpsOp Widen(Type to, CpsOp z) {
            return new CpsOp(z.stmts, new ClrWiden(to, z.head));
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
        List<List<ClrEhSpan>> eh_stack = new List<List<ClrEhSpan>>();
        List<object[]> scope_stack = new List<object[]>();

        public NamProcessor(CpsBuilder cpb, StaticSub sub) {
            this.sub = sub;
            this.cpb = cpb;
        }

        CpsOp AccessLex(object[] zyg) {
            return RawAccessLex( JScalar.S(zyg[0]), JScalar.S(zyg[1]),
                (zyg.Length > 2 ? Scan(zyg[2]) : null) );
        }

        CpsOp AccessLet(object[] zyg) {
            string name = JScalar.S(zyg[1]);
            CpsOp set_to = zyg.Length > 2 ? Scan(zyg[2]) : null;
            Type t;

            if (let_types.TryGetValue(name, out t)) {
                return (set_to == null) ? CpsOp.PeekLet(name, t) :
                    CpsOp.PokeLet(name, set_to);
            }
            throw new Exception("No such let " + name);
        }

        CpsOp RawAccessLex(string type, string name, CpsOp set_to) {
            bool core = type == "corelex";
            bool uplex = type == "outerlex";
            bool upscope = uplex && (scope_stack.Count > 0);
            int uplevel;

            for (int i = (core ? -1 : scope_stack.Count - (upscope ? 2 : 1)); i >= 0; i--) {
                object[] rec = scope_stack[i];
                for (int j = 2; j < rec.Length - 2; j += 2) {
                    if (JScalar.S(rec[j]) == name) {
                        string lname = JScalar.S(rec[j+1]);
                        return (set_to == null) ?
                            CpsOp.PeekLet(lname, let_types[lname]) :
                            CpsOp.PokeLet(lname, set_to);
                    }
                }
            }

            Lexical lex = ResolveLex(name, uplex&& !upscope, out uplevel, core);

            return CpsOp.LexAccess(lex, uplevel,
                set_to == null ? new CpsOp[0] : new CpsOp[] { set_to });
        }

        Lexical ResolveLex(string name, bool upf, out int uplevel, bool core) {
            uplevel = 0;
            StaticSub csr = sub;

            if (upf) {
                csr = csr.outer.Resolve<StaticSub>();
                uplevel++;
            }

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
                csr = csr.outer.Resolve<StaticSub>();
                uplevel++;
            }
        }

        CpsOp ExactNum(int numbase, string digits) {
            BigInteger num = BigInteger.Zero;
            BigInteger den = BigInteger.Zero;

            bool neg = false;

            foreach (char d in digits) {
                if (d == '-') neg = true;
                if (d == '_') continue;
                if (d == '.') {
                    if (den != BigInteger.Zero)
                        throw new Exception("two dots in " + digits);
                    den = BigInteger.One;
                    continue;
                }
                int digval;
                if (d >= '0' && d <= '9') { digval = d - '0'; }
                else if (d >= 'a' && d <= 'z') { digval = d + 10 - 'a'; }
                else if (d >= 'A' && d <= 'Z') { digval = d + 10 - 'A'; }
                else { throw new Exception("invalid digit in " + digits); }

                if (digval >= numbase) { throw new Exception("out of range digit in " + digits); }

                num *= numbase;
                den *= numbase;
                num += digval;
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
                double dval = (double)num / (double)den;
                return CpsOp.MethodCall(Tokens.Kernel_BoxAnyMO_Double, CpsOp.DoubleLiteral(dval), CpsOp.GetSField(Tokens.Kernel_NumMO));
            }

            if (sden == 0) {
                int snum;
                if (num.AsInt32(out snum)) {
                    return CpsOp.MethodCall(Tokens.Kernel_BoxAnyMO_Int32, CpsOp.IntLiteral(snum), CpsOp.GetSField(Tokens.Kernel_IntMO));
                }
                return CpsOp.MethodCall(Tokens.Kernel_BoxAnyMO_BigInteger, CpsOp.BigIntegerLiteral(num), CpsOp.GetSField(Tokens.Kernel_IntMO));
            }

            return CpsOp.MethodCall(Tokens.Kernel_BoxAnyMO_Rat,
                CpsOp.ConstructorCall(Tokens.Rat_ctor, CpsOp.BigIntegerLiteral(num), CpsOp.ULongLiteral(sden)), CpsOp.GetSField(Tokens.Kernel_RatMO));
        }

        CpsOp MakeDispatch(string prefix) {
            HashSet<string> names = new HashSet<string>();
            List<CpsOp> cands = new List<CpsOp>();
            string filter = prefix + ":";
            string pn = prefix + ":(!proto)";

            for (StaticSub csr = sub; ; csr = csr.outer.Resolve<StaticSub>()) {
                bool brk = false;
                foreach (KeyValuePair<string,Lexical> kp in csr.lexicals) {
                    if (Utils.StartsWithInvariant(filter, kp.Key) &&
                            kp.Key != pn &&
                            !names.Contains(kp.Key)) {
                        names.Add(kp.Key);
                        brk = true;
                        cands.Add(CpsOp.MethodCall(Tokens.Variable_Fetch, RawAccessLex("scopedlex", kp.Key, null)));
                    }
                }
                if (csr.outer == null) break;
                // don't go above nearest proto
                if (csr.l_lexicals.ContainsKey(pn)) break;
                if (brk) cands.Add(CpsOp.Null(Tokens.P6any));
            }

            return CpsOp.MethodCall(Tokens.Kernel_NewROScalar,
                CpsOp.MethodCall(Tokens.Kernel_MakeDispatcher,
                    CpsOp.StringLiteral(prefix), CpsOp.Null(Tokens.P6any),
                    CpsOp.NewArray(Tokens.P6any, cands.ToArray())));
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
            int sh = ismethod ? 3 : 2;
            string sig = ((JScalar) zyg[sh-1]).str;
            CpsOp[] args = new CpsOp[zyg.Length - 2];
            if (ismethod) {
                args[0] = AnyStr(zyg[1]);
            }
            for (int i = sh; i < zyg.Length; i++)
                args[i-2] = Scan(zyg[i]);
            return CpsOp.SubyCall(ismethod, sig, args);
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
        static Dictionary<string, Func<CpsOp[], CpsOp>> thandlers;
        static Dictionary<string, Type> namtypes;

        static Type namtype(object z) {
            string name = JScalar.S(z);
            if (name.Length > 4 && name.Substring(0,4) == "clr:")
                return Type.GetType(name.Substring(4));
            return namtypes[name];
        }

        static NamProcessor() {
            namtypes = new Dictionary<string, Type>();
            namtypes["str"] = Tokens.String;
            namtypes["num"] = Tokens.Double;
            namtypes["int"] = Tokens.Int32;
            namtypes["var"] = Tokens.Variable;
            namtypes["obj"] = Tokens.P6any;
            namtypes["fvarlist"] = Tokens.FVarList;
            namtypes["vvarlist"] = Tokens.VVarList;
            namtypes["varhash"] = Tokens.VarHash;
            namtypes["frame"] = Tokens.Frame;
            namtypes["cursor"] = Tokens.Cursor;
            namtypes["strbuf"] = typeof(StringBuilder);
            namtypes["treader"] = typeof(TextReader);

            handlers = new Dictionary<string, Func<NamProcessor,object[],CpsOp>>();
            thandlers = new Dictionary<string, Func<CpsOp[], CpsOp>>();
            Dictionary<string, Func<object[], object>> mhandlers =
                new Dictionary<string, Func<object[], object>>();


            handlers["null"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.Null(namtype(zyg[1])); };
            handlers["str"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.StringLiteral(((JScalar)zyg[1]).str); };
            handlers["int"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.IntLiteral((int) ((JScalar)zyg[1]).num); };
            handlers["char"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.CharLiteral(JScalar.S(zyg[1])[0]); };
            handlers["double"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.DoubleLiteral(((JScalar)zyg[1]).num); };
            handlers["exactnum"] = delegate(NamProcessor th, object[] zyg) {
                return th.ExactNum(JScalar.I(zyg[1]), JScalar.S(zyg[2])); };
            handlers["bool"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.BoolLiteral(((JScalar)zyg[1]).num != 0); };
            handlers["ann"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.Annotate((int) ((JScalar)zyg[2]).num, th.Scan(zyg[3])); };
            handlers["label"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.Label(FixStr(z[1]), true);
            };
            handlers["cgoto"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.Goto(FixStr(z[1]), false, th.Scan(z[2]));
            };
            handlers["ncgoto"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.Goto(FixStr(z[1]), true, th.Scan(z[2]));
            };
            handlers["goto"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.Goto(FixStr(z[1]), false);
            };
            handlers["xspan"] = delegate(NamProcessor th, object[] z) {
                List<ClrEhSpan> xn = new List<ClrEhSpan>();
                string ls = FixStr(z[1]);
                string le = FixStr(z[2]);
                for (int i = 5; i < z.Length; i += 3)
                    xn.Add(new ClrEhSpan(FixInt(z[i]), FixStr(z[i+1]),
                                ls, le, FixStr(z[i+2])));
                th.eh_stack.Add(xn);
                CpsOp ch = th.Scan(z[4]);
                th.eh_stack.RemoveAt(th.eh_stack.Count - 1);
                return CpsOp.Span(ls, le, FixBool(z[3]), xn, ch);
            };
            handlers["letscope"] = delegate(NamProcessor th, object[] zyg) {
                List<ClrEhSpan> xn = new List<ClrEhSpan>();
                string s = "!start" + CLRBackend.Current.nextlabel++;
                string e = "!end" + CLRBackend.Current.nextlabel++;
                for (int i = 2; i < zyg.Length - 2; i += 2) {
                    string vn = JScalar.S(zyg[i]);
                    string ln = JScalar.S(zyg[i+1]);
                    xn.Add(new ClrEhSpan(SubInfo.ON_VARLOOKUP, vn, s, e, ln));
                }
                th.scope_stack.Add(zyg);
                xn.Add(new ClrEhSpan(SubInfo.ON_VARLOOKUP, "", s, e, th.scope_stack.Count));
                CpsOp co = th.Scan(zyg[zyg.Length - 1]);
                th.scope_stack.RemoveAt(th.scope_stack.Count - 1);
                return CpsOp.Span(s, e, false, xn, co);
            };
            handlers["letvar"] = delegate(NamProcessor th, object[] zyg) {
                return th.AccessLet(zyg); };
            handlers["scopedlex"] =
            handlers["outerlex"] =
            handlers["corelex"] = delegate(NamProcessor th, object[] zyg) {
                return th.AccessLex(zyg); };
            handlers["compare"] = handlers["arith"] =
                delegate(NamProcessor th, object[] zyg) {
                    return CpsOp.PolyOp(FixStr(zyg[1]),
                            th.Scan(zyg[2]), th.Scan(zyg[3])); };
            handlers["setslot"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.MethodCall(Tokens.P6any_SetSlot,
                    th.Scan(zyg[2]), th.AnyStr(zyg[1]), th.Scan(zyg[3])); };
            handlers["getslot"] = delegate(NamProcessor th, object[] zyg) {
                Type ty = namtype(zyg[2]);
                return CpsOp.UnboxAny(ty, CpsOp.MethodCall(Tokens.P6any_GetSlot,
                    th.Scan(zyg[3]), th.AnyStr(zyg[1]))); };
            handlers["cast"] = delegate(NamProcessor th, object[] zyg) {
                Type tty = namtype(zyg[1]);
                CpsOp z = th.Scan(zyg[2]);
                Type fty = z.head.Returns;

                if (tty == Tokens.Frame && fty == Tokens.P6any
                        || tty == Tokens.Cursor && fty == Tokens.P6any) {
                    return CpsOp.UnboxAny(tty, z);
                } else if (tty == Tokens.Double && fty == Tokens.Int32) {
                    return CpsOp.Operator(tty, OpCodes.Conv_R8, z);
                } else if (tty == Tokens.Int32 && fty == Tokens.Double) {
                    return CpsOp.Operator(tty, OpCodes.Conv_I4, z);
                } else {
                    throw new NotImplementedException("cast " + fty + " -> " + tty);
                }
            };
            handlers["die"] = delegate(NamProcessor th, object[] zyg) {
                if (zyg[1] is JScalar) {
                    return CpsOp.CpsCall(Tokens.Variable, Tokens.Kernel_Die,
                        CpsOp.StringLiteral(FixStr(zyg[1])));
                } else {
                    return CpsOp.CpsCall(Tokens.Variable, Tokens.Kernel_SFH,
                        CpsOp.IntLiteral(SubInfo.ON_DIE),
                        CpsOp.Null(Tokens.Frame), CpsOp.IntLiteral(-1),
                        CpsOp.Null(Tokens.String), CpsOp.MethodCall(
                            Tokens.Kernel_NewROScalar, th.Scan(zyg[1])));
                }
            };
            handlers["control"] = delegate(NamProcessor th, object[] zyg) {
                if (!(zyg[1] is JScalar)) goto dynamic;
                if (JScalar.S(((object[])zyg[2])[0]) != "null") goto dynamic;
                if (JScalar.S(((object[])zyg[3])[0]) != "int") goto dynamic;
                if (JScalar.S(((object[])zyg[4])[0]) != "null") goto dynamic;

                {
                    int type = JScalar.I(zyg[1]);
                    string lbl = null;
                    for (int i = th.eh_stack.Count - 1; i >= 0; i--)
                        foreach (ClrEhSpan ces in th.eh_stack[i])
                            if (ces.kls == type) {
                                lbl = ces.lg;
                                goto found;
                            }
                    goto dynamic;
found:
                    return CpsOp.GotoReturn(lbl, th.Scan(zyg[5]));
                }

dynamic:
                CpsOp[] z = new CpsOp[5];
                for (int i = 1; i < 5; i++)
                    z[i] = th.Scan(zyg[i+1]);
                z[0] = zyg[1] is JScalar ? CpsOp.IntLiteral(FixInt(zyg[1])) :
                    th.Scan(zyg[1]);

                return CpsOp.CpsCall(Tokens.Variable, Tokens.Kernel_SFH, z);
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
                    mo = CpsOp.GetField(Tokens.P6any_mo, th.Scan(zyg[1]));
                }
                CpsOp boxee = th.Scan(zyg[2]);
                return CpsOp.MethodCall(Tokens.Kernel.GetMethod("BoxAnyMO").MakeGenericMethod(boxee.head.Returns), boxee, mo);
            };
            handlers["unbox"] = delegate(NamProcessor th, object[] zyg) {
                Type t = namtype(zyg[1]);
                CpsOp unboxee = th.Scan(zyg[2]);
                return CpsOp.MethodCall(Tokens.Kernel.GetMethod("UnboxAny").MakeGenericMethod(t), unboxee);
            };
            handlers["newboundvar"] = delegate(NamProcessor th, object[] zyg) {
                CpsOp rhs = th.Scan(zyg[3]);
                bool ro   = JScalar.B(zyg[1]);
                bool list = JScalar.B(zyg[2]);
                return CpsOp.CpsCall(Tokens.Variable,
                    Tokens.Kernel_NewBoundVar, CpsOp.BoolLiteral(ro),
                    CpsOp.BoolLiteral(list),
                    CpsOp.GetSField(Tokens.Kernel_AnyMO), rhs);
            };
            handlers["whileloop"] = delegate(NamProcessor th, object[] z) {
                bool until = ((JScalar)z[1]).num != 0;
                bool once  = ((JScalar)z[2]).num != 0;
                return CpsOp.While(until, once, th.Scan(z[3]), th.Scan(z[4])); };
            handlers["_makesub"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_MakeSub,
                    CpsOp.GetSField(((StaticSub)z[1]).subinfo),
                    CpsOp.CallFrame()); };
            handlers["_newlabel"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_NewLabelVar,
                        CpsOp.CallFrame(),
                        CpsOp.StringLiteral(JScalar.S(z[1]))); };
            handlers["_newdispatch"] = delegate(NamProcessor th, object[] z) {
                return th.MakeDispatch(JScalar.S(z[1])); };
            handlers["class_ref"] = delegate(NamProcessor th, object[] z) {
                string kind = FixStr(z[1]);
                ModuleWithTypeObject m;
                if (z.Length == 3) {
                    m = (ModuleWithTypeObject)th.sub.GetCorePackage(FixStr(z[2]));
                } else {
                    m = (new Xref(z, 2)).Resolve<ModuleWithTypeObject>();
                }
                if (kind != "mo")
                    throw new NotImplementedException();
                return CpsOp.GetSField(m.metaObject);
            };
            handlers["methodcall"] = delegate (NamProcessor th, object[] zyg) {
                return th.SubyCall(true, zyg); };
            handlers["subcall"] = delegate (NamProcessor th, object[] zyg) {
                return th.SubyCall(false, zyg); };
            handlers["_hintset"] = delegate (NamProcessor th, object[] zyg) {
                int d;
                Lexical lx = th.ResolveLex(JScalar.S(zyg[1]),false,out d,false);
                FieldInfo peer = (lx is LexCommon) ? ((LexCommon)lx).stg :
                    ((LexHint)lx).stg;
                return CpsOp.SetField(Tokens.BValue_v, CpsOp.GetSField(peer),
                    CpsOp.MethodCall(Tokens.Kernel_Decontainerize, th.Scan(zyg[2]))); };
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
                return CpsOp.ConstructorCall(Tokens.CC_ctor,
                    CpsOp.NewIntArray(Tokens.Int32, vec));
            };
            handlers["rxpushcapture"] = delegate(NamProcessor th, object[] z) {
                CpsOp strs = CpsOp.StringArray(false, JScalar.SA(2,z));
                return CpsOp.MethodCall(Tokens.RxFrame_PushCapture,
                    CpsOp.GetField(Tokens.Frame_rx, CpsOp.CallFrame()),
                    th.Constant(strs), th.Scan(z[1]));
            };
            handlers["rxbprim"] = delegate(NamProcessor th, object[] z) {
                CpsOp[] args = new CpsOp[z.Length - 1];
                for(int i = 0; i < z.Length - 2; i++)
                    args[i+1] = th.Scan(z[i+2]);
                args[0] = CpsOp.GetField(Tokens.Frame_rx, CpsOp.CallFrame());
                CpsOp call = CpsOp.MethodCall(
                        Tokens.RxFrame.GetMethod(FixStr(z[1])), args);
                return CpsOp.Goto("backtrack", true, call);
            };
            handlers["const"] = delegate(NamProcessor th, object[] z) {
                string code = null;
                object[] ch = z[1] as object[];
                string chh = JScalar.S(ch[0]);
                if (chh == "exactnum") {
                    code = "X" + JScalar.S(ch[1]) + "," + JScalar.S(ch[2]);
                } else if (chh == "box" && ch[1] is JScalar) {
                    string typ = JScalar.S(ch[1]);
                    object[] chch = ch[2] as object[];
                    string chchh = JScalar.S(chch[0]);
                    if (typ == "Str" && chchh == "str") {
                        code = "S" + JScalar.S(chch[1]);
                    } else if (typ == "Num" && chchh == "double") {
                        code = "D" + JScalar.S(chch[1]);
                    } else {
                        Console.WriteLine("odd constant box {0}/{1}", typ, chchh);
                    }
                } else if (chh == "newcc") {
                    return th.sub.unit.CCConst(JScalar.IA(1, ch));
                } else if (chh == "fcclist_new") {
                    StringBuilder sb = new StringBuilder("F");
                    for (int i = 1; i < ch.Length; i++) {
                        sb.Append(',');
                        object[] chch = ch[i] as object[];
                        for (int j = 1; j < chch.Length; j++) {
                            sb.Append(' ');
                            sb.Append(JScalar.I(chch[j]));
                        }
                    }
                    code = sb.ToString();
                } else {
                    Console.WriteLine("odd constant {0}", chh);
                }

                if (code == null)
                    return th.Constant(th.Scan(z[1]));

                CpsOp r;
                if (th.sub.unit.const_pool.TryGetValue(code, out r))
                    return r;

                return th.sub.unit.const_pool[code] = th.Constant(th.Scan(z[1]));
            };

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
                return CpsOp.MethodCall(mi, z); };
            // yuck.
            thandlers["mrl_count"] = thandlers["fvarlist_length"] = delegate(CpsOp[] z) {
                return CpsOp.Operator(Tokens.Int32, OpCodes.Conv_I4,
                    CpsOp.Operator(Tokens.IntPtr, OpCodes.Ldlen, z));
            };
            handlers["_newoftype"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_NewTypedScalar,
                    CpsOp.GetSField((FieldInfo)z[1])); };
            thandlers["newblankrwscalar"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_NewTypedScalar,
                    CpsOp.GetSField(Tokens.Kernel_AnyMO)); };
            thandlers["newtypedscalar"] = Methody(null, Tokens.Kernel_NewTypedScalar);
            // XXX - wrong order - problem?
            thandlers["fvarlist_item"] = delegate(CpsOp[] z) {
                return CpsOp.Operator(Tokens.Variable, OpCodes.Ldelem_Ref,
                    z[1], z[0]); };
            thandlers["mrl_index"] = delegate(CpsOp[] z) {
                return CpsOp.Operator(Tokens.P6any, OpCodes.Ldelem_Ref,
                    z[1], z[0]); };
            thandlers["vvarlist_item"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.VVarList_Item, z[1], z[0]); };
            thandlers["varhash_getindex"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.VarHash_get_Item, z[1], z[0]); };
            thandlers["varhash_setindex"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.VarHash_set_Item,
                    z[1], z[0], z[2]); };
            thandlers["vvarlist_sort"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_SortHelper,
                    CpsOp.CallFrame(), z[0], z[1]); };
            thandlers["bif_make"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.Builtins_Make,
                        CpsOp.CallFrame(), z[0]); };
            thandlers["callnext"] = Methody(Tokens.Variable,
                    Tokens.Builtins.GetMethod("CallNext"));
            thandlers["context_get"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_ContextHelper,
                    CpsOp.CallFrame(), z[0], z[1]); };
            thandlers["status_get"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_StatusHelper,
                    CpsOp.CallFrame(), z[0], z[1]); };
            thandlers["set_status"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall( Tokens.Kernel_SetStatus,
                    CpsOp.CallFrame(), z[0], z[1]); };
            thandlers["newscalar"] = Methody(null, Tokens.Kernel_NewROScalar);
            thandlers["newrwlistvar"] = Methody(null, Tokens.Kernel_NewRWListVar);
            thandlers["iter_hasflat"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_IterHasFlat,
                    z[0], CpsOp.BoolLiteral(true)); };
            thandlers["iter_hasarg"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_IterHasFlat,
                    z[0], CpsOp.BoolLiteral(false)); };
            thandlers["bif_map"] = delegate(CpsOp[] z) {
                return CpsOp.CpsCall(Tokens.Variable, Tokens.Builtins_MEMap,
                        CpsOp.NewArray(Tokens.Variable, z)); };
            thandlers["bif_grep"] = delegate(CpsOp[] z) {
                return CpsOp.CpsCall(Tokens.Variable, Tokens.Builtins_MEGrep,
                        CpsOp.NewArray(Tokens.Variable, z)); };
            thandlers["newrwscalar"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_NewRWScalar,
                    CpsOp.GetSField(Tokens.Kernel_AnyMO), z[0]); };
            thandlers["newvsubvar"] = delegate(CpsOp[] z) {
                return CpsOp.ConstructorCall(Tokens.SV_ctor,
                    CpsOp.BoolLiteral(true), CpsOp.BoolLiteral(false), z[0],
                    CpsOp.ConstructorCall(Tokens.SubViviHook_ctor,
                        z[1]), z[2]); };
            thandlers["newvhashvar"] = delegate(CpsOp[] z) {
                return CpsOp.ConstructorCall(Tokens.SV_ctor,
                    CpsOp.BoolLiteral(true), CpsOp.BoolLiteral(false), z[0],
                    CpsOp.ConstructorCall(Tokens.HashViviHook_ctor,
                        z[1], z[2]), z[3]); };
            thandlers["newvarrayvar"] = delegate(CpsOp[] z) {
                return CpsOp.ConstructorCall(Tokens.SV_ctor,
                    CpsOp.BoolLiteral(true), CpsOp.BoolLiteral(false), z[0],
                    CpsOp.ConstructorCall(Tokens.ArrayViviHook_ctor,
                        z[1], z[2]), z[3]); };
            thandlers["newvnewhashvar"] = delegate(CpsOp[] z) {
                return CpsOp.ConstructorCall(Tokens.SV_ctor,
                    CpsOp.BoolLiteral(true), CpsOp.BoolLiteral(false), z[0],
                    CpsOp.ConstructorCall(Tokens.NewHashViviHook_ctor,
                        z[1], z[2]), z[3]); };
            thandlers["newvnewarrayvar"] = delegate(CpsOp[] z) {
                return CpsOp.ConstructorCall(Tokens.SV_ctor,
                    CpsOp.BoolLiteral(true), CpsOp.BoolLiteral(false), z[0],
                    CpsOp.ConstructorCall(Tokens.NewArrayViviHook_ctor,
                        z[1], z[2]), z[3]); };
            thandlers["strbuf_append"] = delegate(CpsOp[] z) {
                return CpsOp.Sink(CpsOp.MethodCall(Tokens.StringBuilder_Append_String, z)); };
            thandlers["varhash_delete_key"] = delegate(CpsOp[] z) {
                return CpsOp.Sink(CpsOp.MethodCall(Tokens.VarHash_Remove, z)); };
            thandlers["note"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.TW_WriteLine,
                    CpsOp.MethodCall(Tokens.Console_get_Error), z[0]); };
            ConstructorInfo string_ctor = Tokens.String.GetConstructor(new Type[] {
                    typeof(char), Tokens.Int32 });
            thandlers["str_chr"] = delegate(CpsOp[] z) {
                return CpsOp.ConstructorCall(string_ctor,
                    CpsOp.Operator(typeof(char), OpCodes.Conv_U2, z),
                    CpsOp.IntLiteral(1));
            };
            MethodInfo itcommon = Tokens.Builtins.GetMethod("HashIter");
            thandlers["bif_hash_keys"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(itcommon, CpsOp.IntLiteral(0), z[0]); };
            thandlers["bif_hash_values"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(itcommon, CpsOp.IntLiteral(1), z[0]); };
            thandlers["bif_hash_kv"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(itcommon, CpsOp.IntLiteral(2), z[0]); };
            thandlers["bif_hash_pairs"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(itcommon, CpsOp.IntLiteral(3), z[0]); };
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
                return CpsOp.CpsCall((name == "EndWith" || name == "End") ? Tokens.Void : null, Tokens.RxFrame.GetMethod(name), x); };
            handlers["rxinit"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.SetField(Tokens.Frame_rx, CpsOp.CallFrame(),
                    CpsOp.ConstructorCall(Tokens.RxFrame_ctor,
                        th.Scan(z[1]), th.Scan(z[2]),
                        CpsOp.BoolLiteral(FixBool(z[3])),
                        CpsOp.BoolLiteral(FixBool(z[4])))); };
            handlers["rxpushb"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(Tokens.RxFrame_PushBacktrack,
                    CpsOp.GetField(Tokens.Frame_rx, CpsOp.CallFrame()),
                    CpsOp.LabelId(th.cpb.cx, JScalar.S(z[2]))); };
            handlers["ltm_push_alts"] = delegate(NamProcessor th, object[] z) {
                CpsOp ai = CpsOp.ConstructorCall(typeof(AltInfo).GetConstructor(new Type[] { typeof(LAD[]), typeof(string), typeof(int[]) }),
                        th.ProcessLADArr(z[1]),
                        CpsOp.StringLiteral(JScalar.S(z[2])),
                        CpsOp.LabelTable(th.cpb.cx, JScalar.SA(0,z[3])));
                return CpsOp.MethodCall(Tokens.RxFrame.GetMethod("LTMPushAlts"),
                    CpsOp.GetField(Tokens.Frame_rx, CpsOp.CallFrame()),
                    CpsOp.CallFrame(), th.Constant(ai)); };
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
            thandlers["run_dispatch"] = Methody(null, typeof(Lexer).GetMethod("RunDispatch"));
            handlers["rawcall"] = delegate(NamProcessor th, object[] z) {
                string name = JScalar.S(z[1]);
                CpsOp[] rst = JScalar.A<CpsOp>(2, z, th.Scan);
                Type[] tx = new Type[rst.Length - 1];
                for (int i = 0; i < tx.Length; i++)
                    tx[i] = rst[i+1].head.Returns;
                MethodInfo mi = rst[0].head.Returns.GetMethod(name, tx);
                return CpsOp.MethodCall(mi, rst); };
            handlers["rawscall"] = delegate(NamProcessor th, object[] z) {
                string name = JScalar.S(z[1]);
                int ixn = name.LastIndexOf(':');
                Type cpsrt = null;
                if (ixn >= 0) {
                    cpsrt = Type.GetType(name.Substring(ixn+1));
                    name = name.Substring(0, ixn);
                }
                int ix = name.LastIndexOf('.');
                CpsOp[] rst = JScalar.A<CpsOp>(2, z, th.Scan);
                int k = (cpsrt != null) ? 1 : 0;
                Type[] tx = new Type[rst.Length + k];
                for (int i = 0; i < rst.Length; i++)
                    tx[i+k] = rst[i].head.Returns;
                if (cpsrt != null) tx[0] = Tokens.Frame;
                MethodInfo mi = Type.GetType(name.Substring(0, ix))
                    .GetMethod(name.Substring(ix+1), tx);
                return CpsOp.CpsCall(cpsrt, mi, JScalar.A<CpsOp>(2, z, th.Scan)); };

            thandlers["var_islist"] = FieldGet(Tokens.Variable, "islist");
            thandlers["llhow_name"] = FieldGet(Tokens.STable, "name");
            thandlers["stab_what"] = FieldGet(Tokens.STable, "typeObject");
            thandlers["obj_llhow"] = FieldGet(Tokens.P6any, "mo");
            thandlers["varhash_clear"] = Methody(null, Tokens.VarHash.GetMethod("Clear"));
            thandlers["varhash_new"] = Constructy(Tokens.VarHash.GetConstructor(new Type[0]));
            thandlers["varhash_dup"] = Constructy(Tokens.VarHash.GetConstructor(new Type[]{ Tokens.VarHash }));
            thandlers["varhash_contains_key"] = Methody(null, Tokens.VarHash.GetMethod("ContainsKey"));
            thandlers["num_to_string"] = Methody(null, typeof(Utils).GetMethod("N2S"));
            thandlers["str_length"] = Methody(null, Tokens.String.GetMethod("get_Length"));
            thandlers["str_tonum"] = Methody(null, typeof(Utils).GetMethod("S2N"));
            thandlers["str_substring"] = Methody(null, Tokens.Builtins.GetMethod("LaxSubstring2"));
            thandlers["str_tolower"] = Methody(null, Tokens.String.GetMethod("ToLowerInvariant"));
            thandlers["str_toupper"] = Methody(null, Tokens.String.GetMethod("ToUpperInvariant"));
            thandlers["str_flip"] = Methody(null, typeof(Utils).GetMethod("StrFlip"));
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
            thandlers["stab_privatemethod"] = Methody(null, Tokens.STable.GetMethod("GetPrivateMethod"));
            thandlers["path_file_exists"] = Methody(null, typeof(File).GetMethod("Exists"));
            thandlers["path_dir_exists"] = Methody(null, typeof(Directory).GetMethod("Exists"));
            thandlers["path_combine"] = Methody(null, typeof(Path).GetMethod("Combine", new Type[] { Tokens.String, Tokens.String }));
            thandlers["path_change_ext"] = Methody(null, typeof(Path).GetMethod("ChangeExtension", new Type[] { Tokens.String, Tokens.String }));
            thandlers["path_any_exists"] = Methody(null, typeof(Builtins).GetMethod("FileOrDirExists"));
            thandlers["path_realpath"] = Methody(null, typeof(Path).GetMethod("GetFullPath"));
            thandlers["path_modified"] = Methody(null, typeof(Builtins).GetMethod("GetModTime"));
            handlers["_parametricrole"] = delegate(NamProcessor th, object[] z) { return th.FillParamRole(); };
            handlers["_addmethod"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(Tokens.DMO_AddMethod, th.Scan(z[1]), CpsOp.IntLiteral(JScalar.I(z[2])), th.Scan(z[3]), th.Scan(z[4])); };
            thandlers["_invalidate"] = Methody(null, Tokens.STable.GetMethod("Invalidate"));
            handlers["do_require"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(Tokens.Kernel.GetMethod("DoRequire"),
                    CpsOp.StringLiteral(JScalar.S(z[1]))); };
            thandlers["obj_is_defined"] = Methody(null, Tokens.P6any.GetMethod("IsDefined"));
            thandlers["how"] = Methody(Tokens.P6any, Tokens.P6any.GetMethod("HOW"));
            thandlers["obj_what"] = Methody(null, Tokens.P6any.GetMethod("GetTypeObject"));
            thandlers["obj_isa"] = Methody(null, Tokens.P6any.GetMethod("Isa"));
            thandlers["obj_does"] = Methody(null, Tokens.P6any.GetMethod("Does"));
            thandlers["obj_can"] = Methody(null, Tokens.Builtins.GetMethod("can"));
            thandlers["obj_newblank"] = Constructy(Tokens.P6opaque_ctor);
            thandlers["cursor_start"] = Constructy(Tokens.Cursor.GetConstructor(new Type[] { Tokens.P6any, Tokens.String, Tokens.P6any }));
            thandlers["cursor_pos"] = FieldGet(Tokens.Cursor, "pos");
            thandlers["cursor_from"] = FieldGet(Tokens.Cursor, "from");
            thandlers["cursor_butpos"] = Methody(null, Tokens.Cursor.GetMethod("At"));
            thandlers["cursor_backing"] = Methody(null, Tokens.Cursor.GetMethod("GetBacking"));
            thandlers["cursor_ast"] = Methody(null, Tokens.Cursor.GetMethod("AST"));
            thandlers["cursor_dows"] = Methody(null, Tokens.Cursor.GetMethod("SimpleWS"));
            thandlers["cursor_item"] = Methody(null, Tokens.Cursor.GetMethod("GetKey"));
            thandlers["cursor_unpackcaps"] = Methody(null, Tokens.Cursor.GetMethod("UnpackCaps"));
            thandlers["cursor_O"] = Methody(null, Tokens.Cursor.GetMethod("O"));
            thandlers["cursor_synthetic"] = Methody(null, Tokens.Cursor.GetMethod("Synthetic"));
            thandlers["cursor_fresh"] = Methody(null, Tokens.Cursor.GetMethod("FreshClass"));
            thandlers["cursor_unmatch"] = Methody(null, Tokens.Cursor.GetMethod("UnMatch"));
            thandlers["cursor_reduced"] = Methody(null, Tokens.Cursor.GetMethod("Reduced"));
            thandlers["rxstripcaps"] = Methody(null, Tokens.Cursor.GetMethod("StripCaps"));

            thandlers["prog"] = CpsOp.Sequence;

            thandlers["bif_gettimeofday"] = SimpleB("GetTimeOfDay");
            thandlers["bif_array_constructor"] = SimpleB("ArrayConstructor");
            thandlers["bif_numand"] = SimpleB("NumAnd");
            thandlers["bif_numor"] = SimpleB("NumOr");
            thandlers["bif_numxor"] = SimpleB("NumXor");
            thandlers["bif_numlshift"] = SimpleB("NumLShift");
            thandlers["bif_numrshift"] = SimpleB("NumRShift");
            thandlers["bif_numcompl"] = SimpleB("NumCompl");
            thandlers["bif_ord"] = SimpleB("Ord");
            thandlers["bif_chr"] = SimpleB("Chr");
            thandlers["bif_postinc"] = SimpleB("PostIncrement");
            thandlers["bif_numeq"] = SimpleB("NumericEq");
            thandlers["bif_numne"] = SimpleB("NumericNe");
            thandlers["bif_numle"] = SimpleB("NumericLe");
            thandlers["bif_numlt"] = SimpleB("NumericLt");
            thandlers["bif_numge"] = SimpleB("NumericGe");
            thandlers["bif_numgt"] = SimpleB("NumericGt");
            thandlers["bif_rand"] = SimpleB("GetRandom");
            thandlers["bif_streq"] = SimpleB("StringEq");
            thandlers["bif_strne"] = SimpleB("StringNe");
            thandlers["bif_strle"] = SimpleB("StringLe");
            thandlers["bif_strlt"] = SimpleB("StringLt");
            thandlers["bif_strge"] = SimpleB("StringGe");
            thandlers["bif_strgt"] = SimpleB("StringGt");
            thandlers["bif_plus"] = SimpleB("Plus");
            thandlers["bif_minus"] = SimpleB("Minus");
            thandlers["bif_mod"] = SimpleB("Mod");
            thandlers["bif_mul"] = SimpleB("Mul");
            thandlers["bif_divide"] = SimpleB("Divide");
            thandlers["bif_not"] = SimpleB("Not");
            thandlers["bif_now"] = SimpleB("GetTimeOfDay");
            thandlers["bif_negate"] = SimpleB("Negate");
            thandlers["bif_chars"] = SimpleB("Chars");
            thandlers["bif_substr3"] = SimpleB("Substr3");
            thandlers["bif_simple_eval"] = SimpleB("SimpleEval");
            thandlers["bif_rat_approx"] = SimpleB("RatApprox");
            thandlers["bif_coerce_to_int"] = SimpleB("CoerceToInt");
            thandlers["bif_coerce_to_num"] = SimpleB("CoerceToNum");

            thandlers["bif_defined"] = Contexty("mro_defined");
            thandlers["bif_bool"] = Contexty("mro_Bool");
            thandlers["bif_num"] = Contexty("mro_Numeric");
            thandlers["bif_str"] = Contexty("mro_Str");
            thandlers["bif_item"] = Contexty("mro_item");
            thandlers["bif_list"] = Contexty("mro_list");
            thandlers["bif_hash"] = Contexty("mro_hash");
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
            thandlers["bif_cross"] = Methody(Tokens.Variable, Tokens.Builtins.GetMethod("MECross"));
            thandlers["bif_zip"] = Methody(Tokens.Variable, Tokens.Builtins.GetMethod("MEZip"));
            thandlers["var_get_var"] = Methody(null, Tokens.Variable.GetMethod("GetVar"));
            thandlers["var_new_tied"] = Constructy(typeof(TiedVariable).GetConstructor(new Type[] { Tokens.STable, Tokens.P6any, Tokens.P6any, Tokens.P6any }));
            thandlers["obj_typename"] = Methody(null, Tokens.P6any.GetMethod("GetTypeName"));
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
            thandlers["to_json"] = Methody(null, typeof(JsonWriter).GetMethod("ToJson"));
            thandlers["from_json"] = Methody(null, typeof(JsyncReader).GetMethod("FromJson"));
            thandlers["frame_caller"] = FieldGet(Tokens.Frame, "caller");
            thandlers["frame_file"] = Methody(null, Tokens.Frame.GetMethod("ExecutingFile"));
            thandlers["frame_line"] = Methody(null, Tokens.Frame.GetMethod("ExecutingLine"));
            thandlers["frame_hint"] = Methody(null, Tokens.Frame.GetMethod("LexicalFind"));
            thandlers["treader_getc"] = Methody(null, typeof(TextReader).GetMethod("Read", new Type[0]));
            thandlers["treader_slurp"] = Methody(null, typeof(TextReader).GetMethod("ReadToEnd"));
            thandlers["treader_getline"] = Methody(null, typeof(TextReader).GetMethod("ReadLine"));
            thandlers["treader_stdin"] = Methody(null, typeof(Kernel).GetMethod("OpenStdin"));
            ConstructorInfo treader_open = typeof(StreamReader).GetConstructor(new Type[1] { Tokens.String });
            thandlers["treader_open"] = delegate(CpsOp[] z) {
                return CpsOp.Widen(typeof(TextReader),
                    CpsOp.ConstructorCall(treader_open, z)); };

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
                return CpsOp.CpsCall(cps, mi, cpses); };
        }

        static Func<CpsOp[], CpsOp> RxCall(Type cps, string name) {
            MethodInfo mi = Tokens.RxFrame.GetMethod(name);
            return delegate(CpsOp[] cpses) {
                CpsOp[] n = new CpsOp[cpses.Length + 1];
                Array.Copy(cpses, 0, n, 1, cpses.Length);
                n[0] = CpsOp.GetField(Tokens.Frame_rx, CpsOp.CallFrame());
                return CpsOp.CpsCall(cps, mi, n); };
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
            FieldInfo f = Tokens.STable.GetField(name);
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

        public CpsOp ProcessLADArr(object l) {
            int o = sub.unit.thaw_heap.Count;
            sub.unit.EmitLADArr(l);
            return CpsOp.MethodCall(Tokens.RU_LoadLADArr,
                CpsOp.GetSField(sub.unit.rtunit), CpsOp.IntLiteral(o));
        }

        public CpsOp ProcessLAD(object l) {
            int o = sub.unit.thaw_heap.Count;
            sub.unit.EmitLAD(l);
            return CpsOp.MethodCall(Tokens.RU_LoadLAD,
                CpsOp.GetSField(sub.unit.rtunit), CpsOp.IntLiteral(o));
        }

        public void SubInfoCtor(int ix, List<CpsOp> thaw) {
            CpsOp[] args = new CpsOp[3];

            int b = sub.unit.thaw_heap.Count;

            int spec = 0;

            if ((sub.flags & StaticSub.UNSAFE) != 0)
                spec |= RuntimeUnit.SUB_IS_UNSAFE;
            if (sub.sclass != "Sub")
                spec |= RuntimeUnit.SUB_HAS_TYPE;
            if (sub.protopad != null)
                spec |= RuntimeUnit.MAKE_PROTOPAD;
            if (sub.parametric_role_hack != null)
                spec |= RuntimeUnit.SUB_IS_PARAM_ROLE;

            args[0] = CpsOp.GetSField(sub.unit.rtunit);
            args[1] = CpsOp.IntLiteral(b);
            args[2] = CpsOp.DBDLiteral(cpb.mb);

            sub.unit.EmitInt(ix);
            sub.unit.EmitByte(spec);
            sub.unit.EmitStr(sub.unit.name + " " +
                    (sub.name == "ANON" ? cpb.mb.Name : sub.name));
            sub.unit.EmitIntArray(cpb.cx.lineBuffer.ToArray());
            sub.unit.EmitXref(sub.outer);
            sub.unit.EmitLAD(sub.ltm);
            sub.unit.EmitIntArray(cpb.cx.ehspanBuffer.ToArray());
            sub.unit.EmitStrArray(cpb.cx.ehlabelBuffer.ToArray());
            sub.unit.EmitInt(cpb.Spills());

            List<string> dylexn = new List<string>();
            List<int> dylexi = new List<int>();
            foreach (KeyValuePair<string, Lexical> kv in sub.lexicals) {
                int index = (kv.Value is LexVarish) ?
                    ((LexVarish)kv.Value).index : -1;
                if (index >= 0) {
                    dylexn.Add(kv.Key);
                    dylexi.Add(index);
                }
            }
            sub.unit.EmitStrArray(dylexn.ToArray());
            sub.unit.EmitIntArray(dylexi.ToArray());

            if ((spec & RuntimeUnit.SUB_HAS_TYPE) != 0)
                sub.unit.EmitXref(sub.GetCorePackage(sub.sclass).own_xref);

            if (sub.parametric_role_hack != null)
                sub.unit.EmitXref(sub.parametric_role_hack);

            thaw.Add(CpsOp.SetSField(sub.subinfo,
                CpsOp.MethodCall(Tokens.RU_LoadSubInfo, args)));
            if (sub.protopad != null)
                thaw.Add(CpsOp.SetSField(sub.protopad,
                    CpsOp.GetField(Tokens.SubInfo_protopad,
                        CpsOp.GetSField(sub.subinfo))));
        }

        void EnterCode(List<object> frags) {
            List<object> latefrags = new List<object>();
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
                                ls.def.Resolve<StaticSub>() } } });
                } else if (kv.Value is LexSimple) {
                    LexSimple ls = kv.Value as LexSimple;
                    int f = ls.flags;
                    if ((f & LexSimple.NOINIT) != 0) continue;

                    object bit;
                    FieldInfo tc = ls.type == null ?
                        Tokens.Kernel_AnyMO :
                        ls.type.Resolve<Class>().metaObject;
                    if ((f & (LexSimple.HASH | LexSimple.LIST)) != 0) {
                        string s = ((f & LexSimple.HASH) != 0) ? "Hash" : "Array";
                        bit = new object[] { new JScalar("methodcall"),
                            new JScalar("new"), new JScalar(""),
                            new object[] { new JScalar("fetch"), new object[] { new JScalar("corelex"), new JScalar(s) } },
                            new object[] { new JScalar("corelex"), new JScalar(s) } };
                    } else {
                        bit = new object[] { new JScalar("_newoftype"), tc };
                    }
                    frags.Add(new object[] { new JScalar("scopedlex"),
                        new JScalar(kv.Key), bit });
                } else if (kv.Value is LexLabel) {
                    frags.Add(new object[] { new JScalar("scopedlex"),
                        new JScalar(kv.Key),
                        new object[] { new JScalar("_newlabel"),
                            new JScalar(kv.Key) } });
                } else if (kv.Value is LexDispatch) {
                    latefrags.Add(new object[] { new JScalar("scopedlex"),
                        new JScalar(kv.Key),
                        new object[] { new JScalar("_newdispatch"),
                            new JScalar(kv.Key) } });
                }
            }
            foreach (object lf in latefrags) frags.Add(lf);
        }

        CpsOp FillParamRole() {
            ParametricRole pr =
                sub.parametric_role_hack.Resolve<ParametricRole>();
            CpsOp mo = CpsOp.PeekLet("!mo", Tokens.STable);
            CpsOp to = CpsOp.PeekLet("!to", Tokens.P6opaque);
            CpsOp pa = CpsOp.PeekLet("!pa", Tokens.VarHash);

            List<CpsOp> build = new List<CpsOp>();

            CpsOp[] supers = new CpsOp[pr.superclasses.Length];
            for (int i = 0; i < supers.Length; i++)
                supers[i] = CpsOp.GetSField(pr.superclasses[i].Resolve<Class>().metaObject);
            build.Add( CpsOp.MethodCall(Tokens.DMO_FillRole,
                mo, CpsOp.NewArray(Tokens.STable, supers),
                CpsOp.NewArray(Tokens.STable)) );

            foreach (Method m in pr.methods) {
                CpsOp name = (m.name != null) ? CpsOp.StringLiteral(m.name) :
                    Scan(new object[] { new JScalar("obj_getstr"), m.cname });
                CpsOp var  = RawAccessLex("scopedlex", m.var, null);

                build.Add(CpsOp.MethodCall(Tokens.DMO_AddMethod,
                    mo, CpsOp.IntLiteral(m.kind), name,
                    CpsOp.MethodCall(Tokens.Variable_Fetch, var)));
            }

            foreach (Attribute a in pr.attributes) {
                CpsOp name = CpsOp.StringLiteral(a.name);
                CpsOp publ = CpsOp.BoolLiteral(a.publ);
                CpsOp init = a.ivar == null ? CpsOp.Null(Tokens.P6any) :
                    RawAccessLex("scopedlex", a.ivar, null);
                CpsOp type = a.type == null ?
                    CpsOp.GetSField(Tokens.Kernel_AnyMO) :
                    CpsOp.GetSField(a.type.Resolve<Class>().metaObject);
                build.Add(CpsOp.MethodCall(Tokens.DMO_AddAttribute,
                    mo, name, publ, init, type));
            }

            build.Add(CpsOp.MethodCall(Tokens.DMO_Invalidate, mo));
            if (sub.sig != null) {
                object[] rsig = (object[]) sub.sig;
                foreach (object se in rsig) {
                    string slot = JScalar.S( ((object[])se)[2] );
                    if (slot != null)
                        build.Add(CpsOp.MethodCall(Tokens.VarHash_set_Item, pa, CpsOp.StringLiteral(slot), RawAccessLex("scopedlex", slot, null)));
                }
            }

            build.Add(RawAccessLex("scopedlex", "*params", pa));
            build.Add(CpsOp.SetField(Tokens.P6opaque_slots, to,
                        CpsOp.Null(typeof(object[]))));
            build.Add(CpsOp.SetField(Tokens.DMO_typeObject, mo, to));
            build.Add(CpsOp.CpsReturn(CpsOp.MethodCall(Tokens.Kernel_NewROScalar, to)));

            return CpsOp.Let("!mo", CpsOp.ConstructorCall(Tokens.DMO_ctor,
                        CpsOp.StringLiteral(pr.name)),
                    CpsOp.Let("!to", CpsOp.ConstructorCall(Tokens.P6opaque_ctor, mo),
                        CpsOp.Let("!pa", CpsOp.ConstructorCall(Tokens.VarHash.GetConstructor(new Type[0])),
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
            } else if (sub.hint_hack != null) {
                enter.Insert(0, new JScalar("prog"));
                enter.Add(new object[] { new JScalar("_hintset"),
                    sub.hint_hack[1], b });
                enter.Add(new object[] { new JScalar("return") });
                b = enter.ToArray();
            } else if (sub.augment_hack != null) {
                enter = new List<object>();
                object[] tuples = (object[]) sub.augment_hack;
                for (int i = 1; i < tuples.Length; i++) {
                    object[] t = (object[]) tuples[i];
                    string k1 = JScalar.S(t[0]);
                    string k2 = JScalar.S(t[1]);
                    int ik = (k2 == "normal") ? 0 : (k2 == "private") ? 1 : 2;
                    ik += ((k1 == "only") ? 0 : (k1 == "proto") ? 4 : 8);
                    enter.Add(new object[] {
                        new JScalar( "_addmethod" ),
                        new object[] { new JScalar("letvar"), new JScalar("!mo") },
                        new JScalar(ik.ToString()),
                        new object[] { new JScalar("str"), t[2] },
                        new object[] { new JScalar("fetch"), new object[] { new JScalar("scopedlex"), t[3] } } });
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
                    new object[] { new JScalar("xspan"),
                        new JScalar("rstart"), new JScalar("rend"),
                        new JScalar("0"), b,
                        new JScalar("4"), new JScalar(""), new JScalar("rend") } });
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
            if (CLRBackend.Verbose > 1)
                Console.WriteLine("enter " + tag);
            CpsOp r = handler(this, rnode);
            if (CLRBackend.Verbose > 1)
                Console.WriteLine("exit " + tag);
            return r;
        }
    }

    public class CLRBackend {
        internal AssemblyBuilder ab;
        internal ModuleBuilder mob;
        internal TypeBuilder tb;

        [ThreadStatic] internal static CLRBackend Current;

        internal int nextarray;
        internal int nextspill;
        internal int nextlabel;
        internal Unit unit;
        internal string dir;

        internal int constants;
        internal List<CpsOp> thaw = new List<CpsOp>();

        public static int Verbose =
            int.Parse(Environment.GetEnvironmentVariable("NIECZA_CODEGEN_TRACE") ?? "0");
        public static bool Verifiable =
            Environment.GetEnvironmentVariable("NIECZA_CODEGEN_UNVERIFIABLE") != null ? false : true;

        CLRBackend(string dir, string mobname, string filename) {
            AssemblyName an = new AssemblyName(mobname);
            this.dir = dir;
            ab = AppDomain.CurrentDomain.DefineDynamicAssembly(an,
                    (filename == null ? AssemblyBuilderAccess.Run :
                        AssemblyBuilderAccess.Save), dir);
            mob = filename == null ? ab.DefineDynamicModule(mobname) :
                ab.DefineDynamicModule(mobname, filename);

            tb = mob.DefineType(mobname, TypeAttributes.Public |
                    TypeAttributes.Sealed | TypeAttributes.Abstract |
                    TypeAttributes.Class | TypeAttributes.BeforeFieldInit);
        }

        void EncodeSignature(List<CpsOp> thaw, StaticSub obj) {
            if (obj.sig == null) return;
            int b = obj.unit.thaw_heap.Count;

            List<int> sig_i   = new List<int>();
            List<object> sig_r = new List<object>();
            object[] rsig = (object[]) obj.sig;
            foreach (object p in rsig) {
                object[] param = (object[]) p;
                string   name  = JScalar.S(param[0]);
                int      flags = JScalar.I(param[1]);
                string   slot  = JScalar.S(param[2]);
                string[] names = JScalar.SA(0, param[3]);
                Xref     deflt = Xref.from(param[4]);
                Xref     type  = Xref.from(param[5]);

                sig_r.Add(name);
                foreach (string n in names)
                    sig_r.Add(n);
                int ufl = 0;
                if ((flags & 4) != 0) ufl |= SubInfo.SIG_F_RWTRANS;
                else if ((flags & 64) != 0) ufl |= SubInfo.SIG_F_READWRITE;

                if ((flags & 384) != 0) ufl |= SubInfo.SIG_F_BINDLIST;
                if ((flags & 512) != 0) ufl |= SubInfo.SIG_F_DEFOUTER;
                if ((flags & 1024) != 0) ufl |= SubInfo.SIG_F_INVOCANT;
                if ((flags & 2048) != 0) ufl |= SubInfo.SIG_F_MULTI_IGNORED;
                if (deflt != null) {
                    ufl |= SubInfo.SIG_F_HASDEFAULT;
                    sig_r.Add(deflt);
                }
                if (type != null) {
                    ufl |= SubInfo.SIG_F_HASTYPE;
                    sig_r.Add(type);
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
            obj.unit.EmitIntArray(sig_i.ToArray());
            obj.unit.EmitInt(sig_r.Count);
            foreach (object o in sig_r) {
                if (o is string) {
                    obj.unit.EmitStr((string)o);
                } else {
                    obj.unit.EmitStr(null);
                    obj.unit.EmitXref((Xref)o);
                }
            }
            thaw.Add(CpsOp.MethodCall(Tokens.RU_LoadSignature,
                CpsOp.GetSField(obj.unit.rtunit), CpsOp.GetSField(obj.subinfo),
                CpsOp.IntLiteral(b)));
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
                thaw.Add(CpsOp.Sink(CpsOp.MethodCall(Tokens.Kernel_BootModule,
                    CpsOp.StringLiteral(dp), CpsOp.DBDLiteral(CLRBackend.GetUnit(dp).clrType.GetMethod("BOOT")))));
            }
            int unit_slot = thaw.Count;
            thaw.Add(null);

            NamProcessor[] aux = new NamProcessor[unit.xref.Length];
            unit.VisitSubsPreorder(delegate(int ix, StaticSub obj) {
                if (Verbose > 0) Console.WriteLine("sub1 {0}", obj.name);
                CpsBuilder cpb = new CpsBuilder(this,
                    Unit.SharedName('C', ix, obj.name), false);
                NamProcessor np = aux[ix] = new NamProcessor(cpb, obj);
                np.MakeBody();
            });

            unit.VisitPackages(delegate(int ix, Package pkg) {
                if (Verbose > 0) Console.WriteLine("pkg2 {0}", pkg.name);
                if (!(pkg is ModuleWithTypeObject))
                    return;
                ModuleWithTypeObject m = (ModuleWithTypeObject) pkg;
                FieldInfo km = null;
                FieldInfo kp = null;
                bool existing_mo = false;
                if (unit.name == "CORE") {
                    km = Tokens.Kernel.GetField(m.name + "MO");
                    kp = Tokens.Kernel.GetField(m.name + "P");
                    existing_mo = km != null && km.IsInitOnly;
                }
                thaw.Add(CpsOp.SetSField(m.metaObject, existing_mo ?
                        CpsOp.GetSField(km) :
                        CpsOp.ConstructorCall(Tokens.DMO_ctor,
                            new CpsOp[] { CpsOp.StringLiteral(m.name) })));
                thaw.Add(CpsOp.Operator(Tokens.Void, OpCodes.Stelem_Ref,
                    CpsOp.GetField(Tokens.RU_xref,CpsOp.GetSField(unit.rtunit)),
                    CpsOp.IntLiteral(ix), CpsOp.GetSField(m.metaObject)));

                if (m is Role) {
                    Role r = (Role) m;
                    CpsOp[] super = new CpsOp[ r.superclasses.Length ];
                    for (int i = 0; i < super.Length; i++)
                        super[i] = CpsOp.GetSField(r.superclasses[i].Resolve<Class>().metaObject);

                    thaw.Add(CpsOp.MethodCall(Tokens.DMO_FillRole,
                        CpsOp.GetSField(r.metaObject),
                        CpsOp.NewArray(Tokens.STable, super),
                        CpsOp.NewArray(Tokens.STable)));
                } else if (m is ParametricRole) {
                    // The heavy lifting is done in WrapBody
                } else if (m is Class) {
                    Class r = (Class) m;
                    List<string> all_slot = new List<string>();
                    CpsOp[] super = new CpsOp[ r.superclasses.Length ];
                    CpsOp[] mro   = new CpsOp[ r.linearized_mro.Length ];
                    for (int i = 0; i < super.Length; i++)
                        super[i] = CpsOp.GetSField(r.superclasses[i].Resolve<Class>().metaObject );
                    for (int i = 0; i < mro.Length; i++) {
                        Class p = r.linearized_mro[i].Resolve<Class>();
                        mro[i] = CpsOp.GetSField(p.metaObject);
                        foreach (Attribute a in p.attributes)
                            all_slot.Add(a.name);
                    }

                    thaw.Add(CpsOp.MethodCall(Tokens.DMO_FillClass,
                        CpsOp.GetSField(r.metaObject),
                        CpsOp.StringArray(false, all_slot.ToArray()),
                        CpsOp.NewArray(Tokens.STable, super),
                        CpsOp.NewArray(Tokens.STable, mro)));
                }

                thaw.Add(CpsOp.SetSField(m.typeObject,
                    CpsOp.ConstructorCall(Tokens.P6opaque_ctor, new CpsOp[] {
                        CpsOp.GetSField(m.metaObject) })));
                thaw.Add(CpsOp.SetField(Tokens.P6opaque_slots,
                    CpsOp.UnboxAny(Tokens.P6opaque, CpsOp.GetSField(m.typeObject)),
                        CpsOp.Null(typeof(object[]))));
                thaw.Add(CpsOp.SetField(Tokens.DMO_typeObject,
                    CpsOp.GetSField(m.metaObject), CpsOp.GetSField(m.typeObject)));
                thaw.Add(CpsOp.SetSField(m.typeVar, CpsOp.MethodCall(
                    Tokens.Kernel_NewROScalar, CpsOp.GetSField(m.typeObject))));

                if (kp != null)
                    thaw.Add(CpsOp.SetSField(kp, CpsOp.GetSField(m.typeObject)));
                if (km != null && !km.IsInitOnly)
                    thaw.Add(CpsOp.SetSField(km, CpsOp.GetSField(m.metaObject)));
            });

            unit.VisitSubsPreorder(delegate(int ix, StaticSub obj) {
                if (Verbose > 0) Console.WriteLine("sub2 {0}", obj.name);
                aux[ix].SubInfoCtor(ix, thaw);
            });

            unit.VisitPackages(delegate(int ix, Package p) {
                if (Verbose > 0) Console.WriteLine("pkg3 {0}", p.name);
                ModuleWithTypeObject m = p as ModuleWithTypeObject;
                if (m == null) return;
                foreach (object o in m.exports) {
                    thaw.Add(CpsOp.SetField(Tokens.BValue_v,
                        CpsOp.MethodCall(Tokens.Kernel_GetVar,
                            CpsOp.StringArray(false, JScalar.SA(0,o))),
                        CpsOp.GetSField(m.typeVar)));
                }
                if (m is ParametricRole) return;
                Method[] methods = (m is Class) ? ((Class)m).methods :
                    ((Role)m).methods;
                Attribute[] attrs = (m is Class) ? ((Class)m).attributes :
                    ((Role)m).attributes;
                int b = unit.thaw_heap.Count;
                unit.EmitInt(ix);
                unit.EmitInt(methods.Length);
                foreach (Method me in methods) {
                    unit.EmitInt(me.kind);
                    unit.EmitStr(me.name);
                    unit.EmitXref(me.body);
                }
                unit.EmitInt(attrs.Length);
                foreach (Attribute a in attrs) {
                    unit.EmitStr(a.name);
                    unit.EmitByte(a.publ ? 1 : 0);
                    unit.EmitXref(a.ibody);
                    unit.EmitXref(a.type);
                }
                thaw.Add(CpsOp.MethodCall(Tokens.RU_LoadClassMembers,
                    CpsOp.GetSField(unit.rtunit), CpsOp.IntLiteral(b)));
                thaw.Add(CpsOp.SetField(Tokens.DMO_how, CpsOp.GetSField(m.metaObject),
                    CpsOp.MethodCall(Tokens.Kernel.GetMethod("BoxRaw").MakeGenericMethod(Tokens.STable), CpsOp.GetSField(m.metaObject), CpsOp.GetSField( ((Class) unit.GetCorePackage("ClassHOW")).metaObject))));
            });

            unit.VisitSubsPostorder(delegate(int ix, StaticSub obj) {
                if (Verbose > 0) Console.WriteLine("sub3 {0}", obj.name);
                EncodeSignature(thaw, obj);

                if (obj.is_phaser >= 0)
                    thaw.Add(CpsOp.MethodCall(Tokens.Kernel_AddPhaser,
                        CpsOp.IntLiteral(obj.is_phaser),
                        CpsOp.GetField(Tokens.SubInfo_protosub,
                            CpsOp.GetSField(obj.subinfo))));

                if (obj.exports != null) {
                    foreach (object o in obj.exports) {
                        thaw.Add(CpsOp.SetField(Tokens.BValue_v,
                            CpsOp.MethodCall(Tokens.Kernel_GetVar,
                                CpsOp.StringArray(false, JScalar.SA(0,o))),
                            CpsOp.MethodCall(Tokens.Kernel_NewROScalar,
                                CpsOp.GetField(Tokens.SubInfo_protosub,
                                    CpsOp.GetSField(obj.subinfo)))));
                    }
                }

                foreach (KeyValuePair<string,Lexical> l in obj.lexicals) {
                    if (l.Value is LexCommon) {
                        LexCommon lx = (LexCommon)l.Value; /* XXX cname */
                        thaw.Add(CpsOp.SetSField(lx.stg,
                            CpsOp.MethodCall(Tokens.Kernel_GetVar,
                                CpsOp.StringArray(false, lx.path))));
                    } else if (l.Value is LexHint) {
                        LexHint lx = (LexHint)l.Value;
                        thaw.Add(CpsOp.SetSField(lx.stg,
                            CpsOp.MethodCall(Tokens.SubInfo_AddHint,
                                CpsOp.GetSField(obj.subinfo),
                                    CpsOp.StringLiteral(l.Key))));
                    } else if (l.Value is LexSub) {
                        LexSub lx = (LexSub)l.Value;
                        if ((obj.flags & StaticSub.SPAD_EXISTS) == 0) continue;
                        SetProtolex(obj, l.Key, lx, CpsOp.MethodCall(
                            Tokens.Kernel_NewROScalar, CpsOp.GetField(Tokens.SubInfo_protosub,
                                CpsOp.GetSField(lx.def.Resolve<StaticSub>().subinfo))));
                    } else if (l.Value is LexSimple) {
                        LexSimple lx = (LexSimple)l.Value;
                        if ((obj.flags & StaticSub.SPAD_EXISTS) == 0) continue;
                        string type = ((lx.flags & LexSimple.HASH) != 0) ? "Hash" :
                            ((lx.flags & LexSimple.LIST) != 0) ? "Array" : null;
                        if (type != null) {
                            Class c = (Class) obj.GetCorePackage(type);
                            SetProtolex(obj, l.Key, lx, CpsOp.SubyCall(true, "",
                                CpsOp.StringLiteral("new"),
                                CpsOp.GetSField(c.typeObject),
                                CpsOp.GetSField(c.typeVar)));
                        } else {
                            FieldInfo tc = lx.type == null ?
                                Tokens.Kernel_AnyMO :
                                lx.type.Resolve<Class>().metaObject;
                            SetProtolex(obj, l.Key, lx, CpsOp.MethodCall(
                                Tokens.Kernel_NewTypedScalar,
                                CpsOp.GetSField(tc)));
                        }
                    }
                }
            });

            // this needs to come late so that Array and Hash are available
            foreach (object le in unit.log) {
                object[] lea = (object[]) le;
                string t = ((JScalar)lea[0]).str;
                if (t == "pkg" || t == "var") {
                    CpsOp sa = CpsOp.StringArray(false, JScalar.SA(0, lea[1]));
                    if (t == "pkg") {
                        thaw.Add(CpsOp.MethodCall(Tokens.Kernel_CreatePath, sa));
                    } else {
                        thaw.Add(CpsOp.Sink(CpsOp.MethodCall(Tokens.Kernel_GetVar, sa)));
                    }
                }
            }

            thaw.Add(CpsOp.MethodCall(Tokens.SubInfo.GetMethod("SetStringHint"),
                CpsOp.GetSField(unit.mainline_ref.Resolve<StaticSub>().subinfo),
                CpsOp.StringLiteral("$?FILE"), CpsOp.StringLiteral(unit.filename ?? "(eval)")));
            thaw.Add(CpsOp.MethodCall(Tokens.Kernel_FirePhasers,
                CpsOp.IntLiteral(2), CpsOp.BoolLiteral(false)));
            if (asmain)
                thaw.Add(CpsOp.MethodCall(Tokens.Kernel_FirePhasers,
                    CpsOp.IntLiteral(0), CpsOp.BoolLiteral(false)));
            // settings are incomplete modules and have no mainline to run
            if (unit.bottom_ref == null) {
                Type dty = typeof(Dictionary<string,Object>);
                FieldInfo lex = Tokens.Frame.GetField("lex");
                MethodInfo set = dty.GetMethod("set_Item");
                thaw.Add(CpsOp.SetField(lex, CpsOp.CallFrame(),
                    CpsOp.ConstructorCall(dty.GetConstructor(new Type[0]), new CpsOp[0])));
                string s = unit.setting;
                StaticSub m = unit.mainline_ref.Resolve<StaticSub>();
                while (s != null) {
                    thaw.Add(CpsOp.MethodCall(set,
                        CpsOp.GetField(lex, CpsOp.CallFrame()),
                        CpsOp.StringLiteral("*resume_" + s),
                        CpsOp.MethodCall(Tokens.Kernel_NewROScalar,
                            CpsOp.GetField(Tokens.SubInfo_protosub,
                                CpsOp.GetSField(m.subinfo)))));
                    Unit su = CLRBackend.GetUnit(s);
                    s = su.setting;
                    m = su.mainline_ref.Resolve<StaticSub>();
                }
                thaw.Add(CpsOp.CpsReturn(CpsOp.SubyCall(false,"",
                    CpsOp.GetField(Tokens.SubInfo_protosub,
                        CpsOp.GetSField(m.subinfo)))));
            } else {
                thaw.Add(CpsOp.CpsReturn(
                    CpsOp.MethodCall(Tokens.Kernel_NewROScalar,
                        CpsOp.GetSField(Tokens.Kernel_AnyP))));
            }

            List<CpsOp> tdep_rtu = new List<CpsOp>();
            foreach (Unit td in unit.id_to_tdep)
                tdep_rtu.Add(td == unit ? CpsOp.Null(Tokens.RuntimeUnit) :
                        CpsOp.GetSField(td.rtunit));
            List<CpsOp> unit_load = new List<CpsOp>();
            unit_load.Add(null);
            unit_load.Add(unit.EmitCCConsts());

            unit_load[0] = CpsOp.SetSField(unit.rtunit, CpsOp.ConstructorCall(
                Tokens.RuntimeUnit.GetConstructor(new Type[] { typeof(byte[]), typeof(RuntimeUnit[]), typeof(int) }),
                CpsOp.NewByteArray(typeof(byte), unit.thaw_heap.ToArray()),
                CpsOp.NewArray(Tokens.RuntimeUnit, tdep_rtu.ToArray()),
                CpsOp.IntLiteral(unit.xref.Length)));


            thaw[unit_slot] = CpsOp.Sequence(unit_load.ToArray());

            CpsBuilder boot = new CpsBuilder(this, "BOOT", true);
            boot.Build(CpsOp.Sequence(thaw.ToArray()));

            if (asmain)
                DefineMainMethod(unit.name, boot.mb);
        }

        void Finish(string filename) {
            tb.CreateType();

            ab.Save(filename);
        }

        void DefineMainMethod(string name, MethodInfo boot) {
            MethodBuilder mb = tb.DefineMethod("Main", MethodAttributes.Static |
                    MethodAttributes.Public, typeof(void),
                    new Type[] { typeof(string[]) });
            ILGenerator il = mb.GetILGenerator();

            il.Emit(OpCodes.Ldstr, name);
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Ldnull);
            il.Emit(OpCodes.Ldftn, boot);
            il.Emit(OpCodes.Newobj, Tokens.DynBlockDelegate_ctor);
            il.Emit(OpCodes.Call, Tokens.Kernel_RunLoop);
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ret);

            ab.SetEntryPoint(mb);

            mb = tb.DefineMethod("EVAL", MethodAttributes.Static |
                    MethodAttributes.Public, Tokens.Variable,
                    new Type[] { typeof(string[]) });
            il = mb.GetILGenerator();

            il.Emit(OpCodes.Ldstr, name);
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Ldnull);
            il.Emit(OpCodes.Ldftn, boot);
            il.Emit(OpCodes.Newobj, Tokens.DynBlockDelegate_ctor);
            il.Emit(OpCodes.Call, Tokens.Kernel_RunLoop);
            il.Emit(OpCodes.Ret);
        }

        [ThreadStatic] static Dictionary<string, Unit> used_units;
        static Dictionary<string, Unit> avail_units
            = new Dictionary<string, Unit>();
        internal static object Resolve(Xref x) {
            return GetUnit(x.unit).xref[x.index];
        }
        internal static Unit GetUnit(string name) {
            Unit u;
            if (used_units.TryGetValue(name, out u))
                return u;
            return used_units[name] = LoadDepUnit(name);
        }

        internal static Unit LoadDepUnit(string name) {
            lock (avail_units) {
                Unit u;
                if (avail_units.TryGetValue(name, out u))
                    return u;
                string dtx = File.ReadAllText(Path.Combine(Current.dir,
                            name.Replace("::",".") + ".nam"));
                u = new Unit((object[])Reader.Read(dtx), null);
                return avail_units[name] = u;
            }
        }

        internal static void RunMain(string dir, string contents,
                string[] argv) {
            Unit root = new Unit((object[])Reader.Read(contents),
                    (object[])Reader.Read(contents.Substring(contents.IndexOf('\n'))));
            CLRBackend old_Current = Current;
            Dictionary<string,Unit> old_used_units = used_units;
            CLRBackend c = new CLRBackend(dir, root.name, null);
            Current = c;

            used_units = new Dictionary<string, Unit>();
            used_units[root.name] = root;

            foreach (object x in root.tdeps) {
                object[] dn = (object[]) x;
                string name = JScalar.S(dn[0]);
                if (name == root.name) continue;
                used_units[name] = LoadDepUnit(name);
            }
            root.BindDepends(true);

            c.Process(root, true);

            Type t = c.tb.CreateType();
            used_units = old_used_units; Current = old_Current;

            Builtins.eval_result = (Variable) t.InvokeMember("EVAL",
                    BindingFlags.Public | BindingFlags.Static |
                    BindingFlags.InvokeMethod, null, null, new object[] { argv });
        }

        public static void Main(string[] args) {
            if (args.Length != 4) {
                Console.Error.WriteLine("usage : CLRBackend DIR UNITFILE OUTFILE ISMAIN");
                return;
            }
            string dir      = args[0];
            string unitfile = args[1];
            string outfile  = args[2];
            bool   ismain   = args[3] == "1";
            string tx = File.ReadAllText(Path.Combine(dir, unitfile));
            Unit root = new Unit((object[])Reader.Read(tx),
                    (object[])Reader.Read(tx.Substring(tx.IndexOf("\n")+1)));
            CLRBackend c = new CLRBackend(dir, root.name.Replace("::","."), outfile);
            Current = c;

            used_units = new Dictionary<string, Unit>();
            used_units[root.name] = root;

            foreach (object x in root.tdeps) {
                object[] dn = (object[]) x;
                string name = JScalar.S(dn[0]);
                if (name == root.name) continue;
                used_units[name] = LoadDepUnit(name);
            }
            root.BindDepends(true);

            c.Process(root, ismain);

            c.Finish(outfile);
            used_units = null; Current = null;
        }
    }

    // instantiatable for the sake of reflecty loading
    public class DownCallAcceptor: CrossDomainReceiver {
        public override string[] Call(AppDomain up, string[] args) {
            Builtins.up_domain = up;
            if (args[0] == "post_save") {
                CLRBackend.Main(new string[] { args[1], args[2], args[3], args[4] });
                return new string[0];
            } else if (args[0] == "runnam" || args[0] == "evalnam") {
                string[] argv = new string[args.Length - 3];
                Array.Copy(args, 3, argv, 0, argv.Length);
                CLRBackend.RunMain(args[1], args[2],
                        args[0] == "evalnam" ? null : argv);
                return new string[0];
            } else if (args[0] == "safemode") {
                Kernel.SaferMode = true;
                return new string[0];
            } else if (args[0] == "hello") {
                return new string[] { Assembly.GetExecutingAssembly().Location };
            } else {
                return new string[] { "ERROR" };
            }
        }
    }
}
