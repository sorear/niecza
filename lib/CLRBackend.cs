// This is the new CLR backend.  The old one generated C# from Perl, which
// was slow and gave us the limitations of C#; this one aims to be faster.
// Also, by making the Perl code emit a portable format, it makes future
// portability work easier.

using System;
using System.Reflection;
using System.Reflection.Emit;
using System.Collections.Generic;
using System.Collections;
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
                        i == ',' || i == ':') {
                    ix++;
                    continue;
                }
                if (i == '[' || i == '{') {
                    containers.Add(new List<object>());
                    ix++;
                    continue;
                }
                if (i == ']' || i == '}') {
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
                            i == ' ' || i == ']' || i == '}')
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
        public readonly Xref nsroot;
        public readonly string name;
        public readonly object[] nslog;
        public readonly Xref setting_ref;
        public readonly Xref bottom_ref;
        public readonly string filename;
        public readonly double modtime;
        public readonly object[] xref;
        public readonly object[] tdeps;

        public readonly Dictionary<string, int> tdep_to_id;
        public readonly List<Unit> id_to_tdep;

        public Assembly clrAssembly;
        public Type clrType;
        bool depsBound;
        public FieldInfo rtunit;
        public List<byte> thaw_heap;
        public Dictionary<string,int> existing_strings;
        public bool is_eval, is_mainish;

        FieldInfo cc_pool;
        Dictionary<string,int> cc_constant_cache = new Dictionary<string,int>();
        List<int[]> cc_constants = new List<int[]>();

        FieldInfo ccl_pool;
        Dictionary<string,int> ccl_constant_cache = new Dictionary<string,int>();
        List<int[][]> ccl_constants = new List<int[][]>();

        FieldInfo sl_pool;
        Dictionary<string,int> sl_constant_cache = new Dictionary<string,int>();
        List<string[]> sl_constants = new List<string[]>();

        FieldInfo var_pool;
        Dictionary<string,int> var_constant_cache = new Dictionary<string,int>();
        List<string> var_constants = new List<string>();

        FieldInfo alt_info_pool;
        List<object> alt_info_constants = new List<object>();

        public Unit(object[] from, object[] code) {
            name = JScalar.S(from[0]);
            tdeps = from[1] as object[];
            mainline_ref = Xref.from(from[2]);
            setting_ref = Xref.from(from[3]);
            bottom_ref = Xref.from(from[4]);
            filename = JScalar.S(from[5]);
            modtime = from[6] == null ? 0 : JScalar.N(from[6]);
            nsroot = Xref.from(from[7]);
            nslog = from[8] as object[];
            xref = from[9] as object[];

            tdep_to_id = new Dictionary<string,int>();
            id_to_tdep = new List<Unit>();
            thaw_heap = new List<byte>();
            existing_strings = new Dictionary<string,int>();
            for (int i = 0; i < xref.Length; i++) {
                if (xref[i] == null) continue;
                object[] xr = (object[]) xref[i];
                if (CLRBackend.Verbose > 0)
                    Console.WriteLine("Loading {0} {1}...", JScalar.S(xr[0]),i);
                if (JScalar.S(xr[0]) == "sub") {
                    xref[i] = new StaticSub(this, xr, (code != null &&
                            i < code.Length) ? (object[])code[i] : null);
                } else {
                    Package p = Package.From(xr);
                    xref[i] = p;
                    p.own_xref = new Xref(name, i, p.name);
                }
            }
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

        public Package GetCorePackage(string name) {
            StaticSub r = (bottom_ref ?? mainline_ref).Resolve<StaticSub>();
            while (r.unit.name != "CORE")
                r = r.outer.Resolve<StaticSub>();
            StaticSub rs = r;
            while (r != null && !r.l_lexicals.ContainsKey(name))
                r = r.outer != null ? r.outer.Resolve<StaticSub>() : null;
            if (r == null) {
                Console.WriteLine("CORE::" + name + " nonexistant!");
                for (r = rs; r != null; r = r.outer != null ? r.outer.Resolve<StaticSub>() : null) {
                    Console.WriteLine("-- {0}", r.name);
                    foreach (string s in r.l_lexicals.Keys)
                        Console.WriteLine(s);
                }
                throw new ArgumentException();
            }
            LexStash lx = (LexStash)r.l_lexicals[name];
            return lx.GetPackage();
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
            alt_info_pool = binder("ALTINFO", typeof(AltInfo).MakeArrayType());
            ccl_pool = binder("CCL", Tokens.CC.MakeArrayType().MakeArrayType());
            var_pool = binder("UVAR", Tokens.Variable.MakeArrayType());
            sl_pool = binder("STRLISTS", Tokens.String.MakeArrayType().MakeArrayType());
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

        void EmitBigInt(BigInteger x) {
            uint[] w = x.GetWords();
            int[] ws = new int[w.Length];
            for (int i = 0; i < w.Length; i++) ws[i] = (int)w[i];
            EmitByte((byte)x.Sign);
            EmitIntArray(ws);
        }

        public void EmitStrVar(string str) {
            EmitByte(0);
            EmitStr(str);
        }

        public void EmitExactNum(int numbase, string digits) {
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
                long val = BitConverter.DoubleToInt64Bits((double)num / (double)den);
                EmitByte(1);
                EmitInt((int)val);
                EmitInt((int)(val >> 32));
                return;
            }

            if (sden == 0) {
                int snum;
                if (num.AsInt32(out snum)) {
                    EmitByte(2);
                    EmitInt(snum);
                    return;
                }
                EmitByte(3);
                EmitBigInt(num);
                return;
            }

            EmitByte(4);
            EmitBigInt(num);
            EmitInt((int)sden);
            EmitInt((int)(sden >> 32));
        }
        public CpsOp VarConst(string code) {
            int ix;
            if (!var_constant_cache.TryGetValue(code, out ix)) {
                var_constant_cache[code] = ix = var_constants.Count;
                var_constants.Add(code);
            }
            return CpsOp.IsConst(
                CpsOp.Operator(Tokens.Variable, OpCodes.Ldelem_Ref,
                    CpsOp.GetSField(var_pool), CpsOp.IntLiteral(ix)));
        }

        public CpsOp VarConstStr(string s) { return VarConst('S' + s); }

        public CpsOp VarConstNum(double n) {
            char[] c = new char[5];
            c[0] = 'N';
            long b = BitConverter.DoubleToInt64Bits(n);
            c[1] = (char)(b);
            c[2] = (char)(b >> 16);
            c[3] = (char)(b >> 32);
            c[4] = (char)(b >> 48);
            return VarConst(new string(c));
        }

        public CpsOp VarConstExact(int bas, string digs) {
            return VarConst("X" + bas + ',' + digs);
        }

        public CpsOp EmitVarConsts() {
            int b = thaw_heap.Count;
            EmitInt(var_constants.Count);
            foreach (string code in var_constants) {
                if (code[0] == 'S') {
                    EmitByte(0);
                    EmitStr(code.Substring(1));
                } else if (code[0] == 'N') {
                    EmitByte(1);
                    for (int j = 1; j <= 4; j++) EmitUShort(code[j]);
                } else if (code[0] == 'X') {
                    int sep = code.IndexOf(',');
                    EmitExactNum(int.Parse(code.Substring(1,sep-1)), code.Substring(sep+1));
                } else {
                    throw new ArgumentException("wtf? " + code);
                }
            }
            return CpsOp.SetSField(var_pool,
                    CpsOp.MethodCall(Tokens.RuntimeUnit.GetMethod("LoadVariablePool"),
                        CpsOp.GetSField(rtunit), CpsOp.IntLiteral(b)));
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
            return CpsOp.IsConst(
                CpsOp.Operator(Tokens.CC, OpCodes.Ldelem_Ref,
                    CpsOp.GetSField(cc_pool), CpsOp.IntLiteral(ix)));
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

        public CpsOp StringListConst(string[] sl) {
            StringBuilder code = new StringBuilder();
            foreach (string s in sl) {
                code.Append((char)(s.Length >> 16));
                code.Append((char)(s.Length));
                code.Append(s);
            }
            string fcode = code.ToString();
            int ix;
            if (!sl_constant_cache.TryGetValue(fcode, out ix)) {
                sl_constant_cache[fcode] = ix = sl_constants.Count;
                sl_constants.Add(sl);
            }
            return CpsOp.IsConst(CpsOp.Operator(Tokens.String.MakeArrayType(),
                OpCodes.Ldelem_Ref, CpsOp.GetSField(sl_pool),
                CpsOp.IntLiteral(ix)));
        }

        public CpsOp EmitStringListConsts() {
            int b = thaw_heap.Count;
            EmitInt(sl_constants.Count);
            foreach (string[] sl in sl_constants) {
                EmitStrArray(sl);
            }
            return CpsOp.SetSField(sl_pool,
                    CpsOp.MethodCall(Tokens.RuntimeUnit.GetMethod("LoadStrListPool"),
                        CpsOp.GetSField(rtunit), CpsOp.IntLiteral(b)));
        }

        public CpsOp CCListConst(int[][] ccl) {
            StringBuilder code = new StringBuilder();
            foreach (int[] cc in ccl) {
                code.Append((char)(cc.Length >> 16));
                code.Append((char)(cc.Length));
                foreach (int x in cc) {
                    code.Append((char)x);
                    code.Append((char)(x>>16));
                }
            }
            string fcode = code.ToString();
            int ix;
            if (!ccl_constant_cache.TryGetValue(fcode, out ix)) {
                ccl_constant_cache[fcode] = ix = ccl_constants.Count;
                ccl_constants.Add(ccl);
            }
            return CpsOp.IsConst(CpsOp.Operator(Tokens.CC.MakeArrayType(), OpCodes.Ldelem_Ref,
                CpsOp.GetSField(ccl_pool), CpsOp.IntLiteral(ix)));
        }

        public CpsOp EmitCCListConsts() {
            int b = thaw_heap.Count;
            EmitInt(ccl_constants.Count);
            foreach (int[][] ccl in ccl_constants) {
                EmitInt(ccl.Length);
                foreach (int[] cc in ccl)
                    EmitIntArray(cc);
            }
            return CpsOp.SetSField(ccl_pool,
                    CpsOp.MethodCall(Tokens.RuntimeUnit.GetMethod("LoadCCListPool"),
                        CpsOp.GetSField(rtunit), CpsOp.IntLiteral(b)));
        }

        public CpsOp AltInfoConst(CgContext cx, object lads, string dba, string[] labels) {
            int ix = alt_info_constants.Count / 4;
            alt_info_constants.Add(cx);
            alt_info_constants.Add(lads);
            alt_info_constants.Add(dba);
            alt_info_constants.Add(labels);

            return CpsOp.IsConst(CpsOp.Operator(typeof(AltInfo), OpCodes.Ldelem_Ref,
                CpsOp.GetSField(alt_info_pool), CpsOp.IntLiteral(ix)));
        }

        public CpsOp EmitAltInfoConsts() {
            int b = thaw_heap.Count;
            EmitInt(alt_info_constants.Count / 4);
            for (int i = 0; i < alt_info_constants.Count; i += 4) {
                CgContext cx = (CgContext) alt_info_constants[i];
                object lads = alt_info_constants[i+1];
                string dba = (string) alt_info_constants[i+2];
                string[] labels = (string[]) alt_info_constants[i+3];

                EmitLADArr(lads);
                EmitStr(dba);
                int[] lids = new int[labels.Length];
                for (int j = 0; j < labels.Length; j++)
                    lids[j] = cx.named_cases[labels[j]];
                EmitIntArray(lids);
            }

            return CpsOp.SetSField(alt_info_pool,
                    CpsOp.MethodCall(Tokens.RuntimeUnit.GetMethod("LoadAltInfoPool"),
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
            name  = (from.Length - ofs > 2) ? ((JScalar)from[ofs+2]).str : null;
        }
        public T Resolve<T>() { return (T) CLRBackend.Resolve(this); }
    }

    class Package {
        public Xref own_xref;
        public readonly string name;
        public readonly string type;
        public readonly string who;
        public FieldInfo metaObject;

        public Package(object[] p) {
            type = ((JScalar)p[0]).str;
            name = ((JScalar)p[1]).str;
            who  = ((JScalar)p[2]).str;
        }

        public void BindFields(int ix, Func<string,Type,FieldInfo> binder) {
            metaObject = binder(Unit.SharedName('M', ix, name), Tokens.STable);
        }

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
            else if (type == "subset")
                return new Subset(p);
            else
                throw new Exception("unknown package type " + p);
        }
    }

    class Module: Package {
        public Module(object[] p) : base(p) { }
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
        public readonly char   sigil;
        public readonly bool   publ;
        public readonly string ivar;
        public readonly Xref   ibody;
        public readonly Xref   type;

        public Attribute(object[] x) {
            name  = JScalar.S(x[0]);
            sigil = JScalar.S(x[1])[0];
            publ  = JScalar.B(x[2]);
            ivar  = JScalar.S(x[3]);
            ibody = Xref.from(x[4]);
            type  = Xref.from(x[5]);
        }

        public static Attribute[] fromArray(object x) {
            return JScalar.A<Attribute>(0, x, delegate (object o) {
                return new Attribute(o as object[]); });
        }
    }

    class Class: Module {
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

    class Role: Module {
        public readonly Attribute[] attributes;
        public readonly Method[] methods;
        public readonly Xref[] superclasses;
        public Role(object[] p) : base(p) {
            attributes = Attribute.fromArray(p[3]);
            methods    = Method.fromArray(p[4]);
            superclasses = JScalar.A<Xref>(0, p[5], Xref.from);
        }
    }

    class ParametricRole: Module {
        public readonly Attribute[] attributes;
        public readonly Method[] methods;
        public readonly Xref[] superclasses;
        public ParametricRole(object[] p) : base(p) {
            attributes = Attribute.fromArray(p[3]);
            methods    = Method.fromArray(p[4]);
            superclasses = JScalar.A<Xref>(0, p[5], Xref.from);
        }
    }

    class Subset: Module {
        public readonly Xref basetype;
        public readonly Xref where;

        public Subset(object[] p) : base(p) {
            basetype = Xref.from(p[3]);
            where = Xref.from(p[4]);
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
        public readonly Xref cur_pkg;
        public readonly string sclass;
        public readonly object ltm;
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
            sig = s[7];

            object[] r_lexicals = s[8] as object[];

            if (c != null) {
                parametric_role_hack = Xref.from(c[1]);
                augment_hack = c[2];
                hint_hack = c[3] as object[];
                is_phaser = JScalar.IN(c[4]);
                body_of = Xref.from(c[5]);
                in_class = Xref.from(c[6]);
                cur_pkg = Xref.from(c[7]);
                body = c[8];
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
            if ((flags & (SPAD_EXISTS | RUN_ONCE)) != 0)
                protopad = binder(Unit.SharedName('P', ix, name), Tokens.Frame);

            nlexn = 0;
            for (int i = 0; i < lexicals.Count; i++)
                lexicals[i].Value.BindFields(ix, i, this,
                        lexicals[i].Key, binder);
        }

        internal List<StaticSub> GetPhasers(int t1) {
            int t2 = (t1 == Kernel.PHASER_KEEP) ? Kernel.PHASER_LEAVE : t1;
            List<StaticSub> r = new List<StaticSub>();
            foreach (int i in zyg) {
                StaticSub z = unit.xref[i] as StaticSub;
                if (z.is_phaser >= t1 && z.is_phaser <= t2) r.Add(z);
            }
            return r;
        }
    }

    abstract class Lexical {
        public virtual void BindFields(int six, int lix, StaticSub sub,
                string name, Func<string,Type,FieldInfo> binder) { }
        public virtual ClrOp SetCode(int up, bool proto, ClrOp head) {
            throw new Exception("Lexicals of type " + this + " cannot be bound");
        }
        public abstract void EmitInfo(Unit to);
        public abstract ClrOp GetCode(int up, bool proto);
        /* names which are kept in the pad for quick runtime access */
        public static bool IsDynamicName(string name) {
            if (name == "$_" || name == "$/" || name == "$!") return true;
            if (name.Length < 2) return false;
            if (name[0] == '*' || name[0] == '?') return true;
            if (name[1] == '*' || name[1] == '?') return true;
            return false;
        }
    }

    abstract class LexVarish : Lexical {
        public int index;
        public FieldInfo stg;
        public StaticSub owner;

        ClrOp GetProtoFrame() {
            if (owner.protopad != null)
                return new ClrGetSField(owner.protopad);
            StaticSub rs = CLRBackend.Current.unit.mainline_ref.Resolve<StaticSub>();
            ClrOp r = new ClrGetSField(rs.protopad);
            while (rs != owner) {
                rs = rs.outer.Resolve<StaticSub>();
                r = new ClrGetField(Tokens.Frame_outer, r);
            }
            return r;
        }

        public override ClrOp GetCode(int up, bool proto) {
            return (index < 0) ? (ClrOp) new ClrGetSField(stg) :
                proto ? (ClrOp) new ClrProtoGet(index, GetProtoFrame()) :
                new ClrPadGet(up, index);
        }

        public override ClrOp SetCode(int up, bool proto, ClrOp to) {
            return (index < 0) ? (ClrOp)new ClrSetSField(stg, to) :
                proto ? new ClrProtoSet(index, GetProtoFrame(), to) :
                (ClrOp)new ClrPadSet(up, index, to);
        }

        public override void EmitInfo(Unit to) {
            if (index < 0) { to.EmitByte(1); to.EmitStr(stg.Name); }
            else { to.EmitByte(0); to.EmitInt(index); }
        }

        public override void BindFields(int six, int lix, StaticSub sub,
                string name, Func<string,Type,FieldInfo> binder) {
            owner = sub;
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
        public const int ROINIT = 8;
        public const int DEFOUTER = 16;
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

    class LexSub : LexVarish {
        public readonly Xref def;
        public LexSub(object[] l) {
            def = new Xref(l, 2);
        }
    }

    class LexHint : Lexical {
        public FieldInfo stg;
        public LexHint(object[] l) {}
        public override ClrOp GetCode(int up, bool proto) {
            return new ClrGetField(Tokens.BValue_v,
                    new ClrGetSField(stg));
        }
        public override void BindFields(int six, int lix, StaticSub sub,
                string name, Func<string,Type,FieldInfo> binder) {
            stg = binder(Unit.SharedName('B', six, name), Tokens.BValue);
        }
        public override void EmitInfo(Unit to) {
            to.EmitByte(2);
            to.EmitStr(stg.Name);
        }
    }

    class LexCommon : Lexical {
        public readonly Xref package;
        public readonly string name;
        public FieldInfo stg;
        public LexCommon(object[] l) {
            package = Xref.from(l[2]);
            name = JScalar.S(l[3]);
        }
        public override ClrOp GetCode(int up, bool proto) {
            return new ClrGetField(Tokens.BValue_v,
                    new ClrGetSField(stg));
        }
        public override ClrOp SetCode(int up, bool proto, ClrOp to) {
            return new ClrSetField(Tokens.BValue_v,
                    new ClrGetSField(stg), to);
        }
        public override void EmitInfo(Unit to) {
            to.EmitByte(2);
            to.EmitStr(stg.Name);
        }
        public override void BindFields(int six, int lix, StaticSub sub,
                string name, Func<string,Type,FieldInfo> binder) {
            stg = binder(Unit.SharedName('B', six, name), Tokens.BValue);
        }
    }

    class LexAlias : Lexical {
        public readonly string to;
        public LexAlias(object[] l) {
            to = JScalar.S(l[2]);
        }
        public override void EmitInfo(Unit to) {
            to.EmitByte(3);
            to.EmitStr(this.to);
        }
        public override ClrOp GetCode(int up, bool proto) { throw new NotImplementedException(); }
    }

    class LexStash : Lexical {
        public readonly Unit unit;
        public readonly Xref package;
        public override void EmitInfo(Unit to) {
            to.EmitByte(4);
            to.EmitXref(package);
        }
        public Package GetPackage() { return package.Resolve<Package>(); }
        public override ClrOp GetCode(int up, bool proto) {
            Package p = GetPackage();
            return new ClrGetField(Tokens.DMO_typeVar,
                    new ClrGetSField(p.metaObject));
        }
        public LexStash(Unit u, object[] l) {
            unit = u;
            package = new Xref(l, 2);
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
        public LocalBuilder ospill, sspill, pspill, nspill;
        public List<int> lineStack = new List<int>();
        public List<int> lineBuffer = new List<int>();
        public List<int> ehspanBuffer = new List<int>();
        public List<string> ehlabelBuffer = new List<string>();
        public List<LocalBuilder> scratches = new List<LocalBuilder>();

        public void make_ospill() {
            if (ospill == null)
                ospill = il.DeclareLocal(Tokens.Variable);
        }

        public void make_sspill() {
            if (sspill == null)
                sspill = il.DeclareLocal(Tokens.Variable);
        }

        public void save_line() {
            lineBuffer.Add(lineStack.Count == 0 ? 0 : lineStack[lineStack.Count - 1]);
        }

        public void EmitDataArray(Type ty, int ct, byte[] vec) {
            EmitInt(ct);
            if (CLRBackend.Current.dynamic)
                throw new Exception("cannot EmitDataArray with dynamic assembly");
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
        public static readonly Type StashCursor = typeof(StashCursor);

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
        public static readonly ConstructorInfo SC_ctor =
            StashCursor.GetConstructor(new Type[] { typeof(Frame), typeof(int) });
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
        public static readonly MethodInfo Kernel_CreateArray =
            typeof(Kernel).GetMethod("CreateArray");
        public static readonly MethodInfo Kernel_CreateHash =
            typeof(Kernel).GetMethod("CreateHash");
        public static readonly MethodInfo Kernel_GetVar =
            typeof(Kernel).GetMethod("GetVar");
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
        public static readonly MethodInfo RU_LoadPackage =
            RuntimeUnit.GetMethod("LoadPackage");
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
        public static readonly MethodInfo Frame_Return =
            Frame.GetMethod("Return");

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
        public static readonly FieldInfo DMO_typeVar =
            STable.GetField("typeVar");
        public static readonly FieldInfo DMO_initObject =
            STable.GetField("initObject");
        public static readonly FieldInfo DMO_initVar =
            STable.GetField("initVar");
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
        public static readonly FieldInfo Kernel_ParcelMO =
            Kernel.GetField("ParcelMO");
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
            int scratch_ix = -1;
            LocalBuilder scratch_lb = null;
            int i = 0;
            if (!Method.IsStatic) {
                ClrOp o = Zyg[i++];
                o.CodeGen(cx);
                if (o.Returns.IsValueType) {
                    if (Method.DeclaringType == o.Returns) {
                        scratch_ix = 0;
                        while (scratch_ix < cx.scratches.Count &&
                                cx.scratches[scratch_ix].LocalType != o.Returns)
                            scratch_ix++;

                        if (scratch_ix == cx.scratches.Count)
                            cx.scratches.Add(cx.il.DeclareLocal(o.Returns));

                        scratch_lb = cx.scratches[scratch_ix];
                        cx.scratches[scratch_ix] = null;

                        cx.il.Emit(OpCodes.Stloc, scratch_lb);
                        cx.il.Emit(OpCodes.Ldloca, scratch_lb);
                    }
                    else
                        cx.il.Emit(OpCodes.Box, o.Returns);
                }
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
            if (scratch_ix >= 0)
                cx.scratches[scratch_ix] = scratch_lb;
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
            cx.il.Emit(OpCodes.Dup);
            cx.il.Emit(OpCodes.Stloc, cx.ospill);
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

    class ClrProtoGet : ClrOp {
        public readonly int ix;
        public readonly ClrOp zyg;

        public override ClrOp Sink() { return ClrNoop.Instance; }
        public override void CodeGen(CgContext cx) {
            zyg.CodeGen(cx);
            cx.EmitGetlex(ix + Tokens.NumInt32, Tokens.Variable);
        }

        public ClrProtoGet(int ix, ClrOp zyg) {
            TypeCheck(zyg.Returns, Tokens.Frame);
            Returns = Tokens.Variable;
            this.ix = ix;
            this.zyg = zyg;
        }
    }

    class ClrMarkConstant : ClrOp {
        readonly ClrOp real;
        public ClrMarkConstant(ClrOp real) {
            this.real = real;
            Returns = real.Returns;
            HasCases = false;
            Constant = true;
        }
        // no side effects, huh?
        public override ClrOp Sink() {
            return ClrNoop.Instance;
        }
        public override void CodeGen(CgContext cx) {
            real.CodeGen(cx);
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
                cx.EmitInt(zyg.Length - min + (ismethod ? 1 : 0));
                cx.il.Emit(OpCodes.Newarr, Tokens.Variable);
                if (ismethod) {
                    cx.il.Emit(OpCodes.Dup);
                    cx.EmitInt(0);
                    cx.il.Emit(OpCodes.Ldloc, cx.sspill);
                    cx.il.Emit(OpCodes.Stelem_Ref);
                }
                for (int i = min; i < zyg.Length; i++) {
                    cx.il.Emit(OpCodes.Dup);
                    cx.EmitInt(i - min + (ismethod ? 1 : 0));
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

                if (ismethod) {
                    cx.il.Emit(OpCodes.Ldloc, cx.pspill);
                    cx.il.Emit(OpCodes.Ldloc, cx.sspill);
                    cx.il.Emit(OpCodes.Call, typeof(List<Variable>).GetMethod("Add"));
                }

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
            if (ismethod) {
                cx.make_sspill();
                cx.il.Emit(OpCodes.Dup);
                cx.il.Emit(OpCodes.Stloc, cx.sspill);
                cx.il.Emit(OpCodes.Callvirt, Tokens.Variable_Fetch);
            }
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
            int i = 0;
            if (ismethod) TypeCheck(zyg[i++].Returns, Tokens.String);
            TypeCheck(zyg[i++].Returns, ismethod ? Tokens.Variable : Tokens.P6any);
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
            cx.il.Emit(OpCodes.Call, Tokens.Frame_Return);
            cx.il.Emit(OpCodes.Ret);
        }
    }

    class ClrStringLiteral : ClrOp {
        string data;
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public ClrStringLiteral(string data) {
            this.data = data;
            if (data == null) throw new ArgumentNullException();
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

    class ClrTypeLiteral : ClrOp {
        readonly Type body;
        public override ClrOp Sink() { return ClrNoop.Instance; }
        public ClrTypeLiteral(Type body) {
            this.body = body;
            Returns = typeof(Type);
            Constant = true;
        }
        public override void CodeGen(CgContext cx) {
            cx.il.Emit(OpCodes.Ldtoken, body);
            cx.il.Emit(OpCodes.Call, typeof(Type).GetMethod("GetTypeFromHandle"));
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

                bool effects_before_use = false;
                for (int j = i + 1; j < zyg.Length; j++)
                    if (zyg[j].stmts.Length != 0)
                        effects_before_use = true;
                for (int j = 0; j < i; j++)
                    if (!zyg[j].head.Constant)
                        effects_before_use = true;

                // if we have statements, then we need our head
                // spilled right away, because interleaving evaluation
                // (detectably) isn't allowed.
                // unless, nothing with side effects can possibly
                // come between.
                if (!effects_before_use || zyg[i].stmts.Length == 0) {
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

        public static CpsOp TypeLiteral(Type x) {
            return new CpsOp(new ClrTypeLiteral(x));
        }

        public static CpsOp DBDLiteral(MethodInfo x) {
            return new CpsOp(new ClrDBDLiteral(x));
        }

        public static CpsOp Label(string name, bool case_too) {
            return CpsOp.Cps(new ClrLabel(name, case_too), Tokens.Void);
        }

        public static CpsOp IsConst(CpsOp real) {
            if (real.stmts.Length != 0 || real.head.Returns == Tokens.Void)
                throw new ArgumentException();
            return new CpsOp(real.stmts, new ClrMarkConstant(real.head));
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

        public static CpsOp LexAccess(Lexical l, int up, bool p, CpsOp[] zyg) {
            return Primitive(zyg, delegate(ClrOp[] heads) {
                return new CpsOp((heads.Length >= 1)
                    ? l.SetCode(up, p, heads[0])
                    : l.GetCode(up, p));
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

        public static CpsOp RxFrame() {
            return IsConst(GetField(Tokens.Frame_rx, CallFrame()));
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
                return u.StringListConst(vec);
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
                (zyg.Length > 2 ? Scan(zyg[2]) : null), false);
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

        CpsOp CheckScopes(string name, ref int outer, CpsOp set_to) {
            for (int i = scope_stack.Count - 1; i >= 0; i--) {
                if (outer > 0) { outer--; continue; }
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
            return null;
        }

        CpsOp RawAccessLex(string type, string name, CpsOp set_to, bool proto) {
            bool core = type == "corelex";
            int outer = (type == "outerlex") ? 1 : 0;
            int uplevel;

            CpsOp r;
            if (!core && !proto && (r = CheckScopes(name, ref outer, set_to)) != null)
                return r;

            Lexical lex = ResolveLex(name, outer>0, out uplevel, core);

            return CpsOp.LexAccess(lex, uplevel, proto,
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
                        continue;
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

        internal CpsOp MakeDispatch(string prefix, bool proto) {
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
                        cands.Add(CpsOp.MethodCall(Tokens.Variable_Fetch, RawAccessLex("scopedlex", kp.Key, null, proto)));
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
                } else if (fty == Tokens.Boolean && tty == Tokens.Int32) {
                    return CpsOp.Widen(tty, z);
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
            handlers["comma"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.MethodCall(Tokens.Kernel_NewRWListVar,
                    CpsOp.MethodCall(Tokens.Kernel.GetMethod("BoxRaw").MakeGenericMethod(Tokens.FVarList),
                        CpsOp.NewArray(Tokens.Variable, JScalar.A<CpsOp>(1, zyg, th.Scan)),
                        CpsOp.GetSField(Tokens.Kernel_ParcelMO)));
            };
            handlers["makejunction"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.MethodCall(
                        Tokens.Builtins.GetMethod("MakeJunction"),
                        CpsOp.IntLiteral(JScalar.I(zyg[1])),
                        CpsOp.NewArray(Tokens.Variable, JScalar.A<CpsOp>(2, zyg, th.Scan)));
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
                        Class p = (Class)th.sub.unit.GetCorePackage(name);
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
                CpsOp typ = th.Scan(zyg[3]);
                CpsOp rhs = th.Scan(zyg[4]);
                int ro   = JScalar.B(zyg[1]) ? 0 : Kernel.NBV_RW;
                int list = JScalar.B(zyg[2]) ? Kernel.NBV_LIST : 0;
                return CpsOp.MethodCall(Tokens.Kernel_NewBoundVar,
                    CpsOp.IntLiteral(ro+list), typ, rhs);
            };
            handlers["whileloop"] = delegate(NamProcessor th, object[] z) {
                bool until = ((JScalar)z[1]).num != 0;
                bool once  = ((JScalar)z[2]).num != 0;
                return CpsOp.While(until, once, th.Scan(z[3]), th.Scan(z[4])); };
            thandlers["_pushleave"] = Methody(null, Tokens.Frame.GetMethod("PushLeave"));
            handlers["_makesub"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_MakeSub,
                    CpsOp.GetSField(((StaticSub)z[1]).subinfo),
                    CpsOp.CallFrame()); };
            handlers["_newlabel"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_NewLabelVar,
                        CpsOp.CallFrame(),
                        CpsOp.StringLiteral(JScalar.S(z[1]))); };
            handlers["_cpsop"] = delegate(NamProcessor th, object[] z) {
                return z[1] as CpsOp; };
            handlers["_newdispatch"] = delegate(NamProcessor th, object[] z) {
                return th.MakeDispatch(JScalar.S(z[1]), false); };
            handlers["class_ref"] = delegate(NamProcessor th, object[] z) {
                string kind = FixStr(z[1]);
                Package m;
                if (z.Length == 3) {
                    m = th.sub.unit.GetCorePackage(FixStr(z[2]));
                } else {
                    m = (new Xref(z, 2)).Resolve<Package>();
                }
                if (kind == "mo")
                    return CpsOp.GetSField(m.metaObject);
                if (kind == "typeVar")
                    return CpsOp.GetField(Tokens.DMO_typeVar,
                            CpsOp.GetSField(m.metaObject));
                if (kind == "typeObj")
                    return CpsOp.GetField(Tokens.DMO_typeObject,
                            CpsOp.GetSField(m.metaObject));
                throw new NotImplementedException();
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
                CpsOp strs = th.sub.unit.StringListConst(JScalar.SA(2,z));
                return CpsOp.MethodCall(Tokens.RxFrame_PushCapture,
                    CpsOp.RxFrame(), strs, th.Scan(z[1]));
            };
            handlers["rxincorpshift"] = delegate(NamProcessor th, object[] z) {
                CpsOp strs = th.sub.unit.StringListConst(JScalar.SA(0,z[1]));

                return CpsOp.Goto("backtrack", true,
                    CpsOp.MethodCall(Tokens.RxFrame.GetMethod("IncorpShift"),
                        CpsOp.RxFrame(),
                        strs, CpsOp.BoolLiteral(JScalar.B(z[2])),
                        CpsOp.LabelId(th.cpb.cx, JScalar.S(z[3]))));
            };
            handlers["rxincorpcut"] = delegate(NamProcessor th, object[] z) {
                CpsOp strs = th.sub.unit.StringListConst(JScalar.SA(0,z[1]));

                return CpsOp.Goto("backtrack", true,
                    CpsOp.MethodCall(Tokens.RxFrame.GetMethod("IncorpCut"),
                        CpsOp.RxFrame(), strs, CpsOp.IntLiteral(
                            JScalar.I(z[2]) * RxFrame.IC_ZERO_WIDTH +
                            JScalar.I(z[3]) * RxFrame.IC_NEGATIVE +
                            JScalar.I(z[4]) * RxFrame.IC_PASS_CAP),
                        th.Scan(z[5])));
            };
            handlers["rxbprim"] = delegate(NamProcessor th, object[] z) {
                CpsOp[] args = new CpsOp[z.Length - 1];
                for(int i = 0; i < z.Length - 2; i++)
                    args[i+1] = th.Scan(z[i+2]);
                args[0] = CpsOp.RxFrame();
                CpsOp call = CpsOp.MethodCall(
                        Tokens.RxFrame.GetMethod(FixStr(z[1])), args);
                return CpsOp.Goto("backtrack", true, call);
            };
            handlers["const"] = delegate(NamProcessor th, object[] z) {
                object[] ch = z[1] as object[];
                string chh = JScalar.S(ch[0]);
                if (chh == "exactnum") {
                    return th.sub.unit.VarConstExact(JScalar.I(ch[1]),
                        JScalar.S(ch[2]));
                } else if (chh == "box" && ch[1] is JScalar) {
                    string typ = JScalar.S(ch[1]);
                    object[] chch = ch[2] as object[];
                    string chchh = JScalar.S(chch[0]);
                    if (typ == "Str" && chchh == "str") {
                        return th.sub.unit.VarConstStr(JScalar.S(chch[1]));
                    } else if (typ == "Num" && chchh == "double") {
                        return th.sub.unit.VarConstNum(JScalar.N(chch[1]));
                    } else {
                        Console.WriteLine("odd constant box {0}/{1}", typ, chchh);
                    }
                } else if (chh == "newcc") {
                    return th.sub.unit.CCConst(JScalar.IA(1, ch));
                } else if (chh == "fcclist_new") {
                    int[][] ccl = new int[ch.Length - 1][];
                    for (int i = 1; i < ch.Length; i++)
                        ccl[i-1] = JScalar.IA(1, ch[i]);
                    return th.sub.unit.CCListConst(ccl);
                } else {
                    Console.WriteLine("odd constant {0}", chh);
                }

                return th.Scan(z[1]);
            };
            handlers["sc_root"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.ConstructorCall(Tokens.SC_ctor,
                        CpsOp.CallFrame(), CpsOp.IntLiteral(
                            th.scope_stack.Count));
            };

            thandlers["sc_indir"] = Methody(null, Tokens.StashCursor.GetMethod("Indirect"));

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
                        (CpsOp)z[1]); };
            thandlers["newblankrwscalar"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_NewTypedScalar,
                    CpsOp.Null(Tokens.STable)); };
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
            thandlers["make"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(Tokens.Builtins_Make,
                        CpsOp.CallFrame(), z[0]); };
            thandlers["simple_eval"] = Methody(Tokens.Variable,
                    Tokens.Builtins.GetMethod("simple_eval"));
            thandlers["you_are_here"] = Methody(Tokens.Variable,
                    Tokens.Builtins.GetMethod("you_are_here"));
            thandlers["callnext"] = Methody(Tokens.Variable,
                    Tokens.Builtins.GetMethod("CallNext"));
            handlers["context_get"] = delegate(NamProcessor th, object[] z) {
                string name = JScalar.S(z[1]);
                int outer = JScalar.I(z[2]);
                CpsOp r1 = th.CheckScopes(name, ref outer, null);
                if (r1 != null) return r1;
                Lexical l;
                if (outer == 0 && th.sub.l_lexicals.TryGetValue(name, out l))
                    return CpsOp.LexAccess(l, 0, false, new CpsOp[0]);
                return CpsOp.MethodCall(Tokens.Kernel_ContextHelper,
                    CpsOp.CallFrame(), CpsOp.StringLiteral(name),
                    CpsOp.IntLiteral(outer)); };
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
            thandlers["map"] = delegate(CpsOp[] z) {
                return CpsOp.CpsCall(Tokens.Variable, Tokens.Builtins_MEMap,
                        CpsOp.NewArray(Tokens.Variable, z)); };
            thandlers["grep"] = delegate(CpsOp[] z) {
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
            thandlers["hash_keys"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(itcommon, CpsOp.IntLiteral(0), z[0]); };
            thandlers["hash_values"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(itcommon, CpsOp.IntLiteral(1), z[0]); };
            thandlers["hash_kv"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(itcommon, CpsOp.IntLiteral(2), z[0]); };
            thandlers["hash_pairs"] = delegate(CpsOp[] z) {
                return CpsOp.MethodCall(itcommon, CpsOp.IntLiteral(3), z[0]); };
            Func<CpsOp[], CpsOp> real_pushcut = RxCall(null, "PushCutGroup");
            handlers["pushcut"] = delegate(NamProcessor th, object[] z) {
                return real_pushcut(new CpsOp[] { CpsOp.StringLiteral(FixStr(z[1])) }); };
            thandlers["rxframe"] = delegate(CpsOp[] z) {
                return CpsOp.RxFrame(); };
            handlers["rxcall"] = delegate(NamProcessor th, object[] z) {
                CpsOp[] x = new CpsOp[z.Length - 1];
                for (int i = 2; i < z.Length; i++)
                    x[i-1] = th.Scan(z[i]);
                x[0] = CpsOp.RxFrame();
                string name = JScalar.S(z[1]);
                return CpsOp.CpsCall((name == "EndWith" ? Tokens.Void : null),
                        Tokens.RxFrame.GetMethod(name), x); };
            handlers["rxinit"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.SetField(Tokens.Frame_rx, CpsOp.CallFrame(),
                    CpsOp.ConstructorCall(Tokens.RxFrame_ctor,
                        th.Scan(z[1]), th.Scan(z[2]),
                        CpsOp.BoolLiteral(FixBool(z[3])),
                        CpsOp.BoolLiteral(FixBool(z[4])))); };
            handlers["rxpushb"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(Tokens.RxFrame_PushBacktrack,
                    CpsOp.RxFrame(),
                    CpsOp.LabelId(th.cpb.cx, JScalar.S(z[2]))); };
            handlers["ltm_push_alts"] = delegate(NamProcessor th, object[] z) {
                CpsOp ai = th.sub.unit.AltInfoConst(th.cpb.cx, z[1],
                    JScalar.S(z[2]), JScalar.SA(0,z[3]));
                return CpsOp.MethodCall(Tokens.RxFrame.GetMethod("LTMPushAlts"),
                    CpsOp.RxFrame(), CpsOp.CallFrame(), ai); };
            thandlers["popcut"] = RxCall(null, "PopCutGroup");
            thandlers["rxend"] = delegate(CpsOp[] zyg) {
                return CpsOp.Sequence(
                    CpsOp.CpsCall(Tokens.Void,
                        Tokens.RxFrame.GetMethod("MakeMatch"), CpsOp.RxFrame()),
                    CpsOp.CpsCall(Tokens.Void,
                        Tokens.RxFrame.GetMethod("End"), CpsOp.RxFrame())); };
            thandlers["rxfinalend"] = delegate(CpsOp[] zyg) {
                return CpsOp.Sequence(
                    CpsOp.CpsCall(Tokens.Void,
                        Tokens.RxFrame.GetMethod("MakeMatch"), CpsOp.RxFrame()),
                    CpsOp.CpsCall(Tokens.Void,
                        Tokens.RxFrame.GetMethod("FinalEnd"), CpsOp.RxFrame())); };
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
            thandlers["path_realpath"] = Methody(null, typeof(Path).GetMethod("GetFullPath"));
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
            thandlers["cursor_synthetic"] = Methody(Tokens.Void, Tokens.Cursor.GetMethod("Synthetic"));
            thandlers["cursor_fresh"] = Methody(null, Tokens.Cursor.GetMethod("FreshClass"));
            thandlers["cursor_unmatch"] = Methody(null, Tokens.Cursor.GetMethod("UnMatch"));
            thandlers["cursor_reduced"] = Methody(null, Tokens.Cursor.GetMethod("Reduced"));
            thandlers["rxstripcaps"] = Methody(null, Tokens.Cursor.GetMethod("StripCaps"));

            thandlers["prog"] = CpsOp.Sequence;
            thandlers["newarray"] = Methody(null, Tokens.Kernel_CreateArray);
            thandlers["newhash"] = Methody(null, Tokens.Kernel_CreateHash);

            thandlers["shift"] = Contexty("mro_shift");
            thandlers["pop"] = Contexty("mro_pop");
            thandlers["push"] = Pushy("mro_push");
            thandlers["unshift"] = Pushy("mro_unshift");

            thandlers["defined"] = Contexty("mro_defined");
            thandlers["asbool"] = Contexty("mro_Bool");
            thandlers["num"] = Contexty("mro_Numeric");
            thandlers["asstr"] = Contexty("mro_Str");
            thandlers["item"] = Contexty("mro_item");
            thandlers["list"] = Contexty("mro_list");
            thandlers["hash"] = Contexty("mro_hash");
            thandlers["obj_asdef"] = Contexty("mro_defined");
            thandlers["obj_asbool"] = Contexty("mro_Bool");
            thandlers["obj_asnum"] = Contexty("mro_Numeric");
            thandlers["obj_asstr"] = Contexty("mro_Str");
            thandlers["obj_getbool"] = Contexty("mro_raw_Bool");
            thandlers["obj_getdef"] = Contexty("mro_raw_defined");
            thandlers["obj_getnum"] = Contexty("mro_raw_Numeric");
            thandlers["obj_getstr"] = Contexty("mro_raw_Str");
            thandlers["at_key"] = thandlers["obj_at_key"] = Contexty("mro_at_key");
            thandlers["at_pos"] = thandlers["obj_at_pos"] = Contexty("mro_at_pos");
            thandlers["exists_key"] = thandlers["obj_exists_key"] = Contexty("mro_exists_key");
            thandlers["delete_key"] = thandlers["obj_delete_key"] = Contexty("mro_delete_key");
            thandlers["cross"] = Methody(Tokens.Variable, Tokens.Builtins.GetMethod("MECross"));
            thandlers["zip"] = Methody(Tokens.Variable, Tokens.Builtins.GetMethod("MEZip"));
            thandlers["var_get_var"] = Methody(null, Tokens.Variable.GetMethod("GetVar"));
            thandlers["var_new_tied"] = Constructy(typeof(TiedVariable).GetConstructor(new Type[] { Tokens.STable, Tokens.P6any, Tokens.P6any, Tokens.P6any }));
            thandlers["obj_typename"] = Methody(null, Tokens.P6any.GetMethod("GetTypeName"));
            thandlers["fetch"] = Methody(null, Tokens.Variable_Fetch);
            thandlers["bget"] = FieldGet(Tokens.BValue, "v");
            thandlers["default_new"] = delegate(CpsOp[] z) {
                return CpsOp.Sequence(
                    CpsOp.Label("!again", false),
                    CpsOp.CpsCall(Tokens.Void, Tokens.Kernel.GetMethod("DefaultNew"), z),
                    CpsOp.Goto("!again", false),
                    CpsOp.Null(Tokens.Variable));
            };
            thandlers["assign"] = Methody(null, Tokens.Kernel.GetMethod("Assign"));
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
            thandlers["frame_outer"] = FieldGet(Tokens.Frame, "outer");
            thandlers["frame_sub"] = FieldGet(Tokens.Frame, "sub");
            thandlers["frame_args"] = Methody(null, Tokens.Frame.GetMethod("GetArgs"));
            thandlers["frame_dyn_caller"] = Methody(null, Tokens.Frame.GetMethod("DynamicCaller"));
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
                n[0] = CpsOp.RxFrame();
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

        static Func<CpsOp[], CpsOp> Pushy(string name) {
            FieldInfo f = Tokens.STable.GetField(name);
            MethodInfo g = f.FieldType.GetMethod("Invoke");
            return delegate(CpsOp[] cpses) {
                CpsOp[] args = new CpsOp[cpses.Length - 1];
                Array.Copy(cpses, 1, args, 0, args.Length);
                return CpsOp.Contexty(f, g, new CpsOp[2] {
                    cpses[0], CpsOp.NewArray(Tokens.Variable, args) });
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

        public void MakeBody() {
            cpb.ReserveLex(sub.nlexn);
            cpb.Build(Scan(WrapBody()));
        }

        void EncodeSignature(StaticSub obj) {
            if (obj.sig == null) {
                obj.unit.EmitInt(-1);
                return;
            }

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
                if ((flags & 4096) != 0) ufl |= SubInfo.SIG_F_IS_COPY;
                if (deflt != null) {
                    ufl |= SubInfo.SIG_F_HASDEFAULT;
                    sig_r.Add(deflt);
                }
                if (type != null) {
                    ufl |= SubInfo.SIG_F_HASTYPE;
                    sig_r.Add(type);
                }
                if ((flags & 128) != 0) ufl |= SubInfo.SIG_F_IS_LIST;
                if ((flags & 256) != 0) ufl |= SubInfo.SIG_F_IS_HASH;
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
        }

        public void SubInfoCtor(int ix, List<int> thaw) {
            thaw.Add(sub.unit.thaw_heap.Count);
            int spec = 0;

            if ((sub.flags & StaticSub.UNSAFE) != 0)
                spec |= RuntimeUnit.SUB_IS_UNSAFE;
            if (sub == sub.unit.mainline_ref.Resolve<StaticSub>())
                spec |= RuntimeUnit.SUB_MAINLINE;
            if ((sub.flags & StaticSub.RUN_ONCE) != 0)
                spec |= RuntimeUnit.SUB_RUN_ONCE;
            spec |= RuntimeUnit.SUB_HAS_TYPE;
            if (sub.protopad != null)
                spec |= RuntimeUnit.MAKE_PROTOPAD;
            if (sub.parametric_role_hack != null)
                spec |= RuntimeUnit.SUB_IS_PARAM_ROLE;

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

            sub.unit.EmitInt(sub.l_lexicals.Count);
            foreach (KeyValuePair<string, Lexical> kv in sub.l_lexicals) {
                sub.unit.EmitStr(kv.Key);
                kv.Value.EmitInfo(sub.unit);
            }

            sub.unit.EmitXref(sub.unit.GetCorePackage(sub.sclass).own_xref);

            if (sub.parametric_role_hack != null)
                sub.unit.EmitXref(sub.parametric_role_hack);

            /*not used until sub3 time*/
            sub.unit.EmitXref(sub.cur_pkg);
            EncodeSignature(sub);
            sub.unit.EmitByte(sub.is_phaser >= 0 ? sub.is_phaser : 0xFF);
        }

        JScalar j(string s) { return new JScalar(s); }
        object[] a(params object[] ax) { return ax; }
        void EnterCode(List<object> frags) {
            List<object> latefrags = new List<object>();

            if (sub == sub.unit.mainline_ref.Resolve<StaticSub>() &&
                    sub.unit.is_mainish) {
                FieldInfo fi = CLRBackend.Current.tb.DefineField(
                        "RTFRAME", typeof(Frame),
                        FieldAttributes.Public | FieldAttributes.Static);
                frags.Add(a(j("_cpsop"),
                    CpsOp.SetSField(fi, CpsOp.CallFrame())));
            }

            // Lexpad setup: XXX should be done *before* entry, indeed
            // before the binder, so defaults work right

            foreach (KeyValuePair<string,Lexical> kv in sub.lexicals) {
                if ((sub.flags & StaticSub.RUN_ONCE) != 0)
                    continue; // we'll just use the static pad

                if (kv.Value is LexSub) {
                    LexSub ls = (LexSub) kv.Value;
                    frags.Add(a(j("scopedlex"), j(kv.Key),
                        a(j("newscalar"), a(j("_makesub"),
                                ls.def.Resolve<StaticSub>()))));
                } else if (kv.Value is LexSimple) {
                    LexSimple ls = kv.Value as LexSimple;
                    int f = ls.flags;
                    if ((f & LexSimple.NOINIT) != 0) continue;

                    object bit;
                    CpsOp tc = ls.type == null ?
                        CpsOp.Null(Tokens.STable) :
                        CpsOp.GetSField(ls.type.Resolve<Package>().metaObject);
                    if ((f & LexSimple.ROINIT) != 0) {
                        bit = a(j("class_ref"), j("typeVar"), j("Any"));
                    } else if ((f & LexSimple.DEFOUTER) != 0) {
                        bit = a(j("outerlex"), j(kv.Key));
                    } else if ((f & (LexSimple.HASH | LexSimple.LIST)) != 0) {
                        bit = a(j( ((f & LexSimple.HASH) != 0) ?
                            "newhash" : "newarray" ));
                    } else {
                        bit = a(j("_newoftype"), tc);
                    }
                    frags.Add(a(j("scopedlex"), j(kv.Key), bit));
                } else if (kv.Value is LexLabel) {
                    frags.Add(a(j("scopedlex"), j(kv.Key),
                        a(j("_newlabel"), j(kv.Key))));
                } else if (kv.Value is LexDispatch) {
                    latefrags.Add(a(j("scopedlex"), j(kv.Key),
                        a(j("_newdispatch"), j(kv.Key))));
                }
            }
            foreach (object lf in latefrags) frags.Add(lf);

            // PRE
            foreach (StaticSub z in sub.GetPhasers(Kernel.PHASER_PRE)) {
                frags.Add(a(j("ternary"),
                    a(j("obj_getbool"), a(j("subcall"), j(""),
                            a(j("_makesub"), z))),
                    a(j("prog")),
                    a(j("sink"),a(j("die"),
                            j("Precondition failed in " + sub.name)))));
            }

            foreach (StaticSub z in sub.GetPhasers(Kernel.PHASER_POST)) {
                frags.Add(a(j("_pushleave"),
                    (sub.is_phaser == Kernel.PHASER_PRE ?
                        a(j("frame_outer"), a(j("callframe"))) :
                        a(j("callframe"))),
                    a(j("int"), j(LeaveHook.POST.ToString())),
                    a(j("_makesub"), z)));
            }

            // includes UNDO and LEAVE
            foreach (StaticSub z in sub.GetPhasers(Kernel.PHASER_KEEP)) {
                int type = z.is_phaser == Kernel.PHASER_KEEP ? LeaveHook.KEEP :
                    z.is_phaser == Kernel.PHASER_UNDO ? LeaveHook.UNDO :
                    LeaveHook.UNDO + LeaveHook.KEEP;
                frags.Add(a(j("_pushleave"), a(j("callframe")),
                    a(j("int"), j(type.ToString())),
                    a(j("_makesub"), z)));
            }

            foreach (StaticSub z in sub.GetPhasers(Kernel.PHASER_ENTER)) {
                frags.Add(a(j("sink"), a(j("subcall"), j(""),
                                a(j("_makesub"), z))));
            }
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
                supers[i] = CpsOp.GetSField(pr.superclasses[i].Resolve<Package>().metaObject);
            build.Add( CpsOp.MethodCall(Tokens.DMO_FillRole,
                mo, CpsOp.NewArray(Tokens.STable, supers),
                CpsOp.NewArray(Tokens.STable)) );

            foreach (Method m in pr.methods) {
                CpsOp name = (m.name != null) ? CpsOp.StringLiteral(m.name) :
                    Scan(new object[] { new JScalar("obj_getstr"), m.cname });
                CpsOp var  = RawAccessLex("scopedlex", m.var, null, false);

                build.Add(CpsOp.MethodCall(Tokens.DMO_AddMethod,
                    mo, CpsOp.IntLiteral(m.kind), name,
                    CpsOp.MethodCall(Tokens.Variable_Fetch, var)));
            }

            foreach (Attribute a in pr.attributes) {
                int flags = a.publ ? 1 : 0;
                if (a.sigil == '@') flags += 2;
                if (a.sigil == '%') flags += 4;
                CpsOp name = CpsOp.StringLiteral(a.name);
                CpsOp publ = CpsOp.IntLiteral(flags);
                CpsOp init = a.ivar == null ? CpsOp.Null(Tokens.P6any) :
                    RawAccessLex("scopedlex", a.ivar, null, false);
                CpsOp type = a.type == null ?
                    CpsOp.GetSField(Tokens.Kernel_AnyMO) :
                    CpsOp.GetSField(a.type.Resolve<Package>().metaObject);
                build.Add(CpsOp.MethodCall(Tokens.DMO_AddAttribute,
                    mo, name, publ, init, type));
            }

            build.Add(CpsOp.MethodCall(Tokens.DMO_Invalidate, mo));
            if (sub.sig != null) {
                object[] rsig = (object[]) sub.sig;
                foreach (object se in rsig) {
                    string slot = JScalar.S( ((object[])se)[2] );
                    if (slot != null)
                        build.Add(CpsOp.MethodCall(Tokens.VarHash_set_Item, pa, CpsOp.StringLiteral(slot), RawAccessLex("scopedlex", slot, null, false)));
                }
            }

            build.Add(RawAccessLex("scopedlex", "*params", pa, false));
            build.Add(CpsOp.SetField(Tokens.P6opaque_slots, to,
                        CpsOp.Null(typeof(object[]))));
            build.Add(CpsOp.SetField(Tokens.DMO_typeObject, mo, to));
            build.Add(CpsOp.SetField(Tokens.DMO_initObject, mo, to));
            build.Add(CpsOp.SetField(Tokens.DMO_typeVar, mo, CpsOp.MethodCall(Tokens.Kernel_NewROScalar, to)));
            build.Add(CpsOp.SetField(Tokens.DMO_initVar, mo, CpsOp.GetField(Tokens.DMO_typeVar, mo)));
            build.Add(CpsOp.CpsReturn(CpsOp.GetField(Tokens.DMO_typeVar, mo)));

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
            if (!handlers.TryGetValue(tag, out handler)) {
                MethodInfo mi = Tokens.Builtins.GetMethod(tag);
                if (mi == null)
                    throw new Exception("Unhandled nam operator " + tag);
                handlers[tag] = handler = MakeTotalHandler(Methody(null, mi));
            }
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
        internal bool dynamic;

        [ThreadStatic] internal static CLRBackend Current;
        [ThreadStatic] internal static Frame repl_frame;

        internal int nextarray;
        internal int nextspill;
        internal int nextlabel;
        internal Unit unit;
        internal string dir;

        internal List<CpsOp> thaw = new List<CpsOp>();

        public static int Verbose =
            int.Parse(Environment.GetEnvironmentVariable("NIECZA_CODEGEN_TRACE") ?? "0");
        public static bool Verifiable =
            Environment.GetEnvironmentVariable("NIECZA_CODEGEN_UNVERIFIABLE") != null ? false : true;

        CLRBackend(string dir, string mobname, string filename) {
            dynamic = (filename == null);
            AssemblyName an = new AssemblyName(mobname);
            this.dir = dir;
            ab = AppDomain.CurrentDomain.DefineDynamicAssembly(an,
                    (filename == null ? AssemblyBuilderAccess.RunAndSave :
                        AssemblyBuilderAccess.Save), dir);
            mob = filename == null ? ab.DefineDynamicModule(mobname) :
                ab.DefineDynamicModule(mobname, filename);

            tb = mob.DefineType(mobname, TypeAttributes.Public |
                    TypeAttributes.Sealed | TypeAttributes.Abstract |
                    TypeAttributes.Class | TypeAttributes.BeforeFieldInit);
        }

        void SetProtolex(Lexical v, CpsOp init) {
            thaw.Add(CpsOp.LexAccess(v, -1, true, new CpsOp[] { init }));
        }

        void Process(Unit unit, bool asmain) {
            this.unit = unit;

            if (Verbose > 0) Console.WriteLine("bind_fields");
            unit.BindFields(delegate(string name, Type type) {
                return tb.DefineField(name, type, FieldAttributes.Public |
                    FieldAttributes.Static);
            });

            if (Verbose > 0) Console.WriteLine("boot_deps");
            foreach (object o in unit.tdeps) {
                string dp = JScalar.S(((object[])o)[0]);
                if (dp == unit.name) continue;
                thaw.Add(CpsOp.Sink(CpsOp.MethodCall(Tokens.Kernel_BootModule,
                    CpsOp.StringLiteral(dp), CpsOp.DBDLiteral(CLRBackend.GetUnit(dp).clrType.GetMethod("BOOT")))));
            }
            int unit_slot = thaw.Count;
            thaw.Add(null);

            if (Verbose > 0) Console.WriteLine("[sub1]");
            NamProcessor[] aux = new NamProcessor[unit.xref.Length];
            unit.VisitSubsPreorder(delegate(int ix, StaticSub obj) {
                if (Verbose > 0) Console.WriteLine("sub1 {0}", obj.name);
                CpsBuilder cpb = new CpsBuilder(this,
                    Unit.SharedName('C', ix, obj.name), true);
                NamProcessor np = aux[ix] = new NamProcessor(cpb, obj);
                np.MakeBody();
            });

            unit.VisitPackages(delegate(int ix, Package pkg) {
                if (Verbose > 0) Console.WriteLine("pkg2 {0}", pkg.name);
                FieldInfo km = null;
                FieldInfo kp = null;
                bool existing_mo = false;
                if (unit.name == "CORE") {
                    km = Tokens.Kernel.GetField(pkg.name + "MO");
                    kp = Tokens.Kernel.GetField(pkg.name + "P");
                    existing_mo = km != null && km.IsInitOnly;
                }
                int b = unit.thaw_heap.Count;
                unit.EmitInt(ix);
                unit.EmitStr(pkg.name);
                unit.EmitStr(pkg.who);

                if (pkg is Role) {
                    unit.EmitByte(0);
                    Role r = (Role) pkg;
                    unit.EmitInt(r.superclasses.Length);
                    foreach (Xref x in r.superclasses)
                        unit.EmitXref(x);
                } else if (pkg is ParametricRole) {
                    unit.EmitByte(1);
                    // The heavy lifting is done in WrapBody
                } else if (pkg is Class) {
                    unit.EmitByte(2);
                    Class r = (Class) pkg;
                    List<string> all_slot = new List<string>();
                    // TODO: compute this in the compiler
                    for (int i = 0; i < r.linearized_mro.Length; i++) {
                        Class p = r.linearized_mro[i].Resolve<Class>();
                        foreach (Attribute a in p.attributes)
                            all_slot.Add(a.name);
                    }
                    unit.EmitStrArray(all_slot.ToArray());
                    unit.EmitInt(r.superclasses.Length);
                    foreach (Xref x in r.superclasses)
                        unit.EmitXref(x);
                    unit.EmitInt(r.linearized_mro.Length);
                    foreach (Xref x in r.linearized_mro)
                        unit.EmitXref(x);
                } else if (pkg is Subset) {
                    unit.EmitByte(5);
                    unit.EmitXref((pkg as Subset).basetype);
                } else if (pkg is Module) {
                    unit.EmitByte(3);
                } else if (pkg is Package) {
                    unit.EmitByte(4);
                }

                if (pkg is Role || pkg is Class) {
                    Method[] methods = (pkg is Class) ? ((Class)pkg).methods :
                        ((Role)pkg).methods;
                    Attribute[] attrs = (pkg is Class) ? ((Class)pkg).attributes :
                        ((Role)pkg).attributes;
                    unit.EmitInt(methods.Length);
                    foreach (Method me in methods) {
                        unit.EmitInt(me.kind);
                        unit.EmitStr(me.name);
                        unit.EmitXref(me.body);
                    }
                    unit.EmitInt(attrs.Length);
                    foreach (Attribute a in attrs) {
                        int flags = a.publ ? 1 : 0;
                        if (a.sigil == '@') flags += 2;
                        if (a.sigil == '%') flags += 4;
                        unit.EmitStr(a.name);
                        unit.EmitByte((byte)flags);
                        unit.EmitXref(a.ibody);
                        unit.EmitXref(a.type);
                    }
                } else if (pkg is Subset) {
                    unit.EmitInt(-1);
                    unit.EmitXref((pkg as Subset).where);
                } else {
                    unit.EmitInt(-1);
                }

                unit.EmitXref(unit.GetCorePackage("ClassHOW").own_xref);

                thaw.Add(CpsOp.SetSField(pkg.metaObject, CpsOp.MethodCall(
                    Tokens.RU_LoadPackage,
                    CpsOp.GetSField(unit.rtunit),
                    CpsOp.IntLiteral(b),
                    existing_mo ? CpsOp.GetSField(km) :
                        CpsOp.Null(Tokens.STable))));

                if (kp != null)
                    thaw.Add(CpsOp.SetSField(kp, CpsOp.GetField(
                        Tokens.DMO_typeObject, CpsOp.GetSField(pkg.metaObject))));
                if (km != null && !km.IsInitOnly)
                    thaw.Add(CpsOp.SetSField(km, CpsOp.GetSField(pkg.metaObject)));
            });

            if (unit.is_eval) {
                // XXX this caller's caller thing is a fudge
                StaticSub m = unit.mainline_ref.Resolve<StaticSub>();
                FieldInfo rtf = m.outer.Resolve<StaticSub>().unit.clrType.GetField("RTFRAME");
                CpsOp fb;
                if (rtf == null) {
                    fb = CpsOp.GetField(Tokens.SubInfo_protopad,
                            CpsOp.GetSField(m.outer.Resolve<StaticSub>().subinfo));
                } else {
                    fb = CpsOp.GetSField(rtf);
                }
                CpsOp norm = CpsOp.GetField(Tokens.Frame_caller,
                        CpsOp.GetField(Tokens.Frame_caller,
                            CpsOp.CallFrame()));
                CpsOp of = CpsOp.Ternary(
                    CpsOp.Operator(typeof(bool), OpCodes.Ceq, norm, CpsOp.Null(Tokens.Frame)),
                    fb, norm);
                thaw.Add(CpsOp.SetField(Tokens.RuntimeUnit.GetField("context_pad"), CpsOp.GetSField(unit.rtunit), of));
            }

            int sub2_slot = thaw.Count;
            thaw.Add(null);
            List<int> sub2_pointers = new List<int>();

            unit.VisitSubsPreorder(delegate(int ix, StaticSub obj) {
                if (Verbose > 0) Console.WriteLine("sub2 {0}", obj.name);
                aux[ix].SubInfoCtor(ix, sub2_pointers);
            });

            int sub2_pointers_start = unit.thaw_heap.Count;
            unit.EmitIntArray(sub2_pointers.ToArray());

            thaw[sub2_slot] = CpsOp.MethodCall(Tokens.RuntimeUnit.GetMethod("LoadAllSubs"), CpsOp.GetSField(unit.rtunit), CpsOp.IntLiteral(sub2_pointers_start));

            thaw.Add(CpsOp.MethodCall(Tokens.RuntimeUnit.GetMethod("FixupSubs"),
                CpsOp.GetSField(unit.rtunit)));

            unit.VisitSubsPreorder(delegate(int ix, StaticSub obj) {
                if (Verbose > 0) Console.WriteLine("sub3 {0}", obj.name);
                List<CpsOp> latefrags = new List<CpsOp>();
                foreach (KeyValuePair<string,Lexical> l in obj.lexicals) {
                    if (l.Value is LexCommon) {
                        LexCommon lx = (LexCommon)l.Value; /* XXX cname */
                        thaw.Add(CpsOp.SetSField(lx.stg,
                            CpsOp.MethodCall(Tokens.Kernel_GetVar,
                                CpsOp.StringLiteral(lx.package.Resolve<Package>().who),
                                CpsOp.StringLiteral(lx.name))));
                    } else if (l.Value is LexHint) {
                        LexHint lx = (LexHint)l.Value;
                        thaw.Add(CpsOp.SetSField(lx.stg,
                            CpsOp.MethodCall(Tokens.SubInfo_AddHint,
                                CpsOp.GetSField(obj.subinfo),
                                    CpsOp.StringLiteral(l.Key))));
                    } else if (l.Value is LexSub) {
                        LexSub lx = (LexSub)l.Value;
                        if (obj.protopad == null) continue;
                        SetProtolex(lx, CpsOp.MethodCall(
                                Tokens.Kernel_NewROScalar, CpsOp.GetField(Tokens.SubInfo_protosub,
                                CpsOp.GetSField(lx.def.Resolve<StaticSub>().subinfo))));
                    } else if (l.Value is LexDispatch) {
                        LexDispatch lx = (LexDispatch)l.Value;
                        if (obj.protopad == null) continue;
                        latefrags.Add(CpsOp.LexAccess(lx,0,true, new CpsOp[] {
                            aux[ix].MakeDispatch(l.Key, true) }));
                    } else if (l.Value is LexLabel) {
                        LexLabel lx = (LexLabel)l.Value;
                        if (obj.protopad == null) continue;
                        SetProtolex(lx, CpsOp.MethodCall(
                            Tokens.Kernel_NewLabelVar,
                            CpsOp.GetSField(obj.protopad),
                            CpsOp.StringLiteral(l.Key)));
                    } else if (l.Value is LexSimple) {
                        LexSimple lx = (LexSimple)l.Value;
                        if (obj.protopad == null) continue;
                        MethodInfo type =
                            ((lx.flags & LexSimple.HASH) != 0) ? Tokens.Kernel_CreateHash :
                            ((lx.flags & LexSimple.LIST) != 0) ? Tokens.Kernel_CreateArray : null;
                        if (type != null) {
                            SetProtolex(lx, CpsOp.MethodCall(type));
                        } else if ((lx.flags & LexSimple.DEFOUTER) != 0) {
                            StaticSub oi = obj.outer.Resolve<StaticSub>();
                            Lexical li = null;
                            string ln = l.Key;
                            while (true) {
                                if (!oi.l_lexicals.TryGetValue(ln, out li))
                                    oi = oi.outer.Resolve<StaticSub>();
                                else if (li is LexAlias)
                                    ln = (li as LexAlias).to;
                                else
                                    break;
                            }
                            SetProtolex(lx, CpsOp.LexAccess(li, 0,true, new CpsOp[0]));
                        } else if ((lx.flags & LexSimple.ROINIT) != 0) {
                            SetProtolex(lx, CpsOp.GetField(
                                Tokens.DMO_typeVar, CpsOp.GetSField(
                                    Tokens.Kernel_AnyMO)));
                        } else {
                            FieldInfo tc = lx.type == null ?
                                Tokens.Kernel_AnyMO :
                                lx.type.Resolve<Package>().metaObject;
                            SetProtolex(lx, CpsOp.MethodCall(
                                Tokens.Kernel_NewTypedScalar,
                                CpsOp.GetSField(tc)));
                        }
                    }
                }
                foreach (CpsOp o in latefrags) thaw.Add(o);
            });

            int stash_base = unit.thaw_heap.Count;
            unit.EmitInt(unit.nslog.Length);
            foreach (object le in unit.nslog) {
                object[] lea = (object[]) le;
                unit.EmitStr(JScalar.S(lea[0])); //who
                unit.EmitStr(JScalar.S(lea[1])); //name
                unit.EmitXref(Xref.from(lea[2])); //what
            }
            thaw.Add(CpsOp.MethodCall(Tokens.RuntimeUnit.GetMethod("LoadStashes"), CpsOp.GetSField(unit.rtunit), CpsOp.IntLiteral(stash_base)));

            thaw.Add(CpsOp.MethodCall(Tokens.SubInfo.GetMethod("SetStringHint"),
                CpsOp.GetSField(unit.mainline_ref.Resolve<StaticSub>().subinfo),
                CpsOp.StringLiteral("$?FILE"), CpsOp.StringLiteral(unit.filename ?? "(eval)")));
            thaw.Add(CpsOp.MethodCall(Tokens.Kernel_FirePhasers,
                CpsOp.IntLiteral(Kernel.PHASER_UNIT_INIT), CpsOp.BoolLiteral(false)));
            if (asmain)
                thaw.Add(CpsOp.MethodCall(Tokens.Kernel_FirePhasers,
                    CpsOp.IntLiteral(Kernel.PHASER_INIT), CpsOp.BoolLiteral(false)));
            // settings are incomplete modules and have no mainline to run
            if (unit.is_eval) {
                if (Verbose > 0) Console.WriteLine("mainline_runner(eval)");
                StaticSub m = unit.mainline_ref.Resolve<StaticSub>();
                thaw.Add(CpsOp.CpsReturn(CpsOp.SubyCall(false,"",
                    CpsOp.GetField(Tokens.SubInfo_protosub,
                        CpsOp.GetSField(m.subinfo)))));
            } else if (unit.bottom_ref == null) {
                if (Verbose > 0) Console.WriteLine("mainline_runner(norm)");
                Type dty = typeof(Dictionary<string,Object>);
                FieldInfo lex = Tokens.Frame.GetField("lex");
                MethodInfo set = dty.GetMethod("set_Item");
                thaw.Add(CpsOp.SetField(lex, CpsOp.CallFrame(),
                    CpsOp.ConstructorCall(dty.GetConstructor(new Type[0]), new CpsOp[0])));
                string s = (unit.setting_ref != null) ? unit.setting_ref.unit
                    : null;
                StaticSub m = unit.mainline_ref.Resolve<StaticSub>();
                while (s != null) {
                    thaw.Add(CpsOp.MethodCall(set,
                        CpsOp.GetField(lex, CpsOp.CallFrame()),
                        CpsOp.StringLiteral("*resume_" + s),
                        CpsOp.GetSField(m.subinfo)));
                    Unit su = CLRBackend.GetUnit(s);
                    s = (su.setting_ref != null) ? su.setting_ref.unit : null;
                    m = su.mainline_ref.Resolve<StaticSub>();
                }
                thaw.Add(CpsOp.CpsReturn(CpsOp.SubyCall(false,"",
                    CpsOp.GetField(Tokens.SubInfo_protosub,
                        CpsOp.GetSField(m.subinfo)))));
            } else {
                if (Verbose > 0) Console.WriteLine("mainline_runner(setting)");
                thaw.Add(CpsOp.CpsReturn(
                    CpsOp.MethodCall(Tokens.Kernel_NewROScalar,
                        CpsOp.GetSField(Tokens.Kernel_AnyP))));
            }

            if (Verbose > 0) Console.WriteLine("constants");

            List<CpsOp> tdep_rtu = new List<CpsOp>();
            foreach (Unit td in unit.id_to_tdep)
                tdep_rtu.Add(td == unit ? CpsOp.Null(Tokens.RuntimeUnit) :
                        CpsOp.GetSField(td.rtunit));
            List<CpsOp> unit_load = new List<CpsOp>();
            unit_load.Add(null);
            unit_load.Add(unit.EmitCCConsts());
            unit_load.Add(unit.EmitCCListConsts());
            unit_load.Add(unit.EmitAltInfoConsts());
            unit_load.Add(unit.EmitVarConsts());
            unit_load.Add(unit.EmitStringListConsts());

            CpsOp mkheap;
            // https://bugzilla.novell.com/show_bug.cgi?id=696817
            if (CLRBackend.Current.dynamic) {
                mkheap = CpsOp.Null(typeof(byte[]));
                RuntimeUnit.RegisterHeap(unit.name, unit.thaw_heap.ToArray());
            } else {
                mkheap = CpsOp.NewByteArray(typeof(byte), unit.thaw_heap.ToArray());
            }

            unit_load[0] = CpsOp.SetSField(unit.rtunit, CpsOp.ConstructorCall(
                Tokens.RuntimeUnit.GetConstructor(new Type[] { typeof(string), typeof(Type), typeof(byte[]), typeof(RuntimeUnit[]), typeof(int) }),
                CpsOp.StringLiteral(unit.name),
                CpsOp.TypeLiteral(tb),
                mkheap,
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
            avail_units[root.name] = root;
            root.is_eval = (argv == null);
            root.is_mainish = true;
            CLRBackend old_Current = Current;
            Dictionary<string,Unit> old_used_units = used_units;
            string op = Environment.GetEnvironmentVariable("NIECZA_FORCE_SAVE") == null ? null : root.name + "-TEMP.dll";
            CLRBackend c = new CLRBackend(dir, root.name, op);
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
            if (op != null)
                c.Finish(op);

            root.clrType = c.tb.CreateType();
            used_units = old_used_units; Current = old_Current;

            Builtins.eval_result = (DynBlockDelegate)
                Delegate.CreateDelegate(typeof(DynBlockDelegate),
                    root.clrType, "BOOT");
            if (argv != null) {
                Kernel.RunLoop(root.name, argv, Builtins.eval_result);
            }
        }

        public static void Main(string[] args) {
            if (args.Length == 3 && args[0] == "-run") {
                RunMain(args[1], File.ReadAllText(args[2]), new string[0]);
                return;
            }
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
            avail_units[root.name] = root;
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
            root.clrType = c.tb.CreateType();
            used_units = null; Current = null;
        }
    }

    // instantiatable for the sake of reflecty loading
    public abstract class CallReceiver : MarshalByRefObject, IDictionary {
        public bool IsFixedSize { get { return false; } }
        public bool IsReadOnly { get { return false; } }
        public bool IsSynchronized { get { return false; } }
        public int Count { get { return 0; } }
        public object SyncRoot { get { return null; } }
        public ICollection Keys { get { return null; } }
        public ICollection Values { get { return null; } }
        public void Add(object a, object b) { }
        public void Clear() { }
        public IDictionaryEnumerator GetEnumerator() { return null; }
        IEnumerator IEnumerable.GetEnumerator() { return null; }
        public bool Contains(object a) { return false; }
        public void CopyTo(Array a, int offs) { }
        public void Remove(object a) { }
        public abstract object this[object i] { get; set; }
    }

    class Handle: MarshalByRefObject {
        object to;

        public Handle(object to) { this.to = to; }
        public static object Unbox(object h) {
            return h == null ? null : ((Handle)h).to;
        }

        public override string ToString() {
            return string.Format("{0}[{1:X}]", to.ToString(), to.GetHashCode());
        }
    }

    public class DowncallReceiver : CallReceiver {
        [ThreadStatic] static RuntimeUnit currentUnit;
        public override object this[object i] {
            set { }
            get { return Call((object[]) i); }
        }
        static bool TraceDown = Environment.GetEnvironmentVariable("NIECZA_TRACE_DOWNCALLS") != null;

        object AddLexical(object[] args, LexInfo li) {
            li.owner = (SubInfo)Handle.Unbox(args[1]);
            li.name  = (string)args[2];
            li.file  = (string)args[3];
            li.line  = (int)   args[4];
            li.pos   = (int)   args[5];

            LexInfo other;

            if (li.owner.dylex.TryGetValue(li.name, out other))
                return new object[] { "collision", li.name, li.file, li.line,
                    other.file, other.line };
            SubInfo.UsedInScopeInfo uisi;
            if (li.name != "$_" && li.owner.used_in_scope.TryGetValue(li.name, out uisi)) // $_ HACK
                return new object[] { "already-bound", li.name, uisi.levels,
                    uisi.line, li.file, li.line, uisi.orig_file, uisi.orig_line };
            li.owner.dylex[li.name] = li;
            li.owner.dylex_filter |= SubInfo.FilterForName(li.name);
            if (li.name == "self")
                li.owner.self_key = li.SigIndex();
            li.BindFields();

            return new object[] { "" };
        }

        object Call(object[] args) {
            if (TraceDown) {
                Console.WriteLine(args.Length);
                foreach(object a in args)
                    Console.WriteLine(a);
            }
            string cmd = (string) args[0];
            if (cmd == "set_parent") {
                Builtins.up_domain = (AppDomain)args[1];
                return null;
            } else if (cmd == "new_unit") {
                return new Handle(new RuntimeUnit((string)args[1],
                        (string)args[2], (string)args[3], (bool)args[4]));
            } else if (cmd == "set_current_unit") {
                currentUnit = (RuntimeUnit)Handle.Unbox(args[1]);
                Kernel.currentGlobals = currentUnit.globals;
                return null;
            } else if (cmd == "set_mainline") {
                currentUnit.mainline = (SubInfo)Handle.Unbox(args[1]);
                currentUnit.mainline.special |= RuntimeUnit.SUB_MAINLINE;
                return null;
            } else if (cmd == "sub_get_unit") {
                return new Handle(((SubInfo)Handle.Unbox(args[1])).unit);
            } else if (cmd == "lex_names") {
                List<object> ret = new List<object>();
                foreach (string k in ((SubInfo)Handle.Unbox(args[1])).dylex.Keys)
                    ret.Add(k);
                return ret.ToArray();
            } else if (cmd == "unused_lexicals") {
                SubInfo s = (SubInfo)Handle.Unbox(args[1]);
                List<object> ret = new List<object>();
                foreach (KeyValuePair<string,LexInfo> kv in s.dylex) {
                    if (s.used_in_scope.ContainsKey(kv.Key))
                        continue;
                    ret.Add(kv.Key);
                    ret.Add(kv.Value.pos);
                }
                return ret.ToArray();
            } else if (cmd == "unit_stubbed_stashes") {
                RuntimeUnit u = (RuntimeUnit)Handle.Unbox(args[1]);
                List<object> ret = new List<object>();
                foreach (KeyValuePair<int,STable> kv in u.stubbed_stashes) {
                    ret.Add(kv.Key);
                    ret.Add(new Handle(kv.Value));
                }
                return ret.ToArray();
            } else if (cmd == "unit_stub_stash") {
                RuntimeUnit u = (RuntimeUnit)Handle.Unbox(args[1]);
                int    pos = (int)args[2];
                STable type = (STable)Handle.Unbox(args[3]);
                u.stubbed_stashes.Add(new KeyValuePair<int,STable>(pos,type));
                return null;
            } else if (cmd == "unit_get_name") {
                return ((RuntimeUnit)Handle.Unbox(args[1])).name;
            } else if (cmd == "sub_to_unit") {
                SubInfo s = (SubInfo)Handle.Unbox(args[1]);
                while ((s.special & RuntimeUnit.SUB_MAINLINE) == 0)
                    s = s.outer;
                return new Handle(s);
            } else if (cmd == "equal_handles") {
                return Handle.Unbox(args[1]) == Handle.Unbox(args[2]);
            } else if (cmd == "sub_is_routine") {
                SubInfo s = (SubInfo)Handle.Unbox(args[1]);
                return s.mo.HasMRO(Kernel.RoutineMO);
            } else if (cmd == "sub_has_lexical") {
                SubInfo s = (SubInfo)Handle.Unbox(args[1]);
                return s.dylex.ContainsKey((string)args[2]);
            } else if (cmd == "rel_pkg") {
                bool auto = (bool)args[1];
                STable pkg = args[2] == null ? null : (STable)Handle.Unbox(args[2]);
                RuntimeUnit c = currentUnit;
                for (int i = 3; i < args.Length; i++) {
                    string key = (string) args[i];
                    string who = "";
                    if (pkg != null) {
                        if (!pkg.who.Isa(Kernel.StashMO))
                            return new Exception(pkg.name + " fails to name a standard package");
                        who = Kernel.UnboxAny<string>(pkg.who);
                    }
                    StashEnt v;
                    string hkey = (char)who.Length + who + key;
                    if (c.globals.TryGetValue(hkey, out v)) {
                        if (v.v.rw || v.v.Fetch().IsDefined())
                            return new Exception((who + "::" + key).Substring(2) + " names a non-package");
                        pkg = v.v.Fetch().mo;
                    } else if (!auto) {
                        return new Exception((who + "::" + key).Substring(2) + " does not name any package");
                    } else {
                        c.globals[hkey] = v = new StashEnt();
                        v.v = StashCursor.MakePackage((who + "::" + key).Substring(2), Kernel.BoxRaw<string>(who + "::" + key, Kernel.StashMO));
                        pkg = v.v.Fetch().mo;
                    }
                }
                return new Handle(pkg);
            } else if (cmd == "get_name") {
                string who  = (string)args[1];
                string key  = (string)args[2];
                string hkey = (char)who.Length + who + key;
                StashEnt b;
                if (Kernel.currentGlobals.TryGetValue(hkey, out b)) {
                    if (!b.v.rw && !b.v.Fetch().IsDefined()) {
                        return new object[] {
                            new Handle(b.v.Fetch().mo), true
                        };
                    } else if (!b.v.rw && b.v.Fetch().Isa(Kernel.CodeMO)) {
                        return new object[] {
                            new Handle(b.v.Fetch().GetSlot("info")), false
                        };
                    } else return null;
                } else {
                    return null;
                }
            } else if (cmd == "create_sub") {
                string name = (string)args[1];
                SubInfo outer = (SubInfo)Handle.Unbox(args[2]);
                string cls = (string)args[3];
                STable pkg = (STable)Handle.Unbox(args[4]);
                bool once = (bool)args[5];

                // this happens before lexicals are created, so we can't
                // use lexicals.
                STable rcls = (cls == "Sub") ? Kernel.SubMO :
                    (cls == "Routine") ? Kernel.RoutineMO :
                    (cls == "Method") ? Kernel.MethodMO :
                    (cls == "Submethod") ? Kernel.SubmethodMO :
                    (cls == "WhateverCode") ? Kernel.WhateverCodeMO :
                    (cls == "Block") ? Kernel.BlockMO :
                    (cls == "Code") ? Kernel.CodeMO : null;

                if (rcls == null)
                    return new Exception("sub-class lookup fail for " + cls);

                return new Handle(new SubInfo(currentUnit, name, outer,
                            rcls, pkg, once));
            } else if (cmd == "add_my_name") {
                STable  type  = (STable)Handle.Unbox(args[6]);
                int     flags = (int)   args[7];

                return AddLexical(args, new LISimple(flags, type));
            } else if (cmd == "add_hint") {
                return AddLexical(args, new LIHint());
            } else if (cmd == "add_label") {
                return AddLexical(args, new LILabel());
            } else if (cmd == "add_dispatcher") {
                return AddLexical(args, new LIDispatch());
            } else if (cmd == "add_common_name") {
                STable  pkg   = (STable)Handle.Unbox(args[6]);
                string  pname = (string)args[7];
                if (!pkg.who.Isa(Kernel.StashMO))
                    return new Exception("NYI usage of a nonstandard package");
                string  who   = Kernel.UnboxAny<string>(pkg.who);

                string err = currentUnit.NsBind(who, pname, null,
                        (string)args[3], (int)args[4]);
                if (err != null) return new Exception(err);
                return AddLexical(args, new LICommon((char)who.Length + who + pname));
            } else if (cmd == "add_state_name") {
                SubInfo sub   = (SubInfo)Handle.Unbox(args[1]);
                SubInfo outer = (sub.special & RuntimeUnit.SUB_MAINLINE) != 0 ?
                    sub : sub.outer;
                STable  type  = (STable)Handle.Unbox(args[6]);
                int     flags = (int)   args[7];
                string  back  = (string)args[8];
                string  name  = (string)args[2];

                args[1] = outer;
                args[2] = back;
                AddLexical(args, new LISimple(flags, type));
                args[1] = sub;
                args[2] = name;
                return AddLexical(args, new LIAlias(back));
            } else if (cmd == "add_my_stash") {
                STable  type  = (STable)Handle.Unbox(args[6]);

                return AddLexical(args, new LIPackage(type));
            } else if (cmd == "add_my_sub") {
                SubInfo body  = (SubInfo)Handle.Unbox(args[6]);

                return AddLexical(args, new LISub(body));
            } else if (cmd == "sub_no_signature") {
                SubInfo tgt = (SubInfo)Handle.Unbox(args[1]);
                tgt.sig_i = null;
                tgt.sig_r = null;
                return null;
            } else if (cmd == "set_signature") {
                SubInfo tgt = (SubInfo)Handle.Unbox(args[1]);
                int ix = 2;
                List<int> sig_i = new List<int>();
                List<object> sig_r = new List<object>();
                while (ix != args.Length) {
                    int    flags = (int)   args[ix++];
                    string name  = (string)args[ix++];
                    string slot  = (string)args[ix++];
                    sig_r.Add(name);
                    int nnames = 0;
                    while(true) {
                        string a_name = (string)args[ix++];
                        if (a_name == null) break;
                        nnames++;
                        sig_r.Add(a_name);
                    }
                    SubInfo deflt = (SubInfo)Handle.Unbox(args[ix++]);
                    STable  type  = (STable)Handle.Unbox(args[ix++]);
                    if (deflt != null) sig_r.Add(deflt);
                    if (type != null) sig_r.Add(type);
                    sig_i.Add(flags);
                    sig_i.Add(slot == null ? -1 : tgt.dylex[slot].SigIndex());
                    sig_i.Add(nnames);
                }
                tgt.sig_i = sig_i.ToArray();
                tgt.sig_r = sig_r.ToArray();
                return null;
            } else if (cmd == "sub_finish") {
                // TODO: do something here
                return null;
            } else if (cmd == "run_unit") {
                return null;
            } else if (cmd == "post_save") {
                CLRBackend.Main(new string[] { (string)args[1],
                        (string)args[2], (string)args[3], (string)args[4] });
                return null;
            } else if (cmd == "runnam" || cmd == "evalnam") {
                if (CLRBackend.Verbose > 0) {
                    Console.WriteLine("Eval code::");
                    Console.WriteLine(args[2]);
                }
                string[] argv = new string[args.Length - 3];
                Array.Copy(args, 3, argv, 0, argv.Length);
                try {
                    CLRBackend.RunMain((string)args[1], (string)args[2],
                            cmd == "evalnam" ? null : argv);
                }
                catch (Exception ex) {
                    return ex.ToString();
                }
                return null;
            } else if (cmd == "setnames") {
                Builtins.execName = (string)args[1];
                Builtins.programName = (string)args[2];
                return null;
            } else if (cmd == "replrun") {
                string ret = "";
                try {
                    StashEnt b = Kernel.GetVar("::PROCESS", "$OUTPUT_USED");
                    b.v = Kernel.FalseV;
                    // hack to simulate a settingish environment
                    Variable r = Kernel.RunInferior(
                        Kernel.GetInferiorRoot().MakeChild(null,
                            new SubInfo("<repl>", Builtins.eval_result),
                            Kernel.AnyP));
                    if (!b.v.Fetch().mo.mro_raw_Bool.Get(b.v)) {
                        Variable pl = Kernel.RunInferior(
                            r.Fetch().InvokeMethod(Kernel.GetInferiorRoot(),
                                "gist", new Variable[] { r }, null));
                        Console.WriteLine(pl.Fetch().mo.mro_raw_Str.Get(pl));
                    }
                } catch (Exception ex) {
                    ret = ex.Message;
                }
                return ret;
            } else if (cmd == "safemode") {
                Kernel.SaferMode = true;
                return null;
            } else {
                return new Exception("ERROR");
            }
        }
    }
}
