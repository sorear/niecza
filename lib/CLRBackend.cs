// This is the new CLR backend.  The old one generated C# from Perl, which
// was slow and gave us the limitations of C#; this one aims to be faster.
// Also, by making the Perl code emit a portable format, it makes future
// portability work easier.

using System;
using System.Reflection;
using System.Reflection.Emit;
using System.Collections.Generic;
using System.Text;

using Niecza;

namespace Niecza.CLRBackend {
    // The portable format is a subset of JSON, and is current read
    // into a matching internal form.
    class JScalar {
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

    class Unit {
        public static object[] mainline_ref(object[] u) { return (object[])u[0]; }
        public static string name(object[] u) { return ((JScalar)u[1]).str; }
        public static object[] log(object[] u) { return (object[])u[2]; }
        public static string setting(object[] u) { return u[3] as string; }
        public static object[] bottom_ref(object[] u) { return u[4] as object[]; }
        public static object[] xref(object[] u) { return u[5] as object[]; }
        public static object[] tdeps(object[] u) { return u[6] as object[]; }
    }

    // Extra info needed beyond what ILGenerator alone provides.  Note
    // that switch generation is done in another pass.
    class CgContext {
        public ILGenerator il;
        public int next_case;
        public Label[] cases;
        public int num_cases;
        public Dictionary<string,int> named_cases;
        public string[] let_names;
        public Type[] let_types;

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
    }

    sealed class Tokens {
        public static readonly FieldInfo Frame_ip =
            typeof(Frame).GetField("ip");
    }

    // This are expressional CLR operators.  This is lower level than the
    // CPS stuff; if HasCases is true, Returns must be void.  Thus,
    // there is no need to handle argument spills.
    abstract class ClrOp {
        public bool HasCases;
        public Type Returns;
        public abstract void CodeGen(CgContext cx);
        public abstract void ListCases(CgContext cx);

        protected static void TypeCheck(Type sub, Type super, string msg) {
            if (!super.IsAssignableFrom(sub))
                throw new Exception(msg + " " + sub + " not subtype of " + super);
        }
    }

    class ClrMethodCall : ClrOp {
        public readonly MethodInfo Method;
        public readonly ClrOp[] Zyg;

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
            }
            cx.il.Emit(OpCodes.Callvirt, Method); // XXX C#
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
            Returns = cps ? typeof(void) : mi.ReturnType;
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
                throw new Exception("argument list length mismatch");

            for (int i = 0; i < ts.Count; i++) {
                TypeCheck(zyg[i].Returns, ts[i], "arg");
            }
        }
    }

    public class MainClass {
        public static void Main() {
            string tx = (new System.IO.StreamReader(Console.OpenStandardInput(), Console.InputEncoding)).ReadToEnd();
            object[] root = (object[])Reader.Read(tx);
            object[] xref = Unit.xref(root);

            for (int i = 0; i < xref.Length; i++) {
                object[] o = xref[i] as object[];
                if (o == null)
                    Console.WriteLine("Empty slot");
                else if (o.Length > 6)
                    Console.WriteLine("sub {0}", ((JScalar)o[0]).str);
                else
                    Console.WriteLine("{0} {1}", ((JScalar)o[0]).str, ((JScalar)o[1]).str);
            }
        }
    }
}
