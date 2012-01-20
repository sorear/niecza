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
    static class JScalar {
        public static string[] SA(int cut, object x) {
            object[] arr = (object[]) x;
            string[] r = new string[ arr.Length - cut ];
            for (int i = 0; i < r.Length; i++)
                r[i] = S(arr[i+cut]);
            return r;
        }
        public static int[] IA(int cut, object x) {
            object[] arr = (object[]) x;
            int[] r = new int[ arr.Length - cut ];
            for (int i = 0; i < r.Length; i++)
                r[i] = I(arr[i+cut]);
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
        public static int I(object x) { return (int)N(x); }
        public static double N(object x) { return Utils.S2N((string)x); }
        public static int IN(object x) { return x == null ? -1 : (int)N(x); }
        public static string S(object x) { return x == null ? null : ((string)x); }
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

        public static object Read(string inputx, object[] refs) {
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
                    containers[containers.Count - 1].Add(new string(input, start, write - start));
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
                if (input[start] == '!') {
                    string str = new string(input, start+1, ix - start - 1);
                    containers[containers.Count - 1].Add(refs[int.Parse(str)]);
                } else {
                    containers[containers.Count - 1].Add(new string(input, start, ix - start));
                }
            }
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
            // the mono JIT checks for this exact sequence
            il.Emit(OpCodes.Newarr, ty);
            if (vec.Length != 0) {
                FieldBuilder fb = tb.DefineInitializedData(
                        "A" + (EmitUnit.Current.nextid++), vec, 0);
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
                    (Config.CGVerifiable || t.IsValueType)) {
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
            RxFrame.GetConstructor(new Type[] { String, Cursor, Boolean });
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

        public static readonly MethodInfo P6any_InvokeMethod =
            P6any.GetMethod("InvokeMethod");
        public static readonly MethodInfo P6any_Invoke =
            P6any.GetMethod("Invoke");
        public static readonly MethodInfo P6any_SetSlot =
            P6any.GetMethod("SetSlot", new Type[] { STable, String, typeof(object) });
        public static readonly MethodInfo P6any_GetSlot =
            P6any.GetMethod("GetSlot", new Type[] { STable, String });
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
        public static readonly MethodInfo Kernel_GetGlobal =
            typeof(Kernel).GetMethod("GetGlobal");
        public static readonly MethodInfo Kernel_BindGlobal =
            typeof(Kernel).GetMethod("BindGlobal");
        public static readonly MethodInfo Kernel_NewROScalar =
            typeof(Kernel).GetMethod("NewROScalar");
        public static readonly MethodInfo Kernel_NewRWListVar =
            typeof(Kernel).GetMethod("NewRWListVar");
        public static readonly MethodInfo Kernel_NewRWScalar =
            typeof(Kernel).GetMethod("NewRWScalar");
        public static readonly MethodInfo Kernel_NewTypedScalar =
            typeof(Kernel).GetMethod("NewTypedScalar");
        public static readonly MethodInfo Kernel_Assign =
            typeof(Kernel).GetMethod("Assign");
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
        public static readonly MethodInfo Frame_Binder =
            Frame.GetMethod("Binder");

        public static readonly FieldInfo P6any_mo =
            P6any.GetField("mo");
        public static readonly FieldInfo StashEnt_v =
            typeof(StashEnt).GetField("v");
        public static readonly FieldInfo SubInfo_protosub =
            SubInfo.GetField("protosub");
        public static readonly FieldInfo SubInfo_protopad =
            SubInfo.GetField("protopad");
        public static readonly FieldInfo SubInfo_mo =
            SubInfo.GetField("mo");
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
        [Immutable]
        public static readonly FieldInfo[] Frame_lexi32 = new FieldInfo[] {
            typeof(Frame).GetField("lexi0"),
            typeof(Frame).GetField("lexi1")
        };
        [Immutable]
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

        // other random stuff
        public static readonly ClrOp[] EmptyClrOp = new ClrOp[0];
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
                throw new Exception(sub + " not subtype of " + super + ": " + c);
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

        public ClrMethodCall(bool cps, MethodInfo mi, params ClrOp[] zyg) {
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
                TypeCheck(zyg[i].Returns, ts[i], mi);
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
        [Immutable] public static ClrNoop Instance = new ClrNoop();
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
        [Immutable] public static ClrSync Instance = new ClrSync();
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
            if (ismethod) TypeCheck(zyg[i++].Returns, Tokens.String, "methodname");
            TypeCheck(zyg[i++].Returns, ismethod ? Tokens.Variable : Tokens.P6any, "sub");
            int j = 0;
            while (j < sig.Length) {
                string s = sig.Substring(j+1, sig[j]);
                j += (1 + s.Length);
                TypeCheck(zyg[i++].Returns, (s == "flatcap") ? Tokens.P6any : Tokens.Variable, j);
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
            if (Config.CGVerifiable || Returns.IsValueType)
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
        [Immutable] public static ClrCpsFrame Instance = new ClrCpsFrame();
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
                TypeCheck(c.Returns, r, "new-array");
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

        public CpsOp(ClrOp head) : this(Tokens.EmptyClrOp, head) { }
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
                    string ln = "!spill" + EmitUnit.Current.nextid++;
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

            int k = -1;
            foreach (CpsOp t in terms)
                k += 1 + t.stmts.Length;
            ClrOp[] stmts = new ClrOp[k];
            k = 0;

            for (int i = 0; i < terms.Length - 1; i++) {
                if (terms[i].head.Returns != Tokens.Void)
                    throw new Exception("Non-void expression used in nonfinal sequence position" + terms[i].head.Returns);
                foreach (ClrOp s in terms[i].stmts)
                    stmts[k++] = s;
                stmts[k++] = terms[i].head;
            }

            foreach (ClrOp s in terms[terms.Length - 1].stmts)
                stmts[k++] = s;
            return new CpsOp(stmts, terms[terms.Length - 1].head);
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

            EmitUnit eu = EmitUnit.Current;
            string l1 = "!else"  + (eu.nextid++);
            string l2 = "!endif" + (eu.nextid++);

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
            EmitUnit eu = EmitUnit.Current;
            string l1 = "!again" + (eu.nextid++);
            string l2 = "!check" + (eu.nextid++);

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

        public static CpsOp LexAccess(LexInfo l, int up, CpsOp[] zyg) {
            return Primitive(zyg, delegate(ClrOp[] heads) {
                return new CpsOp((heads.Length >= 1)
                    ? l.SetCode(up, heads[0], EmitUnit.Current.np.sub)
                    : l.GetCode(up, EmitUnit.Current.np.sub));
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

        public static CpsOp Widen(Type to, CpsOp z) {
            return new CpsOp(z.stmts, new ClrWiden(to, z.head));
        }
    }

    class CpsBuilder {
        public readonly TypeBuilder tb;
        public readonly MethodBuilder mb;
        public readonly CgContext cx;
        public readonly EmitUnit eu;

        public CpsBuilder(EmitUnit eu, string clrname, bool pub) {
            this.eu = eu;
            this.tb = eu.type_builder;
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
        internal SubInfo sub;
        public readonly CpsBuilder cpb;
        Dictionary<string, Type> let_types = new Dictionary<string, Type>();
        List<List<ClrEhSpan>> eh_stack = new List<List<ClrEhSpan>>();
        List<object[]> scope_stack = new List<object[]>();

        internal List<KeyValuePair<AltInfo, string[]>> altinfo_fixups =
            new List<KeyValuePair<AltInfo, string[]>>();

        public NamProcessor(CpsBuilder cpb, SubInfo sub) {
            this.sub = sub;
            this.cpb = cpb;
        }

        CpsOp AccessLex(object[] zyg) {
            return RawAccessLex( JScalar.S(zyg[0]), JScalar.S(zyg[1]),
                (zyg.Length > 2 ? Scan(zyg[2]) : null));
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

        CpsOp RawAccessLex(string type, string name, CpsOp set_to) {
            bool core = type == "corelex";
            int outer = (type == "outerlex") ? 1 : 0;
            int uplevel;

            CpsOp r;
            if (!core && (r = CheckScopes(name, ref outer, set_to)) != null)
                return r;

            LexInfo lex = ResolveLex(name, outer>0, out uplevel, core);

            return CpsOp.LexAccess(lex, uplevel,
                set_to == null ? new CpsOp[0] : new CpsOp[] { set_to });
        }

        LexInfo ResolveLex(string name, bool upf, out int uplevel, bool core) {
            uplevel = 0;
            SubInfo csr = sub;

            if (upf) {
                csr = csr.outer;
                uplevel++;
            }

            while (true) {
                LexInfo r;
                if ((!core || csr.unit.name == "CORE") &&
                        csr.dylex.TryGetValue(name, out r)) {
                    if (r is LIAlias) {
                        name = (r as LIAlias).to;
                        continue;
                    } else {
                        return r;
                    }
                }
                if (csr.outer == null)
                    throw new Exception("Unable to find lexical " + name + " in " + sub.name);
                csr = csr.outer;
                uplevel++;
            }
        }

        STable ResolvePkg(string name) {
            int dummy;
            LexInfo li = ResolveLex(name, false, out dummy, true);
            return ((LIPackage)li).pkg;
        }

        // synchronize with LIDispatch.MakeDispatch
        internal CpsOp MakeDispatch(string prefix) {
            HashSet<string> names = new HashSet<string>();
            List<CpsOp> cands = new List<CpsOp>();
            CpsOp proto = CpsOp.Null(Tokens.P6any);
            string filter = prefix + ":";
            string pn = prefix + ":(!proto)";

            for (SubInfo csr = sub; ; csr = csr.outer) {
                bool brk = false;
                foreach (KeyValuePair<string,LexInfo> kp in csr.dylex) {
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
                if (csr.dylex.ContainsKey(pn)) {
                    proto = CpsOp.MethodCall(Tokens.Variable_Fetch, RawAccessLex("scopedlex", pn, null));
                    break;
                }
                if (brk) cands.Add(CpsOp.Null(Tokens.P6any));
            }

            return CpsOp.MethodCall(Tokens.Kernel_NewROScalar,
                CpsOp.MethodCall(Tokens.Kernel_MakeDispatcher,
                    CpsOp.StringLiteral(prefix), proto,
                    CpsOp.NewArray(Tokens.P6any, cands.ToArray())));
        }

        object[] InlineCall(SubInfo tgt, object[] zyg) {
            EmitUnit eu = EmitUnit.Current;
            object tgzyg = Reader.Read(tgt.nam_str, tgt.nam_refs);

            string[] lex_names = (new List<string>(tgt.dylex.Keys)).ToArray();
            // locale-independant code generation
            Array.Sort(lex_names, string.CompareOrdinal);

            // letscope: gives visible names to lets
            List<object> scope = new List<object>();
            scope.Add("letscope");
            scope.Add((tgt.special & SubInfo.TRANSPARENT) != 0 ? "1" : "0");

            // let: introduces symbols for !argN, !lexN
            List<object> let = new List<object>();
            let.Add("letn");

            List<object> bind = new List<object>();
            bind.Add("_inlinebind");
            bind.Add(tgt.name);
            bind.Add(tgt.sig);
            bind.Add(null);

            // give unique names to the arguments
            foreach (object a in zyg) {
                string alias = "!arg" + eu.nextid++;
                bind.Add(new object[] { "letvar", alias });
                let.Add(alias);
                let.Add(a);
            }


            string[] slot_to_lex = new string[tgt.num_lex_slots];

            for (int i = 0; i < lex_names.Length; i++) {
                string   ln = lex_names[i];
                LISimple li = (LISimple)tgt.dylex[ln]; // checked in inlinable()
                string let_name = "!var" + eu.nextid++;

                slot_to_lex[li.index] = ln;

                let.Add(let_name);
                if ((li.flags & LISimple.NOINIT) != 0)
                    let.Add(new object[] { "null", "var" });
                else if ((li.flags & LISimple.ROINIT) != 0)
                    let.Add(new object[] { "scopedlex", "Any" });
                else if ((li.flags & LISimple.DEFOUTER) != 0)
                    let.Add(new object[] { "scopedlex", ln });
                else if ((li.flags & LISimple.HASH) != 0)
                    let.Add(new object[] { "newhash" });
                else if ((li.flags & LISimple.LIST) != 0)
                    let.Add(new object[] { "newarray" });
                else
                    let.Add(new object[] { "_newoftype",
                            eu.TypeConstant(li.type) });

                scope.Add(ln);
                scope.Add(let_name);
            }

            string[] lexicals_fixup =
                new string[tgt.sig.parms.Length];
            for (int i = 0; i < lexicals_fixup.Length; i ++) {
                int slot = tgt.sig.parms[i].slot;
                if (slot < 0)
                    lexicals_fixup[i] = null;
                else
                    lexicals_fixup[i] = slot_to_lex[slot];
            }
            bind[3] = lexicals_fixup;

            scope.Add(new object[] {
                "prog",
                bind.ToArray(),
                tgzyg
            });

            let.Add(scope.ToArray());

            if (tgt.IsTopicalizer()) {
                int tid = eu.nextid++;
                return new object[] {
                    "xspan", "!start"+tid, "!end"+tid, "0", let.ToArray(),
                    SubInfo.ON_SUCCEED.ToString(), "", "!end"+tid
                };
            } else {
                return let.ToArray();
            }
        }

        CpsOp SubyCall(bool ismethod, object[] zyg) {
            int sh = ismethod ? 3 : 2;
            string sig = JScalar.S(zyg[sh-1]);
            CpsOp[] args = new CpsOp[zyg.Length - 2];
            if (ismethod) {
                args[0] = AnyStr(zyg[1]);
            }
            for (int i = sh; i < zyg.Length; i++)
                args[i-2] = Scan(zyg[i]);
            return CpsOp.SubyCall(ismethod, sig, args);
        }

        static string FixStr(object z) { return JScalar.S(z); }
        static double FixNum(object z) { return JScalar.N(z); }
        static int FixInt(object z) { return (int)FixNum(z); }
        static bool FixBool(object z) { return FixNum(z) != 0; }
        CpsOp AnyStr(object z) {
            return (z is object[]) ? Scan(z) : CpsOp.StringLiteral(FixStr(z));
        }

        [Immutable] static Dictionary<string, Func<NamProcessor, object[], CpsOp>> handlers;
        [Immutable] static Dictionary<string, Func<CpsOp[], CpsOp>> thandlers;
        [Immutable] static Dictionary<string, Type> namtypes;

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
            namtypes["twriter"] = typeof(TextWriter);

            handlers = new Dictionary<string, Func<NamProcessor,object[],CpsOp>>();
            thandlers = new Dictionary<string, Func<CpsOp[], CpsOp>>();

            handlers["null"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.Null(namtype(zyg[1])); };
            handlers["str"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.StringLiteral(JScalar.S(zyg[1])); };
            handlers["int"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.IntLiteral(JScalar.I(zyg[1])); };
            handlers["char"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.CharLiteral(JScalar.S(zyg[1])[0]); };
            handlers["double"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.DoubleLiteral(JScalar.N(zyg[1])); };
            handlers["bool"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.BoolLiteral(FixBool(zyg[1])); };
            handlers["ann"] = delegate(NamProcessor th, object[] zyg) {
                return CpsOp.Annotate(FixInt(zyg[1]), th.Scan(zyg[2])); };
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
            handlers["_inlinebind"] = delegate(NamProcessor th, object[] zyg) {
                CpsOp[] args = JScalar.A<CpsOp>(4, zyg, th.Scan);
                Signature sig = (Signature)zyg[2];
                string[] lexnames = (string[])zyg[3];
                string name = (string)zyg[1];
                EmitUnit eu = EmitUnit.Current;
                List<CpsOp> ops = new List<CpsOp>();
                int aused = 0;
                for (int i = 0; i < sig.parms.Length; i++) {
                    int flags = sig.parms[i].flags;
                    string lex = lexnames[i];
                    STable type = sig.parms[i].type;

                    CpsOp get = null;
                    if (aused < args.Length) {
                        get = args[aused++];
                    } else if ((flags & Parameter.DEFOUTER) != 0) {
                        get = th.RawAccessLex("outerlex", lex, null);
                    } else if ((flags & Parameter.OPTIONAL) != 0) {
                        get = eu.TypeConstantV(type);
                    } else {
                        get = CpsOp.CpsCall(Tokens.Variable, Tokens.Kernel_Die,
                            CpsOp.StringLiteral("No value in "+name+" available for parameter "+sig.parms[i].name));
                    }

                    if (lex == null) {
                        ops.Add(CpsOp.Sink(get));
                    } else if ((flags & Parameter.IS_COPY) != 0) {
                        CpsOp init;
                        if ((flags & Parameter.IS_HASH) != 0) {
                            init = CpsOp.MethodCall(Tokens.Kernel_CreateHash);
                        } else if ((flags & Parameter.IS_LIST) != 0) {
                            init = CpsOp.MethodCall(Tokens.Kernel_CreateArray);
                        } else {
                            init = CpsOp.MethodCall(Tokens.Kernel_NewTypedScalar,
                                    eu.TypeConstant(type));
                        }
                        ops.Add(th.RawAccessLex("scopedlex", lex, init));
                        ops.Add(CpsOp.Sink(CpsOp.MethodCall(
                            Tokens.Kernel_Assign,
                            th.RawAccessLex("scopedlex", lex, null),
                            get)));
                    } else if ((flags & Parameter.RWTRANS) != 0) {
                        ops.Add(th.RawAccessLex("scopedlex", lex, get));
                    } else {
                        int mode = 0;
                        if ((flags & Parameter.READWRITE) != 0)
                            mode = Kernel.NBV_RW;
                        else if ((flags & (Parameter.IS_LIST |
                                Parameter.IS_HASH)) != 0)
                            mode = Kernel.NBV_LIST;
                        ops.Add(th.RawAccessLex("scopedlex", lex,
                            CpsOp.MethodCall(Tokens.Kernel_NewBoundVar,
                                CpsOp.IntLiteral(mode),
                                eu.TypeConstant(type), get)));
                    }
                }

                return CpsOp.Sequence(ops.ToArray());
            };
            handlers["_inline"] = delegate(NamProcessor th, object[] zyg) {
                object[] rzyg = new object[zyg.Length - 2];
                Array.Copy(zyg, 2, rzyg, 0, rzyg.Length);
                return th.Scan(th.InlineCall((SubInfo)zyg[1], rzyg));
            };
            handlers["letscope"] = delegate(NamProcessor th, object[] zyg) {
                List<ClrEhSpan> xn = new List<ClrEhSpan>();
                string s = "!start" + EmitUnit.Current.nextid++;
                string e = "!end" + EmitUnit.Current.nextid++;
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
                CpsOp scope = (zyg[1] is object[]) ? th.Scan(zyg[1]) :
                    th.cpb.eu.TypeConstant(th.ResolvePkg(JScalar.S(zyg[1])));
                return CpsOp.MethodCall(Tokens.P6any_SetSlot,
                    th.Scan(zyg[3]), scope, th.AnyStr(zyg[2]), th.Scan(zyg[4])); };
            handlers["getslot"] = delegate(NamProcessor th, object[] zyg) {
                Type ty = namtype(zyg[3]);
                CpsOp scope = (zyg[1] is object[]) ? th.Scan(zyg[1]) :
                    th.cpb.eu.TypeConstant(th.ResolvePkg(JScalar.S(zyg[1])));
                return CpsOp.UnboxAny(ty, CpsOp.MethodCall(Tokens.P6any_GetSlot,
                    th.Scan(zyg[4]), scope, th.AnyStr(zyg[2]))); };
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
                if (!(zyg[1] is object[])) {
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
                if (zyg[1] is object[]) goto dynamic;
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
                z[0] = zyg[1] is object[] ? th.Scan(zyg[1]) :
                    CpsOp.IntLiteral(FixInt(zyg[1]));

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
                if (!(zyg[1] is object[])) {
                    string name = FixStr(zyg[1]);
                    // these might need to happen *early*, before user classes
                    // are set up
                    if (name == "Str") {
                        mo = CpsOp.GetSField(Tokens.Kernel_StrMO);
                    } else if (name == "Num") {
                        mo = CpsOp.GetSField(Tokens.Kernel_NumMO);
                    } else {
                        int dummy;
                        LexInfo li = th.ResolveLex(name,false,out dummy,true);
                        mo = th.cpb.eu.TypeConstant((li as LIPackage).pkg);
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
                return CpsOp.While(FixBool(z[1]), FixBool(z[2]),
                        th.Scan(z[3]), th.Scan(z[4])); };
            thandlers["_pushleave"] = Methody(null, Tokens.Frame.GetMethod("PushLeave"));
            handlers["_makesub"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_MakeSub,
                    EmitUnit.Current.SubConstant((SubInfo)z[1]),
                    CpsOp.CallFrame()); };
            handlers["_newlabel"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(Tokens.Kernel_NewLabelVar,
                        CpsOp.CallFrame(),
                        CpsOp.StringLiteral(JScalar.S(z[1]))); };
            handlers["_cpsop"] = delegate(NamProcessor th, object[] z) {
                return z[1] as CpsOp; };
            handlers["_newdispatch"] = delegate(NamProcessor th, object[] z) {
                return th.MakeDispatch(JScalar.S(z[1])); };
            thandlers["_binder"] = Methody(Tokens.Void, Tokens.Frame_Binder);
            handlers["class_ref"] = delegate(NamProcessor th, object[] z) {
                string kind = FixStr(z[1]);
                STable m;
                if (z[2] is STable) {
                    m = (STable)z[2];
                } else {
                    m = th.ResolvePkg(JScalar.S(z[2]));
                }
                if (kind == "mo")
                    return th.cpb.eu.TypeConstant(m);
                if (kind == "typeVar")
                    return th.cpb.eu.TypeConstantV(m);
                if (kind == "typeObj")
                    return th.cpb.eu.TypeConstantP(m);
                throw new NotImplementedException("class_ref " + kind);
            };
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
                while (zyg.Length - i >= 3 && !(zyg[i] is object[])) {
                    string name = FixStr(zyg[i]);
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
                CpsOp strs = th.cpb.eu.StringListConst(JScalar.SA(2,z));
                return CpsOp.MethodCall(Tokens.RxFrame_PushCapture,
                    CpsOp.RxFrame(), strs, th.Scan(z[1]));
            };
            handlers["rxincorpshift"] = delegate(NamProcessor th, object[] z) {
                CpsOp strs = th.cpb.eu.StringListConst(JScalar.SA(0,z[1]));

                return CpsOp.Goto("backtrack", true,
                    CpsOp.MethodCall(Tokens.RxFrame.GetMethod("IncorpShift"),
                        CpsOp.RxFrame(), strs,
                        CpsOp.LabelId(th.cpb.cx, JScalar.S(z[2]))));
            };
            handlers["rxincorpcut"] = delegate(NamProcessor th, object[] z) {
                CpsOp strs = th.cpb.eu.StringListConst(JScalar.SA(0,z[1]));

                return CpsOp.Goto("backtrack", true,
                    CpsOp.MethodCall(Tokens.RxFrame.GetMethod("IncorpCut"),
                        CpsOp.RxFrame(), strs, CpsOp.IntLiteral(
                            JScalar.I(z[2]) * RxFrame.IC_ZERO_WIDTH +
                            JScalar.I(z[3]) * RxFrame.IC_NEGATIVE),
                        th.Scan(z[4])));
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
            handlers["rxlprim"] = delegate(NamProcessor th, object[] z) {
                CpsOp[] args = new CpsOp[z.Length - 1];
                for(int i = 0; i < z.Length - 2; i++)
                    args[i+1] = th.Scan(z[i+2]);
                args[0] = CpsOp.RxFrame();
                return CpsOp.CpsCall(Tokens.Variable,
                        Tokens.RxFrame.GetMethod(FixStr(z[1])), args);
            };
            handlers["const"] = delegate(NamProcessor th, object[] z) {
                object[] ch = z[1] as object[];
                if (ch == null) {
                    return th.cpb.eu.RefConstant("", "", z[1], null);
                }
                string chh = JScalar.S(ch[0]);
                if (chh == "exactnum") {
                    return th.cpb.eu.VarConstExact(JScalar.I(ch[1]),
                        JScalar.S(ch[2]));
                } else if (chh == "box" && !(ch[1] is object[])) {
                    string typ = JScalar.S(ch[1]);
                    object[] chch = ch[2] as object[];
                    string chchh = JScalar.S(chch[0]);
                    if (typ == "Str" && chchh == "str") {
                        return th.cpb.eu.VarConstStr(JScalar.S(chch[1]));
                    } else if (typ == "Num" && chchh == "double") {
                        return th.cpb.eu.VarConstNum(JScalar.N(chch[1]));
                    } else {
                        Console.WriteLine("odd constant box {0}/{1}", typ, chchh);
                    }
                } else if (chh == "newcc") {
                    return th.cpb.eu.CCConst(JScalar.IA(1, ch));
                } else if (chh == "fcclist_new") {
                    int[][] ccl = new int[ch.Length - 1][];
                    for (int i = 1; i < ch.Length; i++)
                        ccl[i-1] = JScalar.IA(1, ch[i]);
                    return th.cpb.eu.CCListConst(ccl);
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
                LexInfo l;
                if (outer == 0 && th.sub.dylex.TryGetValue(name, out l))
                    return CpsOp.LexAccess(l, 0, new CpsOp[0]);
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
                        CpsOp.BoolLiteral(FixBool(z[3])))); };
            handlers["rxpushb"] = delegate(NamProcessor th, object[] z) {
                return CpsOp.MethodCall(Tokens.RxFrame_PushBacktrack,
                    CpsOp.RxFrame(),
                    CpsOp.LabelId(th.cpb.cx, JScalar.S(z[2]))); };
            handlers["ltm_push_alts"] = delegate(NamProcessor th, object[] z) {
                LAD[] prefixes = JScalar.A<LAD>(0, z[1],
                        DowncallReceiver.BuildLadJ);
                AltInfo ai = new AltInfo(prefixes, JScalar.S(z[2]), null);
                th.altinfo_fixups.Add(new KeyValuePair<AltInfo,string[]>(
                        ai, JScalar.SA(0, z[3])));
                CpsOp aic = th.cpb.eu.RefConstant(ai.dba, "", ai, null);
                return CpsOp.MethodCall(Tokens.RxFrame.GetMethod("LTMPushAlts"),
                    CpsOp.RxFrame(), CpsOp.CallFrame(), aic); };
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
                // Horrible, horrible hack.  We need to redirect references
                // to CompilerBlob while compiling the compiler because
                // otherwise they will be resolved to the compiler compiler's
                // copy.
                if (Backend.prefix == "Run.")
                    name = name.Replace("CompilerBlob", "Run.CompilerBlob");
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
            thandlers["var_new_tied"] = Constructy(typeof(TiedVariable).GetConstructor(new Type[] { Tokens.P6any, Tokens.P6any, Tokens.P6any }));
            thandlers["obj_typename"] = Methody(null, Tokens.P6any.GetMethod("GetTypeName"));
            thandlers["fetch"] = Methody(null, Tokens.Variable_Fetch);
            thandlers["default_new"] = delegate(CpsOp[] z) {
                return CpsOp.Sequence(
                    CpsOp.Label("!again", false),
                    CpsOp.CpsCall(Tokens.Void, Tokens.Kernel.GetMethod("DefaultNew"), z),
                    CpsOp.Goto("!again", false),
                    CpsOp.Null(Tokens.Variable));
            };
            thandlers["assign"] = Methody(null, Tokens.Kernel_Assign);
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
            thandlers["treader_close"] = Methody(null, typeof(TextReader).GetMethod("Close"));
            ConstructorInfo treader_open = typeof(StreamReader).GetConstructor(new Type[1] { Tokens.String });
            thandlers["treader_open"] = delegate(CpsOp[] z) {
                return CpsOp.Widen(typeof(TextReader),
                    CpsOp.ConstructorCall(treader_open, z)); };

            ConstructorInfo twriter_open = typeof(StreamWriter).GetConstructor(new Type[1] { Tokens.String });
            thandlers["twriter_open"] = delegate(CpsOp[] z) {
                return CpsOp.Widen(typeof(TextWriter),
                    CpsOp.ConstructorCall(twriter_open, z)); };

            ConstructorInfo twriter_append = typeof(StreamWriter).GetConstructor(new Type[2] { Tokens.String, Tokens.Boolean });
            thandlers["twriter_append"] = delegate(CpsOp[] z) {
                return CpsOp.Widen(typeof(TextWriter),
                    CpsOp.ConstructorCall(twriter_append, z)); };

            thandlers["twriter_puts"] = Methody(null, typeof(TextWriter).GetMethod("Write", new Type[] { Tokens.String }));
            thandlers["twriter_close"] = Methody(null, typeof(TextWriter).GetMethod("Close"));

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

        public void MakeBody(object b) {
            cpb.ReserveLex(sub.num_lex_slots);
            cpb.Build(Scan(WrapBody(b)));
        }

        public void FillSubInfo(Type ty) {
            MethodInfo m = ty.GetMethod(cpb.mb.Name);

            sub.lines = cpb.cx.lineBuffer.ToArray();
            sub.edata = cpb.cx.ehspanBuffer.ToArray();
            sub.label_names = cpb.cx.ehlabelBuffer.ToArray();
            sub.nspill = cpb.Spills();

            foreach (var kv in altinfo_fixups) {
                kv.Key.labels = new int[kv.Value.Length];
                for (int i = 0; i < kv.Value.Length; i++) {
                    kv.Key.labels[i] =
                        cpb.cx.named_cases[kv.Value[i]];
                }
            }

            sub.code = (DynBlockDelegate) Delegate.CreateDelegate(
                    Tokens.DynBlockDelegate, m);
            if (sub.protopad != null) {
                sub.protopad.code = sub.code;
                sub.protopad.EnsureSpills(sub.nspill);
            }
        }

        object j(string s) { return s; }
        object[] a(params object[] ax) { return ax; }
        void EnterCode(List<object> frags) {
            List<object> latefrags = new List<object>();

            // Lexpad setup: XXX should be done *before* entry, indeed
            // before the binder, so defaults work right

            foreach (KeyValuePair<string,LexInfo> kv in sub.dylex) {
                if ((sub.special & SubInfo.RUN_ONCE) != 0)
                    continue; // we'll just use the static pad

                if (kv.Value is LISub) {
                    LISub ls = (LISub) kv.Value;
                    frags.Add(a(j("scopedlex"), j(kv.Key),
                        a(j("newscalar"), a(j("_makesub"), ls.def))));
                } else if (kv.Value is LISimple) {
                    LISimple ls = kv.Value as LISimple;
                    int f = ls.flags;
                    if ((f & LISimple.NOINIT) != 0) continue;

                    object bit;
                    CpsOp tc = EmitUnit.Current.TypeConstant(ls.type);
                    if ((f & LISimple.ROINIT) != 0) {
                        bit = a(j("class_ref"), j("typeVar"), Kernel.AnyMO);
                    } else if ((f & LISimple.DEFOUTER) != 0) {
                        bit = a(j("outerlex"), j(kv.Key));
                    } else if ((f & (LISimple.HASH | LISimple.LIST)) != 0) {
                        bit = a(j( ((f & LISimple.HASH) != 0) ?
                            "newhash" : "newarray" ));
                    } else {
                        bit = a(j("_newoftype"), tc);
                    }
                    frags.Add(a(j("scopedlex"), j(kv.Key), bit));
                } else if (kv.Value is LILabel) {
                    frags.Add(a(j("scopedlex"), j(kv.Key),
                        a(j("_newlabel"), j(kv.Key))));
                } else if (kv.Value is LIDispatch) {
                    latefrags.Add(a(j("scopedlex"), j(kv.Key),
                        a(j("_newdispatch"), j(kv.Key))));
                }
            }
            foreach (object lf in latefrags) frags.Add(lf);

            if (sub.sig != null) frags.Add(a(j("_binder")));

            // PRE
            foreach (SubInfo z in sub.GetPhasers(Kernel.PHASER_PRE)) {
                frags.Add(a(j("ternary"),
                    a(j("obj_getbool"), a(j("subcall"), j(""),
                            a(j("_makesub"), z))),
                    a(j("prog")),
                    a(j("sink"),a(j("die"),
                            j("Precondition failed in " + sub.name)))));
            }

            foreach (SubInfo z in sub.GetPhasers(Kernel.PHASER_POST)) {
                frags.Add(a(j("_pushleave"),
                    (sub.phaser == Kernel.PHASER_PRE ?
                        a(j("frame_outer"), a(j("callframe"))) :
                        a(j("callframe"))),
                    a(j("int"), j(LeaveHook.POST.ToString())),
                    a(j("_makesub"), z)));
            }

            // includes UNDO and LEAVE
            foreach (SubInfo z in sub.GetPhasers(Kernel.PHASER_KEEP)) {
                int type = z.phaser == Kernel.PHASER_KEEP ? LeaveHook.KEEP :
                    z.phaser == Kernel.PHASER_UNDO ? LeaveHook.UNDO :
                    LeaveHook.UNDO + LeaveHook.KEEP;
                frags.Add(a(j("_pushleave"), a(j("callframe")),
                    a(j("int"), j(type.ToString())),
                    a(j("_makesub"), z)));
            }

            foreach (SubInfo z in sub.GetPhasers(Kernel.PHASER_ENTER)) {
                frags.Add(a(j("sink"), a(j("subcall"), j(""),
                                a(j("_makesub"), z))));
            }
        }

        object WrapBody(object b) {
            List<object> enter = new List<object>();
            EnterCode(enter);

            List<object> handlers = new List<object>();
            handlers.Add("xspan");
            handlers.Add("tstart");
            handlers.Add("tend");
            handlers.Add("0");
            handlers.Add(b);

            // TODO: bind a ro container around return values
            if (sub.IsTopicalizer()) {
                handlers.Add("6");
                handlers.Add("");
                handlers.Add("tend");
            }
            foreach (KeyValuePair<string,LexInfo> kv in sub.dylex) {
                if (!(kv.Value is LILabel))
                    continue;
                handlers.Add("8");
                handlers.Add(kv.Key);
                handlers.Add("goto_" + kv.Key);
            }
            if (sub.mo.HasType(Kernel.RoutineMO) &&
                    (sub.special & SubInfo.RETURN_PASS) == 0) {
                handlers.Add("4");
                handlers.Add("");
                handlers.Add("tend");
            }
            if (handlers.Count != 5)
                b = handlers.ToArray();
            enter.Insert(0, "prog");
            enter.Add(new object[] { "return", b });
            b = enter.ToArray();

            return b;
        }

        CpsOp Scan(object node) {
            object[] rnode = (object[]) node;
            string tag = JScalar.S(rnode[0]);
            Func<NamProcessor, object[], CpsOp> handler;
            if (!handlers.TryGetValue(tag, out handler)) {
                MethodInfo mi = Tokens.Builtins.GetMethod(tag);
                if (mi == null)
                    throw new Exception("Unhandled nam operator " + tag);
                handlers[tag] = handler = (mi.ReturnType == Tokens.Frame &&
                        mi.GetParameters().Length >= 1 &&
                        mi.GetParameters()[0].ParameterType == Tokens.Frame)
                    ? MakeTotalHandler(Methody(Tokens.Variable, mi))
                    : MakeTotalHandler(Methody(null, mi));
            }
            if (Config.CGVerbose > 1)
                Console.WriteLine("enter " + tag);
            CpsOp r = handler(this, rnode);
            if (Config.CGVerbose > 1)
                Console.WriteLine("exit " + tag + " " + r.head.Returns);
            return r;
        }
    }

    public class Backend {
        [TrueGlobal]
        public static string obj_dir = AppDomain.CurrentDomain.BaseDirectory;
        [TrueGlobal]
        public static string prefix = (typeof(Backend).Assembly.GetName().Name == "Kernel") ? "" : "Run.";
        [TrueGlobal]
        public static bool cross_level_load;

        public static string LocStr(string fo, int lo, string fn, int ln) {
            return fn == fo ? " (see line " + lo + ")" :
                " (see " + fo + " line " + lo + ")";
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
        public static Handle Wrap(object h) {
            return h == null ? null : new Handle(h);
        }

        public override string ToString() {
            return string.Format("{0}[{1:X}]", to.ToString(), to.GetHashCode());
        }
    }

    public class DowncallReceiver : CallReceiver {
        public override object this[object i] {
            set { }
            get {
                try {
                    return Call((object[]) i);
                } catch (Exception ex) {
                    return new Exception(ex.ToString());
                }
            }
        }
        static readonly bool TraceDown = Environment.GetEnvironmentVariable("NIECZA_TRACE_DOWNCALLS") != null;

        static object AddLexical(object[] args, LexInfo li) {
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
            li.owner.AddLexical(li.name, li);

            if (li.name.Length >= 1 && li.name[0] == '&')
                return new object[] { "sub", li.name };

            return new object[] { "" };
        }

        static STable ResolveSubClass(string cls) {
            // this happens before lexicals are created, so we can't
            // use lexicals.
            STable rcls = (cls == "Sub") ? Kernel.SubMO :
                (cls == "Routine") ? Kernel.RoutineMO :
                (cls == "Regex") ? Kernel.RegexMO :
                (cls == "Method") ? Kernel.MethodMO :
                (cls == "Submethod") ? Kernel.SubmethodMO :
                (cls == "WhateverCode") ? Kernel.WhateverCodeMO :
                (cls == "Block") ? Kernel.BlockMO :
                (cls == "Code") ? Kernel.CodeMO : null;

            return rcls;
        }

        // XXX delete me after killing JScalar
        internal static LAD BuildLadJ(object treex) {
            object[] tree = (object[]) treex;
            string key = JScalar.S(tree[0]);

            if (key == "CC") {
                int[] nar = JScalar.IA(1, tree);
                return new LADCC(new CC(nar));
            } else if (key == "Imp") {
                return new LADImp();
            } else if (key == "Dot") {
                return new LADDot();
            } else if (key == "None") {
                return new LADNone();
            } else if (key == "Null") {
                return new LADNull();
            } else if (key == "Dispatcher") {
                return new LADDispatcher();
            } else if (key == "Str") {
                return new LADStr(JScalar.S(tree[1]));
            } else if (key == "StrNoCase") {
                return new LADStrNoCase(JScalar.S(tree[1]));
            } else if (key == "Param") {
                return new LADParam(JScalar.S(tree[1]));
            } else if (key == "Method") {
                return new LADMethod(JScalar.S(tree[1]));
            } else if (key == "Quant") {
                return new LADQuant(JScalar.I(tree[1]), BuildLadJ(tree[2]),
                        tree.Length > 3 ? BuildLadJ(tree[3]) : null);
            } else if (key == "Sequence" || key == "Any") {
                object[] za = (object[])tree[1];
                LAD[] z = new LAD[za.Length];
                for (int i = 0; i < za.Length; i++)
                    z[i] = BuildLadJ(za[i]);
                return (key == "Any") ? (LAD)new LADAny(z) : new LADSequence(z);
            } else {
                throw new Exception("odd lad key " + key);
            }
        }

        static LAD BuildLad(object[] tree) {
            string key = (string) tree[0];

            if (key == "CC") {
                int[] nar = new int[tree.Length-1];
                Array.Copy(tree, 1, nar, 0, nar.Length);
                return new LADCC(new CC(nar));
            } else if (key == "Imp") {
                return new LADImp();
            } else if (key == "Dot") {
                return new LADDot();
            } else if (key == "None") {
                return new LADNone();
            } else if (key == "Null") {
                return new LADNull();
            } else if (key == "Dispatcher") {
                return new LADDispatcher();
            } else if (key == "Str") {
                return new LADStr((string)tree[1]);
            } else if (key == "StrNoCase") {
                return new LADStrNoCase((string)tree[1]);
            } else if (key == "Param") {
                return new LADParam((string)tree[1]);
            } else if (key == "Method") {
                return new LADMethod((string)tree[1]);
            } else if (key == "Quant") {
                return new LADQuant((int)tree[1], BuildLad((object[])tree[2]),
                        tree.Length > 3 ? BuildLad((object[])tree[3]) : null);
            } else if (key == "Sequence" || key == "Any") {
                object[] za = (object[])tree[1];
                LAD[] z = new LAD[za.Length];
                for (int i = 0; i < za.Length; i++)
                    z[i] = BuildLad((object[])za[i]);
                return (key == "Any") ? (LAD)new LADAny(z) : new LADSequence(z);
            } else {
                throw new Exception("odd lad key " + key);
            }
        }

        static void ShowCall(object[] args) {
            StringBuilder sb = new StringBuilder();
            foreach(object a in args) {
                char ch = (a is int) ? 'i' : (a is Handle) ? 'h' :
                    (a is double) ? 'd' :
                    (a is string) ? 's' : (a is bool) ? 'b' :
                    (a == null) ? 'n' : 'X';
                sb.AppendFormat("{0}:{1}; ", ch, a);
            }
            Console.WriteLine(sb.ToString());
        }

        Dictionary<string, Func<object[],object>> methods;

        public DowncallReceiver() {
            methods = new Dictionary<string, Func<object[],object>>();
            foreach (MethodInfo mi in typeof(DowncallReceiver).GetMethods()) {
                if (mi.ReturnType != typeof(object))
                    continue;
                var pis = mi.GetParameters();
                if (pis.Length != 1 || pis[0].ParameterType != typeof(object[]))
                    continue;
                methods[mi.Name] = (Func<object[],object>) Delegate.CreateDelegate(typeof(Func<object[],object>), mi);
            }
        }

        object Call(object[] args) {
            if (TraceDown) ShowCall(args);
            string cmd = (string) args[0];
            Func<object[],object> fn;
            if (methods.TryGetValue(cmd, out fn))
                return fn(args);
            else {
                ShowCall(args);
                return new Exception("No handler for downcall " + cmd);
            }
        }
        public static object gettype(object[] args) {
            object o = Handle.Unbox(args[1]);
            return (o is SubInfo) ? "sub" : (o is RuntimeUnit) ? "unit" :
                (o is STable) ? "type" : (o is Frame) ? "frame" :
                (o is Variable) ? "value" : (o is Parameter) ? "param" :
                "unknown";
        }
        public static object set_binding(object[] args) {
            if (Environment.GetEnvironmentVariable("NIECZA_DEFER_TRACE") != null) {
                Kernel.TraceFlags = Kernel.TRACE_CUR;
                Kernel.TraceCount = Kernel.TraceFreq = 1;
            }
            Kernel.SetTrace();
            Backend.obj_dir = (string)args[1];
            Builtins.upcall_receiver = (System.Collections.IDictionary)args[2];
            return null;
        }
        public static object push_compartment(object[] args) {
            Compartment.Push();
            return null;
        }
        public static object pop_compartment(object[] args) {
            Compartment.Pop();
            return null;
        }
        public static object new_unit(object[] args) {
            RuntimeUnit ru = new RuntimeUnit((string)args[1],
                    (string)args[2], (string)args[3],
                    (bool)args[4], (bool)args[5]);

            if (Kernel.containerRootUnit == null) {
                // this is a module unit
                Kernel.InitCompartment();
                Kernel.containerRootUnit = ru;
                ru.owner = ru;
                ru.globals = Kernel.currentGlobals =
                    new Dictionary<string,StashEnt>();
            } else {
                // needs to use the same globals as the other units in
                // this serialization unit
                ru.globals = Kernel.currentGlobals;
                ru.owner = Kernel.containerRootUnit;
                ru.owner.subordinates.Add(ru);
            }
            return new Handle(ru);
        }
        public static object unit_need_unit(object[] args) {
            // LinkUnit state is owned by the root
            RuntimeUnit ru = (RuntimeUnit)Handle.Unbox(args[1]);
            string oname   = (string)args[2];

            RuntimeUnit tg;
            try {
                tg = (RuntimeUnit) RuntimeUnit.reg.LoadUnit(oname).root;
            } catch (Exception ex) {
                if (Config.SerFailInfo)
                    Console.WriteLine("Thaw {0} failed: >>>{1}<<<", oname, ex);
                // assume stale at first
                object r1 = Builtins.UpCall(new object[] {
                    "compile_unit", oname });
                if (r1 != null)
                    return r1;
                tg = (RuntimeUnit) RuntimeUnit.reg.LoadUnit(oname).root;
            }
            string err = ru.owner.LinkUnit(tg);
            return err == null ? (object)new Handle(tg) : new Exception(err);
        }
        public static object unit_anon_stash(object[] args) {
            RuntimeUnit ru = (RuntimeUnit)Handle.Unbox(args[1]);
            return ru.name + ":" + (ru.nextid++);
        }
        public static object unit_set_bottom(object[] args) {
            ((RuntimeUnit)Handle.Unbox(args[1])).bottom =
                (SubInfo)Handle.Unbox(args[2]);
            return null;
        }
        public static object unit_bottom(object[] args) {
            return Handle.Wrap(((RuntimeUnit)Handle.Unbox(args[1])).bottom);
        }
        public static object unit_mainline(object[] args) {
            return Handle.Wrap(((RuntimeUnit)Handle.Unbox(args[1])).mainline);
        }
        public static object unit_set_mainline(object[] args) {
            RuntimeUnit ru = (RuntimeUnit)Handle.Unbox(args[1]);
            ru.mainline = (SubInfo)Handle.Unbox(args[2]);
            ru.mainline.special |= SubInfo.MAINLINE;
            return null;
        }
        public static object sub_set_ltm(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).ltm =
                BuildLad((object[])args[2]);
            return null;
        }
        public static object sub_create_static_pad(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).CreateProtopad(null);
            return null;
        }
        public static object sub_noninlinable(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).special |=
                SubInfo.CANNOT_INLINE;
            return null;
        }
        public static object sub_set_return_pass(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).special |=
                SubInfo.RETURN_PASS;
            return null;
        }
        public static object sub_set_transparent(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).special |=
                SubInfo.TRANSPARENT;
            return null;
        }
        public static object sub_set_run_once(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            if ((s.outer.special & SubInfo.RUN_ONCE) != 0) {
                s.CreateProtopad(null);
                s.special |= SubInfo.RUN_ONCE;
            }
            return null;
        }
        public static object sub_set_unsafe(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).special |=
                SubInfo.UNSAFE;
            return null;
        }
        public static object sub_arity(object[] args) {
            return Builtins.get_arity((SubInfo)Handle.Unbox(args[1]));
        }
        public static object sub_is_inlinable(object[] args) {
            return ((SubInfo)Handle.Unbox(args[1])).IsInlinable();
        }
        public static object sub_topicalizer(object[] args) {
            return ((SubInfo)Handle.Unbox(args[1])).IsTopicalizer();
        }
        public static object sub_count(object[] args) {
            return Builtins.get_count((SubInfo)Handle.Unbox(args[1]));
        }
        public static object sub_set_inlined(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).SetInlined();
            return null;
        }
        public static object value_get_type(object[] args) {
            return Handle.Wrap(((Variable)Handle.Unbox(args[1])).Fetch().mo);
        }
        public static object value_to_string(object[] args) {
            return Builtins.ToStr((Variable)Handle.Unbox(args[1]));
        }
        public static object value_enum_type(object[] args) {
            var v = (Variable)Handle.Unbox(args[1]);
            return Builtins.ToStr(Builtins.InvokeMethod("data-type", v));
        }
        public static object value_enum_keys(object[] args) {
            var v = (Variable)Handle.Unbox(args[1]);
            return Builtins.UnboxLoS(Builtins.InvokeMethod("keys", v));
        }
        public static object value_starts_with_pair(object[] args) {
            var ob = ((Variable)Handle.Unbox(args[1])).Fetch();
            if (ob.Isa(Kernel.PairMO))
                return true;
            if (ob.Isa(Kernel.ParcelMO) && Kernel.UnboxAny<Variable[]>(ob)[0].Fetch().Isa(Kernel.PairMO))
                return true;
            return false;
        }
        public static object unit_constant_fold(object[] args) {
            var callee = (SubInfo)Handle.Unbox(args[2]);
            var pos = new List<Variable>();
            var nam = new VarHash();

            for (int ix = 3; ix < args.Length; ix += 2) {
                var v = (Variable)Handle.Unbox(args[ix+1]);
                if (args[ix] == null) {
                    pos.Add(v);
                } else {
                    nam[(string)args[ix]] = v;
                }
            }

            object r = null;
            try {
                r = Handle.Wrap(Kernel.RunInferior(callee.protosub.Invoke(
                    Kernel.GetInferiorRoot(), pos.ToArray(), nam)));
            } catch (Exception) { }
            return r;
        }
        public static object unit_string_constant(object[] args) {
            return Handle.Wrap(Builtins.MakeStr((string)args[2]));
        }
        public static object unit_numeric_constant(object[] args) {
            if (args.Length == 4) {
                int bas       = (int)args[2];
                string digits = (string)args[3];
                return Handle.Wrap(EmitUnit.ExactNum(bas, digits));
            } else {
                double d = (args[2] is double) ? (double)args[2] : (int)args[2];
                return Handle.Wrap(Builtins.MakeFloat(d));
            }
        }
        public static object sub_run_BEGIN_raw(object[] args) {
            SubInfo  si = (SubInfo)Handle.Unbox(args[1]);
            return new Handle(si.RunBEGIN());
        }
        public static object sub_run_BEGIN_CC(object[] args) {
            SubInfo  si = (SubInfo)Handle.Unbox(args[1]);
            Variable v  = si.RunBEGIN();
            // no really this is a horrible hack... we need a way for
            // the compiler to directly manipulate MOP values
            return Niecza.UCD.DataSet.CompileCClass(v);
        }
        public static object sub_run_BEGIN(object[] args) {
            SubInfo  si = (SubInfo)Handle.Unbox(args[1]);
            Variable v  = si.RunBEGIN();
            string   cn = (string)args[2];
            while (si != null && !si.dylex.ContainsKey(cn)) si = si.outer;
            LexInfo li = si == null ? null : si.dylex[cn];
            if (li is LIConstant) {
                ((LIConstant)li).value = v;
            } else if (li is LICommon) {
                StashEnt hkey = Kernel.currentGlobals[((LICommon)li).hkey];
                hkey.constant = true;
                hkey.v = v;
            } else {
                return new Exception("cannot bind constant value");
            }
            return null;
        }
        public static object sub_get_unit(object[] args) {
            return new Handle(((SubInfo)Handle.Unbox(args[1])).unit);
        }
        public static object sub_run_once(object[] args) {
            return (((SubInfo)Handle.Unbox(args[1])).special &
                    SubInfo.RUN_ONCE) != 0;
        }
        public static object sub_transparent(object[] args) {
            return (((SubInfo)Handle.Unbox(args[1])).special &
                    SubInfo.TRANSPARENT) != 0;
        }
        public static object sub_outervar(object[] args) {
            return ((SubInfo)Handle.Unbox(args[1])).outervar;
        }
        public static object sub_name(object[] args) {
            return ((SubInfo)Handle.Unbox(args[1])).name;
        }
        public static object sub_methodof(object[] args) {
            return Handle.Wrap(((SubInfo)Handle.Unbox(args[1])).methodof);
        }
        public static object sub_outer(object[] args) {
            return Handle.Wrap(((SubInfo)Handle.Unbox(args[1])).outer);
        }
        public static object sub_class(object[] args) {
            return ((SubInfo)Handle.Unbox(args[1])).mo.name;
        }
        public static object sub_body_of(object[] args) {
            return Handle.Wrap(((SubInfo)Handle.Unbox(args[1])).body_of);
        }
        public static object sub_in_class(object[] args) {
            return Handle.Wrap(((SubInfo)Handle.Unbox(args[1])).in_class);
        }
        public static object sub_cur_pkg(object[] args) {
            return Handle.Wrap(((SubInfo)Handle.Unbox(args[1])).cur_pkg);
        }
        public static object sub_set_methodof(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).methodof =
                (STable)Handle.Unbox(args[2]);
            return null;
        }
        public static object sub_set_body_of(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).body_of =
                (STable)Handle.Unbox(args[2]);
            return null;
        }
        public static object sub_set_cur_pkg(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).cur_pkg =
                (STable)Handle.Unbox(args[2]);
            return null;
        }
        public static object sub_set_in_class(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).in_class =
                (STable)Handle.Unbox(args[2]);
            return null;
        }
        public static object sub_set_outervar(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).outervar = (string)args[2];
            return null;
        }
        public static object sub_set_name(object[] args) {
            ((SubInfo)Handle.Unbox(args[1])).name = (string)args[2];
            return null;
        }
        public static object sub_set_class(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            STable  c = ResolveSubClass((string)args[2]);
            s.mo = c;
            if (s.protosub != null)
                s.protosub.mo = c;
            return null;
        }
        public static object sub_delete_lex(object[] args) {
            // XXX This leaves a gap in the lexical/number mapping.
            // Don't use it too much.
            SubInfo from = (SubInfo)Handle.Unbox(args[1]);
            string  lkey = (string)args[2];
            from.dylex.Remove(lkey);
            return null;
        }
        public static object sub_lookup_lex(object[] args) {
            SubInfo from = (SubInfo)Handle.Unbox(args[1]);
            string  lkey = (string)args[2];
            string  file = (string)args[3];
            int     line = (int)args[4];

            SubInfo csr;
            int levels = 0;
            for (csr = from; csr != null; csr = csr.outer, levels++)
                if (csr.dylex.ContainsKey(lkey))
                    break;

            if (csr == null)
                return new object[0];
            LexInfo li = csr.dylex[lkey];

            if (file != null) {
                for (SubInfo csr2 = from;
                        csr2.used_in_scope != null && // modify only open units
                        !csr2.used_in_scope.ContainsKey(lkey);
                        csr2 = csr2.outer, levels--) {

                    var uisi = new SubInfo.UsedInScopeInfo();
                    uisi.orig_file = li.file;
                    uisi.orig_line = li.line;
                    uisi.file = file;
                    uisi.line = line;
                    uisi.levels = levels;
                    csr2.used_in_scope[lkey] = uisi;
                    if (csr == csr2)
                        break; // stop *after* reaching defined scope
                }
            }


            object[] r = null;
            var lalias = li as LIAlias;
            if (lalias != null)
                r = new object[] { "alias",null,null,null, lalias.to };
            var lattralias = li as LIAttrAlias;
            if (lattralias != null)
                r = new object[] { "attralias",null,null,null, new Handle(lattralias.atype), lattralias.aname };
            var lsub   = li as LISub;
            if (lsub != null)
                r = new object[] { "sub",null,null,null, new Handle(lsub.def) };
            var lpkg   = li as LIPackage;
            if (lpkg != null)
                r = new object[] { "package",null,null,null, new Handle(lpkg.pkg) };
            var lsimp  = li as LISimple;
            if (lsimp != null)
                r = new object[] { "simple",null,null,null, lsimp.flags, Handle.Wrap(lsimp.type) };
            var ldisp  = li as LIDispatch;
            if (ldisp != null)
                r = new object[] { "dispatch",null,null,null, Handle.Wrap(csr) };
            var llab   = li as LILabel;
            if (llab != null)
                r = new object[] { "label",null,null,null };
            var lhint  = li as LIConstant;
            if (lhint != null)
                r = new object[] { "hint",null,null,null };
            var lcomm  = li as LICommon;
            if (lcomm != null)
                r = new object[] { "common",null,null,null, lcomm.Stash(), lcomm.VarName() };

            r[1] = li.file;
            r[2] = li.line;
            r[3] = li.pos;

            return r;
        }
        public static object lex_names(object[] args) {
            List<object> ret = new List<object>();
            foreach (string k in ((SubInfo)Handle.Unbox(args[1])).dylex.Keys)
                ret.Add(k);
            return ret.ToArray();
        }
        public static object unused_lexicals(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            List<object> ret = new List<object>();
            foreach (KeyValuePair<string,LexInfo> kv in s.dylex) {
                if (s.used_in_scope.ContainsKey(kv.Key))
                    continue;
                ret.Add(kv.Key);
                ret.Add(kv.Value.pos);
            }
            return ret.ToArray();
        }
        public static object unit_stubbed_stashes(object[] args) {
            RuntimeUnit u = (RuntimeUnit)Handle.Unbox(args[1]);
            List<object> ret = new List<object>();
            foreach (KeyValuePair<int,STable> kv in u.stubbed_stashes) {
                ret.Add(kv.Key);
                ret.Add(new Handle(kv.Value));
            }
            return ret.ToArray();
        }
        public static object unit_stub_stash(object[] args) {
            RuntimeUnit ru = (RuntimeUnit)Handle.Unbox(args[1]);
            int    pos = (int)args[2];
            STable type = (STable)Handle.Unbox(args[3]);
            ru.stubbed_stashes.Add(new KeyValuePair<int,STable>(pos,type));
            return null;
        }
        public static object unit_name(object[] args) {
            return ((RuntimeUnit)Handle.Unbox(args[1])).name;
        }
        public static object sub_to_unit(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            while ((s.special & SubInfo.MAINLINE) == 0)
                s = s.outer;
            return new Handle(s);
        }
        public static object equal_handles(object[] args) {
            return Handle.Unbox(args[1]) == Handle.Unbox(args[2]);
        }
        public static object sub_is_routine(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            return s.mo.HasType(Kernel.RoutineMO);
        }
        public static object sub_is_regex(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            return s.mo.HasType(Kernel.RegexMO);
        }
        public static object sub_has_lexical(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            return s.dylex.ContainsKey((string)args[2]);
        }
        public static object sub_lexical_used(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            return s.used_in_scope.ContainsKey((string)args[2]);
        }
        public static object sub_parameterize_topic(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            LISimple li = (LISimple)s.dylex["$_"];
            li.flags = LISimple.NOINIT;
            return null;
        }
        public static object unit_rel_pkg(object[] args) {
            RuntimeUnit c = (RuntimeUnit)Handle.Unbox(args[1]);
            bool auto     = (bool)args[2];
            STable pkg    = args[3] == null ? null :
                (STable)Handle.Unbox(args[3]);

            for (int i = 4; i < args.Length; i++) {
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
                    v.constant = true;
                    v.v = StashCursor.MakePackage((who + "::" + key).Substring(2), Kernel.BoxRaw<string>(who + "::" + key, Kernel.StashMO));
                    pkg = v.v.Fetch().mo;
                }
            }
            return new Handle(pkg);
        }
        public static object unit_list_stash(object[] args) {
            RuntimeUnit c = (RuntimeUnit) Handle.Unbox(args[1]);
            string who = (string)args[2];
            var r = new List<object>();
            string filter = ((char)who.Length) + who;

            foreach (KeyValuePair<string,StashEnt> kv in c.globals) {
                if (!Utils.StartsWithInvariant(filter, kv.Key))
                    continue;
                r.Add(kv.Key.Substring(filter.Length));
                StashEnt b = kv.Value;

                if (!b.v.rw && !b.v.Fetch().IsDefined()) {
                    r.Add(new Handle(b.v.Fetch().mo));
                } else if (!b.v.rw && b.v.Fetch().Isa(Kernel.CodeMO)) {
                    r.Add(new Handle(b.v.Fetch().GetSlot(Kernel.CodeMO, "$!info")));
                } else {
                    r.Add(null);
                }
            }
            return r.ToArray();
        }
        public static object unit_get(object[] args) {
            RuntimeUnit ru = (RuntimeUnit)Handle.Unbox(args[1]);
            string who  = (string)args[2];
            string key  = (string)args[3];
            string hkey = (char)who.Length + who + key;
            StashEnt b;
            if (ru.globals.TryGetValue(hkey, out b)) {
                if (!b.v.rw && !b.v.Fetch().IsDefined()) {
                    return new Handle(b.v.Fetch().mo);
                } else if (!b.v.rw && b.v.Fetch().Isa(Kernel.CodeMO)) {
                    return new Handle(b.v.Fetch().GetSlot(Kernel.CodeMO, "$!info"));
                } else return null;
            } else {
                return null;
            }
        }
        public static object unit_exists(object[] args) {
            RuntimeUnit ru = (RuntimeUnit)Handle.Unbox(args[1]);
            string who  = (string)args[2];
            string key  = (string)args[3];
            string hkey = (char)who.Length + who + key;
            return ru.globals.ContainsKey(hkey);
        }
        public static object unit_bind(object[] args) {
            RuntimeUnit ru = (RuntimeUnit)Handle.Unbox(args[1]);
            string who  = (string)args[2];
            string name = (string)args[3];
            object item = Handle.Unbox(args[4]);
            string file = (string)args[5];
            int    line = (int)args[6];

            Variable vitm = null;
            if (item is STable)
                vitm = ((STable)item).typeVar;
            else if (item is SubInfo)
                vitm = Kernel.NewROScalar(((SubInfo)item).protosub);
            else if (item == null)
                vitm = null;
            else
                return new Exception("weird thing to bind");

            string err = ru.NsBind(who, name, vitm, file, line);
            return err == null ? null : new Exception(err);
        }
        public static object type_CAN(object[] args) {
            STable st = (STable)Handle.Unbox(args[1]);
            //string meth = (string)args[2];
            return (st.mo.type != P6how.PACKAGE && st.mo.type != P6how.MODULE);
        }
        public static object type_add_super(object[] args) {
            STable st = (STable)Handle.Unbox(args[1]);
            STable su = (STable)Handle.Unbox(args[2]);
            st.mo.superclasses.Add(Kernel.ToInheritable(su));
            return null;
        }
        public static object type_add_role(object[] args) {
            STable st = (STable)Handle.Unbox(args[1]);
            STable su = (STable)Handle.Unbox(args[2]);
            st.mo.local_roles.Add(su);
            return null;
        }
        public static object type_add_attribute(object[] args) {
            STable  add_to = (STable)Handle.Unbox(args[1]);
            string  name   = (string)args[2];
            string  sigil  = (string)args[3];
            bool    access = (bool)args[4];
            STable  type   = (STable)Handle.Unbox(args[5]);
            string  file   = (string)args[6];
            int     line   = (int)args[7];

            foreach (P6how.AttrInfo ai in add_to.mo.local_attr)
                if (ai.name == name)
                    return new Exception("Two definitions of attribute " + name + Backend.LocStr(ai.file, ai.line, file, line));

            int flags = (sigil == "@") ? P6how.A_ARRAY :
                (sigil == "%") ? P6how.A_HASH : 0;
            if (access) flags |= P6how.A_PUBLIC;

            add_to.mo.AddAttributePos(name, flags, null, type, file, line);
            return null;
        }
        public static object type_add_initializer(object[] args) {
            STable  add_to = (STable)Handle.Unbox(args[1]);
            string  name   = (string)args[2];
            SubInfo init   = (SubInfo)Handle.Unbox(args[3]);
            for (int i = 0; i < add_to.mo.local_attr.Count; i++) {
                var ai = add_to.mo.local_attr[i];
                if (ai.name == name) {
                    ai.init = init.protosub;
                    add_to.mo.local_attr[i] = ai;
                    break;
                }
            }
            return null;
        }
        public static object type_add_method(object[] args) {
            STable  add_to = (STable)Handle.Unbox(args[1]);
            int     mode   = (int)args[2];
            string  name   = (string)args[3];
            SubInfo sub    = (SubInfo)Handle.Unbox(args[4]);
            string  file   = (string)args[5];
            int     line   = (int)args[6];
            //int   pos    = (int)args[7];

            if ((mode & P6how.M_MASK) == P6how.M_ONLY) {
                foreach (P6how.MethodInfo mi in add_to.mo.lmethods) {
                    if (mi.Name() == name &&
                            ((mi.flags ^ mode) & P6how.V_MASK) == 0) {
                        return new Exception("Two definitions of method " +
                                name + Backend.LocStr(mi.file, mi.line, file, line));
                    }
                }
            }

            add_to.mo.AddMethodPos(mode, name, sub.protosub, file, line);
            return null;
        }
        public static object type_set_instantiation_block(object[] args) {
            STable  add_to = (STable)Handle.Unbox(args[1]);
            SubInfo block  = (SubInfo)Handle.Unbox(args[2]);
            add_to.mo.roleFactory = block.protosub;
            return null;
        }
        public static object type_add_trustee(object[] args) {
            STable st = (STable)Handle.Unbox(args[1]);
            STable nw = (STable)Handle.Unbox(args[2]);
            st.mo.trustees.Add(nw);
            return null;
        }
        public static object type_trusts(object[] args) {
            STable st = (STable)Handle.Unbox(args[1]);
            STable nw = (STable)Handle.Unbox(args[2]);
            return st == nw || st.mo.trustees.Contains(nw);
        }
        public static object type_closed(object[] args) {
            STable st = (STable)Handle.Unbox(args[1]);
            return st.mo.isComposed;
        }
        public static object type_close(object[] args) {
            STable st = (STable)Handle.Unbox(args[1]);
            string err = st.mo.Compose();
            if (err != null)
                return new Exception(err);
            return null;
        }
        public static object type_kind(object[] args) {
            STable st = (STable)Handle.Unbox(args[1]);
            return st.mo.rtype;
        }
        public static object type_name(object[] args) {
            STable st = (STable)Handle.Unbox(args[1]);
            return st.name;
        }
        public static object type_who(object[] args) {
            STable st = (STable)Handle.Unbox(args[1]);
            return Kernel.UnboxAny<string>(st.who);
        }
        public static object type_create(object[] args) {
            RuntimeUnit ru = (RuntimeUnit)Handle.Unbox(args[1]);
            string name = (string)args[2];
            string type = (string)args[3];
            string who  = (string)args[4];

            FieldInfo mof = ru.name == "CORE" ?
                typeof(Kernel).GetField(name + "MO") : null;
            FieldInfo pf = ru.name == "CORE" ?
                typeof(Kernel).GetField(name + "P") : null;

            STable nst = mof == null ? null : (STable)mof.GetValue(null);
            if (nst == null) { // not all FooMO are initialized by kernel
                nst = new STable(name);
                if (mof != null) mof.SetValue(null, nst);
            }
            // Hack - we don't clear the MRO here, because we might need
            // to call methods on the type in the process of defining the
            // type itself.
            nst.mo.superclasses.Clear();
            if (nst.typeObject == null) // AnyMO.typeObject is set up early
                nst.typeObject = new P6opaque(nst, 0);
            ((P6opaque)nst.typeObject).slots = null;
            nst.typeVar = Kernel.NewROScalar(nst.typeObject);

            if (ru.name == "CORE" && name == "Nil") {
                // this anomalous type object is iterable
                nst.typeVar = Kernel.NewRWListVar(nst.typeObject);
            }

            if (pf != null)
                pf.SetValue(null, nst.typeObject);

            nst.initVar    = nst.typeVar;
            nst.initObject = nst.typeObject;
            nst.who        = Kernel.BoxRaw(who, Kernel.StashMO);
            nst.how        = Kernel.BoxRaw<STable>(nst, Kernel.ClassHOWMO);
            nst.mo.rtype   = type;
            nst.mo.type =
                type == "package" ? P6how.PACKAGE :
                type == "module" ? P6how.MODULE :
                type == "class" ? P6how.CLASS :
                type == "grammar" ? P6how.GRAMMAR :
                type == "role" ? P6how.ROLE :
                type == "prole" ? P6how.PARAMETRIZED_ROLE :
                type == "subset" ? P6how.SUBSET :
                -1;
            if (nst.mo.type < 0)
                return new Exception(type);

            return new Handle(nst);
        }
        public static object type_set_basetype(object[] args) {
            STable subset = (STable)Handle.Unbox(args[1]);
            STable basety = (STable)Handle.Unbox(args[2]);

            subset.mo.FillSubset(basety);
            subset.initObject = basety.initObject;
            subset.initVar = basety.initVar;
            return null;
        }
        public static object type_get_basetype(object[] args) {
            return Handle.Wrap(((STable)Handle.Unbox(args[1])).mo.superclasses[0]);
        }
        public static object type_get_type_var(object[] args) {
            return Handle.Wrap(((STable)Handle.Unbox(args[1])).typeVar);
        }
        public static object type_set_where(object[] args) {
            STable  subset = (STable)Handle.Unbox(args[1]);
            SubInfo where  = (SubInfo)Handle.Unbox(args[2]);
            subset.mo.subsetWhereThunk = where.protosub;
            return null;
        }
        public static object create_sub(object[] args) {
            RuntimeUnit ru = (RuntimeUnit)Handle.Unbox(args[1]);
            string name = (string)args[2];
            SubInfo outer = (SubInfo)Handle.Unbox(args[3]);
            string cls = (string)args[4];
            STable pkg = (STable)Handle.Unbox(args[5]);
            STable icl = (STable)Handle.Unbox(args[6]);
            bool once = (bool)args[7];
            Frame outer_frame = (Frame)Handle.Unbox(args[8]);

            if (outer_frame != null && outer_frame.info != outer) {
                Console.WriteLine("MISMATCHED OUTER FRAME!!!");
                outer_frame = null;
            }

            STable rcls = ResolveSubClass(cls);
            if (rcls == null)
                return new Exception("sub-class lookup fail for " + cls);

            SubInfo n = new SubInfo(ru, name, outer, rcls, pkg, once,
                    outer_frame);
            n.in_class = icl;
            if (n.outer != null && n.outer.unit == ru)
                n.outer.children.Add(n);
            ru.our_subs.Add(n);

            if (outer == null) {
                /* Hack - embed build information */
                var li = new LIConstant();
                var info = new VarHash();
                info["name"] = Builtins.MakeStr("niecza");
                string vers = "(unknown)\n";
                try {
                    vers = File.ReadAllText("VERSION");
                } catch (Exception) {
                    // ignore
                }
                info["version"] = Builtins.MakeStr(
                        vers.Substring(0, vers.Length - 1));
                info["build-time"] = Builtins.now();

                li.value = Kernel.BoxAnyMO(info, Kernel.HashMO);

                n.AddLexical("$?PERL", li);
            }

            return new Handle(n);
        }
        public static object add_my_name(object[] args) {
            STable  type  = (STable)Handle.Unbox(args[6]);
            int     flags = (int)   args[7];

            return AddLexical(args, new LISimple(flags, type));
        }
        public static object add_hint(object[] args) {
            return AddLexical(args, new LIConstant());
        }
        public static object add_label(object[] args) {
            return AddLexical(args, new LILabel());
        }
        public static object add_dispatcher(object[] args) {
            return AddLexical(args, new LIDispatch());
        }
        public static object add_common_name(object[] args) {
            SubInfo sub   = (SubInfo)Handle.Unbox(args[1]);
            STable  pkg   = (STable)Handle.Unbox(args[6]);
            string  pname = (string)args[7];
            if (!pkg.who.Isa(Kernel.StashMO))
                return new Exception("NYI usage of a nonstandard package");
            string  who   = Kernel.UnboxAny<string>(pkg.who);

            string err = sub.unit.NsBind(who, pname, null,
                    (string)args[3], (int)args[4]);
            if (err != null) return new Exception(err);
            return AddLexical(args, new LICommon((char)who.Length + who + pname));
        }
        public static object add_state_name(object[] args) {
            SubInfo sub   = (SubInfo)Handle.Unbox(args[1]);
            SubInfo outer = (sub.special & SubInfo.MAINLINE) != 0 ?
                sub : sub.outer;
            STable  type  = (STable)Handle.Unbox(args[6]);
            int     flags = (int)   args[7];
            string  back  = (string)args[8];
            string  name  = (string)args[2];

            args[1] = Handle.Wrap(outer);
            args[2] = back;
            AddLexical(args, new LISimple(flags, type));
            if (name != null) {
                args[1] = Handle.Wrap(sub);
                args[2] = name;
                return AddLexical(args, new LIAlias(back));
            } else {
                return new object[] { "" };
            }
        }
        public static object add_alias(object[] args) {
            return AddLexical(args, new LIAlias((string)args[6]));
        }
        public static object add_attr_alias(object[] args) {
            return AddLexical(args, new LIAttrAlias((STable)Handle.Unbox(args[6]),
                        (string)args[7]));
        }
        public static object add_my_stash(object[] args) {
            STable  type  = (STable)Handle.Unbox(args[6]);

            return AddLexical(args, new LIPackage(type));
        }
        public static object add_my_sub(object[] args) {
            SubInfo body  = (SubInfo)Handle.Unbox(args[6]);

            return AddLexical(args, new LISub(body));
        }
        public static object sub_no_signature(object[] args) {
            SubInfo tgt = (SubInfo)Handle.Unbox(args[1]);
            tgt.sig = null;
            return null;
        }
        public static object param_new(object[] args) {
            SubInfo tgt = (SubInfo)Handle.Unbox(args[1]);
            int ix = 2;
            List<string> names = new List<string>();
            int    flags = (int)   args[ix++];
            string name  = (string)args[ix++];
            string slot  = (string)args[ix++];
            while(true) {
                string a_name = (string)args[ix++];
                if (a_name == null) break;
                names.Add(a_name);
            }
            SubInfo deflt = (SubInfo)Handle.Unbox(args[ix++]);
            STable  type  = (STable)Handle.Unbox(args[ix++]);
            if (deflt != null) flags |= Parameter.HASDEFAULT;
            if (type != null) flags |= Parameter.HASTYPE;
            string attr = (string)args[ix++];
            STable atype = (STable)Handle.Unbox(args[ix++]);

            return Handle.Wrap(new Parameter(flags,
                (slot == null ? -1 : tgt.dylex[slot].SigIndex()),
                name, (names.Count == 0 ? null : names.ToArray()),
                deflt, type ?? Kernel.AnyMO, attr, atype));
        }
        public static object param_constraints(object[] args) {
            Parameter tgt = (Parameter)Handle.Unbox(args[1]);
            tgt.post_constraints = new object[args.Length - 2];
            for (int ix = 2; ix != args.Length; ix++)
                tgt.post_constraints[ix-2] = (args[ix] is Handle) ?
                    Handle.Unbox(args[ix]) : args[ix];
            return null;
        }
        public static object param_subsig(object[] args) {
            Parameter tgt = (Parameter)Handle.Unbox(args[1]);
            int ix = 2;
            List<Parameter> sig = new List<Parameter>();
            while (ix != args.Length)
                sig.Add((Parameter)Handle.Unbox(args[ix++]));
            object[] pc = tgt.post_constraints;
            Array.Resize(ref pc, pc == null ? 1 : pc.Length + 1);
            pc[pc.Length - 1] = new Signature(sig.ToArray());
            tgt.post_constraints = pc;
            return null;
        }
        public static object set_signature(object[] args) {
            SubInfo tgt = (SubInfo)Handle.Unbox(args[1]);
            int ix = 2;
            List<Parameter> sig = new List<Parameter>();
            while (ix != args.Length)
                sig.Add((Parameter)Handle.Unbox(args[ix++]));
            tgt.sig = new Signature(sig.ToArray());
            return null;
        }
        public static object sub_contains_phaser(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            int     p = (int)args[2];
            foreach (SubInfo z in s.children)
                if (z.phaser == p)
                    return true;
            return false;
        }
        public static object sub_set_phaser(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            int     p = (int)args[2];
            s.phaser = p;
            if (p == Kernel.PHASER_CATCH)
                s.outer.catch_ = s;
            if (p == Kernel.PHASER_CONTROL)
                s.outer.control = s;
            if (p == Kernel.PHASER_INIT)
                Compartment.Top.init.Add(s, true);
            if (p == Kernel.PHASER_CHECK)
                Compartment.Top.check.Add(s, true);
            if (p == Kernel.PHASER_END)
                Compartment.Top.end.Add(s, true);
            return null;
        }
        public static object sub_set_extend(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            object[] val = new object[args.Length - 3];
            Array.Copy(args, 3, val, 0, val.Length);
            if (s.extend == null)
                s.extend = new Dictionary<string,object[]>();
            s.extend[(string)args[2]] = val;
            return null;
        }
        public static object sub_get_extend(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            object[] ret = null;
            if (s.extend != null)
                s.extend.TryGetValue((string)args[2], out ret);
            return ret ?? new object[0];
        }
        public static object sub_finish(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            s.nam_str = (string)args[2];
            s.nam_refs = new object[args.Length - 3];
            for (int i = 0; i < s.nam_refs.Length; i++)
                s.nam_refs[i] = Handle.Unbox(args[i+3]);
            s.code = RuntimeUnit.JitCompileSub;
            if (s.protopad != null)
                s.protopad.code = s.code;
            return null;
        }
        public static object sub_finish_dispatcher(object[] args) {
            SubInfo s = (SubInfo)Handle.Unbox(args[1]);
            string  k = (string)args[2];
            if (k == "regex")
                s.code = Lexer.StandardProtoC;
            else if (k == "multi")
                s.code = Kernel.StandardTypeProtoC;
            else
                return new Exception("Unknown dispatcher type " + k);
            if (s.protopad != null)
                s.protopad.code = s.code;
            return null;
        }
        public static object save_unit(object[] args) {
            RuntimeUnit ru = (RuntimeUnit)Handle.Unbox(args[1]);
            if (!ru.is_mainish && ru.bottom == null)
                ru.RunMainline();
            if (ru.is_mainish)
                Compartment.Top.check.Run();
            ru.Save();
            return null;
        }
        public static object run_unit(object[] args) {
            RuntimeUnit ru = (RuntimeUnit)Handle.Unbox(args[1]);
            bool evalmode = (bool)args[2];
            Kernel.commandArgs = new string[args.Length - 3];
            Array.Copy(args, 3, Kernel.commandArgs, 0, args.Length - 3);
            Kernel.currentGlobals = ru.globals;
            ru.PrepareEval();
            Compartment.Top.check.Run();
            Compartment.Top.init.Run();
            if (!evalmode)
                Kernel.RunMain(ru);
            return null;
        }
        public static object setnames(object[] args) {
            Builtins.execName = (string)args[1];
            Builtins.programName = (string)args[2];
            return null;
        }
        public static object unit_replrun(object[] args) {
            RuntimeUnit ru = (RuntimeUnit)Handle.Unbox(args[1]);
            Frame fret = null;
            Compartment.Top.check.Run();
            Compartment.Top.init.Run();
            StashEnt b = Kernel.GetVar("::PROCESS", "$OUTPUT_USED");
            b.Bind(Kernel.FalseV);
            Frame ir = Kernel.GetInferiorRoot();
            fret = ru.mainline.protosub.Invoke(ir, Variable.None, null);
            fret.MarkShared();
            Variable r = Kernel.RunInferior(fret);
            if (!b.v.Fetch().mo.mro_raw_Bool.Get(b.v)) {
                Variable pl = Kernel.RunInferior(
                    r.Fetch().InvokeMethod(Kernel.GetInferiorRoot(),
                        "gist", new Variable[] { r }, null));
                Console.WriteLine(pl.Fetch().mo.mro_raw_Str.Get(pl));
            }
            return new Handle(fret);
        }
        public static object safemode(object[] args) {
            Kernel.SaferMode = true;
            return null;
        }
        public static object get_codepoint(object[] args) {
            return Niecza.UCD.DataSet.GetCodepoint((string)args[1]);
        }
    }
}
