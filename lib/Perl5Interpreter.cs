using Niecza;
using System.Runtime.InteropServices;
using System;

public class Perl5Interpreter : IForeignInterpreter {
    [DllImport("obj/p5embed.so", EntryPoint="p5embed_initialize")]
    public static extern void Initialize();
  
    [DllImport("obj/p5embed.so", EntryPoint="p5embed_dispose")]
    public static extern void Dispose();
  
    [DllImport("obj/p5embed.so", EntryPoint="p5embed_eval")]
    public static extern IntPtr EvalPerl5(string code);

    [DllImport("obj/p5embed.so", EntryPoint="p5embed_SvIV")]
    public static extern int SvIV(IntPtr sv);

    [DllImport("obj/p5embed.so", EntryPoint="p5embed_SvPV_nolen")]
    public static extern string SvPV_nolen(IntPtr sv);

    [DllImport("obj/p5embed.so", EntryPoint="p5embed_SvNV")]
    public static extern double SvNV(IntPtr sv);

    [DllImport("obj/p5embed.so", EntryPoint="p5embed_SvIOKp")]
    public static extern int SvIOKp(IntPtr sv);

    [DllImport("obj/p5embed.so", EntryPoint="p5embed_SvNOKp")]
    public static extern int SvNOKp(IntPtr sv);

    [DllImport("obj/p5embed.so", EntryPoint="p5embed_SvPOKp")]
    public static extern int SvPOKp(IntPtr sv);

    [DllImport("obj/p5embed.so", EntryPoint="p5embed_newSVpvn")]
    public static extern IntPtr newSVpvn(string s,int length);

    [DllImport("obj/p5embed.so", EntryPoint="p5embed_subcall")]
    public static extern IntPtr SubCall(
        int context,
        IntPtr[] arguments,
        int argument_n
    );

    public static Variable SVToVariable(IntPtr sv) {
        if (sv == IntPtr.Zero) {
            //TODO: check - cargo culted
            return Kernel.NilP.mo.typeVar;
        }
        if (SvIOKp(sv) != 0) {
            return Builtins.MakeInt(SvIV(sv));
        } else if (SvNOKp(sv) != 0) {
            return Builtins.MakeFloat(SvNV(sv));
        } else if (SvPOKp(sv) != 0) {
            string s = SvPV_nolen(sv);
            return Kernel.BoxAnyMO(s, Kernel.StrMO);
        } else {
            return new SVVariable(sv);
        }
    }
  
    public Perl5Interpreter() {
        Initialize();
    }
    ~Perl5Interpreter() {
        Dispose();
    }
    public Variable Eval(string code) {
        IntPtr sv = EvalPerl5(code);
        return SVToVariable(sv);
    }
}

public class SVVariable : Variable {
    public IntPtr sv;
    public SVVariable(IntPtr _sv) {
        sv = _sv;
    }
    public override P6any Fetch() {
        return new SVany(sv);
    }
    public override void Store(P6any v) {
    }
    public override Variable GetVar() {
            return Kernel.BoxAnyMO<Variable>(this, Kernel.ScalarMO);

    }
}
public class SVany : P6any {
        [DllImport("obj/p5embed.so", EntryPoint="p5method_call")]
        public static extern IntPtr MethodCall(
            string name,
            IntPtr[] arguments,
            int argument_n
        );

        public static IntPtr VariableToSV(Variable var) {
            P6any obj = var.Fetch();
            if (obj is SVany) {
                return ((SVany)obj).sv;
            } else if (obj.Does(Kernel.StrMO)) {
                string s = Kernel.UnboxAny<string>(obj);
                return Perl5Interpreter.newSVpvn(s,s.Length);
            } else {
                throw new NieczaException("can't convert argument to p5 type");
            }
        }

        static int Context(Variable var) {
            P6any obj = var.Fetch();
            string s = Kernel.UnboxAny<string>(obj);
            if (s == "list") {
                return 0;
            } else if (s == "scalar") {
                return 1;
            } else if (s == "void") {
                return 2;
            } else {
                throw new NieczaException("unknown p5 context type: "+s);
            }
        }

        static IntPtr[] MarshalPositionals(Variable[] pos) {
                IntPtr[] args = new IntPtr[pos.Length];
                for (int i=0;i<pos.Length;i++) {
                    args[i] = VariableToSV(pos[i]);
                }
                return args;
        }

        public IntPtr sv;
        public override Frame InvokeMethod(Frame caller, string name,
                Variable[] pos, VarHash named) {

                if (name == "postcircumfix:<( )>") {
                    int context = 1;
                    if (named["context"] != null) {
                        context = Context(named["context"]);
                    }
                    IntPtr[] args = MarshalPositionals(pos);
                    IntPtr ret = Perl5Interpreter.SubCall(context,args,args.Length);

            
                    caller.resultSlot = Perl5Interpreter.SVToVariable(ret);
                    return caller;
                } else {
                    IntPtr[] args = MarshalPositionals(pos);
                    IntPtr ret = MethodCall(name,args,args.Length);
                    caller.resultSlot = Perl5Interpreter.SVToVariable(ret);
                }

                return caller;
        }

        public SVany(IntPtr _sv) {
            mo = Kernel.AnyMO;
            sv = _sv;
        }
}

