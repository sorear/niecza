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

    [DllImport("obj/p5embed.so", EntryPoint="p5embed_SvIOKp")]
    public static extern int SvIOKp(IntPtr sv);

    [DllImport("obj/p5embed.so", EntryPoint="p5embed_SvNOKp")]
    public static extern int SvNOKp(IntPtr sv);

    [DllImport("obj/p5embed.so", EntryPoint="p5embed_SvPOKp")]
    public static extern int SvPOKp(IntPtr sv);

    public static Variable SVToVariable(IntPtr sv) {
        if (SvIOKp(sv) != 0) {
            return Builtins.MakeInt(SvIV(sv));
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
        return SVToVariable(EvalPerl5(code));
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
            IntPtr[] foo,
            int n
        );

        public static IntPtr VariableToSV(Variable var) {
            P6any obj = var.Fetch();
            if (obj is SVany) {
                return ((SVany)obj).sv;
            } else {
                throw new NieczaException("can't convert argument to p5 type");
            }
        }

        public IntPtr sv;
        public override Frame InvokeMethod(Frame caller, string name,
                Variable[] pos, VarHash named) {
                IntPtr[] args = new IntPtr[pos.Length];
                for (int i=0;i<pos.Length;i++) {
                    args[i] = VariableToSV(pos[i]);
                }
                MethodCall(name,args,args.Length);
                return caller;
        }

        public SVany(IntPtr _sv) {
            mo = Kernel.AnyMO;
            sv = _sv;
        }
}

