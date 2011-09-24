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


  
    public Perl5Interpreter() {
        Initialize();
    }
    ~Perl5Interpreter() {
        Dispose();
    }
    public Variable Eval(string code) {
        return new SVVariable(EvalPerl5(code));
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
        public static extern IntPtr MethodCall(string code,IntPtr invocant);

        public IntPtr sv;
        public override Frame InvokeMethod(Frame caller, string name,
                Variable[] pos, VarHash named) {
                MethodCall(name,sv);
                return caller;
        }
        public SVany(IntPtr _sv) {
            mo = Kernel.AnyMO;
            sv = _sv;
        }
}

