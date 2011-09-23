using Niecza;
using System.Runtime.InteropServices;

public class Perl5Interpreter : IForeignInterpreter {
    [DllImport("obj/p5embed.so", EntryPoint="p5embed_initialize")]
    public static extern void Initialize();
  
    [DllImport("obj/p5embed.so", EntryPoint="p5embed_dispose")]
    public static extern void Dispose();
  
    [DllImport("obj/p5embed.so", EntryPoint="p5embed_eval")]
    public static extern void EvalPerl5(string code);
  
    public Perl5Interpreter() {
        Initialize();
    }
    public void Eval(string code) {
        EvalPerl5(code);
    }
}
