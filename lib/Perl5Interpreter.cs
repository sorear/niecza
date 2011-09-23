using Niecza;
public class Perl5Interpreter : ForeignInterpreter {
    public Perl5Interpreter() {
        System.Console.WriteLine("creating perl5 interpreter");
    }
    public void eval(string code) {
        System.Console.WriteLine("evaling perl5 (NYI):"+code);
    }
}
