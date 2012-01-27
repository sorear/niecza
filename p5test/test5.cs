using System.Runtime.InteropServices;
public class Test4 {
    [DllImport("test5lib", EntryPoint="run_test")]
    public static extern void RunTest();
    public static void Main() {
        RunTest();
    }
}

