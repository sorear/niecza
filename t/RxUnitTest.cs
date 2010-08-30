using Niecza;
using System;

public class RxUnitTest {
    private static SubInfo TestSI = new SubInfo(TestC);
    private static Frame TestC(Frame th) {
        if (Kernel.TraceCont) Console.WriteLine("At {0}", th.ip);
        switch (th.ip) {
            case 0:
                th.rx = new RxFrame(Kernel.MockBox(new Cursor("aaaaab")));
                th.rx.OpenQuant();
                goto case 1;
            case 1:
                th.rx.PushBacktrack("*", 3);
                th.ip = 2;
                return th.rx.ExactOne(th, 'a');
            case 2:
                th.rx.IncQuant();
                goto case 1;
            case 3:
                th.rx.CloseQuant();
                th.ip = 4;
                return th.rx.Exact(th, "ab");
            case 4:
                System.Console.WriteLine("Match!");
                return null;
            default:
                System.Console.WriteLine("Bad IP");
                return null;
        }
    }

    public static void Main() {
        Frame fr = new Frame(null, null, TestSI);

        while (fr != null)
            fr = fr.Continue();
    }
}
