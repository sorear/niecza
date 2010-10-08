using System;
public class LoopMicro {
    //abstract class LoopDel {
    //    public abstract Frame Continue(Frame f);
    //}
    //class T : LoopDel {
    //    public override Frame Continue(Frame f) {
    delegate Frame LoopDel(Frame f);
        static Frame T(Frame f) {
            if (f.lex == 0) Environment.Exit(0);
            f.lex--;
            return f;
        }
    //}
    class Frame {
        internal LoopDel dgt;
        internal int lex;
    }
    static void Main() {
        Frame f = new Frame();
        f.lex = 1000000000;
        //f.dgt = new T();
        //for (;;) { f = f.dgt.Continue(f); }
        f.dgt = T;
        for (;;) { f = f.dgt(f); }
    }
}
