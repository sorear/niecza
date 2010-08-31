using Niecza;
using System;
using System.Collections.Generic;

class CodeBuf {
    public delegate Frame D(Frame th);

    public List<D> ops = new List<D>();
    public List<int> labels = new List<int>();

    public int NewLabel() { labels.Add(0); return labels.Count - 1; }
}

abstract class RxOp {
    public abstract void Compile(CodeBuf cb);
}

class RxExact: RxOp {
    string s;
    public RxExact(string s) { this.s = s; }
    public override void Compile(CodeBuf cb) {
        cb.ops.Add(delegate(Frame th) {
            return th.rx.Exact(th, s);
        });
    }
}

class RxMeta: RxOp {
    public delegate void MD(RxFrame rx);
    MD md;
    public RxMeta(MD md) { this.md = md; }
    public override void Compile(CodeBuf cb) {
        cb.ops.Add(delegate(Frame th) {
            md(th.rx);
            return th;
        });
    }
}

class RxStar: RxOp {
    RxOp inner;
    public RxStar(RxOp inner) { this.inner = inner; }
    public override void Compile(CodeBuf cb) {
        int l1 = cb.NewLabel();
        int l2 = cb.NewLabel();
        cb.ops.Add(delegate(Frame th) {
            th.rx.OpenQuant();
            return th;
        });
        cb.labels[l1] = cb.ops.Count;
        cb.ops.Add(delegate(Frame th) {
            th.rx.PushBacktrack("*", cb.labels[l2]);
            return th;
        });
        inner.Compile(cb);
        cb.ops.Add(delegate(Frame th) {
            th.ip = cb.labels[l1];
            return th;
        });
        cb.labels[l2] = cb.ops.Count;
        cb.ops.Add(delegate(Frame th) {
            th.rx.CloseQuant();
            return th;
        });
    }
}

class RxSeq: RxOp {
    RxOp[] z;
    public RxSeq(RxOp[] z) { this.z = z; }
    public override void Compile(CodeBuf cb) {
        foreach (RxOp ze in z)
            ze.Compile(cb);
    }
}

public class RxUnitTest {
    private static SubInfo TestSI = new SubInfo(TestC);
    private static bool ok;
    private static Frame TestC(Frame th) {
        if (Kernel.TraceCont) Console.WriteLine("At {0}", th.ip);
        CodeBuf cb = (CodeBuf) th.lex0;
        if (th.ip == cb.ops.Count) {
            ok = true;
            return null;
        }
        CodeBuf.D d = cb.ops[th.ip++];
        return d(th);
    }

    private static void RunTest(int ix, bool y, string s, RxOp ot) {
        CodeBuf cb = new CodeBuf();
        ot.Compile(cb);

        Frame th = new Frame(null, null, TestSI);
        th.rx = new RxFrame(Kernel.MockBox(new Cursor(s)));
        th.lex0 = cb;

        ok = false;
        while (th != null)
            th = th.Continue();

        Console.WriteLine((ok == y) ? "ok {0}" : "not ok {0}", ix);
    }


    public static void Main() {
        RunTest(0, true, "aaaaab", new RxSeq(new RxOp[] { new RxStar( new RxExact("a") ), new RxExact("ab") }));
        RunTest(1, false, "aaaaab", new RxSeq(new RxOp[] { new RxStar( new RxExact("a") ), new RxMeta(delegate(RxFrame rx) { rx.CommitAll(); }), new RxExact("ab") }));
    }
}
