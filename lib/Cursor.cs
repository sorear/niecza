using Niecza;
using System;
using System.Collections.Generic;
using System.Text;

public sealed class PSN<X> {
    public X obj;
    public readonly PSN<X> next;
    public PSN(X obj, PSN<X> next) { this.obj = obj; this.next = next; }
}

// stuff that should 'N'est, like subrules do
public sealed class NState {
    public NState next;
    public int quant;
    public DynMetaObject klass;

    public NState(NState proto) {
        next = proto; if (proto != null) klass = proto.klass;
    }
}

public struct State {
    public PSN<Cursor>   captures;
    public PSN<string[]> capnames;
    public NState ns;

    public Variable subrule_iter;
    public int pos;
}

public sealed class Choice {
    public Choice prev;
    public State st;

    // the current Choice doesn't really have any of these fields, which are
    // used for backtrack control
    public int ip;
    public string tag;
    public bool committed;

    public void Commit() {
        // we aren't going to backtrack here, so we can't use this.
        // help out the GC
        st.subrule_iter = null;
        committed = true;
    }

    public Choice(Choice prev, string tag, int ip, State st) {
        this.tag = tag;
        this.ip = ip;
        this.st = st;
        this.prev = prev;
    }

    public void Dump(string st) {
        string ac = st;
        for (Choice x = this; x != null; x = x.prev) {
            ac += x.committed ? " + " : " - ";
            ac += x.tag;
        }
        Console.WriteLine(ac);
    }
}

// extends Frame for a time/space tradeoff
// we keep the cursor in exploded form to avoid creating lots and lots of
// cursor objects
public sealed class RxFrame {
    public Choice bt;
    public State st;

    // when this is set, one value has already been given, so we don't need
    // any more Lists
    public bool return_one;

    // .from in matches
    public int from;

    // our backing string, in a cheap to index form
    public string orig_s;
    public char[] orig;
    // cache of orig.Length
    public int end;

    // don't remove this on backtracking, and quit if we would back into it
    public readonly Choice rootf;

    public RxFrame(string name, Cursor csr) {
        orig = csr.backing_ca;
        orig_s = csr.backing;
        end = orig.Length;
        rootf = bt = new Choice(csr.xact, "RULE " + name, -1, default(State));
        st.ns = new NState(csr.nstate);
        st.ns.klass = csr.klass;
        st.pos = csr.pos;
        from = csr.pos;
    }

    public Frame Backtrack(Frame th) {
        // throw away cut or mark-only frames
        while (bt != rootf && (bt.committed || bt.ip < 0))
            bt = bt.prev;
        if (bt == rootf) {
            if (return_one) {
                return Kernel.Take(th, Kernel.NewROScalar(EMPTYP));
            } else {
                DynObject lst = new DynObject(ListMO);
                lst.slots[0 /*items*/] = new VarDeque();
                lst.slots[1 /*rest*/ ] = new VarDeque();
                lst.slots[2 /*flat*/ ] = false;
                th.caller.resultSlot = Kernel.NewRWListVar(lst);
            }

            return th.caller;
        } else {
            th.ip = bt.ip;
            st = bt.st;
            bt = bt.prev;
            return th;
        }
    }

    public void PushBacktrack(string name, int ip) {
        bt = new Choice(bt, name, ip, st);
    }

    public void PushCapture(string[] cn, Cursor cl) {
        st.capnames = new PSN<string[]>(cn, st.capnames);
        st.captures = new PSN<Cursor>(cl, st.captures);
    }

    public void SetCursorList(Variable cl) {
        st.subrule_iter = cl;
    }

    public Variable GetCursorList() {
        return st.subrule_iter;
    }

    public void SetPos(int pos) {
        st.pos = pos;
    }

    public void CommitAll() {
        Choice x = bt;
        while (x != null) {
            x.Commit();
            x = x.prev;
        }
    }

    public void CommitSpecificRule(string name) {
        Choice x = bt;
        name = "RULE " + name;
        while (x != null) {
            x.Commit();
            if (x.tag.Equals(name))
                break;
            x = x.prev;
        }
    }

    public void CommitRule() {
        Choice x = bt;
        while (x != rootf) {
            x.Commit();
            x = x.prev;
        }
    }

    public void CommitGroup(string open, string close) {
        Choice x = bt;
        if (Cursor.Trace)
            x.Dump("committing " + open + "," + close);
        int level = 1;
        while (x != null) {
            x.Commit();
            if (x.tag == open)
                level--;
            else if (x.tag == close)
                level++;
            x = x.prev;
            if (level == 0)
                break;
        }
    }

    public bool IsTopCut() {
        return bt.committed;
    }

    public bool Exact(string str) {
        if (st.pos + str.Length > end)
            return false;
        foreach (char ch in str)
            if (orig[st.pos++] != ch)
                return false;
        return true;
    }

    public bool ExactOne(char ch) {
        return !(st.pos == end || orig[st.pos++] != ch);
    }

    public bool AnyChar() {
        return !(st.pos++ == end);
    }

    public bool CClass(CC x) {
        return !(st.pos == end || !x.Accepts(orig[st.pos++]));
    }

    public bool ScanCClass(int min, int max, CC x) {
        int i;
        int maxr = end - st.pos;
        if (maxr < max) max = maxr;

        for (i = 0; i < max && x.Accepts(orig[st.pos + i]); i++);
        st.pos += i;
        return (i >= min);
    }

    public void LTMPushAlts(Lexer lx, int[] addrs) {
        PushBacktrack("LTM", -1);
        int[] cases = lx.Run(orig_s, st.pos);
        for (int i = cases.Length - 1; i >= 0; i--) {
            PushBacktrack("LTMALT", addrs[cases[i]]);
        }
    }

    public void OpenQuant() {
        st.ns = new NState(st.ns);
    }

    public int CloseQuant() {
        int x = st.ns.quant;
        st.ns = st.ns.next;
        return x;
    }

    public void IncQuant() {
        st.ns.quant++;
    }

    public int GetQuant() {
        return st.ns.quant;
    }

    public Cursor MakeCursor() {
        return new Cursor(st.ns.klass, st.ns, bt, orig_s, orig, st.pos);
    }

    public Cursor MakeMatch() {
        return new Cursor(orig_s, from, st.pos, st.captures, st.capnames);
    }

    public static DynMetaObject MatchMO;
    public static DynMetaObject ListMO;
    public static DynMetaObject GatherIteratorMO;
    public static IP6 EMPTYP;

    public Frame End(Frame th) {
        return End(th, MakeMatch());
    }
    // currently just used for protoregex
    public Frame End(Frame th, Cursor m) {
        if (return_one) {
            return Kernel.Take(th, Kernel.NewROScalar(m));
        } else {
            return_one = true;
            VarDeque ks = new VarDeque();
            ks.Push(Kernel.NewROScalar(m));
            DynObject it  = new DynObject(GatherIteratorMO);
            it.slots[0 /*frame*/] = Kernel.NewRWScalar(th);
            it.slots[1 /*reify*/] = Kernel.NewRWScalar(Kernel.AnyP);
            VarDeque iss = new VarDeque();
            iss.Push(Kernel.NewROScalar(it));
            DynObject lst = new DynObject(ListMO);
            lst.slots[0 /*items*/] = ks;
            lst.slots[1 /*rest*/ ] = iss;
            lst.slots[2 /*flat*/ ] = false;
            th.caller.resultSlot = Kernel.NewRWListVar(lst);
        }
        return th.caller;
    }
}

// This is used to carry match states in and out of subrules.  Within subrules,
// match states are represented much more ephemerally in the state of RxFrame.

// this does double duty backing Match; note that Cursor and Match need to be
// treated polymorphically in a couple places
public class Cursor : IP6 {
    public static bool Trace =
        Environment.GetEnvironmentVariable("NIECZA_RX_TRACE") != null;

    public DynMetaObject klass;
    public Choice xact;
    public NState nstate;
    public string backing;
    public char[] backing_ca;
    public int from;
    public int pos;
    public PSN<Cursor> captures;
    public PSN<string[]> capnames;

    public Cursor(IP6 proto, string text)
        : this(proto.GetMO(), null, null, text, text.ToCharArray(), 0) { }

    public Cursor(string backing, int from, int pos, PSN<Cursor> captures,
            PSN<string[]> capnames) {
        this.backing = backing;
        this.captures = captures;
        this.capnames = capnames;
        this.pos = pos;
        this.from = from;
        this.klass = RxFrame.MatchMO;
    }

    public Cursor(DynMetaObject klass, NState ns, Choice xact, string backing, char[] backing_ca, int pos) {
        this.klass = klass;
        this.xact = xact;
        this.nstate = ns;
        this.backing = backing;
        this.backing_ca = backing_ca;
        this.pos = pos;
    }

    public override DynMetaObject GetMO() { return klass; }

    public override Frame GetAttribute(Frame caller, string name) {
        return Fail(caller, "Cursors cannot have attributes");
    }

    public override bool IsDefined() {
        return true;
    }

    public Cursor At(int npos) {
        return new Cursor(klass, nstate, xact, backing, backing_ca, npos);
    }

    // TODO: keep variables around so { $<foo> = 1 } will work
    public Variable GetKey(string str) {
        PSN<string[]> cn_it = capnames;
        PSN<Cursor> cr_it = captures;
        VarDeque caps = new VarDeque();
        bool list = false;

        while (cr_it != null) {
            foreach (string cn in cn_it.obj) {
                if (cn == str)
                    goto yes;
            }
            goto no;
yes:
            if (cr_it.obj == null) {
                list = true;
            } else {
                caps.Unshift(Kernel.NewRWScalar(cr_it.obj));
            }
no:
            cr_it = cr_it.next;
            cn_it = cn_it.next;
        }

        if (list) {
            DynObject l = new DynObject(RxFrame.ListMO);
            l.slots[0 /*items*/] = caps;
            l.slots[1 /*rest*/ ] = new VarDeque();
            l.slots[2 /*flat*/ ] = false;
            return Kernel.NewRWListVar(l);
        } else {
            return caps.Count() != 0 ? caps[0] :
                Kernel.NewRWScalar(Kernel.AnyP);
        }
    }

    public Variable SimpleWS() {
        int l = backing_ca.Length;
        int p = pos;

        VarDeque ks = new VarDeque();

        DynObject lst = new DynObject(RxFrame.ListMO);
        lst.slots[0 /*items*/] = ks;
        lst.slots[1 /*rest*/ ] = new VarDeque();
        lst.slots[2 /*flat*/ ] = false;

        if (p != 0 && p != l && CC.Word.Accepts(backing[p]) &&
                CC.Word.Accepts(backing[p-1])) {
            if (Trace)
                Console.WriteLine("! no match <ws> at {0}", pos);
        } else {
            while (p != l && Char.IsWhiteSpace(backing, p)) { p++; }
            if (Trace)
                Console.WriteLine("* match <ws> at {0} to {1}", pos, p);
            ks.Push(Kernel.NewROScalar(At(p)));
        }

        return Kernel.NewROScalar(lst);
    }
}

public sealed class CC {
    public readonly int[] vec;

    public CC(int[] vec) {
        this.vec = vec;
    }

    public CC(char butyes) : this(new int[] { butyes, -1, butyes+1, 0 }) { }
    public CC(int catmask) : this(new int[] { 0, catmask }) { }

    public bool Accepts(char ch) {
        int l = 0;
        int h = vec.Length / 2;

        if (h == 0 || ch < vec[0])
            return false;

        while (h - l > 1) {
            int m = l + (h - l) / 2;
            if (vec[m * 2] > ch) {
                h = m;
            } else {
                l = m;
            }
        }

        int mask = 1 << (int)char.GetUnicodeCategory(ch);
        return (vec[l * 2 + 1] & mask) != 0;
    }

    public const int MAlpha   =       0x1F;
    public const int MMark    =       0xE0;
    public const int MNum     =      0x700;
    public const int MSpace   =     0x3800;
    public const int MControl =    0x3C000;
    public const int MPunct   =  0x1FC0000;
    public const int MSymbol  = 0x1E000000;
    public const int MOther   = 0x20000000;
    public const int MAll     = 0x3FFFFFFF;

    public const int MAlNum   = MAlpha | MNum;

    public static readonly CC Word  = new CC(new int[] { 0, MAlNum,
            '_', MAll, '_'+1, MAlNum });

    public static readonly CC All   = new CC(MAll);
    public static readonly CC None  = new CC(0);
    public static readonly CC AlNum = new CC(MAlNum);

    private static readonly string[] categories = new string[] {
        "Lu", "Ll", "Lt", "Lm",  "Lo", "Mn", "Mc", "Me",
        "Nd", "Nl", "No", "Zs",  "Zl", "Zp", "Cc", "Cf",
        "Cs", "Co", "Pc", "Pd",  "Ps", "Pe", "Pi", "Pf",
        "Po", "Sm", "Sc", "Sk",  "So", "Cn"
    };

    public override string ToString() {
        StringBuilder sb = new StringBuilder();
        for (int ix = 0; ix < vec.Length; ix += 2) {
            if (sb.Length != 0)
                sb.Append(',');
            int l = vec[ix];
            int msk = vec[ix+1];
            int h = (ix + 2 < vec.Length) ? vec[ix+2] : 0x110000;

            if (msk == 0)
                continue;
            if (h != 0x110000 || l != 0)
                sb.AppendFormat("({0:X4}..{1:X4})", l, h-1);
            if ((msk & MAll) != MAll) {
                int used = 0;
                for (int c = 0; c <= 29; c++) {
                    if ((msk & (1 << c)) != 0) {
                        if ((used++) != 0)
                            sb.Append('+');
                        sb.Append(categories[c]);
                    }
                }
            }
        }
        return sb.ToString();
    }
}

public sealed class NFA {
    public sealed class Node {
        public int fate;
        public bool final;
        public List<Edge> edges = new List<Edge>();
        public Node(int curfate) { fate = curfate; }

        public override string ToString() {
            return "(" + fate + ")" + (final ? "+ " : " ") +
                Kernel.JoinS(", ", edges);
        }
    }

    public sealed class Edge {
        public int to;
        public CC when; // null if epsilon

        public override string ToString() {
            return ((when != null) ? when.ToString() : "ε") + " => " + to;
        }
    }

    public List<Node> nodes = new List<Node>();
    public int curfate;

    public DynMetaObject cursor_class;
    public HashSet<string> method_stack = new HashSet<string>();
    public Dictionary<string,LAD> method_cache = new Dictionary<string,LAD>();

    public LAD ResolveMethod(string name) {
        LAD sub = null;
        if (method_cache.TryGetValue(name, out sub))
            return sub;
        IP6 method = cursor_class.Can(name);

        if (Lexer.LtmTrace && method != null)
            Console.WriteLine("+ Found method");

        sub = ((SubInfo)(((DynObject)method).GetSlot("info"))).ltm;

        if (Lexer.LtmTrace)
            Console.WriteLine("+ {0} to sub-automaton",
                    (sub != null ? "Resolved" : "Failed to resolve"));

        method_cache[name] = sub;
        return sub;
    }

    public int AddNode() {
        nodes.Add(new Node(curfate));
        return nodes.Count - 1;
    }
    public void AddEdge(int from, int to, CC when) {
        Edge e = new Edge();
        e.to = to;
        e.when = when;
        nodes[from].edges.Add(e);
    }

    public void Dump() {
        for (int ix = 0; ix < nodes.Count; ix++) {
            Console.WriteLine(ix + ": " + nodes[ix].ToString());
        }
    }
}

// ltm automaton descriptors
public abstract class LAD {
    public abstract void ToNFA(NFA pad, int from, int to);
    public abstract void Dump(int indent);
    public virtual void QueryLiteral(NFA pad, out int len, out bool cont) {
        len = 0; cont = false;
    }
}

public class LADStr : LAD {
    public readonly string text;
    public LADStr(string text) { this.text = text; }

    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        len = text.Length; cont = true;
    }

    public override void ToNFA(NFA pad, int from, int to) {
        if (text.Length == 0) {
            pad.AddEdge(from, to, null);
        } else {
            int len = text.Length;
            for (int c = 0; c < len; c++) {
                int fromp = (c == len - 1) ? to : pad.AddNode();
                pad.AddEdge(from, fromp, new CC(text[c]));
                from = fromp;
            }
        }
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "str: " + text);
    }
}

public class LADCC : LAD {
    public readonly CC cc;
    public LADCC(CC cc) { this.cc = cc; }

    public override void ToNFA(NFA pad, int from, int to) {
        pad.AddEdge(from, to, cc);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "cc: " + cc.ToString());
    }
}

public class LADStar : LAD {
    public readonly LAD child;
    public LADStar(LAD child) { this.child = child; }

    public override void ToNFA(NFA pad, int from, int to) {
        int knot = pad.AddNode();
        pad.AddEdge(from, knot, null);
        pad.AddEdge(knot, to, null);
        child.ToNFA(pad, knot, knot);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "star:");
        child.Dump(indent + 4);
    }
}

public class LADOpt : LAD {
    public readonly LAD child;
    public LADOpt(LAD child) { this.child = child; }

    public override void ToNFA(NFA pad, int from, int to) {
        pad.AddEdge(from, to, null);
        child.ToNFA(pad, from, to);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "opt:");
        child.Dump(indent + 4);
    }
}

public class LADPlus : LAD {
    public readonly LAD child;
    public LADPlus(LAD child) { this.child = child; }

    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        child.QueryLiteral(pad, out len, out cont);
    }

    public override void ToNFA(NFA pad, int from, int to) {
        int knot1 = pad.AddNode();
        int knot2 = pad.AddNode();
        pad.AddEdge(from, knot1, null);
        pad.AddEdge(knot2, to, null);
        pad.AddEdge(knot2, knot1, null);
        child.ToNFA(pad, knot1, knot2);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "plus:");
        child.Dump(indent + 4);
    }
}

public class LADSequence : LAD {
    public readonly LAD[] args;
    public LADSequence(LAD[] args) { this.args = args; }

    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        int i = 0;
        len = 0; cont = true;
        while (cont && i < args.Length) {
            int l1 = len;
            args[i].QueryLiteral(pad, out len, out cont);
            len += l1;
            i++;
        }
    }

    public override void ToNFA(NFA pad, int from, int to) {
        for (int i = 0; i < args.Length; i++) {
            int knot = (i == args.Length - 1) ? to : pad.AddNode();
            args[i].ToNFA(pad, from, knot);
            from = knot;
        }
        if (from != to) pad.AddEdge(from, to, null);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "seq:");
        foreach (LAD l in args)
            l.Dump(indent + 4);
    }
}

public class LADAny : LAD {
    public readonly LAD[] zyg;
    public LADAny(LAD[] zyg) { this.zyg = zyg; }

    public override void ToNFA(NFA pad, int from, int to) {
        foreach (LAD k in zyg)
            k.ToNFA(pad, from, to);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "any:");
        foreach (LAD k in zyg)
            k.Dump(indent + 4);
    }
}

public class LADImp : LAD {
    public override void ToNFA(NFA pad, int from, int to) {
        int knot = pad.AddNode();
        pad.nodes[knot].final = true;
        pad.AddEdge(from, knot, null);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "imp");
    }
}

public class LADNull : LAD {
    public override void ToNFA(NFA pad, int from, int to) {
        pad.AddEdge(from, to, null);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "null");
    }

    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        len = 0; cont = true;
    }
}

public class LADNone : LAD {
    public override void ToNFA(NFA pad, int from, int to) {
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "none");
    }
}

public class LADDot : LAD {
    public override void ToNFA(NFA pad, int from, int to) {
        pad.AddEdge(from, to, CC.All);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "dot");
    }
}

public class LADMethod : LAD {
    public readonly string name;

    public LADMethod(string name) { this.name = name; }

    public override void ToNFA(NFA pad, int from, int to) {
        if (Lexer.LtmTrace)
            Console.WriteLine("+ Processing subrule {0}", name);

        if (pad.method_stack.Contains(name)) {
            // NFAs cannot be recursive, so treat this as the end of the
            // declarative prefix.
            if (Lexer.LtmTrace)
                Console.WriteLine("+ Pruning to avoid recursion");
            int knot = pad.AddNode();
            pad.AddEdge(from, knot, null);
            pad.nodes[knot].final = true;
            return;
        }

        pad.method_stack.Add(name);

        LAD sub = pad.ResolveMethod(name);
        if (sub == null) {
            int knot = pad.AddNode();
            pad.AddEdge(from, knot, null);
            pad.nodes[knot].final = true;
        } else {
            sub.ToNFA(pad, from, to);
        }

        pad.method_stack.Remove(name);
    }

    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        LAD sub = pad.ResolveMethod(name);

        if (pad.method_stack.Contains(name)) {
            len = 0; cont = false;
        } else {
            pad.method_stack.Add(name);
            sub.QueryLiteral(pad, out len, out cont);
            pad.method_stack.Remove(name);
        }
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "methodcall " + name);
    }
}

public class LADProtoRegex : LAD {
    public readonly string name;

    public LADProtoRegex(string name) { this.name = name; }

    public override void ToNFA(NFA pad, int from, int to) {
        foreach (DynObject cand in Lexer.ResolveProtoregex(pad.cursor_class, name)) {
            ((SubInfo)cand.GetSlot("info")).ltm.ToNFA(pad, from, to);
        }
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "protorx " + name);
    }
}
// These objects get put in hash tables, so don't change nstates[] after
// that happens
public class LexerState {
    public int[] nstates;
    public readonly Lexer parent;
    public LexerState(Lexer parent) {
        this.parent = parent;
        this.nstates = new int[(parent.pad.nodes.Count + 31) >> 5];
    }

    public bool alive;

    // But these cachey fields are fair game
    // note there will be no epsilons here
    public List<NFA.Edge> alledges = new List<NFA.Edge>();

    public override int GetHashCode() {
        int o = 0;
        for (int i = 0; i < nstates.Length; i++)
            o = o * 1342883 + nstates[i];
        return o;
    }

    public void AddNFAState(int num) {
        Stack<int> grey = new Stack<int>();
        grey.Push(num);
        alive = true;
        while (grey.Count != 0) {
            int val = grey.Pop();
            int vm  = 1 << (val & 31);
            if ((nstates[val >> 5] & vm) != 0)
                continue;
            nstates[val >> 5] |= vm;
            foreach (NFA.Edge e in parent.pad.nodes[val].edges) {
                if (e.when == null)
                    grey.Push(e.to);
                else
                    alledges.Add(e);
            }
        }
    }

    public void CollectFates(Stack<int> f) {
        for (int i = parent.pad.nodes.Count - 1; i >= 0; i--) {
            if ((nstates[i >> 5] & (1 << (i & 31))) != 0) {
                NFA.Node n = parent.pad.nodes[i];
                if (n.final) {
                    if (Lexer.LtmTrace)
                        Console.WriteLine("+ Adding fate {0}", n.fate);
                    f.Push(n.fate);
                }
            }
        }
    }

    public override string ToString() {
        List<int> li = new List<int>();

        for (int i = 0; i < nstates.Length; i++)
            for (int j = 0; j < 32; j++) {
                if ((nstates[i] & (1 << j)) == 0)
                    continue;
                li.Add(32*i + j);
            }

        return Kernel.JoinS("|", li);
    }
}

public class LexerCache {
    public Dictionary<LAD[], Lexer> nfas = new Dictionary<LAD[], Lexer>();
    public Dictionary<string, DynObject[]> protorx_fns =
        new Dictionary<string, DynObject[]>();
    public Dictionary<string, Lexer> protorx_nfa =
        new Dictionary<string, Lexer>();
}

public class Lexer {
    public LAD[] alts;
    public NFA pad = new NFA();
    public string tag;

    public static bool LtmTrace =
        Environment.GetEnvironmentVariable("NIECZA_LTM_TRACE") != null;

    public static Lexer GetLexer(IP6 cursor, LAD[] lads, string title) {
        LexerCache lc = cursor.GetMO().GetLexerCache();
        Lexer ret;
        if (lc.nfas.TryGetValue(lads, out ret))
            return ret;
        ret = new Lexer(cursor.GetMO(), title, lads);
        lc.nfas[lads] = ret;
        return ret;
    }

    public Lexer(DynMetaObject cmo, string tag, LAD[] alts) {
        pad.cursor_class = cmo;
        this.alts = alts;
        this.tag = tag;
        int root = pad.AddNode();
        int[] alt_shuffle = new int[alts.Length];
        for (int i = 0; i < alts.Length; i++) alt_shuffle[i] = i;
        Array.Sort(alt_shuffle, delegate (int i1, int i2) {
            int j1, j2;
            bool c1, c2;
            alts[i1].QueryLiteral(pad, out j1, out c1);
            alts[i2].QueryLiteral(pad, out j2, out c2);
            return (j1 != j2) ? (j2 - j1) : (i1 - i2);
        });
        for (int ix = 0; ix < alts.Length; ix++) {
            pad.curfate = alt_shuffle[ix];
            int target = pad.AddNode();
            pad.nodes[target].final = true;
            alts[alt_shuffle[ix]].ToNFA(pad, root, target);
        }
        // now the NFA nodes are all in tiebreak order by lowest index
        if (LtmTrace) {
            Dump();
        }
    }

    public void Dump() {
        Console.WriteLine("--- LEXER ({0}) : Tree", tag);
        for (int ix = 0; ix < alts.Length; ix++) {
            Console.WriteLine("{0}:", ix);
            alts[ix].Dump(0);
        }
        Console.WriteLine("--- NFA:");
        pad.Dump();
        Console.WriteLine("--- END");
    }


    public int[] Run(string from, int pos) {
        LexerState state = new LexerState(this);
        state.AddNFAState(0);

        Stack<int> fate = new Stack<int>();

        if (LtmTrace)
            Console.WriteLine("+ Trying lexer {0} at {1}", tag, pos);

        while (true) {
            state.CollectFates(fate);

            if (pos == from.Length || !state.alive) break;
            char ch = from[pos++];

            if (LtmTrace)
                Console.WriteLine("+ Adding character {0}", ch);

            LexerState next = new LexerState(this);

            foreach (NFA.Edge e in state.alledges) {
                if (!e.when.Accepts(ch)) continue;
                next.AddNFAState(e.to);
            }

            if (LtmTrace)
                Console.WriteLine("+ Changing state to {0}", next);

            state = next;
        }

        List<int> uniqfates = new List<int>();
        HashSet<int> usedfates = new HashSet<int>();

        while (fate.Count != 0) {
            int f = fate.Pop();
            if (usedfates.Contains(f))
                continue;
            usedfates.Add(f);
            if (LtmTrace)
                Console.WriteLine("+ Useful fate: {0}", f);
            uniqfates.Add(f);
        }

        return uniqfates.ToArray();
    }

    public static IP6[] RunProtoregex(IP6 cursor, string name) {
        DynMetaObject kl = cursor.GetMO();
        LexerCache lc = kl.GetLexerCache();
        DynObject[] candidates = ResolveProtoregex(kl, name);
        Lexer l;
        if (!lc.protorx_nfa.TryGetValue(name, out l)) {
            if (LtmTrace)
                Console.WriteLine("+ Protoregex lexer MISS on {0}.{1}",
                        kl.name, name);
            LAD[] branches = new LAD[candidates.Length];
            for (int i = 0; i < candidates.Length; i++)
                branches[i] = ((SubInfo) candidates[i].GetSlot("info")).ltm;
            lc.protorx_nfa[name] = l = new Lexer(cursor.GetMO(), name, branches);
        } else {
            if (LtmTrace)
                Console.WriteLine("+ Protoregex lexer HIT on {0}.{1}",
                        kl.name, name);
        }
        Cursor c = (Cursor)cursor;
        int[] brnum = l.Run(c.backing, c.pos);
        IP6[] ret = new IP6[brnum.Length];
        for (int i = 0; i < brnum.Length; i++)
            ret[i] = candidates[brnum[i]];
        return ret;
    }

    public static DynObject[] ResolveProtoregex(DynMetaObject cursor_class,
            string name) {
        DynObject[] ret;
        if (cursor_class.GetLexerCache().
                protorx_fns.TryGetValue(name, out ret)) {
            if (LtmTrace)
                Console.WriteLine("+ Protoregex method list HIT on {0}.{1}",
                        cursor_class.name, name);
            return ret;
        }
        if (LtmTrace)
            Console.WriteLine("+ Protoregex method list MISS on {0}.{1}",
                    cursor_class.name, name);

        IP6 proto = cursor_class.Can(name);

        List<DynObject> raword = new List<DynObject>();

        foreach (DynMetaObject k in cursor_class.mro) {
            if (proto != k.Can(name))
                continue;
            if (k.multiregex == null)
                continue;
            List<DynObject> locord;
            if (k.multiregex.TryGetValue(name, out locord))
                foreach (DynObject o in locord)
                    raword.Add(o);
        }

        HashSet<IP6> unshadowed = cursor_class.AllMethodsSet();
        List<DynObject> useord = new List<DynObject>();
        foreach (DynObject o in raword)
            if (unshadowed.Contains(o))
                useord.Add(o);

        ret = useord.ToArray();
        cursor_class.GetLexerCache().protorx_fns[name] = ret;
        return ret;
    }
}
