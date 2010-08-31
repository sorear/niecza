using Niecza;
using System;
using System.Collections.Generic;
using System.Text;
// this exists to allow O(1) addition, since additions (esp. in the presence
// of backtracking) dominate lookups

public class Matched {
    public Matched  next;
    public string   name;
    public Variable val; // or null for a list-mode sentinel

    public Matched(Matched next, string name, Variable val) {
        this.next = next;
        this.name = name;
        this.val = val;
    }
}
//
//public class Match {
//    public string backing;
//    public int from;
//    public int to;
//    public Dictionary<string,Variable> captures;
//}
//
//public class Xact {
//    

// extends Frame for a time/space tradeoff
// we keep the cursor in exploded form to avoid creating lots and lots of
// cursor objects
public sealed class RxFrame {
    public sealed class PSN<X> {
        public X obj;
        public readonly PSN<X> next;
        public PSN(X obj, PSN<X> next) { this.obj = obj; this.next = next; }
    }

    public struct State {
        public PSN<int> reps;
        public PSN<IP6> captures;
        public PSN<DynMetaObject> klasses;

        public int pos;
        public int ip;

        public XAct xact;
    }
    // invariant: xact of top state is NEVER committed/pruned

    public PSN<State> bt;

    // our backing string, in a cheap to index form
    public char[] orig;
    // cache of orig.Length
    public int end;

    public RxFrame(DynObject csr) {
        Cursor c = (Cursor) Kernel.UnboxDO(csr);
        orig = c.backing.ToCharArray();
        end = orig.Length;
        bt = new PSN<State>(default(State), null);
        bt.obj.klasses = new PSN<DynMetaObject>(csr.klass, null);
        bt.obj.xact = new XAct("MATCH", null);
        bt.obj.pos = c.pos;
    }

    public Frame Backtrack(Frame th) {
        do {
            bt = bt.next;
        } while (bt != null && (bt.obj.xact.committed || bt.obj.ip < 0));
        if (bt == null) {
            return th.caller;
        } else {
            th.ip = bt.obj.ip;
            return th;
        }
    }

    public void PushBacktrack(string name, int ip) {
        bt.obj.ip = ip;
        bt = new PSN<State>(bt.obj, bt);
        bt.obj.xact = new XAct(name, bt.obj.xact);
    }

    public void CommitAll() {
        XAct x = bt.next.obj.xact;
        while (x != null) {
            x.committed = true;
            x = x.next;
        }
    }

    public void CommitSpecificRule(string name) {
        XAct x = bt.next.obj.xact;
        name = "RULE " + name;
        while (x != null) {
            x.committed = true;
            x = x.next;
            if (x.tag.Equals(name))
                break;
        }
    }

    public void CommitRule() {
        XAct x = bt.next.obj.xact;
        while (x != null) {
            x.committed = true;
            x = x.next;
            if (x.tag.StartsWith("RULE "))
                break;
        }
    }

    public Frame Exact(Frame th, string st) {
        if (bt.obj.pos + st.Length > end)
            return Backtrack(th);
        foreach (char ch in st)
            if (orig[bt.obj.pos++] != ch)
                return Backtrack(th);
        return th;
    }

    public Frame ExactOne(Frame th, char ch) {
        if (bt.obj.pos == end || orig[bt.obj.pos++] != ch)
            return Backtrack(th);
        return th;
    }

    public void OpenQuant() {
        bt.obj.reps = new PSN<int>(0, bt.obj.reps);
    }

    public int CloseQuant() {
        int x = bt.obj.reps.obj;
        bt.obj.reps = bt.obj.reps.next;
        return x;
    }

    public void IncQuant() {
        bt.obj.reps.obj++;
    }

    public int GetQuant() {
        return bt.obj.reps.obj;
    }
}

public sealed class XAct {
    public readonly string tag;
    public bool committed;
    public readonly XAct next;

    public XAct(string tag, XAct next) { this.tag = tag; this.next = next; }
}

public class Cursor {
    // XXX It's a bit wrong that we ref the string both from the cursor and
    // from $*ORIG.
    public Matched captures;
    public string backing;
    public XAct xact;
    public int pos;

    public static bool Trace =
        Environment.GetEnvironmentVariable("NIECZA_RX_TRACE") != null;

    public Cursor(Matched captures, string backing, int pos) {
        this.captures = captures;
        this.backing = backing;
        this.pos = pos;
    }

    public Cursor(string backing) : this(null, backing, 0) { }

    public Cursor At(int npos) {
        return new Cursor(captures, backing, npos);
    }

    public Cursor Exact(string what) {
        if (backing.Length - what.Length >= pos &&
                backing.Substring(pos, what.Length) == what) {
            if (Trace)
                Console.WriteLine("* matched {0} at {1}", what, pos);
            return At(pos + what.Length);
        } else {
            if (Trace)
                Console.WriteLine("! no match {0} at {1}", what, pos);
            return null;
        }
    }

    public Cursor AnyChar() {
        if (backing.Length - 1 >= pos) {
            if (Trace)
                Console.WriteLine("* matched any char at {0}", pos);
            return At(pos + 1);
        } else {
            if (Trace)
                Console.WriteLine("! no match any char at {0}", pos);
            return null;
        }
    }

    public Cursor CClass(CC cc) {
        if (backing.Length - 1 >= pos && cc.Accepts(backing[pos])) {
            if (Trace)
                Console.WriteLine("* matched cc {0} at {1}", cc, pos);
            return At(pos + 1);
        } else {
            if (Trace)
                Console.WriteLine("! no match cc {0} at {1}", cc, pos);
            return null;
        }
    }

    public Cursor SetCaps(Matched caps) {
        return new Cursor(caps, backing, pos);
    }

    public Cursor Bind(string name, Variable what) {
        return SetCaps(new Matched(captures, name, what));
    }

    public Cursor SimpleWS() {
        int l = backing.Length;
        int p = pos;
        if (p != 0 && p != l && CC.Word.Accepts(backing[p]) &&
                CC.Word.Accepts(backing[p-1])) {
            if (Trace)
                Console.WriteLine("! no match <ws> at {0}", pos);
            return null;
        }

        while (p != l && Char.IsWhiteSpace(backing, p)) { p++; }
        if (Trace)
            Console.WriteLine("* match <ws> at {0} to {1}", pos, p);

        return At(p);
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

        sub = ((SubInfo)(((DynObject)method).slots["info"])).ltm;

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
    public readonly LAD fst;
    public readonly LAD snd;
    public LADSequence(LAD fst, LAD snd) { this.fst = fst; this.snd = snd; }

    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        fst.QueryLiteral(pad, out len, out cont);
        if (cont) {
            int l1 = len;
            snd.QueryLiteral(pad, out len, out cont);
            len += l1;
        }
    }

    public override void ToNFA(NFA pad, int from, int to) {
        int knot = pad.AddNode();
        fst.ToNFA(pad, from, knot);
        snd.ToNFA(pad, knot, to);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "seq:");
        fst.Dump(indent + 4);
        snd.Dump(indent + 4);
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
            ((SubInfo)cand.slots["info"]).ltm.ToNFA(pad, from, to);
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
        this.nstates = new int[parent.pad.nodes.Count];
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

public class Lexer {
    public LAD[] alts;
    public NFA pad = new NFA();
    public string tag;

    public static bool LtmTrace =
        Environment.GetEnvironmentVariable("NIECZA_LTM_TRACE") != null;

    public Lexer(IP6 cursorObj, string tag, LAD[] alts) {
        pad.cursor_class = ((DynObject)cursorObj).klass;
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
        DynObject dc = (DynObject)cursor;
        DynObject[] candidates = ResolveProtoregex(dc.klass, name);
        LAD[] branches = new LAD[candidates.Length];
        for (int i = 0; i < candidates.Length; i++)
            branches[i] = ((SubInfo) candidates[i].slots["info"]).ltm;
        Lexer l = new Lexer(dc, name, branches);
        Cursor c = (Cursor)Kernel.UnboxAny(cursor);
        int[] brnum = l.Run(c.backing, c.pos);
        IP6[] ret = new IP6[brnum.Length];
        for (int i = 0; i < brnum.Length; i++)
            ret[i] = candidates[brnum[i]];
        return ret;
    }

    public static DynObject[] ResolveProtoregex(DynMetaObject cursor_class,
            string name) {
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

        return useord.ToArray();
    }
}
