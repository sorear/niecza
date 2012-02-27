using Niecza;
using Niecza.Serialization;
using System;
using System.Collections.Generic;
using System.Text;

// global stuff
public sealed class GState {
    public char[] orig_a;
    public string orig_s;

    public int highwater;

    public Variable actions;

    internal Frame CallAction(Frame th, string name, Cursor match) {
        if (actions == null || name == "" || name == null)
            return th;
        if (Cursor.Trace)
            Console.WriteLine("To call action {0}", name);
        DispatchEnt m;
        P6any actions_p = actions.Fetch();
        Variable[] pos;
        if (actions_p.mo.mro_methods.TryGetValue(name, out m)) {
            pos = new Variable[] { actions, Kernel.NewROScalar(match) };
        } else if (actions_p.mo.mro_methods.TryGetValue("FALLBACK", out m)) {
            pos = new Variable[] { actions,
                Kernel.BoxAnyMO<string>(name, Kernel.StrMO),
                Kernel.NewROScalar(match) };
        } else {
            return th;
        }

        return m.info.SetupCall(th, m.outer, m.ip6, pos, null, false, m);
    }

    public GState(string orig, P6any actions) {
        this.actions = (actions == Kernel.AnyP) ? null :
            Kernel.NewROScalar(actions);
        orig_s = orig;
        orig_a = orig.ToCharArray();
        highwater = (orig_a.Length < 100 || !Cursor.HwTrace) ?
            int.MaxValue : 0;
    }
    public static readonly string BailAt =
        Environment.GetEnvironmentVariable("NIECZA_DIE_AT_PCT");
    public static readonly int BailAtI;
    static GState() {
        if (!int.TryParse(BailAt, out BailAtI))
            BailAtI = 101;
    }

    public void IncHighwater(int to) {
        int pct = to * 100 / orig_a.Length;
        // cheat a bit: don't report highwater again until pct increases
        // (pct+1) <= (new-to) * 100 / len
        // (pct+1) * len <= (new-to) * 100
        // ceil(((pct+1) * len) / 100) <= new-to
        highwater = ((pct + 1) * orig_a.Length + 99) / 100;
        Console.WriteLine("Parsed {0}% of {1} characters", pct, orig_a.Length);
        if (pct == BailAtI)
            throw new Exception("Backtrace at percent");
    }
}

// stuff that should 'N'est, like subrules do
public sealed class NState {
    public NState next;
    public string name;
    public Choice cut_to;
    public int quant;
    public STable klass;

    public NState(Choice cut_to, string name, NState proto) {
        next = proto; this.cut_to = cut_to; this.name = name;
        if (proto != null) klass = proto.klass;
    }

    public NState(NState f) {
        next = f.next; name = f.name; cut_to = f.cut_to;
        quant = f.quant; klass = f.klass;
    }
}

public sealed class CapInfo {
    public CapInfo prev;
    public string[] names;
    public Variable cap;

    public CapInfo(CapInfo prev, string[] names, Variable cap) {
        this.prev = prev; this.names = names; this.cap = cap;
    }
}

public struct State {
    public CapInfo captures;
    public NState ns;

    public object subrule_iter;
    public int pos;
}

public sealed class Choice {
    public Choice prev;
    public State st;

    // the current Choice doesn't really have any of these fields, which are
    // used for backtrack control
    public int ip;

    public void Commit() {
        // we aren't going to backtrack here, so we can't use this.
        // help out the GC
        if (ip > 0 || ip < -10) {
            st.subrule_iter = null;
        }
        ip = (ip < 0) ? ip : (-ip-10);
    }

    public Choice(Choice prev, int ip, State st) {
        this.ip = ip;
        this.st = st;
        this.prev = prev;
    }
}

public class AltInfo : IFreeze {
    public int[] labels;
    public LAD[] prefixes;
    public string dba;
    //public readonly STable in_class;

    public AltInfo(LAD[] prefixes, string dba, int[] labels) {
        this.labels = labels;
        this.prefixes = prefixes;
        this.dba = dba;
    }

    private AltInfo() { }
    void IFreeze.Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.AltInfo);
        fb.Ints(labels);
        fb.Refs(prefixes);
        fb.String(dba);
    }

    internal static AltInfo Thaw(ThawBuffer tb) {
        var n = new AltInfo();
        tb.Register(n);
        n.labels = tb.Ints();
        n.prefixes = tb.RefsA<LAD>();
        n.dba = tb.String();
        return n;
    }
}

// extends Frame for a time/space tradeoff
// we keep the cursor in exploded form to avoid creating lots and lots of
// cursor objects
public sealed class RxFrame: IFreeze {
    public Choice bt;
    public State st;

    // when this is set, one value has already been given, so we don't need
    // any more Lists
    public const int RETURN_ONE = 1;
    // <( and )> have been used; MakeMatch needs to search for the end
    // tokens
    public const int USED_ENDS = 2;
    public byte flags;

    // .from in matches
    public int from;

    // these go into the match but currently aren't scoped by backtrack
    // control in any way
    public P6any ast;

    public GState global;
    // our backing string, in a cheap to index form
    public char[] orig;
    // cache of orig.Length
    public int end;
    public string name;

    // don't remove this on backtracking, and quit if we would back into it
    public readonly Choice rootf;

    public RxFrame(string name, Cursor csr, bool passcut) {
        global = csr.global;
        orig = global.orig_a;
        end = orig.Length;
        rootf = bt = csr.xact;
        this.name = name;
        st.ns = passcut ? csr.nstate :
            new NState(rootf, name, csr.nstate);
        if (Cursor.Trace) {
            Console.WriteLine("Entering subrule {0} at {1}/{2}", name, csr.pos,
                    end);
        }
        st.ns.klass = csr.mo;
        st.pos = csr.pos;
        from = csr.pos;
    }

    public Frame Backtrack(Frame th) {
        // throw away cut or mark-only frames
        while (bt != rootf && (bt.ip < 0)) {
            if (bt.ip == -1) {
                // Special frame that does $*GOAL cleanup ...
                th.LexicalBind("$*GOAL", (Variable)bt.st.subrule_iter);
            }
            bt = bt.prev;
        }
        if (st.pos > global.highwater)
            global.IncHighwater(st.pos);
        if (bt == rootf) {
            if ((flags & RETURN_ONE) != 0) {
                if (Cursor.Trace)
                    Console.WriteLine("Failing {0}@{1} after no matches",
                            name, from);
                return Kernel.Take(th, Kernel.NewROScalar(Kernel.EMPTYP));
            } else {
                if (Cursor.Trace)
                    Console.WriteLine("Failing {0}@{1} after some matches",
                            name, from);
                if (EmptyList == null) {
                    P6opaque lst = new P6opaque(Kernel.ListMO);
                    lst.slots[0 /*items*/] = new VarDeque();
                    lst.slots[1 /*rest*/ ] = new VarDeque();
                    EmptyList = Kernel.NewRWListVar(lst);
                }
                th.caller.resultSlot = EmptyList;
            }

            return th.Return();
        } else {
            th.ip = bt.ip;
            st = bt.st;
            bt = bt.prev;
            return th;
        }
    }

    public void PushBacktrack(int ip) {
        bt = new Choice(bt, ip, st);
    }

    public void PushCapture(string[] cn, Variable cl) {
        if (cn.Length == 0) return;
        st.captures = new CapInfo(st.captures, cn, cl);
    }

    public void SetCursorList(object cl) {
        st.subrule_iter = cl;
    }

    public void InitCursorList(Variable obj) {
        st.subrule_iter = Builtins.start_iter(obj);
    }

    public Variable GetCursorList() {
        return (Variable)st.subrule_iter;
    }

    public VarDeque GetCursorIter() {
        return (VarDeque)st.subrule_iter;
    }

    public void SetPos(int pos) {
        st.pos = pos;
    }

    // This is not the most efficient way, but it avoids adding another
    // field to the backtrack state.
    public void SetEndpoint(string which) {
        flags |= USED_ENDS;
        PushCapture(new string[] { null, which }, Builtins.MakeInt(st.pos));
    }

    public void IncorporateChild(string[] names, P6any match) {
        Cursor child = match as Cursor;

        if (child == null)
            throw new NieczaException((names.Length == 0 ?
                "Anonymous submatch" : "Submatch to be bound to " + names[0]) +
                    " returned a " + match.mo.name + " instead of a Cursor, " +
                    "violating the submatch protocol.");

        SetPos(child.pos);

        if (names != null)
            PushCapture(names, Kernel.NewROScalar(child));
    }

    public const int IC_ZERO_WIDTH = 1;
    public const int IC_NEGATIVE = 2;

    public bool IncorpCut(string[] names, int mode, Variable list) {
        Variable match_v = Kernel.GetFirst(list);
        P6any match = match_v.Fetch();
        if (match.IsDefined() == ((mode & IC_NEGATIVE) != 0))
            return false;
        if ((mode & IC_ZERO_WIDTH) == 0)
            IncorporateChild(names, match);
        return true;
    }

    public bool IncorpShift(string[] names, int label) {
        if (!Kernel.IterHasFlat(GetCursorIter(), true))
            return false;
        PushBacktrack(label);
        IncorporateChild(names, GetCursorIter().Shift().Fetch());
        SetCursorList(null);
        return true;
    }

    public int GetPos() {
        return st.pos;
    }

    public void PushCutGroup(string tag) {
        st.ns = new NState(bt, tag, st.ns);
    }

    public void PopCutGroup() {
        st.ns = st.ns.next;
    }

    public void CommitAll() {
        Choice x = bt;
        while (x != null) {
            x.Commit();
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

    public void CommitGroup(string id) {
        NState nsi = st.ns;
        while (nsi != null && !nsi.name.Equals(id))
            nsi = nsi.next;
        Choice to = (nsi != null) ? nsi.cut_to : null;
        Choice it = bt;
        while (it != to) {
            it.Commit();
            it = it.prev;
        }
    }

    public void PushGoal(Frame th, string newGoal) {
        bt = new Choice(bt, -1, st);
        bt.st.subrule_iter = th.LexicalFind("$*GOAL");
        th.LexicalBind("$*GOAL", Builtins.MakeStr(newGoal));
        st.ns = new NState(bt, "GOAL", st.ns);
        st.ns.quant = st.pos;
    }

    public void PopGoal(Frame th) {
        bt = new Choice(bt, -1, st);
        bt.st.subrule_iter = th.LexicalFind("$*GOAL");

        th.LexicalBind("$*GOAL", (Variable)st.ns.cut_to.st.subrule_iter);
        st.ns = st.ns.next;
    }

    public bool Exact(string str) {
        if (st.pos + str.Length > end)
            return false;
        foreach (char ch in str)
            if (orig[st.pos++] != ch)
                return false;
        return true;
    }

    public bool ExactNoCase(string str) {
        if (st.pos + str.Length > end)
            return false;
        foreach (char ch in str)
            if (!CaselessEq(orig[st.pos++], ch))
                return false;
        return true;
    }

    // this is wrong, but it's good enough for now XXX
    private static bool CaselessEq(char src, char tst) {
        return src == char.ToLowerInvariant(tst) ||
            src == char.ToUpperInvariant(tst);
    }

    public bool ExactOne(char ch) {
        return !(st.pos == end || orig[st.pos++] != ch);
    }

    public bool ExactOneNoCase(char ch) {
        return !(st.pos == end || !CaselessEq(orig[st.pos++], ch));
    }

    public bool AnyChar() {
        if (st.pos++ == end) return false;
        if (((uint)(orig[st.pos-1] - 0xD800)) < 0x400u && st.pos != end)
            st.pos++; // skip low surrogate; we assume well-formed input
        return true;
    }

    public bool CClass(CC x) {
        if (st.pos++ == end) return false;
        int ho = orig[st.pos-1];
        int hs = ho - 0xD800;
        if (((uint)hs) < 0x400u && st.pos != end) {
            st.pos++;
            return x.Accepts(0x10000 - 0xDC00 + orig[st.pos-1] + 0x400 * hs);
        } else {
            return x.Accepts(ho);
        }
    }

    public bool Newline() {
        if (st.pos == end) return false;
        char next = orig[st.pos++];
        if (next == '\r' && st.pos != end && orig[st.pos] == '\n') {
            st.pos++;
            return true;
        }
        return CC.VSpace.Accepts(next);
    }

    public bool ZeroWidth(int type) {
        int next = (st.pos == end) ? -1 : orig[st.pos];
        int prev = (st.pos == 0) ? -1 : orig[st.pos-1];
        bool crlf = (prev == '\r' && next == '\n');
        switch(type) {
            case 0:
                return (next >= 0) && CC.Word.Accepts((char)next) &&
                    ((prev < 0) || !CC.Word.Accepts((char)prev));
            case 1:
                return (prev >= 0) && CC.Word.Accepts((char)prev) &&
                    ((next < 0) || !CC.Word.Accepts((char)next));
            case 2:
                return (prev == -1);
            case 3:
                return (next == -1);
            case 4:
                return (prev == -1 ||
                        (!crlf && CC.VSpace.Accepts(prev) && next >= 0));
            case 5:
                return (CC.VSpace.Accepts(next) && !crlf) ||
                    (next == -1 && !CC.VSpace.Accepts(prev));
            default:
                return false;
        }
    }

    public bool BeforeStr(bool not, string tx) {
        bool ok = st.pos <= end - tx.Length;
        for (int ix = 0; ix < tx.Length && ok; ix++)
            ok = orig[st.pos + ix] == tx[ix];
        return (not != ok);
    }

    public bool AfterCCs(bool neg, CC[] vec) {
        int offs = st.pos;
        for (int i = vec.Length - 1; i >= 0; i--) {
            if (offs == 0)
                return neg;
            int ch = orig[--offs];
            if (((uint)(ch - 0xDC00)) < 0x400u && offs != 0) {
                ch = orig[--offs] * 0x400 + ch +
                   (0x10000 - (0xD800 * 0x400 + 0xDC00));
            }
            if (!vec[i].Accepts(ch))
                return neg;
        }
        return !neg;
    }

    public bool BeforeCCs(bool neg, CC[] vec) {
        int offs = st.pos;
        int upto = end;
        for (int i = 0; i < vec.Length; i++) {
            if (offs == upto) return neg;
            int ch = orig[offs++];
            if (((uint)(ch - 0xD800)) < 0x400u && offs != upto) {
                ch = orig[offs++] + ch * 0x400 +
                   (0x10000 - (0xD800 * 0x400 + 0xDC00));
            }
            if (!vec[i].Accepts(ch))
                return neg;
        }
        return !neg;
    }

    public bool ScanCClass(int min, int max, CC x) {
        int i = 0;
        int at = st.pos;
        int upto = end;

        while (i < max && at < upto) {
            int oat = at;
            int ch = orig[at++];
            if (((uint)(ch - 0xD800)) < 0x400u && at != upto) {
                ch = orig[at++] + ch * 0x400 +
                   (0x10000 - (0xD800 * 0x400 + 0xDC00));
            }
            if (x.Accepts(ch)) {
                i++;
            } else {
                at = oat;
                break;
            }
        }

        st.pos = at;

        return (i >= min);
    }

    public void LTMPushAlts(Frame fr, AltInfo ai) {
        Lexer lx = Lexer.GetLexer(fr, GetClass(), ai.prefixes, ai.dba);
        if (st.pos > global.highwater)
            global.IncHighwater(st.pos);
        PushCutGroup("LTM");
        int[] cases = lx.Run(global.orig_s, st.pos);
        for (int i = cases.Length - 1; i >= 0; i--) {
            PushBacktrack(ai.labels[cases[i]]);
        }
    }

    public Frame scalar_var(Frame th, Variable var) { return scalar_common(false, th, var); }
    public Frame scalar_asn(Frame th, Variable var) { return scalar_common(true, th, var); }

    Frame scalar_common(bool eval, Frame th, Variable var) {
        P6any o = var.Fetch();
        if (o.Isa(Kernel.RegexMO)) {
            return o.Invoke(th, new Variable[] { MakeCursorV() }, null);
        } else if (eval) {
            P6any rx = Builtins.compile_bind_regex(th,
                    o.mo.mro_raw_Str.Get(var));
            return rx.Invoke(th, new Variable[] { MakeCursorV() }, null);
        } else {
            if (Exact(o.mo.mro_raw_Str.Get(var))) {
                th.resultSlot = MakeCursorV();
            } else {
                th.resultSlot = Kernel.NilP.mo.typeVar;
            }
            return th;
        }
    }

    public Frame list_var(Frame th, Variable var) { return list_common(false, th, var); }
    public Frame list_asn(Frame th, Variable var) { return list_common(true, th, var); }
    public Frame list_common(bool eval, Frame th, Variable var) {
        VarDeque iter = Builtins.start_iter(var);
        List<object> toks = new List<object>();
        List<LAD> lads = new List<LAD>();

        NFA pad = new NFA();
        pad.cursor_class = st.ns.klass;

        while (Kernel.IterHasFlat(iter, true)) {
            Variable svar = iter.Shift();
            P6any sobj = svar.Fetch();

retry:
            if (sobj.Isa(Kernel.RegexMO)) {
                toks.Add(sobj);

                pad.outer_stack.Add(Kernel.GetOuter(sobj));
                pad.info_stack.Add(Kernel.GetInfo(sobj));
                lads.Add(pad.info_stack[0].ltm.Reify(pad));
                pad.outer_stack.Clear();
                pad.info_stack.Clear();
            } else if (eval) {
                sobj = Builtins.compile_bind_regex(th,
                        sobj.mo.mro_raw_Str.Get(svar));
                svar = Kernel.NewROScalar(sobj);
                goto retry;
            } else {
                string str = sobj.mo.mro_raw_Str.Get(svar);
                toks.Add(str);
                lads.Add(new LADStr(str));
            }
        }

        int[] cases = (new Lexer(pad, "array_var", lads.ToArray())).
            Run(global.orig_s, st.pos);

        Frame nth = th.MakeChild(null, ArrayHelperSI, Kernel.AnyP);
        nth.lex0 = MakeCursor();
        nth.lex1 = toks;
        nth.lex2 = cases;

        return nth;
    }

    public Frame proto_dispatch(Frame th, Variable unused) {
        Frame nth = th.MakeChild(null, Lexer.StandardProtoSI, Kernel.AnyP);
        nth.pos = new Variable[] { MakeCursorV() };
        return nth;
    }

    private static SubInfo ArrayHelperSI = new SubInfo("KERNEL ArrayHelper", ArrayHelperC);
    private static Frame ArrayHelperC(Frame th) {
        int[] cases = (int[]) th.lex2;
        List<object> toks = (List<object>) th.lex1;
        object tok;
        switch (th.ip) {
            default:
                return Kernel.Die(th, "Invalid IP");
            case 1:
                return th.rx.Backtrack(th);
            case 0:
                th.rx = new RxFrame("ArrayHelper", (Cursor) th.lex0, false);
                th.lexi0 = 0;
                goto case 2;
            case 2:
                if (th.lexi0 == cases.Length)
                    goto case 1;
                th.rx.PushBacktrack(2);
                tok = toks[cases[th.lexi0++]];
                if (tok is string) {
                    if (!th.rx.Exact((string)tok))
                        goto case 1;
                    th.ip = 5;
                    return th.rx.MakeMatch(th);
                } else {
                    th.ip = 3;
                    return ((P6any)tok).Invoke(th, new Variable[] {
                            th.rx.MakeCursorV() }, null);
                }
            case 3:
                th.lex2 = Builtins.start_iter((Variable) th.resultSlot);
                goto case 4;
            case 4:
                if (!Kernel.IterHasFlat((VarDeque)th.lex2, true))
                    goto case 1;
                th.ip = 4;
                return th.rx.EndWith(th, (Cursor) ((VarDeque)th.lex2).Shift().Fetch());
            case 5:
                th.ip = 1;
                return th.rx.End(th);
        }
    }
    public void PushConjStart() {
        st.ns = new NState(bt, "CSTART", st.ns);
        st.ns.quant = st.pos;
    }

    public void PushConjEnd() {
        st.ns = new NState(bt, "CEND", st.ns);
        st.ns.quant = st.pos;
    }

    public void GotoConjStart() {
        st.pos = st.ns.next.quant;
    }

    public bool CheckConjEnd() {
        return (st.pos == st.ns.quant);
    }

    public void EndConj() {
        st.ns = st.ns.next.next;
    }

    public void OpenQuant() {
        st.ns = new NState(bt, "QUANT", st.ns);
    }

    public int CloseQuant() {
        int x = st.ns.quant;
        st.ns = st.ns.next;
        return x;
    }

    public void IncQuant() {
        st.ns = new NState(st.ns);
        st.ns.quant++;
    }

    public void SetQuant(int data) {
        st.ns.quant = data;
    }

    public int GetQuant() {
        return st.ns.quant;
    }

    public STable GetClass() { return st.ns.klass; }

    public void SetClass(STable dm) {
        st.ns.klass = dm;
    }

    public Variable StringCapture() {
        return Kernel.NewROScalar(
            new Cursor(global, st.ns.klass, st.ns.quant, st.pos,
                null, null, "ANON"));
    }

    public Cursor MakeCursor() {
        return new Cursor(global, st.ns.klass, this, st.ns, bt, st.pos, st.captures);
    }

    public Variable MakeCursorV() { return Kernel.NewROScalar(MakeCursor()); }

    Cursor _matchObj;
    public Frame MakeMatch(Frame th) {
        int use_from = from;
        int use_to = st.pos;
        if ((flags & USED_ENDS) != 0) {
            for (CapInfo c = st.captures; c != null; c = c.prev) {
                if (c.names[0] == null && c.names[1] == "from")
                    use_from = Kernel.UnboxAny<int>(c.cap.Fetch());
                if (c.names[0] == null && c.names[1] == "to")
                    use_to = Kernel.UnboxAny<int>(c.cap.Fetch());
            }
        }
        if (Cursor.Trace)
            Console.WriteLine("Matching {0} from {1} to {2} (really {3}-{4})",
                    name, use_from, use_to, from, st.pos);
        _matchObj = new Cursor(global, st.ns.klass, use_from, use_to,
                st.captures, ast, name);
        return global.CallAction(th, name, _matchObj);
    }

    [CompartmentGlobal]
    public static Variable EmptyList;

    public Frame FinalEnd(Frame th) {
        if (st.pos > global.highwater)
            global.IncHighwater(st.pos);
        th.caller.resultSlot = Kernel.NewROScalar(_matchObj);
        return th.Return();
    }
    public Frame End(Frame th) {
        return EndWith(th, _matchObj);
    }
    // currently just used for protoregex
    public Frame EndWith(Frame th, Cursor m) {
        if (st.pos > global.highwater)
            global.IncHighwater(st.pos);
        if ((flags & RETURN_ONE) != 0) {
            return Kernel.Take(th, Kernel.NewROScalar(m));
        } else {
            th.MarkSharedChain();
            flags |= RETURN_ONE;
            VarDeque ks = new VarDeque();
            ks.Push(Kernel.NewROScalar(m));
            th.coro_return = th;
            P6opaque it  = new P6opaque(Kernel.GatherIteratorMO);
            it.slots[0 /*frame*/] = Kernel.NewMuScalar(th);
            it.slots[1 /*reify*/] = Kernel.NewMuScalar(Kernel.AnyP);
            VarDeque iss = new VarDeque();
            iss.Push(Kernel.NewROScalar(it));
            P6opaque lst = new P6opaque(Kernel.ListMO);
            lst.slots[0 /*items*/] = ks;
            lst.slots[1 /*rest*/ ] = iss;
            th.caller.resultSlot = Kernel.NewRWListVar(lst);
        }
        return th.Return();
    }
    void IFreeze.Freeze(FreezeBuffer fb) { throw new NotImplementedException(); }
}

// This is used to carry match states in and out of subrules.  Within subrules,
// match states are represented much more ephemerally in the state of RxFrame.

// this does double duty backing Match; note that Cursor and Match need to be
// treated polymorphically in a couple places
public class Cursor : P6any {
    public static readonly bool Trace =
        Environment.GetEnvironmentVariable("NIECZA_RX_TRACE") != null;
    public static readonly bool HwTrace =
        Environment.GetEnvironmentVariable("NIECZA_HIGHWATER_TRACE") != null;

    // common fields
    public GState global;
    public int pos;
    // Cursor only
    public Choice xact;
    public NState nstate;
    public RxFrame feedback;
    // Match only
    public string reduced;
    public P6any ast;
    public int from;
    public CapInfo captures;
    public STable save_klass;

    public override string ReprName() { return "P6cursor"; }

    public override void Freeze(FreezeBuffer fb) { throw new NotImplementedException(); }
    public string GetBacking() { return global.orig_s; }

    public Cursor(P6any proto, string text, P6any actions)
        : this(new GState(text, actions), proto.mo, null, null, null, 0, null) { }

    public Cursor(GState g, STable klass, int from, int pos, CapInfo captures, P6any ast, string reduced) {
        this.global = g;
        this.captures = captures;
        this.pos = pos;
        this.ast = ast;
        this.from = from;
        this.mo = Kernel.MatchMO;
        this.save_klass = klass;
        this.reduced = reduced;
    }

    public Cursor(GState g, STable klass, RxFrame feedback, NState ns, Choice xact, int pos, CapInfo captures) {
        this.mo = this.save_klass = klass;
        this.xact = xact;
        this.nstate = ns;
        this.feedback = feedback;
        this.global = g;
        this.pos = pos;
        this.captures = captures;
    }

    public static Frame Synthetic(Frame th, Cursor parent, string method,
            int from, int to, Variable caplist) {
        VarDeque iter = Builtins.start_iter(caplist);
        CapInfo ci = null;
        while (Kernel.IterHasFlat(iter, true)) {
            P6any pair = iter.Shift().Fetch();
            Variable k = (Variable)pair.GetSlot(Kernel.EnumMO, "$!key");
            Variable v = (Variable)pair.GetSlot(Kernel.EnumMO, "$!value");
            ci = new CapInfo(ci, new string[] {
                    k.Fetch().mo.mro_raw_Str.Get(k) }, v);
        }
        Cursor r = new Cursor(parent.global, parent.save_klass, from, to, ci,
                null, method);
        th.info.dylex["$*match"].Set(th, Kernel.NewROScalar(r));
        if (method == "") {
            return th;
        } else {
            return r.global.CallAction(th, method, r);
        }
    }

    public override bool IsDefined() {
        return true;
    }

    public Cursor At(int npos) {
        return new Cursor(global, mo, feedback, nstate, xact, npos, null);
    }

    public Cursor UnMatch() {
        return new Cursor(global, save_klass, null, null, null, pos, null);
    }

    public Cursor FreshClass(P6any from) {
        return new Cursor(global, from.mo, feedback, nstate, xact, pos, null);
    }

    public Cursor StripCaps() {
        return new Cursor(global, save_klass, from, pos, null, ast, reduced);
    }

    private Variable FixupList(VarDeque caps) {
        if (caps.Count() != 0 && caps[0] == null) {
            caps.Shift();
            P6opaque l = new P6opaque(Kernel.ListMO);
            l.slots[0 /*items*/] = caps;
            l.slots[1 /*rest*/ ] = new VarDeque();
            return Kernel.NewROScalar(l);
        } else {
            return caps.Count() != 0 ? caps[0] : Kernel.AnyMO.typeVar;
        }
    }

    public void Make(Variable itm) {
        if (feedback != null)
            feedback.ast = itm.Fetch();
        else
            ast = itm.Fetch();
    }

    public string Reduced() { return reduced; }
    public P6any AST() {
        P6any a = (feedback != null) ? feedback.ast : ast;
        return a ?? Kernel.AnyMO.typeObject;
    }

    // TODO: cache generated lists
    public Variable GetKey(string str) {
        CapInfo it = captures;
        VarDeque caps = new VarDeque();

        for (; it != null; it = it.prev) {
            if (it.names[0] == null)
                continue; // special node
            foreach (string cn in it.names) {
                if (cn == str) {
                    caps.Unshift(it.cap);
                    break;
                }
            }
        }

        return FixupList(caps);
    }

    public void UnpackCaps(P6any into) {
        List<VarDeque> posr = new List<VarDeque>();
        Dictionary<string,VarDeque> namr = new Dictionary<string,VarDeque>();
        CapInfo it = captures;

        for (; it != null; it = it.prev) {
            if (it.names[0] == null)
                continue; // special node
            foreach (string name in it.names) {
                int nami;
                VarDeque t;
                if (int.TryParse(name, out nami) && nami >= 0) {
                    while(posr.Count <= nami) posr.Add(new VarDeque());
                    t = posr[nami];
                } else {
                    if (!namr.TryGetValue(name, out t))
                        namr[name] = t = new VarDeque();
                }
                t.Unshift(it.cap);
            }
        }

        VarHash nam = new VarHash();
        Variable[] pos = new Variable[posr.Count];

        foreach (KeyValuePair<string, VarDeque> kv in namr)
            nam[kv.Key] = FixupList(kv.Value);
        for (int i = 0; i < pos.Length; i++)
            pos[i] = FixupList(posr[i]);

        into.SetSlot(Kernel.CaptureMO, "$!positionals", pos);
        into.SetSlot(Kernel.CaptureMO, "$!named", nam);
    }

    public Variable O(VarHash caps) {
        Cursor nw = At(pos);
        foreach (KeyValuePair<string,Variable> kv in caps)
            nw.captures = new CapInfo(nw.captures, new string[] { kv.Key },
                    Kernel.NewMuScalar(kv.Value.Fetch()));

        return Kernel.NewROScalar(nw);
    }

    public Variable SimpleWS() {
        string backing = global.orig_s;
        char[] backing_ca = global.orig_a;
        int l = backing_ca.Length;
        int p = pos;

        if (p != 0 && p != l && CC.Word.Accepts(backing[p]) &&
                CC.Word.Accepts(backing[p-1])) {
            if (Trace)
                Console.WriteLine("! no match <ws> at {0}", pos);
            return Kernel.NilP.mo.typeVar;
        } else {
            while (p != l && Char.IsWhiteSpace(backing, p)) { p++; }
            if (Trace)
                Console.WriteLine("* match <ws> at {0} to {1}", pos, p);
            return Kernel.NewROScalar(At(p));
        }
    }
}

public partial class Builtins {
    public static Variable cursor_allcaps(Variable cv) {
        Cursor c = (Cursor) cv.Fetch();
        VarDeque dq = new VarDeque();

        for (CapInfo it = c.captures; it != null; it = it.prev) {
            if (it.names[0] == null || it.cap == null)
                continue; // special node
            if (!it.cap.Fetch().Isa(Kernel.MatchMO))
                continue;
            foreach (string name in it.names)
                dq.Unshift(pair(MakeStr(name), it.cap));
        }

        P6opaque lst = new P6opaque(Kernel.ListMO);
        lst.slots[0 /*items*/] = dq;
        lst.slots[1 /*rest*/ ] = new VarDeque();
        return Kernel.NewRWListVar(lst);
    }
}

public sealed class CC : IFreeze {
    public readonly int[] vec;

    public CC(int[] vec) {
        this.vec = vec;
    }

    public CC(char butyes) : this(new int[] { butyes, -1, butyes+1, 0 }) { }
    public CC(int catmask) : this(new int[] { 0, catmask }) { }
    public CC(char butyes, bool nocase) {
        if (nocase) {
            char a = char.ToLowerInvariant(butyes);
            char b = char.ToUpperInvariant(butyes);
            if (a > b) { char t = a; a = b; b = t; }
            if (a == b) {
                vec = new int[] { a, -1, a+1, 0 };
            } else {
                vec = new int[] { a, -1, a+1, 0, b, -1, b+1, 0 };
            }
        } else {
            vec = new int[] { butyes, -1, butyes+1, 0 };
        }
    }

    public bool Accepts(int ch) {
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

        int mask = (ch < 0x10000) ?
            (1 << (int)char.GetUnicodeCategory((char)ch)) : 1;
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

    // our $VSpace = CClass.enum("\x000A", "\x000B", "\x000C", "\x000D",
    //    "\x0085", "\x2028", "\x2029");
    [Immutable] public static readonly CC VSpace = new CC(new int[] {
            0x000A, MAll, 0x000E, 0, 0x0085, MAll, 0x0086, 0,
            0x2028, MAll, 0x202A, 0 });

    [Immutable] public static readonly CC Word  = new CC(new int[] { 0,263167,9398,1073741823,9451,263167 }); // same as \w

    [Immutable] public static readonly CC All   = new CC(MAll);
    [Immutable] public static readonly CC None  = new CC(0);
    [Immutable] public static readonly CC AlNum = new CC(MAlNum);

    [Immutable]
    private static readonly int[] masks = new int[] {
        MAll, (MAll & ~MOther), 0x71F,
        0x1F, 0xE0, 0x700, 0x3800, 0x3C000, 0x1FC0000, 0x1E000000,

        (1<<0),  (1<<1),  (1<<2),  (1<<3),   (1<<4),  (1<<5),  (1<<6),  (1<<7),
        (1<<8),  (1<<9),  (1<<10), (1<<11),  (1<<12), (1<<13), (1<<14), (1<<15),
        (1<<16), (1<<17), (1<<18), (1<<19),  (1<<20), (1<<21), (1<<22), (1<<23),
        (1<<24), (1<<25), (1<<26), (1<<27),  (1<<28), (1<<29),
    };
    [Immutable]
    private static readonly string[] masknames = new string[] {
        "ALL", "Assigned", "Alnum",
        "Alpha", "Mark", "Num", "Space", "Control", "Punct", "Symbol",

        "Lu", "Ll", "Lt", "Lm",  "Lo", "Mn", "Mc", "Me",
        "Nd", "Nl", "No", "Zs",  "Zl", "Zp", "Cc", "Cf",
        "Cs", "Co", "Pc", "Pd",  "Ps", "Pe", "Pi", "Pf",
        "Po", "Sm", "Sc", "Sk",  "So", "Cn"
    };

    public override string ToString() {
        List<string> clauses = new List<string>();
        for (int ix = 0; ix < vec.Length; ix += 2) {
            string head = "";
            int l = vec[ix];
            int msk = vec[ix+1];
            int h = (ix + 2 < vec.Length) ? vec[ix+2] : 0x110000;

            if (msk == 0)
                continue;
            while (ix + 2 < vec.Length &&
                    (vec[ix+3] == MAll || vec[ix+3] == msk)) {
                h = (ix + 4 < vec.Length) ? vec[ix+4] : 0x110000;

                if (vec[ix+3] == MAll)
                    clauses.Add(h == vec[ix+2] + 1 ?
                        string.Format("({0:X4})", vec[ix+2]) :
                        string.Format("({0:X4}..{1:X4})", vec[ix+2], h-1));
                ix += 2;
            }
            if (h != 0x110000 || l != 0 || ((msk & MAll) == MAll))
                head = (h == l + 1) ?
                    string.Format("({0:X4})", l) :
                    string.Format("({0:X4}..{1:X4})", l, h-1);
            if ((msk & MAll) != MAll) {
                List<string> vc = new List<string>();
                for (int mix = 0; mix < masks.Length; mix++)
                    if ((msk & masks[mix]) == masks[mix]) {
                        msk &= ~masks[mix];
                        vc.Add(masknames[mix]);
                    }
                clauses.Add(head + Kernel.JoinS("+", vc));
            } else
                clauses.Add(head);
        }
        return Kernel.JoinS(",", clauses);
    }

    void IFreeze.Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.CC);
        fb.Ints(vec);
    }

    internal static object Thaw(ThawBuffer tb) {
        return tb.Register(new CC(tb.Ints()));
    }
}

// When building a NFA you may encounter negative pseudo-indexes which
// are used to refer to NFAs that haven't been assigned to numbers yet;
// the form is -{1,2}-2*N
public sealed class NFA {
    struct Node {
        public int fate;
        public int first_edge;

        public string Describe(NFA owner, int ownix) {
            List<Edge> le = new List<Edge>();
            for (int i = first_edge; i < owner.edges.Length; i++) {
                if (ownix + 1 < owner.nodes.Length &&
                        i >= owner.nodes[ownix+1].first_edge)
                    break;
                le.Add(owner.edges[i]);
            }
            return (fate > 0 ? "(" + fate + ") " : "   ") +
                Kernel.JoinS(", ", le);
        }
    }

    public struct Edge {
        public int to;
        public int when; // -2=epsilon -1=use CC
        public CC when_cc;

        public override string ToString() {
            return ((when >= 0) ? "'" + Utils.Chr(when) + "'" : (when == -1) ? when.ToString() : "ε") + " => " + to;
        }
    }

    List<Node> nodes_l = new List<Node>();
    List<int> outgoing = new List<int>();
    List<int> fromv    = new List<int>();
    Edge[] edges = new Edge[8];
    int nedges;
    Node[] nodes;
    public int curfate;

    public STable cursor_class;
    public HashSet<string> name_stack = new HashSet<string>();
    public HashSet<string> used_methods = new HashSet<string>();
    public List<Frame>   outer_stack = new List<Frame>();
    public List<SubInfo> info_stack  = new List<SubInfo>();

    //List<NFA> subnfa_l = new List<NFA>();
    //NFA[] subnfa;
    //int[] subnfa_start;
    //int[] subnfa_continue;
    //int end_node;

    public int AddNode() {
        nodes_l.Add(new Node());
        outgoing.Add(0);
        return nodes_l.Count - 1;
    }
    public void SetFinal(int node) {
        Node n = nodes_l[node];
        n.fate = curfate + 1;
        nodes_l[node] = n;
    }
    public void AddEdge(int from, int to, int when) {
        AddEdge(from, to, when, null);
    }
    public void AddEdge(int from, int to, CC when) {
        AddEdge(from, to, when == null ? -2 : -1, when);
    }
    void AddEdge(int from, int to, int when, CC when_cc) {
        Edge e;
        fromv.Add(from);
        e.to = to;
        e.when = when;
        e.when_cc = when_cc;
        if (nedges == edges.Length) {
            Array.Resize(ref edges, nedges * 2);
        }
        ++outgoing[from];
        edges[nedges++] = e;
    }
    public int NodeCount { get { return nodes.Length; } }
    public Edge[] EdgesOf(int i, ref int index, ref int imax) {
        index = nodes[i].first_edge;
        imax  = (i == nodes.Length - 1) ? edges.Length : nodes[i+1].first_edge;
        return edges;
    }
    public int FateOf(int i) {
        var n = nodes[i];
        return n.fate - 1;
    }

    public void Dump() {
        for (int ix = 0; ix < nodes.Length; ix++) {
            Console.WriteLine(ix + ": " + nodes[ix].Describe(this, ix));
        }
    }

    int[] greybuf;

    public void Close(LexerState ls) {
        int ngrey = 0;
        for (int i = 0; i < ls.nstates.Length; i++) {
            int bm = ls.nstates[i];
            for (int j = 0; j < 32; j++) {
                if ((bm & (1 << j)) != 0)
                    greybuf[ngrey++] = 32*i + j;
            }
        }

        while (ngrey != 0) {
            int val = greybuf[--ngrey];
            int eix = 0, lix = 0;
            EdgesOf(val, ref eix, ref lix);
            while (eix != lix) {
                Edge e = edges[eix++];
                if (e.when == -2) {
                    int ix = e.to >> 5;
                    int m = 1 << (e.to & 31);
                    if ((ls.nstates[ix] & m) == 0) {
                        ls.nstates[ix] |= m;
                        greybuf[ngrey++] = e.to;
                    }
                }
            }
        }
    }

    public void Complete() {
        nodes = nodes_l.ToArray();
        nodes_l = null;
        info_stack = null;
        outer_stack = null;
        greybuf = new int[nodes.Length];

        var oedges = edges;
        edges = new Edge[nedges];

        int i = 0;
        // put the edges in correct order
        for (int j = 0; j < nodes.Length; j++) {
            i += outgoing[j];
            nodes[j].first_edge = i;
        }
        for (i = 0; i < nedges; i++) {
            Edge e = oedges[i];
            edges[--nodes[fromv[i]].first_edge] = e;
        }
        outgoing = null;
        fromv = null;
    }

    public Dictionary<LexerState,LexerState> dfashare
        = new Dictionary<LexerState,LexerState>();
}

// ltm automaton descriptors
public abstract class LAD : IFreeze {
    // return true if to is accessible
    public abstract bool ToNFA(NFA pad, int from, int to);
    public abstract void Dump(int indent);
    public abstract void Freeze(FreezeBuffer fb);
    public virtual void QueryLiteral(NFA pad, out int len, out bool cont) {
        len = 0; cont = false;
    }

    public abstract LAD Reify(NFA pad);
}

public class LADStr : LAD {
    public string text;
    public LADStr(string text) { this.text = text; }

    public override LAD Reify(NFA pad) { return this; }
    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        len = text.Length; cont = true;
    }

    public override bool ToNFA(NFA pad, int from, int to) {
        if (text.Length == 0) {
            pad.AddEdge(from, to, null);
        } else {
            int len = text.Length;
            for (int c = 0; c < len; c++) {
                int fromp = (c == len - 1) ? to : pad.AddNode();
                pad.AddEdge(from, fromp, (int)text[c]); // XXX supplementaries
                from = fromp;
            }
        }
        return true;
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "str: " + text);
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADStr);
        fb.String(text);
    }
    internal static LADStr Thaw(ThawBuffer tb) {
        LADStr n = new LADStr(null);
        tb.Register(n);
        n.text = tb.String();
        return n;
    }
}

public class LADStrNoCase : LAD {
    public string text;
    public LADStrNoCase(string text) { this.text = text; }
    private LADStrNoCase() { }

    public override LAD Reify(NFA pad) { return this; }
    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        len = text.Length; cont = true;
    }

    public override bool ToNFA(NFA pad, int from, int to) {
        if (text.Length == 0) {
            pad.AddEdge(from, to, null);
        } else {
            int len = text.Length;
            for (int c = 0; c < len; c++) {
                int fromp = (c == len - 1) ? to : pad.AddNode();
                pad.AddEdge(from, fromp, new CC(text[c], true));
                from = fromp;
            }
        }
        return true;
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "strnocase: " + text);
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADStrNoCase);
        fb.String(text);
    }
    internal static LADStrNoCase Thaw(ThawBuffer tb) {
        LADStrNoCase n = new LADStrNoCase();
        tb.Register(n);
        n.text = tb.String();
        return n;
    }
}

public class LADCC : LAD {
    public CC cc;
    private LADCC() { }
    public LADCC(CC cc) { this.cc = cc; }

    public override LAD Reify(NFA pad) { return this; }
    public override bool ToNFA(NFA pad, int from, int to) {
        pad.AddEdge(from, to, cc);
        return true;
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "cc: " + cc.ToString());
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADCC);
        fb.ObjRef(cc);
    }
    internal static LADCC Thaw(ThawBuffer tb) {
        LADCC n = new LADCC();
        tb.Register(n);
        n.cc = (CC) tb.ObjRef();
        return n;
    }
}

public class LADImp : LAD {
    public override LAD Reify(NFA pad) { return this; }
    public override bool ToNFA(NFA pad, int from, int to) {
        int knot = pad.AddNode();
        pad.SetFinal(knot);
        pad.AddEdge(from, knot, null);
        return false;
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "imp");
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADImp);
    }
}

public class LADNull : LAD {
    public override LAD Reify(NFA pad) { return this; }
    public override bool ToNFA(NFA pad, int from, int to) {
        pad.AddEdge(from, to, null);
        return true;
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "null");
    }

    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        len = 0; cont = true;
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADNull);
    }
}

public class LADNone : LAD {
    public override LAD Reify(NFA pad) { return this; }
    public override bool ToNFA(NFA pad, int from, int to) {
        return false;
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "none");
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADNone);
    }
}

public class LADDot : LAD {
    public override LAD Reify(NFA pad) { return this; }
    public override bool ToNFA(NFA pad, int from, int to) {
        pad.AddEdge(from, to, CC.All);
        return true;
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "dot");
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADDot);
    }
}

public class LADQuant : LAD {
    public int type; // 1=allow 0  2=allow multiple  4=z1 optional
    public LAD z0, z1;
    public LADQuant(int type, LAD z0, LAD z1) {
        this.type = type; this.z0 = z0; this.z1 = z1;
    }
    private LADQuant() {}

    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        if ((type & 1) == 0) {
            z0.QueryLiteral(pad, out len, out cont);
        } else {
            len = 0;
        }
        cont = false;
    }

    public override LAD Reify(NFA pad) {
        return new LADQuant(type, z0.Reify(pad), z1 != null ? z1.Reify(pad) : null);
    }
    public override bool ToNFA(NFA pad, int from, int to) {
        int knot1 = pad.AddNode();
        int knot2 = pad.AddNode();
        int knot3 = pad.AddNode();
        pad.AddEdge(from, knot1, null);

        z0.ToNFA(pad, knot1, knot2);
        if (z1 != null)
            z1.ToNFA(pad, knot2, knot3);
        else
            pad.AddEdge(knot2, knot3, null);

        pad.AddEdge(knot2, to, null);

        if ((type & 1) != 0)
            pad.AddEdge(from, to, null);
        if ((type & 4) != 0)
            pad.AddEdge(knot3, to, null);
        if ((type & 2) != 0)
            pad.AddEdge(knot3, knot1, null);
        return true; // conservative
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "quant({0}):", type);
        z0.Dump(indent + 4);
        if (z1 != null)
            z1.Dump(indent + 4);
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADQuant);
        fb.Byte((byte) (type | (z1 != null ? 8 : 0)));
        fb.ObjRef(z0);
        if (z1 != null) fb.ObjRef(z1);
    }
    internal static object Thaw(ThawBuffer tb) {
        var n = new LADQuant();
        tb.Register(n);
        n.type = tb.Byte();
        n.z0 = (LAD) tb.ObjRef();
        if ((n.type & 8) != 0) n.z1 = (LAD) tb.ObjRef();
        return n;
    }
}

public class LADSequence : LAD {
    public LAD[] args;
    public LADSequence(LAD[] args) { this.args = args; }
    private LADSequence() { }
    public override LAD Reify(NFA pad) {
        LAD[] nc = new LAD[args.Length];
        for (int i = 0; i < args.Length; i++)
            nc[i] = args[i].Reify(pad);
        return new LADSequence(nc);
    }

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

    public override bool ToNFA(NFA pad, int from, int to) {
        for (int i = 0; i < args.Length; i++) {
            int knot = (i == args.Length - 1) ? to : pad.AddNode();
            if (!args[i].ToNFA(pad, from, knot))
                return false;
            from = knot;
        }
        if (from != to) pad.AddEdge(from, to, null);
        return true;
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "seq:");
        foreach (LAD l in args)
            l.Dump(indent + 4);
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADSequence);
        fb.Refs(args);
    }
    internal static object Thaw(ThawBuffer tb) {
        var n = new LADSequence();
        tb.Register(n);
        n.args = tb.RefsA<LAD>();
        return n;
    }
}

public class LADAny : LAD {
    public LAD[] zyg;
    public LADAny(LAD[] zyg) { this.zyg = zyg; }
    private LADAny() { }
    public override LAD Reify(NFA pad) {
        LAD[] nc = new LAD[zyg.Length];
        for (int i = 0; i < zyg.Length; i++)
            nc[i] = zyg[i].Reify(pad);
        return new LADAny(nc);
    }

    public override bool ToNFA(NFA pad, int from, int to) {
        bool ok = false;
        foreach (LAD k in zyg)
            if (k.ToNFA(pad, from, to)) ok = true;
        return ok;
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "any:");
        foreach (LAD k in zyg)
            k.Dump(indent + 4);
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADAny);
        fb.Refs(zyg);
    }
    internal static object Thaw(ThawBuffer tb) {
        var n = new LADAny();
        tb.Register(n);
        n.zyg = tb.RefsA<LAD>();
        return n;
    }
}

public class LADParam : LAD {
    public string name;
    private LADParam() {}
    public LADParam(string name) { this.name = name; }

    public override LAD Reify(NFA pad) {
        string text = GetText(pad);
        if (text != null) {
            return new LADStr(text);
        } else {
            return new LADImp();
        }
    }

    public string GetText(NFA pad) {
        // XXX we might want to restrict this to "true constants"
        // alternatively, we could generalize to any variable, and add rechecks
        Frame outer = pad.outer_stack[pad.outer_stack.Count - 1];

        Variable vr = outer == null ? Kernel.AnyMO.typeVar :
            outer.LexicalFind(name);

        P6any ob = vr.Fetch();

        if (!vr.rw && ob.IsDefined() && ob.mo == Kernel.StrMO) {
            if (Lexer.LtmTrace)
                Console.WriteLine("Resolved {0} to \"{1}\"", name,
                        Kernel.UnboxAny<string>(ob));
            return Kernel.UnboxAny<string>(ob);
        } else {
            if (Lexer.LtmTrace)
                Console.WriteLine("No LTM for {0}", name);
            return null;
        }
    }

    public override bool ToNFA(NFA pad, int from, int to) {
        throw new InvalidOperationException();
    }

    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        throw new InvalidOperationException();
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "param: " + name);
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADParam);
        fb.String(name);
    }
    internal static LADParam Thaw(ThawBuffer tb) {
        LADParam n = new LADParam();
        tb.Register(n);
        n.name = tb.String();
        return n;
    }
}

public class LADMethod : LAD {
    public string name;

    public LADMethod(string name) { this.name = name; }
    private LADMethod() {}

    public override bool ToNFA(NFA pad, int from, int to) {
        throw new InvalidOperationException();
    }

    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        throw new InvalidOperationException();
    }

    public override LAD Reify(NFA pad) {
        if (Lexer.LtmTrace)
            Console.WriteLine("+ Processing subrule {0}", name);
        if (name == "ws")
            return new LADImp();
        pad.used_methods.Add(name);

        if (pad.name_stack.Contains(name)) {
            // NFAs cannot be recursive, so treat this as the end of the
            // declarative prefix.
            if (Lexer.LtmTrace)
                Console.WriteLine("+ Pruning to avoid recursion");
            return new LADImp();
        }

        DispatchEnt de;
        if (!pad.cursor_class.mro_methods.TryGetValue(name, out de)
                || de.info.ltm == null)
            return new LADImp();

        pad.name_stack.Add(name);
        pad.outer_stack.Add(de.outer);
        pad.info_stack.Add(de.info);

        LAD ret = de.info.ltm.Reify(pad);

        pad.name_stack.Remove(name);
        pad.outer_stack.RemoveAt(pad.outer_stack.Count - 1);
        pad.info_stack.RemoveAt(pad.info_stack.Count - 1);

        return ret;
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "methodcall " + name);
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADMethod);
        fb.String(name);
    }
    internal static LADMethod Thaw(ThawBuffer tb) {
        LADMethod n = new LADMethod();
        tb.Register(n);
        n.name = tb.String();
        return n;
    }
}

// Only really makes sense if used in the static scope of a proto
public class LADDispatcher : LAD {
    public override bool ToNFA(NFA pad, int from, int to) {
        throw new InvalidOperationException();
    }

    public override void QueryLiteral(NFA pad, out int len, out bool cont) {
        throw new InvalidOperationException();
    }

    public override LAD Reify(NFA pad) {
        Frame   of = pad.outer_stack[pad.outer_stack.Count - 1];
        SubInfo si = pad.info_stack[pad.info_stack.Count - 1];

        while ((si == null || si.param == null) && of != null) {
            si = of.info;
            of = of.outer;
        }

        if (si == null || si.param == null || !(si.param[0] is P6any[]))
            throw new NieczaException("Cannot resolve dispatch operator");

        P6any[] cands = si.param[0] as P6any[];
        LAD[] opts = new LAD[cands.Length];

        for (int i = 0; i < opts.Length; i++) {
            pad.outer_stack.Add(Kernel.GetOuter(cands[i]));
            pad.info_stack.Add(Kernel.GetInfo(cands[i]));
            opts[i] = Kernel.GetInfo(cands[i]).ltm.Reify(pad);
            pad.outer_stack.RemoveAt(pad.outer_stack.Count - 1);
            pad.info_stack.RemoveAt(pad.info_stack.Count - 1);
        }

        return new LADAny(opts);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "dispatcher");
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADDispatcher);
    }
}

// These objects get put in hash tables, so don't change nstates[] after
// that happens
public sealed class LexerState {
    public int[] nstates;
    Dictionary<int,LexerState> dfc = new Dictionary<int,LexerState>();

    public LexerState(NFA nf) {
        this.nstates = new int[(nf.NodeCount + 31) >> 5];
    }

    public void Add(int n) {
        nstates[n >> 5] |= 1 << (n & 31);
    }

    public LexerState Next(NFA nf, int ch) {
        LexerState l;
        if (dfc.TryGetValue(ch, out l))
            return l;
        l = new LexerState(nf);
        for (int i = 0; i < nstates.Length; i++) {
            int bm = nstates[i];
            for (int j = 0; j < 32; j++) {
                if ((bm & (1 << j)) == 0)
                    continue;
                int ei = 0, eimax = 0;
                var es = nf.EdgesOf(32*i + j, ref ei, ref eimax);
                while (ei != eimax) {
                    var e = es[ei++];
                    if (e.when == ch || e.when == -1 && e.when_cc.Accepts(ch))
                        l.Add(e.to);
                }
            }
        }

        nf.Close(l);
        LexerState cl;

        if (!nf.dfashare.TryGetValue(l, out cl)) {
            nf.dfashare[l] = cl = l;
        }
        dfc[ch] = cl;
        return cl;
    }

    public bool IsAlive() {
        for (int i = 0; i < nstates.Length; i++)
            if (nstates[i] != 0) return true;
        return false;
    }

    public override bool Equals(object o) {
        LexerState ls = o as LexerState;
        if (ls == null) return false;
        if (ls.nstates.Length != nstates.Length) return false;
        for (int i = 0; i < nstates.Length; i++)
            if (ls.nstates[i] != nstates[i])
                return false;
        return true;
    }

    public override int GetHashCode() {
        int o = 0;
        for (int i = 0; i < nstates.Length; i++)
            o = o * 1342883 + nstates[i];
        return o;
    }

    public void CollectFates(NFA nf, Lexer l) {
        for (int i = nf.NodeCount - 1; i >= 0; i--) {
            if ((nstates[i >> 5] & (1 << (i & 31))) != 0) {
                int fate = nf.FateOf(i);
                if (fate >= 0) {
                    if (Lexer.LtmTrace)
                        Console.WriteLine("+ Adding fate {0}", fate);
                    l.NoteFate(fate);
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
    public STable mo;
    public LexerCache parent;
    public HashSet<string> repl_methods;

    public LexerCache(STable mo) {
        this.mo = mo;
        if (mo.mo.superclasses.Count == 1) {
            parent = mo.mo.superclasses[0].GetLexerCache();
            repl_methods = new HashSet<string>();
            foreach (P6how.MethodInfo mi in mo.mo.lmethods) {
                if ((mi.flags & P6how.V_MASK) != P6how.V_PUBLIC)
                    continue;
                string mn = mi.short_name;
                int ix = mn.IndexOf(':');
                if (ix >= 0) repl_methods.Add(mn.Substring(0,ix));
                else repl_methods.Add(mn);
            }
        }
    }

    public Dictionary<LAD[], Lexer> nfas = new Dictionary<LAD[], Lexer>();
    public Dictionary<SubInfo, Lexer> dispatch_nfas =
        new Dictionary<SubInfo, Lexer>();
}

public class Lexer {
    public LAD[] alts;
    public NFA pad;
    public string tag;

    LexerState start;
    LexerState nil;

    int nfates;
    // 2*nfates+2 elements, arranged in linked list nodes of 2 values
    // each, prev then next.  If prev == -1 this element is not in the
    // list.  Element 0 is the header node; the list is maintained
    // circular.
    int[] fatebuffer;
    int usedfates;

    public static readonly bool LtmTrace =
        Environment.GetEnvironmentVariable("NIECZA_LTM_TRACE") != null;
    public static readonly bool LtmProf =
        Environment.GetEnvironmentVariable("NIECZA_LTM_PROF") != null;

    public static Lexer GetLexer(Frame fromf, STable kl, LAD[] lads, string title) {
        LexerCache lc = kl.GetLexerCache();
        Lexer ret;
        if (lc.nfas.TryGetValue(lads, out ret))
            return ret;
        if (lc.parent != null && lc.parent.mo.name != "Cursor" && lc.parent.mo.name != "Any") {
            ret = GetLexer(fromf, lc.parent.mo, lads, title);
            foreach (string u in ret.pad.used_methods) {
                if (lc.repl_methods.Contains(u))
                    goto anew;
            }
            if (LtmTrace)
                Console.WriteLine("Reused {0} alternation lexer for {1} in {2}",
                        title, lc.parent.mo.name, kl.name);
            return lc.nfas[lads] = ret;
        }
anew:
        if (LtmTrace) {
            Console.WriteLine("Need new alternation lexer for {0} in {1}",
                    title, kl.name);
        }
        NFA pad = new NFA();
        pad.cursor_class = kl;
        LAD[] lads_p = new LAD[lads.Length];
        pad.outer_stack.Add(fromf);
        pad.info_stack.Add(fromf.info);
        for (int i = 0; i < lads_p.Length; i++)
            lads_p[i] = lads[i].Reify(pad);

        ret = new Lexer(pad, title, lads_p);
        lc.nfas[lads] = ret;
        return ret;
    }

    public static Lexer GetDispatchLexer(STable kl, SubInfo disp) {
        LexerCache lc = kl.GetLexerCache();
        Lexer ret;
        if (lc.dispatch_nfas.TryGetValue(disp, out ret))
            return ret;
        if (lc.parent != null && lc.parent.mo.name != "Cursor" && lc.parent.mo.name != "Any") {
            ret = GetDispatchLexer(lc.parent.mo, disp);
            foreach (string u in ret.pad.used_methods) {
                if (lc.repl_methods.Contains(u))
                    goto anew;
            }
            if (LtmTrace)
                Console.WriteLine("Reused {0} dispatch lexer for {1} in {2}",
                        disp.name, lc.parent.mo.name, kl.name);
            return lc.dispatch_nfas[disp] = ret;
        }
anew:
        if (LtmTrace) {
            Console.WriteLine("Need new dispatch lexer for {0} in {1}",
                    disp.name, kl.name);
        }
        NFA pad = new NFA();
        pad.cursor_class = kl;
        P6any[] cands = (P6any[])disp.param[0];
        LAD[] lads_p = new LAD[cands.Length];

        for (int i = 0; i < lads_p.Length; i++) {
            pad.outer_stack.Add(Kernel.GetOuter(cands[i]));
            pad.info_stack.Add(Kernel.GetInfo(cands[i]));
            lads_p[i] = pad.info_stack[0].ltm.Reify(pad);
            pad.outer_stack.RemoveAt(pad.outer_stack.Count - 1);
            pad.info_stack.RemoveAt(pad.info_stack.Count - 1);
        }

        ret = new Lexer(pad, disp.name, lads_p);
        lc.dispatch_nfas[disp] = ret;
        return ret;
    }

    public Lexer(NFA pad, string tag, LAD[] alts) {
        this.pad = pad;
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
            pad.SetFinal(target);
            alts[alt_shuffle[ix]].ToNFA(pad, root, target);
        }
        nfates = alts.Length;
        fatebuffer = new int[nfates*2+2];
        for (int i = 0; i < nfates*2+2; i++)
            fatebuffer[i] = -1;
        fatebuffer[0] = fatebuffer[1] = 0;
        pad.Complete();
        // now the NFA nodes are all in tiebreak order by lowest index
        if (LtmTrace) {
            Dump();
        }
        if (LtmProf) {
            Console.WriteLine("Lexer ({0}) has {1} nodes", tag, pad.NodeCount);
        }
        this.alts = null;
        start = new LexerState(pad);
        start.Add(0);
        pad.Close(start);
        nil = new LexerState(pad);
        pad.dfashare[nil] = nil;
        pad.dfashare[start] = start;
    }

    public void Dump() {
        Console.WriteLine("--- LEXER ({0}) : Tree", tag);
        for (int ix = 0; ix < alts.Length; ix++) {
            int j;
            bool c;
            alts[ix].QueryLiteral(pad, out j, out c);
            Console.WriteLine("{0}: (lit. {1})", ix, j);
            alts[ix].Dump(0);
        }
        Console.WriteLine("--- NFA:");
        pad.Dump();
        Console.WriteLine("--- END");
    }

    internal void NoteFate(int fate) {
        fate++; // 0 is used as a sentinel
        if (fatebuffer[fate * 2] == -1) {
            usedfates++;
        } else {
            // next->prev = prev
            fatebuffer[fatebuffer[fate * 2] * 2 + 1] =
                fatebuffer[fate * 2 + 1];
            fatebuffer[fatebuffer[fate * 2 + 1] * 2] =
                fatebuffer[fate * 2];
        }
        fatebuffer[fate * 2] = fatebuffer[0];
        fatebuffer[fate * 2 + 1] = 0;
        fatebuffer[fatebuffer[0] * 2 + 1] = fate;
        fatebuffer[0] = fate;
    }

    public int[] Run(string from, int pos) {
        LexerState state = start;
        int spos = pos;
        // clear out any old fates
        for (int i = fatebuffer[0]; i != 0; ) {
            int j = fatebuffer[2*i];
            fatebuffer[2*i] = -1;
            i = j;
        }
        fatebuffer[0] = fatebuffer[1] = 0;
        usedfates = 0;

        if (LtmTrace)
            Console.WriteLine("+ Trying lexer {0} at {1}", tag, pos);

        while (true) {
            state.CollectFates(pad, this);

            if (pos == from.Length || state == nil) break;
            int ch = from[pos++];
            if (((uint)(ch - 0xD800)) < 0x400u && pos != from.Length) {
                ch = from[pos++] + ch * 0x400 +
                   (0x10000 - (0xD800 * 0x400 + 0xDC00));
            }

            if (LtmTrace)
                Console.WriteLine("+ Adding character {0}", ch);

            LexerState next = state.Next(pad, ch);

            if (LtmTrace)
                Console.WriteLine("+ Changing state to {0}", next);

            state = next;
        }

        int[] uniqfates = new int[usedfates];

        if (LtmProf)
            Console.WriteLine("Ran {0} from {1} to length {2}", tag, spos,
                    pos - spos);
        int cursor = 0;
        for (int i = 0; i < usedfates; i++) {
            cursor = fatebuffer[cursor * 2];
            uniqfates[i] = cursor - 1;
            if (LtmTrace || LtmProf)
                Console.WriteLine("+ Useful fate: {0}", cursor - 1);
        }
        return uniqfates;
    }

    internal static P6any[] RunDispatch(Frame th, Cursor cursor) {
        Frame fromf = th;
        if (fromf.info.param == null) {
            fromf = fromf.caller;
            while (fromf.info.param == null ||
                    !(fromf.info.param[0] is P6any[]))
                fromf = fromf.outer;
        }
        th.lex3 = fromf.pos;
        th.lex4 = fromf.named;

        STable kl = cursor.mo;

        Lexer l = GetDispatchLexer(kl, fromf.info);
        P6any[] candidates = (P6any[]) fromf.info.param[0];

        Cursor c = (Cursor)cursor;
        int[] brnum = l.Run(c.global.orig_s, c.pos);

        P6any[] ret = new P6any[brnum.Length];
        for (int i = 0; i < brnum.Length; i++)
            ret[i] = candidates[brnum[i]];

        return ret;
    }

    internal static SubInfo StandardProtoSI =
        new SubInfo("KERNEL protoregex", StandardProtoC);
    internal static Frame StandardProtoC(Frame th) {
        Variable[] al;
        switch (th.ip) {
            default:
                return Kernel.Die(th, "Invalid IP");
            case 1:
                return th.rx.Backtrack(th);
            case 0:
                th.lex0 = (Cursor)th.pos[0].Fetch();
                th.rx = new RxFrame(th.info.name, (Cursor)th.lex0, false);
                th.rx.PushCutGroup("LTM");
                th.lex1 = RunDispatch(th, (Cursor)th.lex0);
                th.lexi0 = 0;
                goto case 2;
            case 2:
                if (th.lexi0 == ((P6any[])th.lex1).Length)
                    goto case 1;
                th.rx.PushBacktrack(2);
                th.ip = 3;
                al = (Variable[])(((Variable[])th.lex3).Clone());
                al[0] = Kernel.NewROScalar(th.rx.MakeCursor());
                return (((P6any[])th.lex1)[th.lexi0++]).Invoke(th, al,
                    (VarHash)th.lex4);
            case 3:
                th.lex2 = Builtins.start_iter((Variable) th.resultSlot);
                goto case 4;
            case 4:
                if (!Kernel.IterHasFlat((VarDeque)th.lex2, true))
                    goto case 1;
                th.ip = 4;
                return th.rx.EndWith(th, (Cursor) ((VarDeque)th.lex2).Shift().Fetch());
        }
    }

    public static P6any MakeDispatcher(string name, P6any proto, P6any[] cands) {
        if (proto != null) {
            SubInfo si = new SubInfo(Kernel.GetInfo(proto));
            si.param = new object[] { cands, null };
            return Kernel.MakeSub(si, Kernel.GetOuter(proto));
        } else {
            SubInfo si = new SubInfo(name, StandardProtoC);
            si.param = new object[] { cands, null };
            si.ltm = new LADDispatcher();
            return Kernel.MakeSub(si, null);
        }
    }
}
