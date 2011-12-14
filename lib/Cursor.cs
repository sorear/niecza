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

        return m.info.Binder(th, m.outer, m.ip6, pos, null, false, m);
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
        st.subrule_iter = null;
        ip = (ip < 0) ? ip : (-ip-1);
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
    public bool return_one;

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

    public RxFrame(string name, Cursor csr, bool passcap, bool passcut) {
        global = csr.global;
        orig = global.orig_a;
        end = orig.Length;
        rootf = bt = csr.xact;
        this.name = name;
        st.ns = passcut ? csr.nstate :
            new NState(rootf, name, csr.nstate);
        if (passcap)
            st.captures = csr.captures;
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
        while (bt != rootf && (bt.ip < 0))
            bt = bt.prev;
        if (st.pos > global.highwater)
            global.IncHighwater(st.pos);
        if (bt == rootf) {
            if (return_one) {
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

    public void SetCapturesFrom(Cursor inp) {
        st.captures = inp.captures;
    }

    public void SetPos(int pos) {
        st.pos = pos;
    }

    public void IncorporateChild(string[] names, bool passcap, P6any match) {
        Cursor child = match as Cursor;

        if (child == null)
            throw new NieczaException((names.Length == 0 ?
                "Anonymous submatch" : "Submatch to be bound to " + names[0]) +
                    " returned a " + match.mo.name + " instead of a Cursor, " +
                    "violating the submatch protocol.");

        SetPos(child.pos);
        if (passcap)
            SetCapturesFrom(child);

        if (names != null) {
            if (passcap) child = child.StripCaps();
            PushCapture(names, Kernel.NewROScalar(child));
        }
    }

    public const int IC_ZERO_WIDTH = 1;
    public const int IC_NEGATIVE = 2;
    public const int IC_PASS_CAP = 4;

    public bool IncorpCut(string[] names, int mode, Variable list) {
        Variable match_v = Kernel.GetFirst(list);
        P6any match = match_v.Fetch();
        if (match.IsDefined() == ((mode & IC_NEGATIVE) != 0))
            return false;
        if ((mode & IC_ZERO_WIDTH) == 0)
            IncorporateChild(names, (mode & IC_PASS_CAP) != 0, match);
        return true;
    }

    public bool IncorpShift(string[] names, bool pass_cap, int label) {
        if (!Kernel.IterHasFlat(GetCursorIter(), true))
            return false;
        PushBacktrack(label);
        IncorporateChild(names, pass_cap, GetCursorIter().Shift().Fetch());
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

    public Cursor MakeCursor() {
        return new Cursor(global, st.ns.klass, this, st.ns, bt, st.pos, st.captures);
    }

    public Variable MakeCursorV() { return Kernel.NewROScalar(MakeCursor()); }

    Cursor _matchObj;
    public Frame MakeMatch(Frame th) {
        if (Cursor.Trace)
            Console.WriteLine("Matching {0} from {1} to {2}",
                    name, from, st.pos);
        _matchObj = new Cursor(global, st.ns.klass, from, st.pos,
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
        if (return_one) {
            return Kernel.Take(th, Kernel.NewROScalar(m));
        } else {
            th.MarkSharedChain();
            return_one = true;
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
            Variable k = (Variable)pair.GetSlot("key");
            Variable v = (Variable)pair.GetSlot("value");
            ci = new CapInfo(ci, new string[] {
                    k.Fetch().mo.mro_raw_Str.Get(k) }, v);
        }
        Cursor r = new Cursor(parent.global, parent.save_klass, from, to, ci,
                null, method);
        th.info.dylex["$*match"].Set(th, Kernel.NewROScalar(r));
        return r.global.CallAction(th, method, r);
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
        if (a != null)
            return a;
        else
            return Kernel.BoxRaw<string>(GetBacking().Substring(from, pos - from), Kernel.StrMO);
    }

    // TODO: cache generated lists
    public Variable GetKey(string str) {
        CapInfo it = captures;
        VarDeque caps = new VarDeque();

        while (it != null) {
            foreach (string cn in it.names) {
                if (cn == str) {
                    caps.Unshift(it.cap);
                    break;
                }
            }
            it = it.prev;
        }

        if (str == "0" && caps.Count() == 0)
            caps.Unshift(Kernel.NewROScalar(this));

        return FixupList(caps);
    }

    public void UnpackCaps(P6any into) {
        List<VarDeque> posr = new List<VarDeque>();
        posr.Add(new VarDeque());
        Dictionary<string,VarDeque> namr = new Dictionary<string,VarDeque>();
        CapInfo it = captures;

        while (it != null) {
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
            it = it.prev;
        }

        if (posr[0].Count() == 0)
            posr[0].Unshift(Kernel.NewROScalar(this));

        VarHash nam = new VarHash();
        Variable[] pos = new Variable[posr.Count];

        foreach (KeyValuePair<string, VarDeque> kv in namr)
            nam[kv.Key] = FixupList(kv.Value);
        for (int i = 0; i < pos.Length; i++)
            pos[i] = FixupList(posr[i]);

        into.SetSlot("positionals", pos);
        into.SetSlot("named", nam);
    }

    public Variable O(VarHash caps) {
        Cursor nw = At(pos);
        foreach (KeyValuePair<string,Variable> kv in caps)
            nw.captures = new CapInfo(nw.captures, new string[] { kv.Key },
                    Kernel.NewMuScalar(kv.Value.Fetch()));
        VarDeque ks = new VarDeque();

        P6opaque lst = new P6opaque(Kernel.ListMO);
        lst.slots[0 /*items*/] = ks;
        lst.slots[1 /*rest*/ ] = new VarDeque();

        ks.Push(Kernel.NewROScalar(nw));
        return Kernel.NewRWListVar(lst);
    }

    public Variable SimpleWS() {
        string backing = global.orig_s;
        char[] backing_ca = global.orig_a;
        int l = backing_ca.Length;
        int p = pos;

        VarDeque ks = new VarDeque();

        P6opaque lst = new P6opaque(Kernel.ListMO);
        lst.slots[0 /*items*/] = ks;
        lst.slots[1 /*rest*/ ] = new VarDeque();

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

    [Immutable] public static readonly CC Word  = new CC(new int[] { 0, MAlNum,
            '_', MAll, '_'+1, MAlNum });

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

public sealed class NFA {
    public sealed class Node {
        public int fate;
        public bool final;
        public List<Edge> edges_l = new List<Edge>();
        public Edge[] edges;
        public Node(int curfate) { fate = curfate; }

        public override string ToString() {
            return "(" + fate + ")" + (final ? "+ " : " ") +
                Kernel.JoinS(", ", edges_l);
        }
    }

    public struct Edge {
        public int to;
        public CC when; // null if epsilon

        public override string ToString() {
            return ((when != null) ? when.ToString() : "ε") + " => " + to;
        }
    }

    public List<Node> nodes_l = new List<Node>();
    public Node[] nodes;
    public int curfate;

    public STable cursor_class;
    public HashSet<string> name_stack = new HashSet<string>();
    public HashSet<string> used_methods = new HashSet<string>();
    public List<Frame>   outer_stack = new List<Frame>();
    public List<SubInfo> info_stack  = new List<SubInfo>();

    public int AddNode() {
        nodes_l.Add(new Node(curfate));
        return nodes_l.Count - 1;
    }
    public void AddEdge(int from, int to, CC when) {
        Edge e;
        e.to = to;
        e.when = when;
        nodes_l[from].edges_l.Add(e);
    }

    public void Dump() {
        for (int ix = 0; ix < nodes_l.Count; ix++) {
            Console.WriteLine(ix + ": " + nodes_l[ix].ToString());
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
            foreach (NFA.Edge e in nodes[val].edges) {
                if (e.when == null) {
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
        greybuf = new int[nodes.Length];
        foreach (Node n in nodes)
            n.edges = n.edges_l.ToArray();
    }

    public Dictionary<LexerState,LexerState> dfashare
        = new Dictionary<LexerState,LexerState>();
}

// ltm automaton descriptors
public abstract class LAD : IFreeze {
    public abstract void ToNFA(NFA pad, int from, int to);
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

    public override void ToNFA(NFA pad, int from, int to) {
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
    public override void ToNFA(NFA pad, int from, int to) {
        pad.AddEdge(from, to, cc);
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
    public override void ToNFA(NFA pad, int from, int to) {
        int knot = pad.AddNode();
        pad.nodes_l[knot].final = true;
        pad.AddEdge(from, knot, null);
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
    public override void ToNFA(NFA pad, int from, int to) {
        pad.AddEdge(from, to, null);
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
    public override void ToNFA(NFA pad, int from, int to) {
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
    public override void ToNFA(NFA pad, int from, int to) {
        pad.AddEdge(from, to, CC.All);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "dot");
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADDot);
    }
}

public class LADStar : LAD {
    public LAD child;
    public LADStar(LAD child) { this.child = child; }
    private LADStar() {}

    public override LAD Reify(NFA pad) { return new LADStar(child.Reify(pad)); }
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
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADStar);
        fb.ObjRef(child);
    }
    internal static object Thaw(ThawBuffer tb) {
        var n = new LADStar();
        tb.Register(n);
        n.child = (LAD) tb.ObjRef();
        return n;
    }
}

public class LADOpt : LAD {
    public LAD child;
    public override LAD Reify(NFA pad) { return new LADOpt(child.Reify(pad)); }
    public LADOpt(LAD child) { this.child = child; }
    private LADOpt() {}

    public override void ToNFA(NFA pad, int from, int to) {
        pad.AddEdge(from, to, null);
        child.ToNFA(pad, from, to);
    }

    public override void Dump(int indent) {
        Console.WriteLine(new string(' ', indent) + "opt:");
        child.Dump(indent + 4);
    }
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADOpt);
        fb.ObjRef(child);
    }
    internal static object Thaw(ThawBuffer tb) {
        var n = new LADOpt();
        tb.Register(n);
        n.child = (LAD) tb.ObjRef();
        return n;
    }
}

public class LADPlus : LAD {
    public LAD child;
    public override LAD Reify(NFA pad) { return new LADPlus(child.Reify(pad)); }
    public LADPlus(LAD child) { this.child = child; }
    private LADPlus() { }

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
    public override void Freeze(FreezeBuffer fb) {
        fb.Byte((byte) SerializationCode.LADPlus);
        fb.ObjRef(child);
    }
    internal static object Thaw(ThawBuffer tb) {
        var n = new LADPlus();
        tb.Register(n);
        n.child = (LAD) tb.ObjRef();
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

    public override void ToNFA(NFA pad, int from, int to) {
        foreach (LAD k in zyg)
            k.ToNFA(pad, from, to);
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

    public override void ToNFA(NFA pad, int from, int to) {
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

    public override void ToNFA(NFA pad, int from, int to) {
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
    public override void ToNFA(NFA pad, int from, int to) {
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
            pad.outer_stack.Add((Frame)cands[i].GetSlot("outer"));
            pad.info_stack.Add((SubInfo)cands[i].GetSlot("info"));
            opts[i] = ((SubInfo)cands[i].GetSlot("info")).ltm.Reify(pad);
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
        this.nstates = new int[(nf.nodes.Length + 31) >> 5];
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
                foreach (NFA.Edge e in nf.nodes[32*i + j].edges) {
                    if (e.when != null && e.when.Accepts(ch))
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
        for (int i = nf.nodes.Length - 1; i >= 0; i--) {
            if ((nstates[i >> 5] & (1 << (i & 31))) != 0) {
                NFA.Node n = nf.nodes[i];
                if (n.final) {
                    if (Lexer.LtmTrace)
                        Console.WriteLine("+ Adding fate {0}", n.fate);
                    l.NoteFate(n.fate);
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

    public static Lexer GetDispatchLexer(Frame fromf, STable kl, SubInfo disp) {
        LexerCache lc = kl.GetLexerCache();
        Lexer ret;
        if (lc.dispatch_nfas.TryGetValue(disp, out ret))
            return ret;
        if (lc.parent != null && lc.parent.mo.name != "Cursor" && lc.parent.mo.name != "Any") {
            ret = GetDispatchLexer(fromf, lc.parent.mo, disp);
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
            pad.outer_stack.Add((Frame)cands[i].GetSlot("outer"));
            pad.info_stack.Add((SubInfo)cands[i].GetSlot("info"));
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
            pad.nodes_l[target].final = true;
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

        int cursor = 0;
        for (int i = 0; i < usedfates; i++) {
            cursor = fatebuffer[cursor * 2];
            uniqfates[i] = cursor - 1;
            if (LtmTrace)
                Console.WriteLine("+ Useful fate: {0}", cursor - 1);
        }
        return uniqfates;
    }

    public static P6any[] RunDispatch(Frame fromf, P6any cursor) {
        STable kl = cursor.mo;
        while (fromf.info.param == null ||
                fromf.info.param[0] == null) fromf = fromf.outer;

        Lexer l = GetDispatchLexer(fromf, kl, fromf.info);
        P6any[] candidates = (P6any[]) fromf.info.param[0];

        Cursor c = (Cursor)cursor;
        int[] brnum = l.Run(c.global.orig_s, c.pos);

        P6any[] ret = new P6any[brnum.Length];
        for (int i = 0; i < brnum.Length; i++)
            ret[i] = candidates[brnum[i]];

        return ret;
    }

    // XXX duplicates logic from RxOp::ProtoRedis and Op::RegexBody
    private static Frame StandardProtoC(Frame th) {
        switch (th.ip) {
            default:
                return Kernel.Die(th, "Invalid IP");
            case 1:
                return th.rx.Backtrack(th);
            case 0:
                th.rx = new RxFrame(th.info.name, (Cursor) ((Variable)th.lex0).Fetch(), false, false);
                th.rx.PushCutGroup("LTM");
                th.lex1 = RunDispatch(th, ((Variable)th.lex0).Fetch());
                th.lexi0 = 0;
                goto case 2;
            case 2:
                if (th.lexi0 == ((P6any[])th.lex1).Length)
                    goto case 1;
                th.rx.PushBacktrack(2);
                th.ip = 3;
                return (((P6any[])th.lex1)[th.lexi0++]).Invoke(th,
                    new Variable[] { Kernel.NewROScalar(th.rx.MakeCursor()) }, null);
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

    public static P6any MakeDispatcher(string name, P6any[] cands) {
        SubInfo si = new SubInfo(name, StandardProtoC);
        si.param = new object[] { cands, null };
        si.sig_i = new int[3] { SubInfo.SIG_F_POSITIONAL, 0, 0 };
        si.sig_r = new object[1] { "self" };
        si.ltm = new LADDispatcher();
        return Kernel.MakeSub(si, null);
    }
}
