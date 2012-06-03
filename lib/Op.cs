using System;
using System.Collections.Generic;

using Niecza;
using Niecza.Compiler;

namespace Niecza.Compiler.Op {
    abstract class Op {
        public readonly Cursor pos;

        protected Op(Cursor pos) { this.pos = pos; }

        // These are just placeholders until more of the system is online
        public int LineOf(Cursor c) { return 0; }
        public void Sorry(Cursor c, string msg) { throw new NotImplementedException(); }
        public int GenId() { throw new NotImplementedException(); }
        public string GenSym() { throw new NotImplementedException(); }
        public SubInfo GetCurSub() { throw new NotImplementedException(); }
        public void SetRunOnce(SubInfo s) { throw new NotImplementedException(); }
        public LexInfo LookupLex(SubInfo s, string n) { throw new NotImplementedException(); }
        public STable CompileGetPkg(SubInfo s, string n) { throw new NotImplementedException(); }
        public SubInfo ThunkSub(Op body, string[] args) { throw new NotImplementedException(); }
        public Lexical BlockExpr(Cursor at, SubInfo blk) { throw new NotImplementedException(); }
        public Op BetaCall(Cursor at, string name, params Op[] pos) { throw new NotImplementedException(); }

        // replaces both zyg and ctxzyg
        public virtual Op VisitOps(Func<Op,Op> post) { return post(this); }

        public CgOp cgop(SubInfo body) {
            if (pos != null) {
                return CgOp.ann(LineOf(pos), code(body));
            } else {
                return code(body);
            }
        }

        // minorly ick
        public CgOp cgop_statement(SubInfo body) {
            if (pos != null) {
                return CgOp.ann(LineOf(pos), CgOp.statement(code(body)));
            } else {
                return CgOp.statement(code(body));
            }
        }

        public CgOp cgop_labelled(SubInfo body, string label) {
            if (pos != null) {
                return CgOp.ann(LineOf(pos), code_labelled(body, label));
            } else {
                return code_labelled(body, label);
            }
        }

        public virtual Op to_bind(Cursor at, bool ro, Op rhs) {
            Sorry(at, "Cannot use bind operator with this LHS");
            return new StatementList(null, new Op[0]);
        }

        protected abstract CgOp code(SubInfo body);
        protected virtual  CgOp code_labelled(SubInfo body, string label) {
            return code(body);
        }

        public virtual Op statement_level(Cursor at) { return this; }
        public virtual Op semilist_level(Cursor at) { return this; }
        public virtual bool onlystub() { return false; }
        public virtual Variable const_value(SubInfo body) { return null; }
    }

    class RawCgOp : Op {
        object root;

        public RawCgOp(Cursor c, object root_) : base(c) { root = root_; }

        object dovisit(object node, Func<Op,Op> post) {
            if (node is Op)
                return ((Op)node).VisitOps(post);
            object[] na = node as object[];
            if (na != null)
                for (int i = 0; i < na.Length; i++)
                    na[i] = dovisit(na[i], post);
            return node;
        }
        public override Op VisitOps(Func<Op,Op> post) {
            root = dovisit(root, post);
            return post(this);
        }

        object code(object node, SubInfo body) {
            if (node is Op) return ((Op)node).cgop(body);
            object[] na = (object[])node;
            if (na == null) return node;
            object[] pna = new object[na.Length - 1];
            for (int i = 1; i < na.Length; i++)
                pna[i-1] = code(na[i], body);
            string kind = (string)na[0];
            if (kind == "subcall")
                return CgOp.subcall(pna);
            if (kind == "methodcall")
                return CgOp.methodcall(pna);
            if (kind == "rnull")
                return CgOp.rnull((CgOp)pna[0]);
            if (kind == "string_var")
                return CgOp.string_var((string)pna[0]);
            return CgOp.N(kind, pna);
        }
        protected override CgOp code(SubInfo body) {
            return (CgOp)code(root, body);
        }
    }

    class StatementList : Op {
        Op[] children;
        bool statement;
        public StatementList(Cursor c, Op[] children, bool stmt = false) : base(c) {
            this.children = children; statement = stmt;
        }
        public override Op VisitOps(Func<Op,Op> post) {
            for (int i = 0; i < children.Length; i++)
                children[i] = children[i].VisitOps(post);
            return post(this);
        }
        public override bool onlystub() {
            return children.Length == 1 && children[0].onlystub();
        }
        public override Variable const_value(SubInfo body) {
            return children.Length == 1 ? children[0].const_value(body) : null;
        }
        protected override CgOp code(SubInfo body) {
            if (children.Length == 0) return CgOp.corelex("Nil");
            CgOp[] bits = new CgOp[children.Length];
            for (int i = 0; i < bits.Length; i++) {
                bits[i] = statement ? children[i].cgop_statement(body) :
                    children[i].cgop(body);
                if (i < bits.Length - 1) bits[i] = CgOp.sink(bits[i]);
            }
            return CgOp.prog(bits);
        }
    }

    abstract class CallLike: Op {
        protected bool posonly;
        protected Op[] args;

        protected CallLike(Cursor c, bool p, Op[] a) :base(c) {
            posonly = p;
            args = a;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            for (int i = 0; i < args.Length; i++)
                args[i] = args[i].VisitOps(post);
            return post(this);
        }

        public Op[] getargs() {
            if (!posonly) return args;
            Op[] ret = new Op[args.Length];
            for (int i = 0; i < ret.Length; i++)
                ret[i] = new Paren(pos, args[i]);
            return ret;
        }

        public object[] argblock(SubInfo body, params object[] pre) {
            List<object> ret = new List<object>(pre);
            foreach (Op a in args) {
                SimplePair s;
                CallSub c;
                Lexical l;
                if (!posonly && (s = a as SimplePair) != null) {
                    ret.Add(":" + s.key);
                    ret.Add(s.value.cgop(body));
                } else if (!posonly && (c = a as CallSub) != null &&
                        (l = c.invocant as Lexical) != null &&
                        l.name == "&prefix:<|>" && c.args.Length == 1) {
                    ret.Add("flatcap");
                    ret.Add(CgOp.fetch(CgOp.methodcall(c.args[0].cgop(body),
                        "Capture")));
                } else {
                    ret.Add(a.cgop(body));
                }
            }
            return ret.ToArray();
        }
    }

    class CallSub : CallLike {
        public Op invocant;

        public CallSub(Cursor c, Op i, bool po, Op[] a) : base(c,po,a) {
            invocant = i;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            invocant = invocant.VisitOps(post);
            return base.VisitOps(post);
        }

        public Op adverb(Op adv) {
            return new CallSub(pos, invocant, posonly,
                Utils.AppendArr(getargs(), adv));
        }

        protected override CgOp code(SubInfo body) {
            return CgOp.subcall(argblock(body,CgOp.fetch(invocant.cgop(body))));
        }

        public override Op to_bind(Cursor at, bool ro, Op rhs) {
            Lexical l;
            if ((l = invocant as Lexical) != null &&
                    l.name.Substring(0,15) == "&postcircumfix:") {
                return adverb(new SimplePair(pos, "BIND_VALUE",
                    ro ? new ROify(pos, rhs) : rhs));
            }
            return base.to_bind(at,ro,rhs);
        }
    }

    class CallMethod : CallLike {
        // data rep here badly needs a refactor.
        Op receiver;
        object name; // Op | Str
        bool is_private;
        STable pclass;
        string meta;

        public CallMethod(Cursor pos, Op rcvr, object name, bool pvt,
                STable pcl, string mt, bool po, Op[] a) : base(pos,po,a) {
            receiver = rcvr; this.name = name; is_private = pvt;
            pclass = pcl; meta = mt;
        }

        public Op adverb(Op adv) {
            return new CallMethod(pos, receiver, name, is_private, pclass,
                   meta, posonly, Utils.AppendArr(getargs(), adv));
        }

        public override Op VisitOps(Func<Op,Op> post) {
            receiver = receiver.VisitOps(post);
            if (name is Op) name = ((Op)name).VisitOps(post);
            return base.VisitOps(post);
        }

        protected override CgOp code(SubInfo body) {
            CgOp cn = (name is Op) ? CgOp.obj_getstr((name as Op).cgop(body)) :
                CgOp.str((string)name);
            if (is_private) {
                var kl = CgOp.class_ref("mo", pclass);
                if (pclass.mo.type == P6how.PARAMETRIZED_ROLE)
                    kl = CgOp.obj_llhow(CgOp.fetch(CgOp.scopedlex("$?CLASS")));
                return CgOp.subcall(argblock(body, CgOp.stab_privatemethod(kl,
                                cn), receiver.cgop(body)));
            } else if (meta == "^") {
                return CgOp.Let(receiver.cgop(body), (CgOp r) =>
                    CgOp.methodcall(argblock(body, CgOp.how(CgOp.fetch(r)), cn, r)));
            } else if (meta == "?") {
                // TODO maybe use a lower-level check
                return CgOp.Let(receiver.cgop(body), (CgOp r) =>
                    CgOp.Let(cn, (CgOp n) => CgOp.ternary(
                        CgOp.obj_getbool(CgOp.methodcall(
                            CgOp.how(CgOp.fetch(r)), "can", r,
                            CgOp.box("Str",n))),
                        CgOp.methodcall(argblock(body, r, n)),
                        CgOp.scopedlex("Nil"))));
            } else if (pclass != null) {
                return CgOp.methodcall(argblock(body, receiver.cgop(body),
                    "dispatch:<::>", CgOp.class_ref("typeObj", pclass),
                    CgOp.box("Str", cn)));
            } else {
                return CgOp.methodcall(argblock(body, receiver.cgop(body), cn));
            }
        }
    }

    class GetSlot : Op {
        Op obj;
        string name;
        STable type;

        public GetSlot(Cursor c, Op ob, string n, STable t) : base(c) {
            obj = ob; name = n; type = t;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            obj = obj.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            var kl = CgOp.class_ref("mo", type);
            if (type.mo.type == P6how.PARAMETRIZED_ROLE)
                kl = CgOp.obj_llhow(CgOp.fetch(CgOp.scopedlex("$?CLASS")));
            return CgOp.getslot(kl, name, "var", CgOp.fetch(obj.cgop(body)));
        }

        public override Op to_bind(Cursor at, bool ro, Op rhs) {
            return new SetSlot(pos, obj, name, type,
                    ro ? new ROify(pos,rhs) : rhs);
        }
    }

    class SetSlot : Op {
        Op obj;
        string name;
        STable type;
        Op value;

        public SetSlot(Cursor c, Op ob, string n, STable t, Op v) : base(c) {
            obj = ob; name = n; type = t; value = v;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            obj = obj.VisitOps(post);
            value = value.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            var kl = CgOp.class_ref("mo", type);
            if (type.mo.type == P6how.PARAMETRIZED_ROLE)
                kl = CgOp.obj_llhow(CgOp.fetch(CgOp.scopedlex("$?CLASS")));
            return CgOp.Let(value.cgop(body), (CgOp v) =>
                CgOp.prog(CgOp.setslot(kl, name, "var",
                    CgOp.fetch(obj.cgop(body)), v), v));
        }
    }

    class Paren : Op {
        Op inside;
        public Paren(Cursor c, Op i) : base(c) { inside = i; }
        public override Op VisitOps(Func<Op,Op> post) {
            inside = inside.VisitOps(post);
            return post(this);
        }
        protected override CgOp code(SubInfo body) { return inside.cgop(body); }
        public override Op to_bind(Cursor at, bool ro, Op rhs) { return inside.to_bind(at, ro, rhs); }
        public override Variable const_value(SubInfo body) { return inside.const_value(body); }
    }

    class SimplePair : Op {
        internal readonly string key;
        internal Op value;

        public SimplePair(Cursor c, string k, Op v) : base(c) { key=k;value=v; }

        public override Op VisitOps(Func<Op,Op> post) {
            value = value.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return CgOp.N("pair", CgOp.@const(CgOp.string_var(key)),
                    value.cgop(body));
        }
    }

    class SimpleParcel : Op {
        Op[] items;
        public SimpleParcel(Cursor c, Op[] its) : base(c) { items = its; }

        public override Op VisitOps(Func<Op,Op> post) {
            for (int i = 0; i < items.Length; i++)
                items[i] = items[i].VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            object[] args = new object[items.Length];
            for (int i = 0; i < args.Length; i++)
                args[i] = items[i].cgop(body);
            return CgOp.N("comma", args);
        }
    }

    class Interrogative : Op {
        Op receiver;
        string name;

        public Interrogative(Cursor c, Op r, string n) : base(c) {
            receiver = r; name = n;
        }
        public override Op VisitOps(Func<Op,Op> post) {
            receiver = receiver.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            if (name == "VAR")
                return CgOp.var_get_var(receiver.cgop(body));
            var c = CgOp.fetch(receiver.cgop(body));
            if (name == "HOW") { c = CgOp.how(c); }
            else if (name == "WHO") { c = CgOp.who(c); }
            else if (name == "WHAT") { c = CgOp.obj_what(c); }
            else { throw new NotImplementedException("invalid interrogative"); }
            return c;
        }
    }

    class HereStub : Op {
        internal Op node;

        public HereStub(Cursor c) : base(c) {}

        // TODO: handle cases of too-late definition more cleanly.
        public override Op VisitOps(Func<Op,Op> post) {
            node = node.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) { return node.cgop(body); }
    }

    class Yada : Op {
        public Yada(Cursor c, string kind) : base(c) { }

        public override bool onlystub() { return true; }
        protected override CgOp code(SubInfo body) { return CgOp.die(">>>Stub code executed"); }
    }

    class ShortCircuit : Op {
        int kind;
        Op[] args;

        public const int AND  = 0;
        public const int OR   = 1;
        public const int DAND = 2;
        public const int DOR  = 3;

        public ShortCircuit(Cursor c, int k, Op[] a) : base(c) {
            kind = k; args = a;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            for (int i = 0; i < args.Length; i++)
                args[i] = args[i].VisitOps(post);
            return post(this);
        }

        CgOp red2(CgOp left, CgOp right) {
            switch (kind) {
                case AND:
                    return CgOp.ternary(CgOp.obj_getbool(left), right, left);
                case OR:
                    return CgOp.ternary(CgOp.obj_getbool(left), left, right);
                case DAND:
                    return CgOp.ternary(CgOp.obj_getdef(left), right, left);
                case DOR:
                    return CgOp.ternary(CgOp.obj_getdef(left), left, right);
                default:
                    throw new NotImplementedException();
            }
        }

        protected override CgOp code(SubInfo body) {
            var acc = args[args.Length - 1].cgop(body);

            for (int ix = args.Length - 2; ix >= 0; ix--)
                acc = CgOp.Let(args[ix].cgop(body), (CgOp v) => red2(v,acc));

            return acc;
        }
    }

    // XXX obsolescent
    class ShortCircuitAssign : Op {
        int kind;
        Op left, right;

        public ShortCircuitAssign(Cursor c, int k, Op l, Op r) : base(c) {
            kind = k; left = l; right = r;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            left = left.VisitOps(post);
            right = right.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return (new ShortCircuit(pos, kind, new [] { left,
                new Assign(pos, left, right) })).cgop(body);
        }
    }

    class StringLiteral : Op {
        string text;

        public StringLiteral(Cursor c, string t) : base(c) { text = t; }
        protected override CgOp code(SubInfo body) { return CgOp.@const(CgOp.string_var(text)); }
        public override Variable const_value(SubInfo body) { return Builtins.MakeStr(text); }
    }

    class Conditional : Op {
        Op check, iftrue, iffalse;

        public Conditional(Cursor c, Op i, Op t, Op f) : base(c) {
            check = i; iftrue = t; iffalse = f;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            check = check.VisitOps(post);
            if (iftrue != null) iftrue = iftrue.VisitOps(post);
            if (iffalse != null) iffalse = iffalse.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return CgOp.ternary(CgOp.obj_getbool(check.cgop(body)),
                iftrue != null ? iftrue.cgop(body) : CgOp.corelex("Nil"),
                iffalse != null ? iffalse.cgop(body) : CgOp.corelex("Nil"));
        }
    }

    class WhileLoop : Op {
        Op check, body;
        bool once, until, need_cond;

        public WhileLoop(Cursor p, Op c, Op b, bool o, bool u, bool n = false)
                : base(p) {
            check = c; body = b; once = o; until = u; need_cond = n;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            check = check.VisitOps(post);
            body  = body.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return code_labelled(body, "");
        }
        protected override CgOp code_labelled(SubInfo sub, string l) {
            var id = GenId();
            var cond = need_cond ?
                CgOp.prog(CgOp.letvar("!cond", check.cgop(sub)),
                        CgOp.obj_getbool(CgOp.letvar("!cond"))) :
                CgOp.obj_getbool(check.cgop(sub));
            var loop = CgOp.prog(
                CgOp.whileloop(until?1:0, once?1:0, cond,
                    CgOp.sink(CgOp.xspan("redo"+id,"next"+id,0,body.cgop(sub),
                        1, l, "next"+id, 2, l, "last"+id, 3, l, "redo"+id))),
                CgOp.label("last"+id),
                CgOp.corelex("Nil"));
            return need_cond ? CgOp.letn("!cond", CgOp.scopedlex("Any"), loop) :
                loop;
        }
    }

    class GeneralLoop : Op {
        Op init, cond, step, body;

        public GeneralLoop(Cursor p, Op i, Op c, Op s, Op l) : base(p) {
            init = i; cond = c; step = s; body = l;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            init = init.VisitOps(post);
            cond = cond.VisitOps(post);
            step = step.VisitOps(post);
            body = body.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return code_labelled(body, "");
        }
        protected override CgOp code_labelled(SubInfo sub, string l) {
            var id = GenId();

            return CgOp.prog(
                init != null ? CgOp.sink(init.cgop(sub)) : CgOp.noop(),
                CgOp.whileloop(0, 0,
                    (cond != null ? CgOp.obj_getbool(cond.cgop(sub)) :
                        CgOp.@bool(1)),
                    CgOp.prog(
                        CgOp.sink(CgOp.xspan("redo"+id, "next"+id, 0,
                            body.cgop(sub), 1, l, "next"+id,
                            2, l, "last"+id, 3, l, "redo"+id)),
                        (step != null ? CgOp.sink(step.cgop(sub)) : CgOp.noop()))),
                CgOp.label("last"+id),
                CgOp.corelex("Nil"));
        }
    }

    class ForLoop : Op {
        Op source;
        string sink;

        public ForLoop(Cursor p, Op src, string snk) : base(p) {
            source = src; sink = snk;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            source = source.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return CgOp.methodcall(CgOp.subcall(CgOp.fetch(
                CgOp.corelex("&flat")), source.cgop(body)), "map",
                    CgOp.scopedlex(sink));
        }

        public override Op statement_level(Cursor at) {
            var body = ((LISub)LookupLex(GetCurSub(), sink)).def;
            var vars = new string[Builtins.sig_count(body.sig)];
            var args = new Op[vars.Length];
            for (int i = 0; i < vars.Length; i++) {
                vars[i] = GenSym();
                args[i] = new LetVar(pos, vars[i]);
            }
            return new ImmedForLoop(pos, source, vars, BetaCall(at,sink,args));
        }
    }

    // A for-loop which must be run immediately because it is at statement
    // level, as contrasted with a ForLoop, which turns into a map call.
    // ForLoops turn into this at statement level; if a ForLoop is in any
    // other context, it is regarded as a lazy comprehension.
    class ImmedForLoop : Op {
        Op source;
        string[] vars;
        Op sink;

        public ImmedForLoop(Cursor c, Op src, string[] vs, Op snk) : base(c) {
            source = src;
            vars = vs;
            sink = snk;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            source = source.VisitOps(post);
            sink = sink.VisitOps(post);
            return post(this);
        }

        // used only with 'foo ($_ for 1,2,3)'
        // vars will be a single gensym
        public override Op semilist_level(Cursor at) {
            var aname = GenSym();
            var sub = ThunkSub(new Let(pos,vars[0], new Lexical(pos,aname),
                        sink), new [] { aname });
            var sname = BlockExpr(at, sub).name;

            return new ForLoop(pos, source, sname);
        }

        protected override CgOp code(SubInfo body) { return code_labelled(body, ""); }
        protected override CgOp code_labelled(SubInfo body, string l) {
            var id = GenId();

            var letargs = new List<object>();
            letargs.Add("!iter"+id);
            letargs.Add(CgOp.start_iter(source.cgop(body)));
            foreach (string v in vars) {
                letargs.Add(v);
                letargs.Add(CgOp.@null("var"));
            }
            letargs.Add(CgOp.label("again"+id));
            foreach (string v in vars) {
                letargs.Add(CgOp.ncgoto("last"+id, CgOp.iter_hasflat(
                    CgOp.letvar("!iter"+id))));
                letargs.Add(CgOp.letvar(v,CgOp.vvarlist_shift(CgOp.letvar("!iter"+id))));
            }
            letargs.Add(CgOp.sink(CgOp.xspan("redo"+id,"next"+id,0,
                sink.cgop(body), 1, l, "next"+id,
                2, l, "last"+id, 3, l, "redo"+id)));
            letargs.Add(CgOp.@goto("again"+id));
            letargs.Add(CgOp.label("last"+id));
            return CgOp.rnull(CgOp.letn(letargs.ToArray()));
        }
    }

    class Labelled : Op {
        string name;
        Op stmt;

        public Labelled(Cursor c, string n, Op s) : base(c) { name=n; stmt=s; }

        public override Op VisitOps(Func<Op,Op> post) {
            stmt = stmt.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return CgOp.prog(CgOp.label("goto_"+name),
                stmt.cgop_labelled(body, name));
        }

        public override Op statement_level(Cursor at) {
            return new Labelled(pos, name, stmt.statement_level(at));
        }
    }

    class When : Op {
        Op match, body;

        public When(Cursor c, Op m, Op b) : base(c) { match=m; body=b; }

        public override Op VisitOps(Func<Op,Op> post) {
            match = match.VisitOps(post);
            body = body.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo sub) {
            var id = GenId();

            return CgOp.ternary(CgOp.obj_getbool(CgOp.methodcall(
                        match.cgop(sub), "ACCEPTS", CgOp.scopedlex("$_"))),
                CgOp.xspan("start"+id, "end"+id, 0, CgOp.prog(
                        CgOp.control(SubInfo.ON_SUCCEED, CgOp.@null("frame"),
                            CgOp.@int(-1), CgOp.@null("str"), body.cgop(sub))),
                    SubInfo.ON_PROCEED, "", "end"+id),
                CgOp.corelex("Nil"));
        }
    }

    class Start : Op {
        string condvar;
        Op stmt;

        public Start(Cursor c, string v, Op s) : base(c) { condvar=v; stmt=s; }

        public override Op VisitOps(Func<Op,Op> post) {
            stmt = stmt.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return CgOp.ternary(CgOp.obj_getbool(CgOp.scopedlex(condvar)),
                CgOp.corelex("Nil"),
                CgOp.prog(CgOp.sink(CgOp.assign(CgOp.scopedlex(condvar),
                    CgOp.box("Bool", CgOp.@bool(1)))), stmt.cgop(body)));
        }
    }

    class Try : Op {
        Op body;

        public Try(Cursor c, Op b) : base(c) { body = b; }

        public override Op VisitOps(Func<Op,Op> post) {
            body = body.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo sub) {
            var id = GenId();
            return CgOp.xspan("start"+id, "end"+id, 1, body.cgop(sub),
                SubInfo.ON_DIE, "", "end"+id);
        }
    }

    class Control : Op {
        int type;
        string name;
        Op payload;

        public Control(Cursor c, int k, string n, Op b) : base(c) {
            type = k; name = n; payload = b;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            payload = payload.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return CgOp.control(type, CgOp.@null("frame"), CgOp.@int(-1),
                (name == "" ? CgOp.@null("str") : CgOp.str(name)),
                payload.cgop(body));
        }
    }

    class MakeJunction : Op {
        int type;
        Op[] args;

        public MakeJunction(Cursor c, int k, Op[] a) : base(c) { type=k; args=a; }

        public override Op VisitOps(Func<Op,Op> post) {
            for (int i = 0; i < args.Length; i++)
                args[i] = args[i].VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            object[] jargs = new object[args.Length+1];
            jargs[0] = type;
            for (int i = 0; i < args.Length; i++)
                jargs[i+1] = args[i].cgop(body);
            return CgOp.makejunction(jargs);
        }
    }

    // XXX needs better typing
    class Num : Op {
        object value;

        public Num(Cursor c, object v) : base(c) { value = v; }

        protected override CgOp code(SubInfo body) {
            if (value is Array) {
                return CgOp.@const(CgOp.exactnum((object[])value));
            } else {
                return CgOp.@const(CgOp.box("Num", CgOp.@double(value)));
            }
        }

        public override Variable const_value(SubInfo body) {
            var ar = value as object[];
            if (ar != null) {
                return EmitUnit.ExactNum((int)ar[0], (string)ar[1]);
            } else {
                return Builtins.MakeFloat(value is double ? (double)value : (int)value);
            }
        }
    }

    // just a little hook for rewriting
    class Attribute : Op {
        internal string name;
        internal STable pkg;

        public Attribute(Cursor c, string n, STable p) : base(c) {name=n;pkg=p;}

        protected override CgOp code(SubInfo body) { return CgOp.corelex("Nil"); }
    }

    class Whatever : Op {
        public Whatever(Cursor c) : base(c) { }

        protected override CgOp code(SubInfo body) { return CgOp.corelex("$__Whatever"); }
    }

    class WhateverCode : Op {
        string slot;
        public WhateverCode(Cursor c, string s) : base(c) { slot = s; }

        protected override CgOp code(SubInfo body) { return CgOp.scopedlex(slot); }
    }

    class BareBlock : Op {
        string slot;
        public BareBlock(Cursor c, string s) : base(c) { slot = s; }

        protected override CgOp code(SubInfo body) { return CgOp.scopedlex(slot); }
        public override Op statement_level(Cursor at) {
            SetRunOnce(((LISub)LookupLex(GetCurSub(), slot)).def);
            return BetaCall(at,slot);
        }
    }

    // SubDef: actually completely unused!

    class Lexical : Op {
        internal readonly string name;

        public Lexical(Cursor c, string name) : base(c) { this.name = name; }
        protected override CgOp code(SubInfo body) { return CgOp.scopedlex(name); }

        public override Variable const_value(SubInfo body) {
            var li = LookupLex(body, name);
            if (li is LICommon) {
                StashEnt se;
                Kernel.currentGlobals.TryGetValue(((LICommon)li).hkey, out se);
                if (se.constant) return se.v;
            }
            return (li is LIConstant) ? ((LIConstant)li).value : null;
        }

        public override Op to_bind(Cursor at, bool ro, Op rhs) {
            var lex = LookupLex(GetCurSub(), name);
            if (lex == null) {
                Sorry(at, "Cannot find definition for binding???");
                return new StatementList(null, new Op[0]);
            }
            var list = false;
            var type = CompileGetPkg(GetCurSub(), "Mu");
            string realname = null;
            var lex_s = lex as LISimple;
            if (lex_s != null) {
                list = (lex_s.flags & (LISimple.LIST | LISimple.HASH)) != 0;
                type = lex_s.type;
            }
            else if (lex is LICommon)
                realname = ((LICommon)lex).VarName();
            else if (lex is LIAttrAlias)
                realname = ((LIAttrAlias)lex).aname;
            else
                return base.to_bind(at,ro,rhs);
            if (realname != null)
                list = realname[0] == '@' || realname[0] == '%';
            return new LexicalBind(pos,name,ro,list,type,rhs);
        }
    }

    class ConstantDecl : Op {
        internal string name;
        internal bool init;

        public ConstantDecl(Cursor c, string n, bool i) :base(c){name=n;init=i;}

        protected override CgOp code(SubInfo sub) { return CgOp.scopedlex(name); }
    }

    class ContextVar : Op {
        string name;
        int uplevel;

        public ContextVar(Cursor c,string n,int u) : base(c) {name=n;uplevel=u;}

        protected override CgOp code(SubInfo sub) { return CgOp.context_get(name, uplevel); }
    }

    class Require : Op {
        string unit;

        public Require(Cursor c, string u) : base(c) { unit = u; }

        protected override CgOp code(SubInfo sub) { return CgOp.rnull(CgOp.do_require(unit)); }
    }

    class Take : Op {
        Op value;
        public Take(Cursor c, Op v) : base(c) { value = v; }

        public override Op VisitOps(Func<Op,Op> post) {
            value = value.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) { return CgOp.take(value.cgop(body)); }
    }

    class Gather : Op {
        string var;
        public Gather(Cursor c, string v) : base(c) { var = v; }

        protected override CgOp code(SubInfo body) {
            // construct a frame for our sub ip=0
            // construct a GatherIterator with said frame
            // construct a List from the iterator

            return CgOp.subcall(CgOp.fetch(CgOp.corelex("&_gather")),
                CgOp.startgather(CgOp.fetch(CgOp.scopedlex(var))));
        }
    }

    class MakeCursor : Op {
        public MakeCursor(Cursor c) : base(c) { }

        protected override CgOp code(SubInfo body) {
            return CgOp.prog(CgOp.scopedlex("$/", CgOp.rxcall("MakeCursor")),
                CgOp.scopedlex("$/"));
        }
    }

    // Provides access to a variable with a scope smaller than the sub.  Used
    // internally in a few places; should not be exposed to the user, because
    // these can't be closed over.
    // the existance of these complicates cross-sub inlining a bit
    class LetVar : Op {
        string name;
        public LetVar(Cursor c, string n) : base(c) { name = n; }

        protected override CgOp code(SubInfo body) { return CgOp.letvar(name); }
    }

    // forward defined
    class Assign : Op {
        Op left, right;

        public Assign(Cursor c, Op l, Op r) : base(c) { left = l; right = r; }

        public override Op VisitOps(Func<Op,Op> post) {
            left = left.VisitOps(post);
            right = right.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return CgOp.assign(left.cgop(body), right.cgop(body));
        }
    }

    class Builtin: Op {
        string name;
        Op[] args;
        public Builtin(Cursor c, string n, Op[] a): base(c) { name=n;args=a; }

        public override Op VisitOps(Func<Op,Op> post) {
            for (int i = 0; i < args.Length; i++)
                args[i] = args[i].VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            object[] cops = new object[args.Length];
            for (int i = 0; i < args.Length; i++)
                cops[i] = args[i].cgop(body);
            return CgOp.N(name, args);
        }
    }

    class Let: Op {
        string var;
        Op to;
        Op @in;

        public Let(Cursor c, string v, Op t, Op i) : base(c) {var=v;to=t;@in=i;}

        public override Op VisitOps(Func<Op,Op> post) {
            to = to.VisitOps(post);
            @in = @in.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return CgOp.letn(var, to.cgop(body), @in.cgop(body));
        }
    }

    class LexicalBind: Op {
        string name;
        bool ro;
        bool list;
        STable type;
        Op rhs;

        public LexicalBind(Cursor c, string name, bool ro, bool ls,
                STable tp, Op r) : base(c) {
            this.name = name; this.ro = ro; list = ls; type = tp; rhs = r;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            rhs = rhs.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return CgOp.prog(
                CgOp.scopedlex(name, type == null ? rhs.cgop(body) :
                    CgOp.newboundvar(ro?1:0, list?1:0,
                        CgOp.class_ref("mo", type), rhs.cgop(body))),
                CgOp.scopedlex(name));
        }
    }

    class ROify : Op {
        Op child;
        public ROify(Cursor c, Op z) : base(c) { child = z; }
        public override Op VisitOps(Func<Op,Op> post) {
            child = child.VisitOps(post);
            return post(this);
        }
        protected override CgOp code(SubInfo body) {
            return CgOp.fetch(child.cgop(body));
        }
    }
}
