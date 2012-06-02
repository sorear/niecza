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
        string kind;
        public Yada(Cursor c, string kind) : base(c) { this.kind = kind; }

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

    // forward defined
    class Lexical : Op {
        internal readonly string name;
        internal bool state_decl, list, hash; // XXX are these still needed?

        public Lexical(Cursor c, string name) : base(c) { this.name = name; }
        protected override CgOp code(SubInfo body) { return CgOp.scopedlex(name); }
    }

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
