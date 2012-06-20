using System;
using System.Collections.Generic;

using Niecza;
using Niecza.Compiler;

namespace Niecza.Compiler.Op {
    abstract class Op {
        public readonly Cursor pos;

        protected Op(Cursor pos) { this.pos = pos; }

        // These are just placeholders until more of the system is online

        // replaces both zyg and ctxzyg
        public virtual Op VisitOps(Func<Op,Op> post) { return post(this); }

        public CgOp cgop(SubInfo body) {
            if (pos != null) {
                return CgOp.ann(CompUtils.LineOf(pos), code(body));
            } else {
                return code(body);
            }
        }

        // minorly ick
        public CgOp cgop_statement(SubInfo body) {
            if (pos != null) {
                return CgOp.ann(CompUtils.LineOf(pos), CgOp.statement(code(body)));
            } else {
                return CgOp.statement(code(body));
            }
        }

        public CgOp cgop_labelled(SubInfo body, string label) {
            if (pos != null) {
                return CgOp.ann(CompUtils.LineOf(pos), code_labelled(body, label));
            } else {
                return code_labelled(body, label);
            }
        }

        public virtual Op to_bind(Cursor at, bool ro, Op rhs) {
            CompUtils.Sorry(at, "Cannot use bind operator with this LHS");
            return new StatementList(at);
        }

        protected abstract CgOp code(SubInfo body);
        protected virtual  CgOp code_labelled(SubInfo body, string label) {
            return code(body);
        }

        public virtual Op statement_level(Cursor at) { return this; }
        public virtual Op semilist_level(Cursor at) { return this; }
        public virtual bool onlystub() { return false; }
        public virtual Variable const_value(SubInfo body) { return null; }

        class Simplifier {
            internal Actions actions;
            internal SubInfo body;

            static Dictionary<string,Func<Simplifier,string,CallSub,Op>> funcs =
                new Dictionary<string,Func<Simplifier,string,CallSub,Op>> {
                    { "&postcircumfix:<{ }>", do_atkey },
                    { "&postcircumfix:<[ ]>", do_atpos },
                    { "&last", do_nullary_control(2) },
                    { "&next", do_nullary_control(1) },
                    { "&proceed", do_nullary_control(7) },
                    { "&term:<proceed>", do_nullary_control(7) },
                    { "&redo", do_nullary_control(3) },

                    { "&infix:<&>", do_makejunction(0) },
                    { "&infix:<^>", do_makejunction(2) },
                    { "&infix:<|>", do_makejunction(3) },
                    { "&all", do_makejunction(8) },
                    { "&none", do_makejunction(9) },
                    { "&one", do_makejunction(10) },
                    { "&any", do_makejunction(11) },

                    { "&return", do_return_take },
                    { "&succeed", do_return_take },
                    { "&take", do_return_take },
                };

            Op[] no_named_params(CallSub op) {
                if (!op.posonly) {
                    foreach (Op arg in op.args) {
                        if (arg is SimplePair)
                            return null;
                        var argcs = arg as CallSub;
                        if (argcs != null && argcs.invocant is Lexical &&
                                ((Lexical)argcs.invocant).name == "&prefix:<|>")
                            return null;
                    }
                }

                return op.args;
            }

            bool capture_params(CallSub op, List<Op> pos, Dictionary<string,Op> named) {
                if (op.posonly) {
                    foreach (var a in op.args) pos.Add(a);
                    return true;
                }

                foreach (var a in op.args) {
                    var asp = a as SimplePair;
                    var acs = a as CallSub;
                    var ail = acs == null ? null : acs.invocant as Lexical;
                    if (asp != null) {
                        named[asp.key] = asp.value;
                    } else if (ail != null && ail.name == "&prefix:<|>") {
                        return false;
                    } else {
                        pos.Add(a);
                    }
                }

                return true;
            }

            static Op do_return_take(Simplifier th, string name, CallSub op) {
                var args = th.no_named_params(op);
                if (args == null) return op;
                var parcel = args.Length == 1 ? args[0] : args.Length == 0 ?
                    (Op)new Lexical(op.pos, "Nil") : new SimpleParcel(op.pos, args);
                return name == "&take" ? (Op)new Take(op.pos, parcel) :
                    new Control(op.pos, name == "&return" ? SubInfo.ON_RETURN :
                            SubInfo.ON_SUCCEED, null, parcel);
            }

            static Func<Simplifier,string,CallSub,Op> do_nullary_control(int number) {
                return (Simplifier th, string name, CallSub op) => {
                    var args = th.no_named_params(op);
                    return (args == null || args.Length != 0) ? (Op)op :
                        new Control(op.pos, number, null, new Lexical(op.pos, "Nil"));
                };
            }

            static Func<Simplifier,string,CallSub,Op> do_makejunction(int typecode) {
                return (Simplifier th, string name, CallSub op) => {
                    var args = th.no_named_params(op);
                    return args == null ? (Op)op : new MakeJunction(op.pos,
                        typecode, args);
                };
            }

            int getflag(Dictionary<string,Op> dict, string flag) {
                var val = dict.GetDefault(flag, null) as Lexical;
                if (!dict.Remove(flag)) return 0;
                if (val == null || val.name != "True") return -1;
                return 1;
            }

            static Op do_atkey(Simplifier th, string name, CallSub op) {
                var args = new List<Op>();
                var named = new Dictionary<string,Op>();
                if (!th.capture_params(op, args, named) || args.Count != 2) return op;
                var delete = th.getflag(named, "delete");
                var exists = th.getflag(named, "exists");
                if (named.Count > 0 || delete < 0 || exists < 0 ||
                        (delete == 1 && exists == 1))
                    return op;

                return new Builtin(op.pos, delete != 0 ? "delete_key" :
                    exists != 0 ? "exists_key" : "at_key", args.ToArray());
            }

            static Op do_atpos(Simplifier th, string name, CallSub op) {
                var args = th.no_named_params(op);
                return (args == null || args.Length != 2) ? (Op)op :
                    new Builtin(op.pos, "at_pos", args);
            }

            // XXX should support folding of SimplePair, SimpleParcel too
            Op check_folding(P6any callee, CallSub op) {
                var pos = new List<Variable>();
                var nam = new VarHash();

                foreach (var aop in op.getargs()) {
                    string name = null;
                    var valop = aop;
                    if (aop is SimplePair) {
                        name = ((SimplePair)aop).key;
                        valop = ((SimplePair)aop).value;
                    }

                    var val = valop.const_value(body);
                    if (val == null || val.Rw)
                        return null;
                    // this next one is a bit of a hack to get the right results
                    // while compiling the setting...
                    if (val.Fetch().mo.FindMethod("immutable") != null &&
                            !Builtins.ToBool(Builtins.InvokeMethod("immutable", val)))
                        return null;

                    if (name == null) {
                        pos.Add(val);
                    } else {
                        nam[name] = val;
                    }
                }

                Op r = null;
                try {
                    var ret = Kernel.RunInferior(callee.Invoke(
                        Kernel.GetInferiorRoot(), pos.ToArray(), nam));
                    r = new Const(op.pos, ret);
                } catch (Exception ex) {
                    actions.worry(op.pos, "Operation cannot succeed (constant folding threw exception: {0})", ex.Message);
                }
                return r;
            }

            internal Op visit_one(Op op) {
                var csop = op as CallSub;
                if (csop == null)
                    return op;

                var inv = csop.invocant as Lexical;
                if (inv == null)
                    return op;

                var inv_lex = actions.lookup_lex(body, inv.name);
                var real_lex = inv_lex;
                if (inv_lex == null)
                    return op;

                if (inv_lex is LIDispatch)
                    inv_lex.owner.dylex.TryGetValue(inv.name + ":(!proto)",
                            out inv_lex);

                if (!(inv_lex is LISub))
                    return op;

                var inv_sub = ((LISub)inv_lex).def;

                if (inv_sub.GetExtend0T("pure", false)) {
                    // Note, we have to use the real dispatcher here!  Don't
                    // look past it to the proto.
                    var sub = real_lex is LISub ?
                        ((LISub)real_lex).def.protosub :
                        ((Variable)real_lex.Get(real_lex.owner.protopad)).Fetch();
                    var fold = check_folding(sub, csop);
                    if (fold != null) return fold;
                }

                var builtin = inv_sub.GetExtend("builtin");
                if (builtin.Length > 0) {
                    // check for subs with explicit open-coding annotations
                    var args = no_named_params(csop);
                    if (args == null || args.Length < (int)builtin[1] ||
                            (builtin.Length >= 3 && args.Length > (int)builtin[2]))
                        return op;
                    return new Builtin(op.pos, (string)builtin[0], args);
                } else {
                    // might still have special handling
                    if (inv_sub.unit.name != "CORE") return op;
                    if (!funcs.ContainsKey(inv_lex.name)) return op;

                    return funcs[inv_lex.name](this, inv_lex.name, csop);
                }
            }
        }

        public virtual Op simplify(SubInfo body) {
            return VisitOps((new Simplifier { body = body,
                        actions = CompJob.cur.actions }).visit_one);
        }
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
        internal Op[] children;
        bool statement;
        public StatementList(Cursor c, Op[] children, bool stmt) : base(c) {
            this.children = children; statement = stmt;
        }
        public StatementList(Cursor c, params Op[] children) : this(c, children, false) {}
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
        internal bool posonly;
        internal Op[] args;

        protected CallLike(Cursor c, bool p, Op[] a) :base(c) {
            posonly = p;
            args = a;
        }

        public abstract Op adverb(Op adv);

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

        public CallSub(Cursor c, Op i, bool po=false, params Op[] a) : base(c,po,a) {
            invocant = i;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            invocant = invocant.VisitOps(post);
            return base.VisitOps(post);
        }

        public override Op adverb(Op adv) {
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

        public CallMethod(Cursor pos, string name, Op rcvr, bool po = true,
                params Op[] a) : this(pos,rcvr,name,false,null,"",po,a) {}

        public override Op adverb(Op adv) {
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
        internal Op[] items;
        public SimpleParcel(Cursor c, params Op[] its) : base(c) { items = its; }

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

        public ShortCircuit(Cursor c, int k, params Op[] a) : base(c) {
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
        internal readonly string text;

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
            var id = CompJob.cur.genid();
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
            var id = CompJob.cur.genid();

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
            var body = ((LISub)CompUtils.LookupLex(CompUtils.GetCurSub(), sink)).def;
            var vars = new string[Builtins.sig_count(body.sig)];
            var args = new Op[vars.Length];
            for (int i = 0; i < vars.Length; i++) {
                vars[i] = CompJob.cur.gensym();
                args[i] = new LetVar(pos, vars[i]);
            }
            return new ImmedForLoop(pos, source, vars,
                    CompUtils.BetaCall(at, sink, args));
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
            var aname = CompJob.cur.gensym();
            var sub = CompUtils.ThunkSub(new Let(pos,vars[0], new Lexical(pos,aname),
                        sink), new [] { aname });
            var sname = CompUtils.BlockExpr(at, sub).name;

            return new ForLoop(pos, source, sname);
        }

        protected override CgOp code(SubInfo body) { return code_labelled(body, ""); }
        protected override CgOp code_labelled(SubInfo body, string l) {
            var id = CompJob.cur.genid();

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
            var id = CompJob.cur.genid();

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
            var id = CompJob.cur.genid();
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
        internal string slot;
        internal int vars;
        public WhateverCode(Cursor c, int v, string s) : base(c) { slot = s; vars = v; }

        protected override CgOp code(SubInfo body) { return CgOp.scopedlex(slot); }
    }

    class BareBlock : Op {
        string slot;
        public BareBlock(Cursor c, string s) : base(c) { slot = s; }

        protected override CgOp code(SubInfo body) { return CgOp.scopedlex(slot); }
        public override Op statement_level(Cursor at) {
            CompUtils.SetRunOnce(((LISub)CompUtils.LookupLex(CompUtils.GetCurSub(), slot)).def);
            return CompUtils.BetaCall(at,slot);
        }
    }

    // SubDef: actually completely unused!

    class Lexical : Op {
        internal readonly string name;

        public Lexical(Cursor c, string name) : base(c) { this.name = name; }
        protected override CgOp code(SubInfo body) { return CgOp.scopedlex(name); }

        public override Variable const_value(SubInfo body) {
            var li = CompUtils.LookupLex(body, name);
            if (li is LICommon) {
                StashEnt se;
                Kernel.currentGlobals.TryGetValue(((LICommon)li).hkey, out se);
                if (se.constant) return se.v;
            }
            return (li is LIConstant) ? ((LIConstant)li).value : null;
        }

        public override Op to_bind(Cursor at, bool ro, Op rhs) {
            var lex = CompUtils.LookupLex(CompUtils.GetCurSub(), name);
            if (lex == null) {
                CompUtils.Sorry(at, "Cannot find definition for binding???");
                return new StatementList(at);
            }
            var list = false;
            var type = CompUtils.CompileGetPkg(CompUtils.GetCurSub(), "Mu");
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
        internal string name;
        internal int uplevel;

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

    class RegexBody : Op {
        string name;
        RxOp.RxOp rxop;
        Op[] pre;
        bool passcut;

        public RegexBody(Cursor c, string na, RxOp.RxOp rx, Op[] p,
                bool cut=false) : base(c) {
            name=na; rxop=rx; pre=p; passcut=cut;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            for (int i = 0; i < pre.Length; i++)
                pre[i] = pre[i].VisitOps(post);
            rxop.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            var mcaps = new List<string>();
            var capdict = new Dictionary<string,int>();
            rxop.used_caps(1, capdict);
            foreach (var kv in capdict)
                if (kv.Value >= 2) mcaps.Add(kv.Key);

            var ops = new List<object>();
            foreach (var p in pre)
                ops.Add(CgOp.sink(p.cgop(body)));
            ops.Add(CgOp.rxinit(CgOp.str(name), CgOp.cast("cursor",
                CgOp.fetch(CgOp.scopedlex("self"))), passcut?1:0));
            ops.Add(CgOp.rxpushcapture(CgOp.@null("var"), mcaps.ToArray()));
            rxop.code(body, ops);
            if (body.dylex.ContainsKey("$*GOAL"))
                ops.Insert(0, CgOp.scopedlex("$*GOAL",
                    CgOp.context_get("$*GOAL", 1)));
            ops.Add(rxop.mayback() ? CgOp.rxend() : CgOp.rxfinalend());
            ops.Add(CgOp.label("backtrack"));
            ops.Add(CgOp.rxbacktrack());
            ops.Add(CgOp.@null("var"));
            return CgOp.prog(ops.ToArray());
        }
    }

    class YouAreHere : Op {
        string unitname;
        public YouAreHere(Cursor c, string u) : base(c) { unitname = u; }

        protected override CgOp code(SubInfo body) {
            return CgOp.you_are_here(CgOp.str(unitname));
        }
    }

    class GetBlock : Op {
        bool routine;
        public GetBlock(Cursor c, bool r) : base(c) { routine = r; }

        protected override CgOp code(SubInfo body) {
            var op = CgOp.callframe();
            while(true) {
                if ((body.special & SubInfo.TRANSPARENT) == 0 &&
                        (!routine || body.mo.HasType(Kernel.RoutineMO)))
                    break;
                body = body.outer;
                op = CgOp.frame_outer(op);
            }
            return CgOp.frame_sub(op);
        }
    }

    // BEGIN DESUGARING OPS
    // These don't appear in source code, but are used by other ops to preserve
    // useful structure.
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
        public Builtin(Cursor c, string n, params Op[] a): base(c) { name=n;args=a; }

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

    // LetScope, TopicalHook, LeaveHook, LabelHook not used

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

    class StateDecl : Op {
        Op inside;
        public StateDecl(Cursor c, Op i) : base(c) { inside = i; }
        public override Op VisitOps(Func<Op,Op> post) {
            inside = inside.VisitOps(post);
            return post(this);
        }
        protected override CgOp code(SubInfo body) { return inside.cgop(body); }
        public override Op to_bind(Cursor at, bool ro, Op rhs) { return inside.to_bind(at, ro, rhs); }
    }

    class DoOnceLoop : Op {
        Op body;
        public DoOnceLoop(Cursor c, Op i) : base(c) { body = i; }
        public override Op VisitOps(Func<Op,Op> post) {
            body = body.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo sub) { return code_labelled(sub,""); }
        protected override CgOp code_labelled(SubInfo sub, string l) {
            var id = CompJob.cur.genid();

            return CgOp.xspan("redo"+id, "next"+id, 0, body.cgop(sub),
                SubInfo.ON_NEXT, l, "next"+id, SubInfo.ON_LAST, l, "next"+id,
                SubInfo.ON_REDO, l, "redo"+id);
        }
    }

    class FlipFlop : Op {
        Op lhs, rhs;
        bool excl_lhs, excl_rhs, sedlike;
        string state_var;

        public FlipFlop(Cursor c, Op lhs, Op rhs, bool excl_lhs, bool excl_rhs,
                bool sedlike, string state_var) : base(c) {
            this.lhs = lhs; this.rhs = rhs; this.excl_lhs = excl_lhs;
            this.excl_rhs = excl_rhs; this.sedlike = sedlike;
            this.state_var = state_var;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            lhs = lhs.VisitOps(post);
            rhs = rhs.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            var c = new List<object>();
            var id = CompJob.cur.genid();

            var use_hide = excl_lhs && !sedlike;
            c.Add("!ret");
            c.Add(CgOp.newrwscalar(CgOp.fetch(CgOp.string_var(""))));
            if (use_hide) {
                c.Add("!hide"); c.Add(CgOp.@int(0));
            }

            c.Add(CgOp.cgoto("flop"+id,
                CgOp.obj_getbool(CgOp.scopedlex(state_var))));
            c.Add(CgOp.ncgoto("end"+id, CgOp.obj_getbool(lhs.cgop(body))));

            if (sedlike) {
                c.Add(CgOp.sink(CgOp.N("preinc", CgOp.scopedlex(state_var))));
                if (!excl_lhs) c.Add(CgOp.sink(CgOp.assign(CgOp.letvar("!ret"),
                    CgOp.scopedlex(state_var))));
                c.Add(CgOp.@goto("end"+id));
            }
            else {
                if (use_hide) c.Add(CgOp.letvar("!hide", CgOp.@int(1)));
            }

            c.Add(CgOp.label("flop"+id));
            c.Add(CgOp.sink(CgOp.N("preinc", CgOp.scopedlex(state_var))));
            c.Add(CgOp.ncgoto("check"+id, CgOp.obj_getbool(rhs.cgop(body))));
            if (!excl_rhs) c.Add(CgOp.sink(CgOp.assign(
                CgOp.scopedlex(state_var), CgOp.@const(CgOp.exactnum(10,0)))));
            c.Add(CgOp.@goto("end"+id));

            c.Add(CgOp.label("check"+id));
            // reached if !flopping, !ret will NOT be set at this point, we
            // may be in a lhs if !sedlike
            c.Add(CgOp.sink(CgOp.assign(CgOp.letvar("!ret"),
                CgOp.scopedlex(state_var))));

            c.Add(CgOp.label("end"+id));
            if (use_hide) c.Add(CgOp.ternary(
                CgOp.compare("==", CgOp.letvar("!hide"), CgOp.@int(1)),
                CgOp.sink(CgOp.assign(CgOp.letvar("!ret"),CgOp.string_var(""))),
                CgOp.prog()));

            c.Add(CgOp.letvar("!ret"));

            return CgOp.letn(c.ToArray());
        }
    }

    class Temporize : Op {
        Op var;
        int mode;

        public Temporize(Cursor c, Op v, int m) : base(c) { var=v; mode=m; }
        public override Op VisitOps(Func<Op,Op> post) {
            var = var.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return CgOp.temporize(var.cgop(body), CgOp.callframe(),
                CgOp.@int(mode));
        }
    }

    // Hack to support the $x .= foo; syntax; wraps an Operator
    class DotEqRHS : Op {
        internal Operator wrap;
        public DotEqRHS(Cursor c, Operator w) : base(c) { wrap = w; }

        protected override CgOp code(SubInfo body) {
            throw new NotSupportedException("How did you manage to separate the methodish operator from the .= ?");
        }
    }

    class IndirectVar : Op {
        Op name;
        bool bind_ro;
        Op bind;

        public IndirectVar(Cursor c, Op n, bool r=false, Op b=null) : base(c) {
            name = n; bind_ro = r; bind = b;
        }

        public override Op VisitOps(Func<Op,Op> post) {
            name = name.VisitOps(post);
            if (bind != null) bind = bind.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            return CgOp.sc_indir(CgOp.sc_root(),
                CgOp.obj_getstr(name.cgop(body)), CgOp.@bool(bind_ro?1:0),
                bind != null ? bind.cgop(body) : CgOp.@null("var"));
        }

        public override Op to_bind(Cursor at, bool ro, Op rhs) {
            return new IndirectVar(pos, name, ro, rhs);
        }
    }

    // this one IS still used.
    class CatchyWrapper : Op {
        Op inside;
        public CatchyWrapper(Cursor c, Op i) : base(c) { inside = i; }
        public override Op VisitOps(Func<Op,Op> post) {
            inside = inside.VisitOps(post);
            return post(this);
        }

        protected override CgOp code(SubInfo body) {
            var id = CompJob.cur.genid();

            return CgOp.xspan("start"+id, "end"+id, 0, CgOp.prog(
                CgOp.sink(inside.cgop(body)),
                CgOp.@return(CgOp.corelex("False")),
                CgOp.label("caught"+id),
                CgOp.corelex("True")),
                SubInfo.ON_SUCCEED, "", "caught"+id);
        }
    }

    class Const : Op {
        Variable value;
        public Const(Cursor c, Variable v) : base(c) { value = v; }
        public override Variable const_value(SubInfo s) { return value; }
        protected override CgOp code(SubInfo s) { return CgOp.@const(value); }
    }

    static class Helpers {
        public static Op mklet(Cursor p, Op value, Func<Op,Op> body) {
            var v = CompJob.cur.gensym();
            return new Let(p, v, value, body(new LetVar(p, v)));
        }

        public static Op mkcall(Cursor p, string name, params Op[] positionals){
            CompUtils.MarkUsed(p, name);
            if (name == "&eval") CompUtils.GetCurSub().special |=
                SubInfo.CANNOT_INLINE; // HACK
            return new CallSub(p, new Lexical(p, name), true, positionals);
        }

        public static Op mklex(Cursor p, string name) {
            CompUtils.MarkUsed(p, name);
            if (name == "&eval") CompUtils.GetCurSub().special |=
                SubInfo.CANNOT_INLINE; // HACK
            return new Lexical(p, name);
        }

        public static Op mkbool(Cursor at, bool b) {
            return new Lexical(at, b ? "True" : "False");
        }

        public static Op mktemptopic(Cursor at, Op item, Op expr) {
            return mklet(at, mklex(at, "$_"), (old_) =>
                new StatementList(at,
                    new LexicalBind(at, "$_", false,false,null, item),
                    mklet(at, expr, (result) =>
                        new StatementList(at,
                            new LexicalBind(at,"$_",false,false,null,old_),
                            result))));
        }

        public static Op mkstringycat(Cursor at, params Op[] strings) {
            var a = new List<Op>();
            foreach (Op s in strings) {
                var ac = a.Count;
                if (ac != 0 && a[ac-1] is StringLiteral && s is StringLiteral) {
                    a[ac-1] = new StringLiteral(at,
                            ((StringLiteral)a[ac-1]).text +
                            ((StringLiteral)s).text);
                } else {
                    a.Add(s);
                }
            }
            if (a.Count == 0)
                return new StringLiteral(at, "");
            else if (a.Count == 1)
                return (a[0] is StringLiteral) ? a[0] :
                    mkcall(at, "&prefix:<~>", a[0]);
            else
                return mkcall(at, "&infix:<~>", a.ToArray());
        }
    }
}
