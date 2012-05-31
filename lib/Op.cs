using System;
using System.Collections.Generic;

using Niecza;
using Niecza.Compiler;

namespace Niecza.Compiler.Op {
    abstract class Op {
        Cursor pos;

        protected Op(Cursor pos) { this.pos = pos; }

        // These are just placeholders until more of the system is online
        public int LineOf(Cursor c) { return 0; }
        public void Sorry(Cursor c, string msg) { throw new NotImplementedException(); }

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

        public virtual Op to_bind(Cursor at, bool ro, CgOp rhs) {
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

}
