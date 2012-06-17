using System;
using System.Collections.Generic;

// An Operator is fragment of code that can have operands plugged in;
// they abstract functions, macros, and some syntactic forms like
// method calls.

namespace Niecza.Compiler {
    abstract class Operator {
        protected int arity;

        public abstract Op.Op with_args(Cursor m, params Op.Op[] args);

        public virtual  Op.Op as_function(Cursor m) {
            if (arity < 0) {
                CompJob.cur.actions.sorry(m,"This macro cannot be used as a function");
                return Op.Helpers.mklex(m, "&die");
            }
            var anames = new string[arity];
            var avars  = new Op.Op[arity];
            var job    = CompJob.cur;
            for (int i = 0; i < arity; i++) {
                anames[i] = job.gensym();
                avars[i]  = Op.Helpers.mklex(m,anames[i]);
            }
            return job.actions.block_expr(m,
                job.actions.thunk_sub(with_args(m, avars), anames));
        }

        public virtual bool whatever_curry() { return false; }
        public virtual bool assignish() { return false; }

        public virtual Operator meta_assign() { return new CompoundAssign(this); }
        public virtual Operator meta_not() { return new MetaNot(this); }
        public virtual Operator meta_fun(Cursor m, string fun, int arity,
                params Op.Op[] extra) {
            return new Function(Op.Helpers.mklex(m,fun), arity,
                Utils.AppendArr(extra, as_function(m)));
        }

        public static Operator funop(Cursor m, string fun, int arity,
                params Op.Op[] extra) {
            return new Function(Op.Helpers.mklex(m,fun), arity, extra);
        }

        internal class Function : Operator {
            protected Op.Op function;
            internal Op.Op[] preargs, postargs;

            public Function(Op.Op f, int a, Op.Op[] pr=null, Op.Op[] po=null) {
                function = f; arity = a; preargs = pr ?? new Op.Op[0];
                postargs = po ?? new Op.Op[0];
            }

            public override Op.Op as_function(Cursor m) {
                if (preargs.Length > 0 || postargs.Length > 0)
                    return base.as_function(m);
                else
                    return function;
            }

            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                return new Op.CallSub(m, function, true, Utils.PrependArr(
                    postargs, 0, Utils.PrependArr(args, 0, preargs)));
            }

            string name() {
                var ff = function as Op.Lexical;
                return ff == null ? "" : ff.name;
            }

            // XXX These two should use variable lookups
            public override bool assignish() { return name() == "&infix:<=>"; }
            static HashSet<string> wc = new HashSet<string> { "&infix:<..>",
                "&infix:<..^>", "&infix:<^..>", "&infix:<^..^>",
                "&infix:<...>", "&infix:<...^>", "&infix:<,>", "&infix:<=>",
                "&infix:<xx>" };
            public override bool whatever_curry() { return wc.Contains(name()); }
        }

        internal class PostCall : Operator {
            // .(12); use args
            internal Op.Op[] arglist;
            public PostCall(Op.Op[] a) { arglist = a; arity = 1; }

            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                return new Op.CallSub(m, args[0], false, arglist);
            }
        }

        internal class Method : Operator {
            internal object name; // Str | Op; .foo; use args, meta, private, path
            internal Op.Op[] arglist;
            internal string meta;
            internal bool privat;
            internal STable package;

            public Method(object na, Op.Op[] args, string mt, bool priv,
                    STable p) {
                name = na; arglist = args; meta = mt; privat = priv;
                package = p; arity = 1;
            }

            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                string ns = name as string;
                var job = CompJob.cur;
                if (!privat && meta == "" && (ns == "HOW" || ns == "WHAT" ||
                            ns == "WHO" || ns == "VAR")) {
                    if (arglist.Length > 0)
                        job.actions.sorry(m, "Interrogative operator "+name+" does not take arguments");
                    return new Op.Interrogative(m, args[0], ns);
                }
                if (ns == "eval")
                    job.curlex.special |= SubInfo.CANNOT_INLINE;
                STable pclass = null;
                if (privat) {
                    if (package != null) {
                        pclass = package;
                    } else if (job.curlex.in_class != null) {
                        pclass = job.curlex.in_class;
                    } else {
                        job.actions.sorry(m, "Cannot resolve class for private method");
                    }
                    if (pclass != null && !pclass.Trusts(job.curlex.cur_pkg)) {
                        job.actions.sorry(m, "Cannot call private method '{0}' on {1} because it does not trust {2}", name, pclass.name, job.curlex.cur_pkg.name);
                        pclass = null;
                    }
                } else {
                    pclass = package;
                }
                return new Op.CallMethod(m, args[0], name,
                    privat && pclass != null, pclass, meta, false, arglist);
            }
            public override bool whatever_curry() { return true; }
        }

        internal class FlipFlop : Operator {
            bool excl_lhs, excl_rhs, sedlike;
            public FlipFlop(bool excl_lhs, bool excl_rhs, bool sedlike) {
                this.excl_lhs = excl_lhs; this.excl_rhs = excl_rhs; this.sedlike = sedlike;
            }

            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                var job = CompJob.cur;
                var state_var = job.gensym();
                job.actions.addlex(m, job.curlex.StateOuter(), state_var,
                    new LISimple(0, null));
                if (args[1] is Op.Whatever)
                    args[1] = Op.Helpers.mklex(m, "False");
                return new Op.FlipFlop(m, args[0], args[1], excl_lhs,
                    excl_rhs, sedlike, state_var);
            }
        }

        internal class ShortCircuit : Operator {
            int kind;
            public ShortCircuit(int kind) { this.kind = kind; }

            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                return new Op.ShortCircuit(m, kind, args);
            }

            public override bool whatever_curry() { return true; }
        }

        internal class CompoundAssign : Operator {
            Operator basis;
            public CompoundAssign(Operator basis) { this.basis = basis; arity = basis.arity; }

            public override Op.Op with_args(Cursor m, params Op.Op[] rest) {
                var left = rest[0] as Op.Lexical;
                if (left != null) {
                    rest[0] = new Op.Lexical(m, left.name);
                    return Op.Helpers.mkcall(m, "&infix:<=>", left,
                        basis.with_args(m, rest));
                } else {
                    return Op.Helpers.mklet(m, rest[0], (ll) =>
                        Op.Helpers.mkcall(m, "&infix:<=>", ll,
                            basis.with_args(m, Utils.PrependArr(rest,1,ll))));
                }
            }

            public override bool assignish() { return true; }
        }

        internal class MetaNot : Operator {
            Operator basis;
            public MetaNot(Operator b) { basis = b; arity = b.arity; }

            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                return Op.Helpers.mkcall(m, "&prefix:<!>",
                        basis.with_args(m,args));
            }

            public override bool whatever_curry() { return true; }
        }

        internal class Binding : Operator {
            bool ro;
            public Binding(bool ro) { this.ro = ro; }

            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                return args[0].to_bind(m, ro, args[1]);
            }

            public override bool assignish() { return true; }
        }

        internal class Comma : Operator {
            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                var bits = new List<Op.Op>();
                foreach (var a in args)
                    if (a is Op.SimpleParcel)
                        foreach (var aa in ((Op.SimpleParcel)a).items)
                            bits.Add(aa);
                    else
                        bits.Add(a);
                return new Op.SimpleParcel(m, bits.ToArray());
            }
            public override Op.Op as_function(Cursor m) {
                return Op.Helpers.mklex(m, "&infix:<,>");
            }
        }

        internal class Ternary : Operator {
            Op.Op middle;
            public Ternary(Op.Op mid) { middle=mid; }
            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                return new Op.Conditional(m, args[0], middle, args[1]);
            }
        }

        internal class Temp : Operator {
            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                var rarg = args[0] as Op.ContextVar;
                var job = CompJob.cur;
                if (rarg == null || rarg.uplevel != 0) { // normal case
                    job.curlex.special |= SubInfo.CANNOT_INLINE;
                    return new Op.Temporize(m, args[0], Builtins.T_TEMP);
                }
                job.actions.addlex(m, job.curlex, rarg.name, new LISimple(
                    (rarg.name[0] == '%' ? LISimple.HASH :
                        rarg.name[0] == '@' ? LISimple.LIST : 0), null));
                return Op.Helpers.mkcall(m, "&infix:<=>",
                    Op.Helpers.mklex(m, rarg.name),
                    new Op.ContextVar(m, rarg.name, 1));
            }
        }

        internal class Let : Operator {
            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                CompJob.cur.curlex.special |= SubInfo.CANNOT_INLINE;
                return new Op.Temporize(m, args[0], Builtins.T_LET);
            }
        }

        internal class SmartMatch : Operator {
            public override Op.Op as_function(Cursor m) {
                return Op.Helpers.mklex(m, "&infix:<~~>");
            }
            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                return Op.Helpers.mktemptopic(m, args[0],
                    new Op.CallMethod(m, "ACCEPTS", args[1], true,
                        Op.Helpers.mklex(m, "$_")));
            }
        }

        internal class DotEq : Operator {
            public override bool assignish() { return true; }
            public override Operator meta_assign() { throw new NieczaException(".= may not be metaoperated"); }
            public override Operator meta_not() { throw new NieczaException(".= may not be metaoperated"); }
            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                return ((Op.DotEqRHS)args[1]).wrap.meta_assign()
                    .with_args(m, args[0]);
            }
        }

        internal class Replicate : Operator {
            public override Op.Op as_function(Cursor m) {
                return Op.Helpers.mklex(m, "&infix:<xx>");
            }
            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                var act = CompJob.cur.actions;
                return Op.Helpers.mkcall(m, "&_doreplicate", act.block_expr(m,
                    act.thunk_sub(args[0], new string[0])), args[1]);
            }
        }

        // A bit hackish; handles the macro aspects of $foo does Role(23)
        internal class Mixin : Function {
            public Mixin(Op.Op fun) : base(fun,2) { }
            public override Op.Op with_args(Cursor m, params Op.Op[] args) {
                var a1 = args[1] as Op.CallSub;
                if (a1 == null)
                    return base.with_args(m, args);
                if (a1.invocant is Op.Lexical && ((Op.Lexical)a1.invocant).name == "&_param_role_inst")
                    return base.with_args(m, args);
                var a1args = a1.getargs();
                if (a1args.Length != 1)
                    CompJob.cur.actions.sorry(m, "Can only provide exactly one initial value to a mixin");
                return new Op.CallSub(m, function, false, args[0], a1.invocant,
                    new Op.SimplePair(m, "value", a1args[0] ?? Op.Helpers.mklex(m, "Nil")));
            }
        }
    }
}
