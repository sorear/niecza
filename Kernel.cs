using System;
using System.Collections.Generic;
namespace Niecza {
    // We like to reuse continuation objects for speed - every function only
    // creates one kind of continuation, but tweaks a field for exact return
    // point.  As such, call frames and continuations are in 1:1 correspondence
    // and are unified.  Functions take a current continuation and return a new
    // continuation; we tail recurse with trampolines.

    // Only call other functions in Continue, not in the CallableDelegate or
    // equivalent!
    public delegate Frame CallableDelegate(Frame caller,
            LValue[] pos, Dictionary<string, LValue> named);
    // Used by DynFrame to plug in code
    public delegate Frame DynBlockDelegate(Frame frame);

    public interface IP6 {
        Frame Invoke(Frame caller, LValue[] pos,
                Dictionary<string, LValue> named);
        // include the invocant in the positionals!  it will not usually be
        // this, rather a container of this
        Frame InvokeMethod(Frame caller, string name,
                LValue[] pos, Dictionary<string, LValue> named);
        Frame GetAttribute(Frame caller, string name);
        //public Frame WHERE(Frame caller);
        Frame HOW(Frame caller);
        // These exist as a concession to circularity - FETCH as a completely
        // ordinary method could not work under the current calling convention.
        Frame Fetch(Frame caller);
        Frame Store(Frame caller, IP6 thing);
    }

    // A LValue is the meaning of function arguments, of any subexpression
    // except the targets of := and .VAR.
    //
    // They come in two flavors.  Scalary lvalues hold a container, which
    // can do FETCH and STORE.  Listy lvalues FETCH as the container itself,
    // and STORE as a method to the container.
    //
    // List->scalar context: create a simple container holding the list's
    // object, but .item.  Read only.
    //
    // Scalar->list: call .list or something to get a suitable list object.
    // Bind it same rwness.
    public struct LValue {
        public IP6 container;
        public bool rw;
        public bool islist;

        public LValue(bool rw_, bool islist_, IP6 container_) {
            rw = rw_; islist = islist_;
            container = container_;
        }
    }

    // Variables are things which can produce LValues, and can also bind
    // LValues.  They hold LValues and may or may not be bindable.  Variables
    // also tend to contextualize stuff put into them.
    //
    // Coercions are not used on binding unless necessary.
    //
    // Variables also have type constraints, that's how %foo and @foo differ...
    public class Variable {
        public enum Context {
            // @foo: binds listy lvalues; calls .list on other stuff and uses
            // the result
            List,
            // $foo: binds scalary lvalues; calls .item on other stuff and
            // wraps it in a container
            Scalar,
            // self: binds scalary lvalues, but doesn't call .item; could wind
            // up bound to a Parcel
            WeakScalar,
            // ¢foo: binds any lvalue, no munging, needed for = rhs etc
            Capture
        }
        public LValue lv;
        public bool bvalue;

        public Context context;

        public Variable(bool bv, Context cx, LValue lv_) {
            bvalue = bv;
            lv = lv_;
            context = cx;
        }
    }

    // We need hashy frames available to properly handle BEGIN; for the time
    // being, all frames will be hashy for simplicity
    public class Frame: IP6 {
        public readonly Frame caller;
        public readonly Frame outer;
        public Frame proto;
        public object resultSlot = null;
        public int ip = 0;
        public readonly DynBlockDelegate code;
        public readonly Dictionary<string, object> lex
            = new Dictionary<string, object>();

        public LValue[] pos;
        public Dictionary<string, LValue> named;

        public Frame(Frame outer_) : this(null, outer_, null) {}

        public Frame(Frame caller_, Frame outer_,
                DynBlockDelegate code_) {
            caller = caller_;
            outer = outer_;
            code = code_;
        }

        public Frame Continue() {
            return code(this);
        }

        public Frame GetAttribute(Frame c, string name) {
            c.resultSlot = lex[name];
            return c;
        }

        public Frame Invoke(Frame c, LValue[] p, Dictionary<string, LValue> n) {
            return Kernel.Die(c, "Tried to invoke a Frame");
        }

        public Frame InvokeMethod(Frame c, string nm, LValue[] p,
                Dictionary<string, LValue> n) {
            return Kernel.Die(c, "Method " + nm +
                    " not defined on Frame");
        }

        public Frame Fetch(Frame c) {
            return Kernel.Die(c, "Method FETCH not defined on Frame");
        }

        public Frame Store(Frame c, IP6 o) {
            return Kernel.Die(c, "Method STORE not defined on Frame");
        }

        public Frame HOW(Frame c) {
            //TODO
            return Kernel.Die(c, "No metaobject available for Frame");
        }
    }

    // NOT IP6; these things should only be exposed through a ClassHOW-like
    // façade
    public class DynProtoMetaObject {
        public struct Method {
            public Method(DynBlockDelegate code, Frame proto, int outer_index) {
                this.code  = code;
                this.proto = proto;
                this.outer_index = outer_index;
            }
            public DynBlockDelegate code;
            public Frame proto;
            public int outer_index;
        }

        public IP6 how;
        public string name;

        public InvokeHandler OnInvoke;
        public FetchHandler OnFetch;
        public StoreHandler OnStore;

        public List<DynProtoMetaObject> superclasses
            = new List<DynProtoMetaObject>();
        public Dictionary<string, Method> local
            = new Dictionary<string, Method>();

        public List<Frame> def_outers = new List<Frame>();

        public delegate Frame InvokeHandler(DynObject th, Frame c,
                LValue[] pos, Dictionary<string, LValue> named);
        public delegate Frame FetchHandler(DynObject th, Frame c);
        public delegate Frame StoreHandler(DynObject th, Frame c, IP6 n);
    }

    public class DynMetaObject {
        public DynProtoMetaObject proto;
        public List<Frame> outers = new List<Frame>();
        public List<DynMetaObject> mro;
        public IP6 typeObject;

        public DynMetaObject(DynProtoMetaObject proto) {
            this.proto = proto;
            this.mro = new List<DynMetaObject>();
            mro.Add(this);
        }

        public void BuildC3MRO(List<DynMetaObject> supers) {
            List<List<DynMetaObject>> toMerge = new List<List<DynMetaObject>>();
            mro = new List<DynMetaObject>();
            toMerge.Add(new List<DynMetaObject>());
            toMerge[0].Add(this);

            foreach (DynMetaObject dmo in supers) {
                toMerge[0].Add(dmo);
                toMerge.Add(new List<DynMetaObject>(dmo.mro));
            }

            while (true) {
top:
                foreach (List<DynMetaObject> h in toMerge) {
                    if (h.Count == 0) {
                        continue; // next CANDIDATE
                    }
                    DynMetaObject cand = h[0];
                    foreach (List<DynMetaObject> bs in toMerge) {
                        if (bs.Count == 0) {
                            continue; // next BLOCKER
                        }
                        if (bs[0] == cand) {
                            continue;
                        }
                        if (bs.Contains(cand)) {
                            goto blocked;
                        }
                    }
                    // no reason not to immediately put this, and by loop
                    // order the C3 condition is kept
                    mro.Add(cand);
                    foreach (List<DynMetaObject> l in toMerge) {
                        l.Remove(cand);
                    }
                    goto top;
blocked:
                    ;
                }
                foreach (List<DynMetaObject> l in toMerge) {
                    if (l.Count != 0) {
                        // should refactor this to use a real p6exception
                        throw new Exception("C3 MRO inconsistency detected");
                    }
                }
                return;
            }
        }
    }

    // This is quite similar to DynFrame and I wonder if I can unify them.
    // These are always hashy for the same reason as Frame above
    public class DynObject: IP6 {
        // the slots have to support non-containerized values, because
        // containers are objects now
        public Dictionary<string, object> slots
            = new Dictionary<string, object>();
        public DynMetaObject klass;

        private Frame Fail(Frame caller, string msg) {
            return Kernel.Die(caller, msg + " in class " + klass.proto.name);
        }

        public Frame InvokeMethod(Frame caller, string name,
                LValue[] pos, Dictionary<string, LValue> named) {
            DynProtoMetaObject.Method m;
            while (klass.outers.Count < klass.proto.def_outers.Count) {
                klass.outers.Add(klass.proto.def_outers[klass.outers.Count]);
            }
            foreach (DynMetaObject k in klass.mro) {
                if (k.proto.local.TryGetValue(name, out m)) {
                    Frame n = new Frame(caller, k.outers[m.outer_index],
                            m.code);
                    n.proto = m.proto;
                    n.pos = pos;
                    n.named = named;
                    return n;
                }
            }
            return Fail(caller, "Unable to resolve method " + name);
        }

        public Frame GetAttribute(Frame caller, string name) {
            if (slots == null) {
                return Fail(caller, "Attempted to access slot " + name +
                        " via a protoobject");
            }
            caller.resultSlot = slots[name];
            return caller;
        }

        public Frame HOW(Frame caller) {
            caller.resultSlot = klass.proto.how;
            return caller;
        }

        public Frame Invoke(Frame c, LValue[] p, Dictionary<string, LValue> n) {
            if (klass.proto.OnInvoke != null) {
                return klass.proto.OnInvoke(this, c, p, n);
            } else {
                return Fail(c, "No invoke handler set");
            }
        }

        public Frame Fetch(Frame c) {
            if (klass.proto.OnFetch != null) {
                return klass.proto.OnFetch(this, c);
            } else {
                return Fail(c, "No fetch handler set");
            }
        }

        public Frame Store(Frame c, IP6 o) {
            if (klass.proto.OnStore != null) {
                return klass.proto.OnStore(this, c, o);
            } else {
                return Fail(c, "No store handler set");
            }
        }
    }

    // Allows native CLR objects to be treated as Perl 6 data.  They don't
    // currently support any operations; you'll need to use CLR code to work
    // with them.
    public class CLRImportObject : IP6 {
        public readonly object val;

        public CLRImportObject(object val_) { val = val_; }

        public Frame GetAttribute(Frame c, string nm) {
            return Kernel.Die(c, "Attribute " + nm +
                    " not available on CLRImportObject");
        }

        public Frame Invoke(Frame c, LValue[] p, Dictionary<string, LValue> n) {
            return Kernel.Die(c, "Tried to invoke a CLRImportObject");
        }

        public Frame InvokeMethod(Frame c, string nm, LValue[] p,
                Dictionary<string, LValue> n) {
            return Kernel.Die(c, "Method " + nm +
                    " not defined on CLRImportObject");
        }

        public Frame Fetch(Frame c) {
            return Kernel.Die(c, "Method FETCH not defined on CLRImportObject");
        }

        public Frame Store(Frame c, IP6 o) {
            return Kernel.Die(c, "Method STORE not defined on CLRImportObject");
        }

        public Frame HOW(Frame c) {
            //TODO
            return Kernel.Die(c, "No metaobject available for CLRImportObject");
        }
    }

    // A bunch of stuff which raises big circularity issues if done in the
    // setting itself.
    public class Kernel {
        public static DynBlockDelegate MainlineContinuation;

        private static Frame SCFetch(DynObject th, Frame caller) {
            caller.resultSlot = th.slots["value"];
            return caller;
        }

        private static Frame SCStore(DynObject th, Frame caller, IP6 nv) {
            th.slots["value"] = nv;
            return caller;
        }

        private static Frame SubCloneC(Frame th) {
            DynObject a, b;
            Frame c;
            switch (th.ip) {
                case 0:
                    th.ip = 1;
                    return th.pos[0].container.Fetch(th);
                case 1:
                    th.lex["s0"] = th.resultSlot;
                    th.ip = 2;
                    return th.pos[1].container.Fetch(th);
                case 2:
                    a = (DynObject) th.lex["s0"];
                    c = (Frame) th.resultSlot;
                    b = new DynObject();
                    b.klass = a.klass;
                    b.slots = new Dictionary<string,object>(a.slots);
                    b.slots["outer"] = c;
                    th.caller.resultSlot = NewROScalar(b);
                    return th.caller;
                default:
                    return Kernel.Die(th, "invalid IP");
            }
        }

        private static Frame SubInvoke(DynObject th, Frame caller,
                LValue[] pos, Dictionary<string,LValue> named) {
            Frame proto = (Frame) th.slots["proto"];
            Frame outer = (Frame) th.slots["outer"];
            DynBlockDelegate code = (DynBlockDelegate) th.slots["code"];

            Frame n = new Frame(caller, outer, code);
            n.proto = proto;
            n.pos = pos;
            n.named = named;

            return n;
        }

        public static Frame Die(Frame caller, string msg) {
            Frame f = new Frame(caller, null, new DynBlockDelegate(ThrowC));
            f.pos = new LValue[1] { new LValue(true, false, new CLRImportObject(msg)) };
            f.named = null;
            return f;
        }

        // Needs more special handling for control exceptions.
        private static Frame ThrowC(Frame th) {
            IP6 a;
            switch (th.ip) {
                case 0:
                    th.lex["$cursor"] = NewRWScalar(th.caller);
                    goto case 1;
                case 1:
                    th.ip = 2;
                    th.resultSlot = null;
                    return ((Variable)th.lex["$cursor"])
                        .lv.container.Fetch(th);
                case 2:
                    a = (IP6)th.resultSlot;
                    if (a == null) {
                        throw new Exception("Unhandled Perl 6 exception");
                    }
                    th.ip = 3;
                    th.resultSlot = null;
                    return a.GetAttribute(th, "exn_skipto");
                case 3:
                    // if skipto, skip some frames.  Used to implement CATCH
                    // invisibility
                    if (th.resultSlot != null) {
                        th.ip = 1;
                        a = (IP6)th.resultSlot;
                        th.resultSlot = null;
                        return ((Variable)th.lex["$cursor"])
                            .lv.container.Store(th, a);
                    }
                    th.ip = 4;
                    th.resultSlot = null;
                    return ((Variable)th.lex["$cursor"])
                        .lv.container.Fetch(th);
                case 4:
                    a = (IP6)th.resultSlot;
                    th.ip = 5;
                    th.resultSlot = null;
                    return a.GetAttribute(th, "exn_handler");
                case 5:
                    if (th.resultSlot != null) {
                        // tailcall
                        return ((IP6)th.resultSlot).Invoke(th.caller,
                                th.pos, th.named);
                    }
                    th.ip = 6;
                    th.resultSlot = null;
                    return ((Variable)th.lex["$cursor"])
                        .lv.container.Fetch(th);
                case 6:
                    a = ((Frame)th.resultSlot).caller;
                    th.ip = 1;
                    th.resultSlot = null;
                    return ((Variable)th.lex["$cursor"])
                        .lv.container.Store(th, a);
                default:
                    throw new Exception("IP invalid");
            }
        }

        public static readonly DynMetaObject SubMO;
        public static readonly DynMetaObject ScalarMO;
        public static readonly DynProtoMetaObject SubPMO;
        public static readonly DynProtoMetaObject ScalarPMO;
        public static readonly IP6 DieSub;

        public static IP6 MakeSub(DynBlockDelegate code, Frame proto,
                Frame outer) {
            DynObject n = new DynObject();
            n.klass = SubMO;
            n.slots["outer"] = outer;
            n.slots["code"] = code;
            n.slots["proto"] = proto;
            return n;
        }

        public static Variable BoxAny(object v, IP6 proto) {
            DynObject n = new DynObject();
            n.klass = ((DynObject)proto).klass;
            n.slots["value"] = v;
            return NewROScalar(n);
        }

        public static object UnboxAny(IP6 o) {
            // TODO: Check for compatibility?
            return ((DynObject)o).slots["value"];
        }

        public static IP6 MakeSC(IP6 inside) {
            DynObject n = new DynObject();
            n.klass = ScalarMO;
            n.slots["value"] = inside;
            return n;
        }

        private static Frame BindScalarizeC(Frame th) {
            LValue rhs;
            switch (th.ip) {
                case 0:
                    rhs = (LValue) th.lex["o"];
                    th.ip = 1;
                    return rhs.container.InvokeMethod(th, "item",
                            new LValue[1] { rhs }, null);
                case 1:
                    rhs = ((Variable) th.resultSlot).lv;
                    if (rhs.islist) {
                        throw new Exception(".item didn't do its job and returned a list!");
                    }
                    ((Variable)th.lex["c"]).lv = rhs;
                    return th.caller;
                default:
                    throw new Exception("IP invalid");
            }
        }

        private static Frame BindListizeC(Frame th) {
            LValue rhs;
            switch (th.ip) {
                case 0:
                    rhs = (LValue) th.lex["o"];
                    th.ip = 1;
                    return rhs.container.InvokeMethod(th, "list",
                            new LValue[1] { rhs }, null);
                case 1:
                    rhs = ((Variable) th.resultSlot).lv;
                    if (!rhs.islist) {
                        throw new Exception(".list didn't do its job and returned a scalar!");
                    }
                    ((Variable)th.lex["c"]).lv = rhs;
                    return th.caller;
                default:
                    throw new Exception("IP invalid");
            }
        }

        public static Frame Bind(Frame th, Variable lhs, LValue rhs,
                bool ro, bool forcerw) {
            // TODO: need exceptions for forcerw to be used
            Frame n;
            switch (lhs.context) {
                case Variable.Context.Scalar:
                    if (rhs.islist) {
                        n = new Frame(th, null,
                                new DynBlockDelegate(BindScalarizeC));
                        n.lex["o"] = rhs;
                        n.lex["c"] = lhs;
                        return n;
                    } else {
                        lhs.lv = rhs;
                        if (ro) { lhs.lv.rw = false; }
                        return th;
                    }
                case Variable.Context.WeakScalar:
                    if (rhs.islist) {
                        lhs.lv.rw = false;
                        lhs.lv.islist = false;
                        lhs.lv.container = MakeSC(rhs.container);
                        return th;
                    } else {
                        lhs.lv = rhs;
                        if (ro) { lhs.lv.rw = false; }
                        return th;
                    }
                case Variable.Context.List:
                    if (rhs.islist) {
                        lhs.lv = rhs;
                        if (ro) { lhs.lv.rw = false; }
                        return th;
                    } else {
                        n = new Frame(th, null,
                                new DynBlockDelegate(BindListizeC));
                        n.lex["o"] = rhs;
                        n.lex["c"] = lhs;
                        return n;
                    }
                case Variable.Context.Capture:
                    lhs.lv = rhs;
                    return th;
                default:
                    throw new Exception("invalid context?");
            }
        }

        // ro, not rebindable
        public static Variable NewROScalar(IP6 obj) {
            return new Variable(false, Variable.Context.Scalar,
                    new LValue(false, false, MakeSC(obj)));
        }

        // TODO: Find out from #perl6 more about whether we actually want
        // to be cloning anything.  this one /is/ rebindable
        public static Variable NewRWScalar(IP6 obj) {
            return new Variable(true, Variable.Context.Scalar,
                    new LValue(true, false, MakeSC(obj)));
        }

        public static Variable NewRWListVar(IP6 container) {
            return new Variable(true, Variable.Context.List,
                    new LValue(true, true, container));
        }

        public static Variable NewCaptureVar() {
            return new Variable(true, Variable.Context.Capture,
                    new LValue(false, false, MakeSC(null)));
        }

        public static Variable NewWeakScalar() {
            return new Variable(true, Variable.Context.WeakScalar,
                    new LValue(false, false, MakeSC(null)));
        }

        static Kernel() {
            SubPMO = new DynProtoMetaObject();
            SubPMO.name = "Sub";
            SubPMO.OnInvoke = new DynProtoMetaObject.InvokeHandler(SubInvoke);
            SubPMO.local["clone"] = new DynProtoMetaObject.Method(
                    new DynBlockDelegate(SubCloneC),
                    null, 0);
            SubPMO.def_outers.Add(null);

            SubMO = new DynMetaObject(SubPMO);

            ScalarPMO = new DynProtoMetaObject();
            ScalarPMO.name = "Scalar";
            ScalarPMO.OnFetch = new DynProtoMetaObject.FetchHandler(SCFetch);
            ScalarPMO.OnStore = new DynProtoMetaObject.StoreHandler(SCStore);

            ScalarMO = new DynMetaObject(ScalarPMO);

            DieSub = MakeSub(new DynBlockDelegate(ThrowC), null, null);
        }
    }
}
