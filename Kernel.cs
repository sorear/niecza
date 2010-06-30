using System;
using System.Collections.Generic;
namespace Sprixel {
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

    public struct LValue {
        public IP6 container;
        public bool rw;

        public LValue(bool rw_, IP6 container_) {
            rw = rw_;
            container = container_;
        }
    }

    public class Variable {
        public LValue lv;
        public bool bvalue;

        public Variable() { }
        public Variable(bool bv, LValue lv_) {
            bvalue = bv;
            lv = lv_;
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
    public class DynMetaObject {
        public Dictionary<string, IP6> methods = new Dictionary<string, IP6>();
        public IP6 how;
        public string name;

        public InvokeHandler OnInvoke;
        public FetchHandler OnFetch;
        public StoreHandler OnStore;

        public delegate Frame InvokeHandler(DynObject th, Frame c,
                LValue[] pos, Dictionary<string, LValue> named);
        public delegate Frame FetchHandler(DynObject th, Frame c);
        public delegate Frame StoreHandler(DynObject th, Frame c, IP6 n);
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
            return Kernel.Die(caller, msg + " in class " + klass.name);
        }

        public Frame InvokeMethod(Frame caller, string name,
                LValue[] pos, Dictionary<string, LValue> named) {
            IP6 m = klass.methods[name];
            if (m != null) {
                // XXX this breaks the static call nesting rule; does it need
                // to be rewritten or can the rule be safely loosened?
                return m.Invoke(caller, pos, named);
            } else {
                return Fail(caller, "Unable to resolve method " + name);
            }
        }

        public Frame GetAttribute(Frame caller, string name) {
            caller.resultSlot = slots[name];
            return caller;
        }

        public Frame HOW(Frame caller) {
            caller.resultSlot = klass.how;
            return caller;
        }

        public Frame Invoke(Frame c, LValue[] p, Dictionary<string, LValue> n) {
            if (klass.OnInvoke != null) {
                return klass.OnInvoke(this, c, p, n);
            } else {
                return Fail(c, "No invoke handler set");
            }
        }

        public Frame Fetch(Frame c) {
            if (klass.OnFetch != null) {
                return klass.OnFetch(this, c);
            } else {
                return Fail(c, "No fetch handler set");
            }
        }

        public Frame Store(Frame c, IP6 o) {
            if (klass.OnStore != null) {
                return klass.OnStore(this, c, o);
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
    // Provides: ClassHOW, ClassHOW.HOW, ClassHOW.add_method, ScalarContainer,
    // ScalarContainer.HOW, Code, Code.HOW, Body, Body.HOW, Scope, Scope.HOW,
    // ...
    // This should be enough to implement the rest of ClassHOW :)
    public class Kernel {
        public static readonly Frame KernelFrame = new Frame(null, null, null);

        private static Frame SCFetch(DynObject th, Frame caller) {
            caller.resultSlot = th.slots["!value"];
            return caller;
        }

        private static Frame SCStore(DynObject th, Frame caller, IP6 nv) {
            th.slots["!value"] = nv;
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
                    b.slots["!outer"] = c;
                    th.caller.resultSlot = NewROVar(b);
                    return th.caller;
                default:
                    return Kernel.Die(th, "invalid IP");
            }
        }

        private static Frame SubInvoke(DynObject th, Frame caller,
                LValue[] pos, Dictionary<string,LValue> named) {
            Frame proto = (Frame) th.slots["!proto"];
            Frame outer = (Frame) th.slots["!outer"];
            DynBlockDelegate code = (DynBlockDelegate) th.slots["!code"];

            Frame n = new Frame(caller, outer, code);
            n.proto = proto;
            n.pos = pos;
            n.named = named;

            return n;
        }

        public static Frame Die(Frame caller, string msg) {
            Frame f = new Frame(caller, null, new DynBlockDelegate(ThrowC));
            f.pos = new LValue[1] { new LValue(true, new CLRImportObject(msg)) };
            f.named = null;
            return f;
        }

        // Needs more special handling for control exceptions.
        private static Frame ThrowC(Frame th) {
            IP6 a;
            switch (th.ip) {
                case 0:
                    th.lex["$cursor"] = NewRWVar(th.caller);
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
                    return a.GetAttribute(th, "!exn_skipto");
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
                    return a.GetAttribute(th, "!exn_handler");
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
        public static readonly DynMetaObject ScalarContainerMO;
        public static readonly IP6 DieSub;

        public static IP6 MakeSub(DynBlockDelegate code, Frame proto,
                Frame outer) {
            DynObject n = new DynObject();
            n.klass = SubMO;
            n.slots["!outer"] = outer;
            n.slots["!code"] = code;
            n.slots["!proto"] = proto;
            return n;
        }

        public static IP6 MakeSC(IP6 inside) {
            DynObject n = new DynObject();
            n.klass = ScalarContainerMO;
            n.slots["!value"] = inside;
            return n;
        }

        public static LValue NewROLValue(IP6 inside) {
            return new LValue(false, MakeSC(inside));
        }

        public static LValue NewRWLValue(IP6 inside) {
            return new LValue(true, MakeSC(inside));
        }

        public static Variable NewROVar(IP6 inside) {
            return new Variable(true, NewROLValue(inside));
        }

        public static Variable NewRWVar(IP6 inside) {
            return new Variable(true, NewRWLValue(inside));
        }

        static Kernel() {
            SubMO = new DynMetaObject();
            SubMO.name = "Sub";
            SubMO.OnInvoke = new DynMetaObject.InvokeHandler(SubInvoke);
            SubMO.methods["clone"] = MakeSub(new DynBlockDelegate(SubCloneC),
                    null, null);

            ScalarContainerMO = new DynMetaObject();
            ScalarContainerMO.name = "ScalarContainer";
            ScalarContainerMO.OnFetch = new DynMetaObject.FetchHandler(SCFetch);
            ScalarContainerMO.OnStore = new DynMetaObject.StoreHandler(SCStore);

            DieSub = MakeSub(new DynBlockDelegate(ThrowC), null, null);
        }
    }
}
