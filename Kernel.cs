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
    }

    public class Variable {
        public LValue lv;
        public bool bvalue;
    }

    // We need hashy frames available to properly handle BEGIN; for the time
    // being, all frames will be hashy for simplicity
    public class Frame: IP6 {
        public readonly Frame caller;
        public readonly Frame outer;
        public object resultSlot = null;
        public int ip = 0;
        public readonly DynBlockDelegate code;
        public readonly Dictionary<string, Variable> lex
            = new Dictionary<string, Variable>();

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
            return KernelSetting.Die(c, "Tried to invoke a Frame");
        }

        public Frame InvokeMethod(Frame c, string nm, LValue[] p,
                Dictionary<string, LValue> n) {
            return KernelSetting.Die(c, "Method " + nm +
                    " not defined on Frame");
        }

        public Frame Fetch(Frame c) {
            return KernelSetting.Die(c, "Method FETCH not defined on Frame");
        }

        public Frame Store(Frame c, IP6 o) {
            return KernelSetting.Die(c, "Method STORE not defined on Frame");
        }

        public Frame HOW(Frame c) {
            //TODO
            return KernelSetting.Die(c, "No metaobject available for Frame");
        }
    }

    // This is quite similar to DynFrame and I wonder if I can unify them.
    // These are always hashy for the same reason as Frame above
    public class DynObject: IP6 {
        public Dictionary<string, Variable> slots
            = new Dictionary<string, Variable>();
        public Dictionary<string, IP6> methods;
        public IP6 how;

        public Frame InvokeMethod(Frame caller, string name,
                LValue[] pos, Dictionary<string, LValue> named) {
            IP6 m = methods[name];
            if (m != null) {
                // XXX this breaks the static call nesting rule; does it need
                // to be rewritten or can the rule be safely loosened?
                return m.Invoke(caller, pos, named);
            } else {
                return KernelSetting.Die(caller,
                        "Unable to resolve method " + name);
            }
        }

        public Frame GetAttribute(Frame caller, string name) {
            caller.resultSlot = slots[name];
            return caller;
        }

        public Frame HOW(Frame caller) {
            caller.resultSlot = how;
            return caller;
        }

        public Frame Invoke(Frame c, LValue[] p, Dictionary<string, LValue> n) {
            return KernelSetting.Die(c, "Tried to invoke a DynObject");
        }

        public Frame Fetch(Frame c) {
            return KernelSetting.Die(c, "Method FETCH not defined on DynObject");
        }

        public Frame Store(Frame c, IP6 o) {
            return KernelSetting.Die(c, "Method STORE not defined on DynObject");
        }
    }

    // Allows native CLR objects to be treated as Perl 6 data.  They don't
    // currently support any operations; you'll need to use CLR code to work
    // with them.
    public class CLRImportObject : IP6 {
        public readonly object val;

        public CLRImportObject(object val_) { val = val_; }

        public Frame GetAttribute(Frame c, string nm) {
            return KernelSetting.Die(c, "Attribute " + nm +
                    " not available on CLRImportObject");
        }

        public Frame Invoke(Frame c, LValue[] p, Dictionary<string, LValue> n) {
            return KernelSetting.Die(c, "Tried to invoke a CLRImportObject");
        }

        public Frame InvokeMethod(Frame c, string nm, LValue[] p,
                Dictionary<string, LValue> n) {
            return KernelSetting.Die(c, "Method " + nm +
                    " not defined on CLRImportObject");
        }

        public Frame Fetch(Frame c) {
            return KernelSetting.Die(c, "Method FETCH not defined on CLRImportObject");
        }

        public Frame Store(Frame c, IP6 o) {
            return KernelSetting.Die(c, "Method STORE not defined on CLRImportObject");
        }

        public Frame HOW(Frame c) {
            //TODO
            return KernelSetting.Die(c, "No metaobject available for CLRImportObject");
        }
    }

    // This should be a real class eventualy
    public class ScalarContainer : IP6 {
        public IP6 val;

        public ScalarContainer(IP6 val_) { val = val_; }

        public Frame Fetch(Frame caller) {
            caller.resultSlot = val;
            return caller;
        }

        public Frame Store(Frame caller, IP6 thing) {
            val = thing;
            return caller;
        }

        public Frame GetAttribute(Frame c, string nm) {
            return KernelSetting.Die(c, "Attribute " + nm +
                    " not available on ScalarContainer");
        }

        public Frame Invoke(Frame c, LValue[] p, Dictionary<string, LValue> n) {
            return KernelSetting.Die(c, "Tried to invoke a ScalarContainer");
        }

        public Frame InvokeMethod(Frame c, string nm, LValue[] p,
                Dictionary<string, LValue> n) {
            return KernelSetting.Die(c, "Method " + nm +
                    " not defined on ScalarContainer");
        }

        public Frame HOW(Frame c) {
            //TODO
            return KernelSetting.Die(c, "No metaobject available for ScalarContainer");
        }
    }

    // This, too
    public class Sub : IP6 {
        public DynBlockDelegate body;
        public Frame proto;
        public Frame outer;

        public Sub(DynBlockDelegate body_, Frame proto_, Frame outer_) {
            body = body_; proto = proto_; outer = outer_;
        }

        public Frame Invoke(Frame caller, LValue[] pos,
                Dictionary<string,LValue> named) {
            Frame n = new Frame(caller, outer, body);
            foreach (KeyValuePair<string,Variable> kv in proto.lex) {
                n.lex[kv.Key] = kv.Value; // TODO: Copy into a fresh variable
            }
            n.pos = pos;
            n.named = named;

            return n;
        }

        public Frame InvokeMethod(Frame c, string nm, LValue[] p,
                Dictionary<string, LValue> n) {
            return KernelSetting.Die(c, "Method " + nm +
                    " not defined on Sub");
        }

        public Frame GetAttribute(Frame c, string nm) {
            return KernelSetting.Die(c, "Attribute " + nm +
                    " not available on Sub");
        }

        public Frame Fetch(Frame c) {
            return KernelSetting.Die(c, "Method FETCH not defined on Sub");
        }

        public Frame Store(Frame c, IP6 o) {
            return KernelSetting.Die(c, "Method STORE not defined on Sub");
        }

        public Frame HOW(Frame c) {
            //TODO
            return KernelSetting.Die(c, "No metaobject available for Sub");
        }
    }

    // A bunch of stuff which raises big circularity issues if done in the
    // setting itself.
    // Provides: ClassHOW, ClassHOW.HOW, ClassHOW.add_method, ScalarContainer,
    // ScalarContainer.HOW, Code, Code.HOW, Body, Body.HOW, Scope, Scope.HOW,
    // ...
    // This should be enough to implement the rest of ClassHOW :)
    public class KernelSetting {
        public static readonly IP6 KernelFrame = new Frame(null, null, null);

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
                    th.lex["$cursor"] = new Variable(true, th.caller);
                case 1:
                    th.ip = 2;
                    th.resultSlot = null;
                    return th.lex["$cursor"].lv.Fetch(th);
                case 2:
                    a = th.resultSlot;
                    if (cursor == null) {
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
                        a = th.resultSlot;
                        th.resultSlot = null;
                        return th.lex["$cursor"].lv.Store(th, a);
                    }
                    th.ip = 4;
                    th.resultSlot = null;
                    return th.lex["$cursor"].lv.Fetch(th);
                case 4:
                    a = th.resultSlot;
                    th.ip = 5;
                    th.resultSlot = null;
                    return a.GetAttribute(th, "!exn_handler");
                case 5:
                    if (resultSlot != null) {
                        // tailcall
                        return th.resultSlot.Invoke(th.caller,
                                th.pos, th.named);
                    }
                    th.ip = 6;
                    th.resultSlot = null;
                    return th.lex["$cursor"].lv.Fetch(th);
                case 6:
                    a = ((Frame)th.resultSlot).caller;
                    th.ip = 1;
                    th.resultSlot = null;
                    return th.lex["$cursor"].lv.Store(th, a);
            }
            throw new Exception("IP invalid");
        }
    }

    public class MainClass {
        public static void Main() {
            Frame root_f = new Frame(null, null,
                    new DynBlockDelegate(BootC));
            Frame current = root_f;
            while (current) {
                current = current.Continue();
            }
        }

        // bootstrap function for the compilation unit.  runs phasers, sets up
        // runtime meta objects at BEGIN, then calls the mainline
        public static Frame BootC(Frame th) {
            switch (th.ip) {
                case 0:
                    // BEGIN
                    Frame main_f = new Frame(KernelSetting.KernelFrame);
                    Frame say_f = new Frame(main_f);
                    IP6 say_s = new Sub(
                        new DynBlockDelegate(SayC), say_f, main_f);
                    main_f.slots["&say"] = new Variable(false, say_s);
                    IP6 main_s = new Sub(
                        new DynBlockDelegate(MainC), main_f,
                        KernelSetting.KernelFrame);
                    // CHECK
                    // INIT
                    // DO
                    // could optimize this quite a bit since the mainline
                    // and setting both only run once.  For later.
                    IP6 main_c = ((Sub)main_s).clone(KernelSetting.KernelFrame);
                    th.ip = 1;
                    return main_c.Invoke(th, new LValue[0], null);
                case 1:
                    return th.caller;
            }
            return null;
        }

        public static Frame MainC(Frame th) {
            LValue c;
            IP6 d;
            switch (th.ip) {
                case 0:
                    th.ip = 1;
                    c = th.slots["&say"].lv;
                    return c.container.Fetch(th);
                case 1:
                    d = th.resultSlot;
                    th.slots["&say"] = new Variable(false, ((Sub)d).clone(th));
                    th.ip = 2;
                    th.resultSlot = null;
                    c = th.slots["&say"].lv;
                    return c.container.Fetch(th);
                case 2:
                    c = new LValue(new CLRImportObject("Hello, World"));
                    d = th.resultSlot;
                    th.ip = 3;
                    th.resultSlot = null;
                    return d.Invoke(th, new LValue[1] { c }, null);
                case 3:
                    return th.caller;
            }
        }

        public static Frame SayC(Frame th) {
            IP6 a;
            switch (th.ip) {
                case 0:
                    a = th.pos[0];
                    th.ip = 1;
                    return a.container.Fetch(th);
                case 1:
                    a = resultSlot;
                    System.Console.WriteLine((string)(((CLRImportObject)a).val));
                    return th.caller;
            }
            return null;
        }
    }
}
