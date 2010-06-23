namespace Sprixel {
    // We like to reuse continuation objects for speed - every function only
    // creates one kind of continuation, but tweaks a field for exact return
    // point.  As such, call frames and continuations are in 1:1 correspondence
    // and are unified.  Functions take a current continuation and return a new
    // continuation; we tail recurse with trampolines.

    // Only call other functions in Continue, not in the CallableDelegate or
    // equivalent!
    public delegate FrameBase CallableDelegate(FrameBase caller,
            IPerl6Object argParcel);
    // Used by DynFrame to plug in code
    public delegate void DynBlockDelegate(DynamicFrame frame);

    public interface IPerl6Object {
        public FrameBase Invoke(FrameBase caller, IPerl6Object argParcel);
        public FrameBase InvokeMethod(FrameBase caller, string name,
                IPerl6Object argParcel);
        public FrameBase GetAttribute(FrameBase caller, string name);
        public FrameBase WHERE(FrameBase caller);
        public FrameBase HOW(FrameBase caller);
    }

    public abstract class FrameBase: IPerl6Object {
        public readonly FrameBase caller;
        public readonly FrameBase outer;
        public IPerl6Object resultSlot = null;
        public int ip = 0;

        public abstract FrameBase Continue();
    }

    public class DynFrame: FrameBase {
        public readonly DynBlockDelegate code;
        public readonly Dictionary<string, IPerl6Object> lex
            = new Dictionary<string, IPerl6Object>;

        public DynFrame(FrameBase caller_, FrameBase outer_,
                DynBlockDelegate code_) {
            caller = caller_;
            outer = outer_;
            code = code_;
        }

        public FrameBase Continue() {
            return code(this);
        }

        public FrameBase GetAttribute(FrameBase c, string name) {
            c.resultSlot = lex[name];
        }
    }

    public class ExceptionHelper: FrameBase {
        private FrameBase cursor;
        private IPerl6Object toThrow;

        private ExceptionHelper(FrameBase caller_) { caller = caller_; }

        public static FrameBase Throw(FrameBase caller, IPerl6Object exn) {
            var n = new ExceptionHelper();
            n.cursor = caller;
            n.toThrow = exn;
            return n;
        }

        public FrameBase Continue() {
            switch (ip) {
                case 0:
                    if (cursor == null) {
                        throw new Exception("Unhandled Perl 6 exception");
                    }
                    ip = 1;
                    resultSlot = null;
                    return cursor.GetAttribute(this, "!exn_skipto");
                case 1:
                    // if skipto, skip some frames.  Used to implement CATCH
                    // invisibility
                    if (resultSlot != null) {
                        cursor = (FrameBase)resultSlot;
                        goto case 0;
                    }
                    ip = 2;
                    resultSlot = null;
                    return cursor.GetAttribute(this, "!exn_handler");
                case 2:
                    if (resultSlot != null) {
                        return resultSlot.invoke(caller, toThrow);
                    }
                    cursor = cursor.caller;
                    goto case 0;
            }
        }
    }

    // This is quite similar to DynFrame and I wonder if I can unify them.
    public class DynObject: IPerl6Object {
        public Dictionary<string, IPerl6Object> slots;
        public Dictionary<string, IPerl6Object> methods;
        public IPerl6Object how;

        public FrameBase InvokeMethod(FrameBase caller, string name,
                IPerl6Object argParcel) {
            IPerl6Object m = methods[name];
            if (m != null) {
                // XXX this breaks the static call nesting rule; does it need
                // to be rewritten or can the rule be safely loosened?
                // TODO: methods need arguments, too.
                return m.Invoke(caller, this);
            } else {
                return Sprixel.Callout.InvokeFailed(caller, argParcel);
            }
        }

        public FrameBase Invoke(FrameBase caller, IPerl6Object argParcel) {
            IPerl6Object d = slots["clr-delegate"];
            if (d != null) {
                return (Sprixel.CallableDelegate)(((CLRImportObject)d).val)
                    (caller, argParcel);
            } else {
                // TODO needs to be CPS
                // $.clr-delegate //= self.codegen
                // run it
            }
        }

        public FrameBase GetAttribute(FrameBase caller, string name) {
            caller.resultSlot = slots[name];
            return caller;
        }

        public FrameBase HOW(FrameBase caller) {
            caller.resultSlot = how;
            return caller;
        }

        public FrameBase WHICH(FrameBase caller) {
            /* return a proxy for the Object which uses referential equality */
        }
    }
}
