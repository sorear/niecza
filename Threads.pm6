module Threads;

# Should be a role, since it can be applied to any class with minimal overhead
class Monitor is export {
    method enter() {
        Q:CgOp {
            (prog (rawscall System.Threading.Monitor.Enter:m,Void
                (@ (l self))) (null Variable))
        }
    }
    method exit() {
        Q:CgOp {
            (prog (rawscall System.Threading.Monitor.Exit:m,Void
                (@ (l self))) (null Variable))
        }
    }
    # TODO exception handling
    method lock($f) { self.enter; $f(); self.exit }
}

class Thread is export {
    method new($func) {
        Q:CgOp { (box Thread (rawsccall
            Kernel.StartP6Thread:c,System.Threading.Thread (@ (l $func)))) }
    }

    method join() {
        Q:CgOp { (prog (rawcall (unbox System.Threading.Thread (@ (l self))) Join:m,Void) (null Variable)) }
    }

    method sleep($time) {
        my $t = $time * 1000;
        Q:CgOp { (prog (rawscall System.Threading.Thread.Sleep:m,Void (cast Int32 (unbox Double (@ (l $t))))) (null Variable)) }
    }
}
