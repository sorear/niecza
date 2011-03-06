module Threads;

# Should be a role, since it can be applied to any class with minimal overhead
class Monitor is export {
    method enter() {
        Q:CgOp {
            (rnull (rawscall System.Threading.Monitor.Enter (@ {self})))
        }
    }
    method exit() {
        Q:CgOp {
            (rnull (rawscall System.Threading.Monitor.Exit (@ {self})))
        }
    }
    # TODO exception handling
    method lock($f) { self.enter; $f(); self.exit }
}

sub lock($m,$f) is export { $m.lock($f); }

class Thread is export {
    has $!value;
    method new($func) {
        Q:CgOp { (box (@ {Thread}) (rawscall
            Niecza.Kernel,Kernel.StartP6Thread:System.Threading.Thread (@ {$func}))) }
    }

    method join() {
        Q:CgOp { (rnull (rawcall Join (unbox clr:System.Threading.Thread (@ {self})))) }
    }

    method sleep($time) {
        my $t = $time * 1000;
        Q:CgOp { (rnull (rawscall System.Threading.Thread.Sleep (cast int (unbox num (@ {$t}))))) }
    }
}

sub sleep($time) is export { Thread.sleep($time) }
