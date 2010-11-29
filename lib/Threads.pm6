my module Threads;

# Should be a role, since it can be applied to any class with minimal overhead
# XXX STD doesn't want to do the export if the class is our
my class Monitor is export {
    method enter() {
        Q:CgOp {
            (rnull (rawscall System.Threading.Monitor.Enter:m,Void
                (@ {self})))
        }
    }
    method exit() {
        Q:CgOp {
            (rnull (rawscall System.Threading.Monitor.Exit:m,Void
                (@ {self})))
        }
    }
    # TODO exception handling
    method lock($f) { self.enter; $f(); self.exit }
}

sub lock($m,$f) is export { $m.lock($f); }

my class Thread is export {
    has $!value;
    method new($func) {
        Q:CgOp { (box (@ {Thread}) (rawscall
            Kernel.StartP6Thread:c,System.Threading.Thread (@ {$func}))) }
    }

    method join() {
        Q:CgOp { (rnull (rawcall (unbox clr:System.Threading.Thread (@ {self})) Join:m,Void)) }
    }

    method sleep($time) {
        my $t = $time * 1000;
        Q:CgOp { (rnull (rawscall System.Threading.Thread.Sleep:m,Void (cast int (unbox num (@ {$t}))))) }
    }
}

sub sleep($time) is export { Thread.sleep($time) }
