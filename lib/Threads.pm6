module Threads;

# Should be a role, since it can be applied to any class with minimal overhead
class Monitor is export {
    method enter() { Q:CgOp {
        (rnull (rawscall System.Threading.Monitor.Enter (@ {self})))
    } }
    method exit() { Q:CgOp {
        (rnull (rawscall System.Threading.Monitor.Exit (@ {self})))
    } }
    method pulse() { Q:CgOp {
        (rnull (rawscall System.Threading.Monitor.Pulse (@ {self})))
    } }
    method pulse_all() { Q:CgOp {
        (rnull (rawscall System.Threading.Monitor.PulseAll (@ {self})))
    } }
    method try_enter($t) { Q:CgOp {
        (box Bool (rawscall System.Threading.Monitor.TryEnter (@ {self})
            (cast int (obj_getnum {$t * 1000}))))
    } }
    method wait() { Q:CgOp {
        (box Bool (rawscall System.Threading.Monitor.Wait (@ {self})))
    } }
    method try_wait($t) { Q:CgOp {
        (box Bool (rawscall System.Threading.Monitor.Wait (@ {self})
            (cast int (obj_getnum {$t * 1000}))))
    } }
    # TODO exception handling
    method lock($f) { self.enter; LEAVE self.exit; $f() }
}

sub lock($m,$f) is export { $m.lock($f); }

class ObjectPipe {
    has $!lock = Monitor.new;
    has $!queue = [];

    method get() {
        $!lock.enter;
        $!lock.wait until $!queue;
        my $value = shift $!queue;
        $!lock.exit;
        $value;
    }

    method put($x) {
        $!lock.enter;
        push $!queue, $x;
        $!lock.pulse;
        $!lock.exit;
    }
}

class Thread is export {
    has $!value;
    method new($func) {
        Q:CgOp { (box (@ {Thread}) (start_p6_thread (@ {$func}))) }
    }

    method join() {
        Q:CgOp { (rnull (rawcall Join (unbox clr:System.Threading.Thread (@ {self})))) }
    }

    method sleep($time) { sleep $time }
}
