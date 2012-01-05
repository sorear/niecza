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

my class ObjectPipeWriteHandle {
    has $.op;
    method put($x) {
        $.op.put($x);
    }
    method dup {
        return $!op.write_handle();
    }
    method DESTROY {
        $!op.writer_closed(self);
    }
}

my class ObjectPipeReadHandle {
    has $.op = 0;
    method get() {
        $.op.get();
    }
    method dup {
        return $!op.read_handle();
    }
    method DESTROY {
        $!op.reader_closed(self);
    }
}

class ObjectPipe {
    has $!lock = Monitor.new;
    has $!queue = [];
    has $!max_buffer_size = 10;
    has $!writers = 0;
    has $!readers = 0;
    has $!eod = False;

    method read_handle {
        $!readers++;
        my $read = ObjectPipeReadHandle.new();
        $read.op = self;
        return $read;
    }

    method reader_closed {
        $!readers--;
        sleep 0.1;
        $!lock.enter;
        $!lock.pulse;
        $!lock.exit;
    }

    method write_handle {
        $!writers++;
        my $write = ObjectPipeWriteHandle.new();
        $write.op = self;
        return $write;
    }

    method writer_closed {
        $!writers--;
        $!lock.enter;
        $!lock.pulse;
        $!lock.exit;
    }

    method get() {
        return EMPTY if $!eod;
        $!lock.enter;
        while (!$!queue && $!writers) {
            $!lock.wait;
        }
        if (!($!queue || $!writers)) {
            die "Object Pipe closed";
        }
        my $value = shift $!queue;
        $!lock.pulse;
        $!lock.exit;
        $value;
    }

    method put($x) {
        $!lock.enter;
        while (($!queue.elems >= $!max_buffer_size) && $!readers) {
            $!lock.try_wait(0.1);
        }
        if ($!readers < 1) {
            die "Object Pipe closed";
        }
        push $!queue, $x;
        $!lock.pulse;
        $!lock.exit;
    }
}

sub objectpipe is export {
    my $op = ObjectPipe.new();
    return $op.read_handle(), $op.write_handle();
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
