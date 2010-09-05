# vim: ft=perl6
use Test;

#ok '{}' ~~ / \{ <.ws> \} /, 'ws matches between \W';

sub rxt($C) {
    Q:CgOp {
        (prog
          (setfield rx (callframe) (rawnew RxFrame (cast Cursor (@ {$C}))))
          (rxbprim ExactOne (char x))
          (rawccall (getfield rx (callframe)) End)
          (rawccall (getfield rx (callframe)) Backtrack)
          (null Variable))
    }
}

PRE-INIT {
    Q:CgOp {
        (prog
          (rawsset RxFrame.EMPTYP (@ {EMPTY}))
          (rawsset RxFrame.ListMO (getfield klass (cast DynObject (@ {List}))))
          (rawsset RxFrame.LLArrayMO (getfield klass
              (cast DynObject (@ {LLArray}))))
          (rawsset RxFrame.GatherIteratorMO (getfield klass
              (cast DynObject (@ {GatherIterator}))))
          (null Variable))
    }
}

is +rxt(Cursor.new("x")), 1, "/x/ ~~ x";
is +rxt(Cursor.new("y")), 0, "/x/ !~ y";

done-testing;
