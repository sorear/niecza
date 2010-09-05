# vim: ft=perl6
use Test;

#ok '{}' ~~ / \{ <.ws> \} /, 'ws matches between \W';

sub rxt($C) {
    Q:CgOp {
        (prog
          (setfield rx (callframe) (rawnew RxFrame (cast Cursor (@ {$C}))))
          (rxpushb SEQALT b1)
          (rxbprim ExactOne (char x))
          (goto b2)
          (label b1)
          (rxbprim ExactOne (char y))
          (label b2)
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

is +rxt(Cursor.new("x")), 1, "/x||y/ ~~ x";
is +rxt(Cursor.new("y")), 1, "/x||y/ ~~ y";
is +rxt(Cursor.new("z")), 0, "/x||y/ !~~ z";

done-testing;
