use strict;
use warnings;
use 5.010;

use Body ();
use Decl ();
use Unit ();
use Op ();

use Niecza::Grammar ();
use Niecza::Actions ();

{
    local $::UNITNAME = 'Mainline';
    Niecza::Grammar->parsefile("setting", setting => 'NULL', actions => 'Niecza::Actions')->{_ast}->write;
}

print <<EOF;
public class EntryPoint {
    public static Frame START(Frame th) {
        Frame t;
        switch (th.ip) {
            case 0:
                t = new Frame(th, th, new DynBlockDelegate(Mainline.BOOT));
                t.pos = new LValue[1] { Kernel.NewROLValue(th) };
                th.ip = 1;
                return t;
            case 1:
                th.ip = 2;
                return ((IP6)th.resultSlot).Invoke(th, new LValue[0] {}, null);
            case 2:
                return null;
            default:
                throw new Exception("IP corruption");
        }
    }

    public static void Main() {
        Frame root_f = new Frame(null, null,
                new DynBlockDelegate(START));
        Frame current = root_f;
        while (current != null) {
            current = current.Continue();
        }
    }
}
EOF
