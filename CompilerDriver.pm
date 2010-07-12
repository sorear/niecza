use strict;
use warnings;
use 5.010;

use Body ();
use Decl ();
use Unit ();
use Op ();

use Niecza::Grammar ();
use Niecza::Actions ();

print <<EOH;
using System;
using System.Collections.Generic;
using Niecza;

EOH

local $::SETTING_RESUME;

{
    local $::YOU_WERE_HERE;
    local $::UNITNAME = 'Setting';
    my $setting_ast = Niecza::Grammar->parsefile("setting", setting => 'NULL',
        actions => 'Niecza::Actions')->{_ast};

    $setting_ast->write;
}

{
    local $::UNITNAME = 'Mainline';
    Niecza::Grammar->parse("say('Hello, world')", actions => 'Niecza::Actions')->{_ast}->write;
}

print <<EOF;
public class EntryPoint {
    public static Frame START(Frame th) {
        Frame t;
        switch (th.ip) {
            case 0:
                t = new Frame(th, th, new DynBlockDelegate(Setting.BOOT));
                t.pos = new LValue[1] { Kernel.NewROLValue(th) };
                th.ip = 1;
                return t;
            case 1:
                th.ip = 2;
                return ((Variable)th.resultSlot).lv.container.Fetch(th);
            case 2:
                th.ip = 3;
                return ((IP6)th.resultSlot).Invoke(th, new LValue[0] {}, null);
            case 3:
                return null;
            default:
                throw new Exception("IP corruption");
        }
    }

    public static void Main() {
        Kernel.MainlineContinuation = new DynBlockDelegate(Mainline.BOOT);
        Frame root_f = new Frame(null, null,
                new DynBlockDelegate(START));
        Frame current = root_f;
        while (current != null) {
            current = current.Continue();
        }
    }
}
EOF
