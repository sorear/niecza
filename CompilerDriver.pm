package CompilerDriver;
use strict;
use warnings;
use 5.010;

use Sub::Exporter -setup => {
    exports => [ qw(header trailer setting mainline) ]
};

BEGIN {
    use File::Slurp;
    GETBASE: {
        for (read_file 'Makefile') {
            if (/^STDBASE=(.*)/) {
                unshift @INC, $1;
                $ENV{PERL6LIB} = "$1:$1/lib";
                last GETBASE;
            }
        }
        die "Cannot scrape STDBASE from Makefile";
    }
}

use Body ();
use Decl ();
use Unit ();
use Op ();
use Storable;

use Niecza::Grammar ();
use Niecza::Actions ();

sub header {
    print <<EOH;
using System;
using System.Collections.Generic;
using Niecza;

EOH
}

sub setting {
    local $::SETTING_RESUME;
    local $::YOU_WERE_HERE;
    local $::UNITNAME = 'Setting';
    $STD::ALL = {};
    my $setting_ast = Niecza::Grammar->parsefile("setting", setting => 'NULL',
        actions => 'Niecza::Actions')->{_ast};

    $setting_ast->write;
    store $::SETTING_RESUME, 'setting_ast.store';
}

sub mainline {
    my $code = shift;
    local $::UNITNAME = 'Mainline';
    local $::SETTING_RESUME = retrieve 'setting_ast.store';
    $STD::ALL = {};
    Niecza::Grammar->parse($code, actions => 'Niecza::Actions')->{_ast}->write;
}

sub trailer {
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
}

1;
