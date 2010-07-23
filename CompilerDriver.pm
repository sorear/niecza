package CompilerDriver;
use strict;
use warnings;
use 5.010;

use Sub::Exporter -setup => {
    exports => [ qw(header setting mainline ast) ]
};

open ::NIECZA_OUT, ">&", \*STDOUT;

BEGIN {
    unshift @INC, 'STD_checkout';
    $ENV{PERL6LIB} = "STD_checkout:STD_checkout/lib";
}

use Body ();
use Decl ();
use Unit ();
use Op ();
use Storable;

use Niecza::Grammar ();
use Niecza::Actions ();

sub header {
    print ::NIECZA_OUT <<EOH;
using System;
using System.Collections.Generic;
using Niecza;

EOH
}

sub setting {
    local $::SETTING_RESUME;
    local $::YOU_WERE_HERE;
    local $::UNITNAME = 'CORE';
    $STD::ALL = {};
    my $setting_ast = Niecza::Grammar->parsefile("CORE.setting",
        setting => 'NULL', actions => 'Niecza::Actions')->{_ast};

    $setting_ast->write;
    store $::SETTING_RESUME, 'CORE_ast.store';
}

sub mainline {
    my $code = shift;
    local $::UNITNAME = '';
    local $::SETTING_RESUME = retrieve 'CORE_ast.store';
    $STD::ALL = {};
    Niecza::Grammar->parse($code, actions => 'Niecza::Actions')->{_ast}->write;
}

sub ast {
    my $code = shift;
    local $::UNITNAME = 'Mainline';
    $STD::ALL = {};
    my $a = Niecza::Grammar->parse($code, actions => 'Niecza::Actions')->{_ast};
    delete $a->mainline->{outer};
    delete $a->{setting};
    print YAML::XS::Dump($a);
}

1;
