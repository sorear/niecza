package CompilerDriver;
use strict;
use warnings;
use 5.010;

use Sub::Exporter -setup => {
    exports => [ qw(compile) ]
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

    print ::NIECZA_OUT <<EOH;
using System;
using System.Collections.Generic;
using Niecza;

EOH

sub compile {
    my %args = @_;
    $args{lang} //= 'CORE';

    local $::SETTING_RESUME;
    local $::YOU_WERE_HERE;
    local $::UNITNAME = $args{main} ? '' : $args{file};
    $::UNITNAME =~ s/\.(?:pm6?|setting)//;
    $::UNITNAME =~ s|[\\/]|.|g;
    $STD::ALL = {};

    $::SETTING_RESUME = retrieve($args{lang} . '_ast.store')
        unless $args{lang} eq 'NULL';

    my ($m, $a) = $args{file} ? ('parsefile', $args{file}) :
        ('parse', $args{code});
    my $ast = Niecza::Grammar->$m($a, setting => $args{lang},
        actions => 'Niecza::Actions')->{_ast};

    if ($args{ast}) {
        delete $a->mainline->{outer};
        delete $a->{setting};
        print STDOUT YAML::XS::Dump($a);
        return;
    }

    $::SETTING_RESUME = undef;
    $ast->write;
    store $::SETTING_RESUME, ($::UNITNAME . '_ast.store')
        if $::SETTING_RESUME;
}

1;
