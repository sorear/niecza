package CompilerDriver;
use strict;
use warnings;
use 5.010;

use Sub::Exporter -setup => {
    exports => [ qw(compile) ]
};

use autodie ':all';

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

sub compile {
    my %args = @_;
    $args{lang} //= 'CORE';

    local @::UNITDEPS;
    local $::SETTING_RESUME;
    local $::YOU_WERE_HERE;
    local $::UNITNAME = $args{main} ? '' : $args{file};
    $::UNITNAME =~ s/\.(?:pm6?|setting)//;
    $::UNITNAME =~ s|[\\/]|.|g;
    $STD::ALL = {};

    $::SETTING_RESUME = retrieve($args{lang} . '_ast.store')
        unless $args{lang} eq 'NULL';
    push @::UNITDEPS, $args{lang} if $args{lang} ne 'NULL';

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

    my $basename = $::UNITNAME;
    $basename =~ s/::/\//g;
    $basename ||= 'MAIN';

    open ::NIECZA_OUT, ">", $basename . ".cs";
    print ::NIECZA_OUT <<EOH;
using System;
using System.Collections.Generic;
using Niecza;

EOH
    $ast->write;
    close ::NIECZA_OUT;
    store $::SETTING_RESUME, ($basename . '_ast.store')
        if $::SETTING_RESUME;

    return if $args{csonly};

    system "gmcs", ($args{main} ? () : ("/target:library")), "/r:Kernel.dll",
        (map { "/r:$_.dll" } @::UNITDEPS),
        "/out:${basename}." . ($args{main} ? 'exe' : 'dll'), "${basename}.cs";
}

1;
