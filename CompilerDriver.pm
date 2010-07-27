package CompilerDriver;
use strict;
use warnings;
use 5.010;

use Sub::Exporter -setup => {
    exports => [ qw(compile) ]
};

use Time::HiRes 'time';

use autodie ':all';

open ::NIECZA_OUT, ">&", \*STDOUT;

BEGIN {
    unshift @INC, 'STD_checkout';
    $ENV{PERL6LIB} = ".:STD_checkout:STD_checkout/lib";
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

    my $ast;
    my $basename;

    my @phases = (
        [ 'parse', sub {
            $ast = Niecza::Grammar->$m($a, setting => $args{lang},
                actions => 'Niecza::Actions')->{_ast}; } ],
        [ 'lift_decls', sub {
            $::SETTING_RESUME = undef;
            $ast->lift_decls; } ],
        [ 'extract_scopes', sub { $ast->extract_scopes } ],
        [ 'to_cgop', sub { $ast->to_cgop } ],
        [ 'to_anf', sub { $ast->to_anf } ],
        [ 'writecs', sub {
            $basename = $::UNITNAME;
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
            $ast = undef; } ],
        [ 'gmcs', sub {
            system "gmcs", ($args{main} ? () : ("/target:library")),
                "/r:Kernel.dll", (map { "/r:$_.dll" } @::UNITDEPS),
                "/out:${basename}." . ($args{main} ? 'exe' : 'dll'),
                "${basename}.cs"; } ],
        [ 'aot', sub {
            system "mono", "--aot", "${basename}." .
                ($args{main} ? 'exe' : 'dll'); } ]);

    for my $p (@phases) {
        next if $p->[0] eq 'aot' && !$args{aot};
        my $t1 = time if $args{stagetime};
        $p->[1]->();
        my $t2 = time if $args{stagetime};
        printf "%-20s: %gs\n", (($::UNITNAME || 'MAIN') . " " . $p->[0]),
            $t2 - $t1 if $args{stagetime};
        if ($args{stopafter} && $args{stopafter} eq $p->[0]) {
            if ($ast) {
                print STDERR YAML::XS::Dump($ast);
            }
            return;
        }
    }
}

1;
