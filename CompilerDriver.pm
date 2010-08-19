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

use lib 'STD_checkout';
BEGIN {
    use File::Spec;
    $CursorBase::SET_PERL6LIB = [ File::Spec->curdir ];
}

use Body ();
use Decl ();
use Unit ();
use Op ();
use Optimizer::Beta ();
use ResolveLex ();
use Storable;

use Niecza::Grammar ();
use Niecza::Actions ();

# TODO
my $builddir = File::Spec->curdir;

{
    package
        CursorBase;
    no warnings 'redefine';

    sub sys_save_syml {
        my ($self, $all) = @_;
        $::niecza_mod_symbols = $all;
    }

    sub sys_get_perl6lib {
        File::Spec->curdir
    }

    sub sys_load_modinfo {
        my $self = shift;
        my $module = shift;
        $module =~ s/::/./g;

        my ($symlfile) = File::Spec->catfile($builddir, "$module.store");
        my ($modfile) = $self->sys_find_module($module, 0)
            or return undef;

        unless (-f $symlfile and -M $modfile > -M $symlfile) {
            $self->sys_compile_module($module, $symlfile, $modfile);
        }
        return Storable::retrieve($symlfile)->{'syml'};
    }

    sub load_lex {
        my $self = shift;
        my $setting = shift;
        my $settingx = $setting;
        $settingx =~ s/::/./g;

        if ($setting eq 'NULL') {
            my $id = "MY:file<NULL.pad>:line(1):pos(0)";
            my $core = Stash->new('!id' => [$id], '!file' => 'NULL.pad',
                '!line' => 1);
            return Stash->new('CORE' => $core, 'MY:file<NULL.pad>' => $core,
                'SETTING' => $core, $id => $core);
        }

        my $astf = File::Spec->catfile($builddir, "$settingx.store");
        if (-e $astf) {
            return Storable::retrieve($astf)->{'syml'};
        }

        $self->sorry("Unable to load setting $setting.");
        return $self->load_lex("NULL");
    }
}

sub compile {
    my %args = @_;
    $args{lang} //= 'CORE';

    local %::UNITDEPS;
    local $::SETTING_RESUME;
    local $::niecza_mod_symbols;
    local $::YOU_WERE_HERE;
    local $::UNITNAME = $args{main} ? '' : $args{file};
    local $::SAFEMODE = $args{safe};
    $::UNITNAME =~ s/\.(?:pm6?|setting)//;
    $::UNITNAME =~ s|[\\/]|.|g;
    $STD::ALL = {};

    $::SETTING_RESUME = retrieve($args{lang} . '.store')->{setting}
        unless $args{lang} eq 'NULL';
    $::UNITDEPS{$args{lang}} = 1 if $args{lang} ne 'NULL';

    my ($m, $a) = $args{file} ? ('parsefile', $args{file}) :
        ('parse', $args{code});

    my $ast;
    my $basename = $::UNITNAME || 'MAIN';

    my @phases = (
        [ 'parse', sub {
            $ast = Niecza::Grammar->$m($a, setting => $args{lang},
                actions => 'Niecza::Actions')->{_ast}; } ],
        [ 'lift_decls', sub {
            $::SETTING_RESUME = undef;
            $ast->lift_decls; } ],
        [ 'beta', sub { Optimizer::Beta::run($ast) } ],
        [ 'extract_scopes', sub { $ast->extract_scopes } ],
        [ 'to_cgop', sub { $ast->to_cgop } ],
        [ 'resolve_lex', sub { ResolveLex::run($ast) } ],
        [ 'to_anf', sub { $ast->to_anf } ],
        [ 'writecs', sub {

            open ::NIECZA_OUT, ">", "$basename.cs";
            binmode ::NIECZA_OUT, ":utf8";
            print ::NIECZA_OUT <<EOH;
using System;
using System.Collections.Generic;
using Niecza;

EOH
            $ast->write;
            close ::NIECZA_OUT;
            if ($::SETTING_RESUME || $::niecza_mod_symbols) {
                my $blk = { setting => $::SETTING_RESUME,
                            syml    => $::niecza_mod_symbols };
                store $blk, "$basename.store";
            }
            $ast = undef; } ],
        [ 'gmcs', sub {
            delete $::UNITDEPS{$basename};
            system "gmcs", ($args{main} ? () : ("/target:library")),
                "/r:Kernel.dll", (map { "/r:$_.dll" } sort keys %::UNITDEPS),
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
        printf "%-20s: %gs\n", "$basename " . $p->[0],
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
