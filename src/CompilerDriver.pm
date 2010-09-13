package CompilerDriver;
use strict;
use warnings;
use 5.010;

use Sub::Exporter -setup => {
    exports => [ qw(compile) ]
};

use Time::HiRes 'time';
use File::Basename;
use autodie ':all';

open ::NIECZA_OUT, ">&", \*STDOUT;

use Body ();
use Decl ();
use Unit ();
use Op ();
use Optimizer::Beta ();
use ResolveLex ();
use Storable;

use Niecza::Grammar ();
use Niecza::Actions ();

my ($srcdir, $rootdir, $builddir, $libdir);
{
    $srcdir   = dirname($INC{'CompilerDriver.pm'});
    $rootdir  = dirname($srcdir);
    $builddir = File::Spec->catdir($rootdir, "obj");
    $libdir   = File::Spec->catdir($rootdir, "lib");
}
File::Path::make_path($builddir);

sub build_file { File::Spec->catfile($builddir, $_[0]) }

sub metadata_for {
    my ($unit) = @_;
    $unit =~ s/::/./g;

    Storable::retrieve(File::Spec->catfile($builddir, "$unit.store"))
}

sub get_perl6lib {
    $libdir, File::Spec->curdir
}

sub find_module {
    my $module = shift;
    my $issetting = shift;

    my @toks = split '::', $module;
    my $end = pop @toks;

    for my $d (get_perl6lib) {
        for my $ext (qw( .setting .pm6 .pm )) {
            next if ($issetting xor ($ext eq '.setting'));

            my $file = File::Spec->catfile($d, @toks, "$end$ext");
            next unless -f $file;

            if ($ext eq '.pm') {
                local $/;
                open my $pm, "<", $file or next;
                my $pmtx = <$pm>;
                close $pm;
                next if $pmtx =~ /^\s*package\s+\w+\s*;/m; # ignore p5 code
            }

            return $file;
        }
    }

    return;
}

{
    package
        CursorBase;
    no warnings 'redefine';

    sub sys_save_syml {
        my ($self, $all) = @_;
        $::niecza_mod_symbols = $all;
    }

    sub sys_do_compile_module {
        my ($self, $mod, $syml, $file) = @_;
        CompilerDriver::compile(name => $mod, stagetime => $::stagetime);
    }

    sub sys_load_modinfo {
        my $self = shift;
        my $module = shift;

        # these are handled in the compiler itself
        return { } if $module eq 'MONKEY_TYPING' || $module eq 'lib' ||
            $module eq 'fatal';

        my $csmod = $module;
        $csmod =~ s/::/./g;
        my ($symlfile) = File::Spec->catfile($builddir, "$csmod.store");
        my ($modfile) = CompilerDriver::find_module($module, 0) or do {
            $self->sorry("Cannot locate module $module");
            return undef;
        };

        REUSE: {
            last REUSE unless -f $symlfile;
            my $meta = Storable::retrieve($symlfile);

            for my $dmod (keys %{ $meta->{deps} }) {
                my ($dpath, $dtime) = @{ $meta->{deps}{$dmod} };

                my ($npath) = CompilerDriver::find_module($dmod, 0) or do {
                    $self->sorry("Dependancy $dmod of $module cannot be located");
                    return undef;
                };

                $npath = Cwd::realpath($npath);
                if ($npath ne $dpath) {
                    print STDERR "Recompiling $module because dependancy $dmod now points to $npath, was $dpath\n";
                    last REUSE;
                }

                my $ntime = (stat $npath)[9];
                if ($ntime ne $dtime) {
                    print STDERR "Recompiling $module because dependancy $dmod is newer ($dtime -> $ntime)\n";
                    last REUSE;
                }
            }

            return $meta->{'syml'};
        }

        $self->sys_compile_module($module, $symlfile, $modfile);
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

    my ($name, $file, $code, $lang, $safe, $setting) =
        @args{'name', 'file', 'code', 'lang', 'safe', 'setting'};

    $lang //= 'CORE';

    if (defined($name) + defined($file) + defined($code) != 1) {
        Carp::croak("Exactly one of name, file, and code must be used");
    }

    my $path = $file;
    if (defined($name)) {
        $path = find_module($name, $setting);
        if (!defined($path)) {
            Carp::croak("Module $name not found");
        }
    }

    local $::stagetime = $args{stagetime};
    local %::UNITREFS;
    local %::UNITREFSTRANS;
    local %::UNITDEPSTRANS;
    local $::SETTING_RESUME;
    local $::niecza_mod_symbols;
    local $::YOU_WERE_HERE;
    local $::UNITNAME = $name // 'MAIN';
    $::UNITNAME =~ s/::/./g;
    local $::SAFEMODE = $safe;
    $STD::ALL = {};

    if ($lang ne 'NULL') {
        my $metasetting = metadata_for($lang);
        $::SETTING_RESUME = $metasetting->{setting};
        $::UNITREFS{$lang} = 1;
        $::UNITREFSTRANS{$lang} = 1;
        %::UNITREFSTRANS = (%::UNITREFSTRANS, %{ $metasetting->{trefs} });
    }

    if (defined($name) && !$setting) {
        my $rp = Cwd::realpath($path);
        $::UNITDEPSTRANS{$name} = [ $rp, ((stat $rp)[9]) ];
    }

    if (defined($name)) {
        $::UNITREFSTRANS{$name} = 1;
    }

    my ($m, $a) = defined($path) ? (parsefile => $path) : (parse => $code);

    my $ast;
    my $basename = $::UNITNAME;
    my $csfile = File::Spec->catfile($builddir, "$basename.cs");
    my $outfile = File::Spec->catfile($builddir,
        $basename . (defined($name) ? ".dll" : ".exe"));

    my @phases = (
        [ 'parse', sub {
            $ast = Niecza::Grammar->$m($a, setting => $lang,
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

            open ::NIECZA_OUT, ">", $csfile;
            binmode ::NIECZA_OUT, ":utf8";
            print ::NIECZA_OUT <<EOH;
using System;
using System.Collections.Generic;
using Niecza;

EOH
            $ast->write;
            close ::NIECZA_OUT;
            if (defined $name) {
                my $blk = { setting => $::SETTING_RESUME,
                            deps    => \%::UNITDEPSTRANS,
                            refs    => \%::UNITREFS,
                            trefs   => \%::UNITREFSTRANS,
                            syml    => $::niecza_mod_symbols };
                store $blk, File::Spec->catfile($builddir, "$basename.store");
            }
            $ast = undef;
        } ],
        [ 'gmcs', sub {
            delete $::UNITREFS{$basename};
            my @args;
            if ($args{selfcontained}) {
                @args = ("gmcs",
                    "/out:" . $args{selfcontained},
                    (map { File::Spec->catfile($libdir, $_) }
                        "Kernel.cs", "Cursor.cs"),
                    (map { build_file($_ . ".cs") }
                        (sort keys %::UNITREFSTRANS)),
                    $csfile);
            } else {
                @args = ("gmcs",
                    (defined($name) ? ("/target:library") : ()),
                    "/lib:$builddir",
                    "/r:Kernel.dll",
                    (map { "/r:$_.dll" } sort keys %::UNITREFS),
                    "/out:$outfile",
                    $csfile);
            }
            print STDERR "@args\n" if $args{stagetime};
            system @args;
        } ],
        [ 'aot', sub {
            system "mono", "--aot", $outfile;
        } ]);

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
