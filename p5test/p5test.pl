#!/usr/bin/perl
use strict;
use warnings;
use Test::More;
use Config;
use ExtUtils::Embed;
use File::Spec::Functions qw(rel2abs);

my $cc = $Config{cc};
my $csc = "gmcs";

diag($Config{osname});
system($cc,"test1.c","-o","test1");
my $ok1 = `./test1`;

is $ok1,"OK 1\n","We have a working C compiler.";

my $ccopts = `perl -MExtUtils::Embed -e ccopts`; 
my $ldopts = `perl -MExtUtils::Embed -e ldopts`; 
chomp($ccopts);
chomp($ldopts);
system("$cc test2.c -o test2 $ccopts $ldopts");
my $ok2 = `./test2`;

is $ok2,"OK 2\n","We can embed p5.";

system($csc,"test3.cs");
my $ok3 = `mono test3.exe`;
is $ok3,"OK 3\n","We can run programs under mono";

my $lib_path4 = rel2abs("test4lib.$Config{so}");
open(my $test4_config,">test4.exe.config");
print $test4_config <<END;
 <configuration>
    <dllmap dll="test4lib" target="$lib_path4" />
</configuration>
END

system($csc,"test4.cs");
system("$cc -shared -o test4lib.so test4lib.c");
my $ok4 = `mono test4.exe`;
is $ok4,"OK 4\n","We call C code from mono";

my $lib_path5 = rel2abs("test5lib.$Config{so}");
open(my $test5_config,">test5.exe.config");
print $test5_config <<END;
 <configuration>
    <dllmap dll="test5lib" target="$lib_path5" />
</configuration>
END

system($csc,"test5.cs");
system("$cc -shared -o test5lib.so test5lib.c $ccopts $ldopts");
my $ok5 = `mono test5.exe`;
is $ok5,"OK 5\n","We call C code from mono";


done_testing;
 
