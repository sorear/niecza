#!/usr/bin/perl
use strict;
use warnings;
use Test::More;
use Config;
use ExtUtils::Embed;
use File::Spec::Functions qw(rel2abs);
use File::Path qw(rmtree);

diag('osname = '.$Config{osname});

# get the right flags to embed p5
my $ccopts = `perl -MExtUtils::Embed -e ccopts`; 
my $ldopts = `perl -MExtUtils::Embed -e ldopts`; 
chomp($ccopts);
chomp($ldopts);

my $number = 0;
sub test { 
    $number++;
    my ($opts,$desc) = @_;
    my $path = "tmp";
    for my $cc ("$Config{cc}","$Config{cc} -m32") {
        mkdir($path);
        do_test($number,$path,{cc=>$cc,csc=>"gmcs"},$opts,$desc." using $cc");
        rmtree($path);
    }
}
sub do_test {
    my $ok;
    my ($number,$path,$env,$opts,$desc) = @_;
    my $cc = $env->{cc};
    my $csc = $env->{csc};

    my $p5flags = $opts->{embed_p5} ? " $ccopts $ldopts" : "";

    if ($opts->{cc}) {
        system("$cc test$number.c -o $path/test$number $p5flags");
        $ok = `$path/test$number`;
    }
    # we write a .config file to portably specify where the dynamic library is
    if ($opts->{config}) {
        my $lib_path = rel2abs("$path/test${number}lib.$Config{so}");
        open(my $test_config,">$path/test$number.exe.config");
    print $test_config <<END;
<configuration>
    <dllmap dll="test${number}lib" target="$lib_path" />
</configuration>
END
    }
    if ($opts->{so}) {
        system("$cc -shared -o $path/test${number}lib.$Config{so} test${number}lib.c $p5flags");
    }

    if ($opts->{mono}) {
        system($csc,"/out:$path/test$number.exe","test$number.cs");
        $ok = `mono $path/test$number.exe`;
    }

    is $ok,"OK $number\n",$desc;

}

test({cc=>1},"We have a working C compiler.");
test({cc=>1,embed_p5=>1},"We can embed p5.");
test({mono=>1},"We can run programs under mono");
test({config=>1,so=>1,mono=>1},"We can call C code from mono");
test({config=>1,so=>1,embed_p5=>1,mono=>1},"We can call p5 code from mono");
done_testing;
