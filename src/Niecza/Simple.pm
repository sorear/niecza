package Niecza::Simple;
use v5.10;
use strict;
use warnings;

#XXX yuk!
use CompilerDriver;

use Niecza::Frontend::STD;
use Niecza::Backend::NAM;
use Niecza::Backend::Mono;
use Niecza::Backend::CLisp;
use Niecza::Pass::Beta;
use Niecza::Pass::Simplifier;
use Niecza::Pass::Begin;
use Niecza::Pass::Backend;
use Niecza::Compiler;
use Metamodel;
sub create_compiler {
    my %args = @_;
    my $lang = $args{lang} // 'CORE';
    use File::Temp ();
    my $tmp_file = sub {
        File::Temp::tmpnam();
    };
    my $parser = Niecza::Frontend::STD->new(lang=>$lang,UNITNAME=>$args{UNITNAME});
    
    my @passes;
    
    push @passes,Niecza::Pass::Begin->new(lang=>$lang);
    push @passes,Niecza::Pass::Beta->new();
    push @passes,Niecza::Pass::Simplifier->new();
    
    if ($args{backend} ne 'nam') {
        push @passes,Niecza::Pass::Backend->new(backend=>Niecza::Backend::NAM->new(),tmp_file=>$tmp_file);
    }
    
    my %backends = (
        mono => sub {
            Niecza::Backend::Mono->new(is_main=>1,build_dir=>'obj',tmp_file=>$tmp_file)
        },
        clisp => sub {
            Niecza::Backend::CLisp->new();
        },
        nam => sub {
            Niecza::Backend::NAM->new();
        },
    );

    my $backend = $backends{$args{backend}}->();


    Niecza::Compiler->new(
        frontend =>$parser,
        passes  => \@passes,
        backend => $backend
    );
}
1;
