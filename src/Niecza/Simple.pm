package Niecza::Simple;
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
    use File::Temp ();
    my $tmp_file = sub {
        File::Temp::tmpnam();
    };
    my $parser = Niecza::Frontend::STD->new(lang=>'CORE');
    
    my $begin = Niecza::Pass::Begin->new(lang=>'CORE');
    my $beta = Niecza::Pass::Beta->new();
    my $simplifier = Niecza::Pass::Simplifier->new();
    
    my $write_nam = Niecza::Pass::Backend->new(backend=>Niecza::Backend::NAM->new(),tmp_file=>$tmp_file);
    
    my $backend;
    if ($args{backend} eq 'mono') {
        $backend = Niecza::Backend::Mono->new(is_main=>1,build_dir=>'obj',tmp_file=>$tmp_file);
    } elsif ($args{backend} eq 'clisp') {
        $backend = Niecza::Backend::CLisp->new();
    }
    
    Niecza::Compiler->new(
        frontend =>$parser,
        passes  => [$begin,$beta,$simplifier,$write_nam],
        backend => $backend
    );
}
1;
