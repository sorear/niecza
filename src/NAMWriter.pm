use v5.10;
use strict;
use warnings;
use Niecza::Simple;
use File::Slurp qw(slurp);
sub compile {
    my %args = @_;

    my $filename = "lib/$args{name}.setting";
    my $source = slurp($filename);
    my $compiler = Niecza::Simple::create_compiler(backend=>"nam",lang=>$args{lang},UNITNAME=>$args{name});
    $compiler->compile(source=>$source,filename=>$filename,output=>"obj/$args{name}.nam");


}
1;
