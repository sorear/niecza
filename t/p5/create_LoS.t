use v6;
use Test;
my $LoS = eval(:lang<perl5>,q:to/PERL5/);
use Data::Dumper;
my $LoS = Niecza::create_LoS(["foo1","bar1","baz1"]);
$LoS;
PERL5
is $LoS[0],"foo1";
is $LoS[1],"bar1";
is $LoS[2],"baz1";
done;
