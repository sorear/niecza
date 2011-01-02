use v5.10;
use strict;
use warnings;
use Test::More;
use Niecza::Frontend::STD;
my $parser = Niecza::Frontend::STD->new(lang=>'CORE');
my $ast = $parser->parse('123');
isa_ok($ast,'Unit');
done_testing;
