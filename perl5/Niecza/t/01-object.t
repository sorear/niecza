
use Test::More tests => 10;

BEGIN {
use_ok( 'Niecza' );
}

my $obj;

ok( $obj = Niecza->new(), "no initializer");
isa_ok($obj,"Niecza");

ok( $obj = Niecza->new(1), "initial numeric value");
ok($obj->{value} == 1, "implicit initializer");

ok( $obj = Niecza->new("fish"), "initial string value");
ok($obj->{value} eq "fish", "implicit initializer");

ok( $obj = Niecza->new(color => "red", flavor => "sour"), 
	"hash as initializer");
ok( $obj->{color} eq "red", "first hash key");
ok( $obj->{flavor} eq "sour", "first hash key");
