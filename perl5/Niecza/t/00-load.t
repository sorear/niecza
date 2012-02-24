#!perl -T

use Test::More tests => 1;

BEGIN {
    use_ok( 'Niecza' ) || print "Bail out!\n";
}

diag( "Testing Niecza $Niecza::VERSION, Perl $], $^X" );
