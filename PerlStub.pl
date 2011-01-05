use strict; use warnings;
open my $realstderr, ">&STDERR";
open STDERR, ">&STDOUT";

use lib 'src';
use NAMWriter;

while(1) {
    my $line = <STDIN>;
    last unless defined($line) && length($line);
    eval $line;
    if ($@) {
        print $@;
        syswrite $realstderr, 'E';
    } else {
        syswrite $realstderr, 'R';
    }
}
