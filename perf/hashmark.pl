# vim: ft=perl6
use MONKEY_TYPING;
augment class Hash {
}

augment class Array {
}

my $i = 0;
my %hash;
%hash{$i} = $i until ($i++) == 1000000;
