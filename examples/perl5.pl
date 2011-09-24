eval(q:to/PERL5/,:lang<perl5>);
    print "Hel"."lo "; 
    PERL5
eval(q:to/PERL5/,:lang<perl5>);
    print "World\n";
    PERL5

eval(q:to/PERL5/,:lang<perl5>);
use strict;
use warnings;
package Foo;
sub baz {
    my ($self,$arg) = @_;
    print "calling method\n";
}
sub new {
    bless {},"Foo";
}
PERL5
my $foo = eval(:lang<perl5>,'Foo->new');
$foo.baz(eval(:lang<perl5>,'"Hello World"'));
