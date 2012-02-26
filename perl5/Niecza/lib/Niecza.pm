package Niecza;

use warnings;
use strict;

=head1 NAME

Niecza - The perl5-niecza interoperability support;

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

require XSLoader;
XSLoader::load('Niecza', $VERSION);

use Niecza::Helpers;
use Niecza::Object;

=head1 SYNOPSIS

use v6;
eval(:lang<perl5>,"...");

=head1 AUTHOR

Paweł Murias, C<< <pawelmurias@gmail.com> >>

=head1 BUGS

Please report any bugs or feature requests on the #perl6 on freenode.org IRC channel or using the github bugtracker for niecza.

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2012 Paweł Murias, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1; # End of Niecza
