use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Niecza',
    license             => 'perl',
    dist_author         => 'Paweł Murias <pawelmurias@gmail.com>',
    dist_version_from   => 'lib/Niecza.pm',
    include_dirs        => ['.'],
    requires => {
        'Test::More' => 0,
    },
    add_to_cleanup      => [ 'Niecza-*' ],
);

$builder->create_build_script();
