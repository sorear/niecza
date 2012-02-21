package Niecza::Interoperability;
use v5.10;
use Package::Stash;
my $id = 0;
sub use_module {
    my ($module) = @_; 
    my $stash = Package::Stash->new('main');#'Niecza::Stash::Interopability::TMP'.$id++);
    eval("package ".$stash->name.";use $module;");
    warn $@ if $@;
    my @subs = $stash->list_all_symbols('CODE');
    for my $symbol (@subs) {
        #say "importing sub $symbol from $module";
    }
    $subs[0];
}
1;
