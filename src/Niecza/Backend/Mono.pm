package Niecza::Backend::Mono;
use Moose;
has build_dir=>(is=>'ro');
has is_main=>(is=>'ro');
has tmp_file=>(is=>'ro');
has aot=>(is=>'ro');

sub compile {
    my ($self,$nam_file,$outfile) = @_;
    my @args = ("mono",
        File::Spec->catfile($self->build_dir, "CLRBackend.exe"), $self->build_dir,
        $nam_file,$outfile,$self->is_main);
    system @args;
    if ($self->aot) {
        system "mono", "--aot", $outfile;
    }
}
sub run {
    my ($self,$nam_file) = @_;

    # XXX our backend doesn't except a path
    #my $tmp = $self->tmp_file->();

    my $tmp = 'TMP.exe';

    $self->compile($nam_file,$tmp);
    system "mono",File::Spec->catfile($self->build_dir,$tmp);
}
1;
