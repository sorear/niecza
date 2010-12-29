package Op::Helpers;
# A helper to make creating Op::Let more convenient
sub let {
    my ($value,$body) = @_;
    my $var = Niecza::Actions->gensym;
    Op::Let->new(var => $var, to => $value, in => $body->(Op::LetVar->new(name=>$var)));
}
1;
