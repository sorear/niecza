class NAME {
    has $.name;
    has $.xlex;
    has $.olex;
    has $.of;
    has $.file;
    has $.line;

    method new(:$name, :$xlex, :$olex, :$of, :$file, :$line, *%extras) {
        my $new = self.CREATE;
        $new!name = $name;
        $new!xlex = $xlex;
        $new!olex = $olex;
        $new!of = $of;
        $new!file = $file;
        $new!line = $line;
        $new;
    }
}
