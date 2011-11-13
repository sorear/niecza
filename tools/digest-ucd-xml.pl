my $UCD_XML = "/home/stefan/dl/ucd.nounihan.grouped.xml";
my $UCD_DIR = "/var/cache/stefan-downloads/UCD-6.0.0";


use 5.010;
use strict;
use warnings;

package InputHandler;

use parent qw(XML::SAX::Base);
use YAML;
use File::Slurp;
use Encode;
my @PropertyAliases = read_file("$UCD_DIR/PropertyAliases.txt");

sub start_document {
    my ($self, $e) = @_;

    $self->{description} = '';
    $self->{group} = {};
    $self->{indesc} = 0;
    $self->{stats} = {};
    $self->{count} = 0;
    $self->{last} = -1;
    $self->{tables} = {};

    $self->{files} = [];
    $self->{header} = '';

    $self->{p_named_seq} = [];
    $self->{named_seq} = [];

    print STDERR "Reading characters ... ", " " x 50;
}

sub _add_file {
    my ($self, $name, @buf) = @_;

    $self->{'header'} .= pack "Z*Z*cN*", $name, scalar(@buf),
        map { length } @buf;

    for (my $i = 0; $i < @buf; $i++) {
        $self->{'files'}[$i] .= $buf[$i];
    }

    my $tot = 0;
    $tot += length($_) for @buf;
    print STDERR "add $name, code $buf[0], ", join("+",map { length } @buf), " (tot $tot) bytes.\n";
}

sub collect_tokens {
    my ($self) = @_;

    my %toks;
    my @tx;
    my $t = $self->{tables}{na};
    for (my $i = 0; $i < @$t; $i += 2) {
        push @tx, split /\s+/, $t->[$i+1];
    }
    $t = $self->{tables}{na1};
    for (my $i = 0; $i < @$t; $i += 2) {
        push @tx, split /\s+/, $t->[$i+1];
    }
    $t = $self->{named_seq};
    for (my $i = 0; $i < @$t; $i += 2) {
        push @tx, split /\s+/, $t->[$i];
    }
    $t = $self->{p_named_seq};
    for (my $i = 0; $i < @$t; $i += 2) {
        push @tx, split /\s+/, $t->[$i];
    }

    for (@tx) {
        $toks{$_}++;
    }
    my @tinfo = map { sprintf "%08d%s", 1e8 - $toks{$_}, $_ } keys %toks;
    @tinfo = sort @tinfo;
    print STDERR "(", scalar(keys(%toks)), " distinct tokens) ";
    #write_file("keys", join "\n", @tinfo, "");

    my %tmap;
    my $i = 1;
    my $buf = '';
    for (@tinfo) {
        $tmap{substr($_,8)} = $i++;
        $buf .= pack "Z*", substr($_,8);
    }

    $self->{tok_map} = \%tmap;
    $self->_add_file('!name_tokens', 'T', $buf);
}

sub format_table {
    my ($self, $name, $type) = @_;
    print STDERR "Formatting table $name ($type) ... ";

    my $t = $self->{tables}{$name};
    my $stat = $self->{stats}{$name};
    my @buf;

    #write_file("raw.$name", join "\n", @$t, "");

    if ($name eq 'na' || $name eq 'na1') {
        @buf = ('N', '');
        my $prev = -1;
        my @last;
        for (my $i = 0; $i < @$t; $i += 2) {
            my @tx = split /\s+/, $t->[$i+1];

            my $keep = 0;
            while ($keep < @tx && $keep < @last && $last[$keep] eq $tx[$keep]) {
                $keep++;
            }

            if ($t->[$i] != $prev + 1) {
                $buf[1] .= "\0" . pack "w", $t->[$i] - $prev - 1;
            }
            $prev = $t->[$i];

            $buf[1] .= chr(16 * $keep + @tx);

            for (my $j = $keep; $j < @tx; $j++) {
                $buf[1] .= pack "w", $self->{tok_map}{$tx[$j]};
            }
            @last = @tx;
        }
    }
    elsif ($type eq 'String') {
        @buf = ('S', '', '');
        my @tix = @$t;

        for (my $i = 1; $i < @tix; $i += 2) {
            if ($tix[$i] eq '#') {
                $tix[$i] = "\xD8\x01"; # placeholder
            } elsif ($tix[$i] eq '') {
                $tix[$i] = "\xD8\x00"; # useful to treat 0 chars as 1 here
            } else {
                $tix[$i] = join "", map { chr(hex($_)) } split ' ', $tix[$i];
                $tix[$i] = encode("utf16be",$tix[$i]);
            }
        }

        while (@tix) {
            if (@tix >= 6 && $tix[2] == $tix[0]+1 && $tix[4] == $tix[2]+1 &&
                    length($tix[1]) == length($tix[3]) &&
                    length($tix[3]) == length($tix[5])) {
                # 3+ characters, in sequence, all with equal length values
                my $next = $tix[0];
                my $leng = length($tix[1]);
                $buf[1] .= pack "N", ($next | ($leng << 23));

                while (@tix >= 4 && $tix[0] == $next && $tix[2] == $next+1 &&
                        length($tix[1]) == $leng) {
                    $next++;
                    $buf[2] .= $tix[1];
                    shift @tix;
                    shift @tix;
                }
            }
            else {
                $buf[1] .= pack "N", ($tix[0] | ((length($tix[1])+1) << 23)),
                $buf[2] .= $tix[1];
                shift @tix;
                shift @tix;
            }
        }

        my %look;
        for (my $i = 0; $i < length($buf[2]); $i += 2) {
            $look{substr($buf[2],$i,2)}++;
        }

        print STDERR "(", (length($buf[2])/2), " units ", scalar(keys %look), " distinct) ";
    }
    elsif ($type eq 'Binary') {
        # special optimization: values are 0,1,0,1,0,1... and need not be saved
        @buf = ('B', '');

        my $last = 'N';
        my $lix = 0;
        for (my $i = 0; $i < @$t; $i += 2) {
            next if $t->[$i+1] eq $last;
            $last = $t->[$i+1];
            $buf[1] .= pack "w", $t->[$i] - $lix;
            $lix = $t->[$i];
        }
    }
    elsif (keys(%$stat) <= 256) {
        @buf = ('E', '', '', '');
        my %cat;
        print STDERR "( ", scalar(keys(%$stat)), " values) ";

        my $lix = 0;
        for (my $i = 0; $i < @$t; $i += 2) {
            my $ix = $cat{$t->[$i+1]};
            unless (defined($ix)) {
                $ix = keys %cat;
                $cat{$t->[$i+1]} = $ix;
                $buf[3] .= pack "Z*", $t->[$i+1];
            }
            $buf[2] .= pack "C", $ix;
            $buf[1] .= pack "w", $t->[$i] - $lix;
            $lix = $t->[$i];
        }
    } else {
        @buf = ('M','');
        for (my $i = 0; $i < @$t; $i += 2) {
            $buf[1] .= pack "NZ*", $t->[$i], $t->[$i+1];
        }
    }

    $self->_add_file($name, @buf);
}

sub end_document {
    my ($self, $e) = @_;
    print STDERR "\n";

    $self->collect_tokens;

    my $cooked_pa = '';
    my $type = '';
    for my $line (@PropertyAliases) {
        if ($line =~ /^\w/) {
            my @arr = split /;/, $line;
            s/\s//g for @arr;
            next if $arr[0] =~ /cjk/;
            next if $arr[0] eq 'Name_Alias'; # XXX not in the XML data
            $self->format_table($arr[0], $type);

            $cooked_pa .= pack "Z*", $_ for @arr;
            $cooked_pa .= "\0";
        }
        elsif ($line =~ /(\w+) Properties/) {
            $type = $1;
        }
    }

    for my $t (qw/ named_seq p_named_seq /) {
        my $lst = $self->{$t};
        my $buf = '';
        for (my $i = 0; $i < @$lst; $i += 2) {
            for my $tk (split /\s+/, $lst->[$i]) {
                $buf .= pack "w", $self->{tok_map}{$tk};
            }
            $buf .= "\0";
            for my $cp (split /\s+/, $lst->[$i+1]) {
                $buf .= encode("utf16be", chr(hex($cp)));
            }
            $buf .= "\0\0";
        }
        $self->_add_file("!$t", '_', $buf);
    }

    $self->_add_file('!normalization-corrections', '_', $self->{normalization_correction});
    $self->_add_file('!standardized-variants', '_', $self->{standardized_variant});
    $self->_add_file('!cjk-radicals', '_', $self->{cjk_radical});
    $self->_add_file('!emoji-sources', '_', $self->{emoji_source});
    $self->_add_file('!PropertyAlias', '_', $cooked_pa);

    print $self->{'header'};
    print $_ for @{ $self->{'files'} };
}

sub _mong_attrs {
    my ($into, $attrs) = @_;
    for my $v (values %$attrs) {
        $into->{$v->{LocalName}} = $v->{Value};
    }
    $into;
}

sub start_element {
    my ($self,$e) = @_;
    my $n = $e->{LocalName};

    if ($n eq "group") {
        $self->{group} = _mong_attrs({}, $e->{Attributes});
    }
    elsif ($n eq "char" || $n eq "reserved" || $n eq "noncharacter" || $n eq "surrogate") {
        my $info = _mong_attrs({ %{ $self->{group} } }, $e->{Attributes});

        ## track first/last, update progress
        my $obl = int(($self->{'last'}+1) * 1000 / 1114112);

        my $first = hex($info->{'first-cp'} // $info->{'cp'});
        if ($first != $self->{'last'} + 1) {
            die "Out of order, " . $info->{'first-cp'} // $info->{'cp'};
        }
        my $last = $self->{'last'} = hex($info->{'last-cp'} // $info->{'cp'});

        my $nbl = int(($self->{'last'}+1) * 1000 / 1114112);
        if ($nbl != $obl) {
            my $str = sprintf "%5.1f%%  U+%04X %s", $nbl/10, $last, $info->{na};
            printf STDERR "%s%-50.50s", "\b" x 50, $str;
        }

        delete $info->{'last-cp'};
        delete $info->{'cp'};
        delete $info->{'first-cp'};

        for my $k (keys %$info) {
            $self->{stats}{$k}{$info->{$k}} += $last - $first + 1
                unless $k eq 'na' || $k eq 'na1';

            my $t = ($self->{tables}{$k} //= []);
            if (!@$t || $t->[-1] ne $info->{$k}) {
                push @$t, $first, $info->{$k};
            }
        }
    }
    elsif ($n eq 'block') {
        my $t = ($self->{tables}{blk} //= []);
        my $a = _mong_attrs({}, $e->{Attributes});

        if (@$t && $t->[-2] == hex($a->{'first-cp'})) {
            pop @$t; pop @$t;
        }
        $self->{stats}{blk}{$a->{name}}++;
        push @$t, hex($a->{'first-cp'}), $a->{'name'},
            hex($a->{'last-cp'})+1, '';
    }
    elsif ($n eq 'named-sequence') {
        my $a = _mong_attrs({}, $e->{Attributes});
        push @{ $self->{inprov} ? $self->{p_named_seq} : $self->{named_seq} },
            $a->{name}, $a->{cps};
    }
    elsif ($n eq 'provisional-named-sequences') {
        $self->{inprov} = 1;
    }
    elsif ($n eq 'normalization-correction') {
        my $a = _mong_attrs({}, $e->{Attributes});
        $self->{normalization_correction} .= pack "NNNZ*",
            hex($a->{cp}), hex($a->{old}), hex($a->{new}), $a->{version};
    }
    elsif ($n eq 'standardized-variant') {
        my $a = _mong_attrs({}, $e->{Attributes});
        my @cps = split / /, $a->{cps};
        $self->{standardized_variant} .= pack "NNZ*Z*",
            hex($cps[0]), hex($cps[1]), $a->{desc}, $a->{when};
    }
    elsif ($n eq 'cjk-radical') {
        my $a = _mong_attrs({}, $e->{Attributes});
        $a->{number} =~ /(\d+)('?)/;
        $self->{cjk_radical} .= pack "nnn", ($2 ? 1024 : 0) + $1,
            hex($a->{radical}), hex($a->{ideograph});
    }
    elsif ($n eq 'emoji-source') {
        my $a = _mong_attrs({}, $e->{Attributes});
        $self->{emoji_source} .= pack "nnn", hex($a->{docomo}),
            hex($a->{kddi}), hex($a->{softbank});
        my $uni = encode("utf16be", join "", map { chr(hex($_)) } split / /,
            $a->{unicode});
        $self->{emoji_source} .= $uni . "\0\0";
    }
    elsif ($n eq 'description') {
        $self->{indesc} = 1;
    }
}

sub end_element {
    my ($self,$e) = @_;
    my $n = $e->{LocalName};

    if ($n eq "group") {
        $self->{group} = {};
    } elsif ($n eq "description") {
        $self->{indesc} = 0;
    }
}

sub characters {
    my ($self,$e) = @_;

    if ($self->{indesc}) {
        $self->{description} .= $e->{Data};
    }
}

package main;

use XML::SAX;

XML::SAX::ParserFactory->parser(Handler => InputHandler->new)
    ->parse_uri($UCD_XML);
