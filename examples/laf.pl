my @fdie = 'rabbit' xx 6, 'sheep' xx 2, 'pig' xx 2, 'horse', 'fox';
my @wdie = 'rabbit' xx 6, 'sheep' xx 3, 'pig', 'cow', 'wolf';

my %players =
    stock => { rabbit => 60, sheep => 24, pig => 20, cow => 12,
        horse => 6, 'small dog' => 4, 'big dog' => 2 };

my %value = rabbit => 1, sheep => 6, pig => 12, cow => 36, horse => 72,
    'small dog' => 6, 'big dog' => 36;

sub check_win($p) {
    if %players{$p}{all <rabbit sheep pig cow horse>} {
        say "$p won.";
        exit;
    }
}

sub transfer($from, $to, *@pairs) {
    my %k = @pairs;
    %players{$from}{.key} -= .value for %k;
    %players{$to}{.key}   += .value for %k;
    check_win($to) unless $to eq 'stock';
}

sub eat($p, $protector, *@victims) {
    if %players{$p}{$protector} {
        transfer($p, 'stock', $protector => 1);
    } else {
        transfer($p, 'stock', $_ => %players{$p}{$_}) for @victims;
    }
}

my token word { <.alpha>+ }
my regex offer {:s [ (\d+) (<&word> ** <.ws>) ] ** ',' }

sub scan_offer($p, $/) {
    my $value = 0;
    my %basket;
    for @($0) Z @($1) -> $num, $item is copy {
        $item = $item.words.join(" ");
        if !defined %value{$item} {
            say "No such animal $item";
            goto "again";
        }
        $value += $num * %value{$item};
        %basket{$item} += $num;
    }
    if $p eq 'stock' {
        %basket{$_} min= %players{$p}{$_} for %basket.keys;
    }
    elsif first({ %players{$p}{.key} < .value }, %basket) -> $e {
        say "$p does not have have $e.value() of $e.key()";
        goto "again";
    }
    $value, $(%basket);
}

sub trade($p, $with, $give, $get) {
    if !(%players{$with}:exists) {
        say "No such player $with";
        goto "again";
    }
    my ($vgive,$bgive) = scan_offer($p, $give);
    my ($vget,$bget) = scan_offer($with, $get);
    if $vgive != $vget {
        say "Trade is not balanced.";
        .say for %value;
        goto "again";
    }
    if $with ne 'stock' && prompt("[$with] Accept the trade? ") ne 'yes' {
        goto "again";
    }
    transfer($p, $with, %$bgive);
    transfer($with, $p, %$bget);
}

sub move($p) {
    for %players.kv -> $pn, %h {
        say "[$pn] Inventory: ", join " ",
            map { $^b == 0 ?? () !! $^b > 1 ?? "$^a x$^b" !! $^a }, %h.kv;
    }

again:
    given prompt "[$p] Make what trade if any? " {
        when regex {^ none $} {
        }
        when /:s^ $0=<&offer> for $1=<&offer> with (.*) $/ { #/
            trade($p, $2, $0, $1);
        }
        default {
            say q:to[EOM] ;
Invalid trade syntax.
Valid are 'none', '2 pig, 1 sheep, 6 rabbit for 1 cow with stock'.
EOM
            goto again;
        }
    }
    say "[$p] Wolf roll is: ", my $wolf = @wdie[rand * @wdie];
    say "[$p] Fox  roll is: ", my $fox  = @fdie[rand * @fdie];
    if $fox  eq 'fox'  { eat($p, 'small dog', <rabbit>) }
    if $wolf eq 'wolf' { eat($p, 'big dog', <rabbit sheep pig cow>) }
    my %pairs = %(%players{$p});
    %pairs{$_}++ for $wolf, $fox;

    for %pairs.kv -> $animal, $total {
        my $gain = ($total div 2) min %players<stock>{$animal};
        say "[$p] Adds $gain {$animal}(s)" if $gain;
        transfer('stock', $p, $animal => $gain);
    }
}

my @plr = 1 .. +prompt("How many players? ");
%players{$_} = { } for @plr;
for @plr -> $p { %players{$p}{$_} = 0 for %players<stock>.keys }

loop { say "New round."; move($_) for @plr }
