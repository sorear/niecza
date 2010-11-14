.sub '' :main
    $P0 = box 10000000
    print $P0
    say " iterations"

    $P1 = box 0
    goto check
  again:
    $P1 = $P1 + 1
  check:
    $I0 = $P1 == $P0
    $P2 = box $I0
    unless $P2 goto again
.end
