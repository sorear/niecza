.sub 'main' :main
    $P0 = new 'ResizablePMCArray'
    $I0 = 0
  l1:
    push $P0, $I0
    inc $I0
    unless $I0 == 10000 goto l1

    $P1 = iter $P0
  l2:
    $P2 = shift $P1
    $P3 = iter $P0
  l3:
    $P4 = shift $P3
    if $P3 goto l3
    if $P1 goto l2

    .return()
.end
