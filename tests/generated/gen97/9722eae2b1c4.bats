load ../../harness

@test "9722eae2b1c4" {
  check 'if (x  + y   =    -109444925556282882077328927405450076776  * x   ∧true) then  y    :=x     *   x  else 
  z    := y     -114497345798471956089132312755022019507   ' '⇒ y := (x*x), {}
⇒ skip, {y → 0}'
}