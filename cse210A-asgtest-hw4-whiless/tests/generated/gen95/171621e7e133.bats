load ../../harness

@test "171621e7e133" {
  check 'if true    then y :=     -63537238463610426631238105595387381176*y else 
y    :=    -83064678729884429148942919546572234364-   105469545916284900461159744808707793519' '⇒ y := (-63537238463610426631238105595387381176*y), {}
⇒ skip, {y → 0}'
}