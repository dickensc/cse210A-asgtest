load ../../harness

@test "966385c9f23e" {
  check 'if (¬(-79335179501071331854288648951877776691    *   -77407288973175608410248045916393018070     =    z+    y))     then y  :=   z  + y      else 
skip    ' '⇒ y := (z+y), {}
⇒ skip, {y → 0}'
}