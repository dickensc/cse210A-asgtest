load ../../harness

@test "9ca9fe88c34c" {
  check 'x    :=  44543101257415702029258282385977804173   +    z ;

 y:=     x   +  z     ' '⇒ skip; y := (x+z), {x → 44543101257415702029258282385977804173}
⇒ y := (x+z), {x → 44543101257415702029258282385977804173}
⇒ skip, {x → 44543101257415702029258282385977804173, y → 44543101257415702029258282385977804173}'
}