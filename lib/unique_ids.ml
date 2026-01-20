let counter = ref 0

let make_temporary () =
  let n = !counter in
  counter := n + 1 ;
  "tmp." ^ string_of_int n
