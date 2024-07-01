let memoize f =
  let h = Hashtbl.create 10 in
  print_endline "new memoize";
  let rec g arg =match Hashtbl.find h arg with exception _ -> let r = f g arg in Hashtbl.replace h arg r; r | r -> (* print_endline "got";*) r in
  g
