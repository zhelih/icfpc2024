let v6 v5 =
  let rec v3 v4 = if (v4 = v5 / 2 + 1) then true else if ((v5 mod v4) = 0) then false else (v3 (v4+1)) in v3 2
in
(*
let v7 self v4 = if (v4 = 1) then true else if ((v4 mod 2) = 1) then false else (self (v4/2)) in
let v7 = Memo.memoize v7 in
let rec v3 v4 = (*print_int v4; print_newline ();*) if ((v4 > 1_000_000) && (v7 (v4+1) && (v6 v4))) then v4 else (v3 (v4+1)) in
print_int (v3 1_000_000)
*)
let rec loop n =
  if n > 1_000_000 && v6 (n-1) then n else loop (n*2)
in
print_int @@ loop 2 - 1
