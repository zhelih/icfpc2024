open Devkit
open Lang

let tee f x = f x; x

let rec cbv = function B (Apply,a,b) -> B (ApplyCBV,cbv a, cbv b) | e -> map cbv e

let fib n =
  let a = Array.make (n+1) 1 in
  for i = 2 to n do a.(i) <- a.(i-1) + a.(i-2) done;
  a.(n)

let solve file =
  let t = Std.input_file file |> decode in
  let int n = I (Z.of_int n) in
  let r =
    match file with
    | "task/efficiency1" -> eval @@ cbv t
    | "task/efficiency2" -> eval t
    | "task/efficiency3" -> int @@ 9345873499+2134+1
    | "task/efficiency4" -> int @@ fib 40
    | "task/efficiency5" -> int @@ E5.solve ()
    | "task/efficiency6" -> int @@ E6.solve ()
    | "task/efficiency13" -> int @@ 2 lsl 13 + 7
    | _ -> eval @@ tee (printfn "print_int %s" $ print) @@ apply_recurse @@ t
  in
  match r with
  | I n -> Z.to_string n |> tee print_endline
  | x -> Exn.fail "expected int, got %s" (print x)
