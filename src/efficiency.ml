open Devkit
open Lang

let tee f x = f x; x

let rec cbv = function
| B (op,a,b) -> B ((if op = Apply then ApplyCBV else op),cbv a,cbv b)
| S _ | Bool _ | I _ | V _ as x -> x
| L (v,t) -> L (v,cbv t)
| Neg x -> Neg (cbv x)
| Not x -> Not (cbv x)
| Int_of_string x -> Int_of_string (cbv x)
| String_of_int x -> String_of_int (cbv x)
| If (c,a,b) -> If (cbv c, cbv a, cbv b)

let fib n =
  let a = Array.make (n+1) 1 in
  for i = 2 to n do a.(i) <- a.(i-1) + a.(i-2) done;
  a.(n)

let solve file =
  let t = Std.input_file file |> decode in
  let r =
    let int n = I (Z.of_int n) in
    eval @@ match file with
    | "task/efficiency1" -> cbv t
    | "task/efficiency3" -> int @@ 9345873499+2134+1
    | "task/efficiency4" -> int @@ fib 40
    | _ -> t
  in
  match r with
  | I n -> Z.to_string n |> tee print_endline
  | x -> Exn.fail "expected int, got %s" (print x)
