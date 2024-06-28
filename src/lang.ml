open Printf
open Devkit
open ExtLib

let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
let alphabet_inv = String.init 256 (fun i -> match String.index alphabet (Char.chr i) with exception _ -> '\x00' | p -> Char.chr (p+33))
let () = assert (String.length alphabet = 94)

type binop =
| Plus
| Minus
| Mul
| Div
| Mod
| LT
| GT
| Or
| And
| Eq
| Concat
| Apply
| Drop
| Take
[@@deriving show {with_path=false}]

type t =
| S of string
| T
| F
| B of binop * t * t
| I of int
| V of int
| L of int * t
| Neg of t
| Not of t
| Int_of_string of t
| String_of_int of t
| If of t * t * t
[@@deriving show {with_path=false}]

let decode_int s = fst @@ String.fold_right (fun c (acc,exp) -> (Char.code c - 33) * exp + acc, exp * 94) s (0,1)

let rec decode next =
  match next () with
  | None | Some "" -> Exn.fail "empty"
  | Some s ->
  match s with
  | "T" -> T
  | "F" -> F
  | "?" -> let cond = decode next in let a = decode next in let b = decode next in If (cond,a,b)
  | "U-" -> Neg (decode next)
  | "U!" -> Not (decode next)
  | "U#" -> Int_of_string (decode next)
  | "U$" -> String_of_int (decode next)
  | _ ->
  assert (String.length s > 1);
  let int () = decode_int @@ String.slice ~first:1 s in
  match s.[0] with
  | 'S' -> S (String.map (fun c -> alphabet.[Char.code c - 33]) @@ String.slice ~first:1 s)
  | 'B' -> assert (String.length s = 2);
    let op =
      match s.[1] with
      | '+' -> Plus
      | '-' -> Minus
      | '=' -> Eq
      | '.' -> Concat
      | '$' -> Apply
      | 'D' -> Drop
      | 'T' -> Take
      | '*' -> Mul
      | '/' -> Div
      | '%' -> Mod
      | '<' -> LT
      | '>' -> GT
      | '|' -> Or
      | '&' -> And
      | c -> Exn.fail "unsupported binary operator %c" c
    in
    let a = decode next in
    let b = decode next in
    B (op,a,b)
  | 'L' -> L (int (), decode next)
  | 'v' -> V (int ())
  | 'I' -> I (int ())
  | c -> Exn.fail "unsupported indicator %c" c

let decode s =
  let next = String.split_on_char ' ' s |> List.to_seq |> Seq.to_dispenser in
  let t = decode next in
  assert (next () = None);
  t

let encode_binop = function
| Plus -> '+'
| Minus -> '-'
| Eq -> '='
| Concat -> '.'
| Apply -> '$'
| Take -> 'T'
| Drop -> 'D'
| Mul -> '*'
| Div -> '/'
| Mod -> '%'
| LT ->'<'
| GT -> '>'
| Or -> '|'
| And -> '&'

let rec encode = function
  | S s -> "S" ^ String.map (fun c -> alphabet_inv.[Char.code c]) s
  | T -> "T"
  | F -> "F"
  | If (c,a,b) -> sprintf "? %s %s %s" (encode c) (encode a) (encode b)
  | L _
  | V _
  | I _ -> Exn.fail "todo"
  | Neg x -> sprintf "U- %s" (encode x)
  | Not x -> sprintf "U! %s" (encode x)
  | Int_of_string x -> sprintf "U# %s" (encode x)
  | String_of_int x -> sprintf "U$ %s" (encode x)
  | B (op,a,b) ->
    sprintf "B%c %s %s" (encode_binop op) (encode a) (encode b)

let expect_string = function
  | S s -> s
  | _ -> Exn.fail "received non-string answer"
