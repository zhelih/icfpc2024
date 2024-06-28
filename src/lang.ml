open Printf
open Devkit
open ExtLib

let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
let alphabet_inv = String.init 256 (fun i -> match String.index alphabet (Char.chr i) with exception _ -> '\x00' | p -> Char.chr (p+33))
let () = assert (String.length alphabet = 94)

type binop =
| Plus
| Eq
| Concat

type t =
| S of string
| T
| F
| B of binop * t * t
| I of int

let decode_int s = fst @@ String.fold_right (fun c (acc,exp) -> (Char.code c - 33) * exp + acc, exp * 94) s (0,1)

let rec decode next =
  match next () with
  | None | Some "" -> Exn.fail "empty"
  | Some "T" -> T
  | Some "F" -> F
  | Some s ->
  assert (String.length s > 1);
  match s.[0] with
  | 'S' -> S (String.map (fun c -> alphabet.[Char.code c - 33]) @@ String.slice ~first:1 s)
  | 'B' -> assert (String.length s = 2);
    let op =
      match s.[1] with
      | '+' -> Plus
      | '=' -> Eq
      | '.' -> Concat
      | c -> Exn.fail "unsupported binary operator %c" c
    in
    let a = decode next in
    let b = decode next in
    B (op,a,b)
  | 'I' -> I (decode_int @@ String.slice ~first:1 s)
  | c -> Exn.fail "unsupported indicator %c" c

let decode s = String.split_on_char ' ' s |> List.to_seq |> Seq.to_dispenser |> decode

let encode_binop = function
| Plus -> '+'
| Eq -> '='
| Concat -> '.'

let rec encode = function
  | S s -> "S" ^ String.map (fun c -> alphabet_inv.[Char.code c]) s
  | T -> "T"
  | F -> "F"
  | I _ -> Exn.fail "todo"
  | B (op,a,b) ->
    sprintf "B%c %s %s" (encode_binop op) (encode a) (encode b)

let expect_string = function
  | S s -> s
  | _ -> Exn.fail "received non-string answer"

