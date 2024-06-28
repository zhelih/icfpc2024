open Printf
open ExtLib
open Devkit

let token = "aba831d7-66ed-41d3-b757-fc719f5ad979"

let raw_comm data =
  match Web.http_request
    ~ua:"dysfunctional|piglets"
    ~timeout:5
    ~verbose:false
    ~headers:["Authorization: Bearer " ^ token]
    ~body:(`Raw ("application/icfp",data))
    `POST
    "https://boundvariable.space/communicate"
  with
  | `Ok s -> s
  | `Error s -> Exn.fail "comm %S failed: %s" data s

let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
let alphabet_inv = String.init 256 (fun i -> match String.index alphabet (Char.chr i) with exception _ -> '\x00' | p -> Char.chr (p+33))
let () = assert (String.length alphabet = 94)

type t = (* S of *) string

let decode s =
  assert (s <> "");
  match s.[0] with
  | 'S' -> String.map (fun c -> alphabet.[Char.code c - 33]) @@ String.slice ~first:1 s
  | c -> Exn.fail "unsupported indicator %c" c

let encode = function
  | s -> "S" ^ String.map (fun c -> alphabet_inv.[Char.code c]) s

let comm input =
  eprintfn "> %S" input;
  decode @@ raw_comm @@ encode input

let () =
  assert ("SB%,,/}Q/2,$_" = encode "Hello World!");
  assert ("get index" = decode "S'%4}).$%8");
  match Nix.args with
  | [] ->
    print_endline @@ comm "get scoreboard";
    print_endline @@ comm "get index"
  | "get"::x::[] ->
    print_endline @@ comm @@ sprintf "get %s" x;
  | _ ->
    Exn.fail "wut"
