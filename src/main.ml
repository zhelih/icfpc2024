open Printf
open ExtLib
open Devkit

let token = "aba831d7-66ed-41d3-b757-fc719f5ad979"

let raw_comm data =
  match Web.http_request
    ~ua:"**dysfunctional** | **piglets**"
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

(* type t = S of string *)

let decode s =
  assert (s <> "");
  match s.[0] with
  | 'S' -> String.map (fun c -> alphabet.[Char.code c - 33]) @@ String.slice ~first:1 s
  | c -> Exn.fail "unsupported indicator %c" c

let encode = function
  | s -> "S" ^ String.map (fun c -> alphabet_inv.[Char.code c]) s

let comm input =
  eprintfn "> %S" (if String.length input > 100 then String.slice input ~last:100 ^ "..." else input);
  decode @@ raw_comm @@ encode input

let is_better ~task solution =
  match Std.input_file (task ^ ".solution") with
  | exception _ ->
    eprintfn "%s: new solution %d" task (String.length solution);
    true
  | previous ->
    if String.length solution < String.length previous then (* encoding doesn't matter (yet?) *)
    begin
      eprintfn "%s: our solution %d is better than previous solution %d" task (String.length solution) (String.length previous);
      true
    end
    else
    begin
      eprintfn "%s: existing solution %d is not worse than current solution %d" task (String.length previous) (String.length solution);
      false
    end

let pick_solver = function
| s when String.starts_with s "task/lambdaman" -> Lambdaman.solve
| s -> Exn.fail "idk how to solve %s" s

let solve submit task =
  let solver = pick_solver task in
  let solution = solver task in
  match submit with
  | false ->
    let (_:bool) = is_better ~task solution in
    print_endline solution
  | true ->
    if is_better ~task solution then
    begin
      let answer = comm @@ sprintf "solve %s %s" (Filename.basename task) solution in
      print_endline answer;
      if String.starts_with answer "Correct" then Std.output_file ~filename:(task ^ ".solution") ~text:solution
    end

let () =
  assert ("SB%,,/}Q/2,$_" = encode "Hello World!");
  assert ("get index" = decode "S'%4}).$%8");
  match Nix.args with
  | [] ->
    print_endline @@ comm "get scoreboard";
    print_endline @@ comm "get index"
  | "raw"::s::[] ->
    print_endline @@ raw_comm @@ encode s
  | "get"::x::[] ->
    print_endline @@ comm @@ sprintf "get %s" x;
  | "solve"::task::[] -> solve true task
  | "try"::task::[] -> solve false task
(*   | "solve"::task::[] when String.starts_with task "task/spaceship" -> Spaceship.solve task *)
  | _ ->
    Exn.fail "wut"
