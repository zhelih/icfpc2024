open Printf
open ExtLib
open Devkit

open Lang

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

let comm input =
  eprintfn "> %S" (if String.length input > 100 then String.slice input ~last:100 ^ "..." else input);
  decode @@ raw_comm @@ encode (S input)

let is_better ~task submission =
  let score = String.length submission in
  match Std.input_file (task ^ ".submission") with
  | exception _ ->
    eprintfn "%s: new submission %d" task score;
    true
  | previous ->
    let previous = String.length previous in
    if score < previous then
    begin
      eprintfn "%s: our submission %d is better than previous submission %d" task score previous;
      true
    end
    else
    begin
      eprintfn "%s: existing submission %d is not worse than current submission %d" task previous score;
      false
    end

let pick_solver = function
| s when String.starts_with s "task/lambdaman" -> Lambdaman.solve
| s when String.starts_with s "task/spaceship" -> Spaceship.solve
| s -> Exn.fail "idk how to solve %s" s

let solve submit task =
  let solver = pick_solver task in
  let solution = solver task in
  (* TODO encode smarter *)
  let submission = encode @@ S (sprintf "solve %s %s" (Filename.basename task) solution) in
  match submit with
  | false ->
    let (_:bool) = is_better ~task submission in
    print_endline solution
  | true ->
    if is_better ~task submission then
    begin
      eprintfn "> solve %s ..." (Filename.basename task);
      let answer = expect_string @@ decode @@ raw_comm submission in
      print_endline answer;
      if String.starts_with answer "Correct" then Std.output_file ~filename:(task ^ ".submission") ~text:submission
    end

let pretty pp t =
  let buf = Buffer.create 10 in
  let fmt = Format.formatter_of_buffer buf in
  Format.pp_set_margin fmt 240;
  pp fmt t;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let () =
  assert (encode @@ S "Hello World!" = "SB%,,/}Q/2,$_");
  assert (S "get index" = decode "S'%4}).$%8");
  assert (I 1337 = decode "I/6");
  assert (encode @@ I 1337 = "I/6");
  Printexc.register_printer (function Failure s -> Some s | _ -> None);
  match Nix.args with
  | [] ->
    print_endline @@ expect_string @@ comm "get scoreboard";
    print_endline @@ expect_string @@ comm "get index"
  | "encode"::[] -> Std.input_all stdin |> String.trim |> (fun s -> S s) |> encode |> print_string
  | "decode"::[] -> Std.input_all stdin |> String.trim |> decode |> pretty Lang.pp |> print_string
  | "eval"::[] -> Std.input_all stdin |> String.trim |> decode |> eval |> pretty Lang.pp |> print_string
  | "raw"::s::[] -> print_endline @@ raw_comm @@ encode (S s)
  | "ast"::s::[] -> print_endline @@ pretty Lang.pp @@ decode @@ raw_comm @@ encode (S s)
  | "get"::x::[] -> print_endline @@ expect_string @@ eval @@ comm @@ sprintf "get %s" x;
  | "solve"::task::[] -> solve true task
  | "try"::task::[] -> solve false task
  | _ ->
    Exn.fail "wut"
