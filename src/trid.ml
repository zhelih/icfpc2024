open Printf
open Devkit

exception Finish of int
exception Crash of string
exception Terminated

type dir = U | D | L | R
type binop = Add | Sub | Mul | Div | Rem
type cmpop = EQ | NEQ

type v =
| Empty
| Num of int
| Shift of dir
| Bin of binop
| Cmp of cmpop
| Warp
| Submit

let show = function
| Empty -> "."
| Num i -> string_of_int i
| Shift U -> "^"
| Shift D -> "v"
| Shift R -> ">"
| Shift L -> "<"
| Warp -> "@"
| Submit -> "S"
| Bin Add -> "+"
| Bin Mul -> "*"
| Bin Sub -> "-"
| Bin Div -> "/"
| Bin Rem -> "%"
 | Cmp EQ -> "="
 | Cmp NEQ -> "#"

let show_pos (x,y) = sprintf "(%d,%d)" x y

let binop f a b =
  match f with
  | Add -> a+b
  | Sub -> a-b
  | Div -> a/b
  | Mul -> a*b
  | Rem -> a mod b

let cmp f (a:v) b = match f with EQ -> a = b | NEQ -> a <> b

let opposite = function U -> D | D -> U | L -> R | R -> L
let (++) (x,y) dir = match dir with U -> (x,y+1) | D -> (x,y-1) | L -> (x-1,y) | R -> (x+1,y)

module Grid : sig
type t
val create : unit -> t
val copy : t -> t
val get : t -> (int*int) -> v
val set : t -> (int*int) -> v -> unit
val seq : t -> ((int*int) * v) Seq.t
val print : t -> unit
val show : t -> string
val box : t -> (int*int) * (int*int)
end = struct
type t = (int*int, v) Hashtbl.t
let create () = Hashtbl.create 10
let copy = Hashtbl.copy
let get t p = Option.default Empty @@ Hashtbl.find_opt t p
let set t p v = if v = Empty then Hashtbl.remove t p else Hashtbl.replace t p v
let seq t = Hashtbl.to_seq t
let box t =
  seq t |> Seq.fold_left (fun ((x0,y0),(x1,y1)) ((x,y),_) -> ((min x0  x, min y0 y), (max x1 x, max y1 y))) ((max_int,max_int),(min_int,min_int))
let show t =
  let ((x0,y0),(x1,y1)) = box t in
  let b = Buffer.create 10 in
  for y = y1 downto y0 do
    for x = x0 to x1 do
      Buffer.add_string b (show @@ get t (x,y));
      Buffer.add_char b ' '
    done;
    Buffer.add_char b '\n'
  done;
  Buffer.contents b
let print t = print_string @@ show t
end

let terminate (_:Grid.t) = raise Terminated

let tick ?(trace=false) t =
  if trace then (Grid.print t; print_endline "");
  let next = Grid.copy t in
  let write = Grid.create () in
  Grid.seq t |> Seq.filter_map begin fun (p,v) ->
    match v with
      | Empty -> assert false
      | Num _ -> None
      | Shift dir ->
        let opp = p ++ opposite dir in
        begin match Grid.get t opp with Empty -> None | r -> Some ([opp], [p++dir, r]) end
      | Bin f ->
        begin match Grid.get t (p ++ L), Grid.get t (p ++ U) with
        | Num a, Num b -> let r = Num (binop f a b) in Some ([p++L;p++U], [p++D,r; p++R,r])
        | _ -> None
        end
      | Cmp f ->
        begin match Grid.get t (p ++ L), Grid.get t (p ++ U) with
        | Empty, _ | _, Empty -> None
        | a, b -> if cmp f a b then Some ([p++L;p++U], [p++D,a; p++R,b]) else None
        end
      | Warp -> assert false
      | Submit -> None
    end
    |> Seq.iter begin fun (r,w) ->
      r |> List.iter (fun p -> Grid.set next p Empty);
      w |> List.iter begin fun (p,v) ->
        assert (v <> Empty);
        match Grid.get next p, Grid.get write p with
        | _, Empty -> Grid.set write p v
        | Submit, other when other = v -> Grid.set write p v
        | cur, other -> raise @@ Crash (sprintf "wrong write to %s (%s) : %s %s" (show_pos p) (show cur) (show v) (show other))
     end
    end;
  write |> Grid.seq |> Seq.iter begin fun (p,v) ->
    match Grid.get next p, v with
    | Submit, Num n -> raise @@ Finish n
    | Submit, _ -> raise @@ Crash "can only submit numbers"
    | _ -> Grid.set next p v
  end;
  next

let parse ?(a=7) ?(b=4) lines =
  let start = Grid.create () in
  List.rev lines
  |> List.iteri begin fun y line ->
    String.split_on_char ' ' line |> List.filter ((<>) "")
    |> List.iteri begin fun x token ->
      let v = match token with
      | "." -> Empty
      | ">" -> Shift R | "v" -> Shift D | "<" -> Shift L | "^" -> Shift U
      | "*" -> Bin Mul | "+" -> Bin Add | "-" -> Bin Sub | "/" -> Bin Div | "%" -> Bin Rem
      | "=" -> Cmp EQ | "#" -> Cmp NEQ
      | "S" -> Submit | "A" -> Num a | "B" -> Num b
      | "@" -> Warp
      | _ -> Num (int_of_string token)
      in
      Grid.set start (x,y) v
    end
  end;
  start

let run ?trace ?a ?b lines =
  try
    Seq.ints 1_000_000 |> Seq.fold_left (fun g _ -> tick ?trace g) (parse ?a ?b lines) |> terminate
  with Finish n -> printfn "Result: %d" n

let run_file ?trace ?a ?b file = run ?trace ?a ?b @@ Action.file_lines_exn file

let () =
  let test input expect =
    let r = Grid.show @@ tick @@ parse input in
    let e = Grid.show @@ parse expect in
    if r <> e then Exn.fail "Expected\n%sbut got\n%s" e r
  in
  let fail input =
    try terminate @@ tick @@ parse input with Crash _ -> ()
  in
  fail ["3 > . < 3"];
  test [". < 3 > ."] ["3 < . > 3"];
  test [". < 3 "] ["3 <"];
  test [". 3 ."; "4 + ."] ["+ 7";"7 ."];
  ()
