open Printf
open ExtLib
open Devkit

type d = L | R | U | D
let next (x,y) d = match d with L -> x-1, y | R -> x+1, y | U -> x,y+1 | D -> x,y-1
let neighbours p = List.map (fun d -> d, next p d) [L;R;U;D]

let get grid (x,y) = grid.(x+1).(y+1)
let set grid (x,y) v = grid.(x+1).(y+1) <- v

let tuck l x = l := x :: !l
let tee f x = f x; x
let pee _ x = x

type cell = Wall | Pill | Reach of int

let show_dir = function D -> 'D' | R -> 'R' | L -> 'L' | U -> 'U'
let inverse = function U -> D | D -> U | R -> L | L -> R
let show_pos (x,y) = sprintf "(%d,%d)" x y
let show_cell = function Wall -> "Wall" | Pill -> "Pill" | Reach n -> sprintf "Reach %d" n
let show grid pos = sprintf "%s %s" (show_pos pos) (show_cell @@ get grid pos)

module T = Set.Make(struct type t = (int*int) * d list let compare (a,_pa) (b,_pb) = compare a b end)

let rec flood grid targets step front =
  match front with
  | [] -> ()
  | _ ->
  front |> List.map (fun (f,path) ->
    let neighs = neighbours f |> List.filter (fun (_, p) -> get grid p = Pill) |> List.map (fun (d,p) -> p, d::path) in
    if neighs = [] then targets := T.add (f,List.rev path) !targets;
    neighs |> List.iter (fun (p,_) -> set grid p (Reach step));
(*     neighs |> List.map fst |> List.map (show grid) |> List.iter (printfn "step %d neigh %s" step); *)
    neighs)
  |> List.concat |> flood grid targets (step+1)

let flood grid start =
  let targets = ref T.empty in
  set grid start (Reach 0);
  flood grid targets 1 [start,[]];
  !targets

let grid_of_string s =
  let l = String.split_on_char '\n' s in
  assert (l <> []);
  let dimx = String.length @@ List.hd l in
  let dimy = List.length l in
  l |> List.iter (fun s -> assert (String.length s = dimx));
  let grid = Array.make_matrix (dimx+2) (dimy+2) Wall in
  let start = ref (0,0) in
  l |> List.iteri (fun y s ->
    let y = dimy-1-y in
    s |> String.iteri (fun x c -> if c = 'L' then start := (x,y); if c = '.' || c = 'L' then set grid (x,y) Pill));
  !start, grid

let invert = List.rev_map inverse
let tuck_or_cancel l x = l := match !l with [] -> [x] | h::t when h = inverse x -> t | _ -> x :: !l

(* TODO delete visited targets *)
let solve p grid =
  let targets = flood grid p |> pee (T.iter (fun (p,path) -> printfn "target %s path %d" (show grid p) (List.length path))) |> ref in
  let fullpath = ref [] in
  let _cur = ref p in
  while not @@ T.is_empty !targets do
    let (_,path) as target = T.choose !targets in
(*
      let a = Array.of_list @@ T.elements !targets in
      a.(Random.int (Array.length a))
    in
*)
    targets := T.remove target !targets;
    (* cancel inverse faster but doesnt' matter *)
(*     let realpath = path |> List.dropwhile (fun x -> match !fullpath with h::t when h = inverse x -> fullpath := t; true) in *)
    path |> List.iter (tuck_or_cancel fullpath);
(*     path |> List.iter (fun x -> cur := next !cur x; targets := T.remove (!cur,[]) !targets); *)
    if not @@ T.is_empty !targets then invert path |> List.iter (tuck fullpath);
  done;
  List.rev_map (show_dir) !fullpath |> List.to_seq |> String.of_seq

let solve file =
  Random.self_init ();
  let (start,grid) = grid_of_string @@ String.trim @@ Std.input_file file in
  solve start grid

