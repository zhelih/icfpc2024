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
let unshow_dir = function 'D' -> D | 'R' -> R | 'L' -> L | 'U' -> U | _ -> assert false
let inverse = function U -> D | D -> U | R -> L | L -> R
let show_pos (x,y) = sprintf "(%d,%d)" x y
let show_cell = function Wall -> "Wall" | Pill -> "Pill" | Reach n -> sprintf "Reach %d" n
let show grid pos = sprintf "%s %s" (show_pos pos) (show_cell @@ get grid pos)

let rec flood grid targets step front =
  match front with
  | [] -> !targets
  | _ ->
  front |> List.map (fun (f,path) ->
    let neighs = neighbours f |> List.filter (fun (_, p) -> get grid p = Pill) |> List.map (fun (d,p) -> p, d::path) in
    if neighs = [] then tuck targets (f,List.rev path);
    neighs |> List.iter (fun (p,_) -> set grid p (Reach step));
(*     neighs |> List.map fst |> List.map (show grid) |> List.iter (printfn "step %d neigh %s" step); *)
    neighs)
  |> List.concat |> flood grid targets (step+1)

let flood grid start =
  set grid start (Reach 0);
  flood grid (ref []) 1 [start,[]]

let grid_of_string file s =
  let l = String.split_on_char '\n' s in
  assert (l <> []);
  let dimx = String.length @@ List.hd l in
  let dimy = List.length l in
  l |> List.iter (fun s -> assert (String.length s = dimx));
  let grid = Array.make_matrix (dimx+2) (dimy+2) Wall in
  let start = ref (0,0) in
  let inverted = file = "task/lambdaman21" in
  l |> List.iteri (fun y s ->
    let y = if inverted then y else dimy-1-y in
    s |> String.iteri (fun x c -> let x = if inverted then dimx -1 - x else x in if c = 'L' then start := (x,y); if c = '.' || c = 'L' then set grid (x,y) Pill));
  !start, grid

let invert = List.rev_map inverse
let tuck_or_cancel l x = l := match !l with [] -> [x] | h::t when h = inverse x -> t | _ -> x :: !l

let solve p grid =
  let paths = ref [] in
  flood grid p
  |> pee (List.iter (fun (p,path) -> printfn "target %s path %d" (show grid p) (List.length path)))
  |> List.iter (fun (_,path) -> tuck paths (String.of_seq @@ Seq.map show_dir @@ List.to_seq path));
  let fullpath = ref [] in
  let prev_full = ref [] in
  List.sort ~cmp:String.compare !paths
  |> List.iter begin fun path ->
    let path = String.to_seq path |> Seq.map unshow_dir |> List.of_seq in
    path |> List.iter (tuck_or_cancel fullpath);
    prev_full := !fullpath;
    invert path |> List.iter (tuck fullpath)
  end;
  List.rev_map (show_dir) !prev_full |> List.to_seq |> String.of_seq

let solve file =
  Random.self_init ();
  let (start,grid) = grid_of_string file @@ String.trim @@ Std.input_file file in
  solve start grid
