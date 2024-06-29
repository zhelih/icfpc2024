type t_direction = D | S | U [@@deriving show] (* decrement, stay the same increment (up) *)

let str_of_move = function
| D, U -> "7" | S, U -> "8" | U, U -> "9"
| D, S -> "4" | S, S -> "5" | U, S -> "6"
| D, D -> "1" | S, D -> "2" | U, D -> "3"

let points_of_string s =
  let l = String.split_on_char '\n' s in
  assert (l <> []);
  let make_tuple line =
    let r = String.split_on_char ' ' line in
    match r with
    | hd::tl::[] -> hd, tl
    | _ -> assert false
  in
  List.map make_tuple l

let reorder_points x =
  (* sort by x then y *)
  let arr = Array.of_list x in
  let compare p1 p2 =
    if snd p1 != snd p2 then compare (snd p1) (snd p2) else compare (fst p1) (fst p2)
  in
  Array.sort compare arr;
  Array.to_list arr

(*** EL quick mess to solve something ***)

let solve1d l r v0 k =
  (* solve 1d problem from l to r initial speed v0 and in k steps, monotonic only *)
  let rem0 = r - l - v0*k in
  let is_incr = rem0 >= 0 in
  let rem = abs rem0 in
  if rem0 = 0 then [ List.init k (fun _ -> S) ] else begin (*FIXME This is not the ONLY solution tho *)

  (* compute tight bounds on g for speed *)
  let d = 1 + 8*rem in
  let g_min = Float.to_int @@ Float.ceil ( ( (-1.) -. (Float.sqrt @@ float @@ d)) /. 2. ) in
  let g_max = Float.to_int @@ Float.floor ( ( (-1.) +. (Float.sqrt @@ float @@ d)) /. 2. ) in

  let g_min = max 1 g_min in
  let g_max = min k g_max in

  (* first number at least g_min divisible by g *)
  (*Printf.printf "Optimized g_min to %d and g_max to %d, k = %d\n" g_min g_max k;*)

  let rec loop g sols =
    if g >= g_max+1 then sols else begin
    let rem2 = rem - g*(g+1)/2 in
    let new_sols = if rem2 < 0 || rem2 mod g != 0 then sols else begin (* skip this g *)
      let k1 = k-g-rem2/g in
      if k1 >= 0 then begin
        (* gen solution *)
        let sol_1 = List.init k1 (fun _ -> S) in
        let sol_2 = List.init g (fun _ -> if is_incr then U else D) in
        let sol_3 = List.init (k-k1-g) (fun _ -> S) in
        let sol = List.concat [ sol_1; sol_2; sol_3 ] in
        (* continue the loop *)
        sol::sols
      end else sols
    end in
    if List.length new_sols > 0 then new_sols else
    loop (g+1) new_sols
    end
  in
  loop g_min []
  end

let solve2d_k x0 x1 y0 y1 vx vy k =
  (* solve 2d problem *)
  let res_x = solve1d x0 x1 vx k in
  let res_y = solve1d y0 y1 vy k in
  match res_x, res_y with
  | [], _ | _, [] -> None
  | rx0::_, ry0::_ -> (* take the first res, not optimal in general, no control on the ending speed *)
  let res_str = String.concat "" @@ List.map2 (fun a b -> str_of_move (a,b)) rx0 ry0 in
  let adj v x =
  match x with
  | U -> v+1
  | D -> v-1
  | S -> v
  in
  let new_vx = List.fold_left adj vx rx0 in
  let new_vy = List.fold_left adj vy ry0 in
  Some (res_str, new_vx, new_vy)

let solve2d x0 x1 y0 y1 vx vy =
  let rec loop k =
    match solve2d_k x0 x1 y0 y1 vx vy k with
    | None when k >= 100_000 -> "couldn't solve", 0, 0
    | None -> loop (k+1)
    | Some sol -> sol
  in
  loop 0

let solve2d_dichotomy x0 x1 y0 y1 vx vy =
  let rec find_right_k k =
    match solve2d_k x0 x1 y0 y1 vx vy k with
    | None -> find_right_k (k*10)
    | Some _ -> k
  in
  let right_k = find_right_k 10 in
  (* find smallest k *)
  Printf.printf "Found right k = %d, starting optimizing on the smallest..." right_k;

  let rec loop l r =
    if r-l <= 1 then r else begin
      let mid = (l+r+1) / 2 in (* ceil *)
      match solve2d_k x0 x1 y0 y1 vx vy mid with
      | None -> loop mid r
      | Some _ -> loop l mid
    end
  in
  let min_k = loop 1 right_k in
  Printf.printf "found %d\n" min_k;
  Option.get @@ solve2d_k x0 x1 y0 y1 vx vy min_k (*FIXME reuse result from loop *)

let walk points =
 let rec loop ps x0 y0 vx0 vy0 output iter =
  Printf.printf "Working point %d / %d\n" iter (List.length points);
  match ps with
  | [] -> String.concat "" @@ List.rev output
  | (x,y)::tl ->
    let x = int_of_string x and y = int_of_string y in
    let s, vx, vy = solve2d x0 x y0 y vx0 vy0 in
    loop tl x y vx vy (s :: output) (iter+1)
  in
  loop points 0 0 0 0 [] 0

let solve file =
   let points = reorder_points @@ points_of_string @@ String.trim @@ Std.input_file file in
(*    let problem_id = Scanf.sscanf file "task/spaceship%d" (fun x -> x) in *)
(*    let file = Printf.sprintf "/home/lykhovyd/progs/icfpc2024/icfpc2024/spaceship_sorted/spaceship%d.txt" problem_id in *)
(*    let points = points_of_string @@ String.trim @@ Std.input_file file in *)
   walk points

(*
let () =
  let debug = solve1d 5 10 3 10 in
  List.iter (fun ll -> List.iter (print_endline $ show_t_direction) ll) debug
*)
(*
let () =
  let res_str,_,_ = solve2d 0 5 0 10 0 0 in
  print_endline res_str *)
