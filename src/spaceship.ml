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

let reorder_points x = x

(*** EL quick mess to solve something ***)

let solve1d l r v0 k =
  (* solve 1d problem from l to r initial speed v0 and in k steps, monotonic only *)
  let rem0 = r - l - v0*k in
  let is_incr = rem0 >= 0 in
  let rem = abs rem0 in
  if rem0 = 0 then [ List.init k (fun _ -> S) ] else begin (*FIXME This is not the ONLY solution tho *)
  let rec loop g sols =
    if g >= k+1 then sols else begin
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
    loop (g+1) new_sols
    end
  in
  loop 1 []
  end

let solve2d x0 x1 y0 y1 vx vy =
  (* solve 2d problem *)
  let rec loop k =
    let res_x = solve1d x0 x1 vx k in
    let res_y = solve1d y0 y1 vy k in
    match res_x, res_y with
    | [], _ | _, [] -> if k >= 100_000 then "couldn't solve", 0, 0 else loop (k+1)
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
    res_str, new_vx, new_vy
  in
  loop 0

let walk points =
 let rec loop ps x0 y0 vx0 vy0 output =
  match ps with
  | [] -> output
  | (x,y)::tl ->
    let x = int_of_string x and y = int_of_string y in
    let s, vx, vy = solve2d x0 x y0 y vx0 vy0 in
    loop tl x y vx vy (s ^ output)
  in
  loop points 0 0 0 0 ""

let solve file =
   let points = reorder_points @@ points_of_string @@ String.trim @@ Std.input_file file in
   walk points
(*
let () =
  let debug = solve1d 5 10 3 10 in
  List.iter show_t_direction debug
  *)
