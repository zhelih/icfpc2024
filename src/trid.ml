open Devkit

type v =
| Empty
| Num of int
| Shift of (int * int)
| Bin of (int -> int -> int)
| Cmp of (int -> int -> bool)
| Warp
| Submit

module Grid : sig
type t
val get : (int*int) -> v
val set : (int*int) -> v -> unit
val seq : unit -> ((int*int) * v) Seq.t
end = struct
type t = (int*int, v) Hashtbl.t
let t = Hashtbl.create 10
let get p = Option.default Empty @@ Hashtbl.find_opt t p
let set p v = if v = Empty then Hashtbl.remove t p else Hashtbl.replace t p v
let seq () = Hashtbl.to_seq t
end

let run ?(a=7) ?(b=4) lines =
  List.rev lines
  |> List.iteri begin fun y line ->
    String.split_on_char ' ' line |> List.filter ((<>) "")
    |> List.iteri begin fun x token ->
      let v = match token with
      | "." -> Empty
      | ">" -> Shift (1,0) | "v" -> Shift (0,-1) | "<" -> Shift (-1,0) | "^" -> Shift (0,1)
      | "*" -> Bin Int.mul | "+" -> Bin Int.add | "-" -> Bin Int.sub | "/" -> Bin Int.div | "%" -> Bin Int.rem
      | "=" -> Cmp (=) | "#" -> Cmp (<>)
      | "S" -> Submit | "A" -> Num a | "B" -> Num b
      | "@" -> Warp
      | _ -> Num (int_of_string token)
      in
      Grid.set (x,y) v
    end
  end;
  let tick () =
    let read =
      Grid.seq ()
    in
    ()
  in

  ()

let run_file ?a ?b file = run ?a ?b @@ Action.file_lines_exn file

let () =
  run [". < 3 "];
  run [". 3 ."; "4 + ."];
  ()
