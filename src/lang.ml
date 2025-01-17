open Printf
open Devkit
open ExtLib

let tee f x = f x; x

let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
let alphabet_inv = String.init 256 (fun i -> match String.index alphabet (Char.chr i) with exception _ -> '\x00' | p -> Char.chr (p+33))
let () = assert (String.length alphabet = 94)

type binop =
| Plus
| Minus
| Mul
| Div
| Mod
| LT
| GT
| Or
| And
| Eq
| Concat
| Apply
| ApplyCBV
| Drop
| Take
[@@deriving show {with_path=false}]

module Z = struct
  include Z
  let pp fmt z = Format.pp_print_string fmt (to_string z)
  let show = to_string
end

type t =
| S of string
| Bool of bool
| B of binop * t * t
| I of Z.t
| V of int
| L of int * t
| Neg of t
| Not of t
| Int_of_string of t
| String_of_int of t
| If of t * t * t
| Recurse of int * int * t
[@@deriving show {with_path=false}]

let decode_int s = fst @@ String.fold_right (fun c (acc,exp) -> let c = Char.code c - 33 in Z.(of_int c * exp + acc), Z.(exp * Z.of_int 94)) s Z.(zero,one)
let encode_int n =
  let rec encode acc n =
    if Z.equal n Z.zero && acc <> [] then
      String.of_seq @@ List.to_seq acc
    else
      let c = Char.chr (Z.to_int Z.(rem n (of_int 94)) + 33) in
      encode (c::acc) Z.(n / of_int 94)
  in
  encode [] n

let decode_string = String.map (fun c -> alphabet.[Char.code c - 33])
let encode_string = String.map (fun c -> alphabet_inv.[Char.code c])

let rec decode next =
  match next () with
  | None | Some "" -> Exn.fail "empty"
  | Some s ->
  match s with
  | "T" -> Bool true
  | "F" -> Bool false
  | "?" -> let cond = decode next in let a = decode next in let b = decode next in If (cond,a,b)
  | "U-" -> Neg (decode next)
  | "U!" -> Not (decode next)
  | "U#" -> Int_of_string (decode next)
  | "U$" -> String_of_int (decode next)
  | _ ->
  let int () = decode_int @@ String.slice ~first:1 s in
  match s.[0] with
  | 'S' -> S (decode_string @@ String.slice ~first:1 s)
  | 'B' -> assert (String.length s = 2);
    let op =
      match s.[1] with
      | '+' -> Plus
      | '-' -> Minus
      | '=' -> Eq
      | '.' -> Concat
      | '$' -> Apply
      | '!' -> ApplyCBV
      | 'D' -> Drop
      | 'T' -> Take
      | '*' -> Mul
      | '/' -> Div
      | '%' -> Mod
      | '<' -> LT
      | '>' -> GT
      | '|' -> Or
      | '&' -> And
      | c -> Exn.fail "unsupported binary operator %c" c
    in
    let a = decode next in
    let b = decode next in
    B (op,a,b)
  | 'L' -> L (Z.to_int @@ int (), decode next)
  | 'v' -> V (Z.to_int @@ int ())
  | 'I' -> I (int ())
  | c -> Exn.fail "unsupported indicator %c" c

let decode s =
  let next = String.split_on_char ' ' s |> List.to_seq |> Seq.to_dispenser in
  let t = decode next in
  assert (next () = None);
  t

let encode_binop = function
| Plus -> '+'
| Minus -> '-'
| Eq -> '='
| Concat -> '.'
| Apply -> '$'
| ApplyCBV -> '!'
| Take -> 'T'
| Drop -> 'D'
| Mul -> '*'
| Div -> '/'
| Mod -> '%'
| LT ->'<'
| GT -> '>'
| Or -> '|'
| And -> '&'

let rec encode = function
  | S s -> "S" ^ encode_string s
  | Bool true -> "T"
  | Bool false -> "F"
  | If (c,a,b) -> sprintf "? %s %s %s" (encode c) (encode a) (encode b)
  | L (v,b) -> sprintf "L%s %s" (encode_int @@ Z.of_int v) (encode b)
  | V n -> sprintf "v%s" (encode_int @@ Z.of_int n)
  | I n -> sprintf "I%s" (encode_int n)
  | Neg x -> sprintf "U- %s" (encode x)
  | Not x -> sprintf "U! %s" (encode x)
  | Int_of_string x -> sprintf "U# %s" (encode x)
  | String_of_int x -> sprintf "U$ %s" (encode x)
  | B (op,a,b) ->
    sprintf "B%c %s %s" (encode_binop op) (encode a) (encode b)
  | Recurse _ -> assert false

let expect_string = function
  | S s -> s
  | _ -> Exn.fail "received non-string answer"

let type_error exp got = Exn.fail "Expected value of type %s, got %s" exp (show got)

let substitute fn var exp =
  let rec subst = function
  | S _ | Bool _ | I _ as x -> x
  | B (op, a, b) -> B (op, subst a, subst b)
  | V i -> if i = var then exp else V i
  | L (v, body) -> L (v, if v = var then body else subst body) (* handle shadowing *)
  | Recurse (v,arg,body) -> Recurse (v, arg, if v = var || arg = var then body else subst body)
  | Neg e -> Neg (subst e)
  | Not e -> Not (subst e)
  | Int_of_string e -> Int_of_string (subst e)
  | String_of_int e -> String_of_int (subst e)
  | If (c, a, b) -> If (subst c, subst a, subst b)
  in
  subst fn

let three = Z.of_int 3
let four = Z.of_int 4

let rec print' indent exp =
let print = print' indent in
match exp with
| Bool b -> sprintf "%B" b
| I n -> Z.to_string n
| S s -> sprintf "%S" s
| L (v,fn) -> sprintf "(fun v%d ->\n%s%s)" v (String.make indent ' ') (print' (indent + 2) fn)
| Recurse(self,arg,fn) -> sprintf "(let v%d v%d = (fun v%d ->\n%s%s) in Memo.memoize v%d)" self self arg (String.make indent ' ') (print' (indent+2) fn) self
| V n -> sprintf "v%d" n
| Neg v -> sprintf "-%s" (print v)
| Not v -> sprintf "not %s" (print v)
| Int_of_string v -> sprintf "#%s" (print v)
| String_of_int v -> sprintf "$%s" (print v)
| If (c,a,b) -> sprintf "(if %s then %s else %s)" (print c) (print a) (print b)
| B (op,a,b) ->
  sprintf (match op with
  | Plus -> "(%s+%s)"
  | Mul -> "(%s*%s)"
  | Div -> "(%s/%s)"
  | Mod -> "(%s mod %s)"
  | Minus -> "(%s-%s)"
  | Or -> "(%s || %s)"
  | And -> "(%s && %s)"
  | LT -> "(%s < %s)"
  | GT -> "(%s > %s)"
  | Eq -> "(%s = %s)"
  | Concat -> "(%s ^ %s)"
  | Apply | ApplyCBV -> "(%s %s)"
  | Drop -> "(drop %s %s)"
  | Take -> "(take %s %s)"
  ) (print a) (print b)

let print = print' 2

let rec int_eval ctx x = match eval ctx x with I n -> n | _ -> type_error "int" x
and str_eval ctx x = match eval ctx x with S s -> s | _ -> type_error "string" x
and bool_eval ctx x = match eval ctx x with Bool b -> b | _ -> type_error "bool" x
and eval ctx = function
| Bool _ | I _ | S _ | L _ | Recurse _ as x -> x
| V n -> begin match List.assoc_opt n ctx with Some e -> eval ctx e | None -> Exn.fail "unbound variable %d" n end
| Neg a -> I (Z.neg @@ int_eval ctx a)
| Not a -> Bool (not @@ bool_eval ctx a)
| Int_of_string a -> I (decode_int @@ encode_string @@ str_eval ctx a)
| String_of_int a -> S (decode_string @@ encode_int @@ int_eval ctx a)
| If (c,a,b) -> if bool_eval ctx c then eval ctx a else eval ctx b
| B (op,a,b) ->
  let int e op = I (op (e ctx a) (e ctx b)) in
  let bool e op = Bool (op (e ctx a) (e ctx b)) in
  match op with
  | Plus -> int int_eval Z.add
  | Minus -> int int_eval Z.sub
  | Mul ->
    begin match a, b with
    | I n, _ | _, I n when Z.equal n Z.zero -> I Z.zero
    | _ -> int int_eval Z.mul
    end
  | Div ->
    let a = int_eval ctx a in
    let b = int_eval ctx b in
    if b = four then I (Z.shift_right a 2) else
    int int_eval Z.div
  | Mod ->
    begin match b with
    | I z when Z.equal Z.one z -> I Z.zero
    | _ ->
    let a = int_eval ctx a in
    let b = int_eval ctx b in
    if b = four then I (Z.logand a three) else
    int int_eval Z.rem
    end
  | LT -> bool int_eval (<)
  | GT -> bool int_eval (>)
  | Or ->
    begin match a, b with
    | Bool true, _ | _, Bool true -> Bool true
    | _ -> bool bool_eval (||)
    end
  | And ->
    begin match a, b with
    | Bool false, _ | _, Bool false -> Bool false
    | _ -> bool bool_eval (&&)
    end
  | Eq -> bool eval (=)
  | Concat -> S (str_eval ctx a ^ str_eval ctx b)
  | Drop -> S (String.slice ~first:(Z.to_int @@ int_eval ctx a) (str_eval ctx b))
  | Take -> let s = str_eval ctx b in print_char s.[0]; if Random.int 10 = 0 then flush stdout;  S (String.slice ~last:(Z.to_int @@ int_eval ctx a) s)
  | Apply ->
    begin match eval ctx a with
    | L (var, fn) ->
      eval (List.remove_assoc var ctx) @@ substitute fn var b (* call-by-name *)
(*       @@ tee (fun e -> printfn "apply L : %s" (print e)) *)
    | Recurse(self,var,fn) ->
(*       let b = match eval ctx b with I n -> if Z.equal (Z.rem n (Z.of_int 100_000)) Z.zero then printfn "counter %s" (Z.to_string n); I n | x -> x in *)
      eval ((self,Recurse(self,var,fn)) :: List.remove_assoc self (List.remove_assoc var ctx)) @@
(*       tee (fun e -> printfn "eval %d apply Recurse : %s" (List.length ctx) (print e)) @@ *)
      substitute fn var (eval ctx b)
    | x -> type_error "lambda" x
    end
  | ApplyCBV ->
    begin match eval ctx a with
    | L (var, fn) -> eval (List.remove_assoc var ctx) @@ substitute fn var (eval ctx b)
    | x -> type_error "lambda" x
    end

let eval x = eval [] x

let map f = function
| B (op,a,b) -> B (op,f a,f b)
| S _ | Bool _ | I _ | V _ as x -> x
| L (v,t) -> L (v,f t)
| Neg x -> Neg (f x)
| Not x -> Not (f x)
| Int_of_string x -> Int_of_string (f x)
| String_of_int x -> String_of_int (f x)
| If (c,a,b) -> If (f c, f a, f b)
| Recurse (self,arg,t) -> Recurse(self,arg,f t)

(* (\v1 -> ((\v2 -> v1 (v2 v2)) (\v2 -> v1 (v2 v2)))) (\v3 -> \v4 -> ..) *)
let rec apply_recurse = function
| B(Apply,L(v1,B(Apply,L(v2,l1),L(v3,l2))),L(self,L(v,b)))
  when l1 = B(Apply,V v1,B(Apply,V v2,V v2)) && l2 = B(Apply,V v1,B(Apply,V v3,V v3)) -> (* printfn "detected recurse : %s" (print x); *)
    Recurse(self,v,apply_recurse b)
| x -> map apply_recurse x
