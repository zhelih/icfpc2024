open Printf
open Devkit
open ExtLib

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
| Drop
| Take
[@@deriving show {with_path=false}]

type t =
| S of string
| Bool of bool
| B of binop * t * t
| I of int
| V of int
| L of int * t
| Neg of t
| Not of t
| Int_of_string of t
| String_of_int of t
| If of t * t * t
[@@deriving show {with_path=false}]

let decode_int s = fst @@ String.fold_right (fun c (acc,exp) -> (Char.code c - 33) * exp + acc, exp * 94) s (0,1)

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
  assert (String.length s > 1);
  let int () = decode_int @@ String.slice ~first:1 s in
  match s.[0] with
  | 'S' -> S (String.map (fun c -> alphabet.[Char.code c - 33]) @@ String.slice ~first:1 s)
  | 'B' -> assert (String.length s = 2);
    let op =
      match s.[1] with
      | '+' -> Plus
      | '-' -> Minus
      | '=' -> Eq
      | '.' -> Concat
      | '$' -> Apply
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
  | 'L' -> L (int (), decode next)
  | 'v' -> V (int ())
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
  | S s -> "S" ^ String.map (fun c -> alphabet_inv.[Char.code c]) s
  | Bool true -> "T"
  | Bool false -> "F"
  | If (c,a,b) -> sprintf "? %s %s %s" (encode c) (encode a) (encode b)
  | L _
  | V _
  | I _ -> Exn.fail "todo"
  | Neg x -> sprintf "U- %s" (encode x)
  | Not x -> sprintf "U! %s" (encode x)
  | Int_of_string x -> sprintf "U# %s" (encode x)
  | String_of_int x -> sprintf "U$ %s" (encode x)
  | B (op,a,b) ->
    sprintf "B%c %s %s" (encode_binop op) (encode a) (encode b)

let expect_string = function
  | S s -> s
  | _ -> Exn.fail "received non-string answer"

let type_error exp got = Exn.fail "Expected value of type %s, got %s" exp (show got)

let substitude fn var exp =
  let rec subst = function
  | S _ | Bool _ | I _ as x -> x
  | B (op, a, b) -> B (op, subst a, subst b)
  | V i -> if i = var then exp else V i
  | L (v, body) -> L (v, if v = var then body else subst body) (* handle shadowing *)
  | Neg e -> Neg (subst e)
  | Not e -> Not (subst e)
  | Int_of_string e -> Int_of_string (subst e)
  | String_of_int e -> String_of_int (subst e)
  | If (c, a, b) -> If (subst c, subst a, subst b)
  in
  subst fn

let rec int_eval x = match eval x with I n -> n | _ -> type_error "int" x
and str_eval x = match eval x with S s -> s | _ -> type_error "string" x
and bool_eval x = match eval x with Bool b -> b | _ -> type_error "bool" x
and eval = function
| Bool _ | I _ | S _ as x -> x
| V n -> Exn.fail "unbound variable %d" n
| L (v,e) -> Exn.fail "lambda unchained (fun %d -> %s)" v (show e)
| Neg a -> I (Int.neg @@ int_eval a)
| Not a -> Bool (not @@ bool_eval a)
| Int_of_string a -> I (decode_int @@ str_eval a)
| String_of_int _ -> S (assert false)
| If (c,a,b) -> eval @@ if bool_eval c then a else b
| B (op,a,b) ->
  let int e op = I (op (e a) (e b)) in
  let bool e op = Bool (op (e a) (e b)) in
  match op with
  | Plus -> int int_eval (+)
  | Minus -> int int_eval (-)
  | Mul -> int int_eval ( * )
  | Div -> int int_eval (/)
  | Mod -> int int_eval (mod)
  | LT -> bool int_eval (<)
  | GT -> bool int_eval (>)
  | Or -> bool bool_eval (||)
  | And -> bool bool_eval (&&)
  | Eq -> bool eval (=)
  | Concat -> S (str_eval a ^ str_eval b)
  | Drop -> S (String.slice ~first:(int_eval a) (str_eval b))
  | Take -> S (String.slice ~last:(int_eval a) (str_eval b))
  | Apply ->
    match eval a with
    | L (var, fn) ->
      eval @@ substitude fn var b
    | x -> type_error "lambda" x
