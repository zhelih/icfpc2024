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
  | L _
  | V _ -> Exn.fail "todo"
  | I n -> sprintf "I%s" (encode_int n)
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

let substitute fn var exp =
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

let three = Z.of_int 3
let four = Z.of_int 4

let rec int_eval x = match eval x with I n -> n | _ -> type_error "int" x
and str_eval x = match eval x with S s -> s | _ -> type_error "string" x
and bool_eval x = match eval x with Bool b -> b | _ -> type_error "bool" x
and eval = function
| Bool _ | I _ | S _ | L _ as x -> x
| V n -> Exn.fail "unbound variable %d" n
| Neg a -> I (Z.neg @@ int_eval a)
| Not a -> Bool (not @@ bool_eval a)
| Int_of_string a -> I (decode_int @@ encode_string @@ str_eval a)
| String_of_int a -> S (decode_string @@ encode_int @@ int_eval a)
| If (c,a,b) -> if bool_eval c then eval a else eval b
| B (op,a,b) ->
  let int e op = I (op (e a) (e b)) in
  let bool e op = Bool (op (e a) (e b)) in
  match op with
  | Plus -> int int_eval Z.add
  | Minus -> int int_eval Z.sub
  | Mul -> int int_eval Z.mul
  | Div ->
    let a = int_eval a in
    let b = int_eval b in
    if b = four then I (Z.shift_right a 2) else
    int int_eval Z.div
  | Mod ->
    let a = int_eval a in
    let b = int_eval b in
    if b = four then I (Z.logand a three) else
    int int_eval Z.rem
  | LT -> bool int_eval (<)
  | GT -> bool int_eval (>)
  | Or -> bool bool_eval (||)
  | And -> bool bool_eval (&&)
  | Eq -> bool eval (=)
  | Concat -> S (str_eval a ^ str_eval b)
  | Drop -> S (String.slice ~first:(Z.to_int @@ int_eval a) (str_eval b))
  | Take -> let s = (str_eval b) in print_char s.[0]; if Random.int 10 = 0 then flush stdout;  S (String.slice ~last:(Z.to_int @@ int_eval a) s)
  | Apply ->
    match eval a with
    | L (var, fn) -> eval @@ substitute fn var b (* call-by-name *)
    | x -> type_error "lambda" x

let rec print' indent exp =
let print = print' indent in
match exp with
| Bool b -> sprintf "%B" b
| I n -> Z.to_string n
| S s -> sprintf "%S" s
| L (v,fn) -> sprintf "(\\v%d ->\n%s%s)" v (String.make indent ' ') (print' (indent + 2) fn)
| V n -> sprintf "v%d" n
| Neg v -> sprintf "-%s" (print v)
| Not v -> sprintf "!%s" (print v)
| Int_of_string v -> sprintf "#%s" (print v)
| String_of_int v -> sprintf "$%s" (print v)
| If (c,a,b) -> sprintf "if %s then %s else %s" (print c) (print a) (print b)
| B (op,a,b) ->
  sprintf (match op with
  | Plus -> "(%s+%s)"
  | Mul -> "(%s*%s)"
  | Div -> "(%s/%s)"
  | Mod -> "(%s %% %s)"
  | Minus -> "(%s-%s)"
  | Or -> "(%s or %s)"
  | And -> "(%s and %s)"
  | LT -> "(%s < %s)"
  | GT -> "(%s > %s)"
  | Eq -> "(%s = %s)"
  | Concat -> "(%s ^ %s)"
  | Apply -> "(%s %s)"
  | Drop -> "(drop %s %s)"
  | Take -> "(take %s %s)"
  ) (print a) (print b)

let print = print' 2
