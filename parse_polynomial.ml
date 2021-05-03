(* the type of tokens the lexer returns *)
type token =
| Int of int        (* integer *)
| Float of float    (* float *)
| Add               (* + *)
| Minus             (* - *)
| Var               (* x *)
| Caret             (* ^ *)
| EndOfInput        (* end of the input *)
| LexError of char  (* wrong character *)

(* main lexing function: next_token l examine l and returns the next found token and the rest of the input list *)
let rec next_token = function
| [] -> EndOfInput,[]
(* spacing is ignored *)
| (' ' | 't'):: t -> next_token t
| '+'::t -> Add, t
| '-'::t -> Minus, t
| '^'::t -> Caret, t
| (('0' .. '9' as c))::t -> read_int (String.make 1 c) t
| ('X' | 'x')::t -> Var, t
| c::t -> LexError c,t

(* reading integer: the first argument is the current integer *)
and read_int str = function
| (('0' .. '9' as c))::t -> read_int (str^(String.make 1 c)) t
| '.'::t -> read_float (str^".") t
| l -> Int (int_of_string str),l

(* reading float: the first argument is the current float *)
and read_float str = function
| (('0' .. '9' as c))::t -> read_float (str^(String.make 1 c)) t
| l -> Float (float_of_string str),l

(* Defining an operator to manipulate result type easily
   let* motif = expr1 in expr2 is rewritten ( let* ) expr1 (fun motif -> expr2)
   which means Result.bind expr1 (fun motif -> expr2) and therefore
   match expr1 with
   | Error e -> Error e
   | Ok motif -> Ok expr2
*)
let (let*) x f = Result.bind x f

(* parsing function: it receives the input char list and polynomial constructors
   zero builds a zero polynomial
   create_term builds a term from a coefficient and the corresponding power
   sum adds to polynomial
   sym returns the symmetric of a polynomial
   The parsed grammar is term ((+|-) term)* with term is any non empty (float|int)?(x(^int)?)?
*)
  let parse input zero create_term sum sym =
    (* parsing an integer *)
    let parse_int input =
      match next_token input with
      | Int i, t -> Ok(i,t)
      | LexError c, _ -> Error (Printf.sprintf "unknown character '%c'" c)
      | _, _ -> Error "wrong power must be ^int"
    in
    (* parsing the power, no power means power 1 *)
    let parse_pow input =
      match next_token input with
      | Caret, t -> parse_int t
      | LexError c, _ -> Error (Printf.sprintf "unknown character '%c'" c)
      | _ -> Ok(1,input)
    in
    (* parsing the variable, no variable means power 0 *)
    let parse_var input =
      match next_token input with
      | Var, t -> parse_pow t
      | LexError c, _ -> Error (Printf.sprintf "unknown character '%c'" c)
      | _ -> Ok(0,input)
    in
    (* parsing a term a x^n, a can be an integer, a float or be absent (meaning 1)
       if a is present x^n can be absent meaning n=0
       ^n can be absent meaning n=1
    *)
    let parse_term input =
      match next_token input with
      | Var, t ->
        let* (pow,t) = parse_pow t in
        Ok ((create_term 1.0 pow), t)
      | Int i, t ->
        let* (pow,t) = parse_var t in
        Ok ((create_term (float_of_int i) pow), t)
      | Float f, t ->
        let* (pow,t) = parse_var t in
        Ok ((create_term f pow), t)
      | LexError c, _ -> Error (Printf.sprintf "unknown character '%c'" c)
      | _ -> Error "waiting an integer, a float or x"
    in
    (* parsing the rest, ((+|-) a x^n)* *)
    let rec parse_rest input =
      match next_token input with
      | EndOfInput, _ -> Ok zero
      | Add, t ->
        let* (term,t) = parse_term t in
        let* p = parse_rest t in
        Ok (sum p term)
      | Minus, t ->
        let* (term,t) = parse_term t in
        let* p = parse_rest t in
        Ok (sum p (sym term))
      | LexError c, _ -> Error (Printf.sprintf "unknown character '%c'" c)
      | _, _ -> Error "waiting + or -"
    in
    match next_token input with
    (* small optimisation to return more meaningful error message by looking ahead the first symbol *)
    | (Var | Int _ | Float _), _ ->
      let* (term,t) = parse_term input in
      let* x = parse_rest t in
      Ok (sum term x)
    | EndOfInput,_ -> Error "Empty string"
    | LexError c, _ -> Error (Printf.sprintf "unknown character '%c'" c)
    | (Caret | Add | Minus), _ -> Error "wrong start"

(*******************************************************************)
(* Test with a simple polynomial representation as (coef,pow) list *)
(*******************************************************************)

(* In this example a term is a pair coefficient and power
   A polynomial is a list of such terms
*)

open Printf
(* stringify a term *)
let string_of_term = function
| (coef,0)   -> sprintf "%g" coef
| (1.0,1)    -> "x"
| (coef,1)   -> sprintf "%g x" coef
| (1.0,pow)  -> sprintf "x^%i" pow
| (coef,pow) -> sprintf "%g x^%i" coef pow

(* stringify the result of parsing *)
let to_string = function
| Error msg -> sprintf "Error: %s" msg
| Ok term_list ->
  term_list
  |> List.sort (fun (_,pow1) (_,pow2) -> compare pow1 pow2)
  |> List.fold_left (fun acc (coef,pow) ->
      if acc = "" then
        string_of_term (coef,pow)
      else
        let term, op = if coef < 0.0 then (Float.abs coef,pow), "-" else (coef,pow), "+" in
        sprintf "%s %s %s" acc op @@ string_of_term term) ""

(* function that transforms a string in a char list *)
let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

(* the parsing function *)
let parse_term input = parse input [] (fun c p -> [(c,p)]) (fun l1 l2 -> l1@l2) (fun l -> List.map (fun (c,p) -> (-1.0)*.c,p) l)

(* print the original string and the result of the parsing *)
let original_and_parsed x = printf "%S => %s\n" x @@ to_string @@ parse_term @@ explode x

let _ =
  List.iter original_and_parsed [""; "y"; "xx"; "1."; "x^y"; "x^x"; "x^^2"; "x + 1"; "2x + 1 - x^3"; "254x + 123 - x^353"]
