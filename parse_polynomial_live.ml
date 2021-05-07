let mon_polynome = "2x + 1 - x^3"

(* lexical analysis of mon_polynome would produce *)
(* [#2#;#x#;#+#;#1#;#-#;#x#;#^#;#3#] *)

let mon_polynome_2 = "2x + 1.3 - x^32"

(* lexical analysis of mon_polynome would produce *)
(* [#2#;#x#;#+#;#1.3#;#-#;#x#;#^#;#32#] *)


let mon_polynome_3 = "2x$ + 1.€3 - x^32" (* require to treat errors *)
let mon_polynome_4 = "" (* require to treat empty string but also the end of string *)
type token =
| Int of int
| Var
| Add
| Minus
| Float of float
| Caret
| LexError of char
| EndOfInput

let _ = [Int 2; Var; Add; Float 1.3; Minus; Var; Caret; Int 32; EndOfInput]

let first_result = (Int 2, ['x';' ';'+';' ';'1';'.';'3';' ';'-';' ';'x';'^';'3';'2'])

let rec next_token = function
| [] -> EndOfInput, []
| ' '::t -> next_token t
| ('0'..'9' as c)::t -> read_int (String.make 1 c) t
| c::t -> LexError c, t
and read_int str = function
| ('0'..'9' as c)::t -> read_int (str^(String.make 1 c)) t
| '.'::t -> read_float (str^".") t
| l -> Int (int_of_string str),l
and read_float str = function
| ('0'..'9' as c)::t -> read_float (str^(String.make 1 c)) t
| l -> Float (float_of_string str),l

let test_1 () = (next_token (Utils.explode mon_polynome_2)) = first_result

let _ = test_1 ();;

let _,r = next_token (Utils.explode mon_polynome_2) in
next_token r

let rec next_token = function
| [] -> EndOfInput, []
| ' '::t -> next_token t
| ('0'..'9' as c)::t -> read_int (String.make 1 c) t
| '.'::t -> read_float "." t
| ('x' | 'X')::t -> Var, t
| '+'::t -> Add, t
| '-'::t -> Minus, t
| '^'::t -> Caret, t
| c::t -> LexError c, t
and read_int str = function
| ('0'..'9' as c)::t -> read_int (str^(String.make 1 c)) t
| '.'::t -> read_float (str^".") t
| l -> Int (int_of_string str),l
and read_float str = function
| ('0'..'9' as c)::t -> read_float (str^(String.make 1 c)) t
| l -> Float (float_of_string str),l

exception ParseException

open Printf

let parse_o input =
  match next_token input with
  | (Add | Minus), rest -> rest
  | _,_ -> raise ParseException

let parse_n input =
  match next_token input with
  | (Int _ | Float _), rest -> rest
  | _,_ -> raise ParseException

let parse_int input =
  match next_token input with
  | Int _, rest -> rest
  | _,_ -> raise ParseException

let parse_v input =
  match next_token input with
  | Var, rest ->
    begin
      match next_token rest with (* ici il y avait input *)
      | Caret, rest -> parse_int rest
      | LexError _, _ -> raise ParseException
      | _, _ -> rest
    end
  | _, _ -> raise ParseException

let parse_m input =
  match next_token input with
  | Var, _ -> parse_v input
  | (Int _ | Float _), _ -> let rest = parse_n input in (try parse_v rest with ParseException -> rest) (* ici il manquait la recup de l'exception *)
  | _, _ -> raise ParseException

let rec parse_s input =
  try
    (match next_token (parse_m input) with
     | (Add | Minus), rest -> parse_s rest
     | EndOfInput, _ -> true
     | _, _ -> false)
  with ParseException -> false

let test_and_print p = printf "accept(%S) = %b\n" p @@ parse_s (Utils.explode p)

let _ =
  test_and_print mon_polynome;
  test_and_print mon_polynome_2;
  test_and_print mon_polynome_3;
  test_and_print mon_polynome_4
