(* A second implementation of the automaton *)
(* In this implementation the automaton is a value (a record) *)
(* Notice that the record contains two functions *)
type ('state,'symbol) automaton = {
  init: 'state;
  is_final: 'state -> bool;
  transition: 'state * 'symbol -> 'state option
}

(* the only impact on accept is to use the automaton argument fields instead of values from the context *)
let accept automaton word =
  let rec accept_rec state = function
    | []     -> Some state
    | ch::tl ->
      begin
        match automaton.transition (state,ch) with
        | None -> None
        | Some st -> accept_rec st tl
      end
  in
  match accept_rec automaton.init word with
  | None -> false
  | Some state -> automaton.is_final state

(* Our first simple automaton *)
let fa1 = {
  init = 1;
  is_final = (function 2 -> true | _ -> false);
  transition = function
    | (1, 'a') -> Some 2
    | (2, 'b') -> Some 1
    | (_,_)    -> None
}

let _ =
  Utils.run (fun str -> accept fa1 @@ Utils.explode str) "automate 1"
