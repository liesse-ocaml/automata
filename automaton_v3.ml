(* A third implementation of the automaton, here the automaton may be non-deterministic *)
(* In this implementation the automaton is a value (a record) *)
(* Notice the types for state and symbol are abstracted *)
type ('state,'symbol) automaton = {
  init: 'state list;
  is_final: 'state -> bool;
  transition: 'state * 'symbol -> 'state list
}

(* accept change, it must folllow all paths and find if there is any path *)
let accept automaton word =
  let rec accept_rec states = function
    | []     -> states
    | ch::tl ->
      begin
        match List.fold_left (fun acc state -> Utils.union acc @@ automaton.transition (state,ch)) [] states with
        | [] -> []
        | l -> accept_rec l tl
      end
  in
  match accept_rec automaton.init word with
  | [] -> false
  | states -> List.exists automaton.is_final states

(* Our first simple automaton *)
let fa1 = {
  init = [1];
  is_final = (function 2 -> true | _ -> false);
  transition = function
    | (1,'a') -> [2]
    | (2,'b') -> [1;2]
    | (_,_)   -> []
}

let _ =
  Utils.run (fun str -> accept fa1 @@ Utils.explode str) "automate 1"
