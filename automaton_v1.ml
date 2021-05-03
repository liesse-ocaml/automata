(* A first implementation of the automaton *)
(* In this implementation accept is a recursive function *)
module Fa_1: Itf.FA =
  struct
    let accept str =
      let rec accept_rec = function
        | (1, 'a'::tl) -> accept_rec (1,tl)
        | (2, 'b'::tl) -> accept_rec (2,tl)
        | (2, [])      -> true
        | (_, _)       -> false
      in accept_rec (1,(Utils.explode str))
  end

(* In this version the definition of the automaton is extracted from the accept function *)
module Fa_2: sig include Itf.FA include Itf.FA_DEF end =
  struct
    (* initial state of the automaton *)
    let init = 1
    (* list of final states *)
    let final = [2]
    (* transition function, notice that it is partial, hence the option type *)
    let transition = function
      | (1, 'a') -> Some 2
      | (2, 'b') -> Some 1
      | (_,_)    -> None
        (* now accept is generic (independent of the automaton) *)
    let accept str =
      let rec accept_rec state = function
        | []     -> Some state
        | ch::tl ->
           begin
             match transition (state,ch) with
             | None -> None
             | Some st -> accept_rec st tl
           end
      in
      match accept_rec init (Utils.explode str) with
      | None -> false
      | Some state -> List.mem state final
  end

(* third version juste to show the statisfaction of the generic interface *)
module Fa_3: Itf.FA_G with type symbol := char and type state := int =
  struct
    include Fa_2
    (* here we need to overload the function accept to satisfy the required type *)
    let accept word =
      let rec accept_rec state = function
        | []     -> Some state
        | ch::tl ->
           begin
             match transition (state,ch) with
             | None -> None
             | Some st -> accept_rec st tl
           end
      in
      match accept_rec init word with
      | None -> false
      | Some state -> List.mem state final
  end

let _ =
  Utils.run Fa_1.accept "automate 1";
  Utils.run Fa_2.accept "\nautomate 1 version 2"
