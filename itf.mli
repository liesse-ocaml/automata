(* simple finite automaton *)
module type FA =
  sig
    val accept: string -> bool
  end

(* the definition of a simple finite automaton *)
module type FA_DEF =
  sig
    val init: int
    val final: int list
    val transition: int * char -> int option
  end

(* general finite automaton *)
module type FA_G =
  sig
    type state
    type symbol
    val init: state
    val final: state list
    val transition: state * symbol -> state option
    val accept: symbol list -> bool
  end
