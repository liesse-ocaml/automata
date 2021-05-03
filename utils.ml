(* function that transforms a string in a char list *)
let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

(* running an automaton described by its accept function *)
(* it asks the user for a string to test the automaton repeatedly *)
let run accept_fun msg =
  try
    Printf.printf "%s (CTRL-D to quit)\n" msg;
    while true do
      let input = read_line () in
      match accept_fun input with
      | true -> Printf.printf "La chaîne %s est acceptée\n" input
      | _    -> Printf.printf "La chaîne %s n'est pas acceptée\n" input
    done
  with End_of_file -> ()

(* ste union on list (viewed as sets), beware very naive algorithm *)
let union l1 l2 =
  match (l1,l2) with
  | ([],l) | (l,[]) -> l
  | (_,_) -> List.fold_left (fun acc elt -> if List.mem elt acc then acc else elt::acc) l1 l2
