
(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(*Helper function*)
let rec tail_check acc input =
  match input with 
  | [] -> acc
  | h::t -> 
    if List.mem h acc
    then tail_check acc t
  else
    tail_check (h :: acc) t

(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

(* Implementing first function to traverse through the FA*)
let rec apply_transition_function tf st sym =
  match tf with
  | [] -> None
  | (s,c,s') :: t1 when s = st && c = sym -> Some s'
  | _ :: t1 -> apply_transition_function t1 st sym

(* Implementing second function 
   making a recursive function that goes through each symbol in the input, if input state is not empty it will use
    the transition function to determine the next state. If no valid transition the function will return false *)
let rec accept fa input =
  let rec helpMe state input =
    match input with
    |[] -> state
    |sym::symbs ->
      match state with
      |Some q -> helpMe(apply_transition_function fa.tf q sym) symbs
      |None -> None
    in match helpMe(Some fa.start) input with
    |None -> false
    |Some state -> List.mem state fa.final

(* Implementing third function *)
let next f st s =
  let get_next (s1,sym,s2) =
    if s1 = st && sym = s then [s2] else []
  in
  List.flatten (List.map get_next f)

(* Implementing fourth function:
   checks if any states in the transition function that have multiple transitions with the same symbol *)
let deterministic (f: fa) : bool =
  let rec check_states (s:state list) (tff: tf) : bool =
    (* tff should equal the symbol*)
    match tff with 
    | [] -> true
    | (s1,sy,s2)::t -> 
      (*checking states*)
      (match s with
      | [] -> false
      | state::rest -> 
        if state = s1 then
          check_states rest t
        else
          check_states rest tff)
    in
    check_states f.states f.tf

(* Implementing fifth function: checks for validitiy, a boolean function *)
let rec valid fa =
  let is_state state = List.mem state fa.states
  in is_state fa.start && List.for_all is_state fa.final && deterministic fa

(* Implementing sixth function *)
let reachable fa =
  (* making a list of symbols, do a rec *)
  let alphabet = tail_check [] @@ List.map (fun (_,sym,_) -> sym) fa.tf in
  let rec reachHelp states =
    match states with
    | [] -> [] (*empty*)
    | h::tl ->  let n = List.flatten (List.map (fun a -> next fa.tf h a) alphabet) in
    List.append (reachHelp n) n
    in tail_check [] @@ List.append  (reachHelp fa.states) [fa.start] (*getting rid of duplicates*)
    
(* Implementing seventh function *)
let non_empty fa =
  let reachable_states = reachable fa in
  List.exists (fun state -> List.mem state fa.final) reachable_states

(* Implementing eighth function *)
let remove_dead_states : fa -> fa =
  fun fa ->
    let r = reachable fa in
    let new_fa = {
      states = r;
      start = fa.start;
      tf = List.filter(fun (s,c,s') -> List.mem s r && List.mem s' r)fa.tf;
      final = List.filter(fun a -> List.mem a r) fa.final; 
    } in new_fa