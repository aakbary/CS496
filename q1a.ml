(* Quiz 1 - 1 Feb 2023 

   Student name 1: 
   Student name 2:
   Pledge:
*)

(* Sample Directed Graph *)

let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]


(*
  1 <------ 3
  |      //||
  |     /   | 
  |    /    | 
 \/  /     \/
  2        4
*)
       
(* 
Eg. outgoing ex 3 => [1,4] 
*)
let rec outgoing_nodes g n =
  failwith "implement"

(* 
   The list of nodes of the tree without duplicates. The order of the   nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)
let rec nodes g =
  failwith "implement"

(** [degree g]  returns the degree of [g]. The degree of a graph is 
    the maximum number of outgoing edges that any node has. 
*)
let rec degree g =
  failwith "implement"

(* 
   Remove a node from the graph
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
  failwith "implement"
  
(* Reachable nodes from a source node. (Extra-credit)
   Eg. reachale ex 3 => [1,4,2,3] 
   *)

let reachable g n =
  failwith "implement"

