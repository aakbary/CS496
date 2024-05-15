(*

   Quiz 5 - Mutable Data Structures in OCaml
   31 Mar 2023
   Names:
   Pledge:
*)

type 'a node = {
  mutable data: 'a;
  mutable left: 'a node option;
  mutable right: 'a node option}


type 'a bt = {
  mutable root: 'a node option;
  mutable size: int}

(* Sample binary tree:

      7
     / \
    3   77
       /
      33
*)

let t1:int bt =
  { root = Some { data=7;
                  left = Some {data=3; left=None; right=None};
                  right = Some {data=77;
                                left=Some {data=33; left=None; right=None};
                                right=None} };
    size = 4}


(** [mem e t] determines whether [e] belongs to [t].
    Eg. 
    # mem 77 t1;;
    - : bool = true
    # mem 78 t1;;
    - : bool = false
*)
let mem : 'a -> 'a bt -> bool =
  fun e t ->
  failwith "implement"

(** [maxt t] returns the largest element in [t]. 
    It fails if [t] is empty.
    Eg. 
    # maxt t1;;
    - : int = 77
*)
let maxt : 'a bt -> 'a =
  fun t ->
  failwith "implement"
    
(** [rem_max no] removes the maximum element from a tree rooted at [no].
    NOTE1: this function applies to a NODE not a TREE. 
    NOTE2: this function returns a NODE.
    NOTE3: this function does not rely on the previous one.
    Eg. 
    # t1.root <- rem_max_node t1.root;;
    - : unit = ()
    # t1;;
    - : int bt =
    {root = Some
             {data = 7; left = Some {data = 3; left = None; right = None};
              right = Some {data = 33; left = None; right = None}};
     size = 4}
*)
let rec rem_max : 'a node option -> 'a node option =
  fun no ->
  failwith "implement"
