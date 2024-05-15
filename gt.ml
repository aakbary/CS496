(* 
Name: Amena Akbary
Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)

type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37, 
                     [Node (14, [])]); 
               Node (48, []); 
               Node (103, [])])
       ])

type 'a my_gt = {index: int ref; data: 'a; children: 'a my_gt list}
let t2 : int my_gt =
    {
      index=ref 0;
      data=33;
      children=[
        {
          index=ref 0;
          data=12;
          children=[]
        };
        {
          index=ref 1;
          data=77;
          children=[
            {
              index=ref 0;
              data=37;
              children=[
                {
                  index=ref 0;
                  data=14;
                  children=[]
                };
              ]
            };
            {
              index=ref 1;
              data=48;
              children=[]
            };
            {
              index=ref 2;
              data=103;
              children=[]
            };
          ]
        };
      ]}
let rec range (children: 'a gt list) : int list =
  match children with
  | [] -> []
  | h :: t -> 0::(List.map (fun x -> x +1) (range t))

let rec makingindex : 'a gt -> 'a my_gt =
  fun t ->
   match t with
   | Node(d,[]) -> {
    index = ref 0;
    data = d;
    children = []
   }
   | Node(d,ch) -> let cc = (List.map makingindex ch) in
   let fin = (List.map (fun n -> let ccc = (List.nth cc n) in ccc.index := n; ccc) (range ch))in{
    index = ref 0;
    data = d;
    children = fin
   } 
(* height should return a int t *)
let rec height : 'a gt -> int =
  fun t ->
  match t with
  | Node(_, []) -> 1
  (* recusively calling height onto our new list to check for all children *)
  | Node(_, c) -> let new_l = (List.map (fun x -> 1 + height x) c) in
  List.fold_left (fun x y -> if x > y then x else y) 0 new_l
    
let rec size (t: 'a gt) : int =
  match t with
  | Node(_, []) -> 1
  (* recusively calling size to check for all children *)
  | Node(_, c) -> let new_l = (List.map (fun x -> size x) c) in
  List.fold_left (fun x y -> x+y) 1 new_l


let rec paths_helper (tt: 'a my_gt) : int list list =
  match tt with
  | {index = i; data = d; children = []} -> [!i]::[]
  (* dealing with the just the children *)
  | {index = i; data = d; children = c} -> List.map (fun l-> !i::l) (List.concat (List.map (fun x -> paths_helper x) c))

let paths_to_leaves (t: 'a gt) : int list list =
  let tt = makingindex t in
  match tt with
  | {index = i; data = d; children = []} -> []
  | {index = i; data = d; children = c} -> List.fold_left List.append [] (List.map paths_helper c) 

let rec depth (t: 'a gt) : int =
  match t with
  | Node (_, children) -> 1 + List.fold_left max 0 (List.map depth children)
  (* should find the max depth of a tree by checking if the chidlren have children and adding one if it does *)
  (* do plus one to account for current node *)
    
let rec is_leaf_perfect (t : 'a gt) : bool =
  let rec help (t: 'a gt) (dep: int) : bool =
    match t with
    (* checking if current node is a leaf *)
    | Node (_, []) -> true
    (* if not call depth on children *)
    | Node (_, children) ->
    let d = dep + 1 in
    List.for_all (fun c -> help c d) children
    && List.length (List.map depth children) = 1
    in
    help t 1

(* visits root, left, right,  extract the d and ch and recursivly call preorder on children *)
let rec preorder (Node(d,ch)) : int list =
  d :: List.concat(List.map preorder ch)


let rec mirror (Node(d,ch)) : 'a gt =
  Node(d, List.map mirror @@ List.rev ch)
let rec map f (Node(d,ch)) : 'a gt =
  Node(f d, List.map (map f) ch)
  
let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node(d,ch)) ->
  f d @@ List.map (fold f) ch

let sum t =
  fold (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

let mem t e = 
  fold (fun i rs -> i=e || List.exists (fun i -> i) rs) t

let rec mirror' t : 'a gt = 
  fold (fun d ch -> Node(d, List.rev ch)) t

let rec degree t : int = 
  match t with
  | Node(_,[]) -> 0
  | Node(_,ch) -> let tt = (List.map degree ch) in
  let curr_node = fold (fun _ ch -> List.fold_left (fun x y -> if x > y then x else y) 0 tt) t in
  if curr_node > (List.length ch)
  then curr_node
  else List.length ch
  


