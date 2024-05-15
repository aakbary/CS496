(*
       Quiz 2 - Fruit Basket Processors
  
       8 Feb 2023
       Name 1: Amena Akbary
       Name 2: Yousaf Rajput
       Pledge: I pledge my Honor that I have abided by the Stevens Honor System.
*)
type 'a result = Ok of 'a | Error
type fruit = A | O | K
type 'a basket = Empty | Add of 'a*'a basket
(* Sample fruit baskets *)
let fb1 : fruit basket = Add(A,Add(A,Add(O,Add(A,Add(K,Add(K,Empty))))))
let fb2 : fruit basket = Add(A,Add(A,Add(A,Add(A,Empty))))
(* 
   A fruit basket processor is any expression whose type is:
      fruit basket -> t result
   for any type t. Some examples of types that have this form are: 
   Eg. fruit basket -> int result
   Eg. fruit basket -> bool result
   Eg. fruit basket -> (fruit basket) result
   A fruit basket processor analyzes a fruit basket and can:
   1. Either, fail (Error)
   2. Or, succeed (Ok v, where v the result of processing it)
*)
(* 
   Implement the following fruit basket processors.
   NOTE: You are free to add the "rec" keyword just after the "let", if needed.
 *)
    
(** [no_of_oranges fb] fruit basket processor that returns the number of oranges in
the fruit basket [fb].
    Eg. no_of_oranges fb1 => Ok 1
*)

let rm_opt : int result -> int =
  fun x ->
  match x with
  | Ok x -> x
  | _ -> 0

let rec no_of_oranges : fruit basket -> int result =
  fun fb ->
  match fb with
  | Empty ->  Ok 0
  | Add(O, b) -> Ok (1 + rm_opt(no_of_oranges b))
  | Add(_, b) -> no_of_oranges b
(** [has_apples fb] fruit basket processor that determines whether there are apples
in [fb] 
    Eg. has_apples fb1 => Ok true *)
let rec has_apples : fruit basket -> bool result =
  fun fb ->
  match fb with
  | Empty -> Ok false
  | Add(A, b) -> Ok true
  | Add(_, b) -> has_apples b
    
(** [apples_to_oranges_ratio fb] fruit basket processor that returns
    the quotient between the number of apples and the number of
    oranges in [fb].
    It should return [Error] if there are no oranges. 
    Eg. apples_to_oranges_ratio fb1 => Ok 3
        apples_to_oranges_ratio fb2 => Error
*)
let rec no_of_apples : fruit basket -> int result =
  fun fb ->
  match fb with
  | Empty ->  Ok 0
  | Add(A, b) -> Ok (1 + rm_opt(no_of_apples b))
  | Add(_, b) -> no_of_apples b

let rec has_oranges : fruit basket -> bool result =
  fun fb ->
  match fb with
  | Empty -> Ok false
  | Add(O, b) -> Ok true
  | Add(_, b) -> has_oranges b

let apples_to_oranges_ratio : fruit basket -> int result =
  fun fb ->
  if (has_oranges fb) = Ok false 
  then Error
  else Ok (rm_opt (no_of_apples fb) / rm_opt (no_of_oranges fb))

(** [apples_to_kiwis_ratio fb] fruit basket processor that returns
    the quotient between the number of apples and the number of
    kiwis in [fb].
    It should return [Error] if there are no kiwis. 
    Eg. apples_to_kiwis_ratio fb1 => Ok 1
        apples_to_kiwis_ratio fb2 => Error
*)
let rec has_kiwi : fruit basket -> bool result =
  fun fb ->
  match fb with
  | Empty -> Ok false
  | Add(K, b) -> Ok true
  | Add(_, b) -> has_kiwi b

let rec no_of_kiwis : fruit basket -> int result =
  fun fb ->
  match fb with
  | Empty ->  Ok 0
  | Add(K, b) -> Ok (1 + rm_opt(no_of_kiwis b))
  | Add(_, b) -> no_of_kiwis b

let apples_to_kiwis_ratio : fruit basket -> int result =
  fun fb ->
  if (has_kiwi fb) = Ok false 
  then Error
  else Ok (rm_opt (no_of_apples fb) / rm_opt (no_of_kiwis fb))
(** [ratio_sum fb] fruit basket processor that returns the sum of the
    apples-to-oranges ratio and the apples-to-kiwis ration in [fb].
    IMPORTANT: YOU MUST USE [apples_to_oranges_ratio] AND
    [apples_to_kiwis_ratio] FROM ABOVE.
    Eg. ratio_sum fb1 => Ok 4
        ratio_sum fb2 => Error
*)
let ratio_sum : fruit basket -> int result =
  fun fb ->
  if apples_to_kiwis_ratio fb = Error
  then Error
  else if apples_to_oranges_ratio fb = Error
  then Error
  else Ok (rm_opt(apples_to_kiwis_ratio fb) + rm_opt(apples_to_oranges_ratio fb))
  