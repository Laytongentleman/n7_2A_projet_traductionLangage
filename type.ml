type typ = Bool | Int | Rat | Undefined | Void | Pointeur of typ | Tid of string

let rec string_of_type t = 
  match t with
  | Bool ->  "Bool"
  | Int  ->  "Int"
  | Rat  ->  "Rat"
  | Void -> "Void"
  | Tid n -> n
  | Pointeur t -> string_of_type t ^ "*"
  | Undefined -> "Undefined"


let est_compatible t1 t2 =
  match t1, t2 with
  | Bool, Bool -> true
  | Int, Int -> true
  | Rat, Rat -> true
  | Tid n1, Tid n2 -> n1 = n2 (* Permet des intructions tel que Couleur c = Bleu *)
  | _ -> false 

let%test _ = est_compatible Bool Bool
let%test _ = est_compatible Int Int
let%test _ = est_compatible Rat Rat
let%test _ = not (est_compatible Int Bool)
let%test _ = not (est_compatible Bool Int)
let%test _ = not (est_compatible Int Rat)
let%test _ = not (est_compatible Rat Int)
let%test _ = not (est_compatible Bool Rat)
let%test _ = not (est_compatible Rat Bool)
let%test _ = not (est_compatible Undefined Int)
let%test _ = not (est_compatible Int Undefined)
let%test _ = not (est_compatible Rat Undefined)
let%test _ = not (est_compatible Bool Undefined)
let%test _ = not (est_compatible Undefined Int)
let%test _ = not (est_compatible Undefined Rat)
let%test _ = not (est_compatible Undefined Bool)
let%test _ = not (est_compatible Void (Tid "Long"))
let%test _ = not (est_compatible (Tid "Jour") (Tid "Couleur"))

let est_compatible_list lt1 lt2 =
  try
    List.for_all2 est_compatible lt1 lt2
  with Invalid_argument _ -> false

let%test _ = est_compatible_list [] []
let%test _ = est_compatible_list [Int ; Rat] [Int ; Rat]
let%test _ = est_compatible_list [Bool ; Rat ; Bool] [Bool ; Rat ; Bool]
let%test _ = not (est_compatible_list [Int] [Int ; Rat])
let%test _ = not (est_compatible_list [Int] [Rat ; Int])
let%test _ = not (est_compatible_list [Int ; Rat] [Rat ; Int])
let%test _ = not (est_compatible_list [Bool ; Rat ; Bool] [Bool ; Rat ; Bool ; Int])
(* Tests d'échec sur les noms de Tid *)
let%test _ = not (est_compatible_list [Tid "Jour"] [Tid "Couleur"])
let%test _ = not (est_compatible_list [Int ; Tid "Jour"] [Int ; Tid "Mois"])
(* Tests d'échec sur les Pointeurs *)
let%test _ = not (est_compatible_list [Pointeur Int] [Int])
let%test _ = not (est_compatible_list [Pointeur Int] [Pointeur Bool])
let%test _ = not (est_compatible_list [Pointeur (Tid "Jour")] [Pointeur (Tid "Mois")])
(* Tests de succès avec Tid et Pointeurs *)
let%test _ = est_compatible_list [Tid "Couleur"] [Tid "Couleur"]
let%test _ = est_compatible_list [Int ; Tid "Jour"] [Int ; Tid "Jour"]
let%test _ = est_compatible_list [Pointeur Int ; Rat] [Pointeur Int ; Rat]
let%test _ = est_compatible_list [Pointeur (Tid "Jour")] [Pointeur (Tid "Jour")]

let getTaille t =
  match t with
  | Int -> 1
  | Bool -> 1
  | Rat -> 2
  | Undefined -> 0
  | Pointeur _ -> 1
  | Void -> 0
  | Tid _ -> 1
  
let%test _ = getTaille Int = 1
let%test _ = getTaille Bool = 1
let%test _ = getTaille Rat = 2
let%test _ = getTaille (Pointeur Int) = 1
let%test _ = getTaille (Pointeur Bool) = 1
let%test _ = getTaille (Pointeur Rat) = 1
let%test _ = getTaille (Pointeur (Pointeur Int)) = 1
let%test _ = getTaille Void = 0
let%test _ = getTaille (Tid "Jour") = 1
let%test _ = getTaille (Pointeur (Tid "Jour")) = 1

