open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let path = "../../../../../tests/type/avec_enum/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* Test 1 : déclaration et affectation simple *)
let%test_unit "testEnumType1" =
  let _ = compiler (path ^ "testUnitaire1.rat") in ()

(* Test 2 : Affectation entre valeurs du même enuml *)
let%test_unit "testEnumType2" =
  let _ = compiler (path ^ "testUnitaire2.rat") in ()

(* Test 3 : égalité entre valeurs d'énumérations *)
let%test_unit "testEnumType3" =
  let _ = compiler (path ^ "testUnitaire3.rat") in ()

(* Test 4 : égalité entre enums différents *)
let%test_unit "testEnumType4"= 
  try 
    let _ = compiler (path^"testUnitaire4.rat")
    in raise ErreurNonDetectee
  with
  | TypeBinaireInattendu(Equ,Tid("Couleur"),Tid("Taille")) -> ()

(* Test 5 : affectation enum -> int *)
let%test_unit "testEnumType5"= 
  try 
    let _ = compiler (path^"testUnitaire5.rat")
    in raise ErreurNonDetectee
  with
  | TypeInattendu(Tid("Couleur"),Int) -> ()

(* Test 6 : affectation int -> enum *)
let%test_unit "testEnumType6"= 
  try 
    let _ = compiler (path^"testUnitaire6.rat")
    in raise ErreurNonDetectee
  with
  | TypeInattendu(Int,Tid("Couleur")) -> ()

(* Test 7 : opération arithmétique sur enum *)
let%test_unit "testEnumType7"= 
  try 
    let _ = compiler (path^"testUnitaire7.rat")
    in raise ErreurNonDetectee
  with
  | TypeBinaireInattendu(Plus,Tid("Couleur"),Int)  -> ()

(* Test 8 : comparaison enum int *)
let%test_unit "testEnumType8"= 
  try 
    let _ = compiler (path^"testUnitaire8.rat")
    in raise ErreurNonDetectee
  with
  | TypeBinaireInattendu(Equ,Tid("Couleur"),Int)  -> ()