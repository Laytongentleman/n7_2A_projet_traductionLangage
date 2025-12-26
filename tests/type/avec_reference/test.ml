open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let path = "../../../../../tests/type/avec_reference/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* Test 1 : passage correct par référence *)
let%test_unit "testReferenceType1" =
  let _ = compiler (path ^ "testUnitaire1.rat") in ()

(* Test 2 : lecture via ref *)
let%test_unit "testReferenceType2" =
  let _ = compiler (path ^ "testUnitaire2.rat") in ()

(* Test 3 : ref sur enum *)
let%test_unit "testReferenceType3" =
  let _ = compiler (path ^ "testUnitaire3.rat") in ()

(* Test 4 : appel incorrect (bool/int) *)
let%test_unit "testReferenceType4" = 
  try 
    let _ = compiler (path ^ "testUnitaire4.rat") in
    raise ErreurNonDetectee 
  with
  | TypesParametresInattendus _ -> ()

(* Test 5 : retour incompatible via ref *)
let%test_unit "testReferenceType5" = 
  try 
    let _ = compiler (path ^ "testUnitaire5.rat") in
    raise ErreurNonDetectee 
  with
  | TypeInattendu _ -> ()

(* Test 6 : oubli de ref dans l'appel *)
let%test_unit "testReferenceType6" = 
  try 
    let _ = compiler (path ^ "testUnitaire6.rat") in
    raise ErreurNonDetectee 
  with
  | ParametreRefAttendu _ -> ()

(* Test 7 : paramètre ref non attendu *)
let%test_unit "testReferenceType7" = 
  try 
    let _ = compiler (path ^ "testUnitaire7.rat") in
    raise ErreurNonDetectee 
  with
  | ParametreNonRef _ -> ()