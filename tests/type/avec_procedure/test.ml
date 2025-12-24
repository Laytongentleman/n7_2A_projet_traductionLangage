open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let path = "../../../../../tests/type/avec_procedure/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* Test 1 : procédure simple sans paramètre *)
let%test_unit "testProcedureType1" =
  let _ = compiler (path ^ "testUnitaire1.rat") in ()

(* Test 2 : procédure avec un paramètre *)
let%test_unit "testProcedureType2" =
  let _ = compiler (path ^ "testUnitaire2.rat") in ()

(* Test 3 : procédure avec plusieurs paramètres *)
let%test_unit "testProcedureType3" =
  let _ = compiler (path ^ "testUnitaire3.rat") in ()

(* Test 4 : appel correct d'une procédure *)
let%test_unit "testProcedureType4" =
  let _ = compiler (path ^ "testUnitaire4.rat") in ()

(* Test 5 : appel incorrect trop de paramètres *)
let%test_unit "testProcedureType5" = 
  try 
    let _ = compiler (path ^ "testUnitaire5.rat") in
    raise ErreurNonDetectee 
  with
  | TypesParametresInattendus _ -> ()
