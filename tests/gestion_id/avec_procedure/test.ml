open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let path = "../../../../../tests/gestion_id/avec_procedure/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* Test 1 : procédure sans paramètre *)
let%test_unit "testUnitaireProcedure1" =
  let _ = compiler (path ^ "testUnitaire1.rat") in ()

(* Test 2 : procédure avec paramètres *)
let%test_unit "testUnitaireProcedure2" = 
  let _ = compiler (path ^ "testUnitaire2.rat") in ()

(* Test 3 : procédure avec return de fonction *)
let%test_unit "testUnitaireProcedure3" = 
  try 
    let _ = compiler (path ^ "testUnitaire3.rat") in 
    raise ErreurNonDetectee
  with
  | RetourNonVideDansProcedure -> ()

(* Test 4 : appel de fonction comme procedure *)
let%test_unit "testUnitaireProcedure4" =
  try 
    let _ = compiler (path ^ "testUnitaire4.rat") in 
    raise ErreurNonDetectee
  with
  | AppelFonctionPourProcedure("f") -> ()

(* Test 5 : procédure non déclaré *)
let%test_unit "testUnitaireProcedure5" =
  try 
    let _ = compiler (path ^ "testUnitaire5.rat") in 
    raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("p") -> ()

(* Test 6 : double déclaration de procédure *)
let%test_unit "testUnitaireProcedure6" = 
  try 
    let _ = compiler (path ^ "testUnitaire6.rat") in 
    raise ErreurNonDetectee
  with
  | DoubleDeclaration("p") -> ()

  (*
let%test_unit "all_tam" =
  let p_tam = "../../../../../tests/tam/avec_procedure/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam
*)