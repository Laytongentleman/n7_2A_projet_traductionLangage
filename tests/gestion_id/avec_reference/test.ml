open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/gestion_id/avec_reference/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* Test 1 : Paramètre ref correctement déclaré *)
let%test_unit "testUnitaireRef1" = 
  let _ = compiler (pathFichiersRat^"testUnitaire1.rat") in ()

(* Test 2 : Paramètre non ref *)
let%test_unit "testUnitaireRef2" = 
  let _ = compiler (pathFichiersRat^"testUnitaire2.rat") in ()

(* Test 3 : Adresse d'un paramètre ref *)
let%test_unit "testUnitaireRef3" = 
  let _ = compiler (pathFichiersRat^"testUnitaire3.rat") in ()

(* Test 4 : Mélange de ref et de non ref *)
let%test_unit "testUnitaireRef4" = 
  let _ = compiler (pathFichiersRat^"testUnitaire4.rat") in ()

(* Test 5 : Paramètre ref déjà déclaré localement *)
let%test_unit "testUnitaireRef5" = 
  try 
    let _ = compiler (pathFichiersRat^"testUnitaire5.rat") in 
    raise ErreurNonDetectee
  with
  | MauvaiseUtilisationIdentifiant("a") -> ()

(* Test 6 : Déclaration de ref sur une fonction *)
let%test_unit "testUnitaireRef6" = 
  try 
    let _ = compiler (pathFichiersRat^"testUnitaire6.rat") in 
    raise ErreurNonDetectee
  with
  | MauvaiseUtilisationIdentifiant("g") -> ()

(*
let%test_unit "all_tam" =
  let p_tam = "../../../../../tests/tam/avec_reference/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam
*)
