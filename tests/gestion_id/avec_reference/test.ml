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
  | DoubleDeclaration("a") -> ()

(* Test 6 : Appel d'une procédure avec ref *)
let%test_unit "testUnitaireRef6" = 
  let _ = compiler (pathFichiersRat^"testUnitaire6.rat") in ()

(* Test 7 : Appel d'une procédure avec ref non déclaré*)
let%test_unit "testUnitaireRef7" = 
  try 
    let _ = compiler (pathFichiersRat^"testUnitaire7.rat") in 
    raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare _ -> ()

(* Tests qui passe la tds mais pas le typage *)
(* Test 8 : Appel d'une fonction en oubliant des paramètres *)
let%test_unit "testUnitaireRef8" =
    let _ = compiler (pathFichiersRat ^"testUnitaire8.rat") in ()

(* Test 9 : Appel d'une fonction sans mettre ref *)
let%test_unit "testUnitaireRef9" =
    let _ = compiler (pathFichiersRat ^"testUnitaire9.rat") in ()

(* Test 10 : Appel d'une fonction en mettant ref alors qu'il faut pas *)
let%test_unit "testUnitaireRef10" =
    let _ = compiler (pathFichiersRat ^"testUnitaire10.rat") in () 


(*
let%test_unit "all_tam" =
  let p_tam = "../../../../../tests/tam/avec_reference/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam
*)
