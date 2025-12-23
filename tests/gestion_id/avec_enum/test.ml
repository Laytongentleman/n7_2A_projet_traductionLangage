open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/gestion_id/avec_enum/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* Test 1 : déclaration simple d'un enum *)
let%test_unit "testUnitaireEnum1" = 
  let _ = compiler (pathFichiersRat^"testUnitaire1.rat") in ()

(* Test 2 : double déclaration de valeur *)
let%test_unit "testUnitaireEnum2" =
  try 
    let _ = compiler (pathFichiersRat ^ "testUnitaire2.rat") in 
    raise ErreurNonDetectee
  with
  | DoubleDeclaration("Lundi") -> ()

(* Test 3 : double déclaration d'énum *)
let%test_unit "testUnitaireEnum3" =
  try 
    let _ = compiler (pathFichiersRat ^ "testUnitaire3.rat") in 
    raise ErreurNonDetectee
  with
  | DoubleDeclaration("Etat") -> ()

(* Test 4 : utilisation d'une valeur d'énum inexistante *)
let%test_unit "testUnitaireEnum4" =
  try 
    let _ = compiler (pathFichiersRat ^ "testUnitaire4.rat") in 
    raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("Jaune") -> ()

(* Test 5 : accès correct à une valeur énum *)
let%test_unit "testUnitaireEnum5" = 
  let _ = compiler (pathFichiersRat^"testUnitaire5.rat") in ()

(* Test 6 : deux valeurs identiques dans deux énumérations *)
let%test_unit "testUnitaireEnum6" =
  try
    let _ = compiler (pathFichiersRat ^ "testUnitaire6.rat") in 
    raise ErreurNonDetectee
  with 
  | DoubleDeclaration("Ouvert") -> ()


(*
let%test_unit "all_tam" =
  let p_tam = "../../../../../tests/tam/avec_reference/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam
*)
