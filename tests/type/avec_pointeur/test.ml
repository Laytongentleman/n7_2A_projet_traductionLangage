open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let path = "../../../../../tests/type/avec_pointeur/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* Test 1 : utilisation correcte d’un pointeur *)
let%test_unit "testPointeur1" =
  let _ = compiler (path ^ "testUnitaire1.rat") in ()

(* Test 2 : affectation de null *)
let%test_unit "testPointeur2" =
  let _ = compiler (path ^ "testUnitaire2.rat") in ()

(* Test 3 : affection de pointeur -> pointeur *)
let%test_unit "testPointeur3" =
  let _ = compiler (path ^ "testUnitaire3.rat") in ()

(* Test 4 : déréférencement correct *)
let%test_unit "testPointeur4" =
  let _ = compiler (path ^ "testUnitaire4.rat") in ()

(* Test 5 : écriture via pointeur *)
let%test_unit "testPointeur5" =
  let _ = compiler (path ^ "testUnitaire5.rat") in ()

(* Test 6 : déréférencement non pointeur *)
let%test_unit "testPointeur6" = 
  try 
    let _ = compiler (path ^ "testUnitaire6.rat") in
    raise ErreurNonDetectee 
  with
  | DereferencementIllegal -> ()

(* Test 7 : affectation d'un pointeur vers un non-pointeur *)
let%test_unit "testPointeur7" = 
  try 
    let _ = compiler (path ^ "testUnitaire7.rat") in
    raise ErreurNonDetectee 
  with
  | TypeInattendu(Int,Pointeur(Int)) -> ()

(* Test 8 : affectation non compatible de pointeurs *)
let%test_unit "testPointeur8" = 
  try 
    let _ = compiler (path ^ "testUnitaire8.rat") in
    raise ErreurNonDetectee 
  with
  | TypeInattendu(Pointeur(Int),Pointeur(Bool)) -> () 

(* Test 9 : test de robustesse null comparé à un pointeur *)
let%test_unit "testPointeurRobustesse9" = 
  try 
    let _ = compiler (path ^ "testUnitaire9.rat") in
    raise ErreurNonDetectee 
  with
  | TypeBinaireInattendu(Equ, Pointeur(Int), Pointeur(Undefined)) -> () 