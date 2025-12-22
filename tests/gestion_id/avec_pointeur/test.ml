open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let path = "../../../../../tests/gestion_id/avec_pointeur/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* Test 1 : utilisation correcte d’un pointeur *)
let%test_unit "testPointeur1" =
  let _ = compiler (path ^ "testPointeur1.rat") in ()

(* Test 2 : affectation via un pointeur *)
let%test_unit "testPointeur2" =
  let _ = compiler (path ^ "testPointeur2.rat") in ()

(* Test 3 : pointeur de pointeur *)
let%test_unit "testPointeur3" =
  let _ = compiler (path ^ "testPointeur3.rat") in ()

(* Test 4 : allocation via new *)
let%test_unit "testPointeur4" =
  let _ = compiler (path ^ "testPointeur4.rat") in ()

(* Test 5 : assignation de null *)
let%test_unit "testPointeur5" =
  let _ = compiler (path ^ "testPointeur5.rat") in ()

(* Test 6 : déréférencements imbriqués *)
let%test_unit "testPointeur6" =
  let _ = compiler (path ^ "testPointeur6.rat") in ()

(* variable non déclarée *)
let%test_unit "testPointeurE1" =
  try
    let _ = compiler (path ^ "testPointeur7.rat") in
    raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("x") -> ()

(* adresse d’une constante *)
let%test_unit "testPointeurE2" =
  try
    let _ = compiler (path ^ "testPointeur8.rat") in
    raise ErreurNonDetectee
  with
  | MauvaiseUtilisationIdentifiant("c") -> ()



(* Fichiers de tests de la génération de code -> doivent passer la TDS *)
open Unix
open Filename

let rec test d p_tam = 
  try 
    let file = readdir d in
    if (check_suffix file ".rat") 
    then
    (
     try
       let _ = compiler  (p_tam^file) in (); 
     with e -> print_string (p_tam^file); print_newline(); raise e;
    )
    else ();
    test d p_tam
  with End_of_file -> ()
(*
let%test_unit "all_tam" =
  let p_tam = "../../../../../tests/tam/avec_pointeur/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam
*)