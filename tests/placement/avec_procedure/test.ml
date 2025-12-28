open Rat
open Compilateur
open Passe

(* Return la liste des adresses des variables d'un programme RAT *)
let getListeDep ratfile =
  let input = open_in ratfile in
  let filebuf = Lexing.from_channel input in
  try
  let ast = Parser.main Lexer.token filebuf in
  let past = CompilateurRat.calculer_placement ast in
  let listeAdresses = VerifPlacement.analyser past in
  listeAdresses
  with
  | Lexer.Error _ as e ->
      report_error ratfile filebuf "lexical error (unexpected character).";
      raise e
  | Parser.Error as e->
      report_error ratfile filebuf "syntax error.";
      raise e

(* teste si dans le fichier fichier, dans la fonction fonction (main pour programme principal)
la occ occurence de la variable var a l'adresse dep[registre]
*)
let test fichier fonction (var,occ) (dep,registre) = 
  let l = getListeDep fichier in
  let lmain = List.assoc fonction l in
  let rec aux i lmain = 
    if i=1 
    then
      let (d,r) = List.assoc var lmain in
      (d=dep && r=registre)
    else 
      aux (i-1) (List.remove_assoc var lmain)
  in aux occ lmain

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/placement/avec_procedure/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* Test 1 : procedure sans paramètre *)

let%test "TestProcedureplacement1_1" =
  test (pathFichiersRat ^ "testUnitaire1.rat") "main" ("a",1) (0,"SB")

let%test "TestProcedureplacement1_2" =
  test (pathFichiersRat ^ "testUnitaire1.rat") "f" ("c",1) (3,"LB")

(* Test 2 : procedure avec paramètre *)
let%test "TestProcedureplacement2_1" =
  test (pathFichiersRat ^ "testUnitaire2.rat") "main" ("c",1) (0,"SB")

let%test "TestProcedureplacement2_2" =
  test (pathFichiersRat ^ "testUnitaire2.rat") "f" ("a",1) (-2,"LB")

let%test "TestProcedureplacement2_3" =
  test (pathFichiersRat ^ "testUnitaire2.rat") "f" ("b",1) (-1,"LB")