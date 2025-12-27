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

let pathFichiersRat = "../../../../../tests/placement/avec_pointeur/fichiersRat/"

(* Tests *)

(* Test 1 : fonctionnement de base pointeur *)
let%test "testPlacementP1_1" =
  test (pathFichiersRat ^ "testUnitaire1.rat") "main" ("x",1) (0,"SB")

let%test "testPlacementP1_2" =
  test (pathFichiersRat ^ "testUnitaire1.rat") "main" ("y",1) (1,"SB")

(* Test 2 : fonctionnement de base avec if *)
let%test "testPlacementP2_1" =
  test (pathFichiersRat ^ "testUnitaire2.rat") "main" ("x", 1) (0, "SB")

let%test "testPlacementP2_2" =
  test (pathFichiersRat ^ "testUnitaire2.rat") "main" ("y", 1) (1, "SB")

let%test "testPlacementP2_3" =
  test (pathFichiersRat ^ "testUnitaire2.rat") "main" ("z", 1) (3, "SB")

let%test "testPlacementP2_4" =
  test (pathFichiersRat ^ "testUnitaire2.rat") "main" ("y", 2) (4, "SB")

let%test "testPlacementP2_5" =
  test (pathFichiersRat ^ "testUnitaire2.rat") "main" ("z", 2) (5, "SB")

let%test "testPlacementP2_6" =
  test (pathFichiersRat ^ "testUnitaire2.rat") "main" ("a", 1) (6, "SB")

let%test "testPlacementP2_7" =
  test (pathFichiersRat ^ "testUnitaire2.rat") "main" ("c", 1) (4, "SB")

let%test "testPlacementP2_8" =
  test (pathFichiersRat ^ "testUnitaire2.rat") "main" ("a", 2) (4, "SB")

(* Test 3 : pointeur avec une fonction *)
let%test "testPlacementP3_1" =
  test (pathFichiersRat ^ "testUnitaire3.rat") "main" ("a",1) (0,"SB")

let%test "testPlacementP3_2" =
  test (pathFichiersRat ^ "testUnitaire3.rat") "main" ("res",1) (1,"SB")

let%test "testPlacementP3_3" =
  test (pathFichiersRat ^ "testUnitaire3.rat") "f" ("c",1) (3,"LB")

let%test "testPlacementP3_4" =
  test (pathFichiersRat ^ "testUnitaire3.rat") "f" ("x",1) (-1,"LB")