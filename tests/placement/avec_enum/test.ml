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

    (* Affichage debug *)
    (*print_endline "[DEBUG] Liste des adresses :";
    List.iter (fun (fonction, vars) ->
      Printf.printf "Fonction %s :\n" fonction;
      List.iter (fun (v, (d, r)) ->
        Printf.printf "  Variable %s -> (%d, %s)\n" v d r
      ) vars
    ) listeAdresses; *)

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

let pathFichiersRat = "../../../../../tests/placement/avec_enum/fichiersRat/"

(* Tests *)

let%test "testPlacementE1_1" =
  test (pathFichiersRat ^ "testUnitaire1.rat") "main" ("c",1) (5,"SB")

let%test "testPlacementE1_2" =
  test (pathFichiersRat ^ "testUnitaire1.rat") "main" ("j",1) (6,"SB")

let%test "testPlacementE1_3" =
  test (pathFichiersRat ^"testUnitaire1.rat") "Couleur" ("Rouge",1) (0,"SB")

let%test "testPlacementE1_4" =
  test (pathFichiersRat ^"testUnitaire1.rat") "Couleur" ("Vert",1) (1,"SB")

let%test "testPlacementE1_5" =
  test (pathFichiersRat ^"testUnitaire1.rat") "Couleur" ("Bleu",1) (2,"SB")

let%test "testPlacementE1_6" =
  test (pathFichiersRat ^"testUnitaire1.rat") "Jour" ("Lundi",1) (3,"SB")

let%test "testPlacementE1_7" =
  test (pathFichiersRat ^"testUnitaire1.rat") "Jour" ("Mardi",1) (4,"SB")