open Rat
open Compilateur

(* Changer le chemin d'accès du jar. *)
let runtamcmde = "java -jar ../../../../../tests/runtam.jar"
(* let runtamcmde = "java -jar /mnt/n7fs/.../tools/runtam/runtam.jar" *)

(* Execute the TAM code obtained from the rat file and return the ouptut of this code *)
let runtamcode cmde ratfile =
  let tamcode = compiler ratfile in
  let (tamfile, chan) = Filename.open_temp_file "test" ".tam" in
  output_string chan tamcode;
  close_out chan;
  let ic = Unix.open_process_in (cmde ^ " " ^ tamfile) in
  let printed = input_line ic in
  close_in ic;
  Sys.remove tamfile;    (* à commenter si on veut étudier le code TAM. *)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile =
  print_string (runtamcode runtamcmde ratfile)

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/tam/avec_pointeur/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* requires ppx_expect in jbuild, and `opam install ppx_expect` *)

(* Test 1 : fonctionnement de pointeur simple *)
let%expect_test "testpointeur1" =
  runtam (pathFichiersRat^"testUnitaire1.rat");
  [%expect{| 1 |}]

(* Test 2 : changement de la valeur pointée*)
let%expect_test "testpointeur2" =
  runtam (pathFichiersRat^"testUnitaire2.rat");
  [%expect{| 20 |}]

(* Test 3 : déréférencement multiple *)
let%expect_test "testpointeur3" =
  runtam (pathFichiersRat^"testUnitaire3.rat");
  [%expect{| 42 |}]

(* Test 4 : affectation indirect *)
let%expect_test "testpointeur4" =
  runtam (pathFichiersRat^"testUnitaire4.rat");
  [%expect{| 5 |}]

(* Test 5 : pointeur sur des rationnels *)
let%expect_test "testpointeur5" =
  runtam (pathFichiersRat^"testUnitaire5.rat");
  [%expect{| [5/2] |}]

(* Test 6 : pointeur avec fonction *)
let%expect_test "testpointeur6" =
  runtam (pathFichiersRat^"testUnitaire6.rat");
  [%expect{| 12 |}]