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

let pathFichiersRat = "../../../../../tests/tam/avec_procedure/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* requires ppx_expect in jbuild, and `opam install ppx_expect` *)

(* Test 1 : procédure avec 1 paramètre *)
let%expect_test "testProcedure1" =
  runtam (pathFichiersRat ^ "testUnitaire1.rat");
  [%expect{| 6 |}]

(* Test 2 : procédure avec 2 paramètres *)
let%expect_test "testProcedure2" =
  runtam (pathFichiersRat ^ "testUnitaire2.rat");
  [%expect{| 6 |}]

(* Test 3 : test sujet *)
let%expect_test "testProcedure3" =
  runtam (pathFichiersRat ^ "testUnitaire3.rat");
  [%expect{| [1/2][3/4][5/4] |}]