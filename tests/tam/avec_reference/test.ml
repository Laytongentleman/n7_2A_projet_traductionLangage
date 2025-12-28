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

let pathFichiersRat = "../../../../../tests/tam/avec_reference/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* Test 1 : programme du sujet 1 *)
let%expect_test "testRef1" =
  runtam (pathFichiersRat ^ "testUnitaire1.rat");
  [%expect{| 41 |}]

(* Test 2 : imbrication de ref sujet *)
let%expect_test "testRef2" =
  runtam (pathFichiersRat ^ "testUnitaire2.rat");
  [%expect{| 5556565758 |}]