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

let pathFichiersRat = "../../../../../tests/tam/sans_fonction/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* requires ppx_expect in jbuild, and `opam install ppx_expect` *)

let%expect_test "testprintint" =
  runtam (pathFichiersRat^"testprintint.rat");
  [%expect{| 42 |}]

let%expect_test "testprintbool" =
  runtam (pathFichiersRat^"testprintbool.rat");
  [%expect{| true |}]

let%expect_test "testprintrat" =
   runtam (pathFichiersRat^"testprintrat.rat");
   [%expect{| [4/5] |}]

let%expect_test "testaddint" =
  runtam (pathFichiersRat^"testaddint.rat");
  [%expect{| 42 |}]

let%expect_test "testaddrat" =
  runtam (pathFichiersRat^"testaddrat.rat");
  [%expect{| [7/6] |}]

let%expect_test "testmultint" =
  runtam (pathFichiersRat^"testmultint.rat");
  [%expect{| 440 |}]

let%expect_test "testmultrat" =
  runtam (pathFichiersRat^"testmultrat.rat");
  [%expect{| [14/3] |}]

let%expect_test "testnum" =
  runtam (pathFichiersRat^"testnum.rat");
  [%expect{| 4 |}]

let%expect_test "testdenom" =
  runtam (pathFichiersRat^"testdenom.rat");
  [%expect{| 7 |}]

let%expect_test "testwhile1" =
  runtam (pathFichiersRat^"testwhile1.rat");
  [%expect{| 19 |}]

let%expect_test "testif1" =
  runtam (pathFichiersRat^"testif1.rat");
  [%expect{| 18 |}]

let%expect_test "testif2" =
  runtam (pathFichiersRat^"testif2.rat");
  [%expect{| 21 |}]

let%expect_test "factiter" =
  runtam (pathFichiersRat^"factiter.rat");
  [%expect{| 120 |}]

let%expect_test "complique" =
  runtam (pathFichiersRat^"complique.rat");
  [%expect{| [9/4][27/14][27/16][3/2] |}]

let%expect_test "testTernaire1" =
  runtam (pathFichiersRat^"testTernaire1.rat");
  [%expect{| 2 |}]

let%expect_test "testLoop1" =
  runtam (pathFichiersRat^"testLoop1.rat");
  [%expect{| 1266645 |}]

let%expect_test "testLoop2" =
  runtam (pathFichiersRat^"testLoop2.rat");
  [%expect{| 00010203101112132021222330313233 |}]

let%expect_test "testLoop3" =
  runtam (pathFichiersRat^"testLoop3.rat");
  [%expect{| 0123456789101112131415161718192021222301234567891011121314151617181920212223 |}]

let%expect_test "testLoop4" =
  runtam (pathFichiersRat^"testLoop4.rat");
  [%expect{| 012345678910 |}]

let%expect_test "testLoop8" =
  runtam (pathFichiersRat^"testLoop8.rat");
  [%expect{| 01234567891011121314151617181920212223 |}]

let%expect_test "testLoop9" =
  runtam (pathFichiersRat^"testLoop9.rat");
  [%expect{| 0001020310111213 |}]

let%expect_test "testPointeur1" =
  runtam (pathFichiersRat^"testPointeur1.rat");
  [%expect{| 423 |}]

let%expect_test "testElseOptionnel2" =
  runtam (pathFichiersRat^"testElseOptionnel2.rat");
  [%expect{| 0246810 |}]

let%expect_test "testConstruBonus" =
  runtam (pathFichiersRat^"testConstruBonus.rat");
  [%expect{| 11[22/15] |}]
