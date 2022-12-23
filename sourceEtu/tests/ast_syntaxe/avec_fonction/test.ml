open Rat
open Compilateur
(*open Exceptions*)

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/ast_syntaxe/avec_fonction/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "testElseOptionnel3" = 
  let _ = compiler (pathFichiersRat^"testElseOptionnel3.rat") in ()

let%test_unit "testElseOptionnel4" = 
  let _ = compiler (pathFichiersRat^"testElseOptionnel4.rat") in ()

let%test_unit "testTernaire2" = 
  let _ = compiler (pathFichiersRat^"testTernaire2.rat") in ()

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

let%test_unit "all_tam" =
  let p_tam = "../../../../../tests/tam/avec_fonction/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam