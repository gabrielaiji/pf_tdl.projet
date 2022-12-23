open Rat
open Compilateur
(*open Exceptions*)

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/ast_syntaxe/sans_fonction/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "testElseOptionnel1" = 
  let _ = compiler (pathFichiersRat^"testElseOptionnel1.rat") in ()

let%test_unit "testElseOptionnel2" = 
  let _ = compiler (pathFichiersRat^"testElseOptionnel2.rat") in ()

let%test_unit "testTernaire1" = 
  let _ = compiler (pathFichiersRat^"testTernaire1.rat") in ()

let%test_unit "testLoop1" = 
  let _ = compiler (pathFichiersRat^"testLoop1.rat") in ()


let%test_unit "testLoop3" = 
  let _ = compiler (pathFichiersRat^"testLoop3.rat") in ()

let%test_unit "testLoop4" = 
  let _ = compiler (pathFichiersRat^"testLoop4.rat") in ()

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
  let p_tam = "../../../../../tests/tam/sans_fonction/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam
