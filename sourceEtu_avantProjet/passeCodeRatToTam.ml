(* Module de la passe de génération du code *)
(* doit être conforme à l'interface Passe *)
open Tds
open Ast.AstType
open Ast.AstPlacement
open Type
open Code
open Tam

type t1 = Ast.AstPlacement.programme
type t2 = string


(* analyse_code_expression : AstPlacement.expression -> string *)
(* Paramètre : l'expression à analyser *)
(* Génère le code TAM (string) associé à l'expression *)
(* InternalError si erreur dans les passes précédentes *)
let rec analyse_code_expression e =
  match e with
  | AppelFonction(info_ast,le) ->
    let c = String.concat "" (List.map analyse_code_expression le) in
      (match info_ast_to_info info_ast with
      | InfoFun(name,_,_) -> c ^ (call "ST" name)
      | _ -> failwith "InternalError")
  
  | Ident info_ast ->
    (match info_ast_to_info info_ast with
    | InfoVar(_,t,dep,reg) -> load (getTaille t) dep reg
    | InfoConst(_,i) -> loadl_int i
    | _ -> failwith "InternalError")

  | Booleen b ->
    if b then loadl_int 1
         else loadl_int 0

  | Entier i -> loadl_int i

  | Unaire(op,e) ->
    let c = analyse_code_expression e in
      c ^ (match op with
      | Numerateur -> pop 0 1
      | Denominateur -> pop 1 1)

  | Binaire(op,e1,e2) ->
    let c1 = analyse_code_expression e1 in
      let c2 = analyse_code_expression e2 in
        c1 ^ c2 ^ (match op with
        | Fraction -> call "ST" "norm"
        | PlusInt -> subr "IAdd"
        | PlusRat -> call "ST" "RAdd"
        | MultInt -> subr "IMul"
        | MultRat -> call "ST" "RMul"
        | EquInt -> subr "IEq"
        | EquBool -> subr "IEq"
        | Inf -> subr "ILss")


(* analyse_code_instruction : AstPlacement.instruction -> string *)
(* Paramètre : l'instruction à analyser *)
(* Génère le code TAM (string) associé à l'instruction *)
(* InternalError si erreur dans les passes précédentes *)
let rec analyse_code_instruction i =
  match i with
  | Declaration(info_ast,e) ->
    (match info_ast_to_info info_ast with
    | InfoVar(_,t,d,r) ->
      push (getTaille t) ^ (analyse_code_expression e) ^ (store (getTaille t) d r)
    | _ -> failwith "InternalError")
  
  | Affectation(info_ast,e) ->
    (match info_ast_to_info info_ast with
    | InfoVar(_,t,d,r) ->
      (analyse_code_expression e) ^ (store (getTaille t) d r)
    | _ -> failwith "InternalError")
  
  | AffichageInt e ->
    let ne = analyse_code_expression e in
      ne ^ subr "IOut"

  | AffichageRat r ->
    let nr = analyse_code_expression r in
      nr ^ call "ST" "ROut"

  | AffichageBool b ->
    let nb = analyse_code_expression b in
      nb ^ subr "BOut"

  | Conditionnelle(c,b1,b2) ->
    let nc = analyse_code_expression c in
      let els = getEtiquette() in
        let endif = getEtiquette() in
          let nb1 = analyse_code_bloc b1 in
            let nb2 = analyse_code_bloc b2 in
              nc
              ^ (jumpif 0 els)
              ^ nb1
              ^ (jump endif)
              ^ (label els)
              ^ nb2
              ^ (label endif)

  | TantQue(c,b) ->
    let nc = analyse_code_expression c in
      let whil = getEtiquette() in
        let endwhil = getEtiquette() in
          let nb = analyse_code_bloc b in
            (label whil)
            ^ nc
            ^ (jumpif 0 endwhil)
            ^ nb
            ^ (jump whil)
            ^ (label endwhil)

  | Retour(e,tailleRet,tailleParams) ->
    let ne = analyse_code_expression e in
      ne ^ return tailleRet tailleParams

  | Empty -> ""

(* analyse_code_bloc : AstPlacement.bloc -> string *)
(* Paramètre : liste d'instructions à analyser, avec la taille du bloc associé *)
(* Génère le code TAM (string) associé à la liste d'instructions *)
(* InternalError si erreur dans les passes précédentes *)
and analyse_code_bloc (li,tailleBloc) =
  let c = String.concat "" (List.map analyse_code_instruction li) in
    c ^ (pop 0 tailleBloc)


(* analyse_code_fonction : AstPlacement.fonction -> string *)
(* Paramètre : la fonction à analyser *)
(* Génère le code TAM (string) associé à la fonction *)
(* InternalError si erreur dans les passes précédentes *)
let analyse_code_fonction (Fonction (info_ast,_,li)) =
  let nf = match info_ast_to_info info_ast with
           | InfoFun(n,_,_) -> n
           | _ -> failwith "InternalError" in
  label nf
  ^ analyse_code_bloc li
  ^ halt


(* analyser : AstPlacement.programme -> string *)
(* Paramètre : le programme à analyser *)
(* Génère le code TAM (string) associé au programme *)
(* InternalError si erreur dans les passes précédentes *)
let analyser (Programme (fonctions,prog)) =
  let enTete = getEntete() in
    let fct = String.concat "" (List.map analyse_code_fonction fonctions) in
      let progs = analyse_code_bloc prog in
        enTete
        ^ fct
        ^ label "main"
        ^ progs
        ^ halt