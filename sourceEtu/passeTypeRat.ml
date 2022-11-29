(* Module de la passe de typage *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
open Type

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme


let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t,info_ast,e) ->
    let (ne,nt) = analyse_type_expression e in
      if est_compatible t nt then
        match info_ast_to_info info_ast with
        | InfoVar (n,Undefined, 0, "") -> 
          let info_ast_final = info_to_info_ast (InfoVar(n,nt,0,"")) in
            AstType.Declaration(info_ast_final,ne)
        | _ -> failwith "InternalError"
      else
        raise (TypeInattendu(nt,t))
  | AstTds.Affectation (info_ast,e) ->
    let (ne,t) = analyse_type_expression e in
      let nt = (match info_ast_to_info info_ast with
                | InfoVar (_,nt,_,_) -> nt
                | InfoFun (_,nt,_) -> nt
                | _ -> failwith "InternalError") in
      if est_compatible t nt then
        AstType.Affectation (info_ast,ne)
      else
        raise (TypeInattendu(nt,t))
  | 


(* analyser : AstTds.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.programme *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  let nf = List.map (analyse_type_fonction tds) fonctions in
  let nb = analyse_type_bloc tds None prog in
  AstType.Programme (nf,nb)