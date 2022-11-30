(* Module de la passe de placement mémoire *)
(* doit être conforme à l'interface Passe *)
open Tds
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme


let rec analyse_placement_instruction i depl reg =
  match i with
  | AstType.Declaration (info_ast,e) ->
    (match info_ast_to_info info_ast with
    | InfoVar (_,t,_,_) -> modifier_adresse_variable depl reg info_ast;(AstPlacement.Declaration (info_ast,e), getTaille t)
    | _ -> failwith "InternalError")
  
  | AstType.TantQue (c,b) ->
    let nb = analyse_placement_bloc b depl reg in
      (AstPlacement.TantQue (c,nb), 0)
  
  | AstType.Retour (e,ia) ->
    (match info_ast_to_info ia with
    | InfoFun (_,t,lp) -> (AstPlacement.Retour (e, getTaille t, List.fold_right (fun a r -> r + getTaille a) lp 0), 0)
    | _ -> failwith "InternalError")

  | AstType.Affectation (info_ast,e) -> (AstPlacement.Affectation (info_ast,e), 0)

  | AstType.AffichageInt e -> (AstPlacement.AffichageInt e, 0)

  | AstType.AffichageRat e -> (AstPlacement.AffichageRat e, 0)

  | AstType.AffichageBool e -> (AstPlacement.AffichageBool e, 0)

  | AstType.Conditionnelle (e,b1,b2) ->
    let nb1 = analyse_placement_bloc b1 depl reg in
      let nb2 = analyse_placement_bloc b2 depl reg in
        (AstPlacement.Conditionnelle (e, nb1, nb2), 0)

  | AstType.Empty -> (AstPlacement.Empty, 0)

and analyse_placement_bloc li depl reg =
  match li with
  | [] -> ([],0)
  | i::li_prime ->
    let (ni,taille_i) = analyse_placement_instruction i depl reg in
      let (nli,tli) = analyse_placement_bloc li_prime (depl + taille_i) reg in
        (ni::nli, taille_i + tli)


let analyse_placement_fonction (AstType.Fonction (info_ast,lp,li)) =
  let nli = analyse_placement_bloc li 3 "LB" in
    let rec aux cmpt rlp =
      (match rlp with
      | [] -> cmpt
      | iap::q ->
        let t =
          (match info_ast_to_info iap with
          | InfoVar (_,t,_,_) -> t
          | _ -> failwith "InternalError") in
          let new_cmpt = aux (cmpt - getTaille t) q in
            modifier_adresse_variable new_cmpt "LB" iap;cmpt)
    in let _ = aux 0 (List.rev lp) in
      AstPlacement.Fonction (info_ast,lp,nli)


(* analyser : AstType.programme -> AstPlacement.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie le bon placement mémoire et tranforme le programme
en un programme de type AstPlacement.programme *)
(* Erreur si mauvais placement mémoire *)
let analyser (AstType.Programme (fonctions,prog)) =
  let nf = List.map analyse_placement_fonction fonctions in
  let nb = analyse_placement_bloc prog 0 "SB" in
  AstPlacement.Programme (nf,nb)