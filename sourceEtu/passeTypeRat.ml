(* Module de la passe de typage *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
open Type

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme


(* getType : info_ast -> typ *)
(* Renvoie le type associé à une info_ast donnée en paramètre *)
let getType info_ast =
  match info_ast_to_info info_ast with
  | InfoConst (_,_) -> Int
  | InfoVar (_,t,_,_) -> t
  | InfoFun (_,t,_) -> t

(* analyse_type_affectable : AstTds.affectable -> (AstType.affectable * typ) *)
(* Paramètre a : l'affectable à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'affectable
en un affectable de type AstType.affectable, en renvoyant également le type associé *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_affectable a =
  match a with
  |AstTds.Ident info_ast -> (match info_ast_to_info info_ast with
                            |InfoVar (_,t,_,_) -> (AstTds.Ident info_ast, t)
                            |InfoConst _ -> (AstTds.Ident info_ast, Int)
                            |_ -> failwith "Internal Error"
                            )
  |AstTds.Deref aff -> (match analyse_type_affectable aff with
                      |(naff, Pointeur t) -> (AstTds.Deref naff, t)
                      | _ -> raise NotAPointeur
                        )

(* analyse_type_expression : AstTds.expression -> (AstType.expression * typ) *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'expression
en une expression de type AstType.expression, en renvoyant également le type associé *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_expression e =
  match e with
  (* | AstTds.Ident info_ast -> (AstType.Ident info_ast, getType info_ast) *)
  | AstTds.Booleen b -> (AstType.Booleen b, Bool)
  | AstTds.Entier n -> (AstType.Entier n, Int)
  | AstTds.Unaire (u,e) ->
    let (ne,t) = analyse_type_expression e in
      (match u, t with
      | Numerateur, Rat -> (AstType.Unaire (Numerateur,ne), Int)
      | Denominateur, Rat -> (AstType.Unaire (Denominateur,ne), Int)
      | _ -> raise (TypeInattendu (t,Rat)))

  | AstTds.Binaire (b,e1,e2) ->
    let (ne1,t1) = analyse_type_expression e1 in
      let (ne2,t2) = analyse_type_expression e2 in
        (match b, t1, t2 with
          | Plus, Int, Int -> (AstType.Binaire (PlusInt,ne1,ne2), Int)
          | Plus, Rat, Rat -> (AstType.Binaire (PlusRat,ne1,ne2), Rat)
          | Mult, Int, Int -> (AstType.Binaire (MultInt,ne1,ne2), Int)
          | Mult, Rat, Rat -> (AstType.Binaire (MultRat,ne1,ne2), Rat)
          | Fraction, Int, Int -> (AstType.Binaire (Fraction,ne1,ne2), Rat)
          | Equ, Int, Int -> (AstType.Binaire (EquInt,ne1,ne2), Bool)
          | Equ, Bool, Bool -> (AstType.Binaire (EquBool,ne1,ne2), Bool)
          | Inf, Int, Int -> (AstType.Binaire (Inf,ne1,ne2), Bool)
          | _ -> raise (TypeBinaireInattendu (b,t1,t2)))
    
  | AstTds.AppelFonction (info_ast, el) ->
    let nel = List.map analyse_type_expression el in
      let (expnel, tnel) = List.split nel in
        (match info_ast_to_info info_ast with
        | InfoFun (_,_,tl) ->
            if (est_compatible_list tnel tl) then
              (AstType.AppelFonction (info_ast, expnel), getType (info_ast))
            else
              raise (TypesParametresInattendus (tnel, tl))
        | _ -> failwith "InternalError")
        
  | AstTds.Ternaire (e1, e2, e3) ->
    let ne1,t1 = analyse_type_expression e1 in
      if est_compatible t1 Bool then
          (
            let ne2,t2 = analyse_type_expression e2 in
              let ne3,t3 = analyse_type_expression e3 in
                if est_compatible t2 t3 then
                    (AstType.Ternaire (ne1, ne2, ne3), t2)
                  else
                    raise (TypeInattendu (t2,t3))
          )
          else
            raise (TypeInattendu (t1,Bool))
    | AstTds.New t -> (AstType.New t, Pointeur t)
    | AstTds.Adresse info_ast -> 
        (match info_ast_to_info info_ast with
        |InfoVar(_,t,_,_) -> (AstType.Adresse info_ast, Pointeur t)
        |_ -> failwith "Internal Error"
        )
    | AstTds.Affectable a ->
      let na,t = analyse_type_affectable a in
        (AstType.Affectable na,t)
    | AstTds.Null -> AstType.Null,Undefined


(* analyse_type_instruction : AstTds.instruction -> AstType.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t,info_ast,e) ->
    let (ne,nt) = analyse_type_expression e in
      if est_compatible t nt then
        let _ = modifier_type_variable t info_ast in
          AstType.Declaration(info_ast,ne)
      else
        raise (TypeInattendu(nt,t))

  | AstTds.Affectation (a,e) ->
    let (na,t) = analyse_type_affectable a in
      let (ne,tPrime) = analyse_type_expression e in
        if est_compatible t tPrime then AstType.Affectation(na,ne)
                                   else raise (TypeInattendu(t,tPrime))
  (* | AstTds.Affectation (info_ast,e) ->
    let (ne,t) = analyse_type_expression e in
      let nt = getType info_ast in 
        if est_compatible t nt then
          AstType.Affectation (info_ast,ne)
        else
          raise (TypeInattendu(t,nt)) *)

  | AstTds.Affichage e ->
    let (ne,t) = analyse_type_expression e in
      (match t with
       | Int -> AffichageInt (ne)
       | Rat -> AffichageRat (ne)
       | Bool -> AffichageBool (ne)
       | Undefined -> failwith "InternalError"
       | Pointeur _ -> raise CannotPrintPointeur)

  | AstTds.Conditionnelle (e,b1,b2) ->
    let (ne,t) = analyse_type_expression e in
      if t = Bool then
        AstType.Conditionnelle (ne,analyse_type_bloc b1, analyse_type_bloc b2)
      else
        raise (TypeInattendu (t,Bool))

  | AstTds.TantQue (e,b) ->
    let (ne,t) = analyse_type_expression e in
      if t = Bool then
        AstType.TantQue (ne, analyse_type_bloc b)
      else
        raise (TypeInattendu (t,Bool))

  | AstTds.Retour (e,info_ast) ->
    let (ne,t) = analyse_type_expression e in
      let nt = (match info_ast_to_info info_ast with
                | InfoFun (_,nt,_) -> nt
                | _ -> failwith "InternalError") in
        if est_compatible t nt then
          AstType.Retour (ne,info_ast)
        else
          raise (TypeInattendu(t,nt))

  | AstTds.Empty -> AstType.Empty

  | AstTds.Loop (n,li) ->
      let nli = analyse_type_bloc li in
        AstType.Loop(n, nli)
  | AstTds.Break n -> AstType.Break n
  | AstTds.Continue n -> AstType.Continue n

(* analyse_type_bloc : AstTds.bloc -> AstType.bloc *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le bloc en un bloc de type AstType.bloc *)
(* Erreur si mauvaise utilisation des types *)
and analyse_type_bloc li =
  let nli = List.map analyse_type_instruction li in
    nli


(* analyse_type_fonction : AstTds.fonction -> AstType.fonction *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme la fonction
en une fonction de type AstType.fonction *)
(* Erreur si mauvaise utilisation des types *)
let analyse_type_fonction (AstTds.Fonction (t,info_ast,lp,li)) =
  let tlp = List.map fst lp in
    let iap = List.map (fun (t,info_ast_aux) -> modifier_type_variable t info_ast_aux;info_ast_aux) lp in
      let _ = modifier_type_fonction t tlp info_ast in
        let nli = analyse_type_bloc li in
          AstType.Fonction (info_ast,iap,nli)


(* analyser : AstTds.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.programme *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme (fonctions,prog)) =
  let nf = List.map analyse_type_fonction fonctions in
  let nb = analyse_type_bloc prog in
  AstType.Programme (nf,nb)