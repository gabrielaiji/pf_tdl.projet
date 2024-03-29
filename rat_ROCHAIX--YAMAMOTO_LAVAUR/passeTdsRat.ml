(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme

(* analyse_tds_affectable : tds -> AstSyntax.affectable -> bool -> AstTds.affectable *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre a : l'affectable à analyser *)
(* Paramètre modif : permet de savoir si accès en écriture à la valeur pointée par a *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'affectable
en un affectable de type AstTds.affectable *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_affectable tds a modif =
  match a with
  |AstSyntax.Deref v -> AstTds.Deref (analyse_tds_affectable tds v false)
  |AstSyntax.Ident n -> 
    (match chercherGlobalement tds n with
      |None -> raise (IdentifiantNonDeclare n)
      |Some info_ast -> (match info_ast_to_info info_ast with
                          |InfoConst _ -> if modif then raise (MauvaiseUtilisationIdentifiant n)
                                                            else AstTds.Ident info_ast
                          |InfoVar _ -> AstTds.Ident info_ast
                          |InfoFun _ -> raise (MauvaiseUtilisationIdentifiant n) 
                        )
    )

(* analyse_tds_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e =
  match e with
  |AstSyntax.Booleen b -> AstTds.Booleen b
  |AstSyntax.Entier e -> AstTds.Entier e
  |AstSyntax.Unaire (unaire, exp) -> AstTds.Unaire (unaire,analyse_tds_expression tds exp)
  |AstSyntax.Binaire (binaire, exp1, exp2) -> 
        let ne1 = analyse_tds_expression tds exp1 in
          let ne2  = analyse_tds_expression tds exp2 in
            AstTds.Binaire (binaire, ne1, ne2)
  |AstSyntax.AppelFonction (str, expLs) -> 
                (match chercherGlobalement tds str with
                  |None -> raise  (IdentifiantNonDeclare str)
                  |Some info_ast -> (match info_ast_to_info info_ast with
                                  |InfoFun _ -> let newExpLs = List.map (analyse_tds_expression tds) expLs
                                                in AstTds.AppelFonction (info_ast, newExpLs)
                                  |_ -> raise (MauvaiseUtilisationIdentifiant str)
                                )
                )
  |AstSyntax.Ternaire (e1,e2,e3) ->
    let ne1 = analyse_tds_expression tds e1 in
      let ne2 = analyse_tds_expression tds e2 in
        let ne3 = analyse_tds_expression tds e3 in
          AstTds.Ternaire (ne1, ne2, ne3)
  |AstSyntax.New t -> AstTds.New t
  |AstSyntax.Adresse str ->
        (match chercherGlobalement tds str with
          |None -> raise (IdentifiantNonDeclare str)
          |Some info_ast -> (match info_ast_to_info info_ast with
                              |InfoVar _ -> AstTds.Adresse info_ast
                              |_ -> raise (MauvaiseUtilisationIdentifiant str)
                              )
          )
  |AstSyntax.Affectable a ->
    let na = analyse_tds_affectable tds a false in
      AstTds.Affectable na
  |AstSyntax.Null -> AstTds.Null 

(* Ensemble de fonctions utiles pour traiter le cas des boucles "loop" à la Rust *)
(* Explications dans le rapport du choix de la structure *)


(* estImbriquee : string -> ((string * int) list * (string * int) list) ref -> boolean *)
(* Paramètre name : nom de la loop dont on veut savoir si on est imbriquée dedans *)
(* Paramètre ref_tds_loop : ref de la pile des loops *)
(* Retourne un boolean disant si à la ligne à laquelle on se trouve, on
   est imbriquée dans une loop de nom name *)
let estImbriquee name ref_tds_loop = let lst_labels,_ = !ref_tds_loop in
  match List.assoc_opt name lst_labels with
    |None -> false
    |Some _ -> true

(* estVide : ((string * int) list * (string * int) list) ref -> boolean *)
(* Paramètre ref_tds_loop : ref de la pile des loops *)
(* Retourne un boolean disant si à la ligne à laquelle on se trouve, on
   est imbriquée dans au moins une loop*)
let estVide ref_tds_loop = let lst, _ = !ref_tds_loop in lst = []

(* update_assoc : 'a -> 'b -> ('a*'b) list -> ('a*'b) list *)
(* Paramètre key : clé dont on veut changer la valeur associée *)
(* Paramètre value : nouvelle valeur qu'on associe à la clé *)
(* Paramètre lst : liste associative dont on veut changer une valeur *)
(* Retourne la nouvelle liste associative, dont on a changé la valeur associé à la clé key *)
let rec update_assoc key value lst = match lst with
  |[] -> failwith "Internal Error"
  |(k,v)::tl -> if key = k then (k, value)::tl
                          else (k, v)::(update_assoc key value tl)

(* ajouteLoop : string -> ((string * int) list * (string * int) list) ref -> string *)
(* Paramètre name : nom de la loop qu'on veut ajouter *)
(* Paramètre ref_tds_loop : ref de la pile des loops *)
(* Retourne le label qu'on utilisera au niveau du code TAM de la nouvelle loop *)
let ajouteLoop name ref_tds_loop = let lst_labels, lst_numloop = !ref_tds_loop in
  match List.assoc_opt name lst_numloop with
    |None -> ref_tds_loop := ((name,1)::lst_labels), (name, 1)::lst_numloop;
              name^"1"
    |Some num -> ref_tds_loop := ((name,num+1)::lst_labels), (update_assoc name (num+1) lst_numloop);
                name^string_of_int(num+1)

(* removeLastLoop : ((string * int) list * (string * int) list) ref -> unit() *)  
(* Paramètre ref_tds_loop : ref de la pile des loops *)
(* Enlève de la ref de la pile des loops, la dernière loop ajoutée *)
let removeLastLoop ref_tds_loop = let lst_labels, lst_numloop = !ref_tds_loop in
  match lst_labels with
    |[] -> failwith "Internal Error"
    |_::tl -> ref_tds_loop := tl, lst_numloop

(* getUsedName : string -> ((string * int) list * (string * int) list) ref -> string *)
(* Paramètre name : nom de la loop dont on veut récupérer le label TAM *)
(* Paramètre ref_tds_loop : ref de la pile des loops *)
(* Retourne le label qu'on utilisera au niveau du code TAM, associée à la loop 
    de nom name, et qui est la plus interne *)
let getUsedName name ref_tds_loop = let lst_labels, _ = !ref_tds_loop in
  match List.assoc_opt name lst_labels with
    |None -> failwith "Internal Error"
    |Some num -> name^string_of_int(num)

(* getLastUsedName : string -> ((string * int) list * (string * int) list) ref -> string *)
(* Paramètre ref_tds_loop : ref de la pile des loops *)
(* Retourne le label de la loop la plus interne *)
let getLastUsedName ref_tds_loop = let lst_labels, _ = !ref_tds_loop in
  match lst_labels with
    |[] -> failwith "Internal Error"
    |(name, num)::_ -> name^string_of_int(num)

(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre ref_tds_loop : Référence de la liste des identifiants des loops dans laquelle l'instruction i est imbriquée *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds oia ref_tds_loop i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale,
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *)
            let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
            et l'expression remplacée par l'expression issue de l'analyse *)
            AstTds.Declaration (t, ia, ne)
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale,
            il a donc déjà été déclaré dans le bloc courant *)
            raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (a,e) ->
    let na = analyse_tds_affectable tds a true in
      let ne = analyse_tds_expression tds e in
        AstTds.Affectation(na,ne)
  (* | AstSyntax.Affectation (n,e) ->
      begin
        match chercherGlobalement tds n with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds globale. *)
          raise (IdentifiantNonDeclare n)
        | Some info ->
          (* L'identifiant est trouvé dans la tds globale,
          il a donc déjà été déclaré. L'information associée est récupérée. *)
          begin
            match info_ast_to_info info with
            | InfoVar _ ->
              (* Vérification de la bonne utilisation des identifiants dans l'expression *)
              (* et obtention de l'expression transformée *)
              let ne = analyse_tds_expression tds e in
              (* Renvoie de la nouvelle affectation où le nom a été remplacé par l'information
                 et l'expression remplacée par l'expression issue de l'analyse *)
              AstTds.Affectation (info, ne)
            |  _ ->
              (* Modification d'une constante ou d'une fonction *)
              raise (MauvaiseUtilisationIdentifiant n)
          end
      end *)
  | AstSyntax.Constante (n,v) ->
      begin
        match chercherLocalement tds n with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds locale,
             il n'a donc pas été déclaré dans le bloc courant *)
          (* Ajout dans la tds de la constante *)
          ajouter tds n (info_to_info_ast (InfoConst (n,v)));
          (* Suppression du noeud de déclaration des constantes devenu inutile *)
          AstTds.Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale,
          il a donc déjà été déclaré dans le bloc courant *)
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e ->
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      AstTds.Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds oia ref_tds_loop t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds oia ref_tds_loop e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstTds.Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds oia ref_tds_loop b in
      (* Renvoie la nouvelle structure de la boucle *)
      AstTds.TantQue (nc, bast)
  | AstSyntax.Retour (e) ->
      begin
      (* On récupère l'information associée à la fonction à laquelle le return est associée *)
      match oia with
        (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
      | None -> raise RetourDansMain
        (* Il y a une information -> l'instruction est dans une fonction *)
      | Some ia ->
        (* Analyse de l'expression *)
        let ne = analyse_tds_expression tds e in
        AstTds.Retour (ne,ia)
      end
      
  |AstSyntax.Loop (n, li) ->
      let name = (if n = "" then "autocreated@" else n) in
        begin
          let useableName = ajouteLoop name ref_tds_loop in
            let nli = analyse_tds_bloc tds oia ref_tds_loop li in
              removeLastLoop ref_tds_loop;
              AstTds.Loop(useableName, nli)
        end

  |AstSyntax.Break n ->
      if estVide ref_tds_loop then raise (LoopUndefined n)
        else
          if estImbriquee n ref_tds_loop
            then AstTds.Break (getUsedName n ref_tds_loop)
          else if n = ""
              then AstTds.Break (getLastUsedName ref_tds_loop)
          else
            raise (LoopUndefined n)
    
  |AstSyntax.Continue n ->
    if estVide ref_tds_loop then raise (LoopUndefined n)
        else
          if estImbriquee n ref_tds_loop
            then AstTds.Continue (getUsedName n ref_tds_loop)
          else if n = ""
              then AstTds.Continue (getLastUsedName ref_tds_loop)
          else
            raise (LoopUndefined n)


(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre refListeLoop : Référence de la liste des identifiants des loops dans laquelle la li est imbriquée *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds oia refListeLoop li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc oia refListeLoop) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li))  =
        match chercherLocalement maintds n with
          |Some _ -> raise (DoubleDeclaration n)
          |None -> let typeLst = List.map fst lp in 
                     let info_ast =  info_to_info_ast (InfoFun (n,t,typeLst)) in
                       let _ = ajouter maintds n info_ast in
                         let tdsFille = creerTDSFille maintds in
                           let aux (type_aux, str_aux) =
                             match chercherLocalement tdsFille str_aux with
                              |Some _ -> raise (DoubleDeclaration str_aux)
                              |None -> let info_aux = InfoVar (str_aux, type_aux, 0, "") in
                                         let iap = info_to_info_ast info_aux in
                                           ajouter tdsFille str_aux iap;(type_aux, iap)
                           in
                             let iapLst = List.map aux lp in
                               let nli = analyse_tds_bloc tdsFille (Some info_ast) (ref ([],[])) li in
                                 AstTds.Fonction (t, info_ast, iapLst, nli)

(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  let nf = List.map (analyse_tds_fonction tds) fonctions in
  let nb = analyse_tds_bloc tds None (ref ([],[])) prog in
  AstTds.Programme (nf,nb)
