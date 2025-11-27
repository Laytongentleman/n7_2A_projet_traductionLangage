(* Module de la passe de gestion des types *)
(* doit être conforme à l'interface Passe *)
open Type
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme

(* analyse_type_expression : type -> AstTds.expression -> AstType.expression *)
(* Paramètre type : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des types et transforme l'expression
en une expression de type AstType.expression *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_expression e = match e with
  (* gestion des identifiants *)
  | AstTds.Ident info -> AstType.Ident info
  (* Booléen *)
   | AstTds.Booleen bool -> AstType.Booleen (bool) 
  (* Entier *)
   | AstTds.Entier entier -> AstType.Entier (entier)
  (*  Opération unaire représentée par l'opérateur et l'opérande *) 
  | AstTds.Unaire (op, expr) ->

      let (ne,te) = analyse_type_expression expr in
      if (te = Type.Rat) then
        AstType.Unaire (op, ne, Type.Int)
      else 
        raise TypeInattendu te Type.Rat; 
   (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *) 
  | AstTds.Binaire (op, expr1Type, expr2Type) ->
      let (ne1, te1) = analyse_type_expression expr1Type in
      let (ne2, te2) = analyse_type_expression expr2Type in
      match (te1,te2) with
        | (Int, Int) -> begin
          let nop =  
            match op with 
            | Plus -> PlusInt
            | Mult -> MultInt
            | Equ -> EquInt
            | Inf -> Inf 
            | Fraction -> Fraction 
            (* | _ -> raise TypeBinaireInattendu (op,te1,te2) de toute façon ne passe pas la premiere passe du lexer *)
            in
            AstType.Binaire (nop, ne1, ne2)
        end
        | (Rat, Rat) ->
          let nop =  
            match op with 
            | Plus -> PlusRat
            | Mult -> MultRat
            | _ -> raise TypeBinaireInattendu (op, te1, te2)
            in
            AstType.Binaire (nop, ne1, ne2)
        | (Bool,Bool) ->
            let nop = 
              match op with
              | Equ -> EquBool
              | _ -> raise TypeBinaireInattendu (op, te1, te2)
        | _ -> raise TypeBinaireInattendu (op, te1, te2)
      
  | AstTds.AppelFonction (info, le) ->
        let l = List.map analyse_type_expression le in
        let (nle, lte) = List.split l in 
        begin
        match info_ast_to_info info with 
          | InfoFun (n,tr,tp) -> if est_compatible_list_type lte tp then
                                  AstType.AppelFonction (info, nle, tr)
                                 else 
                                   raise TypseParametresInattendus (nle, tr)
          | InfoVar(n,_,_,_) -> raise MauvaiseUtilisationIdentifiant (n)
          | InfoConst(n,_) -> raise MauvaiseUtilisationIdentifiant (n)
        end

                                  

  (* | _ -> AstType.Booleen (true) *)

(* analyse_type_instruction :  AstTds.instruction -> AstType.instruction *)
(* Paramètre i : l'instruction dont il faut analyser le type *)
(* Vérifie la bonne utilisation des types et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, info, e) ->
      let (ne, te) = analyse_type_expression e in 
        if (t = te) then
          modifier_type_variable t info;
          AstType.Declaration (info, ne)
        else
          raise TypeInattendu (te, t)
  | AstTds.Affectation (info,e) ->
      
      let (ne, te) = analyse_type_expression e in 
      let t = match (info_ast_to_info info) with
        | InfoVar(n,t,_,_) ->  t
        | _ -> raise  
        (* on propose une mini pause  *)

        

      | AstTds.Constante (n,v) ->
      begin
        match chercherLocalement type n with
        | None ->
          (* L'identifiant n'est pas trouvé dans la type locale,
             il n'a donc pas été déclaré dans le bloc courant *)
          (* Ajout dans la type de la constante *)
          ajouter type n (info_to_info_ast (InfoConst (n,v)));
          (* Suppression du noeud de déclaration des constantes devenu inutile *)
          AstType.Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la type locale,
          il a donc déjà été déclaré dans le bloc courant *)
          raise (DoubleDeclaration n)
      end
  | AstTds.Affichage e ->
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_type_expression type e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      AstType.Affichage (ne)
  | AstTds.Conditionnelle (c,t,e) ->
      (* Analyse de la condition *)
      let nc = analyse_type_expression type c in
      (* Analyse du bloc then *)
      let tast = analyse_type_bloc type oia t in
      (* Analyse du bloc else *)
      let east = analyse_type_bloc type oia e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstType.Conditionnelle (nc, tast, east)
  | AstTds.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_type_expression type c in
      (* Analyse du bloc *)
      let bast = analyse_type_bloc type oia b in
      (* Renvoie la nouvelle structure de la boucle *)
      AstType.TantQue (nc, bast)
  | AstTds.Retour (e) ->
      begin
      (* On récupère l'information associée à la fonction à laquelle le return est associée *)
      match oia with
        (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
      | None -> raise RetourDansMain
        (* Il y a une information -> l'instruction est dans une fonction *)
      | Some ia ->
        (* Analyse de l'expression *)
        let ne = analyse_type_expression type e in
        AstType.Retour (ne,ia)
      end

(* analyse_type_bloc : type -> info_ast option -> AstTds.bloc -> AstType.bloc *)
(* Paramètre type : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstType.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_type_bloc type oia li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle type locale
  pointant sur la table du bloc parent *)
  let typebloc = creertypeFille type in
  (* Analyse des instructions du bloc avec la type du nouveau bloc.
     Cette type est modifiée par effet de bord *)
   let nli = List.map (analyse_type_instruction typebloc oia) li in
   (* afficher_locale typebloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyse_type_fonction : type -> AstTds.fonction -> AstType.fonction *)
(* Paramètre type : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstType.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_type_fonction maintype (AstTds.Fonction(t,n,lp,li)) =
  (* On crée l'information associée à la fonction et on l'ajoute dans la type mère *)
  let info_fun = InfoFun (n, t, List.map fst lp) in
  let nType = info_to_info_ast info_fun in
  (* Vérification de l'existence préalable de la fonction dans la type mère *)
  (match chercherGlobalement maintype n with
  | None -> ajouter maintype n nType
  | Some _ -> raise (DoubleDeclaration n));

  (* Création d'une type fille pour le corps de la fonction *)
  let typeFille = creertypeFille maintype in

  (* Ajout des paramètres dans la type fille et construction de la liste (typ * info_ast) *)
  let lpType = List.map (fun (typ, name) ->
  let info_param = InfoVar (name, typ, 0, "") in
  let info_ast = info_to_info_ast info_param in
  (* si besoin on peut vérifier les doubles déclarations locales des paramètres *)
  (match chercherLocalement typeFille name with
  | None -> ajouter typeFille name info_ast
  | Some _ -> raise (DoubleDeclaration name));
  (typ, info_ast)
  ) lp in

  (* Analyse du bloc de la fonction en passant l'information de la fonction *)
  let blocType = analyse_type_bloc typeFille (Some nType) li in

  (* Retourne la représentation AstType de la fonction *)
  AstType.Fonction (t, nType, lpType , blocType)


(* analyser : AstTds.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstType.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstTds.Programme (fonctions,prog)) =
  let type = creertypeMere () in
  let nf = List.map (analyse_type_fonction type) fonctions in
  let nb = analyse_type_bloc type None prog in
  AstType.Programme (nf,nb)
