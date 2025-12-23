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
(* Paramètre ecriture : un booléen pour signifier l'écriture ou la lecture *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'affectable *)
(* en une affectable de type AstTds.affectable *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_affectable tds a ecriture =  
  match a with 
  | AstSyntax.Ident str -> begin
    match Tds.chercherGlobalement tds str with 
     | None -> raise (IdentifiantNonDeclare str) 
     | Some info -> begin 
        match info_ast_to_info info with
        (* On peut écrire et lire une variable *) 
        | InfoVar _  -> AstTds.Ident info
        | InfoParam _ -> AstTds.Ident info
          (* On peut seulement lire une constante *)
        | InfoConst _ ->
          if ecriture then raise (MauvaiseUtilisationIdentifiant str)
          else AstTds.Ident info
        (* Une fonction n'est pas affectable *)
        | _ -> raise (MauvaiseUtilisationIdentifiant str)
     end
  end
  | AstSyntax.Deref a ->
      let na = analyse_tds_affectable tds a ecriture in
      AstTds.Deref na 

(* analyse_tds_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression *)
(* en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e =
  match e with
  (* Booléen *)
   | AstSyntax.Booleen bool -> AstTds.Booleen (bool) 
  (* Entier *)
   | AstSyntax.Entier entier -> AstTds.Entier (entier)
  (*  Opération unaire représentée par l'opérateur et l'opérande *) 
  | AstSyntax.Unaire (op, expr) ->
      let exprTds = analyse_tds_expression tds expr in 
      AstTds.Unaire (op, exprTds)
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *) 
  | AstSyntax.Binaire (op, expr1Tds, expr2Tds) ->
      let ne1 = analyse_tds_expression tds expr1Tds in 
      let ne2 = analyse_tds_expression tds expr2Tds in 
      AstTds.Binaire (op, ne1, ne2)
  (* Gestion de l'appel de fonction *)
  | AstSyntax.AppelFonction (id, le) ->
    (* Cherche l'existence de la fonction dans la TDS globale *)
    begin
      match Tds.chercherGlobalement tds id with
      | None ->
          (* La fonction n'existe pas *)
          raise (IdentifiantNonDeclare id)
      | Some info_ast ->
          begin
            match info_ast_to_info info_ast with
            | InfoFun (_, _, _) ->
                (* Analyse de tous les arguments de l'appel *)
                let leTds = List.map (analyse_tds_expression tds) le in
                (* Construction de l'AST TDS pour l'appel de fonction *)
                AstTds.AppelFonction (info_ast, leTds)
            | _ ->
                (* On appelle quelque chose qui n'est pas une fonction *)
                raise (MauvaiseUtilisationIdentifiant id)
          end
    end
  (* Gestion des appel à une adresse avec & *)
  | AstSyntax.Adresse id -> begin
    match chercherGlobalement tds id with
    (* Cas où le pointeur n'existe pas *)
    | None -> raise (IdentifiantNonDeclare id)
    | Some info -> begin
      match info_ast_to_info info with 
      (* On ne peut que accéder à l'adresse d'une variable *)
      | InfoVar _ -> AstTds.Adresse info 
      | InfoParam (_,_,true) -> AstTds.Adresse info 
      | _ -> raise (MauvaiseUtilisationIdentifiant id)
    end
  end
  (* Gestion des affectables *)
  | AstSyntax.Affectable a -> begin
    let na = analyse_tds_affectable tds a false in
    (* Gestion des constantes *)
    match na with 
    | AstTds.Ident info -> begin
      match info_ast_to_info info with
      (* Remplacement de la constante par sa valeur entière *) 
      | InfoConst (_,valeur) -> AstTds.Entier valeur
      | _ -> AstTds.Affectable na
    end
    | _ -> AstTds.Affectable na
  end
  (* Gestion de new *)
  | AstSyntax.New typ ->  AstTds.New typ
  (* Gestion de null *)
  | AstSyntax.Null -> AstTds.Null
  (* Gestion des références *)
  | AstSyntax.Ref e ->
    (* Analyse de son expression *)
    let ne = analyse_tds_expression tds e in
    AstTds.Ref ne
  (* Gestion de l'utilisation d'une valeur enum *) 
  | AstSyntax.EnumE n -> begin
    match Tds.chercherGlobalement tds n with 
    | None -> raise (IdentifiantNonDeclare n)
    | Some info -> begin 
      (* On vérifie que c'est une valeur d'énum *)
      match info_ast_to_info info with 
      | InfoEnumVal(_,_,_,_) -> AstTds.EnumE info 
      | _ -> raise (MauvaiseUtilisationIdentifiant n)
    end
  end

(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds oia i =
  match i with
  | AstSyntax.Declaration (t, n, e) -> begin
      match chercherLocalement tds n with
      | None ->
        (* L'identifiant n'est pas trouvé dans la tds locale,
        il n'a donc pas été déclaré dans le bloc courant *)
        (* Vérification de la bonne utilisation des identifiants dans l'expression *)
        (* et obtention de l'expression transformée *)
        let ne = analyse_tds_expression tds e in 
        (* Création de l'information associée à l'identfiant *)
        let info = InfoVar (n, Undefined, 0, "") in
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
  | AstSyntax.Affectation (a, e) ->
      (* Analyse de l'affectable *)
      let na = analyse_tds_affectable tds a true in
      (* Analyse de l'expression *)
      let ne = analyse_tds_expression tds e in 
      AstTds.Affectation (na, ne)
  | AstSyntax.Constante (n,v) -> begin
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
      let tast = analyse_tds_bloc tds oia t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds oia e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstTds.Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in 
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds oia b in
      (* Renvoie la nouvelle structure de la boucle *)
      AstTds.TantQue (nc, bast)
  | AstSyntax.Retour (e) -> begin
      (* On récupère l'information associée à la fonction à laquelle le return est associée *)
      match oia with
        (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
      | None -> raise RetourDansMain
        (* Il y a une information -> l'instruction est dans une fonction ou une procédure *)
      | Some ia -> begin 
        match info_ast_to_info ia with 
        | InfoFun (_, t, _) -> 
          if t=Void then 
            raise (RetourNonVideDansProcedure)
          else 
            (* Analyse de l'expression *)
            let ne = analyse_tds_expression tds e in 
            AstTds.Retour (ne,ia)
        | _ -> failwith "erreur interne"
        end
  end
  | AstSyntax.RetourVoid -> begin 
    (* On doit s'assurer que le return se trouve bien dans une procédure *)
    match oia with
    (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
      | None -> raise RetourDansMain
        (* Il y a une information -> l'instruction est dans une procédure *)
      | Some ia -> begin 
        match info_ast_to_info ia with 
        | InfoFun (_,t,_) -> 
          if t=Void then (AstTds.RetourVoid ia) else raise (RetourVideDansFonction)
        | _ -> failwith "erreur interne" 
      end
    end
  | AstSyntax.AppelProcedure (n,lp) -> begin
    (* On cherche l'existence de la procédure *)
    match Tds.chercherGlobalement tds n with
    | None -> raise (IdentifiantNonDeclare n)
    | Some info_ast ->
        begin match info_ast_to_info info_ast with
        | InfoFun (_, t, _) when t = Void ->
            let args_tds = List.map (analyse_tds_expression tds) lp in
            AstTds.AppelProcedure (info_ast, args_tds)
        | InfoFun (_, _, _) -> raise (AppelFonctionPourProcedure n)
        | _ -> failwith "erreur interne"
        end
      end

(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds oia li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc oia) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli

(* analyse_tds_enum : tds -> AstSyntax.Enum -> AstTds.Enum *)
(* Paramètres : 
      - tds : la table des symboles courante
      - Enum  : la déclaration d'énumération issue de l'AST syntaxique
   Résultat : 
      - renvoie une déclaration d'énumération dans l'AST TDS
   Fonctionnalité :
      - Vérifie que le nom de l'énum n'est pas déjà déclaré dans le bloc courant
      - Ajoute l'énum dans la TDS
      - Pour chaque membre, crée une info associée et l'ajoute dans la TDS
      - Renvoie un AstTds.Enum contenant les infos de l'énum et de ses membres
   Exceptions :
      - DoubleDeclaration si le nom de l'énum existe déjà dans la TDS
*)
let analyse_tds_enum tds (AstSyntax.Enum (nom_enum, membres)) =
  (* Vérification que l'énum n'existe pas déjà localement *)
  match Tds.chercherLocalement tds nom_enum with
  | Some _ -> raise (DoubleDeclaration nom_enum)
  | None ->
      (* Création de l'information associée à l'énum *)
      let info_enum = InfoEnum (nom_enum) in
      let ia_enum = info_to_info_ast info_enum in
      (* Ajout de l'énum dans la TDS *)
      Tds.ajouter tds nom_enum ia_enum;

      (* Analyse et ajout des membres *)
      let ia_membres =
        List.map (fun membre ->
          (* Vérification que le membre n'existe pas déjà *)
          match Tds.chercherLocalement tds membre with
          | Some _ -> raise (DoubleDeclaration membre)
          | None ->
              let info_membre = InfoEnumVal (membre, nom_enum, 0, "") in
              let ia_membre = info_to_info_ast info_membre in
              Tds.ajouter tds membre ia_membre;
              ia_membre
        ) membres
      in
      (* Retourne la déclaration d'énumération TDS *)
      AstTds.Enum (ia_enum, ia_membres)


(* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li)) =
  (* On crée l'information associée à la fonction et on l'ajoute dans la TDS mère *)
  let info_fun = InfoFun (n, t, List.map (fun (_, typ, _) -> typ) lp) in
  let nTds = info_to_info_ast info_fun in
  (* Vérification de l'existence préalable de la fonction dans la TDS mère *)
  (match chercherGlobalement maintds n with
  | None -> ajouter maintds n nTds
  | Some _ -> raise (DoubleDeclaration n));

  (* Création d'une tds fille pour le corps de la fonction *)
  let tdsFille = creerTDSFille maintds in

  (* Analyse des paramètres *)
  let lpTds =
    List.map (fun (is_ref, typ, name) ->
      (* Création de l'info pour le paramètre avec le booléen de passage par référence *)
      let info_param = InfoParam (name, typ, is_ref) in
      let ia_param = info_to_info_ast info_param in
      (* Vérification des doublons dans la TDS fille *)
      (match Tds.chercherLocalement tdsFille name with
      | None -> Tds.ajouter tdsFille name ia_param
      | Some _ -> raise (DoubleDeclaration name));
      (typ, ia_param)
    ) lp
  in

  (* Analyse du bloc de la fonction en passant l'information de la fonction *)
  let blocTds =
    List.map (analyse_tds_instruction tdsFille (Some nTds)) li
  in

  (* Retourne la représentation AstTds de la fonction *)
  AstTds.Fonction (t, nTds, lpTds , blocTds)

(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (enums, fonctions, bloc_principal)) =
  (* Création de la TDS mère *)
  let tds = creerTDSMere () in

  (* Analyse des enums *)
  let enumsTds = List.map (fun e -> analyse_tds_enum tds e) enums in

  (* Analyse des fonctions *)
  let fonctionsTds = List.map (fun f -> analyse_tds_fonction tds f) fonctions in

  (* Analyse du bloc principal *)
  let blocTds = analyse_tds_bloc tds None bloc_principal in

  (* Retour du programme complet en AST TDS *)
  AstTds.Programme (enumsTds, fonctionsTds, blocTds)