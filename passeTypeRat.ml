(* Module de la passe de gestion des types *)
(* doit être conforme à l'interface Passe *)
open Exceptions
open Ast
open Tds
open Type

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme

(* analyse_type_affectable : AstTds.affectable -> AstType.affectable * Type.typ *)
(* Cette fonction réalise l'analyse de type d'un affectable (variable,          *)
(* déréférencement, etc.) dans la passe de typage.                              *)
(* Paramètre a : l'affectable de l'astTds                                       *)
(* Résultat na : l'affectable après la passe de typage                          *)
(* Résultat t  : le type de l'affectable                                        *)
let rec analyse_type_affectable a =
  (* Filtrage pour distinguer une variable et un déréférencement *)
  match a with 
  | AstTds.Ident info -> begin 
    (* Récupération des informations associées à l'identifiant *)
    match info_ast_to_info info with 
    | InfoVar(_,t,_,_) ->
      (* Vérifie que la variable n'est pas de type Void *)
      if t=Void then 
        raise TypeVoidInattendu
      else
        (* Retourne l'affectable typé et son type *)
        ((AstType.Ident info), t)
    | InfoParam(_,t,_) -> 
      (* Vérifie qu'un paramètre n'est pas de type Void *)
      if t = Void then
        raise TypeVoidInattendu
      else
        (* Retourne l'affectable typé et son type *)
        (AstType.Ident info, t)
    | _ -> failwith "erreur interne"  
  end
  | AstTds.Deref a1 -> begin 
    (* Analyse récursive de l'affectable sous-jacent *)
    let (na1, t) = analyse_type_affectable a1 in
    (* Vérifie que l'affectable à déréférencer est un pointeur *)
    match t with
    | Pointeur t_pointe -> (AstType.Deref na1, t_pointe)
    | _ -> raise DereferencementIllegal
  end 


(* analyse_type_binaire : AstSyntax.op -> Type.typ -> Type.typ -> AstType.op * Type.typ *)
(* Cette fonction résout le typage d’une opération binaire.                             *)
(* Elle vérifie la compatibilité des types des deux opérandes et :                      *)
(*  - choisit l’opérateur typé correspondant (AstType.binaire)                          *)
(*  - calcule le type résultant                                                         *)
(* Paramètre op : opérateur syntaxique (AstSyntax.binaire)                              *)
(* Paramètre t1  : type du premier opérande                                             *)
(* Paramètre t2  : type du second opérande                                              *)
(* Résultat :                                                                           *)
(*  (nop, tres) :                                                                       *)
(*  nop  : opérateur binaire typé (AstType.binaire)                                     *)
(*  tres : type du résultat                                                             *)
(* Exceptions :                                                                         *)
(*  - TypeBinaireInattendu si les types sont incompatibles                              *)
let analyse_type_binaire op t1 t2 =
  match (t1, t2) with
  (* Cas des entiers *)
  | (Type.Int, Type.Int) ->
      let nop =
        match op with
        | AstSyntax.Plus      -> AstType.PlusInt
        | AstSyntax.Mult      -> AstType.MultInt
        | AstSyntax.Equ       -> AstType.EquInt
        | AstSyntax.Inf       -> AstType.Inf
        | AstSyntax.Fraction  -> AstType.Fraction
      in
      let tres =
        match nop with
        | AstType.PlusInt | AstType.MultInt -> Type.Int
        | AstType.EquInt  | AstType.Inf     -> Type.Bool
        | AstType.Fraction                  -> Type.Rat
        | _ -> assert false
      in
      (nop, tres)

  (* Cas des rationnels *)
  | (Type.Rat, Type.Rat) ->
      let nop =
        match op with
        | AstSyntax.Plus -> AstType.PlusRat
        | AstSyntax.Mult -> AstType.MultRat
        | _ -> raise (TypeBinaireInattendu (op, t1, t2))
      in
      (nop, Type.Rat)

  (* Cas des booléens *)
  | (Type.Bool, Type.Bool) ->
      let nop =
        match op with
        | AstSyntax.Equ -> AstType.EquBool
        | _ -> raise (TypeBinaireInattendu (op, t1, t2))
      in
      (nop, Type.Bool)

  (* Cas des énumérations *)
  | (Type.Tid n1, Type.Tid n2) ->
      if n1 = n2 && op = AstSyntax.Equ then
        (AstType.EquEnum, Type.Bool)
      else
        raise (TypeBinaireInattendu (op, t1, t2))

  (* Tous les autres cas sont invalides *)
  | _ ->
      raise (TypeBinaireInattendu (op, t1, t2))

(* analyse_type_expression : AstTds.expression -> AstType.expression * Type.typ              *)
(* Cette fonction réalise l'analyse de type d'une expression dans la passe de typage.        *)
(* Elle vérifie que les opérations sont correctes selon les types, et annotera l'expression. *)
(* Paramètre e : expression à analyser (AstTds.expression)                                   *)
(* Résultat ne : expression annotée pour la passe de typage (AstType.expression)             *)
(* Résultat t  : type de l'expression                                                        *)
let rec analyse_type_expression e =
  match e with
  | AstTds.Entier n -> 
    (* Les entiers ont toujours le type Int *)
    (AstType.Entier n, Type.Int)
  | AstTds.Booleen b ->
    (* Les booléens ont toujours le type Bool *) 
    (AstType.Booleen b, Type.Bool)
  | AstTds.Affectable a ->
    (* Analyse de l'affectable *)
    let (na,ta) = analyse_type_affectable a in 
    (AstType.Affectable na, ta)
  | AstTds.Adresse info -> begin 
    match info_ast_to_info info with 
    | InfoVar (_,t,_,_) -> (AstType.Adresse info, Pointeur t)
    | _ -> failwith "erreur inerne"
  end
  | AstTds.Unaire (op, expr) -> begin
    (* Analyse de l'expression *)
    let (ne,te) = analyse_type_expression expr in 
    (* opérations unaires (num/den) s'appliquent à Rat et rendent Int *)
    if te = Type.Rat then
        let nop = match op with 
        |AstSyntax.Denominateur -> AstType.Denominateur
        |AstSyntax.Numerateur -> AstType.Numerateur
      in 
        (AstType.Unaire (nop, ne), Type.Int)
      else
        raise (TypeInattendu (te, Type.Rat))
  end
  | AstTds.New (t) ->
    (* On donne le type pointeur au type *) 
    (AstType.New t, Pointeur t) 
  | AstTds.Null -> 
    (AstType.Null, Pointeur Undefined)
  | AstTds.Binaire (op,e1,e2) -> begin 
    (* analyse des deux expressions *)
    let (ne1,te1) = analyse_type_expression e1 in
    let (ne2,te2) = analyse_type_expression e2 in
    (* Résolution du typage de l’opération binaire *)
    let (nop,tres) = analyse_type_binaire op te1 te2 in
    (AstType.Binaire (nop, ne1, ne2), tres)
  end
  | AstTds.EnumE info -> begin
    (* Une valeur d’énumération a pour type Enum <nom> *)
    match info_ast_to_info info with
    | InfoEnumVal (_, enum_name,_,_) -> (AstType.EnumE info, Type.Tid enum_name)
    | _ -> failwith "erreur interne : EnumE mal typé"
  end
  | AstTds.Ref info ->
    begin
      match info_ast_to_info info with
      | InfoVar (_, t, _,_) ->
          (AstType.Ref info, t)
      | _ ->
          failwith "ref appliqué à quelque chose qui n'est pas une variable"
    end
  | AstTds.AppelFonction (info, le) -> begin
    (* Analyse typée des arguments *)
    let l = List.map analyse_type_expression le in
    let (nle, lte) = List.split l in

    match info_ast_to_info info with
    | InfoFun (_, tret, tparams) ->
        (* Vérification du nombre de paramètres *)
        if List.length lte <> List.length tparams then
          raise (TypesParametresInattendus (tparams, lte))
        else begin
          (* Vérification type à type *)
          List.iter2
            (fun t_arg t_param ->
               if not (est_compatible t_arg t_param) then
                 raise (TypesParametresInattendus (lte, tparams))
            )
            lte tparams;

          (AstType.AppelFonction (info, nle), tret)
        end
    | _ -> failwith "erreur interne"
  end



(* analyse_type_instruction : AstTds.instruction -> AstType.instruction *)
(* analyse d'une instruction lors de la phase de typage                 *)
(* Paramètre i : l'instruction                                          *)
(* Retour ni : la nouvelle instruction après la passe de typage.        *)
let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, info, e) ->
    (* Analyse de l’expression d’initialisation *)
    let (ne, te) = analyse_type_expression e in
    (* Le type de l’expression doit correspondre au type déclaré *)
    if est_compatible te t then (
      (* On fixe définitivement le type de la variable dans la TDS *)
      modifier_type_variable t info;
      AstType.Declaration (info, ne)
    ) else
      raise (TypeInattendu (te, t))
  | AstTds.Affectation (a, e) -> begin
    (* Analyse de l’expression affectée *)
    let (ne, te) = analyse_type_expression e in
    (* Analyse de l'affectable *)
    let (na, ta) = analyse_type_affectable a in 
    if est_compatible ta te then 
      AstType.Affectation(na,ne)
    else
      raise (TypeInattendu (te,ta)) 
  end
  | AstTds.Affichage e ->
      let (ne, te) = analyse_type_expression e in
      begin match te with
      |Type.Int -> (AstType.AffichageInt ne)
      |Type.Bool -> (AstType.AffichageBool ne)
      |Type.Rat -> (AstType.AffichageRat ne)
      (* Les pointeurs, enums, void, etc. ne sont pas affichables *)
      |_ -> raise (TypesParametresInattendus ([te],[Type.Int;Type.Bool;Type.Rat]))
      end
  | AstTds.Conditionnelle (c, bt, be) ->
      let (nc, tc) = analyse_type_expression c in
      (* La condition doit être booléenne *)
      if tc <> Type.Bool then 
        raise (TypeInattendu (tc, Type.Bool))
      else
        let nt = analyse_type_bloc bt in
        let ne = analyse_type_bloc be in
        AstType.Conditionnelle (nc, nt, ne)
  | AstTds.TantQue (c, b) ->
      let (nc, tc) = analyse_type_expression c in
      (* Condition booléenne obligatoire *)
      if tc <> Type.Bool then 
        raise (TypeInattendu (tc, Type.Bool))
      else
        let nb = analyse_type_bloc b in
        AstType.TantQue (nc, nb)
  | AstTds.Retour (e, infofun) -> begin 
    let (ne, te) = analyse_type_expression e in
    match info_ast_to_info infofun with
    | InfoFun (_, tret, _) ->
        (* Le type retourné doit correspondre au type de la fonction *)
        if te = tret then
          AstType.Retour (ne, infofun)
        else
          raise (TypeInattendu (te, tret))
    | _ -> failwith "erreur interne: retour hors fonction"
    end
  | AstTds.RetourVoid infofun -> begin 
      match info_ast_to_info infofun with
      | InfoFun (_, Type.Void, _) ->
          (* Retour vide autorisé uniquement dans une procédure *)
          AstType.RetourVoid infofun
      | InfoFun (_, t, _) ->
          (* Une fonction non void doit retourner une valeur *)
          raise (TypeInattendu (Type.Void, t))
      | _ ->
          failwith "Erreur interne : retour void hors fonction"
  end
  | AstTds.AppelProcedure (info, le) -> begin
      let l = List.map analyse_type_expression le in
      let (nle, lte) = List.split l in
      match info_ast_to_info info with
      | InfoFun (_, Type.Void, tparams) ->
          (* Même règle que AppelFonction, mais sans valeur de retour *)
          if est_compatible_list lte tparams then
            AstType.AppelProcedure (info, nle)
          else
            raise (TypesParametresInattendus (tparams, lte))
      | _ -> failwith "Erreur interne : appel de procédure invalide"
  end
  | AstTds.Empty -> AstType.Empty

(* analyse_type_bloc : AstTds.bloc -> AstType.bloc *)
and analyse_type_bloc li =
  List.map analyse_type_instruction li

(* analyse_type_fonction : AstTds.fonction -> AstType.fonction *)
let analyse_type_fonction (AstTds.Fonction (tdecl, infofun, lp, li)) =
  match info_ast_to_info infofun with
  | InfoFun (_, tret, ltpar) ->
      (* Vérification de la cohérence du type de retour *)
      (* Le type déclaré dans l’AST syntaxique doit correspondre *)
      (* au type enregistré dans l’InfoFun (créé en TDS) *)
      if tret <> tdecl then
        raise (TypeInattendu (tret, tdecl))
      (* Vérification de la compatibilité des types des paramètres *)
      (* lp : (typ * info_ast) list *)
      (* ltpar : typ list (types des paramètres formels) *)
      else if not (est_compatible_list (List.map fst lp) ltpar) then
        raise (TypesParametresInattendus (ltpar, List.map fst lp))
      else
        (* Analyse de typage du corps *)
        let nli = analyse_type_bloc li in
        (* Construction du nœud AstType *)
        (* Distinction fonction / procédure *)
        if tret = Type.Void then
          AstType.Procedure (infofun, List.map snd lp, nli)
        else
          AstType.Fonction (infofun, List.map snd lp, nli)
  | _ ->
      (* Impossible normalement : la TDS garantit que infofun est InfoFun *)
      failwith "analyse_type_fonction : identifiant n’est pas une fonction"

(* analyse_type_enum : AstTds.enum_decl -> AstType.enum_decl *)
(* Cette fonction réalise l’analyse de typage d’une déclaration d’énumération. *)
(* À ce stade, toute la structure de l’énumération a déjà été vérifiée en TDS : *)
(*  - unicité du nom de l’énumération                                         *)
(*  - unicité des valeurs                                                     *)
(*  - association correcte des InfoEnum / InfoEnumVal                         *)
(* La phase de typage se contente donc de propager l’information.             *)
let analyse_type_enum (AstTds.Enum (info_enum, lvals)) =
  match info_ast_to_info info_enum with
  | InfoEnum _ ->
      (* Les valeurs sont déjà des InfoEnumVal correctement typées *)
      AstType.Enum (info_enum, lvals)
  | _ -> failwith "analyse_type_enum : identifiant n’est pas une énumération"

(* analyser : AstTds.programme -> AstType.programme *)
let analyser (AstTds.Programme (enums,fonctions, prog)) =
  let nenums = List.map analyse_type_enum enums in 
  let nf = List.map analyse_type_fonction fonctions in
  let nb = analyse_type_bloc prog in
  AstType.Programme (nenums,nf, nb)
