(* Module de la passe de gestion des types *)
(* doit être conforme à l'interface Passe *)
open Exceptions
open Ast
open Tds
open Type

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme

(* analyse_type_expression : AstTds.expression -> AstType.expression * Type.typ *)
let rec analyse_type_expression e =
  match e with
  | AstTds.Ident info -> begin 
    let te = match info_ast_to_info info with
      | InfoVar (_, t, _, _) -> t
      | InfoConst (_, _) -> Type.Int
      | InfoFun _ -> Type.Undefined
    in
    (AstType.Ident info, te) end

  | AstTds.Booleen b -> (AstType.Booleen b, Type.Bool)
  | AstTds.Entier n -> (AstType.Entier n, Type.Int)

  | AstTds.Unaire (op, expr) -> begin 
      let (ne, te) = analyse_type_expression expr in
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

  | AstTds.Binaire (op, e1, e2) ->
      let (ne1, te1) = analyse_type_expression e1 in
      let (ne2, te2) = analyse_type_expression e2 in
      begin
        match (te1, te2) with
        | (Type.Int, Type.Int) ->
            let nop =
              begin match op with
              | AstSyntax.Plus -> AstType.PlusInt
              | AstSyntax.Mult -> AstType.MultInt
              | AstSyntax.Equ  -> AstType.EquInt
              | AstSyntax.Inf  -> AstType.Inf
              | AstSyntax.Fraction -> AstType.Fraction 
            end in
            (* le type du résultat *)
            let tres =
              begin match nop with
              | AstType.PlusInt | AstType.MultInt -> Type.Int
              | AstType.EquInt | AstType.Inf -> Type.Bool
              | AstType.Fraction -> Type.Rat
              | _ -> assert false
            end in
            (AstType.Binaire (nop, ne1, ne2), tres)

        | (Type.Rat, Type.Rat) ->
            let nop =
              begin match op with
              | AstSyntax.Plus -> AstType.PlusRat
              | AstSyntax.Mult -> AstType.MultRat
              (*| AstSyntax.Equ  -> AstType.EquRat *)
              | _ -> raise (TypeBinaireInattendu (op, te1, te2))
            end in
            let tres =
              begin match nop with
              | AstType.PlusRat | AstType.MultRat -> Type.Rat
              | _ -> assert false
            end in
            (AstType.Binaire (nop, ne1, ne2), tres)

        | (Type.Bool, Type.Bool) ->
            let nop =
              begin match op with
              | AstSyntax.Equ -> AstType.EquBool
              | _ -> raise (TypeBinaireInattendu (op, te1, te2))
            end in
            (AstType.Binaire (nop, ne1, ne2), Type.Bool)

        | _ ->
            raise (TypeBinaireInattendu (op, te1, te2))
        end
  | AstTds.AppelFonction (info, le) ->
    let l = List.map analyse_type_expression le in
    let (nle, lte) = List.split l in
    begin
      match info_ast_to_info info with
      | InfoFun (_, tret, tparams) ->
          if est_compatible_list lte tparams then
            (AstType.AppelFonction (info, nle), tret)
          else
            raise (TypesParametresInattendus (tparams, lte))
      | InfoVar (n, _, _, _) -> raise (MauvaiseUtilisationIdentifiant n)
      | InfoConst (n, _) -> raise (MauvaiseUtilisationIdentifiant n)
    end

(* analyse_type_instruction : AstTds.instruction -> AstType.instruction *)
let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, info, e) ->
      let (ne, te) = analyse_type_expression e in
      if te = t then (
        modifier_type_variable t info;
        AstType.Declaration (info, ne)
      ) else
        raise (TypeInattendu (te, t))

  | AstTds.Affectation (info, e) ->
      let (ne, te) = analyse_type_expression e in
      begin match info_ast_to_info info with
        | InfoVar (_, tdecl, _, _) ->
            if te = tdecl then AstType.Affectation (info, ne)
            else raise (TypeInattendu (te, tdecl))
        | InfoConst (n, _) -> raise (MauvaiseUtilisationIdentifiant n)
        | InfoFun (n, _, _) -> raise (MauvaiseUtilisationIdentifiant n)
      end

  | AstTds.Affichage e ->
      let (ne, te) = analyse_type_expression e in
      begin match te with
      |Type.Int -> (AstType.AffichageInt ne)
      |Type.Bool -> (AstType.AffichageBool ne)
      |Type.Rat -> (AstType.AffichageRat ne)
      |_ -> raise (TypesParametresInattendus ([te],[Type.Int;Type.Bool;Type.Rat]))
      end

  | AstTds.Conditionnelle (c, bt, be) ->
      let (nc, tc) = analyse_type_expression c in
      if tc <> Type.Bool then raise (TypeInattendu (tc, Type.Bool))
      else
        let nt = analyse_type_bloc bt in
        let ne = analyse_type_bloc be in
        AstType.Conditionnelle (nc, nt, ne)

  | AstTds.TantQue (c, b) ->
      let (nc, tc) = analyse_type_expression c in
      if tc <> Type.Bool then raise (TypeInattendu (tc, Type.Bool))
      else
        let nb = analyse_type_bloc b in
        AstType.TantQue (nc, nb)

  | AstTds.Retour (e, infofun) -> begin 
    let (ne, te) = analyse_type_expression e in
    match info_ast_to_info infofun with
    | InfoFun (_, tret, _) ->
        if te = tret then
          AstType.Retour (ne, infofun)
        else
          raise (TypeInattendu (te, tret))
    | InfoVar (_, _, _, _) 
    | InfoConst (_, _) ->
        failwith "Retour utilisé sur un identifiant qui n'est pas une fonction"
    end
  |AstTds.Empty -> AstType.Empty

(* analyse_type_bloc : AstTds.bloc -> AstType.bloc *)
and analyse_type_bloc li =
  List.map analyse_type_instruction li

(* analyse_type_fonction : AstTds.fonction -> AstType.fonction *)
let analyse_type_fonction (AstTds.Fonction (t, n, lp, li)) =
  match info_ast_to_info n with
  | InfoFun (_, tret, ltpar) ->
      if tret <> t then 
        raise (TypeInattendu (tret, t))
      else if est_compatible_list (List.map fst lp) ltpar then
        let nli = analyse_type_bloc li in
        AstType.Fonction (n, List.map snd lp, nli)
      else
        raise (TypesParametresInattendus (ltpar, List.map fst lp))
  | InfoVar (_, _, _, _) 
  | InfoConst (_, _) ->
      (* Impossible normalement, on lève une erreur si ce n’est pas une fonction *)
      failwith "analyse_type_fonction: identifiant n’est pas une fonction"

(* analyser : AstTds.programme -> AstType.programme *)
let analyser (AstTds.Programme (fonctions, prog)) =
  let nf = List.map analyse_type_fonction fonctions in
  let nb = analyse_type_bloc prog in
  AstType.Programme (nf, nb)
