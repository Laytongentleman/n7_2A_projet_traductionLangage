(* Module de la passe de gestion de Placement mémoire *)
(* doit être conforme à l'interface Passe *)
(* open Exceptions *)
open Ast
open Tds
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

(* analyse_placement_instruction : AstType.instruction -> int -> String -> AstPlacement.instruction * int *)
let rec analyse_placement_instruction i depl reg =
  match i with
  | AstType.Declaration (info, e) ->
      begin
        match info_ast_to_info info with
        | InfoVar (_, tdecl, _, _) ->
            modifier_adresse_variable depl reg info;
            (AstPlacement.Declaration (info, e), getTaille tdecl)
        | _ -> failwith "erreur interne"
      end

  | AstType.Conditionnelle (c, bt, be) ->
      let (nbt, _) = analyse_placement_bloc bt depl reg and
      (nbe, _) = analyse_placement_bloc be depl reg in
      (AstPlacement.Conditionnelle (c, nbt, nbe), 0)

  | AstType.TantQue (c, b) ->
      let (nb, _) = analyse_placement_bloc b depl reg in
      (AstPlacement.TantQue (c, nb), 0)

  | AstType.Retour (e, infofun) -> begin 
    match info_ast_to_info infofun with
    | InfoFun (_, tret, tp) ->
        (AstPlacement.Retour (e, (getTaille tret), (List.fold_right (fun t tq -> tq + (getTaille t))  (List.map fst tp) 0 )) , 0)
    | _ -> failwith "erreur interne"
    end

  | AstType.Affectation (a, e) ->
      AstPlacement.Affectation (a, e), 0

  | AstType.AffichageInt e ->
      AstPlacement.AffichageInt (e), 0
 
  | AstType.AffichageRat e ->
      AstPlacement.AffichageRat (e),0 

  | AstType.AffichageBool e ->
      AstPlacement.AffichageBool (e), 0

  |AstType.Empty -> AstPlacement.Empty, 0

  | AstType.AppelProcedure (info,args) -> (AstPlacement.AppelProcedure (info,args),0)

  | AstType.RetourVoid infofun -> begin
    match info_ast_to_info infofun with
    | InfoFun(_,Void,tparams) ->
      let taille_params = List.fold_right (fun t acc -> acc + getTaille t) 
        (List.map fst tparams) 0 in
      (AstPlacement.RetourVoid taille_params, 0)
    | _ -> failwith "erreur interne"
  end

(* analyse_type_bloc : AstType.bloc -> int -> String -> AstPlacement.bloc * int *)
and analyse_placement_bloc li depl reg =
  match li with 
  | [] ->  (([], 0), 0)
  | h::q -> let (ni, ti) = analyse_placement_instruction h depl reg in
            let (nq,tq),_ = analyse_placement_bloc q (depl+ti) reg in
            ((ni::nq), (ti+tq)), 0

(* analyse_placement_fonction : AstType.fonction -> AstPlacement.fonction *)
let analyse_placement_fonction f =
  match f with
  | AstType.Fonction (info, lip, b) ->
      (* Placement des paramètres formels dans le cadre de pile (LB) *)
      let _ =
        List.fold_right
          (fun param depl ->
            match Tds.info_ast_to_info param with
            | InfoVar (_, tparam, _, _) ->
                modifier_adresse_variable (depl - getTaille tparam) "LB" param;
                depl - getTaille tparam
            | _ -> failwith "erreur interne : paramètre non variable"
          )
          lip
          0
      in
      (* Placement du bloc de la fonction *)
      let nb, _ = analyse_placement_bloc b 3 "LB" in
      AstPlacement.Fonction (info, lip, nb)
  | AstType.Procedure (info, lip, b) ->
      (* Placement des paramètres formels dans le cadre de pile (LB) *)
      let _ =
        List.fold_right
          (fun param depl ->
            match Tds.info_ast_to_info param with
            | InfoVar (_, tparam, _, _) ->
                modifier_adresse_variable (depl - getTaille tparam) "LB" param;
                depl - getTaille tparam
            | _ -> failwith "erreur interne : paramètre non variable"
          )
          lip
          0
      in
      (* Placement du bloc de la procédure *)
      let nb, _ = analyse_placement_bloc b 3 "LB" in
      AstPlacement.Procedure (info, lip, nb)

(* analyse_placement_enum : enum_decl list -> int -> enum_decl list * int *)
(* Place les énumérations globales et leurs valeurs en SB, retourne la liste avec adresses mises à jour et le prochain emplacement SB libre *)
let analyse_placement_enum enums depl_init =
  let rec aux enums depl acc =
    match enums with
    | [] -> (List.rev acc, depl)
    | AstType.Enum(info_enum, values) :: rest ->
        (* On ne place pas l'énum elle-même, seulement ses valeurs *)
        let values_placées, depl_final =
          List.fold_left (fun (acc_vals, d) value_info ->
            Tds.modifier_adresse_variable d "SB" value_info;
            (value_info :: acc_vals, d + 1)
          ) ([], depl) values
        in
        aux rest depl_final (AstPlacement.Enum(info_enum, List.rev values_placées) :: acc)
  in
  aux enums depl_init []

(* analyse : AstType.programme -> AstPlacement.programme *)
let analyser (AstType.Programme (enums, fonctions, prog)) =
  let nenums, next_sb = analyse_placement_enum enums 0 in
  let nf = List.map analyse_placement_fonction fonctions in
  let nb, _ = analyse_placement_bloc prog next_sb "SB" in
  AstPlacement.Programme (nenums, nf, nb)