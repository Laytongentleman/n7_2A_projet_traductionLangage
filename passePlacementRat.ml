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
        (AstPlacement.Retour (e, (getTaille tret), (List.fold_right (fun t tq -> tq + (getTaille t))  tp 0 )) , 0)
    | _ -> failwith "erreur interne"
    end

  | AstType.Affectation (info, e) ->
      AstPlacement.Affectation (info, e), 0

  | AstType.AffichageInt e ->
      AstPlacement.AffichageInt (e), 0
 
  | AstType.AffichageRat e ->
      AstPlacement.AffichageRat (e),0 

  | AstType.AffichageBool e ->
      AstPlacement.AffichageBool (e), 0

  |AstType.Empty -> AstPlacement.Empty, 0

(* analyse_type_bloc : AstType.bloc -> int -> String -> AstPlacement.bloc * int *)
and analyse_placement_bloc li depl reg =
  match li with 
  | [] ->  (([], 0), 0)
  | h::q -> let (ni, ti) = analyse_placement_instruction h depl reg in
            let (nq,tq),_ = analyse_placement_bloc q (depl+ti) reg in
            ((ni::nq), (ti+tq)), 0

(* analyse_type_fonction : AstType.fonction -> AstPlacement.fonction *)
let analyse_placement_fonction (AstType.Fonction (info, lip, b)) =
  
  let _ = List.fold_right (fun param depl ->
    match Tds.info_ast_to_info param with
    | InfoVar (_, tparam, _, _) ->
        modifier_adresse_variable depl "LB" param;
        (* (Printf.printf "%d ") (depl); *)
        depl - (getTaille tparam)
    | _ -> failwith "erreur interne"
  ) lip (-1) in

  let nb, _ = analyse_placement_bloc b 3 "LB" in
  AstPlacement.Fonction(info, lip, nb)

(* analyse : AstType.programme -> AstPlacement.programme *)
let analyser (AstType.Programme (fonctions, prog)) =
  let nf = List.map analyse_placement_fonction fonctions in
  let nb, _ = analyse_placement_bloc prog 0 "SB" in
  AstPlacement.Programme (nf, nb)
