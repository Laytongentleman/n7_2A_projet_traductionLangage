(* Module de la passe de génération de code *)
(* doit être conforme à l'interface Passe *)
open Ast
open Tds
open Type
open Code
open Tam

type t1 = Ast.AstPlacement.programme
type t2 = string

(* analyse_code_expression: AstPlacement.expression -> String  *)
(* permet de générer le code tam associé à une expression     *)
(* Paramètre e : l'expression dont on veut générer le code tam *)
let rec analyse_code_expression e =
  match e with 
  | AstType.AppelFonction (info, le) ->
    let code_args =
      List.fold_right
        (fun e acc -> analyse_code_expression e ^ acc)
        le
        ""
    in
    let nom_fonction =
      match info_ast_to_info info with
      | InfoFun(n, _, _) -> n
      | InfoVar(_, _, _, _) | InfoConst(_, _) ->
          failwith "erreur interne"
    in
    code_args ^ call "SB" nom_fonction
  | AstType.Ident info -> 
    (*Récupération de l'adresse de la variable et de son type*)
    let (t,d,r) = 
      match info_ast_to_info info with
      |InfoVar(_,tr,dr,rr) -> (tr,dr,rr)
      |_ -> failwith "erreur interne"
    in
    load (getTaille t) d r
  | AstType.Booleen b ->
    loadl_int (if b then 1 else 0)
  | AstType.Entier n ->
    loadl_int n
  | AstType.Unaire (op, e) ->
    let se = analyse_code_expression e in
    let sop =
      match op with
      | AstType.Numerateur   -> pop 0 1   
      | AstType.Denominateur -> pop 1 1
    in
    se ^ sop
  | AstType.Binaire (op, e1, e2) ->
    let se1 = analyse_code_expression e1 in
    let se2 = analyse_code_expression e2 in
    let sop =
      match op with
      | AstType.Fraction ->
          (* constructeur n/d → pousse un rat : num puis den *)
          se1 ^ se2
      | AstType.PlusInt ->
          se1 ^ se2 ^ subr "IAdd"
      | AstType.MultInt ->
          se1 ^ se2 ^ subr "IMul"
      | AstType.EquInt ->
          se1 ^ se2 ^ subr "IEq"
      | AstType.EquBool ->
          se1 ^ se2 ^ subr "BAnd"   
      | AstType.Inf ->
          se1 ^ se2 ^ subr "ILeq"
      | AstType.PlusRat ->
          se1 ^ se2 ^ call "SB" "RAdd"
      | AstType.MultRat ->
          se1 ^ se2 ^ call "SB" "RMul"
    in
    sop
  
(* analyse_code_instruction: AstPlacement.instruction -> String  *)
(* permet de générer le code tam associé à une instruction     *)
(* Paramètre i : l'instruction dont on veut générer le code tam *)
let rec analyse_code_instruction i =
  match i with 
  | AstPlacement.Declaration (info, e) ->
    let se = analyse_code_expression e in
    let (t,d,r) = match info_ast_to_info info with
      |InfoVar(_,tr,dr,rr) -> (tr,dr,rr)
      |_ -> failwith "erreur interne"
    in
    let taille_t = getTaille t in
    (push taille_t)^se^(store taille_t d r)
  | AstPlacement.Affectation (info, e) ->
    let se = analyse_code_expression e in
    let (t,d,r) = match info_ast_to_info info with
      |InfoVar(_,tr,dr,rr) -> (tr,dr,rr)
      |_ -> failwith "erreur interne"
    in
    let taille_t = getTaille t in
    se^(store taille_t d r)
  | AstPlacement.AffichageInt e ->
    let se = analyse_code_expression e in
    se^(subr "IOut")
  | AstPlacement.AffichageRat e ->
    let se = analyse_code_expression e in
    se^(call "SB" "ROut")
  | AstPlacement.AffichageBool e ->
    let se = analyse_code_expression e in
    se^(subr "BOut")
  | AstPlacement.Conditionnelle (c, t, e) ->
    let sc = analyse_code_expression c in
    let st = analyse_code_bloc t in
    let se = analyse_code_bloc e in

    let lbl_else = getEtiquette () in
    let lbl_end  = getEtiquette () in

    sc^ jumpif 0 lbl_else^ st^ jump lbl_end^ lbl_else ^ ":\n"^ se^ lbl_end ^ ":\n"
  | AstPlacement.TantQue (c, b) ->
    let lbl_debut = getEtiquette () in
    let lbl_fin   = getEtiquette () in

    let sc = analyse_code_expression c in
    let sb = analyse_code_bloc b in

    lbl_debut ^ ":\n"^ sc^ jumpif 0 lbl_fin^ sb^ jump lbl_debut^ lbl_fin ^ ":\n"
  | AstPlacement.Retour (e, tailleRet, tailleParam) ->
    analyse_code_expression e
    ^ return tailleRet tailleParam
  | AstPlacement.Empty ->
    ""
(* analyse_code_bloc : AstPlacement.bloc * int -> String *)
and analyse_code_bloc (li, taille) =
  let code_liste = List.map analyse_code_instruction li in
  let code = String.concat "" code_liste in
  code ^ pop 0 taille

(* analyse_code_fonction: AstPlacement.fonction -> String *)
let analyse_code_fonction (AstPlacement.Fonction (info, _, (li, _))) =
  (* Récupération des infos de la fonction *)
  let (nom_fonction, _, _) = match info_ast_to_info info with
    | InfoFun(n, _, _) -> (n, 0, 0)  (* les autres valeurs ne sont pas utiles ici *)
    | _ -> failwith "erreur interne"
  in

  (* Code du bloc de la fonction. Le RETURN est généré dans le bloc lui-même. *)
  let code_liste = List.map analyse_code_instruction li in
  let code_li = String.concat "" code_liste in

  nom_fonction ^ ":\n" ^ code_li

(* analyse : AstPlacement.programme -> String *)
(* Permet de passer d'un AstPlacement au code TAM d'un programme *)
let analyser (AstPlacement.Programme (fonctions, prog)) =
  let code_entete = getEntete () in
  let code_func_list = List.map analyse_code_fonction fonctions in
  let code_func = String.concat "" code_func_list in
  let code_prog = analyse_code_bloc prog in
  code_entete ^ code_func ^ code_prog




   