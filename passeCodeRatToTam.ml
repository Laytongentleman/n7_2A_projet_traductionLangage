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
  | AstType.Ident info -> begin
    (*Récupération de l'adresse de la variable et de son type*)
      match info_ast_to_info info with
      | InfoVar(_, tr, dr, rr) -> load (getTaille tr) dr rr
      | InfoConst(_, n) -> loadl_int n
      | InfoFun(_, _, _) -> failwith "erreur interne: utilisation d'une fonction comme variable"
    end 
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
      | Fraction ->
          (* constructeur n/d → pousse un rat : num puis den *)
          se1 ^ se2 ^ call "SB" "norm"
      | PlusInt ->
          se1 ^ se2 ^ subr "IAdd"
      | MultInt ->
          se1 ^ se2 ^ subr "IMul"
      | EquInt ->
          se1 ^ se2 ^ subr "IEq"
      | EquBool ->
          se1 ^ se2 ^ subr "IEq"   
      | Inf ->
          se1 ^ se2 ^ subr "ILss"
      | PlusRat ->
          se1 ^ se2 ^ call "SB" "RAdd"
      | MultRat ->
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

    let nom_else = getEtiquette () in
    let lbl_else = label nom_else in
    let nom_end  = getEtiquette () in
    let lbl_end  = label nom_end in

    sc ^ jumpif 0 nom_else ^ st ^ jump nom_end ^ lbl_else ^ se ^ lbl_end

  | AstPlacement.TantQue (c, b) ->
    let nom_debut = getEtiquette () in
    let lbl_debut = label nom_debut in
    let nom_fin   = getEtiquette () in
    let lbl_fin   = label nom_fin in

    let sc = analyse_code_expression c in
    let sb = analyse_code_bloc b in

    lbl_debut ^ sc ^ jumpif 0 nom_fin ^ sb ^ jump nom_debut ^ lbl_fin  
  | AstPlacement.Retour (e, tailleRet, tailleParam) ->
    analyse_code_expression e
    ^ return tailleRet tailleParam
  | AstPlacement.Empty ->
    "\n"
(* analyse_code_bloc : AstPlacement.bloc * int -> String *)
and analyse_code_bloc (li, taille) =
  List.fold_left (fun acc i -> acc ^ analyse_code_instruction i) "" li
  ^ pop 0 taille

(* analyse_code_fonction: AstPlacement.fonction -> String *)
let analyse_code_fonction (AstPlacement.Fonction (info, _, (li, taille))) =
  (* Récupération des infos de la fonction *)
  let (nom_fonction, _, _) = match info_ast_to_info info with
    | InfoFun(n, _, _) -> (n, 0, 0)  (* les autres valeurs ne sont pas utiles ici *)
    | _ -> failwith "erreur interne"
  in

  (* Code du bloc de la fonction. Le RETURN est généré dans le bloc lui-même. *)
  let code_li = analyse_code_bloc (li, taille) in

  label nom_fonction ^ code_li ^ halt

(* analyse : AstPlacement.programme -> String *)
(* Permet de passer d'un AstPlacement au code TAM d'un programme *)
let analyser (AstPlacement.Programme (fonctions, prog)) =
  let code_entete = getEntete () in
  let code_func_list = List.map analyse_code_fonction fonctions in
  let code_func = String.concat "" code_func_list in
  let code_prog = label "main"^analyse_code_bloc prog in
  code_entete ^ code_func ^ code_prog ^ halt




   