(* Module de la passe de génération de code *)
(* doit être conforme à l'interface Passe *)
open Ast
open Tds
open Type
open Code
open Tam

type t1 = Ast.AstPlacement.programme
type t2 = string

(* analyse_code_affectable: AstPlacement.affectable -> bool -> string -> int -> string *)
(* Paramètre a: l'affectable à analyser *)
(* Paramètre ecriture: true si on écrit (STOREI), false si on lit (LOADI) *)
(* Paramètre acc : code accumulé correspondant aux déréférencements précédents *)
(* Paramètre prof_deref: nombre de Deref rencontrés *)
let rec analyse_code_affectable a ecriture acc prof_deref =
  match a with 
  | AstType.Ident info -> begin
      match info_ast_to_info info with
      | InfoVar (_, t, depl, reg, est_ref) ->
          (* On calcule le type réellement accédé *)
          let t_acces = profondeur_type t prof_deref in
          let taille = getTaille t_acces in

          if est_ref then
            (* On charge l'adresse contenue dans la variable (LOAD 1) *)
            load 1 depl reg
            ^ acc
            (* On lit ou écrit à cette adresse *)
            ^ (if ecriture then storei taille else loadi taille)
          else
            (* On charge l'adresse de la variable elle-même (LOADA) *)
            loada depl reg
            ^ acc
            (* On lit ou écrit à cette adresse *)
            ^ (if ecriture then storei taille else loadi taille)

      | _ -> failwith "erreur interne"
    end
  | AstType.Deref a ->
      (* Pour une déréférence explicite (\*a), on ajoute une indirection *)
      analyse_code_affectable a ecriture (acc ^ loadi 1) (prof_deref + 1)

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
      | _ -> failwith "erreur interne"
    in
    code_args ^ call "SB" nom_fonction
  | AstType.Affectable a -> 
    let sa = analyse_code_affectable a false "" 0 in
    sa
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
      | EquEnum ->
          se1 ^ se2 ^ subr "IEq"   
      | Inf ->
          se1 ^ se2 ^ subr "ILss"
      | PlusRat ->
          se1 ^ se2 ^ call "SB" "RAdd"
      | MultRat ->
          se1 ^ se2 ^ call "SB" "RMul"
    in
    sop
  | AstType.Null -> ""
  | AstType.Adresse info -> begin 
    match info_ast_to_info info with 
    | InfoVar(_,_,d,r,_) -> loada d r 
    | _ -> failwith "erreur interne"
  end 
  | AstType.New t -> loadl_int (getTaille t) ^ subr "MAlloc" 
  | AstType.EnumE info -> begin
    (* Récupération de l'adresse et du registre*)
    (* Puis chargement de la valeur *)
    match info_ast_to_info info with 
    | InfoEnumVal(_,n,d,r) -> load (getTaille (Tid n)) d r
    | _ -> failwith "erreur interne"
  end
  | AstType.Ref info -> begin
    (* Ref renvoie l'adresse de la variable *)
    match info_ast_to_info info with
    | InfoVar(_, t, d, r, est_ref) ->
        if est_ref then 
          load (getTaille t) d r
        else
          loada d r 
    | _ -> failwith "erreur interne"
  end

(* analyse_code_instruction: AstPlacement.instruction -> String  *)
(* permet de générer le code tam associé à une instruction     *)
(* Paramètre i : l'instruction dont on veut générer le code tam *)
let rec analyse_code_instruction i =
  match i with 
  | AstPlacement.Declaration (info, e) ->
    let se = analyse_code_expression e in
    let (t,d,r) = match info_ast_to_info info with
      |InfoVar(_,tr,dr,rr,_) -> (tr,dr,rr)
      |_ -> failwith "erreur interne"
    in
    let taille_t = getTaille t in
    (push taille_t)^se^(store taille_t d r)
  | AstPlacement.Affectation (a, e) ->
    let se = analyse_code_expression e in
    let sa = analyse_code_affectable a true "" 0 in
    se ^ sa 
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
  | AstPlacement.RetourVoid tailleParam ->
    return 0 tailleParam
  | AstPlacement.AppelProcedure (info,le) ->
    let code_args =
      List.fold_right
        (fun e acc -> analyse_code_expression e ^ acc)
        le
        ""
    in
    let nom_fonction =
      match info_ast_to_info info with
      | InfoFun(n, _, _) -> n
      | _ -> failwith "erreur interne"
    in
    code_args ^ call "SB" nom_fonction
  | AstPlacement.Empty ->
    "\n"

(* analyse_code_bloc : AstPlacement.bloc * int -> String *)
and analyse_code_bloc (li, taille) =
  List.fold_left (fun acc i -> acc ^ analyse_code_instruction i) "" li
  ^ pop 0 taille

(* analyse_code_fonction: AstPlacement.fonction -> String *)
let analyse_code_fonction f =
  match f with 
  | AstPlacement.Fonction (info, _, (li,taille)) 
  | AstPlacement.Procedure (info, _, (li,taille)) ->
  (* Récupération des infos de la fonction *)
  let (nom_fonction, _, _) = match info_ast_to_info info with
    | InfoFun(n, _, _) -> (n, 0, 0)  (* les autres valeurs ne sont pas utiles ici *)
    | _ -> failwith "erreur interne"
  in

  (* Code du bloc de la fonction. Le RETURN est généré dans le bloc lui-même. *)
  let code_li = analyse_code_bloc (li, taille) in

  label nom_fonction ^ code_li ^ halt

(* analyse_code_enum : AstPlacement.Enum -> string *)
let analyse_code_enum (AstPlacement.Enum (_ ,vals_enum)) =
  (* Pour chaque valeur, on génère une instruction loadl avec son entier *)
  let rec aux vals acc =
    match vals with
    | [] -> ""
    | info_val :: rest ->
        let code_val =
          match Tds.info_ast_to_info info_val with
          | InfoEnumVal (_, _, d, r) -> (push 1) ^ (loadl_int acc) ^ (store 1 d r)
          | _ -> failwith "analyse_code_enum : info invalide"
        in
        code_val ^ aux rest (acc + 1)
  in
  aux vals_enum 0

(* analyse : AstPlacement.programme -> String *)
(* Permet de passer d'un AstPlacement au code TAM d'un programme *)
let analyser (AstPlacement.Programme (enums, fonctions, prog)) =
  let code_entete = getEntete () in
  let code_enums = String.concat "" (List.map analyse_code_enum enums) in
  let code_func_list = List.map analyse_code_fonction fonctions in
  let code_func = String.concat "" code_func_list in
  let code_prog = label "main" ^ analyse_code_bloc prog in
  let code_total = code_enums ^ code_entete ^ code_func ^ code_prog ^ halt in
  (* Affichage pour debug *)
  (*print_endline "===== Code TAM généré =====";
  print_endline code_total;
  print_endline "============================";*)
  code_total





   