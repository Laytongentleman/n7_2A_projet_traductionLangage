open Ast
open Type

(* Interface d'affichage des arbres abstraits *)
module type PrinterAst =
sig
  module A:Ast

(* string_of_expression :  expression -> string *)
(* transforme une expression en chaîne de caractère *)
val string_of_expression : A.expression -> string

(* string_of_instruction :  instruction -> string *)
(* transforme une instruction en chaîne de caractère *)
val string_of_instruction : A.instruction -> string

(* string_of_fonction :  fonction -> string *)
(* transforme une fonction en chaîne de caractère *)
val string_of_fonction : A.fonction -> string

(* string_of_ast :  ast -> string *)
(* transforme un ast en chaîne de caractère *)
val string_of_programme : A.programme -> string

(* print_ast :  ast -> unit *)
(* affiche un ast *)
val print_programme : A.programme -> unit

end

(*Module d'affiche des AST issus de la phase d'analyse syntaxique *)
module PrinterAstSyntax : PrinterAst with module A = AstSyntax =
struct

  module A = AstSyntax
  open A

  (* Conversion des opérateurs unaires *)
  let string_of_unaire op =
    match op with
    | Numerateur -> "num "
    | Denominateur -> "denom "
    
  (* Conversion des opérateurs binaires *)
  let string_of_binaire b =
    match b with
    | Fraction -> "/ " (* not used *)
    | Plus -> "+ "
    | Mult -> "* "
    | Equ -> "= "
    | Inf -> "< "

  (* Conversion des affectables *)
  let rec string_of_affectable = function
    | Ident s -> s 
    | Deref a2 -> "* " ^  (string_of_affectable a2)

  (* Conversion des expressions *)
  let rec string_of_expression e =
    match e with
    | AppelFonction (n,le) -> "call "^n^"("^((List.fold_right (fun i tq -> (string_of_expression i)^tq) le ""))^") "
    | Affectable a -> string_of_affectable a^" "
    | Booleen b -> if b then "true " else "false "
    | Entier i -> (string_of_int i)^" "
    | Unaire (op,e1) -> (string_of_unaire op) ^ (string_of_expression e1)^" "
    | New t -> "(new " ^ (string_of_type t) ^ ") "
    | Null -> "NULL "
    | Adresse s -> "& " ^ s^" " 
    | Ref id -> "ref " ^ id
    | EnumE s -> s ^ " "
    | Binaire (b,e1,e2) ->
        begin
          match b with
          | Fraction -> "["^(string_of_expression e1)^"/"^(string_of_expression e2)^"] "
          | _ -> (string_of_expression e1)^(string_of_binaire b)^(string_of_expression e2)^" "
        end

  (* Conversion des instructions *)
  let rec string_of_instruction i =
    match i with
    | Declaration (t, n, e) -> "Declaration  : "^(string_of_type t)^" "^n^" = "^(string_of_expression e)^"\n"
    | Affectation (a,e) ->  "Affectation  : "^(string_of_affectable a)^" = "^(string_of_expression e)^"\n"
    | Constante (n,i) ->  "Constante  : "^n^" = "^(string_of_int i)^"\n"
    | Affichage e ->  "Affichage  : "^(string_of_expression e)^"\n"
    | Conditionnelle (c,t,e) ->  "Conditionnelle  : IF "^(string_of_expression c)^"\n"^
                                  "THEN \n"^((List.fold_right (fun i tq -> (string_of_instruction i)^tq) t ""))^
                                  "ELSE \n"^((List.fold_right (fun i tq -> (string_of_instruction i)^tq) e ""))^"\n"
    | TantQue (c,b) -> "TantQue  : TQ "^(string_of_expression c)^"\n"^
                                  "FAIRE \n"^((List.fold_right (fun i tq -> (string_of_instruction i)^tq) b ""))^"\n"
    | Retour (e) -> "Retour  : RETURN "^(string_of_expression e)^"\n"
    | RetourVoid -> "Retour : return;\n"
    | AppelProcedure (n,le) -> "AppelProc : " ^ n ^ "(" ^ (String.concat ", " (List.map string_of_expression le)) ^ ");\n"
  
  (*Conversion de paramètres *)
  let string_of_param (is_ref, t, n) =
    (if is_ref then "ref " else "")
    ^ (string_of_type t) ^ " " ^ n

  (* Conversion des fonctions *)
  let string_of_fonction (Fonction (t, n, lp, li)) =
    (string_of_type t) ^ " " ^ n ^ " ("
    ^ (String.concat ", " (List.map string_of_param lp))
    ^ ") = \n"
    ^ (String.concat "" (List.map string_of_instruction li))
    ^ "\n"

  (* Conversion des déclarations d'énumérations *)
  let string_of_enum (Enum(n, members)) =
    "enum " ^ n ^ " { " ^ (String.concat ", " members) ^ " };\n"

  (* Conversion d'un programme Rat complet *)
  let string_of_programme (Programme (enums, fonctions, instructions)) =
    (String.concat "" (List.map string_of_enum enums)) ^ "\n" ^
    (String.concat "\n" (List.map string_of_fonction fonctions)) ^ "\n" ^
    "Main :\n" ^ (String.concat "" (List.map string_of_instruction instructions))
    
  (* Affichage d'un programme Rat *)
  let print_programme programme =
    print_string "AST : \n";
    print_string (string_of_programme programme);
    flush_all ()

end
