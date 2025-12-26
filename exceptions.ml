open Type
open Ast.AstSyntax

(* Exceptions pour la gestion des identificateurs *)

(* Un identifiant est déclaré plus d’une fois dans la même portée *)
exception DoubleDeclaration of string 

(* Utilisation d’un identifiant qui n’a pas été déclaré dans la portée courante ou englobante *)
exception IdentifiantNonDeclare of string 

(* L’identifiant est utilisé dans un contexte incompatible avec sa nature
   (ex : variable utilisée comme fonction, fonction utilisée comme variable, etc.) *)
exception MauvaiseUtilisationIdentifiant of string 

(* Exceptions pour le typage *)

(* Le type réel d’une expression ne correspond pas au type attendu *)
exception TypeInattendu of typ * typ     
(* (type réel, type attendu) *)

(* Les types des paramètres effectifs ne correspondent pas
   aux types des paramètres formels attendus *)
exception TypesParametresInattendus of typ list * typ list
(* (types réels, types attendus) *)

(* Les types des opérandes ne sont pas compatibles avec l’opérateur binaire utilisé *)
exception TypeBinaireInattendu of binaire * typ * typ
(* (opérateur, type opérande gauche, type opérande droit) *)

(* Une expression de type void est utilisée dans un contexte
   où une valeur est attendue *)
exception TypeVoidInattendu 

(* Tentative de déréférencement d’une expression qui n’est pas une référence *)
exception DereferencementIllegal

(* Mauvaise utilisation d’une référence (ex : affectation, passage de paramètre, etc.) *)
exception MauvaiseUtilisationRef

(* Un paramètre passé par valeur est utilisé alors qu’un passage par référence est attendu *)
exception ParametreRefAttendu of string

(* Un paramètre passé par référence est utilisé alors qu’un passage par valeur est attendu *)
exception ParametreNonRef of string



(* Exceptions pour les énumérations *)

(* Utilisation d’une valeur qui n’appartient pas à l’énumération correspondante *)
exception ValeurEnumInexistante of string


(* Exceptions pour les fonctions et procédures *)

(* Instruction return sans expression dans une fonction
   qui doit obligatoirement retourner une valeur *)
exception RetourVideDansFonction

(* Instruction return avec une expression dans une procédure
   qui ne retourne pas de valeur *)
exception RetourNonVideDansProcedure

(* Une fonction est appelée comme une procédure
   (appel en tant qu’instruction sans utiliser la valeur de retour) *)
exception AppelFonctionPourProcedure of string

(* Une procédure est appelée comme une fonction
   (appel dans une expression alors qu’elle ne retourne rien) *)
exception AppelProcedurePourFonction of string


(* Utilisation illégale de return dans le programme principal *)

(* Présence d’une instruction return dans le corps du programme principal *)
exception RetourDansMain

