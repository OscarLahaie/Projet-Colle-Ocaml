(* Début des fonctions de décompositions *)

let ponctuation_char = [' '; '\n'; '.'; ','; ';'; '-'; '!'; '?'];;
let ponctuation_string = [" "; "\n"; "."; ","; ";"; "-"; "!"; "?"];;

let rec contient_element_caractere phrase liste = 
  match liste with
  | [] -> false
  | t :: _ when String.contains phrase t -> true
  | _ :: q -> contient_element_caractere phrase q 
;;

let rec minuscule phrase =
  String.lowercase_ascii phrase
;;

let rec espace_autour partie_de_phrase liste =
  match liste with
  | [] -> partie_de_phrase
  | t :: q when t = partie_de_phrase -> " " ^ t ^ " "
  | _ :: q -> espace_autour partie_de_phrase q
;;

let espace_symbols_phrase phrase =
  let rec iter phrase2 =
    match (String.sub phrase2 0 1) with
    | partie when String.length phrase2 = 1 -> (espace_autour partie ponctuation_string)
    | partie  -> (espace_autour partie ponctuation_string) ^  iter (String.sub phrase2 1 (String.length phrase2 - 1)) 
  in iter phrase 
;;

let decompose_phrase phrase =
  let phrase = minuscule phrase in
  let rec decompose_aux phrase2 = 
    match phrase2 with
    | [] -> []
    | t :: q when t = "" -> decompose_aux q
    | t :: q -> t :: decompose_aux q
  in decompose_aux (String.split_on_char ' ' (espace_symbols_phrase phrase))
;;

(*Fin des fonctions de décompositions*)
