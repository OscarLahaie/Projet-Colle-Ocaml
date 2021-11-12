#use "bdd.ml"
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

(*Début des fonctions d'analyses*)

let rec count_elements liste element = 
  match liste with
  | [] -> 0
  | t :: q when t = element -> 1 + count_elements q element
  | _ :: q -> count_elements q element 
;;

let rec plusieurs_symbols liste =
  match ponctuation_string with
  | []  -> false
  | t :: q -> (count_elements liste t >= 2) || plusieurs_symbols q 
;;

(*Fin des fonctions d'analyses*)

(*Divers*)

let element_hasard liste =
  let index = Random.int (List.length liste) in
  let rec pick liste index =
    match liste with
    | [] -> failwith("liste vide")
    | t :: [element] -> element
    | t :: q when index = 0 -> t
    | t :: q -> pick q (index - 1)
  in pick liste index
;;

let rec contient_element liste element =
  match liste with
  | [] -> false
  | t :: q when t = element -> true
  | t :: q -> contient_element q element
;;

(*Fin Divers*)

(*Système*)
exception Fini;;
exception Ban;;
let ecoute_le_patient () =
  let () = print_string ">> " in
  read_line ()
;;

let message s = print_endline s;;

let bonjour () = 
  message (element_hasard hello)
;;

let au_revoir () =
  message (element_hasard goodbye)
;;

let renvoie () =
  message (element_hasard banning_phrase)
;;

let fin phrase =
  let rec fin_aux fins =
  match fins with
  | [] -> false
  | t :: q when contient_element phrase t -> true
  | t :: q -> fin_aux q
  in fin_aux phrase_de_fin
;;

let repond_au_patient reponse =
  let phrase = decompose_phrase reponse in
  if fin phrase then
    raise Fini
  else
    (* ToDo : supprimer la ligne suivante et décommenter ce qui suit *)
    let reponses_possibles = ["I can't speak yet..."] in
    let () = print_newline () in
    let () = message (List.hd reponses_possibles) in
    print_newline ()
;;

let colleur () =
  let () = bonjour () in
  let () = message (element_hasard demande_nom) in
  let nom = ecoute_le_patient () in
  let rec boucle_interactive () =
    repond_au_patient (ecoute_le_patient ());
    boucle_interactive ()
  in
  try
    boucle_interactive ()
  with
  | Fini -> au_revoir ()
  | Ban -> renvoie ()
  | End_of_file | Sys.Break ->
     let () = message "\n\n\nYou could be polite and say goodbye to me ...\n\n\n" in
     au_revoir ()
;;

if !Sys.interactive then
  ()
else
  let () = Sys.catch_break true in
  let () = colleur () in
  exit 0
;;

(*Fin Système*)