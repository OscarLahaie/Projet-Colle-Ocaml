let ponctuation = [' '; '\n'; '.'; ','; ';'; '-'; '!'; '?'];;

let rec contient_element_caractere phrase liste = 
  match liste with
  | [] -> false
  | t :: _ when String.contains phrase t -> true
  | _ :: q -> contient_element_caractere phrase q 
;;

let separateur_de_phrase phrase element =
  let rec separateur_aux n =
  match phrase with
  | "" -> ""
  | phrase when (String.sub phrase n  1) = element  -> (String.sub phrase 0 n) ^ " " ^ element ^ " " ^(String.sub phrase (n+1) (String.length phrase - n - 1))
  | phrase -> separateur_aux (n + 1)
  in separateur_aux 0
;;
;;

let rec diviseur_de_phrase phrase =
  let liste_de_mots = String.split_on_char ' ' phrase in 
  let rec extraction_de_symbole mot_en_liste =
    match mot_en_liste with
    | [] -> []
    | t :: q when contient_element_caractere t ponctuation -> 
