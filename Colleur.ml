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
   if phrase <> ""
     then
     let rec decompose_aux phrase2 = 
       match phrase2 with
       | [] -> []
       | t :: q when t = "" -> decompose_aux q
       | t :: q -> t :: decompose_aux q
     in decompose_aux (String.split_on_char ' ' (espace_symbols_phrase phrase))
   else 
     [""]
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
  if liste = [] then failwith("liste vide")
  else
   let () = Random.self_init () in
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
 
 let rec contient_tout phrase mots_clefs = 
   match mots_clefs with
   | [] -> true
   | t :: q when contient_element phrase t -> true && contient_tout phrase q
   | _ -> false
 ;;
 
 let rec contient_partiel phrase mots_clefs =
   match mots_clefs with
   | [] -> false
   | t :: q when contient_element phrase t -> true || contient_partiel phrase q
   | _ :: q -> contient_partiel phrase q
 ;;
 
 let rec supprime_element liste element =
   match liste with
   | [] -> []
   | t :: q when t = element -> q
   | t :: q -> t :: (supprime_element q element)
 ;;
 
 let rec remplacer_prenom prenom phrase =
     match (String.sub phrase 0 1) with
     | partie when partie = "$" -> prenom ^ (String.sub phrase 1 (String.length phrase - 1)) 
     | partie when (String.length phrase) = 1 -> partie
     | partie -> partie ^  remplacer_prenom prenom (String.sub phrase 1 (String.length phrase - 1)) 
 ;;
 
 
 (*Fin Divers*)
 
 (*Système*)
 exception Fini of int;;
 exception Ban;;
 let ecoute_eleve () =
   let () = print_string ">> " in
   read_line ()
 ;;
 
 let message s = print_endline s;;
 
 let bonjour () = 
   message (element_hasard hello)
 ;;
 
 let demande_prenom () = 
   let () = message(element_hasard demande_nom) in
   ecoute_eleve()
 
 let au_revoir prenom note =
   let () = message("Your score is : " ^ (string_of_int note) ^ "/20.")
   in message (remplacer_prenom prenom (element_hasard goodbye))
 ;;
 
 let renvoie () =
   message (element_hasard banning_phrase)
 ;;
 
 let afficher_question liste = 
   let question_phrases = List.hd liste in 
   let phrase = List.hd question_phrases in
   let aux = List.nth question_phrases 1 in
   let () = message (phrase) in 
   message (aux)
 ;;
 
 let fin phrase =
   let rec fin_aux fins =
   match fins with
   | [] -> false
   | t :: q when contient_element phrase t -> true
   | t :: q -> fin_aux q
   in fin_aux phrase_de_fin
 ;;
 
 let questionne question note prenom =
   let () = afficher_question question in
   let phrase = decompose_phrase (ecoute_eleve ()) in
   if fin phrase then
     raise (Fini note)
   else
     
     let couple = 
       if contient_tout phrase (List.nth question 1)
         then (remplacer_prenom prenom (element_hasard good), 2)
       else if contient_partiel phrase (List.nth question 1)
         then (remplacer_prenom prenom (element_hasard medium), 1)
       else
         (remplacer_prenom prenom (element_hasard not_good), 0)
     in
     let (reponse, points) = couple in
     let () = print_newline () in
     let () = message (reponse) in
     let () = print_newline () in
     points
   ;;
   
   let colleur () =
     let () = print_newline () in
     let () = bonjour () in
     let prenom = demande_prenom () in
     let () = print_newline () in
     let rec boucle_interactive nb_questions questions prenom note =
       if (nb_questions > 0) 
       then
         let question = (element_hasard questions) in
         let points = questionne question note prenom in
         boucle_interactive (nb_questions - 1) (supprime_element questions question) prenom (note + points)
       else
         au_revoir prenom note 
     in
   
     try
       boucle_interactive 5 question_answer prenom 0
     with
     | Fini note -> au_revoir prenom (note)
     | Ban -> renvoie ()
     | End_of_file | Sys.Break ->
        let () = message "\n\n\nYou could be polite and say goodbye to me ...\n\n\n" in
        au_revoir prenom (0)
   ;;
   
   if !Sys.interactive then
     ()
   else
     let () = Sys.catch_break true in
     let () = colleur () in
     exit 0
   ;;
   
   (*Fin Système*)