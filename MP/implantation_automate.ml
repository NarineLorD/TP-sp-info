(*
TP: implantation des automates finis
 *)

type automate = {
  etats : int list ; (* ensemble des Ã©tats *)
  q0 : int; (* Ã©tat initial *)
  finaux : int list; (* liste des Ã©tats finaux *)
  (* la fonction de transition est reprÃ©sentÃ©e par la liste des
  triplets (q, a, q') tels que delta(q, a) = q' : *)
  transitions : (int * char * int) list;
}
exception Blocage


let auto a = 
  (* automate reconnaissant le langage a* où a est une chaîne de caractères*)
  {
    etats = [0;1];
    q0 = 0;
    finaux = [1];
    transitions = [(0,a,1);(1,a,1)];
  }

let auto_mot_vide = 
  (*automate reconnaissant le langage {e} où e est le mot vide*)
  {
    etats = [0];
    q0 = 0;
    finaux = [0];
    transitions = [];
  }

let auto__vide = 
  (*automate reconnaissant le langage vide*)
  {
    etats = [];
    q0 = 42;
    finaux = [17];
    transitions = [];
  }
    


let rec execute afd c = 
  let rec execute_aux l a b = match l 
    with
    |[] -> raise Blocage
    |(e,f,g)::r -> if (e,f) = (a,b) then g
                   else execute_aux r a b
  in 
  match c with
  |[] -> afd.q0
  |x::r -> let q = execute afd r in execute_aux afd.transitions q x
;;

let reconnait afd c = List.mem (execute afd c) afd.finaux;;

let est_complet c afd  = List.for_all (fun x -> List.exists (fun (a,b,c) -> a=x) afd.transitions) c;;




