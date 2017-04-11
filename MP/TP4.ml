(*TP sur la déterminisation des automates finis*)



(*I./Fonctions générales sur les ensemble finis*)

type 'a ensf == 'a list;;
(*Les ensembles sont représentés par des listes triées.*)

(*Renvoie l'intersection des deux ensemble a et b.*)  
let rec (inter: 'a ensf -> 'a ensf -> 'a ensf) =
  fun a b -> match a,b with 
             |[],_ -> []
             |_,[] -> []
             |x::r,y::t -> if x==y then x::(inter r t)
                           else inter r t;;
(*Rennvoie la réunion des deux ensembles a et b.*)
let rec (union: 'a ensf -> 'a ensf -> 'a ensf) = 
  fun a b -> match a,b with
             |[],_ -> b
             |_,[] -> a
             |x::r, y::t -> if x<y then x::(union r b)
                            else if x>y then y::(union a t)
                            else x::(union r t);;

(*Si t est une liste d'ensemble, union_liste de t renvoie la réunion des éléments de t.*)  
let rec union_liste t = 
  match t with
  |[] -> []
  |[a] -> a
  |x::y::r -> union (union x y) (union_liste r);;


let union_fun t f = union_liste (map f t);;

(*Renvoie l'ensemble (trié) des éléments d'uns liste.*)
let rec ensemble l  = union_fun l (fun x -> [x]);;

(*ajout d'un élément à un ensemble*)
let (ajoute: 'a -> 'a ensf -> 'a ensf) = fun x t ->
  match t with
  |[] -> [x]
  |y::r -> if x<y then x::t
           else if x>y then y::x::r
           else t;;

(*Si e est un ensemble, parties e renvoie l'ensemble des parties de e.*)
let rec (parties: 'a ensf -> 'a ensf ensf) = fun e ->  
  match e with
  |[] -> [[]]
  |x::r -> let p1 = parties r in (*parties de e qui ne contiennent pas x.*)
           let p2 = (map (fun s -> ajoute x s) p1) in (* parties qui contiennent x.*)
           union p1 p2;;(*parties e est l'union des deux*)



(*II./Fonctions générales  sur les automates finis non déterministes*)

type ('a, 'b) automate =
  {alphabet: 'a ensf;
   etats: 'b ensf;
   initiaux: 'b ensf;
   finaux: 'b ensf;
   transitions: ('b*'a*'b) ensf;
  };;

type 'a mot == 'a list;;

let (execute_lettre:('a, 'b) automate -> 'a -> 'b -> 'b ensf) = fun auto etats lettre ->
  let transi = auto.transitions in
  let rec execute_aux t e l= 
    (*calcule l'ensemble de états atteins partant de l'état e en executant la lettre l*)
    match t with
    |[] -> []
    |(a,b,a')::r -> if a=e && b=l then ajoute a' (execute_aux r e l)
                   else execute_aux r e l
  in
  execute_aux transi etats lettre;;

let rec execute_mot auto etats mot =
  match mot with
  |[] ->  etats
  |x::r -> let suiv = union_fun etats (fun e -> execute_lettre auto e x) in
           (*ensemble des états atteints en executant la première lettre du mot*)
           execute_mot auto suiv r;;


let est_reconnu auto mot = 
  let depart = auto.initiaux in
  execute_mot auto depart mot;;















