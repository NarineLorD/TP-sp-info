(*I Quelques fonctions essentielles sur les listes *)

(*1*)
let rec list_length l = match l with [] -> 0
				|_::r -> list_length r + 1
;;

(*2*)
let rec append l1 l2 = match l1 with [] -> l2
				    |x::r -> x::(append r l2)
;;


let rec rev l = (*calcule (rev u @ v*)
  let rec rev_append u v  = match u with -> []
				      |x::r -> x::v
		in
		rev_append l []
;;


;;

let rec map f l = match l with [] -> []
			  |x::r -> (f x)::(map f r)
;;


  
let rec filter l f =  match l with [] -> []
			      |x::r -> if f x then x::(filter r f) else filter r f
;;

  

let rec flat_map f l = match l with [] -> []
				   |x::r -> append (f x) (flat_map f r)
;;

(* int -> int list
pour a > 0
 div a renvoie la liste des diviseurs positifs de a*)
let div a =
  (*div_aux calcule l'ensemble des diviseurs positifs de a plus petits que b*)
  let rec div_aux a b = if b = 0 then []
		  else if a mod b = 0
		      then b::(div_aux a (b-1))
		      else div_aux a (b-1)
  in div_aux a a;;



let rec for_all p l = match l with [] -> true
			      |x::r ->  (p x) && (for_all p r)
;;
  
let rec exists f l = match l with [] -> false
				  |x::r -> f x || exists f r
;;


let rec mem a l = match l with [] -> false
			      |x::r -> a=x || mem a r
;;


(*II D'autrse fonctions sur les listes*)

let rec it_list f a l = match l with [] -> a
				    |x::r -> it_list f (f a x) r;;

let rec list_it f l b = match l with [] -> b
			       | x::r -> f x (list_it f r b);;

let sum l = it_list (fun x y -> x + y) 0 l;;

let length_list l = it_list (fun x y -> x+1) 0 l;;

let rev l = it_list (fun x y -> y::x) [] l;;

let append l1 l2 = let f = fun x r -> x::r in
		 list_it f l1 l2;;

let map f l = list_it (fun x e-> (f x)::e) l [];;

let filter p l = let f x l = if p x then x::l else l in
		 list_it f l [];;

let a = [0;1;2;3;4];;
let b = [10;11;12;13;14];;
rev a;;
append a b;;
let f x = x+10;;
map f a;;
let g x = x mod 3 = 0;;
filter g a;;


let range n = let rec  aux n m l = if m=0 then l
			      else aux n (m-1) ((m-1)::l) in
	      aux n n []
;;

let rec random_list n p = if n=0 then []
		      else (random__int p )::(random_list (n-1) p)
;;

let rec est_triee l = match l with [] -> true
			      |[x] -> true
			      |x::y::r -> x<=y && est_triee(y::r)
