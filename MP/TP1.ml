(*La hauteur h d'un B-arbre de paramètre t est telle que
log(t) < h < log(t)+1 *)


let param =3;;
type ('k,'v) btree = 
{
  mutable keys: 'k vect;
  mutable size: int;
  mutable vals: ('k, 'v) intext;
}
and ('k, 'v) intext =
  |Values of 'v vect
  |Sons of ('k, 'v) btree vect;;

let empty() = {
    size=0;
    keys=[| |];
    vals=Values [| |];
};;


let insert_vect n v k x = 
  for i=0 to n-k+1 do
    v.(n+1-i) <- v.(n-i);
  done;
  v.(k) <- x;;

let is_full b = b.size=2*param;;

let lookup t n k =
  let r= ref 0 in
  for i = 0 to n-1 do
    if t.(i)<k then incr r;
  done;
  !r;;

let v = [|1;2;3;3;3;4;5;67;|];;

let rec find b k= 
  let i = lookup b.keys b.size k in
  match b.vals with
  |Values(v) -> if i=vect_length v then failwith"404 Not Found" 
                else v.(i)
  |Sons(s) -> find s.(i) k;;
(*find a un complexité en O(h) où h est la hauteur du B-arbre qu'on fouille*)


let split t n =
  (*pour un tableau et sa taille donnés, 
renvoie les deux sous-tableaux de longeur n/2 
contenant le début et la fin du tableau t*)
  let u=init_vect (n/2) (fun i -> t.(i)) in
  let v = init_vect (n-n/2) (fun i -> t.(n/2+i)) in
  u,v;;

let maxv t =
  (*Renvoie le maximum d'un tableau t non vide*)
  let x = ref t.(0) in
  for i = 0 to vect_length t -1 do
    if t.(i)> !x then x:=t.(i)
  done;
  !x;;
  
split v (vect_length v);;
maxv v;;

let rec insert_aux a k v =
  match v with
  |Values t -> let i = lookup a.keys a.size k in
	       insert_vect a.size t i v
  |Sons s -> let i = lookup a.keys a.size k in
	     if is_full s.(i) then
	       let n = 2*param in
	       let valg,vald = split s.(i).vals n  in
	       let kg,kd = split s.(i).keys n in
	       let ng = {keys=kg; size=param; vals=valg} in
	       let nd = {keys=kd; size=param; vals=vald} in
	       let k = maxv kg in
	       begin
		 insert_vect param a.keys (a.size-1) k;
		 
		   
	       
	       
	       
	       

