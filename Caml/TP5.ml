let A = 100000;;
let N = 888;;

let tableau_test = init_vect A (fun i -> (random__int N, random__int N));;

let debroussaille t =
  let taillemax = ref 0 in
  let taille = make_vect N 1 in
  let r = init_vect N (fun i -> i) in
  let u = make_vect (A+1) 0  in
  for k = 0 to A do
    let i,j = t.(k) in
    let  nouvelle_taille = calc_taille r taille j in
    if !taillemax < nouvelle_taille then taillemax := nouvelle_taille;
    u.(k) <- !taillemax;
  done;
  u;;
  


let rec find r i = if r.(i) = i then
		     i
		   else
		     let f = find r r.(i) in
		     r.(i) <- f;
		     f;;

let calc_taille r taille i = taille.(find r i);;

let union r taille i j = let a,b = find r i, find r j in
			 if a <> b then
			   begin
			     if taille.(a) < taille.(b) then
			       begin
				 r.(a) <- b;
				 taille.(b) <- taille.(a) + taille.(b)
			       end
			     else
			       begin
				 r.(b) <- a;
				 taille.(a) <- taille.(a) + taille.(b)
			       end;
			   end;;
