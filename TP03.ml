let rec distance s1 s2 = let l1,l2 = string_length s1-1, string_length s2-1 in
			 if l1 = 0 then l2
			 else if l2 = 0 then l1
			 else let e1,e2 = sub_string s1 0 l1, sub_string s2 0 l2 in
			      let c  = if s1.[l1]=s2.[l2] then 0 else 1 in
			      min (1+ distance e1 s2) (min (1+distance s1 e2) (c+distance e1 e2));;

type mot == string*int;;

let rec distance2 ((u,p):mot) ((v,q):mot) = if p = 0 then q
					else if q = 0 then p
					else let c = if v.[q-1] = u.[p-1] then 0 else 1 in
					     min (1+distance2 (u,(p-1)) (v,q)) (min (1+distance2 (u,p) (v,(q-1)) ) (c+distance2 ((u,p-1)) (v,q-1)));;


let distance_memo ((u,p):mot) ((v,q):mot) = let M = make_matrix (p+1) (q+1) (-1) in
					    (*mot -> mot -> int 
                                              dist_aux a b renvoie la distance entre a et b*)
					    let rec dist_aux ((u,p):mot) ((v,q):mot) =
					      if p = 0 then q
					      else if q = 0 then p
					      else let c = if u.[p-1] = v.[q-1] then 0 else 1 in
						   min (1+dist_aux_mem (u,(p-1)) (v,q)) (min (dist_aux_mem (u,(p-1)) (v,q) + 1) (c+dist_aux_mem (u,(p-1)) (v,(q-1))))
                                            (*mot -> mot -> int renvoie la distance entre deux mots*)
					     and dist_aux_mem ((u,p):mot) ((v,q):mot) =
					       if M.(p).(q) = -1 then let x = dist_aux (u,p) (v,q) in
								      begin
								      M.(p).(q) <- x;
								      x;
								      end
								      
					       else M.(p).(q) in
					    dist_aux (u,p) (v,q);;
distance_memo ("courant",7) ("satan",5);;
