(*Tri insertion impératif*)
let swap t i j = let temp = t.(i) in
		 begin
		   t.(i) <- t.(j);
		   t.(j) <- temp;
		 end;;

let tri_insertion t = let n = vect_length t in
		      for j=0 to n-1 do
			let i  = ref j in
			while !i>0 && t.(!i)<t.(!i-1) do
			  swap t !i (!i-1);
			  decr i;
			done;
		      done;;


let mk_rand_vect n = init_vect n (fun i -> random__float 1000.);;

let t = mk_rand_vect 1000;;
tri_insertion t;;






(*Tri flash*)

type descr_classes = {nb:int; vmin:float; vmax:float};;

let minmax t = let m = ref t.(0) and M = ref t.(0) in
	       for i=0 to vect_length t -1 do
		 if t.(i) <. !m then m:=t.(i);
		 if t.(i) >. !M then M:=t.(i);
	       done;
	       !m,!M;;

minmax t;;
let decr_classes t = let u,v = minmax t in
		     let n = ceil (0.42 *. float_of_int (vect_length t)) +. 1. in
		     {nb=int_of_float n; vmin=u; vmax=v};;

let classe d x = int_of_float(float_of_int d.nb *.x/.(d.vmax-.d.vmin));;

let effectifs t d = let v = make_vect d.nb 0 in
		    for i=0 to vect_length t -1 do
		      let c = classe d t.(i) in
		      v.(c) <- v.(c)+1
		    done;
		    v;;

let debut_classe  e = let u = let n = vect_length e in
			      let u = make_vect n 0. in
			      for i=0
