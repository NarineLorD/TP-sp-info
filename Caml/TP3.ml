type automate = {
  etats : int list ; (* ensemble des Ã©tats *)
  q0 : int; (* Ã©tat initial *)
  finaux : int list; (* liste des Ã©tats finaux *)
  (* la fonction de transition est reprÃ©sentÃ©e par la liste des
  triplets (q, a, q') tels que delta(q, a) = q' : *)
  transitions : (int * char * int) list;
};;
exception Blocage;;


let automate_a =
  { etats = [0;1;2];
    q0=0;
    finaux = [2];
    transitions = [(0,'a',1);(1,'a',1);(1,'',2)]
  };;

let automate_motvide =
  {etats = [0;1];
   q0=0;
   finaux=[1];
   transitions = [(0,"",1)]
  };;

let rec trouve_transi trans i c = match trans with
  |[] -> raise Blocage
  |(a,b,d)::l -> if a=i && b=c then d else trouve_transi l i c;;
let rec est_dans l c = match l with
  |[] -> false
  |x::r -> x=c && est_dans r c;;

let execute a u = match a with
  |{etats=et;q0=q;finaux=fin;transitions=trans} -> let n = string_length u in
			let x = ref q in
			for i = 0 to n-1 do
			  x := trouve_transi trans i u.[i];
			done;
			  est_dans fin !x;;
