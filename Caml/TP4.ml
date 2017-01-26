let rec inter a b = match a,b with
  |[],_ -> []
  |_,[] -> []
  |x::r,h::t -> if x<h then inter r b
		else if x>h then inter a t
		else x::(inter r t);;

let rec union a b = match a,b with
  |[],_ -> b
  |_,[] -> a
  |x::r,h::t -> if x<h then x::(union r b)
		else if x>h then h::(union a t)
		else x::(union r t);;

let rec union_liste t = match t with
  |[] -> []
  |[x] -> x
  |x::y::r -> let e = union_liste r in
	      union (union x y) e;;

let union_fun e f = union_liste (map f e);;

let ensemble l = match l with
  |[] -> []
  |x::r -> let p = ensemble r in
	   match p with
	   |[] -> [x]
	   |f::t -> if x<f then x::p
		    else if f<x then f::x::t
		    else p;;

let rec ajoute x l = match l with
  |[] -> [x]
  |a::r -> if x=a then l else a::(ajoute x r);;

let rec parties e = match e with
  |[] -> [[]]
  |x::r -> let p = parties r in
	   union p (union_fun p (fun t -> [ajoute x t]));;

  
