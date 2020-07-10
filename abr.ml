#use "arb.ml";;

let rec has abr less e = match abr with (* O(h) *)
  | V -> false
  | N(r, g, d) -> not (less e r || less r e)
     || less e r && has g less e
     || less r e && has d less e;;

let rec size = function (* O(h) *)
  | V -> 0
  | N(r, g, d) -> 1 + size g + size d;;

let rec add abr less e = match abr with (* O(h) *) 
  | V -> N(e, V, V) 
  | N(r, g, d) -> if less e r then N(r, add g less e, d)
    else N(r, g, add d less e);;

let rec take_min = function (* O(h) *)
  | N(r, g, V) -> r, g
  | N(r, g, d) -> let m, g' = take_min g in
		  m, N(r, g', d);;
		    
let rec take_root = function (* O(h) *)
  | N(r, V, d) -> r, d
  | N(r, g, d) -> let m, d' = take_min d in
		  r, N(m, g, d');;

let rec take abr less e = match abr with (* O(h) *)
  | N(r, g, _) when less e r -> take g less e
  | N(r, g, d) when less r e-> take d less e
  | N(r, _, _) -> snd (take_root abr);;

let rec of_list l less = match l with (* O(n**2) *)
  | [] -> V
  | e::q -> add (of_list q less) less e;;

let of_vect_trie t =
  let rec aux i j =
    if i >= j then V
    else let m = (i + j) / 2 in
	 N(t.(m), aux i m, aux (m + 1) j)
  in aux 0 (vect_length t);;

let rec fusion_naif less a1 a2 = match a1 with
  | V -> a2
  | N(r, g, d) -> add (fusion_naif less g (fusion_naif less d a2)) less r;;

let rec fusion g d = match g with 
  | V -> d
  | N(gr, gg, gd) -> N(gr, gg, fusion gd d);;

let rec del e = function
  | V -> V
  | N(r, g, d) when e = r -> fusion g d
  | N(r, g, d) when e < r -> N(r, del e g, d)
  | N(r, g, d) -> N(r, g, del e d);;


