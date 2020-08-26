load "arb";;
load "abr";;

type 'a avl = V | N of 'a * 'a avl * 'a avl * int;;
(* N: racine, fils gauche, fils droit, hauteur *)

let rec to_arb = function
  | V -> arb__V
  | N(r, g, d, h) -> arb__N(r, to_arb g, to_arb d);;

let size avl = arb__size (to_arb avl);;

let rec has avl less e = match avl with
  | V -> false
  | N(r, g, d, _) -> not (less e r || less r e)
     || less e r && has g less e
     || less r e && has d less e;;

let ht = function
  | V -> -1
  | N(r, g, d, h) -> h;;

let node r g d = N(r, g, d, 1 + max (ht g) (ht d));;

(*  r      Rotation droite       gr
   / \     -------------->      /  \
  gr  d                        gg   r
 /  \      <--------------         / \
gg  gd	   Rotation gauche       gd   d *)

let rotd = function N(r, N(gr, gg, gd, _), d, _) -> node gr gg (node r gd d);;
let rotg = function N(gr, gg, N(r, gd, d, _), _) -> node r (node gr gg gd) d;;

(* assume g and d are balanced *)
let balance r g d = 
  if ht g > 1 + ht d then match g with
    | N(_, gg, gd, _) when ht gg > ht gd -> rotd (node r g d)
    | _ -> rotd (node r (rotg g) d) (* rotation gauche-droite *)
  else if ht d > 1 + ht g then match d with (* cas symetrique *)
    | N(dr, dg, dd, _) when ht dd > ht dg -> rotg (node r g d)
    | _ -> rotg (node r g (rotd d))
  else node r g d;;

let rec add avl less e = match avl with
x  | V -> N(e, V, V, -1); 
  | N(r, g, d, h) -> if less e r then balance r (add g less e) d
    else balance r g (add d less e);;

let rec del_max = function (* renvoie (max, arbre obtenu en supprimant max) *)
  | N(r, g, V, _) -> r, g 
  | N(r, g, d, _) -> let m, d' = del_max d in
		     m, balance r g d';;

let del a e = match a with
  | V -> V (* l'élément à supprimer n'a pas été trouvé *)
  | N(r, g, d, _) when e < r -> balance r (del g e) d
  | N(r, g, d, _) when e > r -> balance r g (del d e)
  | N(r, g, d, _) -> let m, g' = del_max g in
		     balance m g' d;;
     
let rec take_min = function
  | N(r, V, d, _) -> r, d
  | N(r, g, d, _) -> let m, g' = take_min g in
		  m, balance r g' d;;

let rec peek_min = function
  | N(r, V, _, _) -> r
  | N(_, g, _, _) -> peek_min g;;

let rec take_root = function
  | N(r, g, V, _) -> r, g
  | N(r, g, d, _) -> let m, d' = take_min d in
		  r, balance m g d';;

let rec del avl less e = match avl with
  | V -> V
  | N(r, g, d, _) when less e r -> balance r (del g less e) d
  | N(r, g, d, _) when less r e -> balance r g (del d less e)
  | N(r, _, _, _) -> snd (take_root avl);;

let rec of_list l less = match l with
  | [] -> V
  | e::q -> add (of_list q less) less e;;

let infixe =
  let rec aux acc = function
  | V -> acc
  | N(r, g, d, h) -> aux (r::aux acc d) g in
  aux [];;

let rec fusion g d = match g with
  | V -> d
  | N(gr, gg, gd, _) -> balance gr gg (fusion gd d);;

let rec del e = function
  | V -> V
  | N(r, g, d, _) when e = r -> fusion g d
  | N(r, g, d, _) when e < r -> balance r (del e g) d
  | N(r, g, d, _) when e > r -> balance r g (del e d);;
