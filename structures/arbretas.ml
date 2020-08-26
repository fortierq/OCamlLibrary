load "arb";;
#open "arb";;
load "abr";;
load "draw";;

(*  r      Rotation droite       gr
   / \     -------------->      /  \
  gr  d                        gg   r
 /  \      <--------------         / \
gg  gd	   Rotation gauche       gd   d *)

let rotd = function N(r, N(gr, gg, gd), d) -> N(gr, gg, N(r, gd, d));;
let rotg = function N(gr, gg, N(r, gd, d)) -> N(r, N(gr, gg, gd), d);;
let rotd (N(r, N(gr, gg, gd), d)) = N(gr, gg, N(r, gd, d));;
let prio = function
  | V -> max_int
  | N(r, g, d) -> snd r;;
      
let rec add a e = match a with
  | V -> N(e, V, V)
  | N(r, g, d) when e < r -> let g' = add g e in
			     if snd r < prio g' then N(r, g', d)
			     else rotd (N(r, g', d))
  | N(r, g, d) -> let d' = add d e in
		  if snd r < prio d' then N(r, g, d')
		  else rotg (N(r, g, d'));;
 
let rec del a e = match a with
  | N(r, g, d) when e < fst r -> N(r, del g e, d)
  | N(r, g, d) when fst r < e -> N(r, g, del d e)
  | N(r, V, V) -> V (* e = r *)
  | N(r, g, d) -> del (if prio g < prio d then rotd a else rotg a) e;;

let rec of_list = function
  | [] -> V
  | e::q -> add (of_list q) e;;

let a = of_list [4, 1; 2, 4; 1, 6; 7, 6; 5, 8; 9, 9; 6,0];;
draw__arb a (fun (x, y) -> string_of_int x  ^ string_of_int y);;
draw__arb (del a 6) (fun (x, y) -> string_of_int x  ^ string_of_int y);;
draw__arb (del (del a 6) 7) (fun (x, y) -> string_of_int x  ^ string_of_int y);;

