load "arb";;

type 'a arn = V | R of 'a * 'a arn * 'a arn | N of 'a * 'a arn * 'a arn;;

let noircir a = match a with
  | R(r, g, d) -> N(r, g, d)
  | _ -> a;;

let rouge = function
  | R(_, _, _) -> true
  | _ -> false;;

(*  r      Rotation droite       gr
   / \     -------------->      /  \
  gr  d                        gg   r
 /  \      <--------------         / \
gg  gd	   Rotation gauche       gd   d *)

let rotd = function R(r, R(gr, gg, gd), d) -> R(gr, gg, R(r, gd, d));;
let rotg = function R(gr, gg, R(r, gd, d)) -> R(r, R(gr, gg, gd), d);;

let rec balance (N(r, g, d)) = match g, d with
  | R(gr, gg, gd), d when rouge gg -> R(gr, noircir gg, N(r, gd, d))
  | R(gr, gg, gd), d when rouge gd -> balance (N(r, rotg g, d))
  | g, R(dr, dg, dd) when rouge dd -> R(dr, N(r, g, dg), noircir dd)
  | g, R(dr, dg, dd) when rouge dg -> balance (N(r, g, rotd d))  
  | _, _ -> N(r, g, d);;

let rec add a e = match a with
  | V -> R(e, V, V)
  | N(r, g, d) when e < r -> balance (N(r, add g e, d))
  | R(r, g, d) when e < r -> R(r, add g e, d)
  | N(r, g, d) -> balance (N(r, g, add d e))
  | R(r, g, d) -> R(r, g, add d e);;

let rec to_arb = function
  | V -> arb__V
  | R(r, g, d) -> arb__N(`R`, to_arb g, to_arb d)
  | N(r, g, d) -> arb__N(`N`, to_arb g, to_arb d);;
 
let rec of_list l = match l with
  | [] -> V
  | e::q -> add (of_list q) e;;


let rec has abr less e = match abr with
  | V -> false
  | N(r, g, d) -> not (less e r || less r e)
     || less e r && has g less e
     || less r e && has d less e
  | R(r, g, d) -> not (less e r || less r e)
     || less e r && has g less e
     || less r e && has d less e;;
