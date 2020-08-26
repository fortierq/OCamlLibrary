(* arbres enracinés quelconques *)

type 'a arb_n = N of 'a * 'a arb_n list;;

let rec taille a = match a with
  | N(_, []) -> 1
  | N(r, e::q) -> taille e + taille (N(r, q));;

let rec hauteur a = match a with
  | N(_, []) -> 0
  | N(r, e::q) -> max (1 + hauteur e) (hauteur (N(r, q)));;

(* tests *)
let a = (N(1, [N(2, []); N(3, []); N(4, [N(5, []); N(6, [])])]));;
taille a;;
hauteur a;;
