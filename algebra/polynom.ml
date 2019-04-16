open Complex;;

let ( +! ) = add;;
let ( -! ) = sub;;
let ( *! ) = mul;;
let ( /! ) = div;;

let c0 = {re = 0.; im = 0.};;
let float_to_c r = {re = r; im = 0.};;

let rec fft w p = (* renvoie p(w^0)::p(w^1)::... *)
  if List.tl p = [] then p 
  else let rec split = function
  | [] -> [], []
  | [a] -> failwith "le degré doit être une puissance de 2"
  | a::b::q -> let l1, l2 = split q in
	       a::l1, b::l2 in
       let p1, p2 = split p in
       let wcarre = w *! w in
       let dft1, dft2 = fft wcarre p1, fft wcarre p2 in
       let rec dft wk = function
	 | [], [] -> []
	 | y1::q1, y2::q2 -> (y1 +! (wk *! y2))::dft (w *! wk) (q1, q2) in
       dft {re = 1.; im = 0.} (dft1, dft2) @ dft {re = -1.; im = 0.} (dft1, dft2);;

let puiss2 p = (* rajoute des 0 pour que la taille devienne une puissance de 2 *)
  let rec aux puiss t = function
    | [] -> if t = 0 then [] else c0::aux puiss (t-1) []
    | a::q -> if t = 0 then a::aux (2*puiss) (puiss-1) q
      else a::aux puiss (t - 1) q in
  aux 1 1 p;;

let mult_fft p1 p2 =
  let pp1, pp2 = puiss2 ((puiss2 p1)@[c0]), puiss2 ((puiss2 p2)@[c0]) in (* double la taille pour avoir le bon degré du produit *)
  let n = List.length pp1 in
  let w = exp {re = 0.; im = 2.*.3.14159265 /. float n} in
  let dft1, dft2 = fft w pp1, fft w pp2 in
  List.map (fun x -> x /! {re = float n; im = 0.}) (fft (Complex.conj w) (List.map2 (fun x y -> x*!y)  dft1 dft2));;

let rec lf_to_lc = function
  | [] -> []
  | e::q -> float_to_c e::lf_to_lc q;;

mult_fft (lf_to_lc (1.::2.::[])) (lf_to_lc (2.::3.::[]));;
