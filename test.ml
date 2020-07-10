(* listes *)

load "l2c";;

let l = l2c__new 1;;
l2c__to_list l;;
l2c__add l 2;;
l2c__add l 3;;
l2c__mem 0 l;;
l2c__mem 1 l;;
l2c__mem 2 l;;
l2c__to_list l;;

let l2 = l2c__new 7;;
l2c__add l2 8;;
l2c__fusion l l2;;
l2c__to_list l;;

(* arbres *)

load "arb";;
load "abr";;
load "avl";;
load "arn";;
load "draw";;
load "util";;

(* affichage d'ABR/AVL/ARN construits au hasard *)
let l = util__list_shuffle (util__list_range 0 50);;
let a = abr__of_list l (prefix <);;
arb__diam a;;
draw__arb a string_of_int;;
draw__arb (snd (arb__reroot arb__V a)) string_of_int;;
let a = abr__of_list l (prefix <) in
let g = graph__undirect (graph__of_adj_list 50) in graph__of_arb g a; 
util__write_file "ex_abr.dot" (graph__to_dot g string_of_int); 
draw__arb a string_of_int;; 
let avl = avl__of_list l (prefix <) in 
draw__arb (avl__to_arb avl) string_of_int;;
let arn = arn__of_list l (prefix <) in 
draw__arb (arn__to_arb arn) string_of_char;;

(* affichage d'ABR/AVL/ARN construits au hasard *)
let l = (util__list_range 0 10);;
let a = abr__of_list l (prefix <) in
draw__arb a string_of_int;; 
let avl = avl__of_list l (prefix <) in 
draw__arb (avl__to_arb avl) string_of_int;;
let arn = arn__of_list l (prefix <) in 
draw__arb (arn__to_arb arn) string_of_char;;

let compare_height l =
  let rel_ht a =
    let ht_opt = floor (util__log2 (float_of_int (list_length l))) in
    float_of_int (arb__ht a) /. ht_opt in
  let t = sys__time () in
  let a = abr__of_list l (prefix <) in
  print_string "(abr) temps construction: "; print_float (sys__time () -. t);
  print_string " hauteur relative: "; print_float (rel_ht a);
  let t = sys__time () in
  let a = avl__of_list l (prefix <) in
  print_string "\n(avl) temps construction: "; print_float (sys__time () -. t);
  print_string " hauteur relative: "; print_float (rel_ht (avl__to_arb a));
  let t = sys__time () in
  let a = arn__of_list l (prefix <) in
  print_string "\n(arn) temps construction: "; print_float (sys__time () -. t);
  print_string " hauteur relative: "; print_float (rel_ht (arn__to_arb a));; 

let compare_time l = 
  (let t = sys__time () in
  let a = abr__of_list l (prefix <) in
  do_list (fun x -> abr__has a (prefix <) x; ()) l;
  sys__time () -. t), 
  (let t = sys__time () in
  let a = avl__of_list l (prefix <) in
  do_list (fun x -> avl__has a (prefix <) x; ()) l; 
  sys__time () -. t),
  (let t = sys__time () in
  let a = arn__of_list l (prefix <) in
  do_list (fun x -> arn__has a (prefix <) x; ()) l; 
  sys__time () -. t);;

(* hauteur d'ABR/AVL construit au hasard, sur hauteur optimale *)
let n = 60000 in
let l = util__list_rdm n n in
compare_time l;;

compare_height (util__list_shuffle (util__list_range 0 50000));; (* hauteur d'ABR/AVL construit avec permutation aleatoire, sur hauteur optimale *)

(* hauteur d'ABR/AVL construit avec une liste croissante *)
compare_height (util__list_range 0 6000);;

(* set & dictionnaires *)

load "set";;
#open "set";;

let doublons l =
  let s = new_set () in 
  let rec aux = function
    | [] -> []
    | e::q -> if s.has e then aux q
              else (s.add e;
	            e::aux q)
  in aux l;;

let freq l = (* renvoie l'élément le plus frequent dans l *)
  let d = of_avl V in
  let rec add_d = function
    | [] -> ()
    | e::q -> let g = d.get e in
	      if g <> [] then begin d.del e; d.add (e, hd g + 1) end
	      else d.add (e, 1)
  in add_d l; (* ajoute les elements et leur frequence dans le dico *)
  let rec maxi = function
    | [e] -> d.get e, e
    | e::q -> let m2, e2 = maxi q and m1 = d.get e in
	      if m1 > m2 then m1, e else m2, e2
  in snd (maxi l);; (* on renvoie la clé de valeur maximum *)

		
load "dico";;
#open "dico";;
load "avl";;
#open "avl";;

(* stocke dans di les temps de vol pour a <= nmax *)
let syracuse di nmax =
  let rec temps_vol a =
    let l = di.get a in 
    if l <> [] then hd l
    else let next = if a mod 2 = 0 then a/2 else 3*a + 1 in
	 let res = 1 + temps_vol next in
	 di.add a res;
	 res
  in di.add 1 0;
  for a = 2 to nmax do temps_vol a done;;

let di = dico_create ();; 

let rec t a = match di.get a with (* t a renvoie le temps de vol pour u0 = a *)
  | [] -> let res = 1 + t (if a mod 2 = 0 then a/2 else 3*a + 1) in
	  di.add a res;
	  res
  | e::q -> e;; (* le temps de vol a déjà été calculé *)

for a = 2 to 100 do (* stocke les temps de vol dans di *)
  t a
done;;

let time_syr n di =
  let t1 = sys__time () in
  syracuse di n;
  sys__time () -. t1;;

let rec syracuse2 n =
  if n = 1 then 0
  else if n mod 2 = 0 then 1 + syracuse2 (n / 2)
  else 1 + syracuse2 (3 * n + 1);;

let n = 1000001 in let f = time_syr n in
f (dico__of_avl V (prefix <)),
f (dico__of_hashtbl (hashtbl__new n)),
		    (let t1 = sys__time () in
		     let v = make_vect (n+1) 0 in 
		   for i = 1 to n do
		     v.(i) <- syracuse2 i;
		   done;
		   sys__time () -. t1);;

(* tas *)

load "tas";;
load "draw";;

let tas = tas__of_dynvect (dynvect__of_vect [|3; 1; 2; 0|]) (prefix <);;
tas__take_min tas;;
tas__add tas 5;;
tas__add tas 0;;
tas__add tas 1;;
tas__add tas 4;;
tas__add tas (-1);;
draw__arb (tas__to_arb tas) string_of_int;;

(* graphes *)

load "graph";;
#open "graph";;
load "util";;

let g = graph__of_adj_list([|[1; 2; 5]; [0; 2]; [0; 1]; [4]; [3]; [3]|]);;

load "parcours";;
#open "parcours";;
load "util";;
bfs g 0;;

let g = graph__of_mat 10;;
graph__rdm g 0.5;;
util__write_file "rdm_graph.dot" (to_dot g string_of_int);;
bfs g 4;;

load "graph";;
#open "graph";;
load "shortest";;
let n = 20 in
let g = graph__of_mat n in 
graph__rdm g 0.5;
let w = util__mat_rdm n n n in
(*util__write_file "rdm_graph.dot" (to_dotw g w string_of_int);*)
w, shortest__dijkstra g w 0;;

type arbr = Vr | Nr of int * arbr * arbr * arbr ref;;

let rec remplir a pere = match a with 
  | Vr -> ()
  | Nr(r, g, d, p) -> p := pere; remplir g a; remplir d a;;

let a = N(0,
	  N(1, N(2, V, V, ref V), N(3, V, V, ref V), ref V),
	  N(4, V, V, ref V),
	  ref V);;

remplir a V;;
a;;

type arb = V | N of int * arb * arb * arb;;

let rec arbr_to_arb = function
  | Vr -> V
  | Nr(r, g, d, p) -> N(r, arbr_to_arb g, arbr_to_arb d, !p);;
