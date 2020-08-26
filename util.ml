
let rec do_list f = function
  | [] -> ()
  | e::q -> (f e;
	     do_list f q);;

let list_max = it_list max min_int;;

let rec list_max = function
  | [] -> failwith "vide"
  | [e] -> e
  | e::q -> max e (list_max q);;

let rec list_range mini maxi =
  if mini = maxi then []
  else mini::list_range (mini + 1) maxi;;

let vect_max t =
  let res = ref t.(0) in
  for i = 1 to vect_length t - 1 do
    res := max !res t.(i)
  done; !res;;

let log2 x = (log x) /. (log 2.);; (* log base 2 *)

let rec pow x n = (* puissance entiere *)
  if n = 0 then 1
  else let e = pow x (n/2) in
       if n mod 2 = 0 then e*e else e*e*x;;

let vect_rdm maxi n =
  let t = make_vect n 0 in
  for i = 0 to n - 1 do
    t.(i) <- random__int maxi
  done; t;;

let mat_rdm maxi n p =
  let m = make_vect n [||] in
  for i = 0 to n - 1 do
    m.(i) <- vect_rdm maxi p
  done; m;;

let rec list_rdm maxi n = match n with
  | 0 -> []
  | n -> (random__int maxi)::list_rdm maxi (n-1);;

let rec tri_fusion = function
  | [] -> []
  | [e] -> [e]
  | l ->
     let rec split = function
       | [] -> [], []
       | [e] -> [e], []
       | e1::e2::q -> let l1, l2 = split q in
		      e1::l1, e2::l2 in
     let l1, l2 = split l in
     list_fusion (tri_fusion l1) (tri_fusion l2);;

let rec tri_fusion
    
let rec klist_fusion = function (* fusionne une liste de listes triées *)
  | [l] -> l
  | _ ->
     let rec fusion_paire = function (* fusionne les listes par paires *)
       | [] -> []
       | [l] -> [l]
       | l1::l2::q -> (list_fusion l1 l2)::fusion_paire q in
     klist_fusion (fusion_paire ll);;

let rec list_fusion l1 l2 = match l1, l2 with (* fusion de 2 listes triées *)
  | [], _ -> l2
  | _, [] -> l1
  | e1::q1, e2::q2 -> if e1 < e2 then e1::list_fusion q1 l2
                      else e2::list_fusion q2 l1;;

let rec divise l = match l with
  | [] -> [], []
  | [e] -> [e], []
  | e1::e2::q -> let l1, l2 = divise q in
		 e1::l1, e2::l2;;

let rec klist_fusion ll = match ll with (* fusionne une liste de listes triées *)
  | [l] -> l
  | _ -> let ll1, ll2 = divise ll in
	 list_fusion (klist_fusion ll1) (klist_fusion ll2);;

klist_fusion [[1; 4; 7]; [2; 5; 6]; [3; 8]; [0; 6; 9]];;

let rec klist_rdm = function
  | 0 -> []
  | k -> (list_rdm 1000 10)::klist_rdm (k-1);;
let ll = map tri_fusion (klist_rdm 8);;
klist_fusion ll;;

let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp;;

let shuffle t =
  for i = 0 to Array.length t - 1 do
    swap t i (Random.int (i+1))
  done;;

let flist_of_vect f l = (* transform function f on vect to function on list *)
  let t = vect_of_list l in
  f t; list_of_vect t;;

let list_shuffle = flist_of_vect vect_shuffle;; 
    
let eq x y less = not (less x y || less y x);;

let write_file file str =
  let channel = open_out file in
  output_string channel str;
  close_out channel;;

let premier p =
  let rec aux k =
    if k * k > p then true
    else p mod k <> 0 && aux (k+1)
  in p > 1 && aux 2;;

let rec inter l1 l2 = (* intersection de listes triées *)
  match l1, l2 with
  | [], _ -> []
  | _, [] -> []
  | e1::q1, e2::q2 when e1 = e2 -> e1::inter q1 q2
  | e1::q1, e2::q2 when e1 < e2 -> inter q1 l2
  | e1::q1, e2::q2 -> inter l1 q2;;
