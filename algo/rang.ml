(* calcul de rang (TD 2) *)

type 'a arb_rang = V | N of 'a * 'a arb_rang * 'a arb_rang * int;;

let rec add a e = match a with
  | V -> N(e, V, V, 1)
  | N(r, g, d, n) -> if e < r then N(r, add g e, d, n+1)
    else N(r, g, add d e, n+1);;
let rec of_list = function
  | [] -> V
  | e::q -> add (of_list q) e;;

let sz = function
  | V -> 0
  | N(_, _, _, n) -> n;;
let rec get_kth a k = match a with
  | N(r, g, d, _) when k = sz g + 1 -> r
  | N(r, g, d, _) when k < sz g + 1 -> get_kth g k
  | N(r, g, d, _) -> get_kth d (k - sz g - 1);;

(* exemple *)
let a = of_list [3; 2; 5; 1; 9; 0; 7];;
get_kth a 4;;

let partition t i j =
  let pivot = t.(i) and ipivot = ref i in
  for k = i + 1 to j do
    if t.(k) < pivot
    then (t.(!ipivot) <- t.(k);
	  incr ipivot;
	  t.(k) <- t.(!ipivot))
  done;
  t.(!ipivot) <- pivot;
  !ipivot;;
  
let rec get_kth t k i j = (* renvoie l'élément de rang k de t en sachant qu'il est entre les indices i et j *)
  let ipivot = partition t i j in
  if k = ipivot + 1 then t.(ipivot)
  else if k < ipivot + 1 then get_kth t k i (ipivot - 1)
  else get_kth t k (ipivot + 1) j;;

(* exemple *)
let t = [|3; 2; 5; 1; 9; 0; 7|];;
for k = 1 to Array.length t do
  print_int (get_kth t k 0 6)
done;;

let rec get_rank tas k =
  if k = 1 then (let res = take_min tas in
		 add tas res;
		 res)
  else (let mini = take_min tas in
	let res = get_rank tas (k-1) in
	add tas mini;
	res);;
