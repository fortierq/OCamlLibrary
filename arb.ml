#use "util";;

type 'a arb = V | N of 'a * 'a arb * 'a arb;;

let rec ht = function
  | V -> -1
  | N(_, g, d) -> 1 + max (ht g) (ht d);;

(*let print a printer =  affiche a en utilisant printer pour afficher les sommets 
  let h = ht a in
  let rec bfs prof space cur next = match cur with
    | [] -> print_string "\n"; if prof < h then bfs (prof + 1) ((space-1)/2) (rev next) []
    | e::q -> print_string (make_string (if next = [] then (space-1)/2 else space) ' ');
      match e with
      | V -> print_char ' '; bfs prof space q (V::V::next)
      | N(r, g, d) -> printer r; bfs prof space q (d::g::next) in
  bfs 0 (util__pow 2 (h+1) - 1) [a] [];;*)

let infixe a =
  let rec aux acc = function
  | V -> acc
  | N(r, g, d) -> aux (r::aux acc d) g in
  aux [] a;;

let rec prefixe a = match a with
  | V -> []
  | N(r, g, d) -> r::(prefixe g @ prefixe d);;

let rec size = function
  | V -> 0
  | N(r, g, d) -> 1 + (size g) + (size d);;

let rec reroot acc = function
  | V -> acc
  | N(r, g, d) -> if ht g > ht d then reroot (N(r, d, acc)) g
		  else reroot (N(r, g, acc)) d;;

let diam a = ht (reroot V a);;

let rec diam a = match a with
  | V -> -1
  | N(r, g, d) -> let diamg, diamd = diam g, diam d in
		  max (diam g) (max (diam d) (ht g + ht d + 2));;
type 'a arb = N of 'a * 'a arb list;;

