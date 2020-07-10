load "util";;
load "arb";;

type 'v graph =
  { add_edge : 'v -> 'v -> unit;
    del_edge : 'v -> 'v -> unit;
    is_edge : 'v -> 'v -> bool;
    n : int; (* nombre de sommets *)
    voisins : 'v -> 'v list };;

let to_dot g str_of_v =
  let rec do_v = function
    | [] -> ""
    | v::q -> (str_of_v v) ^ "; " ^ do_v q in
  let rec do_s = function
    | [] -> "}\n"
    | u::q -> (str_of_v u) ^ " -> {" ^ do_v (g.voisins u) ^ " };\n" ^ do_s q in
  "digraph {\n" ^ do_s (util__list_range 0 g.n);;

let undirect g =
  { add_edge = (fun u v -> g.add_edge u v; g.add_edge v u);
    del_edge = (fun u v -> g.del_edge u v; g.del_edge v u);
    is_edge = g.is_edge;
    n = g.n;
    voisins = g.voisins
  };;
  
let new_adj_mat n = 
  let m = Array.make_matrix n n 0 in
  { add_edge = (fun u v -> m.(u).(v) <- 1);   (* O(1) *)
    del_edge = (fun u v -> m.(u).(v) <- 0);   (* O(1) *)
    is_edge = (fun u v -> m.(u).(v) = 1);     (* O(1) *)
    n = n;                                    (* O(1) *)
    voisins = (fun u ->                       (* O(n) *)
      let rec aux v =
	if v = n then []
	else if m.(u).(v) = 0 then aux (v + 1)
	else v::aux (v + 1) in
      aux 0)
  };;

let new_adj_list n = 
  let t = Array.make n [] in
  { add_edge = (fun u v -> t.(u) <- v::t.(u));   (* O(1) *) 
    del_edge = (fun u v ->                       (* O(deg(u)) *)
      let rec del_v = function
	| [] -> []
	| e::q -> if e = v then q else e::del_v q in
                  t.(u) <- del_v t.(u));
    is_edge = (fun u v -> List.mem v t.(u));     (* O(deg(u)) *)
    n = n;                                       (* O(1) *)
    voisins = (fun u -> t.(u))                   (* O(1) *)
  };;

let mat_of_list g =
  let n = Array.length g in
  let res = Array.make_matrix n n 0 in
  for u = 0 to n - 1 do
    do_list (fun v -> res.(u).(v) <- 1) g.(u)
  done;
  res;;

let list_of_mat g =
  let n = Array.length g in
  let res = Array.make n [] in
  for u = 0 to n - 1 do
    for v = 0 to n - 1 do
      if g.(u).(v) = 1 then res.(u) <- v::res.(u)
    done
  done;
  res;;

let g = [|[1]; [4]; [0; 1]; [5]; [2]; [3;2]|];;
list_of_mat (mat_of_list g);;
    
let rdm_graph g p =
  for u = 0 to g.n - 1 do
    for v = 0 to g.n - 1 do
      if random__float 1. < p then g.add_edge u v
    done
  done;;

   

