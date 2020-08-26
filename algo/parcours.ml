load "graph";;
#open "graph";;

let rec do_list f l = match l with
  | [] -> ()
  | e::q -> (f e; do_list f q);;

let rec trou_noir g c v =
  if v = vect_length g then c
  else trou_noir g (if g.(c).(v) = 1 then v else c) (v + 1);;

let rec chemin pere v =
  if pere.(v) = v then [v]
  else v::(chemin pere pere.(v));;

let arb_to_pere a =
  let pere = Array.length (size a) 0 in
  let rec aux pere_r = function
    | V -> ()
    | N(r, g, d) -> (pere.(r) <- pere_r;
		     aux r g;
		     aux r d) in
  aux (-1) a; pere;;

load "arbn.ml";;
#open "arbn";;

let arb_of_pere pere =
  let n = Array.length pere in
  let fils = Array.length n [] in
  let r = ref 0 in (* contiendra la racine *)
  for i = 0 to n - 1 do
    if pere.(i) = i then r := i
    else fils.(pere.(i)) <- i::fils.(pere.(i))
  done;
  let rec fils_to_arb i =
    N(i, map fils_to_arb fils.(i)) in
  fils_to_arb !r;;

arb_of_pere [|1; 1; 1; 0; 2; 0|];;
    
let traite = print_int;;

let bfs g r =
  let vu = Array.make g.n false in
  let rec aux cur next = match cur with
    | [] -> if next <> [] then aux next [] (* couche suivante *)
    | v::q when vu.(v) -> aux q next 
    | v::q -> (vu.(v) <- true;
	       (* traiter v *)
	       aux q (g.voisins v @ next)) in
  aux [r] [];;

let bfs g r =
  let vu = Array.make g.n false in
  let rec aux cur next = match cur with
    | [] -> if next <> [] then aux next []
    | v::q when vu.(v) -> aux q next 
    | v::q -> (vu.(v) <- true;
	       aux q (g.voisins v @ next))
  in aux [r] [];;

let bfs g r =
  let dist = Array.make g.n (-1) in
  let rec aux d cur next = match cur with
    | [] -> if next = [] then dist
            else aux (d+1) next [] 
    | v::q when dist.(v) <> -1 -> aux d q next 
    | v::q -> (dist.(v) <- d;
	       aux d q (g.voisins v @ next)) in
  aux 0 [r] [];;

let biparti g s =
  let color = Array.length g.n (-1) in
  let rec aux cur next c = match cur with
    | [] -> if next = [] then color else aux next [] (1-c) 
    | e::q when color.(e) = c -> aux q next c
    | e::q when color.(e) = 1-c -> failwith "Pas biparti"
    | e::q -> (color.(e) <- c;
	       aux q (g.voisins e @ next) c)
  in aux [s] [] 0;;

load "file";;
#open "file";;

let bfs g r =
  let vu = Array.make g.n false in
  let f = file_new () in
  let add v =
    if not vu.(v) then (vu.(v) <- true; f.add v) in
  add r;
  while not f.is_empty () do
    let u = f.take () in
    (* traiter u *)
    do_list add (g.voisins u)
  done;;

let bfs g r =
  let pere = Array.make g.n (-1) in
  let f = file_new () in
  let add p v = (* p est le pere de v *)
    if pere.(v) = -1 then (pere.(v) <- p; f.add v) in
  add r r;
  while not f.is_empty () do
    let u = f.take () in 
    do_list (add u) (g.voisins u)
  done;
  pere;;

load "pile";;

let dfs g r =
  let vu = Array.length g.n false in
  let p = of_list [] in
  let add v = 
    if not vu.(v) then (vu.(v) <- true; p.push v) in
  add r;
  while not p.is_empty () do
    let u = p.pop () in (* traiter u *)
    print_int u;
    do_list add (g.voisins u)
  done;;

let g = new_adj_list 10;;
rdm_graph g 0.3;;
dfs g 0;;
util__write_file "g.dot" (to_dot g string_of_int);;

let dfs g r =
  let vu = Array.make g.n false in
  let p = new_pile () in
  p.push r;
  while not p.is_empty () do
    let u = p.pop () in
    if not vu.(u) then
      (vu.(u) <- true;
       (* traiter u *)
       do_list p.push (g.voisins u))
  done;;

let dfs g r =
  let vu = Array.length g.n false in
  let p = of_list [] in
  p.push r;
  while not p.is_empty () do
    let u = p.pop () in (* traiter u *)
    if not vu.(u) then
      (print_int u;
       vu.(u) <- true;
       do_list p.push (g.voisins u))
  done;;
dfs g 0;;

let dfs g r =
  let vu = Array.make g.n false in
  let rec aux v =
    if not vu.(v) then (vu.(v) <- true;
			(* traiter v *)
			do_list aux (g.voisins v)) in
  aux r;;

let atteignable g r u =
  let n = Array.length g in
  let vu = Array.length n false in
  let res = ref false in
  let rec aux v =
    if v = u then res := true
    else if not vu.(v) then
      (vu.(v) <- true;
       do_list aux g.(v))
  in aux r;
  !res;;

let atteignable g r u =
  let n = Array.length g in
  let vu = Array.length n false in
  let res = ref false in
  let rec aux v =
    if v = u then res := true
    else if not vu.(v) then (vu.(v) <- true;
			     do_list aux g.(v)) in
  aux r;
  !res;;

dfs g 0;;

let dfs g =
  let vu = Array.make g.n false in
  let rec aux v =
    if not vu.(v) then (vu.(v) <- true;
			(* traiter v *)
			do_list aux (g.voisins v)) in
  for r = 0 to g.n - 1 do
    if not vu.(r) then aux r
  done;;

let tr g =
  let n = Array.length g in
  let res = Array.make n [] in
  for u = 0 to n - 1 do
    let rec aux = function
      | [] -> ()
      | v::q -> res.(v) <- u::res.(v) in
    aux g.(u)
  done;
  res;;
  
let g = [|[1]; [2; 3]; []; [2]|];;
tr g;;

let rec post_dfs g vu r =
  if vu.(r) then []
  else (vu.(r) <- true;
	let rec do_voisins = function
	  | [] -> [r]
	  | v::q -> (post_dfs g vu v) @ (do_voisins q) in
	do_voisins g.(r));;

let rec post_dfs g vu r =
  if vu.(r) then []
  else (let res = ref [] in
	vu.(r) <- true;
	do_list (fun v -> res := (post_dfs g vu v) @ !res) g.(r);
	!res @ [r]);;

let tri_topo g =
  let postfixe = ref [] in
  let n = Array.length g in
  let vu = Array.length n false in
  for u = 0 to Array.length g - 1 do
    postfixe := !postfixe @ post_dfs g vu u
  done;
  rev !postfixe;;

let tri_topo g =
  let n = Array.length g in
  let vu = Array.length n false in
  let rec aux v =
    if v = n then [r]
    else post_dfs g vu v @ aux (v+1) in
  List.rev (aux 0);;

let kosaraju g =
  let n = Array.length g in
  let vu, tg = Array.length n false, tr g in
  let rec dfs2 = function
    | [] -> []
    | v::q -> (post_dfs tg vu v)::dfs2 q in
  dfs2 (tri_topo g);;

let g = [|[1]; [4]; [0; 1]; [5]; [2]; [3;2]|];;
kosaraju g;;

let m = new_adj_list 6;;
m.add_edge 0 1; m.add_edge 1 4; m.add_edge 2 1; m.add_edge 2 0; m.add_edge 4 2; m.add_edge 2 3; m.add_edge 3 5; m.add_edge 5 3; m.add_edge 5 2;;

post_dfs g (Array.length m.n false) 0;;
kosaraju m;;
let m = undirect (new_adj_mat 5);;
m.add_edge 0 1; m.add_edge 0 2; m.add_edge 2 1; m.add_edge 3 1; m.add_edge 3 4; m.add_edge 3 0; dfs m 0;;

let has_cycle g r =
  let pere = Array.make g.n (-1) in
  let res = ref false in
  let rec aux p v = (* p a permis de découvrir v *)
    if pere.(v) = -1 then (pere.(v) <- p;
			   do_list (aux v) (g.voisins v))
    else if pere.(p) <> v then res := true in
  aux (-1) r;
  !res;;

let has_cycle g r =
  let vu = Array.length g.n false in
  let res = ref false in
  let rec aux p u = (* p a permis de découvrir u *)
    if vu.(u) then res := true
    else (vu.(u) <- true;
	  do_list (fun v -> if v <> p then aux u v) (g.voisins u)) in
  aux (-1) r;
  !res;;

let has_cycle_dir g r =
  let vu = Array.make g.n 0 in
  let res = ref false in
  let rec aux v =
    if vu.(v) = 0 then (vu.(v) <- 1;
			do_list aux (g.voisins v);
			vu.(v) <- 2)
    else if vu.(v) = 1 then res := true in
  aux r;
  !res;;

let biparti g =
  let color = Array.length g.n (-1) in
  let rec aux c v =
    if color.(v) = 1 - c then failwith "non biparti" 
    else if color.(v) = -1 then
      (color.(v) <- c;
       do_list (aux (1 - c)) (g.voisins v)) in
  aux 0 0;
  color;;

let exc g r =
  let vu = Array.length (Array.length g) false in
  let rec aux d cur next = match cur with
    | [] -> if next = [] then d else aux (d+1) next []
    | v::q when vu.(v) -> aux d q next
    | v::q -> (vu.(v) <- true;
	       aux d q (g.voisins v)@next) in
  aux 0 [r] [];;

let exc g r =
  let n = Array.length g in
  let vu = Array.make n false in
  let res = ref 0 in (* dernière distance calculée *)
  let f = file_new () in
  f.add (r, 0);
  while not f.is_empty () do
    let v, d = f.take () in
    if not vu.(v) then
      (vu.(v) <- true;
       res := d;
       let rec voisins l = match l with
	 | [] -> ()
	 | e::q -> f.add (e, d+1); voisins q in
       voisins g.(v))
  done;
  !res;;

let rec diam r g =
  if r = Array.length g then 0
  else max (exc g r) (diam (r+1) g);;

let rec diam = function
  | N(r, []) -> 0, 0
  | N(r, g::q) -> let dg, hg = diam g in
		  let dq, hq = diam (N(r, q)) in
		  max dg (max dq (hg + 1 + hq)), max (hg + 1) hq;;

let bfs1 g =
  let vu = Array.length g.n false in
  let rec aux v cur next = match cur with
    | [] -> if next = [] then v else aux v next []
    | e::q when vu.(e) -> aux e q next
    | e::q -> (vu.(e) <- true;
	       aux e q next) in
  aux 0 [0] [];;
let diam g = exc g (bfs1 g);;

let g = new_adj_list 100;;

let centre g =
  let rec aux r =
    if r = Array.length g - 1 then exc g r, r
    else min (exc g r, r) (aux (r + 1)) in
  snd (aux 0);;

let rec centre = function
  | N(r, []) -> 0, 0
  | N(r, g::q) -> let cg, dg, hg = centre g in
		  let cq, dq, hq = centre (N(r, q)) in
		  let dmax = max dg (max dq (hg + 1 + hq)) in
		  let hmax = max (hg + 1) hq in
		  if dmax = dg then cg, dg, hmax
		  else if dmax = dq then cq, dq, hmax
		    else if hq = hg + 1 then r

		      
