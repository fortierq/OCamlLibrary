load "graph";;
#open "graph";;
load "fp";;
#open "fp";;

let rec poids_chemin w c = match c with
  | [] -> 0
  | [u] -> 0
  | u::v::q -> w.(u).(v) + poids_chemin w (v::q);;
	
let dijkstra g w r =
  let pere = Array.make g.n (-1) in
  let fp = fp_new () in
  fp.add (0, r, r);
  while not fp.is_empty () do
    let d, u, p = fp.take_min () in
    if pere.(u) = -1 then
      (pere.(u) <- p;
      do_list (fun v -> fp.add (d + w u v, v, u)) (g.voisins u))
  done;
  pere;;
	
let dijkstra g w r =
  let dist = Array.make g.n max_int in
  let fp = fp_new () in
  fp.add (0, r);
  while not fp.is_empty () do
    let d, u = fp.take_min () in
    if dist.(u) = max_int then
      (dist.(u) <- d;
       do_list (fun v -> fp.add (d + w u v, v)) (g.voisins u))
  done; dist;;

let dijkstra g w r =
  let dist = Array.make g.n max_int in
  let vu = Array.make g.n false in
  dist.(r) <- 0;
  for k = 0 to g.n - 1 do
    let rec mini i = 
      if i = g.n then max_int, i
      else if vu.(i) then mini (i+1)
      else min (dist.(i), i) (mini (i+1)) in
    let du, u = mini 0 in
    vu.(u) <- true;
    do_list (fun v -> dist.(v) <- min dist.(v) (du + w u v)) (g.voisins u);
  done; dist;;

let sum x y =
  if x = max_int || y = max_int then max_int
  else x + y;;

let floyd_warshall d =
  let n = Array.length d in
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
	d.(i).(j) <- min d.(i).(j) (sum d.(i).(k) d.(k).(j))
      done
    done
  done;;

let bellman_all g w =
  let n = Array.length g in
  let d = make_matrix n max_int in
  d.(r) <- 0;
  for k = 0 to log2 n do
    for u = 0 to n - 1 do
      for v = 0 to n - 1 do
	for x = 0 to n - 1 do
	  d.(u).(v) <- min d.(u).(v) (sum d.(u).(x) d.(x).(v))
	done
      done
    done
  done; d;;

let init_pere n =
  let pere = Array.make_matrix n n (-1) in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      pere.(i).(j) <- i
    done
  done;
  pere;;

let floyd_warshall d =
  let n = Array.length d in
  let pere = init_pere n in
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
	if d.(i).(j) > sum d.(i).(k) d.(k).(j)
	then (d.(i).(j) <- sum d.(i).(k) d.(k).(j);
	      pere.(i).(j) <- pere.(k).(j))
      done
    done
  done;
  pere;;

let rec chemin pere u v =
  if u = v then [u]
  else v::chemin pere u pere.(u).(v);;

let bellman g w r =
  let n = Array.length g in
  let d = Array.make n max_int in
  d.(r) <- 0;
  for k = 0 to n - 2 do
    for u = 0 to n - 1 do
      do_list (fun v -> d.(v) <- min d.(v) (sum d.(u) (w u v))) g.(u)
    done
  done;
  d;;

let distance_acyclique g w r =
  let n = Array.length g in
  let d = Array.make n max_int in
  d.(r) <- 0;
  let relax u v = d.(v) <- min d.(v) (sum d.(u) (w u v)) in    
  do_list (fun u -> do_list (relax u) g.(u)) (tri_topo g);
  d;;

let johnson g w =
  let d0 = bellman g w 0 in
  let wh u v = d0.(u) + w u v - d0.(v) in
  let d = Array.make n [||] in
  for u = 0 to n - 1 do
    d.(u) <- dijkstra g wh u
  done;
  d;;

let prim g w =
  let n = Array.length g in
  let fp = fp_new () in
  let pere = Array.make n (-1) in
  fp.add (0, 0, 0);
  while not fp.is_empty () do
    let _, u, p = fp.take_min () in
    if pere.(u) = -1 then
      (pere.(u) <- p;
       do_list (fun v -> fp.add (w u v, v, u)) g.(u))
  done;
  pere;;

let prim g w =
  let fp = fp_new () in
  let vu = Array.make g.n false in
  fp.add (0, 0);
  let rec poids k =
    if k = g.n then 0
    else match fp.take_min () with
    | _, u when vu.(u) -> poids k
    | p, u -> (vu.(u) <- true;
	       do_list (fun v -> fp.add (w u v, v)) g.(u);
	       p + poids (k + 1)) in
  poids 0;;

load "arb";;
#open "arb";;
let rec minmax a u v = match a with
  | V -> []
  | N(r, g, d) -> match minmax g u v, minmax d u v with
    | [], cd -> if r = u || r = v then r::cd else cd
    | cg, [] -> if r = u || r = v then r::cg else cg
    | cg, cd -> cg @ r::cd;;
  
let pfs g r =
  let pere = Array.make g.n (-1) in
  

