load "graph";;
#open "graph";;
    
let dfs g =
  let n = Array.length g in
  let pere = Array.make n (-1) in
  let rec aux u =
      for v = 0 to n - 1 do
        if g.(u).(v) > 0 && pere.(v) = -1
	then (pere.(v) <- u;
	      aux v)
      done in
  aux 0;
  pere;;

let cmin g pere =
  let rec aux v = 
    let p = pere.(v) in
    if p = 0 then g.(0).(v)
    else min g.(p).(v) (aux p) in
  aux 1;;

let augment g pere =
  let cm = cmin g pere in
  let rec aux v =
    if v = 0 then cm
    else (let p = pere.(v) in
	  g.(p).(v) <- g.(p).(v) - cm;
	  g.(v).(p) <- g.(v).(p) + cm;
	  aux p) in
  aux 1;;

let rec ford g chemin =
  let pere = chemin g in
  if pere.(1) = -1 then 0
  else let cm = augment g pere in
       cm + ford g chemin;;


let cmax g =
  let res = ref g.(0).(0) in
  let n = Array.length g in
  for u = 0 to n - 1 do
    for v = 0 to n - 1 do
      res := max !res g.(u).(v)
    done;
  done; !res;;

let kdfs g k =
  let n = Array.length g in
  let pere = Array.make n (-1) in
  let rec aux p u =
    if pere.(u) = -1 then (pere.(u) <- p;
			   for v = 0 to n - 1 do
			     if g.(u).(v) >= k then aux u v
			   done) in
  aux 0 0;
  pere;;

(* renvoie le k maximum avec k1 <= k < k2 *)
let rec maxkdfs g k1 k2 =
  let pere = kdfs g m in
  if k2 = k1 + 1 then pere
  else let m = (k1 + k2)/2 in (* k1 < m < k2 *)
       if pere.(1) = -1 then maxkdfs g k1 m
       else maxkdfs g m k2;;

let rec kford k g =
  if k = 0 then 0
  else let pere = kdfs g k in
       if pere.(1) = -1 then kford (k/2) g
       else let cm = augment g pere in
	    cm + kford k g;;

let voisins_k g u k =
  
let rec dfs_k g s k =
  let n = Array.length g in
  let pere = Array.make n (-1) in
  let rec aux p u =
    if pere.(u) = -1 then (pere.(u) <- p;
			   do_list (aux u) (adj g u)) in
  aux s s;
  pere;;

load "file";;
#open "file";;

let bfs g =
  let n = Array.length g in
  let pere = Array.make n (-1) in
  let f = file_new () in
  f.add 0;
  pere.(0) <- 0;
  while not f.is_empty () do
    let u = f.take () in
    for v = 0 to n - 1 do
      if g.(u).(v) > 0 && pere.(v) = -1 then
	(pere.(v) <- u;
	 f.add v)
    done;
  done; pere;;

let bfs g =
  let n = Array.length g in
  let pere = Array.make n (-1) in
  let f = file_new () in
  (* initialiser f et pere *)
  while not f.is_empty () do
    let u = f.take () in
    (* à compléter *)
  done;
  pere;;

let cut g s = 
