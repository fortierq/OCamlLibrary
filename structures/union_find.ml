type union_find = { pere : int vect; rang : int vect };;

let create n =
  let t = Array.make n 0 and rg = Array.make n 0 in
  for i = 1 to n - 1 do
    t.(i) <- i
  done;
  { pere = t; rang = rg };;

let rec find uf i =
  if uf.pere.(i) = i then i
  else let rep = find uf uf.pere.(i) in
       uf.pere.(i) <- rep;
       rep;;
       
let union uf i j =
  let repi = find uf i and repj = find uf j in
  if uf.rang.(repi) < uf.rang.(repj) then uf.pere.(repi) <- repj
  else uf.pere.(repj) <- repi;
  if uf.rang.(repi) = uf.rang.(repj) then uf.rang.(repj) <- uf.rang.(repj) + 1;;

let uf = new 10;;
union uf 1 7;;
union uf 3 4;;
union uf 3 7;;
union uf 7 9;;
find uf 1;;

load "graph";;
#open "graph";;

let comp_connexes g =
  let uf = uf_new g.n in
  for u = 0 to g.n - 1 do
    do_list (uf_union uf u) (g.voisins u)
  done;
  uf;;

