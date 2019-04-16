
let rec exp_rapide a k = 
  if k = 0 then 1
  else let e = exp_rapide a (k/2) in
       if k mod 2 = 0 then e*e else a*e*e;;

let rec exp_rapide_mod a k n =
  if k = 0 then 1
  else let e = exp_rapide_mod a (k/2) n in
       (if k mod 2 = 0 then e*e else a*e*e) mod n;;

let rec bezout a b = (* renvoie (PGCD(a, b), u, v) tq PGCD(a, b) = au + bv *)
  if b = 0 then (a, 1, 0)
  else let d, u', v' = bezout b (a mod b) in (d, v', u' - (a/b)*v');;

let inv a n =
  let d, u, _ = bezout a n in
  if d = 1 then u
  else failwith "a n'est pas inversible mod n";;

