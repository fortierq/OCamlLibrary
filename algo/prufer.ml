
let to_tree t =
  let fp = fp_new () in
  let n = vect_length t in
  let pere = make_vect (n+2) (-1) in
  let deg = make_vect (n+2) 0 in
  for i = 0 to n - 1 do
    deg.(t.(i)) <- deg.(t.(i)) + 1
  done;
  for i = 0 to n + 1 do
    fp.add (deg.(i), i)
  done;
  for i = 0 to n - 1 do
    pere.(snd (fp.take_min ())) <- t.(i);
    deg.(t.(i)) <- deg.(t.(i)) - 1;
    fp.add (deg.(t.(i)), t.(i))
  done;
  pere;;
    
  
