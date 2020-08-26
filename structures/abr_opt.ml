load "arb";;
#open "arb";;
load "draw";;

let opt p =
  let n = Array.length p in
  let c = Array.make_matrix n (n + 1) 0. in
  for l = 1 to n do
    let w = ref 0. in
    for i = 0 to n - l do
      w := !w +. p.(i + l - 1);
      c.(i).(l) <- !w +. c.(i).(l - 1);
      for k = 0 to l - 2 do (* calcul de minimum *)
	if !w +. c.(i).(k) +. c.(i + k).(l - k - 1) < c.(i).(l)
	then c.(i).(l) <- !w +. c.(i).(k) +. c.(i + k + 1).(l - k - 1)
      done
    done
  done;
  c.(0).(n);;

let opt_abr p =
  let n = Array.length p in
  let c = Array.make_matrix n (n + 1) 0. in
  let a = Array.make_matrix n (n + 1) 0. in
  for l = 1 to n do
    let w = ref 0. in
    for i = 0 to n - l do
      w := !w +. p.(i + l - 1);
      c.(i).(l) <- !w +. c.(i).(l - 1);
      a.(i).(l) <- N(p.(i + l - 1), V, a.(i).(l - 1));
      for k = 0 to l - 2 do (* calcul de minimum *)
	if !w +. c.(i).(k) +. c.(i + k).(l - k - 1) < c.(i).(l)
	then
	  (c.(i).(l) <- !w +. c.(i).(k) +. c.(i + k + 1).(l - k - 1);
	   a.(i).(l) <- N(p.(i + k), a.(i).(k), a.(i + k + 1).(l - k - 1)))
      done
    done
  done;
  c.(0).(n), a.(0).(n);;
 
      for k = 0 to l - 2 do
	if w.(i).(l) +. c.(i).(k) +. c.(i + k).(l - k - 1) < c.(i).(l)
	then (c.(i).(l) <- w.(i).(l) +. c.(i).(k) +. c.(i + k + 1).(l - k - 1);
	      )
let opt p =
  let n = Array.length p in
  let w = Array.make_matrix n (n + 1) 0. in
  let c = Array.make_matrix n (n + 1) 0. in
  for l = 1 to n do
    for i = 0 to n - l do
      w.(i).(l) <- w.(i).(l-1) +. p.(i + l - 1);
      c.(i).(l) <- w.(i).(l) +. c.(i).(l - 1);
      for k = 0 to l - 2 do (* calcul de minimum *)
	if w.(i).(l) +. c.(i).(k) +. c.(i + k).(l - k - 1) < c.(i).(l)
	then c.(i).(l) <- w.(i).(l) +. c.(i).(k) +. c.(i + k + 1).(l - k - 1)
      done
    done
  done;
  c.(0).(n);;

let opt p =
  let n = Array.length p in
  let w = Array.make_matrix n (n + 1) 0. in
  let c = Array.make_matrix n (n + 1) 0. in
  for l = 1 to n do
    for i = 0 to n - l do
      w.(i).(l) <- w.(i).(l-1) +. p.(i + l - 1);
      c.(i).(l) <- w.(i).(l) +. c.(i).(l - 1);
      for k = 0 to l - 2 do (* calcul de minimum *)
	if w.(i).(l) +. c.(i).(k) +. c.(i + k).(l - k - 1) < c.(i).(l)
	then c.(i).(l) <- w.(i).(l) +. c.(i).(k) +. c.(i + k + 1).(l - k - 1)
      done
    done
  done;
  c.(0).(n);;
let rec opt p =
  let n = vect_length p in
  let w = make_matrix n (n + 1) 0. in
  let c = make_matrix n (n + 1) 0. in
  let a = make_matrix n (n + 1) V in
  for l = 1 to n do
    for i = 0 to n - l do
      w.(i).(l) <- w.(i).(l-1) +. p.(i + l - 1);
      c.(i).(l) <- w.(i).(l) +. c.(i).(l - 1);
      a.(i).(l) <- N(p.(i + l - 1), V, a.(i).(l - 1));
      for k = 0 to l - 2 do
	if w.(i).(l) +. c.(i).(k) +. c.(i + k).(l - k - 1) < c.(i).(l)
	then (c.(i).(l) <- w.(i).(l) +. c.(i).(k) +. c.(i + k + 1).(l - k - 1);
	      a.(i).(l) <- N(p.(i + k), a.(i).(k), a.(i + k + 1).(l - k - 1)))
      done
    done
  done;
  c.(0).(n), a.(0).(n);;
let c, a = opt [|0.2; 0.4; 0.12; 0.16; 0.12|];;   
draw__arb a string_of_float;;
