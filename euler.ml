load "../struct/l2c";;
#open "l2c";;
load "../util";;
#open "util";;

let debruijn l p =
  let sz = pow 2 p in
  let mots = make_vect sz false in
  let cur = ref 0 in
  let next_cur b = cur := (2 * !cur) mod sz + b in
  let rl = ref l in
  for i = 0 to p - 1 do next_cur (to_right rl) done;
  let start = !rl in
  let rec aux () =
    mots.(!cur) <- true;
    next_cur (to_right rl);
    !rl == start || (not mots.(!cur) && aux ()) in
  aux ();;

let l = l2c__of_list [0; 0; 1; 1] in
debruijn l 2;;
let l = l2c__of_list [0; 0; 0; 1; 0; 1; 1; 1] in
debruijn l 3;;
