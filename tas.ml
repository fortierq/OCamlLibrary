load "util";;
load "dynvect";;
#open "dynvect";;
load "arb";;
#open "arb";;
  
type 'a tas = { t : 'a dynvect; less : 'a -> 'a -> bool };;

let pere i = (i - 1) / 2;;
let fg i = 2 * i + 1;;
let fd i = 2 * i + 2;;
let swap tas i j =
  let tmp = tas.t.get i in
  tas.t.set i (tas.t.get j);
  tas.t.set j tmp;;

let rec up tas i =
  if i <> 0 && tas.less (tas.t.get i) (tas.t.get (pere i)) then begin
    swap tas i (pere i);
    up tas (pere i)
  end;;

let add tas e =
  tas.t.add e;
  up tas (tas.t.size () - 1);;

let rec down tas i =
  let j = ref i in
  if fg i < tas.t.size () && tas.less (tas.t.get (fg i)) (tas.t.get i) then j := fg i;
  if fd i < tas.t.size () && tas.less (tas.t.get (fd i)) (tas.t.get !j) then j := fd i;
  if !j != i then begin
    swap tas i !j;
    down tas !j
  end;;

let take_min tas =
  let res = tas.t.get 0 in
  swap tas 0 (tas.t.size () - 1);
  tas.t.del ();
  down tas 0;
  res;;

let peek_min tas = tas.t.get 0;;

let is_empty tas = tas.t.size () = 0;;

let of_dynvect t less =
  let tas = { t = t; less = less } in
  for i = tas.t.size () / 2 - 1 downto 0 do
    down tas i
  done; tas;;

let new less = of_dynvect (dynvect__of_vect [||]) less;;

let to_arb tas =
  let n = tas.t.size () in
  let rec aux i =
    if i >= n then arb__V
    else arb__N(tas.t.get i, aux (fg i), aux (fd i)) in
  aux 0;;

let is_tas tas =
  let rec aux i =
    let n = tas.t.size () in
    if i > n then true
    else (fd i >= n || tas.t.get (fd i) < tas.t.get i)
      && (fg i >= n || tas.t.get (fg i) < tas.t.get i)
      && aux (fd i) && aux (fg i) in
  aux 0;;
      
let of_arb a = (* plus simple qu'un parcours en largeur pour un arbre quelconque *)
  let t = make_vect (arb__size a) 0 in
  let rec aux a i = match a with
    | V -> ()
    | N(r, g, d) -> t.(i) <- r;
      aux g (fg i);
      aux d (fd i) in
  aux a 0;
  t;;

of_arb (N(1, N(2, V, V), N(3, V, V)));;
  
(*
let tas_tri t =
  let tas = tas_of_vect t in
  for i = 0 to tas.t.n - 1 do
    take tas
  done;;

let update tas i new_e =
  let prev_e = tas.t.t.(i) in
  tas.t.t.(i) <- new_e;
  if prev_e < new_e then monter tas i
  else descendre tas i;;
*)
