
type 'a group = (* type abstrait de groupe *)
  { mult : 'a -> 'a -> 'a;
    inv : 'a -> 'a;
    elem : unit -> 'a list };;

let zn n = (* groupe Z/nZ *)
  { mult = (fun x y -> (x + y) mod n);
    inv = (fun x -> -x mod n);
    elem = fun () ->
      let rec aux = function
	| p when n = p -> []
	| p -> p::aux (p+1) in
      aux 0 };;

let in_center g x = (* x commute t-il avec tous les elements? *)
  let rec aux = function
    | [] -> true
    | e::q -> g.mult x e = g.mult e x && aux q in
  aux (g.elem ());;

let center g = (* elements commutants avec tous les autres *)
  let rec aux = function
    | [] -> []
    | e::q -> (if in_center g e then [e] else []) @ aux q in
  aux (g.elem ());;

let abelien g = center g = g.elem ();; (* g est-il commutatif? *)

let z6 = zn 6 in
abelien z6;;
