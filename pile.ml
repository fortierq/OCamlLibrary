
type 'a pile =
  { push : 'a -> unit;
    pop : unit -> 'a;
    is_empty : unit -> bool };; 

let of_list l =
  let p = ref l in
  { push = (fun e -> p := e::!p);
    pop = (fun () -> let res = List.hd !p in
		     p := List.tl !p;
		     res);
    is_empty = (fun () -> !p = []) };;

(* push reversed p1 in p2 *)
let push_rev p1 p2 =
  while not p1.is_empty () do
    p2.push (p1.pop ())
  done;;

