load "pile";;
load "l2c";;
  
type 'a file = {
  add : 'a -> unit;
  is_empty : unit -> bool;
  peek : unit -> 'a;
  take : unit -> 'a };;

let of_2pile deb fin =
  { add = fin.pile__push; 
    is_empty = (fun () -> deb.pile__is_empty () && fin.pile__is_empty ());
    peek = (fun () -> (if deb.pile__is_empty () then pile__push_rev fin deb); deb.pile__peek ());
    take = (fun () -> (if deb.pile__is_empty () then pile__push_rev fin deb); deb.pile__pop ()) };;

let of_l2c l =
  { add = l2c__add l;
    is_empty = (fun () -> l2c__is_singleton l);
    peek = (fun () -> l.l2c__prev.l2c__elem);
    take = (fun () -> let res = l.l2c__prev.l2c__elem in l2c__del l; res) };;

let of_2list deb fin = of_2pile (pile__of_list deb) (pile__of_list fin);;

