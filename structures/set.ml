load "util";;
load "dico";;
#open "dico";;
load "avl";;

type 'a set =
  { add : 'a -> unit;
    del : 'a -> unit;
    has : 'a -> bool };;

let union_add s1 s2 =
  let rec aux = function
    | [] -> ()
    | e::q -> if not s2.has e then s2.add e in
  aux (s1.to_list ());;

let add s1 s2 =
  let rec aux = function
    | [] -> ()
    | e::q -> s2.add e in
  aux (s1.to_list ());;

let copy s =
  let res = s.new () in
  add s res; res;;
  
let dico_to_set di =
  { add = (fun e -> di.add e ());
    del = di.del;
    has = (fun e -> di.get e <> []) };;

let of_hashtbl h = of_dico (dico__of_hashtbl h);;

(*let of_binary =
  let s = ref b in
  { add = (fun k -> s := !s lor (1 lsl k));             (* O(1) *)
    del = (fun k -> s := !s land (lnot (1 lsl k)));     (* O(1) *)
    has = (fun k -> !s land (1 lsl k) <> 0);            (* O(1) *)
    data = (fun () -> !s);                              
    union = (fun b' -> s := !s lor b'.data ());         (* O(1) *)
    inter = (fun b' -> s := b land b'.data ());         (* O(1) *)
    size = fun () -> let res = ref 0 and b' = ref !s in (* O(n) *)
		     while !b' <> 0 do
		       if !b' mod 2 = 1 then incr res;
		       b' := !b' / 2
  done; !res };;*)


let rec of_avl avl less =
  let a = ref avl in
  { add = (fun e -> a := avl__add !a less e); (* O(log(n)) *)
    del = (fun e -> a := avl__del !a less e); (* O(log(n)) *)
    has = avl__has !a less;                   (* O(log(n)) *)
    new = (fun () -> of_avl avl__V less);        (* O(1) *)
    to_list = (fun () -> avl__infixe !a) };;  (* O(n), return sorted list *)

let rec of_list l =
  let s = ref l in
  { add = (fun e -> s := e::!s);             (* O(1) *)
    del = (fun e -> s := list__except e !s); (* O(n) *)
    has = (fun e -> list__mem e !s);         (* O(n) *)
    new = (fun () -> of_list []);
    to_list = fun () -> !s };;      
