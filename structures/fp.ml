load "avl";;
load "tas";;

type 'a fp =
  { add : 'a -> unit;
    is_empty : unit -> bool;
    take_min : unit -> 'a };;

let of_avl a less =
  let f = ref a in
  { add = (fun e -> f := avl__add !f less e);            (* O(log(n)) *) 
    is_empty = (fun () -> !f = avl__V);                  (* O(1) *)
    peek_min = (fun () -> avl__peek_min !f);             (* O(log(n)) *)
    take_min = (fun () -> let m, a = avl__take_min !f in (* O(log(n)) *)
			  f := a; m) };;
    
let of_tas tas =
  { add = tas__add tas;                          (* O(log(n)) *)
    is_empty = (fun () -> tas__is_empty tas);    (* O(1) *)
    peek_min = (fun () -> tas__peek_min tas);    (* O(1) *)
    take_min = (fun () -> tas__take_min tas) };; (* O(log(n)) *)
let new_tas less = of_tas (tas__new less);;

let of_sorted_list l less =
  let f = ref l in
  let rec add l e = match l with
    | [] -> [e]
    | e'::q -> if less e e' then e::l
      else e'::add q e in
  { add = (fun e -> f := add !f e);       (* O(n) *)
    is_empty = (fun () -> !f = []);       (* O(1) *)
    peek_min = (fun () -> hd !f);         (* O(1) *)
    take_min = fun () -> let m = hd !f in (* O(1) *)
			  f := tl !f; m };;

let fp_new () = of_avl avl__V (prefix <);;
let klist_fusion fp ll = 
  let rec init_fp = function
    | [] -> ()
    | l::q -> fp.add l; init_fp q in
  init_fp ll;
  let rec aux () = match fp.take_min () with
    | [] -> []
    | e::q -> fp.add q; e::aux () in
  aux ();;

let less_list l1 l2 = match l1, l2 with
  | [], _ -> false
  | _, [] -> true
  | e1::q1, e2::q2 -> e1 < e2;;

load "util";;
#open "util";;
let ll = map tri_fusion (klist_rdm 8);;


klist_fusion (new_tas less_list) ll;;

