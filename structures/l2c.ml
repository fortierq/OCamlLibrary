
type 'a l2c = {elem : 'a; mutable prev : 'a l2c; mutable next : 'a l2c};;

let new e = let rec l = {elem = e; prev = l; next = l} in l;;

let add l e = (* add after l *)
  let l_new = {elem = e; prev = l; next = l.next} in
  l.next.prev <- l_new;
  l.next <- l_new;;
let del l = (* del l *)
  l.prev.next <- l.next;
  l.next.prev <- l.prev;;
let to_right l =
  let res = !l.elem in
  l := !l.next;
  res;;

let length l =
  let rec aux l1 =
    if l1 == l then 1 else 1 + aux l1.next in
  aux l.next;;
let mem e l =
  let cur = ref l.next in
  while !cur.elem <> e && !cur != l do
    cur := !cur.next
  done;
  !cur.elem = e;;

let is_singleton l = l.next == l;;

let fusion l1 l2 =
  (l1.next).prev <- l2;
  (l2.next).prev <- l1;
  let l1n = l1.next in
  l1.next <- l2.next;
  l2.next <- l1n;;

let to_list l =
 let rec aux l1 =
    if l1 == l then [l1.elem] else l1.elem::(aux l1.next) in
  aux l.next;;

let rec of_list = function
  | [e] -> new e
  | e::q -> let l = of_list q in
	    add l e; l;;
