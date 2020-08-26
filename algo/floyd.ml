type 'a case = { elem : 'a; mutable next : 'a l1c }
and 'a l1c = V | C of 'a case;; 

let rec has_cycle lievre tortue =
  match lievre, tortue with
  | V, _ -> false
  | C(l), C(t) -> l == t ||
     (match l.next with
     | V -> false
     | C(ll) -> has_cycle ll.next t.next);;

let next l = match l with | C(c) -> c.next;;

let l = C({ elem = 1; next = C({ elem = 2; next = V }) }) in
has_cycle l (next l);;
let rec l = C({ elem = 1; next = C({ elem = 2; next = l }) }) in
has_cycle l (next l);;

