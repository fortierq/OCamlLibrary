load "arb";;
#open "arb";;
load "avl";;
#open "avl";;
load "util";;
load "abr";;

type ('key, 'value) dico =
  { add : 'key -> 'value -> unit;
    get : 'key -> 'value list;
    del : 'key -> unit };;

let of_list l =
  let di = ref l in
  { add = (fun k v -> di := (k, v)::!di); 
    get = (fun k -> let rec aux = function
    | [] -> []
    | (k', v)::q when k = k' -> v::aux q
    | e::q -> aux q in
		      aux !di);
    del = (fun k ->                      
      let rec aux = function
	| [] -> []
	| (k', v)::q when k' = k -> aux q
	| e::q -> e::aux q in
      di := aux !di)
  };;

(*let of_abr a less =
  let di = ref a in 
  let comp x y = less (fst x) (fst y) in
  { add = (fun (k, v) -> di := abr__add !di comp (k, v));
    get = (fun k ->
      let rec aux = function
	| arb__V -> []
	| arb__N(r, g, d) when less k (fst r) -> aux g
	| arb__N(r, g, d) when less (fst r) k -> aux d 
	| arb__N(r, g, d) -> [snd r]
      in aux !di);
    del = fun k ->
      let rec aux = function
	| arb__V -> arb__V
	| arb__N(r, g, d) when less k (fst r) -> aux g
	| arb__N(r, g, d) when less (fst r) k -> aux d
	| arb__N(r, g, d) -> 
      in di := aux !di done
  };;

let of_avl a less =
  let di = ref a in 
  let comp x y = less (fst x) (fst y) in
  { add = (fun (k, v) -> di := avl__add !di comp (k, v));
    get = (fun k ->
      let rec aux = function
	| V -> []
	| N(r, g, d, _) when less k (fst r) -> aux g
	| N(r, g, d, _) when less (fst r) k -> aux d 
	| N(r, g, d, _) -> [snd r]
      in aux !di);
    del = (fun k -> match a with
    | V -> ()
    | N(r, _, _, _) -> di := avl__del !di comp (k, snd r));
    keys = fun () -> map fst (infixe !di)
  };;*)

let rec of_hashtbl h =
  { add = (fun k v -> hashtbl__add h k v);    (* O(1) moyenne *)
    get = (fun k -> hashtbl__find_all h k);   (* O(1) moyenne *)
    del = (fun k -> hashtbl__remove h k);     (* O(1) moyenne *)
    new = (fun () -> of_hashtbl h);           
    to_list = fun () -> let res = ref [] in   (* O(n) *)
		     let f k v = res := (k, v)::!res
		     in hashtbl__do_table f h;
		     !res
  };;


