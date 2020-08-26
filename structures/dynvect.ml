
type 'a dynvect =
  { add : 'a -> unit;
    del : unit -> unit;
    get : int -> 'a;
    set : int -> 'a -> unit;
    size : unit -> int;
    to_list : unit -> 'a list };;

let of_vect t =
  let sz = ref (vect_length t) in
  let d = ref t in
  { add = (fun e ->                 (* O(1) amortie *)
    incr sz;
    if !sz > vect_length !d then begin
      let d_new = make_vect (2 * !sz) e in
      for i = 0 to vect_length !d - 1 do
	d_new.(i) <- !d.(i)
      done;
      d := d_new
    end
    else !d.(!sz - 1) <- e);
    del = (fun () -> decr sz);      (* O(1) *)
    get = (fun i -> !d.(i));        (* O(1) *)
    set = (fun i e -> !d.(i) <- e); (* O(1) *)
    size = (fun () -> !sz);         (* O(1) *)
    to_list = (fun () ->            (* O(1) *)
      let rec aux = function
	| i when i = !sz -> []
	| i -> !d.(i)::aux (i+1) in
      aux 0)
  };;    
  
