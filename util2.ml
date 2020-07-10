(* list of integers between mini and maxi *)
let rec list_range mini maxi =
  if mini = maxi then []
  else mini::list_range (mini + 1) maxi;;

(* execution time of f e *)
let timeit f e =
  let t = sys__time () in
  f e; sys__time () -. t;;

(* random list with n integers between 0 and maxi *)
let rec list_rdm maxi n = match n with
  | 0 -> []
  | n -> (random__int maxi)::list_rdm maxi (n-1);;

(* exchange t.(i) and t.(j) *)
let swap t i j = let tmp = t.(i) in t.(i) <- t.(j); t.(j) <- tmp;;

(* random permutation of a vect *)
let vect_shuffle t =
  for i = 0 to vect_length t - 1 do
    swap t (random__int (i+1)) i
  done;;

(* transform function f on vect to function on list *)
let flist_of_vect f l = 
  let t = vect_of_list l in
  f t; list_of_vect t;;

(* random permutation of a list *)
let list_shuffle = flist_of_vect vect_shuffle;; 

(* write str in file *)
let write_file file str =
  let channel = open_out file in
  output_string channel str;
  close_out channel;;

let read_file file =
  let channel = open_in file in
  let res = ref "" in
  let rec aux () =
    res := !res ^ input_line channel ^ "\n";
    aux () in
  (try (aux ())
  with | _ -> ());
    !res;;
