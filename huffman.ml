
load "tas";;
load "dico";;
#open "dico";;
load "fp";;
#open "fp";;

type 'a arb_huffman = F of 'a | N of 'a arb_huffman * 'a arb_huffman;;

let to_huffman fp freq = 
  let rec init = function
    | [] -> ()
    | (f, c)::q -> fp.add (f, F(c)); init q in
  init freq;
  let rec build () =
    let f1, a1 = fp.take_min () in
    if fp.is_empty () then a1
    else let f2, a2 = fp.take_min () in
	 fp.add (f1 + f2, N(a1, a2));
	 build ()
  in build ();;

let to_huffman fp freq = 
  let rec init = function
    | [] -> ()
    | (f, c)::q -> fp.add (f, F(c)); init q in
  init freq;
  for i = 0 to List.length freq - 2 do
    let (f1, a1), (f2, a2) = fp.take_min (), fp.take_min () in
    fp.add (f1 + f2, N(a1, a2))
  done;
  snd (fp.take_min ());;


let freq = [(20, `a`); (15, `b`); (7, `c`); (14, `d`); (44, `e`)];;
let a = to_huffman (fp__of_tas (tas__new (prefix <))) freq;;

let rec read arb l = match arb with 
  | F(e) -> e, l
  | N(g, d) -> read (if (List.hd l) = 0 then g else d) (List.tl l);;
   
let rec decode arb l =
  if l = [] then []
  else let e, next = read arb l in
       e::(decode arb next);;

let rec to_dico arb di chemin = match arb with
  | F(e) -> di.add e (List.rev chemin)
  | N(g, d) -> to_dico g di (0::chemin); to_dico d di (1::chemin);; 
  
let rec code di = function
  | [] -> []
  | c::q -> (List.hd (di.get c)) @ code di q;;

let di = dico__of_list [];;
to_dico a di [];;
code di ["b"; "a"; "c"];;
