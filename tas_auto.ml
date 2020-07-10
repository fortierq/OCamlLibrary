load "arb";;
#open "arb";;

let rec fusion a1 a2 less = match a1, a2 with
  | V, _ -> a2
  | _, V -> a1
  | N(r1, g1, d1), N(r2, g2, d2) ->
     if less r1 r2 then N(r1, fusion d1 a2 less, g1)
     else N(r2, fusion d2 a1 less, g2);;

let add a e = fusion a (N(e, V, V));;

let min = function
  | V -> failwith "vide"
  | N(r, _, _) -> r;;

let del_min = function
  | V -> failwith "vide"
  | N(_, g, d) -> fusion g d;; 
