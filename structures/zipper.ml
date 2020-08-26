type 'a zipper = { left : 'a list; right : 'a list };;

let move_right z = { left = (hd z.right)::z.left; right = tl z.right };;

let add_right z e = { left = z.left; right = e::z.right };;
let del_right z = { left = z.left; right = tl z.right };;

let rec to_list z = match z.left with
  | [] -> z.right
  | e::q -> to_list { left = q; right = e::z.right };;

let z = {left = [2; 1; 0]; right = [3; 4; 5; 6] };;
to_list z;;
