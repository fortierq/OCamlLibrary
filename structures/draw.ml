open Graphics;;
#load "graphics.cma";;

let suite xmin y =
  open_graph "600x400";
  let ymax = util__vect_max y in
  let yscale = 600 / (ymax+1) in
  let xscale = 400 / vect_length y in
  for i = 0 to vect_length y - 2 do
    moveto (i * xscale) (y.(i) * yscale);
    lineto ((i + 1) * xscale) (y.(i + 1) * yscale)
  done;
  let _ = read_key () in
  close_graph ();;

load "arb";;
#open "arb";;

let g_circle_size = ref 0.5;;
let g_min_spacing = ref 1.;;
type pos = {x: float; y: float};;
type bbox = {x1 : float; x2 : float; y1 : float; y2 : float};;
let p x y = {x = x; y = y};;

type 'a instruction =
| PrintNode of pos * 'a
| PrintEdge of pos * pos;;

let min_scale scale v = (scale (p v v)).x;;

let execute_single scale = function
| PrintNode(p, content) -> draw_circle (int_of_float p.x) (int_of_float p.y) (int_of_float (min_scale scale !g_circle_size));
                            let (w, h) = text_size content in
                            moveto (int_of_float (p.x -. (float_of_int w) /. 2.)) (int_of_float (p.y -. (float_of_int h) /. 2.));
                            draw_string content
| PrintEdge(p1, p2) ->
    let vec = p (p2.x -. p1.x) (p2.y -. p1.y) in
    let len = sqrt ((vec.x *. vec.x) +. (vec.y *. vec.y)) in
    let vec = p (vec.x /. len) (vec.y /. len) in
    let p1 = p (p1.x +. (min_scale scale !g_circle_size) *. vec.x) (p1.y +. (min_scale scale !g_circle_size) *. vec.y)
    and p2 = p (p2.x -. (min_scale scale !g_circle_size) *. vec.x) (p2.y -. (min_scale scale !g_circle_size) *. vec.y) in
    moveto (int_of_float p1.x) (int_of_float p1.y);
    lineto (int_of_float p2.x) (int_of_float p2.y);;

let rec execute_inst scale = function
| [] -> ()
| a::q -> execute_single scale a; execute_inst scale q;;

let shift_pos shift pp = p (pp.x +. shift.x) (pp.y +. shift.y);;

let shift_single shift = function
| PrintNode(p, t) -> PrintNode(shift_pos shift p, t)
| PrintEdge(p1, p2) -> PrintEdge(shift_pos shift p1, shift_pos shift p2);;

let rec shift_inst shift = function
| [] -> []
| a::q -> (shift_single shift a)::(shift_inst shift q);;

let scale_pos scale pp = scale pp;;

let scale_single scale = function
| PrintNode(p, t) -> PrintNode(scale_pos scale p, t)
| PrintEdge(p1, p2) -> PrintEdge(scale_pos scale p1, scale_pos scale p2);;

let rec scale_inst scale = function
| [] -> []
| a::q -> (scale_single scale a)::(scale_inst scale q);;

let mk_bbox pp = {x1 = pp.x; x2 = pp.x; y1 = pp.y; y2 = pp.y};;

let merge_bbox bb1 bb2 =
    {x1 = min bb1.x1 bb2.x1; x2 = max bb1.x2 bb2.x2;
     y1 = min bb1.y1 bb2.y1; y2 = max bb1.y2 bb2.y2};;

let bbox_single = function
| PrintNode(p, _) -> mk_bbox p
| PrintEdge(p1, p2) -> merge_bbox (mk_bbox p1) (mk_bbox p2);;

let rec bbox_inst = function
| [] -> {x1 = 0.; x2 = 0.; y1 = 0.; y2 = 0.}
| [a] -> bbox_single a
| a::q -> let bba = bbox_single a and bbq = bbox_inst q in merge_bbox bba bbq;;

let center_inst l =
    let bbox = bbox_inst l in
    let s = p ((float_of_int (size_x ())) /. 2. -. (bbox.x2 +. bbox.x1) /. 2.)
              ((float_of_int (size_y ())) /. 2. -. (bbox.y2 +. bbox.y1) /. 2.) in
    shift_inst s l;;

let best_scale_inst l =
    let bbox = bbox_inst l in
    let wscale = (float_of_int (size_x ()) *. 0.9 /. (bbox.x2 -. bbox.x1))
    and hscale = (float_of_int (size_y ()) *. 0.9 /. (bbox.y2 -. bbox.y1)) in
    p wscale hscale;;

type 'a arb = V | N of 'a * 'a arb * 'a arb;;
let rec gen_arb leq = function
| V -> []
| N(r, g, d) ->
    let instl = gen_arb leq g and instr = gen_arb leq d in
    let bl = bbox_inst instl and br = bbox_inst instr in
    let sl = p (-. bl.x2 -. !g_min_spacing /. 2.) (-.1.) and sr = p (-. br.x1 +. !g_min_spacing /. 2.) (-.1.) in
    let root_p = p (-. (bl.x2 +. br.x1) /. 2.) 0. in
    let instl = shift_inst sl instl and instr = shift_inst sr instr in
    let inst = ref (PrintNode(root_p, leq r)::(instl @ instr)) in
    if instl <> [] then
        inst := PrintEdge(root_p, sl)::(!inst);
    if instr <> [] then
        inst := PrintEdge(root_p, sr)::(!inst);
    shift_inst (p (-. root_p.x) (-. root_p.y)) !inst;;

let arb a leq =
    open_graph "";
    g_min_spacing := 2.;
    let inst = gen_arb leq a in
    let best_scale = best_scale_inst inst in
    let scale = function pp -> p (pp.x *. best_scale.x) (pp.y *. best_scale.y) in
    g_circle_size := (float_of_int (snd (text_size "T"))) /. (min best_scale.x best_scale.y) /. 2. *. 1.4;
    let inst = scale_inst scale inst in
    let inst = center_inst inst in
    execute_inst scale inst;
    let _ = read_key () in
    close_graph ();;

let exemple = N(1, N(2, V, N(3, V, V)), N(4, V, V));;
arb exemple string_of_int;;
