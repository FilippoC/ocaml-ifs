#load "graphics.cma"

open List;;

let draw_quad = function quad ->
    let x, y = nth quad ((length quad) - 1) in
    Graphics.moveto x y ;
    let rec foreach_p = function quad ->
        match quad with
          [] -> ()
        | (x, y)::quad -> begin Graphics.lineto x y ; foreach_p quad end
    in
    foreach_p quad
;;

let rec iter = function matrix_multiply -> function nb_quads -> function nb_iter -> function quad ->
    if nb_iter == 0
    then
        draw_quad quad
    else
        (* yeah...not very convincing...we must get it from the number of element of the matrix of transformation *)
        let tmp_l = Array.to_list (Array.make nb_quads ()) in
        let quads2 = mapi (function i -> function _ -> map (matrix_multiply i) quad) tmp_l in
        ignore (map (function quad -> iter matrix_multiply nb_quads (nb_iter - 1) quad) quads2)

;;

Graphics.open_graph "";;
Graphics.clear_graph ();;
Graphics.resize_window 600 600;;


let matrix_multiply = function matrix -> function i -> function p ->
begin
    let (do_mult : (int * int) -> (float * float * float) * (float * float * float) -> int * int) =
        function x, y ->
        function matrix ->
            let x = float_of_int x in
            let y = float_of_int y in
            let ((xx, xy, xc), (yx, yy, yc)) = matrix in
                (
                    int_of_float (x *. xx +. y *. xy +. xc),
                    int_of_float (x *. yx +. y *. yy +. yc)
                )
    in
    let rec foreach_trans = function matrix -> function p ->
        match matrix with
          [] -> p
        (* todo : do the matrix multiplication ! *)
        | e :: matrix -> foreach_trans matrix (do_mult p e)
    in
    foreach_trans (nth matrix i) p
end
in
let base = [(0, 0); (600, 0); (600, 600); (0, 600)] in

let pi = 4. *. atan 1. in
let to_orig = ((1.0, 0.0, -. 300.0), (0.0, 1.0, -. 300.0)) in
let to_middle = ((1.0, 0.0, 300.0), (0.0, 1.0, 300.0)) in
let scale_1 = ((0.8, 0.0, 0.0), (0.0, 0.8, 0.0)) in
let scale_2 = ((0.6, 0.0, 0.0), (0.0, 0.6, 0.0)) in
let trans = ((1.0, 0.0, 0.0), (0.0, 1.0, 50.0)) in
let rot_1 = ((cos (pi /. 4.0), -. sin (pi /. 4.0), 0.0), (sin (pi /. 4.0), cos (pi /. 4.0), 0.0)) in
let matrix = [[to_orig ; scale_1 ; rot_1 ; to_middle ; trans]  ; [scale_2]] in
iter (matrix_multiply matrix) 2 20 base;;

ignore (Graphics.wait_next_event []);;
