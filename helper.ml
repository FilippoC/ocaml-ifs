
let pi = 4. *. atan 1.;;

let make_rot = function v ->
    (
        (cos (pi /. v), -. sin (pi /. v), 0.0), 
        (sin (pi /. v), cos (pi /. v), 0.0)
    )
;;

let make_scale = function v ->
    (
        (v, 0.0, 0.0),
        (0.0, v, 0.0)
    )
;;

let make_trans = function x -> function y ->
    (
        (1.0, 0.0, x),
        (0.0, 1.0, y)
    )
;;

let make_origin_trans = function x -> function y ->
    let to_orig = ((1.0, 0.0, -. x), (0.0, 1.0, -. y)) in
    let to_middle = ((1.0, 0.0, x), (0.0, 1.0, y)) in
    (to_orig, to_middle)
;;
