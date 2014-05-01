
let base = [(0.0, 0.0); (600.0, 0.0); (600.0, 600.0); (0.0, 600.0)] in

let pi = 4. *. atan 1. in
let to_orig = ((1.0, 0.0, -. 300.0), (0.0, 1.0, -. 300.0)) in
let to_middle = ((1.0, 0.0, 300.0), (0.0, 1.0, 300.0)) in
let scale_1 = ((0.8, 0.0, 0.0), (0.0, 0.8, 0.0)) in
let scale_2 = ((0.6, 0.0, 0.0), (0.0, 0.6, 0.0)) in
let trans = ((1.0, 0.0, 0.0), (0.0, 1.0, 50.0)) in
let rot_1 = ((cos (pi /. 4.0), -. sin (pi /. 4.0), 0.0), (sin (pi /. 4.0), cos (pi /. 4.0), 0.0)) in
let matrix = [[to_orig ; scale_1 ; rot_1 ; to_middle ; trans]  ; [scale_2]] in
Ifs.iter matrix 20 base;;

Gui.display_buffer ();;
