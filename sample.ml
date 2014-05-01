
let base = [(0.0, 0.0); (600.0, 0.0); (600.0, 600.0); (0.0, 600.0)] in

let to_orig, to_middle = Helper.make_origin_trans 300.0 300.0 in 
let scale_1 = Helper.make_scale 0.8 in 
let scale_2 = Helper.make_scale 0.6 in 
let trans = Helper.make_trans 0.0 50.0 in
let rot_1 = Helper.make_rot 4.0 in
let matrix = [[to_orig ; scale_1 ; rot_1 ; to_middle ; trans]  ; [scale_2]] in
Ifs.iter matrix 20 base;;

Gui.display_buffer ();;
