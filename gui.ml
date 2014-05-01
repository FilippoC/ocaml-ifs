
let buffer = Array.make_matrix 600 600 0;;
let buffer_x = ref 0.0;;
let buffer_y = ref 0.0;;

let move_to = function x -> function y ->
    buffer_x := x ;
    buffer_y := y
;;
let draw_line = function x -> function y ->

    let rec draw_points = function (points : (float * float) list) ->
        match points with
          [] -> ()
        | (x, y) :: points -> 
            begin 
                let x = int_of_float x in
                let y = int_of_float y in
                if x >= 0 && x <= 599 && y >= 0 && y <= 599
                then
                    buffer.(x).(y) <- succ buffer.(x).(y)
                else
                    ()
                ;
                 draw_points points 
            end
    in
    let points = Bresenham.alg2 (!buffer_x, !buffer_y) (x, y) in
    draw_points points ;
    buffer_x := x ;
    buffer_y := y
;;


let display_buffer = function (_ : unit) ->
    Graphics.open_graph "";
    Graphics.auto_synchronize false;
    Graphics.clear_graph ();
    Graphics.resize_window 600 600;

    (* copy the buffer content to the screen *)

    (* can't we do something more functionnal than this ? :-) *)
    let max_value = ref 0 in
    let s = ref 0 in
    for x = 0 to 599 do
    for y = 0 to 599 do
        max_value := max buffer.(x).(y) !max_value;
        s := !s + buffer.(x).(y) 
    done
    done;

    let draw_point = function (x, y) -> function (r, g, b) ->
    begin
        Graphics.set_color (Graphics.rgb r g b) ;
        Graphics.plot x y
    end
    in
    let max_value = float_of_int (!max_value) in
    for x = 0 to 599 do
    for y = 0 to 599 do
        let value = (buffer.(x).(y)) in
        if value = 0
        then
            draw_point (x, 599 - y) (0, 0, 0)
        else
            let value = float_of_int (buffer.(x).(y)) in
            let value = value /. max_value in
    (*        let r = 255.0 *. (value ** (-. 0.1 /. value)) in
            let g = 255.0 *. (value ** 0.4) in
            let b = 255.0 *. (value ** value) in *)
            let r = 255.0 *. (value) in
            let g = r in
            let b = r in
            let r = int_of_float r in
            let g = int_of_float g in
            let b = int_of_float b in 
            let r = min r 255 in
            let g = min g 255 in
            let b = min b 255 in
            let r = max r 0 in
            let g = max g 0 in
            let b = max b 0 in
            draw_point (x, 599 - y) (r, g, b)
    done
    done;

    Graphics.synchronize ();
    ignore (Graphics.wait_next_event [])
;;
