open List;;

let draw_quad = function quad ->
    let x, y = nth quad ((length quad) - 1) in
    Gui.move_to x y ;
    let rec foreach_p = function quad ->
        match quad with
          [] -> ()
        | (x, y)::quad -> begin Gui.draw_line x y ; foreach_p quad end
    in
    foreach_p quad
;;

let rec iter = function matrix -> function nb_iter -> function quad ->
    let matrix_multiply = function matrix -> function p ->
    begin
        let (do_mult : (float * float) -> (float * float * float) * (float * float * float) -> float * float) =
            function x, y ->
            function matrix ->
                let ((xx, xy, xc), (yx, yy, yc)) = matrix in
                    (
                        x *. xx +. y *. xy +. xc,
                        x *. yx +. y *. yy +. yc
                    )
        in
        let rec foreach_trans = function matrix -> function p ->
            match matrix with
              [] -> p
            | e :: matrix -> foreach_trans matrix (do_mult p e)
        in
        foreach_trans matrix p
    end
    in
    if nb_iter = 0
    then
        begin
        draw_quad quad
        end
    else
        let quads2 = map (function matrix -> map (matrix_multiply matrix) quad) matrix in
        ignore (map (function quad -> iter matrix (nb_iter - 1) quad) quads2)

;;
