
(* from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#OCaml with some minor modifications *)
let alg1 = function x0, y0 -> function x1, y1 ->
begin
  let steep = abs_float(y1 -. y0) > abs_float(x1 -. x0) in
 
  let x0, y0, x1, y1 =
    if steep
    then y0, x0, y1, x1
    else x0, y0, x1, y1
  in
  let x0, x1, y0, y1 =
    if x0 > x1
    then x1, x0, y1, y0
    else x0, x1, y0, y1
  in
 
  let delta_x = x1 -. x0
  and delta_y = abs_float(y1 -. y0) in
  let error = -. delta_x /. 2.0
  and y_step =
    if y0 < y1 then 1.0 else -1.0
  in
  let rec loop x y error =
    if x <= x1 then
      let error = error +. delta_y in
      let y, error =
        if error > 0.0
        then (y +. y_step), (error -. delta_x)
        else y, error
      in
      (if steep then (y, x) else (x, y)) :: (loop (x +. 1.0) y error)
    else
      []
  in
  loop x0 y0 error
end
;;

let alg2 = function x1, y1 -> function x2, y2 ->
    let x1 = floor x1 in
    let y1 = floor y1 in
    let x2 = floor x2 in
    let y2 = floor y2 in

    let dx = abs_float (x2 -. x1) in
    let dy = abs_float (y2 -. y1) in
    let xincr = if x1 < x2 then 1.0 else -. 1.0 in
    let yincr = if y1 < y2 then 1.0 else -. 1.0 in

    if dx > dy
    then
    begin
        let rec loop = function i -> function (x, y) -> function erreur ->
        begin
            if i >= int_of_float dx
            then
                []
            else
                let x = x +. xincr in
                let erreur = erreur +. dy in
                let cond = erreur > dx in
                let erreur = erreur -. if cond then dx else 0.0 in
                let y = y +. if cond then yincr else 0.0 in
                (x, y) :: (loop (i + 1) (x, y) erreur)
        end
        in
        let erreur = dx /. 2.0 in
        loop 0 (x1, y1) erreur
    end
    else
    begin
        let rec loop = function i -> function (x, y) -> function erreur ->
        begin
            if i >= int_of_float dy
            then
                []
            else
                let y = y +. yincr in
                let erreur = erreur +. dx in
                let cond = erreur > dy in
                let erreur = erreur -. if cond then dy else 0.0 in
                let x = x +. if cond then xincr else 0.0 in
                (x, y) :: (loop (i + 1) (x, y) erreur)
        end
        in
        let erreur = dy /. 2.0 in
        loop 0 (x1, y1) erreur
    end
;;
