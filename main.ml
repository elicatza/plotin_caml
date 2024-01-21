open Raylib
open Vector

let bg = {r= 0x28; g= 0x28; b= 0x28; a= 0xff}

let fg = {r= 0xeb; g= 0xdb; b= 0xb2; a= 0xff}

let red = {r= 0xcc; g= 0x24; b= 0x1d; a= 0xff}

let green = {r= 0x98; g= 0x97; b= 0x1a; a= 0xff}

let margin = 0.15

(** int32x2 *)
module Intx2 = struct
  type t = {x: int; y: int}

  let ( +^ ) a b = {x= a.x + b.x; y= a.y + b.y}

  let is_null a = if a.x = 0 && a.y = 0 then true else false

  let ( -^ ) a b = {x= a.x - b.x; y= a.y - b.y}

  let ( =^ ) a b = if a.x = b.x && a.y = b.y then true else false
end

module Interval = struct
  type t = {min: float; max: float}

  let contains interval value =
    if interval.min <= value && value <= interval.max then true else false

  let surrounds interval value =
    if interval.min < value && value < interval.max then true else false

  let clamp interval value =
    if value < interval.min then interval.min
    else if value > interval.max then interval.max
    else value

  let diff interval = interval.max -. interval.min

  let range interval step =
    List.init
      (int_of_float (diff interval /. step) + 1)
      (fun x -> interval.min +. (step *. float_of_int x))
end

module Plot = struct
  type t =
    { offset: Intx2.t
    ; dim: Intx2.t
    ; interval_x: Interval.t
    ; interval_y: Interval.t
    ; mul_factor_x: float
    ; mul_factor_y: float
    ; y_top: float
    ; y_bottom: float
    ; x_left: float
    ; x_right: float }

  let create (offset : Intx2.t) (dim : Intx2.t) interval_x interval_y =
    let margin_min = float_of_int (min dim.x dim.y) *. margin in
    let y_top = float_of_int offset.y +. margin_min in
    let y_bottom = float_of_int offset.y +. float_of_int dim.y -. margin_min in
    let x_left = float_of_int offset.x +. margin_min in
    let x_right = float_of_int offset.x +. float_of_int dim.x -. margin_min in
    let mul_factor_x = (x_right -. x_left) /. Interval.diff interval_x in
    let mul_factor_y = (y_bottom -. y_top) /. Interval.diff interval_y in
    { offset
    ; dim
    ; interval_x
    ; interval_y
    ; mul_factor_x
    ; mul_factor_y
    ; y_top
    ; y_bottom
    ; x_left
    ; x_right }

  let translate_x plot x =
    if plot.interval_x.min < 0. then
      plot.x_left +. ((x -. plot.interval_x.min) *. plot.mul_factor_x)
    else plot.x_left +. (x *. plot.mul_factor_x)

  let translate_y plot y = plot.y_bottom -. (y *. plot.mul_factor_y)

  let draw_x_axis_tics plot step thick color =
    let xs = Interval.range plot.interval_x step in
    List.iter
      (fun x ->
        let trans_x = translate_x plot x in
        let start = {x= trans_x; y= plot.y_bottom} in
        let stop = {x= trans_x; y= plot.y_bottom +. 7.} in
        draw_line start stop thick color ;
        draw_text (string_of_float x) (int_of_float trans_x)
          (int_of_float (plot.y_bottom +. 20.))
          4 fg )
      xs

  let draw_y_axis_tics plot step thick color =
    let ys = Interval.range plot.interval_y step in
    List.iter
      (fun y ->
        let trans_y = translate_y plot y in
        let start = {x= plot.x_left; y= trans_y} in
        let stop = {x= plot.x_left -. 7.; y= trans_y} in
        draw_line start stop thick color ;
        draw_text (string_of_float y)
          (int_of_float (plot.x_left -. 20.))
          (int_of_float trans_y) 4 fg )
      ys

  let draw_x_axis plot step thick color =
    let start = {x= plot.x_left; y= plot.y_bottom} in
    let stop = {x= plot.x_right; y= plot.y_bottom} in
    draw_x_axis_tics plot step thick color ; draw_line start stop thick color

  let draw_y_axis plot step thick color =
    let start = {x= plot.x_left; y= plot.y_bottom} in
    let stop = {x= plot.x_left; y= plot.y_top} in
    draw_y_axis_tics plot step thick color ; draw_line start stop thick color

  let plot_points plot vals radius color =
    List.iter
      (fun (x, y) ->
        draw_circle {x= translate_x plot x; y= translate_y plot y} radius color )
      vals

  let plot_points_line plot vals thick color =
    let rec aux tail =
      match tail with
      | (x1, y1) :: (x2, y2) :: tl ->
          let start = {x = translate_x plot x1; y = translate_y plot y1} in
          let stop = {x = translate_x plot x2; y = translate_y plot y2} in
          draw_line start stop thick color;
          aux ((x2, y2) :: tl)
      | _ -> ()
    in aux vals
end

let square x = x *. x

let () =
  let width = 700 in
  let aspect_ratio = 16. /. 9. in
  let height = int_of_float (float_of_int width /. aspect_ratio) in
  let plot =
    Plot.create {x= 30; y= 0} {x= width; y= height} {min= -10.; max= 10.}
      {min= 0.; max= 30.}
  in
  let xs = List.init 10 (fun x -> float_of_int x /. 2.) in
  let ys = List.map square xs in
  let vals = List.combine xs ys in
  init_window width height "Plotting..." ;
  set_target_fps 10 ;
  let rec loop frame =
    match window_should_close () with
    | false ->
        begin_drawing () ;
        clear_background bg ;
        Plot.draw_x_axis plot 1. 2. fg;
        Plot.draw_y_axis plot 5. 2. fg;
        Plot.plot_points plot vals 3. red ;
        Plot.plot_points_line plot vals 2. fg;
        end_drawing () ;
        loop (frame + 1)
    | true ->
        ()
  in
  loop 0 ; Raylib.close_window ()
