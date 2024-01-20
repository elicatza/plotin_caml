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
end

module Plot = struct
  type t =
    { offset: Intx2.t
    ; dim: Intx2.t
    ; interval_x: Interval.t
    ; interval_y: Interval.t
    ; y_top: float
    ; y_bottom: float
    ; x_left: float
    ; x_right: float }

  let create (offset : Intx2.t) (dim : Intx2.t) interval_x interval_y =
    let lowest_margin = float_of_int (min dim.x dim.y) *. margin in
    let y_top = float_of_int offset.y +. lowest_margin in
    let y_bottom = float_of_int offset.y +. float_of_int dim.y -. lowest_margin in
    let x_left = float_of_int offset.x +. lowest_margin in
    let x_right = float_of_int offset.x +. float_of_int dim.x -. lowest_margin in
    {offset; dim; interval_x; interval_y; y_top; y_bottom; x_left; x_right}

  let draw_x_axis_tics plot step =
    let mul_factor = (plot.x_right -. plot.x_left) /. Interval.diff plot.interval_x in
    let rec aux cur max step =
      if cur > max then ()
      else
        let trans_x = (cur *. mul_factor) +. plot.x_left in
        let _ =
          draw_line
            {x= trans_x; y= plot.y_bottom}
            {x= trans_x; y= plot.y_bottom +. 7.}
            2. fg
        in
        let _ =
          draw_text (string_of_float cur) (int_of_float trans_x)
            (int_of_float (plot.y_bottom +. 20.))
            4 fg
        in
        aux (cur +. step) max step
    in
    aux plot.interval_x.min plot.interval_x.max step

  let draw_y_axis_tics plot step =
    let mul_factor = (plot.y_bottom -. plot.y_top) /. Interval.diff plot.interval_y in
    let rec aux cur max step =
      if cur > max then ()
      else
        let trans_y = plot.y_bottom -. (cur *. mul_factor) in
        let _ =
          draw_line
            {x= plot.x_left; y= trans_y}
            {x= plot.x_left -. 7.; y= trans_y}
            2. fg
        in
        let _ =
          draw_text (string_of_float cur)
            (int_of_float (plot.x_left -. 20.))
            (int_of_float trans_y) 4 fg
        in
        aux (cur +. step) max step
    in
    aux plot.interval_y.min plot.interval_y.max step

  let draw_x_axis plot step =
    let start =
      { x= plot.x_left
      ; y= plot.y_bottom }
    in
    let stop =
      { x= plot.x_right
      ; y= plot.y_bottom }
    in
    let _ = draw_x_axis_tics plot step in
    draw_line start stop 2. fg

  let draw_y_axis plot step =
    let start =
      { x= plot.x_left
      ; y= plot.y_bottom }
    in
    let stop =
      { x= plot.x_left
      ; y= plot.y_top }
    in
    let _ = draw_y_axis_tics plot step in
    draw_line start stop 2. fg

  let plot_points plot vals inter_x inter_y =
    let mul_factor_y =
      float_of_int (plot.dim.y - plot.offset.y) /. Interval.diff inter_y
    in
    let mul_factor_x =
      float_of_int (plot.dim.x - plot.offset.x) /. Interval.diff inter_x
    in
    let rec aux vals =
      match vals with
      | [] ->
          ()
      | (x, y) :: tl ->
          let _ =
            draw_circle
              { x= x *. mul_factor_x +. plot.x_left
              ; y= float_of_int plot.dim.y -. (y *. mul_factor_y) -. plot.y_top }
              3. red
          in
          aux tl
    in
    aux vals
end

let square x = x *. x

let () =
  let width = 700 in
  let aspect_ratio = 16. /. 9. in
  let height = int_of_float (float_of_int width /. aspect_ratio) in
  let plot = Plot.create {x= 30; y= 0} {x= width - 30; y= height} {min = 0.; max = 10.} {min = 0.; max = 30.} in
  let xs = List.init 10 (fun x -> float_of_int x /. 2.) in
  let ys = List.map square xs in
  let vals = List.combine xs ys in
  let _ = List.iter (Printf.printf "xs: %.2f\n") xs in
  let _ = init_window width height "Hello caml" in
  let _ = set_target_fps 10 in
  let rec loop frame =
    match window_should_close () with
    | false ->
        begin_drawing () ;
        clear_background bg ;
        Plot.draw_x_axis plot 1. ;
        Plot.draw_y_axis plot 5. ;
        Plot.plot_points plot vals {min= 0.; max= 10.} {min= 0.; max= 30.} ;
        end_drawing () ;
        loop (frame + 1)
    | true ->
        ()
  in
  loop 0 ; Raylib.close_window ()
