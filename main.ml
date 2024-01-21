include Plot
include Raylib

module F32x3 = struct
  type t = {x: float; y: float; z: float}

  let ( +^ ) u v = {x= u.x +. v.x; y= u.y +. v.y; z= u.z +. v.z}

  let ( -^ ) u v = {x= u.x -. v.x; y= u.y -. v.y; z= u.z -. v.z}

  let ( *^ ) u scaler = {x= u.x *. scaler; y= u.y *. scaler; z= u.z *. scaler}

  let ( /^ ) u divisor = {x= u.x /. divisor; y= u.y /. divisor; z= u.z /. divisor}

  let dot u v = (u.x *. v.x) +. (u.y *. v.y) +. (u.z *. v.z)

  let cross u v =
    { x= (u.y *. v.z) -. (u.z *. v.y)
    ; y= (u.z *. v.x) -. (u.x *. v.z)
    ; z= (u.x *. v.y) -. (u.y *. v.x) }
end
open F32x3

let bg = {r= 0x28; g= 0x28; b= 0x28; a= 0xff}
let fg = {r= 0xeb; g= 0xdb; b= 0xb2; a= 0xff}
let red = {r= 0xcc; g= 0x24; b= 0x1d; a= 0xff}
let green = {r= 0x98; g= 0x97; b= 0x1a; a= 0xff}

let q = 1.
let m = 0.01

let e_field = {x= 0.0; y= -0.004; z= 0.0}
let b = {x= 0.0; y= 0.0; z= -0.01}

let v0 = {x= 0.24; y= 0.0; z= 0.0}
let r0 = {x= 0.0; y= 0.0; z= 0.0}
let dt = 0.01

let force_e () = e_field *^ q

let force_m v = cross (v *^ q) b

let accel v = (force_m v +^ force_e ()) /^ m
(* let accel v = {x = 0.; y = -9.81; z = 0.} *)

let rec generate_vals v0 r0 t_max =
  let rec aux vals time vel pos =
    if time < t_max then
      let a = accel vel in
      let v = vel +^ (a *^ dt) in
      let r = pos +^ (v *^ dt) in
      aux ((r.x, r.y) :: vals) (time +. dt) v r
    else vals
  in
  aux [] 0. v0 r0

let () =
  let width = 700 in
  let aspect_ratio = 16. /. 9. in
  let height = int_of_float (float_of_int width /. aspect_ratio) in
  let plot =
    Plot.create {x= 0.; y= 0.}
      {x= float_of_int width; y= float_of_int height}
      {min= 0.; max= 5.} {min= -0.4; max= 0.}
  in
  let vals = generate_vals v0 r0 14. in
  set_config_flags (int_of_flag FLAG_MSAA_4X_HINT);
  init_window width height "Plotting..." ;
  set_target_fps 1 ;
  let rec loop frame =
    match window_should_close () with
    | false ->
        begin_drawing () ;
        clear_background bg ;
        Plot.draw_x_axis plot 1. 2. fg ;
        Plot.draw_y_axis plot 0.1 2. fg ;
        (* Plot.plot_points plot vals 3. red ; *)
        Plot.plot_points_line plot vals 2. fg ;
        end_drawing () ;
        loop (frame + 1)
    | true ->
        ()
  in
  loop 0 ; Raylib.close_window ()
