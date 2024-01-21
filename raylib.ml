type color = {r: int; g: int; b: int; a: int}

module Vector = struct
    type t = {x: float; y: float}
end

type flags = FLAG_MSAA_4X_HINT

let int_of_flag flag : Int32.t =
  match flag with
  | FLAG_MSAA_4X_HINT -> Int32.of_int 0x00000020 

external unix_getentropy : unit -> int = "caml_unix_getentropy"
external init_window : int -> int -> string -> unit = "caml_init_window"
external close_window : unit -> unit = "caml_close_window"
external begin_drawing : unit -> unit = "caml_begin_drawing"
external end_drawing : unit -> unit = "caml_end_drawing"
external window_should_close : unit -> bool = "caml_window_should_close"
external clear_background : color -> unit = "caml_clear_background"
external draw_circle : Vector.t -> float -> color -> unit = "caml_draw_circle"
external draw_rectangle : int -> int -> int -> int -> color -> unit = "caml_draw_rectangle"
external draw_line : Vector.t -> Vector.t -> float -> color -> unit = "caml_draw_line"
external set_target_fps : int -> unit = "caml_set_target_fps"
external is_key_pressed : int -> bool = "caml_is_key_pressed"
external draw_text : string -> int -> int -> int -> color -> unit = "caml_draw_text"
external set_config_flags : Int32.t -> unit = "caml_set_config_flags"
