val display_status: string -> unit

val show_status: string -> unit

(* val save_note: string -> string -> unit *)

val get_note_name: unit -> string

val format_notes: string -> string -> Brr.El.t

val open_modal: unit -> unit

val close_modal: unit -> unit

val display_notes: Brr.El.t -> unit

val preview_markdown: string -> unit

val main: unit -> unit