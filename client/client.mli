val unit: 'a Lwt.t

val display_text: string -> unit

val show_status: unit

val save_note: string -> string -> unit Lwt.t

val get_note_name: string

val open_modal: unit -> unit

val close_modal: unit -> unit

val display_notes: Brr.El.t -> unit

val preview_markdown: Jv.t -> unit

val main: unit -> unit