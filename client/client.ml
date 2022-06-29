open Brr
(* open Lwt.Syntax *)
open Lwt.Infix

let init () = Store.init (Uri.to_jstr (Window.location G.window))

let display_text text =
  let status_element =  (Document.find_el_by_id G.document) (Jstr.v "status") in
   Option.iter
    (fun elem ->
      El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v text) elem)
    status_element

let show_status msg = 
  let set_timeout = fun ~ms f -> ignore @@ Jv.apply (Jv.get Jv.global "setTimeout") Jv.[| repr f; of_int ms |] in
  set_timeout ~ms:1000 @@ fun () -> display_text msg;
  set_timeout ~ms:2000 @@ fun () -> display_text ""

(* let save_note note_name note_content =
  let res = Store.save_note_temp note_name note_content in
  match res with 
  | Ok () -> show_status "Successful commit"
  | Error _ -> show_status "Set error" *)
  
let get_note_name =
  let markdown_name_elem = (Document.find_el_by_id G.document) (Jstr.v "markdown_name") in
  match markdown_name_elem with
  | Some elem ->
    let elem_value = Jstr.to_string (El.prop El.Prop.value elem) in
    if (Jstr.is_empty (Jstr.of_string elem_value))
      then  "unnamed_note"
    else elem_value
  | None -> "new_note"

let open_modal () =
  let modal = (Document.find_el_by_id G.document) (Jstr.v "note_modal") in
  Option.iter
    (fun elem ->
      El.(set_inline_style Style.display (Jstr.v "block") elem))
    modal

let close_modal () =
  let modal = (Document.find_el_by_id G.document) (Jstr.v "note_modal") in
  Option.iter
    (fun elem ->
      El.(set_inline_style Style.display (Jstr.v "none") elem))
    modal

let display_notes elem =
  let start () =
    init () >>= fun store ->
    Store.list store >>= fun files ->
    let file = List.hd files in
    Store.local_get store [ file ] >>= fun content ->
    El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v content) elem;
    (* El.set_children editable [ El.txt' content ];
    El.set_children el [ editable ];
    let initial = { file; files; editor = Jstr.v content } in
    El.append_children el [ editor ~store ~initial ]; *)
    Lwt.return ()
  in
  Lwt_js_events.async start

let preview_markdown elem =
  let note_content = Jv.to_string elem in
  let markdown_preview_btn = (Document.find_el_by_id G.document) (Jstr.v "markdown_preview") in
  Option.iter (fun markdown_preview_elem -> 
    let note_content_as_elem = Jstr.v (Omd.to_html (Omd.of_string (note_content))) in
    El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) note_content_as_elem markdown_preview_elem
    (* let note_name = get_note_name in
    save_note note_name note_content *)
    (* Lwt.async (fun _ -> save_note note_name note_content) *)
  ) markdown_preview_btn

let main () =
  let open_modal_btn = (Document.find_el_by_id G.document) (Jstr.v "new_note") in
  Option.iter
    (fun elem ->
      Ev.listen Ev.click (fun _ -> open_modal ()) (El.as_target elem))
      open_modal_btn;
  let close_modal_btn = (Document.find_el_by_id G.document) (Jstr.v "close_modal") in
  Option.iter
    (fun elem ->
      Ev.listen Ev.click (fun _ -> close_modal ()) (El.as_target elem))
      close_modal_btn;
  let note_list_div = (Document.find_el_by_id G.document) (Jstr.v "note_list") in
  Option.iter
    (fun elem -> display_notes elem)
      note_list_div;
  let markdown_content = (Document.find_el_by_id G.document) (Jstr.v "markdown_content") in
  Option.iter
    (fun elem ->
      Ev.listen Ev.keyup
        (fun e -> preview_markdown (Jv.get (Jv.get (Ev.to_jv e) "target") "value" ))
        (El.as_target elem))
    markdown_content

let () = main ()
