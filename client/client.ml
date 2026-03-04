open Brr
open Lwt.Syntax

let get_element_by_id id = (Document.find_el_by_id G.document) (Jstr.v id)
  
let stop_loader () = 
  let loader = get_element_by_id "overlay" in
  Option.iter(fun elem -> El.(set_inline_style Style.display (Jstr.v "none") elem)) loader;
  let content = get_element_by_id "content" in
  Option.iter(fun elem -> El.(set_inline_style Style.display (Jstr.v "block") elem)) content

let open_modal () =
  let modal = get_element_by_id "note_modal" in
  Option.iter
    (fun elem -> El.(set_inline_style Style.display (Jstr.v "block") elem))
    modal

let close_modal () =
  let modal = get_element_by_id "note_modal" in
  Option.iter
    (fun elem -> El.(set_inline_style Style.display (Jstr.v "none") elem))
    modal

let pull_str () =
  let* _ = Store.pull_store () in
  Lwt.return ()

let push_str () =
  let* _ = Store.push_store () in
  Lwt.return ()

let display_status status =
  let status_element = get_element_by_id "status" in
  Option.iter
    (fun elem ->
      El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v status) elem)
    status_element

let show_status msg =
  let set_timeout ~ms f =
    ignore
    @@ Jv.apply (Jv.get Jv.global "setTimeout") Jv.[| repr f; of_int ms |]
  in
  set_timeout ~ms:1000 @@ fun () ->
  display_status msg;
  set_timeout ~ms:2000 @@ fun () -> display_status ""

let get_note_name () =
  let markdown_name_elem = get_element_by_id "markdown_name"
  in
  match markdown_name_elem with
  | Some elem ->
      let note_name = Jstr.to_string (El.prop El.Prop.value elem) in
      let key =
        if Jstr.is_empty (Jstr.of_string note_name) then (
          let time = string_of_float (Unix.gettimeofday ()) in
          let new_name = String.concat "-" [ "unnamed_note"; time ] in
          El.set_prop (El.Prop.jstr (Jstr.v "value")) (Jstr.v new_name) elem;
          new_name)
        else note_name
      in
      key
  | None -> "new_note"

let set_modal_default_values note_title note_content =
  let markdown_name_elem = get_element_by_id "markdown_name"
  in
  Option.iter
    (fun elem -> El.set_prop (El.Prop.jstr (Jstr.v "value")) note_title elem)
    markdown_name_elem;
  let markdown_content = get_element_by_id "markdown_content"
  in
  Option.iter
    (fun elem -> El.set_prop (El.Prop.jstr (Jstr.v "value")) note_content elem)
    markdown_content;
  let markdown_preview = get_element_by_id "markdown_preview"
  in
  Option.iter
    (fun markdown_preview_elem ->
      let note_content_as_value =
        Jstr.v (Omd.to_html (Omd.of_string (Jstr.to_string note_content)))
      in
      El.set_prop
        (El.Prop.jstr (Jstr.v "innerHTML"))
        note_content_as_value markdown_preview_elem)
    markdown_preview

let confirm_note_delete note_name =
  let alert = Jv.get Jv.global "confirm" in
  let alert v = Jv.apply alert Jv.[| of_string v |] in
  alert (String.concat "" ["Are you sure you want to delete note "; "\""; note_name; "\""])

let delete_note note_name =
  let reponse = Jv.to_bool (confirm_note_delete note_name) in
  if reponse = true then 
    let path = Store.get_path [ note_name ] in
    Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
      let* store = Store.get_store () in
      let* _ = Store.temporary_delete store path in
      let* () = push_str () in
      Lwt.return ())

let save_note_every_two_seconds note_content =
  let note_name = get_note_name () in
  let set_timeout ~ms f =
    ignore
    @@ Jv.apply (Jv.get Jv.global "setTimeout") Jv.[| repr f; of_int ms |]
  in
  set_timeout ~ms:2000 @@ fun () ->
  let path = Store.get_path [ note_name ] in
  Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
      let* store = Store.get_store () in
      let* result = Store.temporary_save store path note_content in
      show_status result;
      Lwt.return ())

let get_single_note note_title_to_edit =
  Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
      let* store = Store.get_store () in
      let* note_to_edit = Store.get_value store [ note_title_to_edit ] in
      set_modal_default_values (Jstr.v note_title_to_edit) (Jstr.v note_to_edit);
      Lwt.return ())

let format_notes title content =
  let header_wrapper = El.div ~at:At.[ class' (Jstr.v "header_wrapper") ] [] in
  let h2 = El.h2 ~at:At.[ class' (Jstr.v "note_title") ] [ El.txt (Jstr.v title) ] in
  Ev.listen Ev.click
    (fun e ->
      open_modal ();
      let note_title_to_edit =
        Jstr.to_string
          (Jv.to_jstr (Jv.get (Jv.get (Ev.to_jv e) "target") "innerHTML"))
      in
      get_single_note note_title_to_edit)
    (El.as_target h2)
  |> ignore;
  let delete_icon = El.span ~at:At.[ class' (Jstr.v "delete_icon") ] [ El.txt (Jstr.v "Delete") ] in
  Ev.listen Ev.click
    (fun e ->
      let note_title_to_delete =
        Jv.to_string
          (Jv.get
             (Jv.get (Jv.get (Ev.to_jv e) "target") "previousElementSibling")
             "innerHTML")
      in
      delete_note note_title_to_delete)
    (El.as_target delete_icon)
  |> ignore;
  El.append_children header_wrapper [ h2; delete_icon ];
  let note_content = El.div ~at:At.[ class' (Jstr.v "note_content") ] [] in
  let note_as_markdown = Jstr.v (Omd.to_html (Omd.of_string content)) in
  let note_content_as_value =
    Jstr.v
      (Jstr.to_string
         (Jstr.concat
            [ Jstr.slice ~start:0 ~stop:100 note_as_markdown; Jstr.v "..." ]))
  in
  El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) note_content_as_value note_content;
  let section = El.section ~at:At.[ class' (Jstr.v "note") ] [] in
  El.append_children section [ header_wrapper; note_content ];
  section

let rec get_contents store files elem =
  match files with
  | [] -> Lwt.return ()
  | title :: other_titles ->
      let* content = Store.get_value store [ title ] in
      let formatted_note = format_notes title content in
      El.append_children elem [ formatted_note ];
      get_contents store other_titles elem

let display_notes elem =
  El.set_children elem [];
  let* store = Store.get_store () in
  let* files = Store.list_store_content store in
  let* () = get_contents store files elem in
  Lwt.return ()

let list_notes () =
  let note_list_div = Option.get @@ get_element_by_id "note_list" in
  display_notes note_list_div

let preview_markdown note_content =
  let markdown_preview_btn = get_element_by_id "markdown_preview"
  in
  Option.iter
    (fun markdown_preview_elem ->
      let note_content_as_value =
        Jstr.v (Omd.to_html (Omd.of_string note_content))
      in
      El.set_prop
        (El.Prop.jstr (Jstr.v "innerHTML"))
        note_content_as_value markdown_preview_elem)
    markdown_preview_btn

let main () =
  let* () = pull_str () in
  let* () = list_notes () in
  stop_loader ();
  let start_new_note = get_element_by_id "new_note"
  in
  Option.iter
  (fun elem ->
    Ev.listen Ev.click
      (fun _ ->
        set_modal_default_values (Jstr.v "") (Jstr.v "");
        open_modal ())
      (El.as_target elem)
    |> ignore)
  start_new_note;
  let close_modal_btn = get_element_by_id "close_modal"
  in
  Option.iter
    (fun elem ->
      Ev.listen Ev.click
  (fun _ ->
     Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
       let open Lwt.Syntax in
       let* () = push_str () in
       let* () = list_notes () in
       close_modal ();
       Lwt.return_unit))
  (El.as_target elem)
|> ignore)
  close_modal_btn;
  let markdown_content = get_element_by_id "markdown_content"
  in
  match markdown_content with
  | Some elem ->
      Ev.listen Ev.keyup
        (fun e ->
          let value =
            Jv.to_string (Jv.get (Jv.get (Ev.to_jv e) "target") "value")
          in
          save_note_every_two_seconds value;
          preview_markdown value)
          (El.as_target elem)
          |> ignore;
          Lwt.return_unit
       | None -> Lwt.return ()

let () = Js_of_ocaml_lwt.Lwt_js_events.async main
